library(shiny)
library(plotly)
library(readr)
library(dplyr)
library(hms)

# Shiny 默认上传限制较小；允许上传最大 500 MB 的仪器数据文件
options(shiny.maxRequestSize = 500 * 1024^2)

ui <- fluidPage(
  # 在head中添加CSS样式
  tags$head(
    tags$style(HTML(" 
      #video_help_btn {
        position: fixed; 
        top: 20px; 
        right: 20px;
        z-index: 999;
        background: #00A1D6; /* B站主题蓝 */
        color: white;
        border-radius: 25px; /* 椭圆形状 */
        padding: 8px 15px;
        font-size: 14px;
        font-family: 'Microsoft YaHei';
        box-shadow: 0 3px 10px rgba(0,161,214,0.3);
        display: flex;
        align-items: center;
        gap: 8px;
        transition: all 0.3s;
        border: none;
        cursor: pointer;
      }
      #video_help_btn:hover {
        background: #0086b3; /* 深蓝色 */
        transform: translateY(-2px);
        box-shadow: 0 5px 15px rgba(0,161,214,0.4);
      }
      #video_help_btn i {
        font-size: 16px;
      }
    "))
  ),
  
  # 悬浮按钮（带图标和文字）
  div(id = "video_help_btn",
      icon("play-circle"), 
      span("视频教程"),
      onclick = "window.open('https://www.bilibili.com/video/BV1bhE4zQEKx/?vd_source=97bb2a54115f0ffa99be6857cdb781f8',  '_blank')"), 
  
  tags$head(HTML("<title>Picarro数据处理系统</title>")),
  titlePanel(
    tags$div(
      style = "background: white;
             padding: 15px;
             border-radius: 8px;
             box-shadow: 0 2px 10px rgba(0,0,0,0.1);
             display: flex;
             align-items: center;
             gap: 15px;",
      tags$div(
        style = "background: #3498db; 
               color: white;
               width: 50px;
               height: 50px;
               border-radius: 50%;
               display: flex;
               align-items: center;
               justify-content: center;
               font-size: 24px;",
        icon("database")
      ),
      tags$div(
        tags$h2(
          style = "margin: 0 0 5px 0; color: #2c3e50;",
          "Picarro数据处理系统"
        ),
        tags$p(
          style = "margin: 0; color: #7f8c8d; font-size: 12px;",
          "黄利东开发"
        )
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload your .dat or .csv file", accept = c(".dat", ".csv")),
      uiOutput("file_status"),
      selectInput("x_var", "Select X Variable", choices = NULL),
      selectInput("y_var", "Select Y Variable", choices = NULL),
      uiOutput("x_cutoff_ui"),  # 动态生成时间截断输入框
      actionButton(
        "apply_filter", 
        "更新图形",
        style = "color: white; background-color: #FF5733; border-radius: 8px;"
      ),
      verbatimTextOutput("filtered_info"),
      verbatimTextOutput("selected_avg"),  #
      numericInput("interval_minutes", "每个样品时间间隔（分钟）", value = 1, min = 1, step = 1),
      tableOutput("segment_table"),
      tags$style(HTML(" 
    #calc_segments {
      background: linear-gradient(to right, #FF8C00, #FF5733); /* 橙红渐变 */
      color: white;
      border: none;
      border-radius: 30px;
      padding: 12px 28px;
      font-size: 16px;
      font-weight: bold;
      box-shadow: 0 4px 15px rgba(255, 140, 0, 0.4);
      position: relative;
      overflow: hidden;
    }
    #calc_segments:hover::after {
      content: '';
      position: absolute;
      top: -50%;
      left: -50%;
      width: 200%;
      height: 200%;
      background: linear-gradient(
        to bottom right,
        rgba(255,255,255,0.3),
        rgba(255,255,255,0)
      );
      transform: rotate(30deg);
    }
  ")), 
      actionButton("calc_segments", "计算时间段平均值"),
      downloadButton(
        "download_segments",
        "下载分析结果（CSV）",
        style = "margin-top: 10px; color: white; background-color: #3498db; border-radius: 8px;"
      )
    ),
    mainPanel(
      plotlyOutput("xy_plot", height = "500px")  
    )
  ),
  tags$footer(
    style = "
      text-align: center;
      padding: 10px;
      background-color: #f8f9fa;
      border-top: 1px solid #e7e7e7;
      position: fixed;
      bottom: 0;
      width: 100%;
    ",
    HTML("&copy; 2026 资源环境学院 黄利东. 版权所有")
  )
)

server <- function(input, output, session) {

  file_status <- reactiveVal(NULL)

  output$file_status <- renderUI({
    status <- file_status()
    if (is.null(status)) return(NULL)

    tags$div(
      class = paste("alert", paste0("alert-", status$type)),
      style = "padding: 8px 12px; margin-bottom: 10px;",
      icon(if (status$type == "danger") "exclamation-triangle" else "info-circle"),
      tags$span(style = "margin-left: 6px;", status$message)
    )
  })
  
  # 自动解析可能是时间格式的列
  data_input <- reactive({
    req(input$file)

    file_size_mb <- input$file$size / 1024^2
    if (file_size_mb >= 50) {
      file_status(list(
        type = "warning",
        message = sprintf("文件较大（%.1f MB），读取和绘图可能需要较长时间，请耐心等待。", file_size_mb)
      ))
    } else {
      file_status(list(
        type = "info",
        message = sprintf("正在读取 %s（%.1f MB）……", input$file$name, file_size_mb)
      ))
    }

    df <- tryCatch(
      withProgress(message = "正在读取数据文件", value = 0, {
        incProgress(0.15, detail = "检查文件格式……")
        extension <- tolower(tools::file_ext(input$file$name))
        separator <- if (extension == "csv") "," else ""

        result <- read.table(
          input$file$datapath,
          header = TRUE,
          sep = separator,
          fill = TRUE,
          na.strings = c("", "NA", "NaN", "NULL", "-"),
          stringsAsFactors = FALSE,
          strip.white = TRUE,
          comment.char = "",
          check.names = FALSE
        )
        incProgress(0.65, detail = "检查缺失值和数据类型……")
        result
      }),
      error = function(e) {
        message <- paste0("文件读取失败：", conditionMessage(e), "。请检查表头、分隔符和末尾数据行。")
        file_status(list(type = "danger", message = message))
        showNotification(message, type = "error", duration = NULL)
        NULL
      }
    )

    validate(need(!is.null(df), "无法读取该文件，请根据左侧提示检查文件格式。"))
    validate(need(nrow(df) > 0 && ncol(df) > 0, "文件中没有可用的数据。"))
    validate(need("TIME" %in% names(df), "文件缺少 TIME 列，无法进行时间分析。"))

    # 数值列中少量空白或非法字符转换为 NA，避免整列被误判成文本
    converted_invalid <- 0L
    for (column_name in setdiff(names(df), "TIME")) {
      if (is.character(df[[column_name]])) {
        raw_values <- trimws(df[[column_name]])
        present <- !is.na(raw_values) & nzchar(raw_values)
        numeric_values <- suppressWarnings(as.numeric(raw_values))
        convertible_ratio <- if (any(present)) mean(!is.na(numeric_values[present])) else 0

        if (convertible_ratio >= 0.8) {
          converted_invalid <- converted_invalid + sum(present & is.na(numeric_values))
          df[[column_name]] <- numeric_values
        }
      }
    }
    
    invalid_time_values <- 0L

    # TIME 中个别空白或错误值按 NA 处理，不让整份文件读取失败
    if ("TIME" %in% names(df) && is.character(df$TIME)) {
      df$TIME <- tryCatch({
        parsed <- strptime(df$TIME, format = "%H:%M:%OS")
        present_time <- !is.na(df$TIME) & nzchar(trimws(df$TIME))
        invalid_time_values <- sum(present_time & is.na(parsed))
        time_hms <- hms::as_hms(parsed)
        hms::as_hms(round(as.numeric(time_hms)))
      }, error = function(e) {
        file_status(list(type = "danger", message = "TIME 列无法识别，请使用 HH:MM:SS 格式。"))
        rep(hms::as_hms(NA_real_), nrow(df))
      })
    }
    
    incomplete_rows <- sum(rowSums(is.na(df)) > 0)
    status_messages <- c(sprintf("读取完成：%s 行、%s 列。", format(nrow(df), big.mark = ","), ncol(df)))
    status_type <- "success"

    if (nrow(df) >= 200000) {
      status_type <- "warning"
      status_messages <- c(status_messages, "数据量较大，首次绘图可能较慢。")
    }
    if (incomplete_rows > 0) {
      status_type <- "warning"
      status_messages <- c(status_messages, sprintf("发现 %s 行含空缺单元格，已按缺失值处理。", format(incomplete_rows, big.mark = ",")))
    }
    if (converted_invalid > 0) {
      status_type <- "warning"
      status_messages <- c(status_messages, sprintf("发现 %s 个非数字内容，已按缺失值处理。", format(converted_invalid, big.mark = ",")))
    }
    if (invalid_time_values > 0) {
      status_type <- "warning"
      status_messages <- c(status_messages, sprintf("发现 %s 个无效时间，已按缺失值处理。", format(invalid_time_values, big.mark = ",")))
    }
    file_status(list(type = status_type, message = paste(status_messages, collapse = " ")))

    updateSelectInput(session, "x_var", choices = names(df), selected = if ("TIME" %in% names(df)) "TIME")
    numeric_columns <- names(df)[vapply(df, is.numeric, logical(1))]
    y_choices <- setdiff(numeric_columns, "TIME")
    updateSelectInput(session, "y_var", choices = y_choices)
    
    df <- df[!duplicated(df$TIME), ]
    df
  })

  # 根据 X 类型动态显示时间截断输入框
  output$x_cutoff_ui <- renderUI({
    req(data_input(), input$x_var)
    df <- data_input()
    x_data <- df[[input$x_var]]
    if (input$x_var == "TIME" && inherits(x_data, "hms")) {
      min_time <- suppressWarnings(min(x_data, na.rm = TRUE))
      
      default_val <- tryCatch({
        secs <- as.numeric(min_time)
        posix_time <- as.POSIXct("1970-01-01", tz = "UTC") + secs
        format(posix_time, "%H:%M:%S")
      }, error = function(e) {
        showNotification("无法格式化时间为 %H:%M:%S", type = "error")
        as.character(min_time)
      })
      
      return(textInput("x_cutoff", "开始测试时间 (e.g., 17:03:00)", value = default_val))
      
    } else if (is.numeric(x_data)) {
      return(numericInput("x_cutoff", "Trim X < Value",
                          value = min(x_data, na.rm = TRUE),
                          min = min(x_data, na.rm = TRUE),
                          max = max(x_data, na.rm = TRUE)))
    } else {
      return(helpText("Only numeric or 'TIME' column can be trimmed."))
    }
  })
  
  filtered_data <- eventReactive(input$apply_filter, {
    df <- data_input()
    
    # 先做变量检查
    if (!(input$x_var %in% names(df)) || !(input$y_var %in% names(df))) {
      showNotification("选择的 X 或 Y 变量不存在于数据中", type = "error")
      return(NULL)
    }
    
    req(input$x_cutoff)
    x_data <- df[[input$x_var]]
    
    # 根据类型筛选
    if (input$x_var == "TIME" && inherits(x_data, "hms")) {
      cutoff_time <- tryCatch(
        hms::as_hms(input$x_cutoff),
        error = function(e) {
          showNotification("开始时间格式不正确，请按 HH:MM:SS 输入，例如 17:03:00。", type = "error")
          NULL
        }
      )
      if (is.null(cutoff_time)) return(NULL)
      df <- df %>% filter(.data[[input$x_var]] >= cutoff_time)
    } else if (is.numeric(x_data)) {
      df <- df %>% filter(.data[[input$x_var]] >= as.numeric(input$x_cutoff))
    }
    
    df
  })
  
 
  # 主图绘制（包含截断逻辑）
  output$xy_plot <- renderPlotly({
    req(filtered_data(), input$x_var, input$y_var)
    
    df <- filtered_data()
    x <- df[[input$x_var]]
    y <- df[[input$y_var]]
    
    df1 <- data.frame(x = x, y = y) %>%
      filter(!is.na(x), !is.na(y)) %>%
      arrange(x)
    
    validate(need(nrow(df1) > 0, "筛选后没有可绘制的有效数据，请调整开始时间或所选指标。"))
    
    df1 <- df1 %>%
      mutate(
        x_time = as.POSIXct("1970-01-01", tz = "UTC") + as.numeric(x)
      )
    
    # TIME 作为横轴时，从当前起始时间开始按样品间隔自动绘制标记线
    marker_times <- if (input$x_var == "TIME") {
      seq(
        from = min(df1$x_time),
        to = max(df1$x_time),
        by = input$interval_minutes * 60
      )
    } else {
      NULL
    }
    marker_shapes <- if (length(marker_times) > 0) {
      lapply(marker_times, function(marker_time) {
        list(
          type = "line",
          xref = "x", yref = "paper",
          x0 = marker_time, x1 = marker_time,
          y0 = 0, y1 = 1,
          line = list(color = "red", width = 1.5, dash = "dash")
        )
      })
    } else {
      list()
    }

    plot_ly(data = df1, x = ~x_time, y = ~y,
            type = "scatter", mode = "lines+markers", source = "sub_plot") %>%
      layout(
        title = paste(input$y_var, "vs", input$x_var),
        xaxis = list(title = input$x_var),
        yaxis = list(title = input$y_var),
        dragmode = "select",
        shapes = marker_shapes
      ) %>%
      event_register("plotly_selected")
     
  })
  
   output$filtered_info <- renderPrint({
    req(input$apply_filter)
    df <- filtered_data()
    cat("=== 截断后数据概览 ===\n")
    cat("总行数：", nrow(df), "\n")
    
    if ("TIME" %in% names(df) && inherits(df$TIME, "hms")) {
      cat("起始时间：", format(as.POSIXct("1970-01-01", tz = "UTC") + min(df$TIME, na.rm = TRUE), "%H:%M:%S"), "\n")
      cat("结束时间：", format(as.POSIXct("1970-01-01", tz = "UTC") + max(df$TIME, na.rm = TRUE), "%H:%M:%S"), "\n")
    } else if (is.numeric(df[[input$x_var]])) {
      cat("X范围：", min(df[[input$x_var]], na.rm = TRUE), "到", max(df[[input$x_var]], na.rm = TRUE), "\n")
    }
  })
output$selected_avg <- renderPrint({
     eventdata <- event_data("plotly_selected", source = "sub_plot")
     
     if (is.null(eventdata) || nrow(eventdata) == 0) {
       cat("⚠️ 尚未选中任何数据点。\n")
     } else {
       y_vals <- eventdata$y
       mean_val <- mean(y_vals, na.rm = TRUE)
       cat("✅ 选中区域的平均值为：", round(mean_val, 3), "\n")
       cat("选中点数量：", length(y_vals), "\n")
     }
   })
   
processed_segments <- reactiveVal(NULL)
   
observeEvent(input$calc_segments, {
  req(filtered_data(), input$interval_minutes, input$y_var)
  tryCatch(
    withProgress(message = "正在计算分段平均值", value = 0, {
      df <- filtered_data()
      incProgress(0.2, detail = "清理无效数据……")

      if (is.numeric(df$TIME)) {
        df$TIME <- as.POSIXct("1970-01-01", tz = "UTC") + df$TIME
      } else {
        df$TIME <- as.POSIXct(df$TIME)
      }

      df <- df %>%
        filter(!is.na(.data[[input$y_var]]), !is.na(TIME)) %>%
        arrange(TIME)

      if (nrow(df) == 0) {
        stop("所选指标没有可用于计算的有效数值")
      }

      df <- df %>%
        mutate(
          segment_id = as.integer(difftime(TIME, min(TIME), units = "secs")) %/% (input$interval_minutes * 60)
        )

      incProgress(0.4, detail = "计算各时间段……")
      result <- df %>%
        group_by(segment_id) %>%
        mutate(row_id = row_number(),
               n = n(),
               lower_cut = floor(n / 3),
               upper_cut = ceiling(n * 2 / 3)) %>%
        filter(row_id > lower_cut, row_id <= upper_cut) %>%
        summarise(
          start_time = min(TIME),
          end_time = max(TIME),
          mean_val = mean(.data[[input$y_var]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(start_time) %>%
        mutate(
          start_time = format(start_time, "%H:%M:%S"),
          end_time = format(end_time, "%H:%M:%S")
        )

      processed_segments(result)
      incProgress(0.4, detail = "计算完成")
    }),
    error = function(e) {
      processed_segments(NULL)
      showNotification(
        paste0("无法完成分析：", conditionMessage(e), "。请检查时间列和所选指标。"),
        type = "error",
        duration = NULL
      )
    }
  )
})

 output$segment_table <- renderTable({
     req(processed_segments())
   df <- processed_segments()
   df$segment_id <- df$segment_id+1
   df$segment_id <- as.integer(df$segment_id)
   df
   })

 output$download_segments <- downloadHandler(
   filename = function() {
     paste0("picarro_analysis_", format(Sys.Date(), "%Y%m%d"), ".csv")
   },
   content = function(file) {
     req(processed_segments())
     df <- processed_segments()
     df$segment_id <- as.integer(df$segment_id) + 1L
     write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
   }
 )
}

shinyApp(ui = ui, server = server)
