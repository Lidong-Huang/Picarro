library(shiny)
library(plotly)
library(readr)
library(dplyr)
library(hms)

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
  
  # 自动解析可能是时间格式的列
  data_input <- reactive({
    req(input$file)
    df <- read.table(input$file$datapath, header = TRUE, stringsAsFactors = FALSE)
    
    # ✅ 如果有 TIME 列，转换为整数秒的 hms 类型
    if ("TIME" %in% names(df) && is.character(df$TIME)) {
      df$TIME <- tryCatch({
        parsed <- strptime(df$TIME, format = "%H:%M:%OS")
        if (any(is.na(parsed))) stop("TIME 格式错误")
        time_hms <- hms::as_hms(parsed)
        hms::as_hms(round(as.numeric(time_hms)))  # 👈 四舍五入到整数秒
      }, error = function(e) {
        showNotification("TIME 列无法识别为时间格式", type = "error")
        df$TIME
      })
    }
    
    updateSelectInput(session, "x_var", choices = names(df))
    updateSelectInput(session, "y_var", choices = names(df))
    
    df<-  df[!duplicated(df$TIME), ]
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
      cutoff_time <- hms::as_hms(input$x_cutoff)
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
    
    req(nrow(df1) > 0)
    
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
      )
     
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
  df <- filtered_data()
  
  # 👉 TIME 类型转换
  if (is.numeric(df$TIME)) {
    df$TIME <- as.POSIXct("1970-01-01", tz = "UTC") + df$TIME
  } else {
    df$TIME <- as.POSIXct(df$TIME)
  }
  
  df <- df %>%
    filter(!is.na(.data[[input$y_var]]), !is.na(TIME)) %>%
    arrange(TIME) %>%
    mutate(
      segment_id = as.integer(difftime(TIME, min(TIME), units = "secs")) %/% (input$interval_minutes * 60)
      )

  result <- df %>%
    group_by(segment_id) %>%
    mutate(row_id = row_number(), 
           n = n(),
           lower_cut = ceiling(n / 3),
           upper_cut = floor(n * 2 / 3)) %>%
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
