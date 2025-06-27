library(shiny)
library(data.table)
library(ggplot2)
library(scales)
library(dplyr)
library(ggrepel)

# ---------- 读取并合并五类指标 ----------
read_add_indicator <- function(path, indicator_name) {
  dt <- fread(path)
  dt[, indicator := indicator_name]
  return(dt)
}

EE_all <- rbindlist(list(
  read_add_indicator("EE_outputs/EE_biomass_total_biomass.csv",    "Total Biomass"),
  read_add_indicator("EE_outputs/EE_yield_total_yield.csv",        "Total Yield"),
  read_add_indicator("EE_outputs/EE_LFI40_stats.csv",              "LFI40"),
  read_add_indicator("EE_outputs/EE_meanTL_stats.csv",             "Mean Trophic Level"),
  read_add_indicator("EE_outputs/EE_meanLength_stats.csv",         "Mean Length")
), use.names = TRUE, fill = TRUE)

# 添加参数类型分类
EE_all[, param_type := fcase(
  grepl("^mortality\\.additional\\.(rate|larva\\.rate)", param_name), "Mortality",
  grepl("^(fisheries\\.rate\\.base|species\\.catchability)", param_name), "Fisheries",
  grepl("^(species\\.length2weight\\.condition\\.factor|species\\.k|species\\.l0|species\\.linf|species\\.maturity\\.size)", param_name), "Growth",
  grepl("^species\\.accessibility2fish", param_name), "Prey Field",
  grepl("^predation\\.predPrey\\.sizeRatio", param_name), "Predation",
  default = "Other"
)]

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("Elementary Effects on Selected Indicators"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_indicator",
        "Select Indicator:",
        choices = unique(EE_all$indicator),
        selected = "Total Biomass"
      ),
      checkboxGroupInput(
        "selected_types",
        "Select Parameter Types to Display:",
        choices = unique(EE_all$param_type),
        selected = unique(EE_all$param_type)
      ),
      radioButtons(
        "scale_mode", 
        "Coordinate Scale:", 
        choices = c("Linear" = "linear", "Log-Log" = "log"),
        selected = "linear",
        inline = TRUE
      ),
      actionButton("update_btn", "Confirm")
    ),
    mainPanel(
      verbatimTextOutput("click_info"),
      plotOutput("eePlot", height = "500px", click = "plot_click")
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  filtered_data <- eventReactive(input$update_btn, {
    req(input$selected_types, input$selected_indicator)
    EE_all[indicator == input$selected_indicator & param_type %in% input$selected_types]
  })
  
  param_colors <- c(
    "Fisheries"   = "#E41A1C",  # 红色（Set1[1]）
    "Mortality"   = "#4DAF4A",  # 绿色（Set1[3]）
    "Growth"      = "#377EB8",  # 蓝色（Set1[2]）
    "Prey Field"  = "#FFFF10",  # 黄色（Set1[6]）
    "Predation"   = "#984EA3",  # 紫色（Set1[4]）
    "Other"       = "grey70"    # 灰色
  )
  
  output$eePlot <- renderPlot({
    dt <- filtered_data()
    
    # 创建参考线数据（y = x）
    line_data <- data.table(mu_star = range(dt$mu_star, na.rm = TRUE))
    line_data[, sigma := mu_star]
    
    # 获取 mu_star 前十的参数用于标签
    top_labels <- dt %>% arrange(desc(mu_star)) %>% head(10)
    
    p <- ggplot(dt, aes(x = mu_star, y = sigma, color = param_type)) +
      geom_line(data = line_data, aes(x = mu_star, y = sigma), 
                inherit.aes = FALSE, linetype = "dashed", color = "grey60") +
      geom_point(size = 2, alpha = 0.6) +
      geom_text_repel(data = top_labels, aes(label = param_name),
                      size = 3, max.overlaps = 50, show.legend = FALSE) +
      scale_color_manual(values = param_colors) +
      labs(
        x = "mu*",
        y = "sigma",
        color = "Parameter Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.margin = margin(5, 20, 5, 5),
        plot.title.position = "plot"
      )
    
    # 应用坐标缩放
    if (input$scale_mode == "log") {
      p <- p + scale_x_log10(labels = scales::label_number()) +
        scale_y_log10(labels = scales::label_number())
    }
    
    p
  })
  
  output$click_info <- renderPrint({
    req(input$plot_click)
    dt <- filtered_data()
    dist <- sqrt((log10(dt$mu_star) - log10(input$plot_click$x))^2 + 
                   (log10(dt$sigma) - log10(input$plot_click$y))^2)
    near_idx <- which.min(dist)
    clicked <- dt[near_idx]
    paste0("Parameter: ", clicked$param_name, 
           "\nmu*: ", round(clicked$mu_star, 2),
           "\nsigma: ", round(clicked$sigma, 2),
           "\nType: ", clicked$param_type)
  })
}

# ---------- Run App ----------
shinyApp(ui, server)
