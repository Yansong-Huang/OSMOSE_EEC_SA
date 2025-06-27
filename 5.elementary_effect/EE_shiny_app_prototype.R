library(shiny)
library(data.table)
library(ggplot2)
library(scales)

# 读取数据，只用生物量
EE_biomass <- fread("EE_outputs/EE_biomass_total_biomass.csv")

# 添加参数类型分类
EE_biomass[, param_type := fcase(
  grepl("^mortality\\.additional\\.(rate|larva\\.rate)", param_name), "Mortality",
  grepl("^(fisheries\\.rate\\.base|species\\.catchability)", param_name), "Fisheries",
  grepl("^(species\\.length2weight\\.condition\\.factor|species\\.k|species\\.l0|species\\.linf|species\\.maturity\\.size)", param_name), "Growth",
  grepl("^species\\.accessibility2fish", param_name), "Prey Field",
  grepl("^predation\\.predPrey\\.sizeRatio", param_name), "Predation",
  default = "Other"
)]

ui <- fluidPage(
  titlePanel("Elementary Effects on Total Biomass"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "selected_types",
        "Select Parameter Types to Display:",
        choices = unique(EE_biomass$param_type),
        selected = unique(EE_biomass$param_type)
      ),
      checkboxGroupInput(
        "log_axes", 
        "Log-transform axes:", 
        choices = c("mu_star", "sigma"), 
        selected = NULL, 
        inline = TRUE
      ),
      actionButton("update_btn", "Confirm")
    ),
    mainPanel(
      plotOutput("eePlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  selected_types <- eventReactive(input$update_btn, {
    input$selected_types
  })
  
  log_axes <- eventReactive(input$update_btn, {
    input$log_axes
  })
  
  filtered_data <- reactive({
    EE_biomass[param_type %in% selected_types()]
  })
  
  param_colors <- c(
    "Fisheries"   = "#E41A1C",  # 红色（Set1[1]）
    "Mortality"   = "#4DAF4A",  # 绿色（Set1[3]）
    "Growth"      = "#377EB8",  # 蓝色（Set1[2]）
    "Prey Field"  = "#FFFF33",  # 黄色（Set1[6]）
    "Predation"   = "#984EA3",  # 紫色（Set1[4]）
    "Other"       = "grey70"    # 可选：灰色
  )
  
  output$eePlot <- renderPlot({
    dt <- filtered_data()
    
    p <- ggplot(dt, aes(x = mu_star, y = sigma, color = param_type)) +
      geom_abline(slope = c(0.1, 0.5, 1), intercept = 0, 
                  linetype = "dashed", color = "grey60", show.legend = FALSE) +
      geom_point(size = 2, alpha = 0.6) +
      scale_color_manual(values = param_colors) +
      labs(
        x = expression(mu["*"]),
        y = expression(sigma),
        color = "Parameter Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
    if ("mu_star" %in% log_axes()) {
      p <- p + scale_x_log10(labels = scales::label_number())
    }
    if ("sigma" %in% log_axes()) {
      p <- p + scale_y_log10(labels = scales::label_number())
    }
    
    p
  })
}

shinyApp(ui, server)
