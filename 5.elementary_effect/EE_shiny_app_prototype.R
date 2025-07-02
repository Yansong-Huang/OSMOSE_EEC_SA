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

# 添加物种 ID 和营养级分组（根据 param_name 中的 spX 提取）
EE_all[, species_id := fifelse(grepl("sp\\d+", param_name),
                               as.integer(sub(".*sp(\\d+).*", "\\1", param_name)), NA_integer_)]

EE_all[, trophic_group := fcase(
  species_id %in% c(0, 5, 15), "High",
  species_id %in% c(3, 4, 6, 7, 8, 9, 10), "Medium",
  species_id %in% c(11, 12), "Low",
  species_id %in% c(13, 14), "Cephalopod",
  species_id %in% c(16:26), "Resource",
  is.na(species_id), "Unspecified",
  default = "Unspecified"
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
      selectInput(
        "color_by",
        "Color Points By:",
        choices = c("Parameter Type" = "param_type", "Trophic Group" = "trophic_group"),
        selected = "param_type"
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
  
  scale_mode <- eventReactive(input$update_btn, {
    input$scale_mode
  })
  
  color_by <- eventReactive(input$update_btn, {
    input$color_by
  })
  
  param_colors <- c(
    "Fisheries"   = "#E41A1C",
    "Mortality"   = "#4DAF4A",
    "Growth"      = "#377EB8",
    "Prey Field"  = "#E6B800",
    "Predation"   = "#984EA3",
    "Other"       = "grey70"
  )
  
  trophic_colors <- c(
    "High"        = "#E41A1C",
    "Medium"      = "#377EB8",
    "Low"         = "#4DAF4A",
    "Cephalopod"  = "#984EA3",
    "Resource"    = "#E6B800",
    "Unspecified" = "grey70"
  )
  
  output$eePlot <- renderPlot({
    dt <- filtered_data()
    color_col <- color_by()
    
    line_data <- data.table(mu_star = range(dt$mu_star, na.rm = TRUE))
    line_data[, sigma := mu_star]
    
    top_labels <- dt %>% arrange(desc(mu_star)) %>% head(10)
    
    p <- ggplot(dt, aes(x = mu_star, y = sigma, color = .data[[color_col]])) +
      geom_line(data = line_data, aes(x = mu_star, y = sigma), 
                inherit.aes = FALSE, linetype = "dashed", color = "grey60") +
      geom_point(size = 2, alpha = 0.6) +
      geom_text_repel(data = top_labels, aes(label = param_name),
                      size = 3, max.overlaps = 50, show.legend = FALSE) +
      labs(
        x = "mu*",
        y = "sigma",
        color = if (color_col == "param_type") "Parameter Type" else "Trophic Group"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.margin = margin(5, 20, 5, 5),
        plot.title.position = "plot"
      )
    
    if (scale_mode() == "log") {
      p <- p + scale_x_log10(labels = scales::label_number()) +
        scale_y_log10(labels = scales::label_number())
    }
    
    if (color_col == "param_type") {
      p <- p + scale_color_manual(values = param_colors)
    } else {
      p <- p + scale_color_manual(values = trophic_colors)
    }
    
    p
  })
  
  output$click_info <- renderText({
    req(input$plot_click)
    dt <- filtered_data()
    log_mode <- scale_mode() == "log"
    
    dist <- if (log_mode) {
      sqrt((log10(dt$mu_star) - log10(input$plot_click$x))^2 + 
             (log10(dt$sigma) - log10(input$plot_click$y))^2)
    } else {
      sqrt((dt$mu_star - input$plot_click$x)^2 + 
             (dt$sigma - input$plot_click$y)^2)
    }
    
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