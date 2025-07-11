library(shiny)
library(data.table)
library(ggplot2)
library(viridis)
library(plotly)
library(stringr)

# ---------- 读取数据 ----------
EE_stats <- fread("EE_outputs/EE_biomass_by_species_stats.csv")

# 将 species 设为因子并固定顺序（横坐标）
species_all <- paste0("sp", 0:15)
EE_stats[, species := factor(species, levels = species_all)]

# 提取 param_name 中的 sp编号，做为参数纵轴排序辅助
EE_stats[, sp_order := str_extract(param_name, "sp\\d+")]
EE_stats[, sp_order := as.integer(str_remove(sp_order, "sp"))]

# 排序参数：无 sp 的参数排后面
EE_stats[, sp_order_na := is.na(sp_order)]
setorder(EE_stats, sp_order_na, sp_order, param_name)
EE_stats[, sp_order_na := NULL]

# 按排序结果重新定义 param_name 因子顺序（纵坐标）
EE_stats[, param_name := factor(param_name, levels = unique(param_name))]

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("Elementary Effects Heatmap (Biomass)"),
  sidebarLayout(
    sidebarPanel(
      width = 12, 
      selectInput(
        "value_col",
        "Select Effect Metric:",
        choices = c("mu_star", "mu", "sigma"),
        selected = "mu_star"
      )
    ),
    mainPanel(
      width = 12,
      plotlyOutput("heatmap", height = "1800px")
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  output$heatmap <- renderPlotly({
    heat_dt <- copy(EE_stats)
    heat_dt[, value := get(input$value_col)]
    
    heat_dt[, tooltip := paste0(
      "Species: ", species,
      "<br>Parameter: ", param_name,
      "<br>", input$value_col, ": ", round(value, 2)
    )]
    
    # 自定义图例刻度断点（覆盖默认）
    legend_breaks <- c(0, 10, 100, 1000, 10000, 100000, 500000)
    legend_labels <- scales::label_number(accuracy = 1)(legend_breaks)
    
    # 色带设定
    fill_scale <- if (input$value_col %in% c("mu_star", "sigma")) {
      scale_fill_viridis(
        option = "C",
        trans = "log1p",
        na.value = "grey90",
        name = input$value_col,
        breaks = legend_breaks,
        labels = legend_labels
      )
    } else {
      scale_fill_gradient2(
        low = "blue", high = "red", mid = "white",
        midpoint = 0,
        trans = "pseudo_log",
        na.value = "grey90",
        name = input$value_col,
        labels = scales::label_number(accuracy = 1)
      )
    }
    
    p <- ggplot(heat_dt, aes(x = species, y = param_name, fill = value, text = tooltip)) +
      geom_tile(color = "white", width = 0.9, height = 0.9) +
      fill_scale +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 60, r = 20, b = 80, t = 30))
  })
}

# ---------- Run ----------
shinyApp(ui, server)