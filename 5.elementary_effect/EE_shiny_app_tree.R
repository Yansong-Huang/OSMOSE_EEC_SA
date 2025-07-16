library(shiny)
library(ggplot2)
library(ggdendro)
library(data.table)
library(dplyr)

# ---------- 配置颜色 ----------
param_colors <- c(
  "Fisheries"   = "#E41A1C",
  "Mortality"   = "#4DAF4A",
  "Growth"      = "#377EB8",
  "PreyField"   = "#E6B800",
  "Predation"   = "#984EA3",
  "Other"       = "grey70"
)

# ---------- 读取数据 ----------
row_hclust <- readRDS("5.elementary_effect/EE_outputs/row_hclust_wardD.rds")
param_map <- fread("5.elementary_effect/param_name_map.csv")

# ---------- 准备 dendrogram 数据 ----------
dend <- as.dendrogram(row_hclust)
dend_data <- dendro_data(dend, type = "rectangle")

# ---------- 将参数类型合并到 label 上 ----------
label_df <- data.frame(label = row_hclust$labels)
label_df <- left_join(label_df, param_map, by = c("label" = "param_label"))

# 替换 label 显示为 param_label
label_map <- setNames(label_df$param_type, row_hclust$labels)
dend_data$labels$param_type <- label_map[dend_data$labels$label]

# ---------- Shiny app ----------
ui <- fluidPage(
  titlePanel("Parameter tree of EE on biomass by species (ward.D method)"),
  plotOutput("dend_plot", height = "1200px")
)

server <- function(input, output, session) {
  output$dend_plot <- renderPlot({
    ggplot() +
      geom_segment(data = dend_data$segments,
                   aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_text(data = dend_data$labels,
                aes(x = x, y = y - 0.02, label = label, color = param_type),
                angle = 90, hjust = 1, size = 2.5) +
      scale_color_manual(values = param_colors, na.value = "grey50") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank()) +
      labs(title = element_blank())
  })
}

shinyApp(ui, server)
