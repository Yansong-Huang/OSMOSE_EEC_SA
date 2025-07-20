library(shiny)
library(data.table)
library(ggplot2)
library(scales)
library(dplyr)
library(ggforce)
library(ggrepel)
library(viridis)

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

# ---------- 合并 param_type 和 param_label ----------
mapping <- fread("param_name_map.csv")
EE_all <- merge(EE_all, mapping, by = "param_name", all.x = TRUE)

# ---------- 添加物种 ID 和营养级分组 ----------
EE_all[, species_id := fifelse(grepl("sp\\d+", param_name),
                               as.integer(sub(".*sp(\\d+).*", "\\1", param_name)), NA_integer_)]

EE_all[, trophic_group := fcase(
  species_id %in% c(0, 5, 15), "High",
  species_id %in% c(1, 2, 3, 4, 6, 7, 8, 9, 10), "Medium",
  species_id %in% c(11, 12), "Low",
  species_id %in% c(13, 14), "Cephalopod",
  species_id %in% c(16:26), "Resource",
  is.na(species_id), "Unspecified",
  default = "Unspecified"
)]

EE_all[, param_species_plot := fifelse(is.na(param_species), "other", param_species)]

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
        choices = c("Parameter Type" = "param_type", 
                    "Trophic Group" = "trophic_group", 
                    "Species" = "param_species"),
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
    "PreyField"   = "#E6B800",
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
  
  species_list <- unique(na.omit(EE_all$param_species))
  # 指定灰色的组
  gray_labels <- c("resource", "unspecified")
  
  # 将灰色组排在前面，其余组进行调色
  non_gray_labels <- setdiff(species_list, gray_labels)
  n_colors <- length(non_gray_labels)
  palette_colors <- viridis(n_colors)
  
  # 全部标签的颜色映射（灰色 + 调色板）
  species_colors <- setNames(
    c(rep("#B0B0B0", length(gray_labels)), palette_colors),
    c(gray_labels, non_gray_labels)
  )

  
  output$eePlot <- renderPlot({
    dt <- filtered_data()
    color_col <- color_by()
    
    if (color_col == "param_species") {
      dt[, param_species_plot := fifelse(is.na(param_species), "other", param_species)]
    }

    
    # 假设 color_col 是分组变量（如 param_species）
    # 提取凸包和重心
    hull_data <- dt %>%
      filter(!is.na(.data[[color_col]])) %>%
      group_by(group = .data[[color_col]]) %>%
      filter(n() >= 3) %>%  # 至少3个点才能构成凸包
      slice(chull(mu_star, sigma)) %>%
      ungroup()
    
    centroid_data <- dt %>%
      filter(!is.na(.data[[color_col]])) %>%
      group_by(group = .data[[color_col]]) %>%
      summarise(
        mu_star = mean(mu_star),
        sigma = mean(sigma),
        .groups = "drop"
      )
    
    
    line_data <- data.table(mu_star = range(dt$mu_star, na.rm = TRUE))
    line_data[, sigma := mu_star]
    
    top_labels <- dt %>% arrange(desc(mu_star)) %>% head(10)
    
    p <- ggplot(dt, aes(
      x = mu_star, y = sigma, 
      color = if (color_col == "param_species") param_species_plot else .data[[color_col]])) +
      geom_line(data = line_data, aes(x = mu_star, y = sigma), 
                inherit.aes = FALSE, linetype = "dashed", color = "grey60") +
      geom_point(size = 2, alpha = 0.6) +
      geom_text_repel(
        data = top_labels,
        aes(label = ifelse(is.na(param_label), param_name, param_label)),
        size = 3,
        max.overlaps = 50,
        show.legend = FALSE
      ) +
      labs(
        x = "mu*",
        y = "sigma",
        color = switch(color_col,
                       "param_type" = "Parameter Type",
                       "trophic_group" = "Trophic Group",
                       "param_species" = "Species")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.margin = margin(5, 20, 5, 5),
        plot.title.position = "plot"
      )
    
    
    # 添加到现有 ggplot 中
    
    p <- p +
      geom_polygon(
        data = hull_data,
        aes(x = mu_star, y = sigma, group = group, fill = group),
        alpha = 0.15, color = NA, inherit.aes = FALSE, show.legend = FALSE
      ) +
      geom_point(
        data = centroid_data,
        aes(x = mu_star, y = sigma),
        shape = 21, fill = "white", size = 2.5, stroke = 0.5, color = "black", inherit.aes = FALSE
      ) +
      geom_text_repel(
        data = centroid_data,
        aes(x = mu_star, y = sigma, label = group),
        size = 3, inherit.aes = FALSE, max.overlaps = 50
      )
    
    if (scale_mode() == "log") {
      p <- p + scale_x_log10(labels = label_number()) +
        scale_y_log10(labels = label_number())
    }
    
    if (color_col == "param_type") {
      p <- p + scale_color_manual(values = param_colors) + scale_fill_manual(values = param_colors)
    } else if (color_col == "trophic_group") {
      p <- p + scale_color_manual(values = trophic_colors) + scale_fill_manual(values = trophic_colors)
    } else if (color_col == "param_species") {
      p <- p + scale_color_manual(values = species_colors) + scale_fill_manual(values = species_colors) 
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
    
    paste0("Parameter: ", ifelse(is.na(clicked$param_label), clicked$param_name, clicked$param_label), 
           "\nmu*: ", round(clicked$mu_star, 2),
           "\nsigma: ", round(clicked$sigma, 2),
           "\nType: ", clicked$param_type)
  })
}

# ---------- Run App ----------
shinyApp(ui, server)
