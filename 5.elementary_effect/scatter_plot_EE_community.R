# ---------- 加载包 ----------
library(data.table)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(viridis)
library(patchwork)

# ---------- 数据读取 ----------
read_add_indicator <- function(path, indicator_name) {
  dt <- fread(path)
  dt[, indicator := indicator_name]
  return(dt)
}

EE_all <- rbindlist(list(
  read_add_indicator("5.elementary_effect/EE_outputs/EE_biomass_total_biomass.csv",    "Total Biomass"),
  read_add_indicator("5.elementary_effect/EE_outputs/EE_yield_total_yield.csv",        "Total Yield"),
  read_add_indicator("5.elementary_effect/EE_outputs/EE_LFI40_stats.csv",              "LFI40"),
  read_add_indicator("5.elementary_effect/EE_outputs/EE_meanLength_stats.csv",         "Mean Length"),
  read_add_indicator("5.elementary_effect/EE_outputs/EE_meanTL_stats.csv",             "Mean Trophic Level")
), use.names = TRUE, fill = TRUE)

# ---------- 合并 param_type 和 param_label ----------
mapping <- fread("5.elementary_effect/param_name_map.csv")
EE_all <- merge(EE_all, mapping, by = "param_name", all.x = TRUE)

# ---------- 添加物种 ID 和营养级分组 ----------
EE_all[, species_id := fifelse(grepl("sp\\d+", param_name),
                               as.integer(sub(".*sp(\\d+).*", "\\1", param_name)), NA_integer_)]

EE_all[, trophic_group := fcase(
  species_id %in% c(0, 5, 15), "High",
  species_id %in% c(1,2,3,4,6,7,8,9,10,13,14), "Medium",
  species_id %in% c(11,12), "Low",
  species_id %in% c(16:26), "Resource",
  is.na(species_id), "Fleet",
  default = "Unspecified"
)]

EE_all[, param_species_plot := fifelse(is.na(param_species), "other", param_species)]

# ---------- 颜色方案 ----------
param_colors <- c(
  "Fisheries"   = "#E41A1C",
  "Mortality"   = "#4DAF4A",
  "Growth"      = "#377EB8",
  "PreyField"   = "#E6B800",
  "Predation"   = "#984EA3",
  "Other"       = "grey70"
)

species_list <- unique(na.omit(EE_all$param_species))
gray_labels <- c("resource", "fleet")
non_gray_labels <- setdiff(species_list, gray_labels)
n_colors <- length(non_gray_labels)
palette_colors <- viridis(n_colors)

species_colors <- setNames(
  c(rep("#B0B0B0", length(gray_labels)), palette_colors),
  c(gray_labels, non_gray_labels)
)

# ---------- 作图函数 ----------
make_one_plot <- function(dt, indicator_name, color_col, color_scale) {
  subdt <- dt[indicator == indicator_name]
  
  # 前十个参数
  top_labels <- subdt %>%
    arrange(desc(mu_star)) %>%
    head(10)
  
  # 添加凸包和重心
  hull_data <- subdt %>%
    filter(!is.na(.data[[color_col]])) %>%
    group_by(group = .data[[color_col]]) %>%
    filter(n() >= 3) %>%
    slice(chull(mu_star, sigma)) %>%
    ungroup()
  
  centroid_data <- subdt %>%
    filter(!is.na(.data[[color_col]])) %>%
    group_by(group = .data[[color_col]]) %>%
    summarise(mu_star = mean(mu_star), sigma = mean(sigma), .groups = "drop")
  
  ggplot(subdt, aes(x = mu_star, y = sigma, color = .data[[color_col]])) +
    geom_point(size = 2, alpha = 0.6) +
    geom_polygon(data = hull_data,
                 aes(x = mu_star, y = sigma, group = group, fill = group),
                 alpha = 0.15, color = NA, inherit.aes = FALSE, show.legend = FALSE) +
    geom_point(data = centroid_data,
               aes(x = mu_star, y = sigma),
               shape = 21, fill = "white", size = 2.5, stroke = 0.5,
               color = "black", inherit.aes = FALSE) +
    geom_text_repel(data = centroid_data,
                    aes(x = mu_star, y = sigma, label = group),
                    size = 3, inherit.aes = FALSE, max.overlaps = 50) +
    geom_text_repel(
      data = top_labels,
      aes(x = mu_star, y = sigma,
          label = ifelse(is.na(param_label), param_name, param_label),
          color = .data[[color_col]]),
      size = 3,
      inherit.aes = FALSE,
      max.overlaps = 15,    # 不要太大
      box.padding = 0.5,    # 文字周围留白
      point.padding = 0.3,  # 点和文字间距
      force = 2.5             # 排斥力
    )+
    scale_x_log10() +
    scale_y_log10() +
    scale_color_manual(values = color_scale) +
    scale_fill_manual(values = color_scale) +
    labs(x = "mu*", y = "sigma", title = indicator_name, color = color_col) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}


plot_indicator_panels <- function(dt, color_col, color_scale, outfile) {
  indicators <- c("Total Biomass", "Total Yield", "Mean Trophic Level", "LFI40", "Mean Length")
  
  plots <- lapply(indicators, function(ind) {
    make_one_plot(dt, ind, color_col, color_scale) +
      theme(legend.position = "bottom")  # 先保留，后面 collect
  })
  
  fig <- wrap_plots(plots, ncol = 3, guides = "collect") +
    plot_layout(guides = "collect") +
    plot_annotation(title = paste("Elementary Effects colored by", color_col)) &
    theme(legend.position = "right")
  
  ggsave(outfile, fig, width = 13, height = 8, dpi = 300)  # 只输出 PNG
}

# ---------- 生成两张复合图 ----------
plot_indicator_panels(EE_all, "param_type",   param_colors,   "figures/EE_panels_by_param_type.png")
plot_indicator_panels(EE_all, "param_species_plot", species_colors, "figures/EE_panels_by_species.png")