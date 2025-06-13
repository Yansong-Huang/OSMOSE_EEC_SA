library(data.table)
library(ggplot2)
library(viridis)  # 更美观的色带
library(stringr)
library(ggplot2)

plot_EE_heatmap <- function(
    EE_stats,
    value_col = "mu_star",
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    out_file = NULL,
    base_width = 8,
    row_height = 0.25,
    title = "Elementary Effects (μ*)"
) {
  stopifnot(value_col %in% names(EE_stats))
  
  heat_dt <- dcast(EE_stats, param_name ~ species, value.var = value_col)
  
  # 保持param_name因子顺序
  heat_dt[, param_name := factor(param_name, levels = levels(EE_stats$param_name))]
  setorder(heat_dt, param_name)
  
  heat_mat <- as.matrix(heat_dt[, -1, with = FALSE])
  rownames(heat_mat) <- heat_dt$param_name
  
  if (cluster_rows) {
    row_order <- hclust(dist(heat_mat))$order
    heat_mat <- heat_mat[row_order, , drop = FALSE]
  }
  if (cluster_cols) {
    col_order <- hclust(dist(t(heat_mat)))$order
    heat_mat <- heat_mat[, col_order, drop = FALSE]
  }
  
  heat_long <- as.data.table(as.table(heat_mat))
  setnames(heat_long, c("param_name", "species", "value"))
  
  # 保持y轴因子顺序
  heat_long[, param_name := factor(param_name, levels = levels(EE_stats$param_name))]
  
  n_param <- length(levels(heat_long$param_name))
  plot_height <- max(4, n_param * row_height)
  
  p <- ggplot(heat_long, aes(x = species, y = param_name, fill = value)) +
    geom_tile(color = "white", width = 0.9, height = 0.9) +
    scale_fill_gradient(name = value_col, low = "white", high = "black") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 9),
      panel.grid = element_blank()
    ) +
    labs(x = "Species", y = "Parameter", title = title)
  
  if (!is.null(out_file)) {
    ggsave(
      filename = out_file,
      plot = p,
      width = base_width,
      height = plot_height,
      units = "in",
      dpi = 300,
      limitsize = FALSE
    )
    message("✓ Heatmap saved to: ", out_file)
  } else {
    print(p)
  }
  
  invisible(p)
}

# ---- EE_stats 数据 ----
EE_stats <- fread("5.elementary_effect/biomass/EE_biomass_stats.csv")
# ---- EE_stats 是 data.table 且有 param_name 列 ----

# 假设 EE_stats 中 param_name 是参数名称（如 predation.xx.sp0）
# 提取 param_name 中的 sp 编号作为 sp_order（允许无 sp 的参数）
EE_stats[, sp_order := str_extract(param_name, "sp\\d+")]
EE_stats[, sp_order := as.integer(str_remove(sp_order, "sp"))]

# 按 sp_order 数值排序，NA（无 sp 的参数）排在最后
EE_stats[, sp_order_na := is.na(sp_order)]
setorder(EE_stats, sp_order_na, sp_order, param_name)
EE_stats[, sp_order_na := NULL]

# 现在 param_name 按排序好的顺序变成因子levels
EE_stats[, param_name := factor(param_name, levels = unique(param_name))]

# 可选：删除临时列
EE_stats[, c("sp_index", "sp_order", "is_na_sp") := NULL]

plot_EE_heatmap(
  EE_stats,
  value_col = "mu_star",
  cluster_rows = FALSE,  # 手动排序，不要再聚类
  cluster_cols = TRUE,   # 可选
  out_file = "5.elementary_effect/biomass/EE_heatmap_sorted_by_species.png",
  base_width = 10,
  row_height = 0.3,
  title = "Biomass Effects Sorted by Species"
)

