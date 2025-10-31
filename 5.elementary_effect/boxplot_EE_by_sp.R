library(data.table)
library(ggplot2)
library(colorspace)
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


# 确保是 data.table 或 data.frame
dt <- as.data.table(EE_all)

species_list <- unique(na.omit(EE_all$param_species))
gray_labels <- c("resource", "fleet")
non_gray_labels <- setdiff(species_list, gray_labels)
n_colors <- length(non_gray_labels)
palette_colors <- rainbow_hcl(n_colors)  # 高亮对比、色相均匀分布


species_colors <- setNames(
  c(rep("#B0B0B0", length(gray_labels)), palette_colors),
  c(gray_labels, non_gray_labels)
)

gg_box_mu_star <- function(data) {
  ggplot(data, aes(x = param_species, y = mu_star, fill = param_species)) +
    geom_boxplot(alpha = 0.7, outlier.size = 1) +   # 箱形图
    # geom_jitter(width = 0.1, size = 1, alpha = 0.5) + # 显示单个参数点
    facet_wrap(~indicator, scales = "free_y") +     # 每个指标一个分面，y轴独立
    scale_y_log10(name = expression(mu^"*")) +      # 对数坐标
    scale_fill_manual(values = species_colors) +      # 自定义颜色方案
    xlab("Species") +
    theme_bw(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"                       # 不显示图例
    )
}

# 绘图
p <- gg_box_mu_star(dt)
p