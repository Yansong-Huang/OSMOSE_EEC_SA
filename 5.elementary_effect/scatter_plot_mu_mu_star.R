library(data.table)
library(ggplot2)

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


# 生成 mu / mu* 散点图
gg_scatter_mu_ratio <- function(data) {
  # 先计算比值
  data[, mu_ratio := mu / mu_star]
  
  ggplot(data, aes(x = mu_star, y = mu_ratio, color = param_type)) +
    geom_point(alpha = 0.7, size = 2) +
    facet_wrap(~indicator, scales = "free_y") +   # 每个指标独立 y 轴
    scale_x_log10(name = expression(mu^"*")) +    # 横轴对数坐标
    scale_y_log10(name = expression(mu / mu^"*")) + # 纵轴对数坐标也可以
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank()
    )
}

gg_scatter_mu_ratio(dt)
