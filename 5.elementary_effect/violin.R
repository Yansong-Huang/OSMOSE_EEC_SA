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

# 基础小提琴图函数
gg_violin_mu_star <- function(data) {
  ggplot(data, aes(x = param_type, y = mu_star, fill = param_type)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    geom_jitter(width = 0.1, size = 1, alpha = 0.5) +  # 显示单个参数点
    facet_wrap(~indicator, scales = "free_y") +        # 每个指标一个分面，y轴独立
    scale_y_continuous(name = expression(mu^"*")) +
    xlab("Parameter Type") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# 绘图
p <- gg_violin_mu_star(dt)
p
