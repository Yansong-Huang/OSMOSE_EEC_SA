library(data.table)
library(ggplot2)

EE_LFI <- fread("5.elementary_effect/EE_LFI40_stats.csv")
EE_mean_TL <- fread("5.elementary_effect/EE_meanTL_stats.csv")
EE_mean_length <- fread("5.elementary_effect/EE_meanLength_stats.csv")


# 合并三个数据集并添加指标标签
EE_all <- rbindlist(list(
  EE_LFI[, indicator := "LFI"],
  EE_mean_TL[, indicator := "Mean TL"],
  EE_mean_length[, indicator := "Mean Length"]
), fill = TRUE)

# 统一参数类型分类（如果不同数据集分类规则一致）
EE_all[, param_type := fcase(
  grepl("^mortality\\.additional\\.(rate|larva\\.rate)", param_name), "Mortality",
  grepl("^(fisheries\\.rate\\.base|species\\.catchability)", param_name), "Fisheries",
  grepl("^(species\\.length2weight\\.condition\\.factor|species\\.k|species\\.l0|species\\.linf|species\\.maturity\\.size)", param_name), "Growth",
  grepl("^species\\.accessibility2fish", param_name), "Prey Field",
  grepl("^predation\\.predPrey\\.sizeRatio", param_name), "Predation",
  default = "Other"
)]

# 为每个指标计算最大mu_star值
max_vals <- EE_all[, .(max_mu = max(mu_star)), by = indicator]

# 创建标注标签和相对位置
label_types <- c("almost linear", "monotonic", "almost-monotonic", "non-monotonic")
y_multipliers <- c(0.05, 0.3, 0.75, 1.2)

# 对每个指标构造对应的标签和坐标
labels_df <- max_vals[, .(
  label = label_types,
  x = max_mu * 0.9,
  y = max_mu * 0.9 * y_multipliers
), by = indicator]

# 增加一个排序列
EE_all[, rank_mu := frank(-mu_star), by = indicator]

# 创建分面图
EE_plot <- ggplot(EE_all, aes(x = mu_star, y = sigma, color = param_type)) +
  geom_abline(slope = c(0.1, 0.5, 1), intercept = 0, 
              linetype = "dashed", color = "grey60", show.legend = FALSE) +
  geom_point(size = 2, alpha = 0.8) +
  # geom_text_repel 中只标注前 10 个
  ggrepel::geom_text_repel(
    data = EE_all[rank_mu <= 10],
    aes(label = param_name, x = mu_star, y = sigma, color = param_type),
    max.overlaps = 20,
    size = 3,
    box.padding = 0.3,
    force = 0.5,
    segment.color = "grey50",
    show.legend = FALSE
  )+
  # 添加分面专属标注
  geom_text(
    data = labels_df,
    aes(x = x, y = y, label = label),
    color = "grey40",
    size = 3, 
    inherit.aes = FALSE  # 不继承美学映射
  ) +
  facet_wrap(~indicator, scales = "free", ncol = 3) + # 自由刻度适应不同指标范围
  labs(
    x = expression(mu["*"]~""),
    y = expression(sigma~""),
    color = "Parameter Type"
  ) +
  theme_minimal(base_size = 12) +
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    strip.text = element_text(face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("figures/EE_combined.png", plot = EE_plot, width = 12, height = 5, dpi = 300)

##  μ* 条形图（影响强度排序）
# EE[, param_name := factor(param_name, levels = param_name[order(-mu_star)])]
# 
# ggplot(EE[order(-mu_star)][1:25],         # 先看前 25 个最敏感参数
#        aes(x = param_name, y = mu_star)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(
#     title = "Top-25 sensitive parameters",
#     x = NULL,
#     y = expression(mu["*"])
#   ) +
#   theme_minimal(base_size = 13)
