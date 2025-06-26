library(data.table)
library(ggplot2)
library(ggrepel)
library(scales)

# ---- 读取数据 ----
EE_biomass <- fread("5.elementary_effect/EE_outputs/EE_biomass_total_biomass.csv")
EE_yield   <- fread("5.elementary_effect/EE_outputs/EE_yield_total_yield.csv")

# ---- 添加 indicator 字段 ----
EE_biomass[, indicator := "Total Biomass"]
EE_yield[,   indicator := "Total Yield"]

# ---- 合并数据 ----
EE_all <- rbindlist(list(EE_biomass, EE_yield), fill = TRUE)

# ---- 添加参数类型分类 ----
EE_all[, param_type := fcase(
  grepl("^mortality\\.additional\\.(rate|larva\\.rate)", param_name), "Mortality",
  grepl("^(fisheries\\.rate\\.base|species\\.catchability)", param_name), "Fisheries",
  grepl("^(species\\.length2weight\\.condition\\.factor|species\\.k|species\\.l0|species\\.linf|species\\.maturity\\.size)", param_name), "Growth",
  grepl("^species\\.accessibility2fish", param_name), "Prey Field",
  grepl("^predation\\.predPrey\\.sizeRatio", param_name), "Predation",
  default = "Other"
)]

# ---- 排除 "Other" 类 ----
EE_all <- EE_all[param_type != "Other"]

# ---- 计算每个面板最大 mu_star，用于放标签 ----
max_vals <- EE_all[, .(max_mu = max(mu_star, na.rm = TRUE)), by = indicator]

# 标签位置（参考 Sanchez 等）
label_types <- c("almost linear", "monotonic", "almost-monotonic", "non-monotonic")
y_multipliers <- c(0.05, 0.3, 0.75, 1.2)
labels_df <- max_vals[, .(
  label = label_types,
  x = max_mu * 0.9,
  y = max_mu * 0.9 * y_multipliers
), by = indicator]

# ---- 排名，用于标注前 10 大和后 5 小 ----
EE_all[, rank_mu := frank(-mu_star), by = indicator]
# EE_all[, rank_mu_min := frank(mu_star), by = indicator]     # 最小效应

# ---- 绘图 ----
EE_combined_plot <- ggplot(EE_all, aes(x = mu_star, y = sigma, color = param_type)) +
  geom_abline(slope = c(0.1, 0.5, 1), intercept = 0, 
              linetype = "dashed", color = "grey60", show.legend = FALSE) +
  geom_point(size = 2, alpha = 0.6) +
  ggrepel::geom_text_repel(
    data = EE_all[rank_mu <= 10],
    aes(label = param_name),
    max.overlaps = 20,
    size = 3,
    box.padding = 0.3,
    force = 0.5,
    segment.color = "grey50",
    show.legend = FALSE
  ) +
  geom_text(
    data = labels_df,
    aes(x = x, y = y, label = label),
    color = "grey40",
    size = 3,
    inherit.aes = FALSE
  ) +
  facet_wrap(~indicator, scales = "free", ncol = 2) +
  labs(
    x = expression(mu["*"]),
    y = expression(sigma),
    color = "Parameter Type",
    # title = "Elementary Effects on Total Biomass and Yield"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    strip.text = element_text(face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# ---- 保存图形 ----
ggsave("figures/EE_biomass_yield_combined.png", plot = EE_combined_plot, width = 12, height = 5.5, dpi = 300)
