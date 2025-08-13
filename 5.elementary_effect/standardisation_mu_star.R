library(dplyr)
library(tidyr)
library(readr)

#-----------------------------
# 1) 读取与合并 5 个指标文件
#-----------------------------
indicators <- c("biomass_rel", "mean_length", "LFI", "yield_rel", "mean_TL")

all_data <- bind_rows(lapply(indicators, function(ind) {
  read_csv(
    paste0("5.elementary_effect/EE_outputs/EE_", ind, "_by_species_stats.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(indicator = ind)
}))

# 保证核心列存在：param_name, species, mu_star
# 如果你的原始列名不同，请相应调整

#-----------------------------
# 2) 过滤特定物种（LFI / yield_rel 排除 sp4, sp6）
#-----------------------------
all_data <- all_data %>%
  filter(!(indicator %in% c("LFI", "yield_rel") & species %in% c("sp4", "sp6")))

#-----------------------------
# 3) 生成 参数 × 物种-指标组合 的 mu_star 矩阵
#    行 = param_name
#    列 = paste(species, indicator, sep = "_")
#-----------------------------
mu_star_mat <- all_data %>%
  mutate(species_indicator = paste(species, indicator, sep = "_")) %>%
  select(param_name, species_indicator, mu_star) %>%
  distinct() %>%  # 以防重复
  pivot_wider(names_from = species_indicator, values_from = mu_star) %>%
  arrange(param_name)

param_names <- mu_star_mat$param_name
mu_star_mat <- mu_star_mat %>% select(-param_name) %>% as.matrix()
rownames(mu_star_mat) <- param_names

#-----------------------------
# 4) 按“列”做 z-score（以物种-指标为单位）
#    - 先计算每列均值/标准差（忽略 NA）
#    - 去除零方差列
#    - z = (x - col_mean) / col_sd
#    - 剩余 NA 用 0 代替（即用该列均值的 z 值 0）
#-----------------------------
col_means <- colMeans(mu_star_mat, na.rm = TRUE)
col_sds   <- apply(mu_star_mat, 2, sd, na.rm = TRUE)

keep_cols <- which(is.finite(col_sds) & col_sds > 0)  # 去掉零方差列
mu_star_mat2 <- mu_star_mat[, keep_cols, drop = FALSE]
col_means <- col_means[keep_cols]
col_sds   <- col_sds[keep_cols]

# z-score 按列
mu_star_scaled <- sweep(mu_star_mat2, 2, col_means, FUN = "-")
mu_star_scaled <- sweep(mu_star_scaled, 2, col_sds,   FUN = "/")

# 用 0 填补 NA（等于该列的“均值水平”）
mu_star_scaled[is.na(mu_star_scaled)] <- 0

# 质量检查：标准化后每列应 ~ N(0,1)
print(round(colMeans(mu_star_scaled), 6))   # 约等于 0
print(apply(mu_star_scaled, 2, sd))         # 约等于 1

write_csv(
  as.data.frame(mu_star_scaled) %>% mutate(param_name = rownames(mu_star_scaled)) %>%
    relocate(param_name),
  "5.elementary_effect/EE_pattern/mu_star_scaled_by_column.csv"
)
