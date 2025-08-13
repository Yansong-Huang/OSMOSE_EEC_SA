rm(list = ls())
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

#-----------------------------
# 1) 设置指标和路径
#-----------------------------
indicators <- c("biomass_rel", "mean_length", "LFI", "yield_rel", "mean_TL")
sp_list <- paste0("sp", 0:15)  # 主物种列表

#-----------------------------
# 2) 循环处理每个指标
#-----------------------------
for (ind in indicators) {
  
  # 2a) 读取数据
  df <- read_csv(
    paste0("5.elementary_effect/EE_outputs/EE_", ind, "_by_species_stats.csv"),
    show_col_types = FALSE
  )
  
  # 2b) 过滤 LFI/yield_rel 特定物种
  if (ind %in% c("LFI", "yield_rel")) {
    df <- df %>% filter(!species %in% c("sp4", "sp6"))
  }
  
  # 2c) 生成 参数 × 物种组合 的 mu_star 矩阵
  df <- df %>%
    mutate(species_indicator = paste(species, ind, sep = "_")) %>%
    select(param_name, species_indicator, mu_star) %>%
    distinct() %>%
    pivot_wider(names_from = species_indicator, values_from = mu_star)
  
  param_names <- df$param_name
  mu_star_mat <- df %>% select(-param_name) %>% as.matrix()
  rownames(mu_star_mat) <- param_names
  
  #-----------------------------
  # 2d) 排除不对应主物种的参数
  #      只保留中含 sp0-sp15 的行
  #-----------------------------
  # 精确匹配 sp0-sp15，避免 sp16 被误保留
  mu_star_mat <- mu_star_mat[
    grepl("\\bsp([0-9]|1[0-5])\\b", rownames(mu_star_mat)), , drop = FALSE
  ]
  
  
  # 2e) 找出前10%参数-物种组合
  threshold <- quantile(mu_star_mat, probs = 0.9, na.rm = TRUE)
  top5_mat <- (mu_star_mat >= threshold) * 1  # 0/1 矩阵
  
  # 2f) 保存结果
  write_csv(as.data.frame(top5_mat) %>% mutate(param_name = rownames(top5_mat)) %>% relocate(param_name),
            paste0("5.elementary_effect/EE_pattern/top10pct_", ind, ".csv"))
  
  message("指标 ", ind, " 处理完成，前10% 参数-物种组合已保存")
}
