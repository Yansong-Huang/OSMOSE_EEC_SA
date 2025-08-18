rm(list = ls())
library(dplyr)
library(tidyr)
library(readr)

# 指标列表
indicators <- c("biomass", "yield", "meanTL", "meanLength")

# 存放所有指标结果
results_list <- list()

# 遍历指标
for (ind in indicators) {
  
  # 读取 baseline 文件
  baseline_sp <- readRDS(
    paste0("~/OSMOSE/Sensitivity_analysis/6.baseline/baseline_indicators_by_species/baseline_", ind, "_sp.rds")
  )
  
  # 假设数据结构为 [time, species, replicate]，维度类似 (20,16,10)
  baseline_sel <- baseline_sp[3:20, , ]
  
  # 对 time 和 replicate 求平均，只保留 species 维度
  baseline_mean <- apply(baseline_sel, 2, mean)  
  
  # 转为数据框
  df <- data.frame(
    species = paste0("sp", seq_along(baseline_mean)),
    value   = round(baseline_mean, 2)   # 保留两位小数
  )
  
  # 添加指标名
  df$indicator <- ind
  
  results_list[[ind]] <- df
}

# 合并所有指标
results_all <- bind_rows(results_list)

# 转换为宽表（每个指标一列）
results_wide <- results_all %>%
  select(species, indicator, value) %>%
  pivot_wider(names_from = indicator, values_from = value)

# 输出 CSV
write_csv(results_wide, "~/OSMOSE/Sensitivity_analysis/6.baseline/baseline_indicators_summary.csv")

print("✅ 导出完成！（已保留两位小数）")
