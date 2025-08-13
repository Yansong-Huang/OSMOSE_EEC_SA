rm(list = ls())

library(dplyr)
library(tidyr)
library(stringr)
library(readr)

#-----------------------------
# 1) 设置指标列表
#-----------------------------
indicators <- c("biomass_rel", "mean_length", "LFI", "yield_rel", "mean_TL")

#-----------------------------
# 2) 读取各指标 top5% 矩阵并合并
#    假设 CSV 文件结构：行=参数, 列=物种-指标组合, 值=0/1
#-----------------------------
all_top <- lapply(indicators, function(ind) {
  df <- read_csv(paste0("5.elementary_effect/EE_pattern/top5pct_", ind, ".csv"),
                 show_col_types = FALSE)

  return(df)
})

names(all_top) <- indicators
library(dplyr)
library(stringr)

summary_tables <- lapply(all_top, function(df) {
  
  # 先提取物种列
  sp_cols <- grep("^sp[0-9]+", colnames(df), value = TRUE)
  
  df <- df %>%
    rowwise() %>%
    mutate(
      main_sp = str_extract(param_name, "sp[0-9]+"),
      affected_sp = list(str_extract(sp_cols[which(c_across(all_of(sp_cols)) == 1)], "sp[0-9]+")),
      class = case_when(
        length(affected_sp) == 0 ~ 4,
        main_sp %in% affected_sp & length(affected_sp) == 1 ~ 1,
        main_sp %in% affected_sp & length(affected_sp) > 1 ~ 2,
        !(main_sp %in% affected_sp) & length(affected_sp) > 0 ~ 3,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    group_by(class) %>%
    summarise(
      n = n(),
      prop = n / nrow(df)
    ) %>%
    arrange(class)
  
  return(df)
})


# 查看某个指标
summary_tables$biomass_rel


