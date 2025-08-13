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
# 2) 读取各指标矩阵并合并
#    假设 CSV 文件结构：行=参数, 列=物种-指标组合, 值=0/1
#-----------------------------
all_top <- lapply(indicators, function(ind) {
  df <- read_csv(paste0("5.elementary_effect/EE_pattern/top10pct_", ind, ".csv"),
                 show_col_types = FALSE)

  return(df)
})

names(all_top) <- indicators

# 生成两个结果：分类表和汇总表
results <- lapply(all_top, function(df) {
  
  # 提取物种列
  sp_cols <- grep("^sp[0-9]+", colnames(df), value = TRUE)
  
  # 先生成 param_classes
  param_classes_df <- df %>%
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
    select(param_name, main_sp, affected_sp, class)  # main_sp 不需要就不保留
  
  # 再生成 summary_table
  summary_df <- param_classes_df %>%
    group_by(class) %>%
    summarise(
      n = n(),
      prop = n / nrow(param_classes_df),
      .groups = "drop"
    ) %>%
    arrange(class)
  
  return(list(
    param_classes = param_classes_df,
    summary_table = summary_df
  ))
})

# 分别提取
param_classes <- lapply(results, `[[`, "param_classes")
summary_tables <- lapply(results, `[[`, "summary_table")

species_summary_list <- lapply(param_classes, function(df) {
  df %>%
    group_by(main_sp, class) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(prop = n / sum(n)) %>%
    summarise(
      dominant_class = if (max(prop) >= 0.5) class[which.max(prop)] else NA,
      dominant_prop  = max(prop),
      .groups = "drop"
    )
})


