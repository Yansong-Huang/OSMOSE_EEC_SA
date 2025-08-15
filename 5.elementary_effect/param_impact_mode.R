rm(list = ls())
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

#-----------------------------
# 1) 设置指标和路径
#-----------------------------
indicators <- c("biomass_rel","yield_rel", "mean_TL", "LFI", "mean_length")
sp_list <- paste0("sp", 0:15)  # 主物种列表

#-----------------------------
# 2) 找出每个指标下效应高的参数-物种组合，以Pareto法建立阈值
#-----------------------------

all_top <- list()

for(ind in indicators){
  # 1) 读取原始 mu_star 表
  df <- read_csv(paste0("5.elementary_effect/EE_outputs/EE_", ind, "_by_species_stats.csv"),
                 show_col_types = FALSE)
  
  # 2) 过滤特定物种（LFI / yield_rel 排除 sp4, sp6）
  df <- df %>%
    filter(!(ind %in% c("LFI", "yield_rel") & species %in% c("sp4", "sp6")))
  
  #排除不对应主物种的参数
  # 精确匹配 sp0-sp15，避免 sp16 被误保留
  df <- df[
    grepl("\\bsp([0-9]|1[0-5])\\b", df$param_name), , drop = FALSE
  ]
  
  
  # 3) 计算 Pareto 拐点阈值
  all_mu <- sort(df$mu_star, decreasing = TRUE)
  cum_eff <- cumsum(all_mu)/sum(all_mu)
  pareto_idx <- which(cum_eff >= 0.6)[1]  # 例如 80% 累积贡献
  threshold <- all_mu[pareto_idx]
  
  # 4) 生成 0-1 矩阵
  df <- df %>%
    mutate(signif = ifelse(mu_star >= threshold, 1, 0)) %>%
    mutate(species_indicator = paste(species, ind, sep = "_")) %>%
    select(param_name, species_indicator, signif) %>%
    pivot_wider(names_from = species_indicator, values_from = signif, values_fill = 0)
  
  all_top[[ind]] <- df
}
#-----------------------------
# 3)总结参数的效应模式
#-----------------------------
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


species_summary_df <- bind_rows(
  Map(function(ind, df) {
    df %>% 
      mutate(indicator = ind,
             dominant_prop = round(dominant_prop, 2))  # 保留两位小数
  }, indicators, species_summary_list)
)

species_summary_wide <- species_summary_df %>%
  pivot_wider(
    names_from = indicator,
    values_from = c(dominant_class, dominant_prop),
    names_glue = "{indicator}_{.value}"
  )

# 设定物种顺序
species_order <- paste0("sp", 0:15)

species_order <- paste0("sp", 0:15)

species_summary_wide <- species_summary_wide %>%
  mutate(main_sp = factor(main_sp, levels = species_order)) %>%
  arrange(main_sp)  # 按因子顺序排序行

fwrite(species_summary_wide, "5.elementary_effect/EE_pattern/param_impact_mode.csv")
