library(dplyr)
library(tidyr)
library(readr)
library(data.table)

indicators <- c("biomass_rel","yield_rel", "mean_TL", "LFI", "mean_length")

# 建立存放结果的 list
res_mu_star <- list()
res_outsp <- list()
res_type <- list()

for(ind in indicators) {
  EE_long <- read_csv(paste0("5.elementary_effect/EE_outputs/EE_", ind, "_by_species_stats.csv"),
                      show_col_types = FALSE)
  sp_names <- c(
    "sp0" = "SYC",  "sp1" = "MUR",  "sp2" = "BIB",  "sp3" = "WHG",
    "sp4" = "POD",  "sp5" = "COD",  "sp6" = "LYY",  "sp7" = "SOL",
    "sp8" = "PLE",  "sp9" = "HOM",  "sp10" = "MAC", "sp11" = "HER",
    "sp12" = "PIL", "sp13" = "SQZ", "sp14" = "CTC", "sp15" = "RJC"
  )
  
  # 将物种列改为缩写
  EE_long <- EE_long %>%
    mutate(species = recode(species, !!!sp_names),
           indicator = ind) %>%       # ✅ 增加 indicator 列
    rename(output_species = species)
  
  # ---------- 合并 param_type 和 param_label ----------
  mapping <- fread("5.elementary_effect/param_name_map.csv")
  EE_long_label <- merge(EE_long, mapping, by = "param_name", all.x = TRUE)
  
  # 排除不对应主物种的参数
  EE_long_label <-  EE_long_label %>%
    filter(!param_species %in% c("fleet","resource"))
  
  # 找每个 param_species × indicator 的最大 mu_star
  EE_max <- EE_long_label %>%
    group_by(param_species, indicator) %>%
    slice_max(mu_star, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # 分别存到 list
  res_mu_star[[ind]]     <- EE_max %>% select(param_species, indicator, mu_star)
  res_outsp[[ind]]  <- EE_max %>% select(param_species, indicator, output_species)
  res_type[[ind]]   <- EE_max %>% select(param_species, indicator, param_type)
}

# ----------- 循环结束后拼表 -----------
# 定义排序顺序
species_order <- sp_names

# 1) 最大效应值表
tab_mu_star <- bind_rows(res_mu_star) %>%
  pivot_wider(names_from = indicator, values_from = mu_star) %>%
  mutate(param_species = factor(param_species, levels = species_order)) %>%
  arrange(param_species)

# 2) 对应输出效应物种表
tab_outsp <- bind_rows(res_outsp) %>%
  pivot_wider(names_from = indicator, values_from = output_species) %>%
  mutate(param_species = factor(param_species, levels = species_order)) %>%
  arrange(param_species)

# 3) 对应参数类型表
tab_type <- bind_rows(res_type) %>%
  pivot_wider(names_from = indicator, values_from = param_type) %>%
  mutate(param_species = factor(param_species, levels = species_order)) %>%
  arrange(param_species)

fwrite(tab_mu_star,"5.elementary_effect/EE_pattern/highest_mu_star.csv")
fwrite(tab_outsp,"5.elementary_effect/EE_pattern/highest_out_sp.csv")
fwrite(tab_type,"5.elementary_effect/EE_pattern/highest_param_type.csv")
