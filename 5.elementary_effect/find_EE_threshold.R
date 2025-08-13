# 系统化审视阈值
# 1 阈值扫描 + 稳定性评估（找“稳定高原”）
# 2 数据驱动阈值（Pareto 覆盖率）
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)

# -----------------------------
# 工具函数
# -----------------------------

# 读取并转为 参数×(物种-指标) 矩阵（值=mu_star）
load_indicator_matrix <- function(ind) {
  df <- read_csv(
    paste0("5.elementary_effect/EE_outputs/EE_", ind, "_by_species_stats.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(indicator = ind) %>%
    filter(!(indicator %in% c("LFI","yield_rel") & species %in% c("sp4","sp6"))) %>%
    mutate(species_indicator = paste0(species, "_", indicator)) %>%
    select(param_name, species_indicator, mu_star) %>%
    distinct()
  
  mat <- df %>%
    pivot_wider(names_from = species_indicator, values_from = mu_star) %>%
    arrange(param_name)
  
  param_names <- mat$param_name
  mat <- mat %>% select(-param_name)
  mat <- as.matrix(mat)
  rownames(mat) <- param_names
  return(mat)
}

# 过滤不属于 sp0-sp15 的参数（例如渔业全局、资源物种等）
keep_main_species_params <- function(mat, sp_max = 15L) {
  pat <- paste0("sp(", paste0(0:sp_max, collapse="|"), ")")
  keep <- grepl(pat, rownames(mat))
  mat[keep, , drop = FALSE]
}

# 按“全体参数×物种列”选前 q% 为 1，其余 0（ ties 允许略多于 q% ）
binarize_top_q <- function(mat, q = 0.05) {
  v <- as.numeric(mat)
  t <- quantile(v, probs = 1 - q, na.rm = TRUE, type = 8)
  out <- (mat >= t) * 1L
  out[is.na(out)] <- 0L
  out
}

# Pareto 覆盖率阈值（默认 80%）
pareto_threshold <- function(mat, cover = 0.8) {
  v <- sort(as.numeric(mat), decreasing = TRUE)
  v <- v[!is.na(v) & v > 0]
  if (length(v) == 0) return(Inf)
  cs <- cumsum(v) / sum(v)
  k <- which(cs >= cover)[1]
  thr <- v[k]
  return(thr)
}

binarize_by_threshold <- function(mat, thr) {
  out <- (mat >= thr) * 1L
  out[is.na(out)] <- 0L
  out
}

# 参数分类（1-4 类）
classify_params <- function(bin_mat) {
  df <- as.data.frame(bin_mat)
  df$param_name <- rownames(bin_mat)
  
  sp_cols <- grep("^sp[0-9]+", colnames(df), value = TRUE)
  
  out <- df %>%
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
    ungroup()
  
  return(out %>% select(param_name, main_sp, affected_sp, class))
}

# 物种层面比例 + dominant
species_summary <- function(param_classes_df) {
  prop_tbl <- param_classes_df %>%
    group_by(main_sp, class) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  dom_tbl <- prop_tbl %>%
    group_by(main_sp) %>%
    summarise(
      dominant_class = if (max(prop) >= 0.5) class[which.max(prop)] else NA_real_,
      dominant_prop  = max(prop),
      .groups = "drop"
    )
  
  list(prop_tbl = prop_tbl, dom_tbl = dom_tbl)
}

# 相邻阈值之间的稳定性（dominant_class 一致的物种占比）
dominant_stability <- function(dom1, dom2) {
  df <- dom1 %>%
    rename(dc1 = dominant_class) %>%
    inner_join(dom2 %>% rename(dc2 = dominant_class), by = "main_sp")
  valid <- !is.na(df$dc1) & !is.na(df$dc2)
  if (!any(valid)) return(NA_real_)
  mean(df$dc1[valid] == df$dc2[valid])
}

# -----------------------------
# 1 阈值扫描 + 稳定性
# -----------------------------

indicators <- c("biomass_rel", "mean_length", "LFI", "yield_rel", "mean_TL")
ths <- c(0.02, 0.05, 0.08, 0.10, 0.15, 0.20)

scan_results <- list()
for (ind in indicators) {
  mat <- load_indicator_matrix(ind) %>% keep_main_species_params()
  
  # 为每个 q 计算 param_classes 和物种 dominant
  per_q <- lapply(ths, function(q) {
    bin <- binarize_top_q(mat, q = q)
    pc  <- classify_params(bin)
    ss  <- species_summary(pc)
    list(q = q, param_classes = pc, species_prop = ss$prop_tbl, species_dom = ss$dom_tbl)
  })
  names(per_q) <- paste0("q", ths)
  
  # 稳定性：相邻 q 的 dominant 是否保持
  stab <- tibble(
    q1 = ths[-length(ths)],
    q2 = ths[-1],
    stability = map2_dbl(per_q[-length(per_q)], per_q[-1], ~ dominant_stability(.x$species_dom, .y$species_dom))
  )
  
  # class 4 占比随 q 的变化（看“无效应”随阈值的敏感度）
  class4_prop <- map_dfr(per_q, function(x) {
    x$species_prop %>%
      group_by(class) %>%
      summarise(mean_prop = mean(prop), .groups = "drop") %>%
      mutate(q = x$q)
  }) %>% filter(class == 4)
  
  scan_results[[ind]] <- list(per_q = per_q, stability = stab, class4_prop = class4_prop)
}

# 例子：查看 LFI 的稳定性与 class4 占比
scan_results$LFI$stability
scan_results$LFI$class4_prop

#------ 2 用 Pareto（80%覆盖）做一个“数据驱动阈值”对照------
pareto_results <- list()
for (ind in indicators) {
  mat <- load_indicator_matrix(ind) %>% keep_main_species_params()
  thr <- pareto_threshold(mat, cover = 0.8)  # 可改 0.7–0.9 做敏感性
  bin <- binarize_by_threshold(mat, thr)
  pc  <- classify_params(bin)
  ss  <- species_summary(pc)
  pareto_results[[ind]] <- list(threshold = thr, param_classes = pc, species_prop = ss$prop_tbl, species_dom = ss$dom_tbl)
}

# 对比：某指标下，Pareto vs 固定 q=5% 的 dominant_class 一致度
compare_pareto_q <- function(ind, q = 0.05) {
  dom_p <- pareto_results[[ind]]$species_dom
  dom_q <- scan_results[[ind]]$per_q[[paste0("q", q)]]$species_dom
  dominant_stability(dom_p, dom_q)
}

compare_pareto_q("LFI", 0.05)
compare_pareto_q("mean_length", 0.10)
