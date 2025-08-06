rm(list = ls())
library(data.table)

compute_specieswise_EE <- function(
    ind_list,      # 每次模拟的输出列表，每个元素是 20 × 16 × 10 的数组
    sim_key,       # simulation_id、step、replicate、changed_param_idx
    param_names,   # 参数名称向量
    grid_jump,     # Morris 方法的跳步大小
    levels,        # Morris 方法的等级数
    out_name,    # 输出前缀
    out_dir,  # 输出目录
    # 默认聚合函数：每个模拟输出一个 16 维向量（每个物种平均生物量） -------
    agg_fun_species = function(arr) {
      apply(arr, 2, # 第二列为物种列，求每列均值
            function(mat) mean(mat, na.rm = TRUE))  
    },
    verbose   = TRUE
) {
  ## ------- 0. 尺寸检查 -------
  n_step  <- max(sim_key$step)
  n_repl  <- max(sim_key$replicate)
  n_param <- length(param_names)
  n_sim   <- length(ind_list)
  stopifnot(n_sim == n_step * n_repl)
  
  ## ------- 
  
  ## ------- 2. 聚合所有模拟输出 -------
  Y_mat <- t(vapply(ind_list, agg_fun_species, numeric(16)))  # n_sim × 16
  colnames(Y_mat) <- paste0("sp", c(0:15))  # 可换成实际物种名
  
  ## ------- 3. 计算 ΔY 和 ΔX -------
  Delta <- grid_jump / (levels - 1)  # 标准化步长
  sim_mat_idx <- matrix(sim_key$simulation_id, nrow = n_step, ncol = n_repl)
  
  delta_Y_list <- vector("list", n_repl)
  for (r in seq_len(n_repl)) {
    idx <- sim_mat_idx[, r]
    delta_Y_list[[r]] <- Y_mat[idx[2:n_step], ] - Y_mat[idx[1:(n_step - 1)], ]
  }
  delta_Y <- do.call(rbind, delta_Y_list)  # (n_step - 1) × n_repl × 16 → 展平
  
  param_idx_mat <- matrix(sim_key$changed_param_idx, nrow = n_step, ncol = n_repl)[2:n_step, ]
  
  ## ------- 4. 计算每个物种 × 每个参数的 EE -------
  EE_list_all <- vector("list", 16)
  names(EE_list_all) <- paste0("sp", c(0:15))
  
  for (s in seq(16)) {
    EE_list <- vector("list", n_param)
    for (p in seq_len(n_param)) {
      mask <- param_idx_mat == p
      EE_list[[p]] <- delta_Y[mask, s] / Delta
    }
    EE_list_all[[s]] <- EE_list
  }
  
  ## ------- 5. 统计 μ, μ*, σ -------
  EE_stats_list <- lapply(seq_along(EE_list_all), function(s) {
    EE_list <- EE_list_all[[s]]
    data.table(
      species    = names(EE_list_all)[s],
      param_id   = seq_len(n_param),
      param_name = param_names,
      mu         = vapply(EE_list, mean,    numeric(1), na.rm = TRUE),
      mu_star    = vapply(EE_list, function(x) mean(abs(x), na.rm = TRUE), numeric(1)),
      sigma      = vapply(EE_list, sd,      numeric(1), na.rm = TRUE),
      n_ee       = vapply(EE_list, function(x) sum(!is.na(x)), integer(1))
    )
  })
  EE_stats_all <- rbindlist(EE_stats_list)
  
  ## ------- 6. 输出结果 -------
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  fwrite(EE_stats_all, file = file.path(out_dir, paste0("EE_", out_name, "_by_species_stats.csv")))
  saveRDS(EE_list_all, file = file.path(out_dir, paste0("EE_", out_name, "_by_species_raw.rds")))
  
  if (verbose) {
    message("✓ Species-wise elementary effects for ", out_name, " are saved in ", out_dir)
  }
  
  invisible(EE_stats_all)
}

# ==== 1. 加载必要数据 ====
sim_key     <- fread("4.indicators/indicators_output/simulation_key.csv")
param_names <- readRDS("2.get-doe/doe/par_names_0425.rds")
# biomass_list <- readRDS("4.indicators/indicators_output/biomass.rds")
# yield_list   <- readRDS("4.indicators/indicators_output/yield.rds")
# baseline_biomass <- readRDS("6.baseline/baseline_indicators_by_species/baseline_biomass_sp.rds")  # 20×16×10
# baseline_yield <- readRDS("6.baseline/baseline_indicators_by_species/baseline_yield_sp.rds")  # 20×16×10
# LFI_list <- readRDS("4.indicators/indicators_output/lfi_sp.rds")
mean_TL_list <- readRDS("4.indicators/indicators_output/meanTL_by_sp.rds")
mean_length_list <- readRDS("4.indicators/indicators_output/meanLength_by_sp.rds")
# ==== 2. 计算 μ*：基于未经标准化的原始值 ====
# compute_specieswise_EE(
#   ind_list    = biomass_list,
#   sim_key     = sim_key,
#   param_names = param_names,
#   grid_jump   = 4,
#   levels      = 8,
#   out_name    = "biomass_raw",
#   out_dir     = "5.elementary_effect/EE_outputs"
# )

# compute_specieswise_EE(
#   ind_list    = yield_list,
#   sim_key     = sim_key,
#   param_names = param_names,
#   grid_jump   = 4,
#   levels      = 8,
#   out_name    = "yield_raw",
#   out_dir     = "5.elementary_effect/EE_outputs"
# )

compute_specieswise_EE(
  ind_list    = mean_TL_list,
  sim_key     = sim_key,
  param_names = param_names,
  grid_jump   = 4,
  levels      = 8,
  out_name    = "mean_TL",
  out_dir     = "5.elementary_effect/EE_outputs"
)

compute_specieswise_EE(
  ind_list    = mean_length_list,
  sim_key     = sim_key,
  param_names = param_names,
  grid_jump   = 4,
  levels      = 8,
  out_name    = "mean_length",
  out_dir     = "5.elementary_effect/EE_outputs"
)

# ==== 3. 相对基线标准化函数 ====
# standardize_to_baseline <- function(ind_list, baseline_arr, method = "rel") {
#   baseline_mean <- apply(baseline_arr, 2, mean, na.rm = TRUE)
#   names(baseline_mean) <- paste0("sp", 0:15)
#   
#   if (method == "rel") {
#     lapply(ind_list, function(arr) sweep(arr, 2, baseline_mean, FUN = "/") - 1)
#   } else if (method == "logrel") {
#     lapply(ind_list, function(arr) log(sweep(arr, 2, baseline_mean, FUN = "/")))
#   } else {
#     stop("Unknown standardization method.")
#   }
# }

# ==== 4. 生成相对变化率的输入 ====
# biomass_list_rel <- standardize_to_baseline(biomass_list, baseline_biomass, method = "rel")
# yield_list_rel <- standardize_to_baseline(yield_list, baseline_yield, method = "rel")

# ==== 5. 计算 μ*：基于经过标准化的指标 ====
# compute_specieswise_EE(
#   ind_list    = biomass_list_rel,
#   sim_key     = sim_key,
#   param_names = param_names,
#   grid_jump   = 4,
#   levels      = 8,
#   out_name    = "biomass_rel",
#   out_dir     = "5.elementary_effect/EE_outputs",
#   agg_fun_species <- function(arr) {
#     apply(arr[3:20, , ], 2, function(mat) mean(mat, na.rm = TRUE))# 只取第3年到第20年的数据
#   }
# )
# 
# ## 释放大对象
# rm(biomass_list)
# 
# compute_specieswise_EE(
#   ind_list    = yield_list_rel,
#   sim_key     = sim_key,
#   param_names = param_names,
#   grid_jump   = 4,
#   levels      = 8,
#   out_name    = "yield_rel",
#   out_dir     = "5.elementary_effect/EE_outputs",
#   agg_fun_species <- function(arr) {
#     apply(arr[3:20, , ], 2, function(mat) mean(mat, na.rm = TRUE))# 只取第3年到第20年的数据
#   }
# )