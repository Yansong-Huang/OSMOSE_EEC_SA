# ---------------------------------------------------------------
#  函数: compute_morris_EE
# ---------------------------------------------------------------
compute_morris_EE <- function(ind_list,
                              sim_key,
                              param_names,
                              grid_jump,
                              levels,
                              # ---- 可选参数 ----
                              agg_fun   = function(mat) mean(colMeans(mat, na.rm = TRUE), na.rm = TRUE),
                              out_name  = "indicator",
                              out_dir   = "5.elementary_effect") {
  ## ------- 0. 基本尺寸 -------
  n_step <- max(sim_key$step)
  n_repl <- max(sim_key$replicate)
  n_param <- length(param_names)
  n_sim  <- length(ind_list)
  stopifnot(n_sim == n_step * n_repl)
  
  ## ------- 1. 把每个模拟矩阵聚合为单值 Ȳ -------
  Y_mean <- vapply(ind_list, agg_fun, numeric(1))
  
  sim_key[, Y_mean := Y_mean]   # 贴到键表
  rm(ind_list, Y_mean); gc()
  
  ## ------- 2. ΔY & ΔX -------
  Delta <- grid_jump / (levels - 1)           # 无量纲步长
  sim_mat_idx <- matrix(sim_key$simulation_id, nrow = n_step, ncol = n_repl)
  
  delta_Y <- sim_key$Y_mean[sim_mat_idx[2:n_step, ]] -
    sim_key$Y_mean[sim_mat_idx[1:(n_step-1), ]]
  
  param_idx_mat <- matrix(sim_key$changed_param_idx,
                          nrow = n_step, ncol = n_repl)[2:n_step, ]
  
  ## ------- 3. 计算各参数 EE -------
  EE_list <- vector("list", n_param)
  for (p in seq_len(n_param)) {
    EE_list[[p]] <- delta_Y[param_idx_mat == p] / Delta
  }
  
  ## ------- 4. μ, μ*, σ 统计 -------
  EE_stats <- data.table(
    param_id   = seq_len(n_param),
    param_name = param_names,
    mu         = vapply(EE_list, mean,    numeric(1), na.rm = TRUE),
    mu_star    = vapply(EE_list, function(x) mean(abs(x), na.rm = TRUE), numeric(1)),
    sigma      = vapply(EE_list, sd,      numeric(1), na.rm = TRUE),
    n_ee       = vapply(EE_list, function(x) sum(!is.na(x)), integer(1))
  )
  
  ## ------- 5. 保存 -------
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  fwrite(EE_stats, file = file.path(out_dir, paste0("EE_", out_name, "_stats.csv")))
  saveRDS(EE_list,  file = file.path(out_dir, paste0("EE_", out_name, "_raw.rds")))
  
  message("✓ Elementary effects for ", out_name, " is saved in ", out_dir)
  invisible(EE_stats)
}

library(data.table)

# 1) 读取准备好的数据
sim_key  <- fread("4.indicators/indicators_output/simulation_key.csv")
param_names <- readRDS("2.get-doe/doe/par_names_0425.rds")

# 2) LFI40
LFI40_list <- readRDS("4.indicators/indicators_output/lfi40.rds")

compute_morris_EE(
  ind_list    = LFI40_list,
  sim_key     = sim_key,
  param_names = param_names,
  grid_jump   = 4,
  levels      = 8,
  out_name    = "LFI40"
)

# 3) 计算平均营养级 (meanTL)
meanTL_list <- readRDS("4.indicators/indicators_output/meanTL.rds")

compute_morris_EE(
  ind_list    = meanTL_list,
  sim_key     = sim_key,
  param_names = param_names,
  grid_jump   = 4,
  levels      = 8,
  # 平均营养级同样是 20×10 矩阵，聚合函数可复用默认
  out_name    = "meanTL"
)

# 4) 计算平均体长
meanLength_list <- readRDS("4.indicators/indicators_output/meanLength.rds")

compute_morris_EE(
  ind_list    = meanLength_list,
  sim_key     = sim_key,
  param_names = param_names,
  grid_jump   = 4,
  levels      = 8,
  out_name    = "meanLength"
)
