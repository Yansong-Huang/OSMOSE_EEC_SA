library(data.table)

compute_specieswise_EE <- function(
    ind_list,      # 每次模拟的输出列表，每个元素是 20 × 16 × 10 的数组
    sim_key,       # simulation_id、step、replicate、changed_param_idx
    param_names,   # 参数名称向量
    grid_jump,     # Morris 方法的跳步大小
    levels,        # Morris 方法的等级数
    out_name,    # 输出前缀
    out_dir,  # 输出目录
    verbose   = TRUE
) {
  ## ------- 0. 尺寸检查 -------
  n_step  <- max(sim_key$step)
  n_repl  <- max(sim_key$replicate)
  n_param <- length(param_names)
  n_sim   <- length(ind_list)
  stopifnot(n_sim == n_step * n_repl)
  
  ## ------- 1. 聚合函数：每个模拟输出一个 16 维向量（每个物种平均生物量） -------
  agg_fun_species <- function(arr) {
    apply(arr, 2, function(mat) mean(mat, na.rm = TRUE))  # 20×16×10 → 每列均值
  }
  
  ## ------- 2. 聚合所有模拟输出 -------
  Y_mat <- t(vapply(ind_list, agg_fun_species, numeric(16)))  # n_sim × 16
  colnames(Y_mat) <- paste0("sp", seq_len(16))  # 可换成实际物种名
  
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
  names(EE_list_all) <- paste0("sp", seq_len(16))
  
  for (s in seq_len(16)) {
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
  fwrite(EE_stats_all, file = file.path(out_dir, paste0("EE_", out_name, "_stats.csv")))
  saveRDS(EE_list_all, file = file.path(out_dir, paste0("EE_", out_name, "_raw.rds")))
  
  if (verbose) {
    message("✓ Species-wise elementary effects for ", out_name, " are saved in ", out_dir)
  }
  
  invisible(EE_stats_all)
}

# 准备数据
sim_key     <- fread("4.indicators/indicators_output/simulation_key.csv")
param_names <- readRDS("2.get-doe/doe/par_names_0425.rds")
biomass_list <- readRDS("4.indicators/indicators_output/biomass.rds")  # 每个元素是 20×16×10 的 array

# 调用函数
compute_specieswise_EE(
  ind_list    = biomass_list,
  sim_key     = sim_key,
  param_names = param_names,
  grid_jump   = 4,
  levels      = 8,
  out_name    = "biomass",
  out_dir     = "5.elementary_effect/biomass"
)


