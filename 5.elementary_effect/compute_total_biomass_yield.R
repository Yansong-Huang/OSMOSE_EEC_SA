rm(list = ls())

library(data.table)

compute_total_indicator_EE <- function(
    ind_list,      # 每次模拟的输出列表，每个是 array: [step × time × replicate]
    sim_key,       # 包含 simulation_id、step、replicate、changed_param_idx
    param_names,   # 参数名向量
    grid_jump,     # Morris 跳步
    levels,        # Morris 等级
    out_name,      # 输出文件前缀
    out_dir,       # 输出目录
    agg_fun = function(x) mean(colSums(x)),  # 聚合函数
    verbose = TRUE
) {
  if (verbose) message("✓ Starting total indicator EE computation...")
  
  # ---------- 1. 将每个模拟转换为单一指标值 ----------
  indicator_vec <- vapply(ind_list, agg_fun, numeric(1))  # 得到 n_sim 长向量
  sim_key_dt <- as.data.table(sim_key)
  sim_key_dt[, indicator := indicator_vec]
  
  # ---------- 2. 构造轨迹矩阵 ----------
  n_step <- max(sim_key_dt$step)
  n_repl <- max(sim_key_dt$replicate)
  n_param <- length(param_names)
  Delta <- grid_jump / (levels - 1)
  
  # reshape 成 [step × replicate] 矩阵
  Y_mat <- matrix(sim_key_dt$indicator, nrow = n_step, ncol = n_repl)
  param_idx_mat <- matrix(sim_key_dt$changed_param_idx, nrow = n_step, ncol = n_repl)
  
  # ---------- 3. 计算跳步差值，并按参数分类 ----------
  EE_list <- vector("list", n_param)
  
  for (p in seq_len(n_param)) {
    diffs <- c()  # 存放对 p 参数的所有跳步
    for (r in seq_len(n_repl)) {
      for (s in 2:n_step) {
        if (param_idx_mat[s, r] == p) {
          delta_y <- Y_mat[s, r] - Y_mat[s - 1, r]
          diffs <- c(diffs, delta_y / Delta)
        }
      }
    }
    EE_list[[p]] <- diffs
  }
  
  # ---------- 4. 计算统计量 ----------
  EE_stats <- data.table(
    param_id   = seq_len(n_param),
    param_name = param_names,
    mu         = vapply(EE_list, mean, numeric(1), na.rm = TRUE),
    mu_star    = vapply(EE_list, function(x) mean(abs(x), na.rm = TRUE), numeric(1)),
    sigma      = vapply(EE_list, sd, numeric(1), na.rm = TRUE),
    n_ee       = vapply(EE_list, function(x) sum(!is.na(x)), integer(1))
  )
  
  # ---------- 5. 输出 ----------
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, paste0("EE_", out_name, "_total_", out_name, ".csv"))
  fwrite(EE_stats, out_file)
  if (verbose) message("✓ EE results saved to: ", out_file)
  
  invisible(EE_stats)
}


# 准备数据
sim_key     <- fread("4.indicators/indicators_output/simulation_key.csv")
param_names <- readRDS("2.get-doe/doe/par_names_0425.rds")
biomass_list <- readRDS("4.indicators/indicators_output/biomass.rds")  # 每个元素的维度为20年×16物种×10模拟重复

# 调用函数
EE_total_biomass <- compute_total_indicator_EE(
  ind_list = biomass_list,
  sim_key = sim_key,
  param_names = param_names,
  grid_jump = 4,
  levels = 8,
  out_name = "biomass",
  out_dir = "5.elementary_effect/EE_outputs",
  agg_fun = function(x) {
    # 只取第3年到第20年的数据
    x_sub <- x[3:20, , , drop = FALSE]
    mean(apply(x_sub, c(1, 3), sum))
  }
)

yield_list <- readRDS("4.indicators/indicators_output/yield.rds")  # 每个元素是 20×16×10 的 array

# 调用函数
EE_total_yield <- compute_total_indicator_EE(
  ind_list = yield_list,
  sim_key = sim_key,
  param_names = param_names,
  grid_jump = 4,
  levels = 8,
  out_name = "yield",
  out_dir = "5.elementary_effect/EE_outputs",
  agg_fun = function(x) {
    # 只取第3年到第20年的数据
    x_sub <- x[3:20, , , drop = FALSE]
    mean(apply(x_sub, c(1, 3), sum))
  }
)
