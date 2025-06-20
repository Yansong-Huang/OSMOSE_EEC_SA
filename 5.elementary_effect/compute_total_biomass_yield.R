library(data.table)

compute_total_indicator_EE <- function(
    ind_list,      # 每次模拟的输出列表，每个是 20 × 16 × 10 的 array
    sim_key,       # 包含 simulation_id、step、replicate、changed_param_idx
    param_names,   # 参数名向量
    grid_jump,     # Morris 跳步
    levels,        # Morris 等级
    out_name,      # 输出文件前缀
    out_dir,       # 输出目录
    verbose = TRUE
) {
  if (verbose) message("✓ Starting total indicator EE computation...")
  
  stopifnot(length(ind_list) == nrow(sim_key))
  
  # 第一步：把每个模拟的数组转为 1 个数值（每个replicate的一条路径，总生物量时间平均）
  indicator_matrix <- lapply(ind_list, function(arr) {
    apply(arr, 3, function(x) mean(colSums(x)))  # 得到一个长度为 r 的向量
  })
  indicator_matrix <- do.call(rbind, indicator_matrix)  # [n_sim × r] 数组
  
  # 确保 sim_key 中有 param_name 和 param_id
  sim_key_dt <- as.data.table(sim_key)
  sim_key_dt[, replicate := as.integer(replicate)]
  sim_key_dt[, changed_param := param_names[changed_param_idx]]
  
  # 核心：分组（按参数），每个参数对应一组轨迹，计算 EE
  EE_list <- sim_key_dt[, {
    mat <- indicator_matrix[.I, , drop = FALSE]  # 当前参数的所有路径 (路径数 × replicate)
    ee <- apply(mat, 2, function(x) diff(x) / grid_jump)  # 每列是某个replicate的轨迹，求差分
    if (is.null(dim(ee))) ee <- matrix(ee, nrow = 1)  # 处理只有一条轨迹时的情况
    list(
      mu = mean(ee),
      mu_star = mean(abs(ee)),
      sigma = sd(ee),
      n_ee = length(ee)
    )
  }, by = .(param_id = changed_param_idx, param_name = changed_param)]
  
  # 写入 CSV
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_file <- file.path(out_dir, paste0("EE_", out_name, "_total_",out_name,".csv"))
  fwrite(EE_list, out_file)
  if (verbose) message("✓ EE results saved to: ", out_file)
  
  return(EE_list)
}


# 准备数据
sim_key     <- fread("4.indicators/indicators_output/simulation_key.csv")
param_names <- readRDS("2.get-doe/doe/par_names_0425.rds")
biomass_list <- readRDS("4.indicators/indicators_output/biomass.rds")  # 每个元素是 20×16×10 的 array

# 调用函数
EE_total_biomass <- compute_total_indicator_EE(
  ind_list = biomass_list,
  sim_key = sim_key,
  param_names = param_names,
  grid_jump = 4,
  levels = 8,
  out_name = "biomass",
  out_dir = "5.elementary_effect/biomass"
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
  out_dir = "5.elementary_effect/yield"
)
