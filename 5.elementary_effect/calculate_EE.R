## ---------------------------------------------------------------
##  0. 准备环境
## ---------------------------------------------------------------
rm(list = ls())
library(data.table)

## ---------------------------------------------------------------
##  1. 读取数据
## ---------------------------------------------------------------
LFI_list <- readRDS("4.indicators/indicators_output/lfi40.rds")    # 长度 196000
sim_key  <- fread("4.indicators/indicators_output/simulation_key.csv")  # 196000 行
param_names <- readRDS("2.get-doe/doe/par_names_0425.rds")   # 长度 195

stopifnot(length(LFI_list) == nrow(sim_key))

n_step <- length(param_names)+1
n_repl <- max(sim_key$replicate)
n_param <- length(param_names)

## ---------------------------------------------------------------
##  2. 把每个模拟的 20×10 矩阵 → 单值 Ȳ  (先对 20 年取均值，再对 10 重复取均值)
## ---------------------------------------------------------------
avg_LFI <- vapply(LFI_list, function(mat) {
  mean(colMeans(mat, na.rm = TRUE), na.rm = TRUE)    # 20 年 → 1×10，再 10 → 1
}, numeric(1))

sim_key[, Y_mean := avg_LFI]        # 把指标贴到键表中
rm(LFI_list, avg_LFI)               # 省内存

## ---------------------------------------------------------------
##  3. 计算逐步 ΔY 及 ΔX
##      - ΔY: 相邻两步 Y_mean 差
##      - ΔX: Morris 设计的标准化步长 Δ (因为已做无量纲格点)
## ---------------------------------------------------------------
grid_jump <- 4       # 你的 Morris 设置（示例值）
levels    <- 8       # 你的 Morris 设置（示例值）
Delta     <- grid_jump / (levels - 1)   # 全局常数，≈ 0.571 等

## 先把键表按 replicate+step 排成 196×n_repl 的矩阵形索引
sim_mat_index <- matrix(sim_key$simulation_id, nrow = n_step, ncol = n_repl)

## ΔY 矩阵 (195 × n_repl)
delta_Y <- sim_key$Y_mean[sim_mat_index[2:n_step, ]] -
  sim_key$Y_mean[sim_mat_index[1:(n_step-1), ]]

## 改变的参数索引同样 reshape 成矩阵，去掉第一步
param_idx_mat <- matrix(sim_key$changed_param_idx,
                        nrow = n_step, ncol = n_repl)[2:n_step, ]

## ---------------------------------------------------------------
##  4. 把ΔY/Δ 归档到对应参数，计算 EE
## ---------------------------------------------------------------
EE_list <- vector("list", n_param)  # 每个参数一个 numeric 向量

for (p in seq_len(n_param)) {
  ee_vec <- delta_Y[param_idx_mat == p] / Delta     # 取到该参数所有 EE
  EE_list[[p]] <- ee_vec
}

## ---------------------------------------------------------------
##  5. 统计 μ, μ*, σ
## ---------------------------------------------------------------
EE_stats <- data.table(
  param_id   = seq_len(n_param),
  param_name = param_names,
  mu         = vapply(EE_list, mean,    numeric(1), na.rm = TRUE),
  mu_star    = vapply(EE_list, function(x) mean(abs(x), na.rm = TRUE), numeric(1)),
  sigma      = vapply(EE_list, sd,      numeric(1), na.rm = TRUE),
  n_ee       = vapply(EE_list, function(x) sum(!is.na(x)), integer(1))
)

## ---------------------------------------------------------------
##  6. 保存结果
## ---------------------------------------------------------------
fwrite(EE_stats, file = "5.elementary_effect/EE_LFI40_stats.csv")
saveRDS(EE_list,  file = "5.elementary_effect/EE_LFI40_raw.rds")

message("✓ Elementary effects for LFI40 完成并保存！")
