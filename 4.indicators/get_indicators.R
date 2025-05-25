## ------------------------------------------------------------------
##  0. 依赖包  ----
## ------------------------------------------------------------------
libs <- c("data.table", "stringr")
invisible(lapply(libs, require, character.only = TRUE))

## ------------------------------------------------------------------
##  1. 路径 & 数据  ----
## ------------------------------------------------------------------
input_dir   <- "morris_simulation_results"           # .rds 存放目录
output_dir  <- "4.indicators/indicators_output"      # 指标/日志输出
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

## Morris 设计
doe_0425 <- readRDS("2.get-doe/doe/doe_0425.rds")   # 得到  doe_0425$doe  &  doe_0425$parameter
D          <- dim(doe_0425$doe)            # (196, 195, 1000)
n_step     <- D[1]                         # 196
n_param    <- D[2]                         # 195
n_repl     <- D[3]                         # 1000

param_names <- doe_0425$parameter          # 长度 195

## ------------------------------------------------------------------
##  2. 列出并排序结果文件  ----
## ------------------------------------------------------------------
simu_files <- list.files(path = input_dir,
                         pattern = "^result_part_(\\d+)_(\\d{5})\\.rds$",
                         full.names = TRUE)

## → 用 “part + 索引” 做**自然排序**确保顺序稳定
simu_files <- simu_files[
  order(
    as.integer(str_extract(basename(simu_files), "(?<=part_)\\d+")),
    as.integer(str_extract(basename(simu_files), "(?<=_)\\d{5}(?=\\.rds$)"))
  )
]

stopifnot(length(simu_files) == n_step * n_repl)

## ------------------------------------------------------------------
##  3. 预先生成『哪一步改了哪个参数』矩阵  ----
##     changed_param_idx:  [n_repl × n_step]；第 1 步 = 0 (baseline)
## ------------------------------------------------------------------
## 逻辑：比较相邻两步的网格点，找到哪一列有差异
changed_param_idx <- matrix(0L, nrow = n_repl, ncol = n_step)  # baseline=0

for (j in seq_len(n_repl)) {
  step_mat <- doe_0425$doe[, , j]          # 196 × 195
  diff_idx <- apply(step_mat[-1, , drop = FALSE] != step_mat[-n_step, , drop = FALSE],
                    1, which)              # 长度 195，元素 1:195
  changed_param_idx[j, 2:n_step] <- unlist(diff_idx)
}

## ------------------------------------------------------------------
##  4. 初始化指标 List  ----
## ------------------------------------------------------------------
n_sim <- length(simu_files)
out_meanLength      <- vector("list", n_sim)
out_meanTL          <- vector("list", n_sim)
out_lfi40           <- vector("list", n_sim)
out_biomass_sp      <- vector("list", n_sim)
out_yield_sp        <- vector("list", n_sim)
out_meanTL_sp       <- vector("list", n_sim)
out_meanLength_sp   <- vector("list", n_sim)

## ------------------------------------------------------------------
##  5. 主循环：读取 + 计算指标 + 记录日志  ----
## ------------------------------------------------------------------
# 假设 changed_param_idx 是 196 × 1000 的矩阵，第一步用0表示无变化
changed_param_idx[changed_param_idx == 0] <- NA          # 0替换为NA，避免索引出错

changed_param <- param_names[changed_param_idx]           # 转成对应参数名，NA保持

simulation_key <- data.table(
  simulation_id     = seq_len(n_sim),                     # 1 到 196000
  replicate         = rep(seq_len(n_repl), each = n_step),# 重复1000次，每次196步
  step              = rep(seq_len(n_step), times = n_repl),# 1~196循环1000次
  changed_param_idx = as.vector(t(changed_param_idx)),    # 按行转成长向量
  changed_param     = param_names[as.vector(t(changed_param_idx))],# 同上，字符或NA
  sim_file          = simu_files                           # 文件名对应196000条
)

## 指标计算函数在 indicators.R
source("4.indicators/indicators.R")

for (i in seq_len(n_sim)) {
  object <- readRDS(simu_files[i])
  
  biomass        <- object$osmose.biomass
  yield          <- object$osmose.yield
  meanTL         <- object$osmose.meanTL
  meanLength     <- object$osmose.meanLength
  yield_by_size  <- object$osmose.yieldBySize
  
  out_meanLength[[i]]   <- .MeanTL(biomass, meanLength)
  out_meanTL[[i]]       <- .MeanTL(biomass, meanTL)
  out_lfi40[[i]]        <- .LFI(yield, yield_by_size, thr = 40)
  
  out_biomass_sp[[i]]    <- biomass
  out_yield_sp[[i]]      <- yield
  out_meanTL_sp[[i]]     <- meanTL
  out_meanLength_sp[[i]] <- meanLength
  
  ## 释放大对象（可选）
  rm(object, biomass, yield, meanTL, meanLength, yield_by_size)
  if (i %% 1000 == 0) gc()
}

## ------------------------------------------------------------------
##  6. 保存所有指标  ----
## ------------------------------------------------------------------
saveRDS(out_meanLength       , file = file.path(output_dir, "meanLength.rds"))
saveRDS(out_meanTL           , file = file.path(output_dir, "meanTL.rds"))
saveRDS(out_lfi40            , file = file.path(output_dir, "lfi40.rds"))
saveRDS(out_biomass_sp       , file = file.path(output_dir, "biomass.rds"))
saveRDS(out_yield_sp         , file = file.path(output_dir, "yield.rds"))
saveRDS(out_meanTL_sp        , file = file.path(output_dir, "meanTL_by_sp.rds"))
saveRDS(out_meanLength_sp    , file = file.path(output_dir, "meanLength_by_sp.rds"))

## ------------------------------------------------------------------
##  7. 保存 simulation_key  ----
## ------------------------------------------------------------------
fwrite(simulation_key, file = file.path(output_dir, "simulation_key.csv"))

message("✓ All done!  Indicators + simulation_key saved in: ", output_dir)
