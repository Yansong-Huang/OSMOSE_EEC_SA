# 为什么鲱鱼参数扰动导致大鱼指数发生了明显变化？
rm(list = ls())
## ------------------------------------------------------------------
##  0. 依赖包  ----
## ------------------------------------------------------------------
libs <- c("data.table", "stringr", "qs")
invisible(lapply(libs, require, character.only = TRUE))

## 指标计算函数在 indicators.R
source("4.indicators/indicators.R")

## ------------------------------------------------------------------
##  1. 路径 & 数据  ----
## ------------------------------------------------------------------
input_dir   <- "morris_simulation_results"           # .rds 存放目录
output_dir  <- "4.indicators/indicators_output"      # 指标/日志输出
simulation_key <- fread("4.indicators/indicators_output/simulation_key.csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
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

## ------------------------------------------------------------------
##  4. 初始化指标 List  ----
## ------------------------------------------------------------------
n_sim <- length(simu_files)
out_LFI_sp                <- vector("list", length(n_sim))

## ------------------------------------------------------------------
##  5. 主循环：读取 + 计算指标  ----
## ------------------------------------------------------------------

for (i in seq_len(n_sim)) { 
  object <- readRDS(simu_files[i])
  
  yield          <- object$osmose.yield
  yield_by_size  <- object$osmose.yieldBySize
  
  out_LFI_sp[[i]]              <- .LFI_by_species(yield, yield_by_size, thr = 40)
  
  ## 释放大对象
  rm(object, yield, yield_by_size)
  if (i %% 1000 == 0) gc()
}

## ------------------------------------------------------------------
##  6. 保存所有指标  ----
## ------------------------------------------------------------------

qsave(out_LFI_sp, file = file.path(output_dir, "lfi_sp.qs"))
