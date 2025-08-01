# 为什么鲱鱼参数扰动导致大鱼指数发生了明显变化？

## ------------------------------------------------------------------
##  0. 依赖包  ----
## ------------------------------------------------------------------
libs <- c("data.table", "stringr")
invisible(lapply(libs, require, character.only = TRUE))

## 指标计算函数在 indicators.R
source("4.indicators/indicators.R")

## ------------------------------------------------------------------
##  1. 路径 & 数据  ----
## ------------------------------------------------------------------
input_dir   <- "morris_simulation_results"           # .rds 存放目录
output_dir  <- "4.indicators/indicators_output/herring_perturbation"      # 指标/日志输出
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
##  3. 找到所有变动了鲱鱼参数的模拟  ----
## ------------------------------------------------------------------
## 

# 找到所有变动了鲱鱼参数的模拟，并选取当前模拟与前一模拟
sel <- simulation_key[grepl("sp11", changed_param)]


# 收集需要的 simulation_id：当前的 + 前一个
sim_ids_to_load <- unique(c(sel$simulation_id, sel$simulation_id-1))
# 筛选不合法编号
sim_ids_to_load <- unique(c(sel$simulation_id, sel$simulation_id - 1))
sim_ids_to_load <- sim_ids_to_load[sim_ids_to_load > 0 & sim_ids_to_load <= length(simu_files)]


## ------------------------------------------------------------------
##  4. 初始化指标 List  ----
## ------------------------------------------------------------------

out_LFI_sp                <- vector("list", length(sim_ids_to_load))
out_yield_by_size_sp      <- vector("list", length(sim_ids_to_load))


## ------------------------------------------------------------------
##  5. 主循环：读取 + 计算指标  ----
## ------------------------------------------------------------------

for (i in seq_along(sim_ids_to_load)) { 
  sim_id <- sim_ids_to_load[i]
  cat("Processing sim_id:", sim_id, "file:", basename(simu_files[sim_id]), "\n")
  
  object <- readRDS(simu_files[sim_id])
  
  yield          <- object$osmose.yield
  yield_by_size  <- object$osmose.yieldBySize
  
  out_yield_by_size_sp[[i]]    <- .YieldBySize_to_df(yield_by_size)
  out_LFI_sp[[i]]              <- .LFI_by_species(yield, yield_by_size, thr = 40)
  
  ## 释放大对象
  rm(object, yield, yield_by_size)
  if (i %% 100 == 0) gc()
}

## ------------------------------------------------------------------
##  6. 保存所有指标  ----
## ------------------------------------------------------------------
saveRDS(out_LFI_sp            , file = file.path(output_dir, "lfi_sp.rds"))
saveRDS(out_yield_by_size_sp  , file = file.path(output_dir, "out_yield_by_size_sp.rds"))


