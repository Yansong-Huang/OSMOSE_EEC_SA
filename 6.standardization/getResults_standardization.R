

# Post processing indicators after the UA
rm(list = ls())
require("abind")

# baseline indicators -----------------------------------------------------
baseline_dir <- "5.baseline/mean_baseline/"

baseline_meanLength            = readRDS(file.path(baseline_dir,"baseline_meanLength.rds"))
baseline_meanTL                = readRDS(file.path(baseline_dir,"baseline_meanTL.rds"))
baseline_lfi40                 = readRDS(file.path(baseline_dir,"baseline_lfi40.rds"))
baseline_biomass_sp            = readRDS(file.path(baseline_dir,"baseline_biomass_sp.rds"))
baseline_yield_sp              = readRDS(file.path(baseline_dir,"baseline_yield_sp.rds"))
baseline_meanTL_sp             = readRDS(file.path(baseline_dir,"baseline_meanTL_sp.rds"))
baseline_meanLength_sp         = readRDS(file.path(baseline_dir,"baseline_meanLength_sp.rds"))

# Functions ---------------------------------------------------------------

#average over replicates
# .meanRep_2D = function(x){
#   
#   x = apply(x, 1, mean, na.rm = TRUE) 
#   return(x)
# }
.meanRep_3D = function(x){
  
  x = apply(x, c(1,2), mean, na.rm = TRUE) #remove osmose replicates
  return(x)
}
# 封装函数：对重复求平均并拼接
processMeanReplicates <- function(data_list) {
  # data_list: 一个包含若干个 2D 矩阵的列表，每个矩阵 行=指标，列=重复数
  
  # 定义单个矩阵的行均值函数
  meanRep_2D <- function(x) {
    rowMeans(x, na.rm = TRUE)
  }
  
  # 对列表中每个矩阵求行均值
  result_list <- lapply(data_list, meanRep_2D)
  
  # 将所有结果拼接成 data.frame，列名设为NULL
  result_df <- do.call(cbind.data.frame, result_list)
  colnames(result_df) <- NULL
  
  return(result_df)
}

# standardization
.relative_change = function(sim, baseline){
  
  ind_relative = (sim - baseline) / baseline
  #ind_mean     = apply(ind_relative, 1, mean, na.rm = TRUE)
  
  return(ind_relative)
}

# get indicatiors after processing
# procesingIndicators = function(sp, scenario){
  
  test_dir <- "4.indicators/indicators_output/"
  
  
  # read indicators
  meanLength            = readRDS(file.path(test_dir,"meanLength.rds"))
  meanTL                = readRDS(file.path(test_dir,"meanTL.rds"))
  lfi40                 = readRDS(file.path(test_dir,"lfi40.rds"))
  biomass_sp            = readRDS(file.path(test_dir,"biomass.rds"))
  yield_sp              = readRDS(file.path(test_dir,"yield.rds"))
  meanTL_sp             = readRDS(file.path(test_dir,"meanTL_by_sp.rds"))
  meanLength_sp         = readRDS(file.path(test_dir,"meanLength_by_sp.rds"))
  
  
  # average over osmose replicates
  meanTL_average        = processMeanReplicates(meanTL)
  lfi40_average         = processMeanReplicates(lfi40)
  biomass_sp            = lapply(biomass_sp       , FUN = .meanRep_3D)
  yield_sp              = lapply(yield_sp         , FUN = .meanRep_3D)
  meanTL_sp             = lapply(meanTL_sp        , FUN = .meanRep_3D)
  meanLength_sp         = lapply(meanLength_sp    , FUN = .meanRep_3D)
  
  # standardization
  relativeChange = list(
    meanLength            = apply(meanLength            , MARGIN = 2, FUN = .relative_change, baseline_meanLength[,1]),
    meanTL                = apply(meanTL                , MARGIN = 2, FUN = .relative_change, baseline_meanTL[,1]),
    lfi40                 = apply(lfi40                 , MARGIN = 2, FUN = .relative_change, baseline_lfi40[,1]),
    biomass_sp            = lapply(biomass_sp           , FUN = .relative_change, baseline_biomass_sp),
    yield_sp              = lapply(yield_sp             , FUN = .relative_change, baseline_yield_sp),
    meanTL_sp             = lapply(meanTL_sp            , FUN = .relative_change, baseline_meanTL_sp),
    meanLength_sp         = lapply(meanLength_sp        , FUN = .relative_change, baseline_meanLength_sp)
  )
  
  biomass_sp    = abind(biomass_sp   , along = 3)
  abundance_sp  = abind(abundance_sp , along = 3)
  yield_sp      = abind(yield_sp     , along = 3)
  meanTL_sp     = abind(meanTL_sp    , along = 3)
  meanLength_sp = abind(meanLength_sp, along = 3)
  
  coefficientVariation = list(
    
    meanLength            = (apply(meanLength            , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(meanLength            , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    meanTL                = (apply(meanTL                , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(meanTL                , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    biomass_sp            = (apply(biomass_sp            , MARGIN = c(1,2), FUN = sd, na.rm = TRUE)) / (apply(biomass_sp            , MARGIN = c(1,2), FUN = mean, na.rm = TRUE)),
    yield_sp              = (apply(yield_sp              , MARGIN = c(1,2), FUN = sd, na.rm = TRUE)) / (apply(yield_sp              , MARGIN = c(1,2), FUN = mean, na.rm = TRUE)),
    meanTL_sp             = (apply(meanTL_sp             , MARGIN = c(1,2), FUN = sd, na.rm = TRUE)) / (apply(meanTL_sp             , MARGIN = c(1,2), FUN = mean, na.rm = TRUE)),
    meanLength_sp         = (apply(meanLength_sp         , MARGIN = c(1,2), FUN = sd, na.rm = TRUE)) / (apply(meanLength_sp         , MARGIN = c(1,2), FUN = mean, na.rm = TRUE))
    
  )
  
  out = list(relativeChange = relativeChange, coefficientVariation = coefficientVariation)
  
#   return(out)
#   
# }

# Processing indicators ---------------------------------------------------

# 10P
# test_10p = procesingIndicators(sp = "sp0", scenario = "10p")


# Saving processed indicators ---------------------------------------------

saveRDS(object = sim10p_sp0, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim10p_sp0.rds"))
