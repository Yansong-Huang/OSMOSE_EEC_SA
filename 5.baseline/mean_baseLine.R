
rm(list = ls())

# Read baseline files -----------------------------------------------------
input_dir <- "5.baseline/indicators_baseline/"

baseline_meanLength            = readRDS(file.path(input_dir,"baseline_meanLength.rds"))
baseline_meanTL                = readRDS(file.path(input_dir,"baseline_meanTL.rds"))
baseline_lfi40                 = readRDS(file.path(input_dir,"baseline_lfi40.rds"))
baseline_biomass_sp            = readRDS(file.path(input_dir,"baseline_biomass_sp.rds"))
baseline_yield_sp              = readRDS(file.path(input_dir,"baseline_yield_sp.rds"))
baseline_meanTL_sp             = readRDS(file.path(input_dir,"baseline_meanTL_sp.rds"))
baseline_meanLength_sp         = readRDS(file.path(input_dir,"baseline_meanLength_sp.rds"))

# Mean baseline indicators ------------------------------------------------

#mean over baseline indicators
baseline_meanLength            = as.data.frame(apply(baseline_meanLength           , 1     , mean, na.rm = TRUE)); colnames(baseline_meanLength)       = "mean"
baseline_meanTL                = as.data.frame(apply(baseline_meanTL               , 1     , mean, na.rm = TRUE)); colnames(baseline_meanTL)           = "mean"
baseline_lfi40                 = as.data.frame(apply(baseline_lfi40                , 1     , mean, na.rm = TRUE)); colnames(baseline_lfi40) = "mean"
baseline_biomass_sp            = as.data.frame(apply(baseline_biomass_sp           , c(1,2), mean, na.rm = TRUE))
baseline_yield_sp              = as.data.frame(apply(baseline_yield_sp             , c(1,2), mean, na.rm = TRUE))
baseline_meanTL_sp             = as.data.frame(apply(baseline_meanTL_sp            , c(1,2), mean, na.rm = TRUE))
baseline_meanLength_sp         = as.data.frame(apply(baseline_meanLength_sp        , c(1,2), mean, na.rm = TRUE))

# Saving indicators -------------------------------------------------------

output_dir = "5.baseline/mean_baseline"
dir.create(output_dir)

saveRDS(object = baseline_meanLength            , file = file.path(output_dir, "baseline_meanLength.rds"))
saveRDS(object = baseline_meanTL                , file = file.path(output_dir, "baseline_meanTL.rds"))
saveRDS(object = baseline_lfi40                 , file = file.path(output_dir, "baseline_lfi40.rds"))
saveRDS(object = baseline_biomass_sp            , file = file.path(output_dir, "baseline_biomass_sp.rds"))
saveRDS(object = baseline_yield_sp              , file = file.path(output_dir, "baseline_yield_sp.rds"))
saveRDS(object = baseline_meanTL_sp             , file = file.path(output_dir, "baseline_meanTL_sp.rds"))
saveRDS(object = baseline_meanLength_sp         , file = file.path(output_dir, "baseline_meanLength_sp.rds"))
