
rm(list = ls())

# Read baseLine files -----------------------------------------------------

baseLine_meanLength            = readRDS("indicators_baseLine/baseLine_meanLength.rds")
baseLine_meanTL                = readRDS("indicators_baseLine/baseLine_meanTL.rds")
baseLine_meanLifespan          = readRDS("indicators_baseLine/baseLine_meanLifespan.rds")
baseLine_biomassOverYield      = readRDS("indicators_baseLine/baseLine_biomassOverYield.rds")
baseLine_mti                   = readRDS("indicators_baseLine/baseLine_mti.rds")
baseLine_slopeSizeSpectrum     = readRDS("indicators_baseLine/baseLine_slopeSizeSpectrum.rds")
baseLine_interceptSizeSpectrum = readRDS("indicators_baseLine/baseLine_interceptSizeSpectrum.rds")
baseLine_lfi20                 = readRDS("indicators_baseLine/baseLine_lfi20.rds")
baseLine_lfi30                 = readRDS("indicators_baseLine/baseLine_lfi30.rds")
baseLine_lfi40                 = readRDS("indicators_baseLine/baseLine_lfi40.rds")
baseLine_biomass_sp            = readRDS("indicators_baseLine/baseLine_biomass_sp.rds")
baseLine_abundance_sp          = readRDS("indicators_baseLine/baseLine_abundance_sp.rds")
baseLine_yield_sp              = readRDS("indicators_baseLine/baseLine_yield_sp.rds")
baseLine_meanTL_sp             = readRDS("indicators_baseLine/baseLine_meanTL_sp.rds")
baseLine_meanLength_sp         = readRDS("indicators_baseLine/baseLine_meanLength_sp.rds")

# Mean baseLine indicators ------------------------------------------------

#mean over baseLine indicators
baseLine_meanLength            = as.data.frame(apply(baseLine_meanLength           , 1     , mean, na.rm = TRUE)); colnames(baseLine_meanLength)       = "mean"
baseLine_meanTL                = as.data.frame(apply(baseLine_meanTL               , 1     , mean, na.rm = TRUE)); colnames(baseLine_meanTL)           = "mean"
baseLine_meanLifespan          = as.data.frame(apply(baseLine_meanLifespan         , 1     , mean, na.rm = TRUE)); colnames(baseLine_meanLifespan)     = "mean"
baseLine_biomassOverYield      = as.data.frame(apply(baseLine_biomassOverYield     , 1     , mean, na.rm = TRUE)); colnames(baseLine_biomassOverYield) = "mean"
baseLine_mti                   = as.data.frame(apply(baseLine_mti                  , 1     , mean, na.rm = TRUE)); colnames(baseLine_mti)              = "mean"
baseLine_slopeSizeSpectrum     = baseLine_slopeSizeSpectrum
baseLine_interceptSizeSpectrum = baseLine_interceptSizeSpectrum
baseLine_lfi20                 = as.data.frame(apply(baseLine_lfi20                , 1     , mean, na.rm = TRUE)); colnames(baseLine_lfi20) = "mean"
baseLine_lfi30                 = as.data.frame(apply(baseLine_lfi30                , 1     , mean, na.rm = TRUE)); colnames(baseLine_lfi30) = "mean"
baseLine_lfi40                 = as.data.frame(apply(baseLine_lfi40                , 1     , mean, na.rm = TRUE)); colnames(baseLine_lfi40) = "mean"
baseLine_biomass_sp            = as.data.frame(apply(baseLine_biomass_sp           , c(1,2), mean, na.rm = TRUE))
baseLine_abundance_sp          = as.data.frame(apply(baseLine_abundance_sp         , c(1,2), mean, na.rm = TRUE))
baseLine_yield_sp              = as.data.frame(apply(baseLine_yield_sp             , c(1,2), mean, na.rm = TRUE))
baseLine_meanTL_sp             = as.data.frame(apply(baseLine_meanTL_sp            , c(1,2), mean, na.rm = TRUE))
baseLine_meanLength_sp         = as.data.frame(apply(baseLine_meanLength_sp        , c(1,2), mean, na.rm = TRUE))

# Saving indicators -------------------------------------------------------

outInd = "indicatorsMean_baseLine"
dir.create(outInd)

saveRDS(object = baseLine_meanLength            , file = file.path(outInd, paste0("baseLine_", "meanLength.rds")))
saveRDS(object = baseLine_meanTL                , file = file.path(outInd, paste0("baseLine_", "meanTL.rds")))
saveRDS(object = baseLine_meanLifespan          , file = file.path(outInd, paste0("baseLine_", "meanLifespan.rds")))
saveRDS(object = baseLine_biomassOverYield      , file = file.path(outInd, paste0("baseLine_", "biomassOverYield.rds")))
saveRDS(object = baseLine_mti                   , file = file.path(outInd, paste0("baseLine_", "mti.rds")))
saveRDS(object = baseLine_slopeSizeSpectrum     , file = file.path(outInd, paste0("baseLine_", "slopeSizeSpectrum.rds")))
saveRDS(object = baseLine_interceptSizeSpectrum , file = file.path(outInd, paste0("baseLine_", "interceptSizeSpectrum.rds")))
saveRDS(object = baseLine_lfi20                 , file = file.path(outInd, paste0("baseLine_", "lfi20.rds")))
saveRDS(object = baseLine_lfi30                 , file = file.path(outInd, paste0("baseLine_", "lfi30.rds")))
saveRDS(object = baseLine_lfi40                 , file = file.path(outInd, paste0("baseLine_", "lfi40.rds")))
saveRDS(object = baseLine_biomass_sp            , file = file.path(outInd, paste0("baseLine_", "biomass_sp.rds")))
saveRDS(object = baseLine_abundance_sp          , file = file.path(outInd, paste0("baseLine_", "abundance_sp.rds")))
saveRDS(object = baseLine_yield_sp              , file = file.path(outInd, paste0("baseLine_", "yield_sp.rds")))
saveRDS(object = baseLine_meanTL_sp             , file = file.path(outInd, paste0("baseLine_", "meanTL_sp.rds")))
saveRDS(object = baseLine_meanLength_sp         , file = file.path(outInd, paste0("baseLine_", "meanLength_sp.rds")))
