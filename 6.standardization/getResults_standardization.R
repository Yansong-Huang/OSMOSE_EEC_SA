

# Post processing indicators after the UA
rm(list = ls())
require("abind")

# BaseLine indicators -----------------------------------------------------
baseLine_meanLength            = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_meanLength.rds")
baseLine_meanTL                = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_meanTL.rds")
baseLine_meanLifespan          = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_meanLifespan.rds")
baseLine_biomassOverYield      = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_biomassOverYield.rds")
baseLine_mti                   = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_mti.rds")
baseLine_slopeSizeSpectrum     = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_slopeSizeSpectrum.rds")
baseLine_interceptSizeSpectrum = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_interceptSizeSpectrum.rds")
baseLine_lfi20                 = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_lfi20.rds")
baseLine_lfi30                 = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_lfi30.rds")
baseLine_lfi40                 = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_lfi40.rds")
baseLine_biomass_sp            = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_biomass_sp.rds")
baseLine_abundance_sp          = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_abundance_sp.rds")
baseLine_yield_sp              = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_yield_sp.rds")
baseLine_meanTL_sp             = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_meanTL_sp.rds")
baseLine_meanLength_sp         = readRDS("paper_results/indicators_baseLine/indicatorsMean_baseLine/baseLine_meanLength_sp.rds")

# Functions ---------------------------------------------------------------

#removing replicates
.meanRep_2D = function(x){
  
  x = apply(x, 1, mean, na.rm = TRUE) #remove osmose replicates
  return(x)
}
.meanRep_3D = function(x){
  
  x = apply(x, c(1,2), mean, na.rm = TRUE) #remove osmose replicates
  return(x)
}

# standardization
.relative_change = function(sim, baseLine){
  
  ind_relative = (sim - baseLine) / baseLine
  #ind_mean     = apply(ind_relative, 1, mean, na.rm = TRUE)
  
  return(ind_relative)
}

# get indicatiors after processing
procesingIndicators = function(sp, scenario){
  
  # read indicators
  meanLength            = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "meanLength.rds", sep="_")))
  meanTL                = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "meanTL.rds", sep="_")))
  meanLifespan          = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "meanLifespan.rds", sep="_")))
  biomassOverYield      = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "biomassOverYield.rds", sep="_")))
  mti                   = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "mti.rds", sep="_")))
  slopeSizeSpectrum     = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "slopeSizeSpectrum.rds", sep="_")))
  interceptSizeSpectrum = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "interceptSizeSpectrum.rds", sep="_")))
  lfi20                 = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "lfi20.rds", sep="_")))
  lfi30                 = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "lfi30.rds", sep="_")))
  lfi40                 = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "lfi40.rds", sep="_")))
  biomass_sp            = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "biomass_sp.rds", sep="_")))
  abundance_sp          = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "abundance_sp.rds", sep="_")))
  yield_sp              = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "yield_sp.rds", sep="_")))
  meanTL_sp             = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "meanTL_sp.rds", sep="_")))
  meanLength_sp         = readRDS(file.path("paper_results/indicators_results/1.indicators_datarmor-outputs", paste(sp, scenario, sep="_"), paste(scenario, sp, "meanLength_sp.rds", sep="_")))
  
  # remove osmose replicates
  meanLength            = do.call(cbind.data.frame, (lapply(meanLength       , FUN = .meanRep_2D))); colnames(meanLength)            = NULL
  meanTL                = do.call(cbind.data.frame, (lapply(meanTL           , FUN = .meanRep_2D))); colnames(meanTL)                = NULL
  meanLifespan          = do.call(cbind.data.frame, (lapply(meanLifespan     , FUN = .meanRep_2D))); colnames(meanLifespan)          = NULL
  biomassOverYield      = do.call(cbind.data.frame, (lapply(biomassOverYield , FUN = .meanRep_2D))); colnames(biomassOverYield)      = NULL
  mti                   = do.call(cbind.data.frame, (lapply(mti              , FUN = .meanRep_2D))); colnames(mti)                   = NULL
  slopeSizeSpectrum     = do.call(cbind.data.frame, slopeSizeSpectrum                             ); colnames(slopeSizeSpectrum)     = NULL
  interceptSizeSpectrum = do.call(cbind.data.frame, interceptSizeSpectrum                         ); colnames(interceptSizeSpectrum) = NULL
  lfi20                 = do.call(cbind.data.frame, (lapply(lfi20            , FUN = .meanRep_2D))); colnames(lfi20)                 = NULL
  lfi30                 = do.call(cbind.data.frame, (lapply(lfi30            , FUN = .meanRep_2D))); colnames(lfi30)                 = NULL
  lfi40                 = do.call(cbind.data.frame, (lapply(lfi40            , FUN = .meanRep_2D))); colnames(lfi40)                 = NULL
  biomass_sp            = lapply(biomass_sp       , FUN = .meanRep_3D)
  abundance_sp          = lapply(abundance_sp     , FUN = .meanRep_3D)
  yield_sp              = lapply(yield_sp         , FUN = .meanRep_3D)
  meanTL_sp             = lapply(meanTL_sp        , FUN = .meanRep_3D)
  meanLength_sp         = lapply(meanLength_sp    , FUN = .meanRep_3D)
  
  # standardization
  relativeChange = list(
    meanLength            = apply(meanLength            , MARGIN = 2, FUN = .relative_change, baseLine_meanLength[,1]),
    meanTL                = apply(meanTL                , MARGIN = 2, FUN = .relative_change, baseLine_meanTL[,1]),
    meanLifespan          = apply(meanLifespan          , MARGIN = 2, FUN = .relative_change, baseLine_meanLifespan[,1]),
    biomassOverYield      = apply(biomassOverYield      , MARGIN = 2, FUN = .relative_change, baseLine_biomassOverYield[,1]),
    mti                   = apply(mti                   , MARGIN = 2, FUN = .relative_change, baseLine_mti[,1]),
    slopeSizeSpectrum     = apply(slopeSizeSpectrum     , MARGIN = 2, FUN = .relative_change, baseLine_slopeSizeSpectrum),
    interceptSizeSpectrum = apply(interceptSizeSpectrum , MARGIN = 2, FUN = .relative_change, baseLine_interceptSizeSpectrum),
    lfi20                 = apply(lfi20                 , MARGIN = 2, FUN = .relative_change, baseLine_lfi20[,1]),
    lfi30                 = apply(lfi30                 , MARGIN = 2, FUN = .relative_change, baseLine_lfi30[,1]),
    lfi40                 = apply(lfi40                 , MARGIN = 2, FUN = .relative_change, baseLine_lfi40[,1]),
    biomass_sp            = lapply(biomass_sp           , FUN = .relative_change, baseLine_biomass_sp),
    abundance_sp          = lapply(abundance_sp         , FUN = .relative_change, baseLine_abundance_sp),
    yield_sp              = lapply(yield_sp             , FUN = .relative_change, baseLine_yield_sp),
    meanTL_sp             = lapply(meanTL_sp            , FUN = .relative_change, baseLine_meanTL_sp),
    meanLength_sp         = lapply(meanLength_sp        , FUN = .relative_change, baseLine_meanLength_sp)
  )
  
  biomass_sp    = abind(biomass_sp   , along = 3)
  abundance_sp  = abind(abundance_sp , along = 3)
  yield_sp      = abind(yield_sp     , along = 3)
  meanTL_sp     = abind(meanTL_sp    , along = 3)
  meanLength_sp = abind(meanLength_sp, along = 3)
  
  coefficientVariation = list(
    
    meanLength            = (apply(meanLength            , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(meanLength            , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    meanTL                = (apply(meanTL                , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(meanTL                , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    meanLifespan          = (apply(meanLifespan          , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(meanLifespan          , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    biomassOverYield      = (apply(biomassOverYield      , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(biomassOverYield      , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    mti                   = (apply(mti                   , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(mti                   , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    slopeSizeSpectrum     = (apply(slopeSizeSpectrum     , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(slopeSizeSpectrum     , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    interceptSizeSpectrum = (apply(interceptSizeSpectrum , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(interceptSizeSpectrum , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    lfi20                 = (apply(lfi20                 , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(lfi20                 , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    lfi30                 = (apply(lfi30                 , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(lfi30                 , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    lfi40                 = (apply(lfi40                 , MARGIN = 1     , FUN = sd, na.rm = TRUE)) / (apply(lfi40                 , MARGIN = 1     , FUN = mean, na.rm = TRUE)),
    biomass_sp            = (apply(biomass_sp            , MARGIN = c(1,2), FUN = sd, na.rm = TRUE)) / (apply(biomass_sp            , MARGIN = c(1,2), FUN = mean, na.rm = TRUE)),
    abundance_sp          = (apply(abundance_sp          , MARGIN = c(1,2), FUN = sd, na.rm = TRUE)) / (apply(abundance_sp          , MARGIN = c(1,2), FUN = mean, na.rm = TRUE)),
    yield_sp              = (apply(yield_sp              , MARGIN = c(1,2), FUN = sd, na.rm = TRUE)) / (apply(yield_sp              , MARGIN = c(1,2), FUN = mean, na.rm = TRUE)),
    meanTL_sp             = (apply(meanTL_sp             , MARGIN = c(1,2), FUN = sd, na.rm = TRUE)) / (apply(meanTL_sp             , MARGIN = c(1,2), FUN = mean, na.rm = TRUE)),
    meanLength_sp         = (apply(meanLength_sp         , MARGIN = c(1,2), FUN = sd, na.rm = TRUE)) / (apply(meanLength_sp         , MARGIN = c(1,2), FUN = mean, na.rm = TRUE))
    
  )
  
  out = list(relativeChange = relativeChange, coefficientVariation = coefficientVariation)
  
  return(out)
  
}

# Processing indicators ---------------------------------------------------

# 10P
sim10p_sp0 = procesingIndicators(sp = "sp0", scenario = "10p")
sim10p_sp1 = procesingIndicators(sp = "sp1", scenario = "10p")
sim10p_sp2 = procesingIndicators(sp = "sp2", scenario = "10p")
sim10p_sp3 = procesingIndicators(sp = "sp3", scenario = "10p")
sim10p_sp4 = procesingIndicators(sp = "sp4", scenario = "10p")
sim10p_sp5 = procesingIndicators(sp = "sp5", scenario = "10p")
sim10p_sp6 = procesingIndicators(sp = "sp6", scenario = "10p")
sim10p_sp7 = procesingIndicators(sp = "sp7", scenario = "10p")
sim10p_sp8 = procesingIndicators(sp = "sp8", scenario = "10p")

# 20P
sim20p_sp0 = procesingIndicators(sp = "sp0", scenario = "20p")
sim20p_sp1 = procesingIndicators(sp = "sp1", scenario = "20p")
sim20p_sp2 = procesingIndicators(sp = "sp2", scenario = "20p")
sim20p_sp3 = procesingIndicators(sp = "sp3", scenario = "20p")
sim20p_sp4 = procesingIndicators(sp = "sp4", scenario = "20p")
sim20p_sp5 = procesingIndicators(sp = "sp5", scenario = "20p")
sim20p_sp6 = procesingIndicators(sp = "sp6", scenario = "20p")
sim20p_sp7 = procesingIndicators(sp = "sp7", scenario = "20p")
sim20p_sp8 = procesingIndicators(sp = "sp8", scenario = "20p")

# 30P
sim30p_sp0 = procesingIndicators(sp = "sp0", scenario = "30p")
sim30p_sp1 = procesingIndicators(sp = "sp1", scenario = "30p")
sim30p_sp2 = procesingIndicators(sp = "sp2", scenario = "30p")
sim30p_sp3 = procesingIndicators(sp = "sp3", scenario = "30p")
sim30p_sp4 = procesingIndicators(sp = "sp4", scenario = "30p")
sim30p_sp5 = procesingIndicators(sp = "sp5", scenario = "30p")
sim30p_sp6 = procesingIndicators(sp = "sp6", scenario = "30p")
sim30p_sp7 = procesingIndicators(sp = "sp7", scenario = "30p")
sim30p_sp8 = procesingIndicators(sp = "sp8", scenario = "30p")

# Saving processed indicators ---------------------------------------------

#10P
saveRDS(object = sim10p_sp0, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim10p_sp0.rds"))
saveRDS(object = sim10p_sp1, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim10p_sp1.rds"))
saveRDS(object = sim10p_sp2, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim10p_sp2.rds"))
saveRDS(object = sim10p_sp3, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim10p_sp3.rds"))
saveRDS(object = sim10p_sp4, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim10p_sp4.rds"))
saveRDS(object = sim10p_sp5, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim10p_sp5.rds"))
saveRDS(object = sim10p_sp6, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim10p_sp6.rds"))
saveRDS(object = sim10p_sp7, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim10p_sp7.rds"))
saveRDS(object = sim10p_sp8, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim10p_sp8.rds"))


#20P
saveRDS(object = sim20p_sp0, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim20p_sp0.rds"))
saveRDS(object = sim20p_sp1, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim20p_sp1.rds"))
saveRDS(object = sim20p_sp2, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim20p_sp2.rds"))
saveRDS(object = sim20p_sp3, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim20p_sp3.rds"))
saveRDS(object = sim20p_sp4, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim20p_sp4.rds"))
saveRDS(object = sim20p_sp5, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim20p_sp5.rds"))
saveRDS(object = sim20p_sp6, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim20p_sp6.rds"))
saveRDS(object = sim20p_sp7, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim20p_sp7.rds"))
saveRDS(object = sim20p_sp8, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim20p_sp8.rds"))


#30P
saveRDS(object = sim30p_sp0, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim30p_sp0.rds"))
saveRDS(object = sim30p_sp1, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim30p_sp1.rds"))
saveRDS(object = sim30p_sp2, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim30p_sp2.rds"))
saveRDS(object = sim30p_sp3, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim30p_sp3.rds"))
saveRDS(object = sim30p_sp4, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim30p_sp4.rds"))
saveRDS(object = sim30p_sp5, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim30p_sp5.rds"))
saveRDS(object = sim30p_sp6, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim30p_sp6.rds"))
saveRDS(object = sim30p_sp7, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim30p_sp7.rds"))
saveRDS(object = sim30p_sp8, file = file.path("paper_results/indicators_results/2.indicators_processed", "sim30p_sp8.rds"))
