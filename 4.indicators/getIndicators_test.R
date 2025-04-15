
# Functions ---------------------------------------------------------------
rm(list = ls())
source("4.indicators/indicators.R") 

# Define input directory and files -------------------------------------------------------------
input_dir = "simulation_results_test"
simu_results = list.files(path = input_dir, pattern = "\\.rds$", full.names = TRUE)


# Indicators outputs ------------------------------------------------------

# indicator outputs
out_meanLength            = list()
out_meanTL                = list()
out_lfi40                 = list()

out_biomass_sp            = list()
out_yield_sp              = list()
out_meanTL_sp             = list()
out_meanLength_sp         = list()

for(i in 1:length(simu_results)) {
  
  object         = readRDS(simu_results[i])
  
  # getting variables
  biomass        = object$osmose.biomass
  yield          = object$osmose.yield
  meanTL         = object$osmose.meanTL
  meanLength     = object$osmose.meanLength
  yield_by_size  = object$osmose.yieldBySize
  
  out_meanLength[[i]]             = .MeanTL(biomass, meanLength)
  out_meanTL[[i]]                 = .MeanTL(biomass, meanTL)
  out_lfi40[[i]]                  = .LFI(yield, yield_by_size, thr = 40)

  out_biomass_sp[[i]]             = biomass
  out_yield_sp[[i]]               = yield
  out_meanTL_sp[[i]]              = meanTL
  out_meanLength_sp[[i]]          = meanLength
  
}


# Save outputs ------------------------------------------------------------

output_dir  = "4.indicators/indicators_output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(object = out_meanLength            , file = file.path(output_dir, "meanLength.rds"))
saveRDS(object = out_meanTL                , file = file.path(output_dir, "meanTL.rds"))
saveRDS(object = out_lfi40                 , file = file.path(output_dir, "lfi40.rds"))

saveRDS(object = out_biomass_sp             , file = file.path(output_dir, "biomass.rds"))
saveRDS(object = out_yield_sp               , file = file.path(output_dir, "yield.rds"))
saveRDS(object = out_meanTL_sp              , file = file.path(output_dir, "meanTL_by_sp.rds"))
saveRDS(object = out_meanLength_sp          , file = file.path(output_dir, "meanLength_by_sp.rds"))
