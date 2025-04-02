
scenario  = "20p"
species   = "sp5"

# Functions ---------------------------------------------------------------

setwd("/home1/datahome/clujanpa/uncertainty_indicators")
source("indicators.R") 
source("auxiliar_functions.R")

# Reading rds -------------------------------------------------------------

outputRds = file.path("/home/datawork-marbec-scenlab/OSMOSE/Criscely/ua_osmose_paper/outputs_uncertainty", paste(species, scenario, sep = "_"))
files     = list.files(path = outputRds, pattern = "\\d.rds")
nfiles    = length(files)

# Indicators outputs ------------------------------------------------------

# indicator outputs
out_meanLength            = list()
out_meanTL                = list()
out_meanLifespan          = list()
out_biomassOverYield      = list()
out_mti                   = list()
out_slopeSizeSpectrum     = list()
out_interceptSizeSpectrum = list()
out_lfi20                 = list()
out_lfi30                 = list()
out_lfi40                 = list()

out_biomass_sp            = list()
out_abundance_sp          = list()
out_yield_sp              = list()
out_meanTL_sp             = list()
out_meanLength_sp         = list()

for(i in seq_len(nfiles)) {
  
  object         = readRDS(file = file.path(outputRds, files[i]))
  
  # getting variables
  biomass        = object$osmose.biomass
  abundance      = object$osmose.abundance
  yield          = object$osmose.yield
  meanTL         = object$osmose.meanTL
  meanLength     = object$osmose.meanLength
  sizeSpectrum   = object$osmose.sizeSpectrum
  sizeSpectrumB  = object$osmose.sizeSpectrumB
  
  out_meanLength[[i]]            = .MeanLength(abundance, meanLength)
  out_meanTL[[i]]                = .MeanTL(biomass, meanTL)
  out_meanLifespan[[i]]          = .MeanLifespan(biomass)
  out_biomassOverYield[[i]]      = .BiomassOverYield(biomass, yield)
  out_mti[[i]]                   = .MTI(meanTL, yield)
  out_slopeSizeSpectrum[[i]]     = .FitSizeSpectrum(sizeSpectrum, output = "slope")
  out_interceptSizeSpectrum[[i]] = .FitSizeSpectrum(sizeSpectrum, output = "intercept")
  out_lfi20[[i]]                 = .LFI(biomass, sizeSpectrumB, thr = 20)
  out_lfi30[[i]]                 = .LFI(biomass, sizeSpectrumB, thr = 30)
  out_lfi40[[i]]                 = .LFI(biomass, sizeSpectrumB, thr = 40)

  out_biomass_sp[[i]]             = biomass
  out_abundance_sp[[i]]           = abundance
  out_yield_sp[[i]]               = yield
  out_meanTL_sp[[i]]              = meanTL
  out_meanLength_sp[[i]]          = meanLength
  
}


# Save outputs ------------------------------------------------------------

outputDir  = file.path("/home/datawork-marbec-scenlab/OSMOSE/Criscely/ua_osmose_paper/indicators", paste(species, scenario, sep = "_"))

saveRDS(object = out_meanLength            , file = file.path(outputDir, paste(scenario, species, "meanLength.rds", sep = "_")))
saveRDS(object = out_meanTL                , file = file.path(outputDir, paste(scenario, species, "meanTL.rds", sep = "_")))
saveRDS(object = out_meanLifespan          , file = file.path(outputDir, paste(scenario, species, "meanLifespan.rds", sep = "_")))
saveRDS(object = out_biomassOverYield      , file = file.path(outputDir, paste(scenario, species, "biomassOverYield.rds", sep = "_")))
saveRDS(object = out_mti                   , file = file.path(outputDir, paste(scenario, species, "mti.rds", sep = "_")))
saveRDS(object = out_slopeSizeSpectrum     , file = file.path(outputDir, paste(scenario, species, "slopeSizeSpectrum.rds", sep = "_")))
saveRDS(object = out_interceptSizeSpectrum , file = file.path(outputDir, paste(scenario, species, "interceptSizeSpectrum.rds", sep = "_")))
saveRDS(object = out_lfi20                 , file = file.path(outputDir, paste(scenario, species, "lfi20.rds", sep = "_")))
saveRDS(object = out_lfi30                 , file = file.path(outputDir, paste(scenario, species, "lfi30.rds", sep = "_")))
saveRDS(object = out_lfi40                 , file = file.path(outputDir, paste(scenario, species, "lfi40.rds", sep = "_")))

saveRDS(object = out_biomass_sp             , file = file.path(outputDir, paste(scenario, species, "biomass_sp.rds"   , sep = "_")))
saveRDS(object = out_abundance_sp           , file = file.path(outputDir, paste(scenario, species, "abundance_sp.rds" , sep = "_")))
saveRDS(object = out_yield_sp               , file = file.path(outputDir, paste(scenario, species, "yield_sp.rds"     , sep = "_")))
saveRDS(object = out_meanTL_sp              , file = file.path(outputDir, paste(scenario, species, "meanTL_sp.rds"    , sep = "_")))
saveRDS(object = out_meanLength_sp          , file = file.path(outputDir, paste(scenario, species, "meanLength_sp.rds", sep = "_")))
