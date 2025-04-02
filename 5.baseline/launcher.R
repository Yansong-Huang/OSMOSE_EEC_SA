

source("4.indicators/indicators.R")
source("4.indicators/auxiliar_functions.R")

# Baseline simulation -----------------------------------------------------

library("osmose")
setwd("/run/media/criscelylp/Disk1/RUN/run_uncertainty_osmose-datarmor/indicators_baseLine")

configDir  = "osmose-hum_v3"
configFile = file.path(configDir, "osmose_hum.csv")
outputDir  = "output_baseLine"

jarFile   = file.path(configDir, "osmose.jar")

run_osmose(input = configFile, output = outputDir, osmose = jarFile, version=3)

# Reading outputs ---------------------------------------------------------

base_hum3 = read_osmose(path = file.path(outputDir), version = 3)

# Estimation of indicators ------------------------------------------------

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

#new
out_biomass_sp             = list()
out_abundance_sp           = list()
out_yield_sp               = list()
out_meanTL_sp              = list()
out_meanLength_sp          = list()

# getting variables
biomass        = base_hum3$biomass
abundance      = base_hum3$abundance
yield          = base_hum3$yield
meanTL         = base_hum3$meanTL
meanLength     = base_hum3$meanSize
sizeSpectrum   = base_hum3$SizeSpectrum
sizeSpectrumB  = base_hum3$SizeSpectrumB

out_meanLength            = .MeanLength(abundance, meanLength)
out_meanTL                = .MeanTL(biomass, meanTL)
out_meanLifespan          = .MeanLifespan(biomass)
out_biomassOverYield      = .BiomassOverYield(biomass, yield)
out_mti                   = .MTI(meanTL, yield)
out_slopeSizeSpectrum     = .FitSizeSpectrum(sizeSpectrum, output = "slope")
out_interceptSizeSpectrum = .FitSizeSpectrum(sizeSpectrum, output = "intercept")
out_lfi20                 = .LFI(biomass, sizeSpectrumB, thr = 20)
out_lfi30                 = .LFI(biomass, sizeSpectrumB, thr = 30)
out_lfi40                 = .LFI(biomass, sizeSpectrumB, thr = 40)

out_biomass_sp             = biomass
out_abundance_sp           = abundance
out_yield_sp               = yield
out_meanTL_sp              = meanTL
out_meanLength_sp          = meanLength

outInd = "indicators_baseLine"
dir.create(outInd)

saveRDS(object = out_meanLength            , file = file.path(outInd, paste0("baseLine_", "meanLength.rds")))
saveRDS(object = out_meanTL                , file = file.path(outInd, paste0("baseLine_", "meanTL.rds")))
saveRDS(object = out_meanLifespan          , file = file.path(outInd, paste0("baseLine_", "meanLifespan.rds")))
saveRDS(object = out_biomassOverYield      , file = file.path(outInd, paste0("baseLine_", "biomassOverYield.rds")))
saveRDS(object = out_mti                   , file = file.path(outInd, paste0("baseLine_", "mti.rds")))
saveRDS(object = out_slopeSizeSpectrum     , file = file.path(outInd, paste0("baseLine_", "slopeSizeSpectrum.rds")))
saveRDS(object = out_interceptSizeSpectrum , file = file.path(outInd, paste0("baseLine_", "interceptSizeSpectrum.rds")))
saveRDS(object = out_lfi20                 , file = file.path(outInd, paste0("baseLine_", "lfi20.rds")))
saveRDS(object = out_lfi30                 , file = file.path(outInd, paste0("baseLine_", "lfi30.rds")))
saveRDS(object = out_lfi40                 , file = file.path(outInd, paste0("baseLine_", "lfi40.rds")))

saveRDS(object = out_biomass_sp    , file = file.path(outInd, paste0("baseLine_", "biomass_sp.rds")))
saveRDS(object = out_abundance_sp  , file = file.path(outInd, paste0("baseLine_", "abundance_sp.rds")))
saveRDS(object = out_yield_sp      , file = file.path(outInd, paste0("baseLine_", "yield_sp.rds")))
saveRDS(object = out_meanTL_sp     , file = file.path(outInd, paste0("baseLine_", "meanTL_sp.rds")))
saveRDS(object = out_meanLength_sp , file = file.path(outInd, paste0("baseLine_", "meanLength_sp.rds")))
