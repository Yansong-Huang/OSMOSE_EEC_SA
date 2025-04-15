
source("4.indicators/indicators.R")
source("4.indicators/auxiliar_functions.R")

# baseline simulation -----------------------------------------------------

library("osmose")
# 
configDir  = "osmose-eec"
configFile = file.path(configDir, "initial_config.csv")
outputDir  = "output_baseline"

jarFile   = file.path(configDir, "osmose_4.4.0-jar-with-dependencies.jar")

run_osmose(input = configFile, output = outputDir, osmose = jarFile, version="4.4.0")# Reading outputs ---------------------------------------------------------

base_eec = read_osmose(path = file.path(outputDir))

# Estimation of indicators ------------------------------------------------

# indicator outputs
out_meanLength            = list()
out_meanTL                = list()
out_lfi40                 = list()

out_biomass_sp             = list()
out_yield_sp               = list()
out_meanTL_sp              = list()
out_meanLength_sp          = list()

# getting variables
biomass        = base_eec$biomass
yield          = base_eec$yield
meanTL         = base_eec$meanTL
meanLength     = base_eec$meanSize
yield_by_size   = base_eec$yieldBySize


out_meanLength            = .MeanLength(biomass, meanLength)
out_meanTL                = .MeanTL(biomass, meanTL)
out_lfi40                 = .LFI(yield, yield_by_size, thr = 40)

out_biomass_sp             = biomass
out_yield_sp               = yield
out_meanTL_sp              = meanTL
out_meanLength_sp          = meanLength

output_dir = "5.baseline/indicators_baseline"
dir.create(output_dir,recursive = TRUE, showWarnings = FALSE)

saveRDS(object = out_meanLength            , file = file.path(output_dir, paste0("baseline_", "meanLength.rds")))
saveRDS(object = out_meanTL                , file = file.path(output_dir, paste0("baseline_", "meanTL.rds")))
saveRDS(object = out_lfi40                 , file = file.path(output_dir, paste0("baseline_", "lfi40.rds")))

saveRDS(object = out_biomass_sp    , file = file.path(output_dir, paste0("baseline_", "biomass_sp.rds")))
saveRDS(object = out_yield_sp      , file = file.path(output_dir, paste0("baseline_", "yield_sp.rds")))
saveRDS(object = out_meanTL_sp     , file = file.path(output_dir, paste0("baseline_", "meanTL_sp.rds")))
saveRDS(object = out_meanLength_sp , file = file.path(output_dir, paste0("baseline_", "meanLength_sp.rds")))
