
# Source of scripts -------------------------------------------------------

source("run_up/internal-functions.R")
source("run_up/random-sampling.R")
source("run_up/elementary-effects.R")
source("run_up/methods.R")
source("run_up/auxiliar.R")

# 1. Parameter perturbation -----------------------------------------------

species = paste0("sp", c(0:8))

for(i in seq_along(species)){
  
  # Initial set of parameters: testing 27
  parametersData = read.csv(file = paste0("2.get-doe/dataPerturbation/osm_parameters_", species[i], ".csv"),
                            header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  # Estimation of min and max (limits of parameter distribution)
  parametersData$percentage = rep(0.10, dim(parametersData)[1])
  parametersData = rangeEstimation(parametersData, percentage = "percentage")
  parametersData = get_limits(parametersData, range_min =  "range_min", range_max = "range_max")
  
  # 2. Doe (design of experiments) ------------------------------------------
  # Building the matrix with the design of experiments (doe)
  doe = random_sampling(par = parametersData, r = 3, levels = 8, grid.jump = 4/7) # CHECK IT
  
  saveRDS(object = doe, file = paste0("2.get-doe/doe/10p/doe_", species[i], "_10p.rds"))
  
}
