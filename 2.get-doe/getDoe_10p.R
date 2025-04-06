
# Source of scripts -------------------------------------------------------

source("run_up/internal-functions.R")
source("run_up/random-sampling.R")
source("run_up/elementary-effects.R")
source("run_up/methods.R")
source("run_up/auxiliar.R")

# 1. Parameter perturbation -----------------------------------------------



  # Initial set of parameters
  parametersData = read.csv(file = "2.get-doe/data_perturbation/K.csv",
                            header = FALSE,
                            col.names = c("parameter", "value", "scale"),
                            sep = ",",
                            stringsAsFactors = FALSE)
  
  # Estimation of min and max (limits of parameter distribution)
  parametersData$percentage = rep(0.10, dim(parametersData)[1])
  parametersData = rangeEstimation(parametersData,
                                   x="value",
                                   scale = "scale",
                                   percentage = "percentage")
  parametersData = get_limits(parametersData, 
                              x="value",
                              scale = "scale",
                              range_min =  "range_min",
                              range_max = "range_max")
  
  # 2. Doe (design of experiments) ------------------------------------------
  # Building the matrix with the design of experiments (doe)
  doe = random_sampling(par = parametersData, r = 3, levels = 8, grid.jump = 4/7) # CHECK IT
  
  saveRDS(object = doe, file = "2.get-doe/doe/test_10p.rds")

