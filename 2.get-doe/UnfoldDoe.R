# Transform the large doe file and save useful information for the simulation 
# Yansong Huang
# 15/04/2025

# read doe (design of experiments) 
doe = readRDS(file = "2.get-doe/doe/doe_0417.rds")

# transform doe matrix (copied from run_experiments_test() function)
# after transformation, matrix row are simulations, columns are parameters

doe_transformed = aperm(doe$doe, c(2,1,3))
doe_transformed_2 = matrix(doe_transformed, 
                           nrow=prod(dim(doe_transformed)[-1]),
                           ncol=dim(doe_transformed)[1],
                           byrow=TRUE)
saveRDS(doe_transformed_2, file = "2.get-doe/doe/doe_0417_transformed.rds")

# save parameter names
parameter_names <- doe$parameter

saveRDS(parameter_names, "2.get-doe/doe/par_names_0417.rds")
