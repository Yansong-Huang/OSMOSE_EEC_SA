# read doe (design of experiments) 
doe = readRDS(file = "2.get-doe/doe/split_3_0413.rds")

# transform doe matrix (copied from run_experiments_test() function)
# after transformation, matrix row are simulations, columns are parameters

doe_transformed = aperm(doe$doe, c(2,1,3))
doe_transformed_2 = matrix(doe_transformed, 
                           nrow=prod(dim(doe_transformed)[-1]),
                           ncol=dim(doe_transformed)[1],
                           byrow=TRUE)
