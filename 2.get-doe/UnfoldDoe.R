# Transform the large doe file and save parameter names for the simulation 
# Yansong Huang
# 15/04/2025

# read doe (design of experiments) 
doe = readRDS(file = "2.get-doe/doe/doe_0423.rds")

# transform doe matrix (copied from run_experiments_test() function)
# after transformation, matrix row are simulations, columns are parameters

doe_transformed = aperm(doe$doe, c(2,1,3))
doe_transformed_2 = matrix(doe_transformed, 
                           nrow=prod(dim(doe_transformed)[-1]),
                           ncol=dim(doe_transformed)[1],
                           byrow=TRUE)
# split the matrix
split_doe <- function(complete_doe, n_splits = 4) {
  r_total <- dim(complete_doe)[1]  # 总轨迹数量，比如196000
  split_size <- ceiling(r_total / n_splits)
  split_list <- vector("list", n_splits)
  
  for (k in 1:n_splits) {
    idx_start <- (k - 1) * split_size + 1
    idx_end   <- min(k * split_size, r_total)
    
    splited_doe      <- complete_doe[idx_start:idx_end,]
    split_list[[k]] <- splited_doe
  }
  return(split_list)
}

splits <- split_doe(doe_transformed_2, n_split=4)

for (j in 1:4) {
  saveRDS(splits[[j]],file.path("2.get-doe/doe",paste0("doe_0423_part_", j, ".rds"))) 
}


# save parameter names
parameter_names <- doe$parameter

saveRDS(parameter_names, "2.get-doe/doe/par_names_0423.rds")
