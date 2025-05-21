# Verify if simulation results are complete
# Raphaël Girardin
# 2025-05-19

# path <- "F://OSMOSE_EEC_SA-main/morris_simulation_results/"
path <- "morris_simulation_results/"
list_results <- list.files(path)
# list_results <- list.files("/scale/user/rgirardi/OSMOSE_EEC_SA-main/morris_simulation_results/")

test <- list_results[grep("result_part_1_",list_results)]
test <- gsub("result_part_1_","",test)
test <- as.numeric(gsub(".rds","",test))

test1 <- 1:49000
test1[!test1 %in% test]
range(test1[!test1 %in% test])

failed_runs <- c()

grep("result_part_1_44234.rds",list_results)

# list_results[22847:length(list_results)]
for(i in list_results){
  print(i)
  res <- tryCatch(res <- readRDS(paste0(path,i)),error = function(e){"error"})
  if(is.character(res)){failed_runs <- c(failed_runs, i)
  } else {
    if(dim(res[[1]])[1]!= 20 ) failed_runs <- c(failed_runs, i)
  }
}
failed_runs

#recheck after rerunning on datarmor

for(i in failed_runs){
  print(i)
  res <- tryCatch(res <- readRDS(paste0(path,i)),error = function(e){"error"})
  if(!is.character(res)){
    if(dim(res[[1]])[1] == 20) failed_runs <- subset(failed_runs, failed_runs!=i)
  }
}

failed_runs