
# Source of scripts -------------------------------------------------------

source("run_up/internal-functions.R")
source("run_up/random-sampling.R")
source("run_up/elementary-effects.R")
source("run_up/methods.R")
source("run_up/auxiliar.R")

# 1. Parameter perturbation -----------------------------------------------

  
  # 文件夹路径
  parameter_path <- "2.get-doe/initial_parameters"
  
  # 获取所有初始参数文件路径
  csv_files <- list.files(path = parameter_path, pattern = "\\.csv$", full.names = TRUE)
  
  # 读取并合并，添加 source 列
  parametersData <- do.call(rbind, lapply(csv_files, function(file) {
    df <- read.csv(file,
                   header = FALSE,
                   col.names = c("parameter", "value", "scale"),
                   sep = ",",
                   stringsAsFactors = FALSE)
    # 添加文件名作为新列（不含扩展名）
    df$source <- tools::file_path_sans_ext(basename(file))
    return(df)
  }))
  
  
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
  
  saveRDS(object = doe, file = "2.get-doe/doe/test_complete_10p.rds")

