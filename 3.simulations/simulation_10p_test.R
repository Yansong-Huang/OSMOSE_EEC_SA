# simulation script for 10p scenario, adapted for pbs job array
# Yansong Huang, Criscely Lujan
# 01/04/2025

# Packages ----------------------------------------------------------------
rm(list = ls())
require(osmose)

# source("run_up/internal-functions.R")
source("run_up/random-sampling.R")
source("run_up/elementary-effects.R")
source("run_up/methods.R")
source("run_up/auxiliar.R")

# 定义路径
config_dir  = "osmose-eec"
main_file = "initial_config.csv"
config_file = file.path(config_dir, main_file)
# define the java and osmose executables
jar_file    = file.path(config_dir, "osmose_4.4.0-jar-with-dependencies.jar")

# 物种列表
species_list <- c(
  "lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole",
  "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay"
)

# 创建物种与编号的映射关系
species_codes <- setNames(0:15, species_list)


# 1. Doe (design of experiments) ------------------------------------------
# Building the matrix with the design of experiments (doe)

doe = readRDS(file = "2.get-doe/doe/split_3_0413.rds")
# 假设 X 已经定义好
# 一次性转换，注意维度 c(2,1,3)
par <- aperm(doe$doe, c(2,1,3))
par <- matrix(par, nrow=prod(dim(par)[-1]), ncol=dim(par)[1], byrow=TRUE)


# 2. run function ---------------------------------------------------------
# The user has to provide a function to evaluate for each parameter vector
# 替换函数
replace_maturity_size <- function(conf, par) {
  # 提取所有 spX 编号
  get_sp_number <- function(name) sub(".*\\.sp", "", name)
  
  ratio_names <- names(par)[grepl("^species\\.maturity\\.size\\.ratio\\.sp", names(par))]
  linf_names  <- names(par)[grepl("^species\\.linf\\.sp", names(par))]
  L0_names    <- names(par)[grepl("^species\\.l0\\.sp", names(par))]
  
  ratio_sp <- sapply(ratio_names, get_sp_number)
  Linf_sp  <- sapply(linf_names, get_sp_number)
  L0_sp    <- sapply(L0_names, get_sp_number)
  
  # 取三者交集（保证这三个值都存在的物种）
  common_sp <- Reduce(intersect, list(ratio_sp, Linf_sp, L0_sp))
  
  # 按 sp 编号排序，确保是数值顺序而非字典序
  common_sp <- sort(as.integer(common_sp))
  
  for (sp in common_sp) {
    # 构建对应名字
    ratio_name <- paste0("species.maturity.size.ratio.sp", sp)
    Linf_name  <- paste0("species.linf.sp", sp)
    L0_name    <- paste0("species.l0.sp", sp)
    target_name <- paste0("species.maturity.size.sp", sp)
    
    # 计算新的成熟体长
    new_value <- par[[ratio_name]] * (par[[Linf_name]] - par[[L0_name]]) + par[[L0_name]]
    
    # 替换进 conf
    conf[[target_name]] <- new_value
  }
  
  return(conf)
}

replace_t0 <- function(conf, par) {

  # 提取所有 spX 编号
  get_sp_number <- function(name) sub(".*\\.sp", "", name)
  
  K_names <- names(par)[grepl("^species\\.K\\.sp", names(par))]
  Linf_names  <- names(par)[grepl("^species\\.linf\\.sp", names(par))]
  L0_names    <- names(par)[grepl("^species\\.l0\\.sp", names(par))]
  
  K_sp <- sapply(K_names, get_sp_number)
  Linf_sp  <- sapply(Linf_names, get_sp_number)
  L0_sp    <- sapply(L0_names, get_sp_number)
  
  # 取三者交集（保证这三个值都存在的物种）
  common_sp <- Reduce(intersect, list(K_sp, Linf_sp, L0_sp))
  
  # 按 sp 编号排序，确保是数值顺序而非字典序
  common_sp <- sort(as.integer(common_sp))
  
  for (sp in common_sp) {
    # 构建对应名字
    K_name <- paste0("species.K.sp", sp)
    Linf_name  <- paste0("species.linf.sp", sp)
    L0_name    <- paste0("species.l0.sp", sp)
    target_name <- paste0("species.t0.sp", sp)
    
    # 计算新L0与t0
    new_l0 <- par[[L0_name]] * par[[Linf_name]]
    new_t0 <- (1/par[[K_name]])*log(1-(new_l0/par[[Linf_name]]))
    
    # 替换进 conf
    conf[[target_name]] <- new_t0
  }
  
  return(conf)
}

replace_predation_sizeratio <- function(conf, par) {
  get_sp_number <- function(name) sub(".*\\.sp", "", sub("\\.stage.*", "", name))
  get_stage_number <- function(name) sub(".*\\.stage", "", name)
  
  theta_names <- names(par)[grepl("^predation\\.predPrey\\.sizeRatio\\.teta\\.sp\\d+\\.stage\\d+", names(par))]
  alpha_names <- names(par)[grepl("^predation\\.predPrey\\.sizeRatio\\.alpha\\.sp\\d+\\.stage\\d+", names(par))]
  
  theta_df <- data.frame(
    name = theta_names,
    sp = sapply(theta_names, get_sp_number),
    stage = sapply(theta_names, get_stage_number),
    stringsAsFactors = FALSE
  )
  
  results <- split(theta_df, theta_df$sp)
  
  for (sp in names(results)) {
    stages <- results[[sp]]$stage
    min_vec <- numeric(length(stages))
    max_vec <- numeric(length(stages))
    
    for (i in seq_along(stages)) {
      stage <- stages[i]
      theta_name <- paste0("predation.predPrey.sizeRatio.teta.sp", sp, ".stage", stage)
      alpha_name <- paste0("predation.predPrey.sizeRatio.alpha.sp", sp, ".stage", stage)
      
      theta <- par[[theta_name]]
      alpha <- par[[alpha_name]]
      
      # 使用已有的 maxSlope(angle, m_min) 函数
      min_val <- 1 / maxSlope(angle = theta, m_min = 0)
      max_val <- 1 / maxSlope(angle = alpha, m_min = 1 / min_val)  # min_val 已算好
      
      min_vec[i] <- min_val
      max_vec[i] <- max_val
    }
    
    conf[[paste0("predation.predprey.sizeratio.min.sp", sp)]] <- min_vec
    conf[[paste0("predation.predprey.sizeratio.max.sp", sp)]] <- max_vec
  }
  
  return(conf)
}

update_larval_mortality <- function(species_name, par, conf) {
  # 获取物种代码
  sp_code <- species_codes[species_name]
  
  # 读取死亡率数据
  larvalMortality.sp <- read.csv(file.path(config_dir, conf[paste0("mortality.additional.larva.rate.bytdt.file.sp", sp_code)]), 
                                 stringsAsFactors = FALSE, sep = ",")
  
  # 计算对数偏移量
  lx  = log(larvalMortality.sp$x)
  mlx = mean(lx) # perturbation using mlx: lx = exp()
  dlx = lx - mlx
  
  # 获取附加死亡率并计算新的死亡率
  Lx = par[paste0("mortality.additional.larva.rate.mean.sp", sp_code)]
  new_lx = dlx + log(Lx)
  new_x = exp(new_lx)
  
  # 更新数据
  larvalMortality.sp$x = new_x
  
  # 保存修改后的数据
  modified_file <- paste0("mortality/modified_larval_mortality-sp", sp_code, ".csv")
  write.table(larvalMortality.sp, file = file.path(config_dir, modified_file), row.names = FALSE, sep = ",")
  
  # 更新配置文件
  conf[paste0("mortality.additional.larva.rate.bytdt.file.sp", sp_code)] <- modified_file
  
  return(conf)  # 返回更新后的配置
}

update_catchability_matrix <- function(conf, par) {
  # 读取原矩阵
  file_path <- file.path(config_dir, conf["fisheries.catchability.file"])
  catch_matrix <- read.csv(file_path, row.names = 1, check.names = FALSE)
  
  # 匹配参数
  param_idx <- grep("^species\\.catchability\\.fsh\\d+\\.sp\\d+$", names(par))
  param_names <- names(par)[param_idx]
  
  for (pname in param_names) {
    # 提取 fsh 和 sp 编号
    matches <- regmatches(pname, regexec("^species\\.catchability\\.fsh(\\d+)\\.sp(\\d+)$", pname))[[1]]
    fsh_idx <- as.integer(matches[2]) + 1
    sp_idx  <- as.integer(matches[3]) + 1
    
    # 替换对应位置
    catch_matrix[sp_idx, fsh_idx] <- par[[pname]]
  }
  
  # 写入新文件
  modified_file <- "fishing/modified_eec_fisheries_catchability.csv"
  write.table(catch_matrix,
              file = file.path(config_dir, modified_file),
              sep = ",", row.names = TRUE, col.names = NA)
  
  # 更新 conf
  conf["fisheries.catchability.file"] <- modified_file
  
  return(conf)
}

log_message <- function(...) {
  cat(format(Sys.time(), "%H:%M:%S"), "-", ..., "\n")
}


run_model = function(par,names, id, ...) {
  
  # set parameter names
  names(par) = names
  
  # read initial config
  conf = read_osmose(input=config_file)
  
  # temporary output directory
  output_temp = paste0("output_temp_",id)
  
  # Yansong: modify configuration according to doe
  conf_names <- names(conf)
  par_names  <- names(par)
  
  # 1. change following  parameters
  # predation.efficiency.critical predation.ingestion.rate.max mortality.starvation.rate.max species.egg.size
  # species.sexratio species.k species.length2weight.condition.factor species.linf species.maturity.size
  # species.vonbertalanffy.threshold.age fisheries.rate.base
  common_names <- intersect(conf_names, par_names)
  
  conf[common_names] <- par[common_names]
  
  # Manually changes about PREDATION SIZE RATIOS
  conf <- replace_predation_sizeratio(conf, par)
 
  # Manually changes about larval mortality
  conf <- update_larval_mortality("sole", par, conf)  
  conf <- update_larval_mortality("plaice", par, conf)  
  
  # catchability 
  conf <- update_catchability_matrix(conf, par)
  
  # maturity size
  conf <- replace_maturity_size(conf, par)
  
  # t0
  conf <- replace_t0(conf, par)
    
  # NEW configuration file
  write_osmose(conf, file = file.path(config_dir, "modified_config.csv"),sep = ",")
  
  save_conf <- function(conf, i) {
    out_path <- file.path(config_dir, sprintf("conf_simulation_%03d.csv", i))
    write.table(as.data.frame(conf), file = out_path, sep = ",", row.names = TRUE)
    log_message("Saved conf to", out_path)
  }
  
  # run Osmose Model
  
  new_config <- file.path(config_dir, "modified_config.csv")
  run_osmose(input = new_config, output = output_temp, osmose = jar_file, version = "4.4.0")
  
  # read Osmose outputs 
  data = read_osmose(path = file.path(output_temp), version = "4.4.0")
  unlink(output_temp, recursive = TRUE) # remove outputs after read the results of simulation
  
  # extract the biomass and yields variables (monthly data)
  output = list(osmose.biomass        = get_var(data, what = "biomass", expected = FALSE), # expected = FALSE, no averaging
                osmose.yield          = get_var(data, what = "yield", expected = FALSE),
                osmose.meanTL         = get_var(data, what = "meanTL", expected=FALSE),
                osmose.meanLength     = get_var(data, what = "meanSize", expected = FALSE),
                osmose.mortality      = get_var(data, what = "mortality", expected = FALSE),
                osmose.yieldBySize    = get_var(data, what="yieldBySize",expected = FALSE)
                )
  
  return(output)
}

run_experiments_test <- function(par, FUN, i=NULL, names, ..., control=list()) {
  if (is.null(control$output)) control$output = "doe"
  if (is.null(control$output.dir)) control$output.dir = getwd()
  
  dir.create(control$output.dir, recursive = TRUE, showWarnings = FALSE)
  
  if (is.null(i)) {
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) > 0) {
      i <- as.numeric(args[1])
    } else {
      stop("please provide index in pbs script")
    }
  }
  
  FUN <- match.fun(FUN)
  fn  <- function(par_row, id=0) FUN(par_row, names, id, ...)
  
  Nmax <- floor(log10(nrow(par))) + 1
  patt <- sprintf("%s_%%0%dd.rds", control$output, Nmax)
  files <- file.path(control$output.dir, sprintf(patt, seq_len(nrow(par))))
  
  out <- fn(par[i, , drop=FALSE], id=i)
  saveRDS(out, file=files[i])
}



# 3. save outputs ---------------------------------------------------------

start = date()
test_10p = run_experiments_test(
  par = par,
  FUN = run_model,
  # i = 5,
  names = doe$parameter,
  parallel = TRUE,
  control = list(
    output = "result",
    output.dir = "simulation_results_test_0415"
  )
)
end   = date()

saveRDS(object = test_10p, file = "simulation_results/test_10p_0415.rds")
