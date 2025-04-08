
# Packages ----------------------------------------------------------------
rm(list = ls())
require(osmose)

# Source of scripts -------------------------------------------------------

source("run_up/internal-functions.R")
source("run_up/random-sampling.R")
source("run_up/elementary-effects.R")
source("run_up/methods.R")
source("run_up/auxiliar.R")

# 1. Doe (design of experiments) ------------------------------------------
# Building the matrix with the design of experiments (doe)

doe = readRDS(file = "2.get-doe/doe/test_complete_10p.rds")

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
  
  # 按sp编号排序
  common_sp <- sort(common_sp)
  
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


run_model = function(par,names, ...) {
  
  # set parameter names
  names(par) = names
  
  # 读取配置数据
  config_dir  = "osmose-eec"
  main_file = "initial_config.csv"
  config_file = file.path(config_dir, main_file)
  
  conf = read_osmose(input=config_file)
  
  # define the java and osmose executables
  # configDir  = "osmose-peru"
  jar_file    = file.path(config_dir, "osmose_4.4.0-jar-with-dependencies.jar")
  
  # # initial configuration file
  # modelConfig = read.csv(file = file.path(configDir, "config.csv"), stringsAsFactors = FALSE, na.strings = c(""))
  
  # temporary output directory
  output_temp = "output_temp"
  
  # Yansong: modify configuration according to doe
  conf_names <- names(conf)
  par_names  <- names(par)
  
  # 取交集，直接替换以下参数
  # predation.efficiency.critical predation.ingestion.rate.max mortality.starvation.rate.max species.egg.size
  # species.sexratio species.k species.length2weight.condition.factor species.linf species.maturity.size
  # species.vonbertalanffy.threshold.age
  common_names <- intersect(conf_names, par_names)
  
  # 用名字赋值，确保一一对应
  conf[common_names] <- par[common_names]
  
  # Manually changes about PREDATION ACCESSIBILITY
  # predationAccessibility = read.csv(file.path(configDir, "input/predation-accessibility.csv"), stringsAsFactors = FALSE, sep = ",")
  # pred = as.matrix(predationAccessibility[,-1])
  # 
  # predx = tail(pred[sp, ]/pred[1:9, sp], -1) 
  # 
  # predationAccessibility$anchovy[c(1:9)[-sp]] = pmin(par[c(1:8)], 1)
  # predationAccessibility$hake[sp]             = pmin(predx[1]*par[1], 1)
  # predationAccessibility$sardine[sp]          = pmin(predx[2]*par[2], 1)
  # predationAccessibility$jurel[sp]            = pmin(predx[3]*par[3], 1)
  # predationAccessibility$caballa[sp]          = pmin(predx[4]*par[4], 1)
  # predationAccessibility$meso[sp]             = pmin(predx[5]*par[5], 1)
  # predationAccessibility$munida[sp]           = pmin(predx[6]*par[6], 1)
  # predationAccessibility$pota[sp]             = pmin(predx[7]*par[7], 1)
  # predationAccessibility$euphausidos[sp]      = pmin(predx[8]*par[8], 1)
  # colnames(predationAccessibility)[1] = ""
  # write.table(predationAccessibility, file = file.path(configDir, "newPredationAccessibility.csv"), row.names = FALSE, sep = ";")
  # modelConfig[modelConfig[,1] == "predation.accessibility.file", 2] = "newPredationAccessibility.csv"
  
  
  # Manually changes about PREDATION SIZE RATIOS
  conf <- replace_predation_sizeratio(conf, par)
  # theta.sp0.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp0.stage1"]) * (pi/2)
  # alpha.sp0.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp0.stage1"]) * ((pi/2)-theta.sp0.stage1)
  # min.sp0.stage1 = 1/maxSlope(angle = theta.sp0.stage1, m_min = 0)
  # max.sp0.stage1 = 1/maxSlope(angle = alpha.sp0.stage1, m_min = 1/min.sp0.stage1)
  # 
  
  # Manually changes about larval mortality: 19 par but perturbing the mean
  # larvalMortality.sp0 = read.csv(file.path(configDir, "input/larval/larval_mortality-anchovy.csv"), stringsAsFactors = FALSE, sep = ",")
  # lx  = log(larvalMortality.sp0$larval_mortality_by_dt)
  # mlx = mean(lx) # perturbation using mlx: lx = exp()
  # dlx = lx - mlx
  # 
  # Lx = par[names(par) == "mortality.natural.larva.rate.Lx.sp0"]
  # new_lx = dlx + log(Lx)
  # new_x = exp(new_lx)
  # 
  # newLarvalMortality.sp = larvalMortality.sp0
  # newLarvalMortality.sp$larval_mortality_by_dt = new_x
  # colnames(newLarvalMortality.sp)[1] = ""
  # write.table(newLarvalMortality.sp, file = file.path(configDir, "newLavalMortality-anchovy.csv"), row.names = FALSE, sep = ",")
  # modelConfig[modelConfig[,1] == "mortality.natural.larva.rate.bytDt.file.sp0", 2] = "newLavalMortality-anchovy.csv"
  
  # catchability #### TO CHECK

  
  # maturity size
  conf <- replace_maturity_size(conf, par)
  
  # sx.sp0   = par[names(par) == "species.maturity.size.sp0"]
  # smat.sp0 = ((sx.sp0)*(Linf.sp0.per - newl0.sp0)) + newl0.sp0
  # modelConfig[modelConfig[,1] == "species.maturity.size.sp0", 2]  = smat.sp0
  
  # NEW configuration file
  write_osmose(conf,file = file.path(config_dir, "modified_config.csv"),sep = ",")
  
  # run Osmose Model
  # 检查校准参数文档中的内容是否已经迁移
  
  new_config <- file.path(config_dir, "modified_config.csv")
  run_osmose(input = new_config, output = output_temp, osmose = jar_file, version = "4.4.0")
  
  # read Osmose outputs 
  data = read_osmose(path = file.path(output_temp), version = "4.4.0")
  unlink(output_temp, recursive = TRUE) # remove outputs after read the results of simulation
  
  # extract the biomass and yields variables (monthly data)
  output = list(osmose.biomass        = get_var(data, what = "biomass", expected = FALSE),
                osmose.abundance      = get_var(data, what = "abundance", expected = FALSE),
                osmose.yield          = get_var(data, what = "yield", expected = FALSE),
                osmose.yieldN         = get_var(data, what = "yieldN", expected = FALSE),
                osmose.meanTL         = get_var(data, what = "meanTL", type="trophic", expected=FALSE),
                osmose.meanLength     = get_var(data, what = "meanSize", expected = FALSE) #,
                # osmose.sizeSpectrum   = get_var(data, what = "SizeSpectrum", expected=FALSE),
                # osmose.sizeSpectrumN  = get_var(data, what = "SizeSpectrumN", expected=FALSE),
                # osmose.sizeSpectrumB  = get_var(data, what = "SizeSpectrumB", expected=FALSE),
                # osmose.sizeSpectrumC  = get_var(data, what = "SizeSpectrumC", expected=FALSE),
                # osmose.sizeSpectrumY  = get_var(data, what = "SizeSpectrumY", expected=FALSE)
                )
  
  return(output)
}

# 3. save outputs ---------------------------------------------------------

start = date()
test_10p = run_experiments(X = doe, FUN = run_model, names=doe$parameter,  parallel=TRUE)
end   = date()

saveRDS(object = test_10p, file = "output_Yansong/test_10p.rds")
