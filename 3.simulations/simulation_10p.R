
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
  # theta.sp0.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp0.stage1"]) * (pi/2)
  # alpha.sp0.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp0.stage1"]) * ((pi/2)-theta.sp0.stage1)
  # min.sp0.stage1 = 1/maxSlope(angle = theta.sp0.stage1, m_min = 0)
  # max.sp0.stage1 = 1/maxSlope(angle = alpha.sp0.stage1, m_min = 1/min.sp0.stage1)
  # 
  # theta.sp0.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp0.stage2"]) * (pi/2)
  # alpha.sp0.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp0.stage2"]) * ((pi/2)-theta.sp0.stage2)
  # min.sp0.stage2 = 1/maxSlope(angle = theta.sp0.stage2, m_min = 0)
  # max.sp0.stage2 = 1/maxSlope(angle = alpha.sp0.stage2, m_min = 1/min.sp0.stage2)
  # 
  # modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.max.sp0", c(2,3)] = c(max.sp0.stage1, max.sp0.stage2)
  # modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.min.sp0", c(2,3)] = c(min.sp0.stage1, min.sp0.stage2)
  # 
  
  # # PredPrey stage threshold
  # Linf.sp0.per = par[names(par) == "species.lInf.sp0"]
  # modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp0", 2]    = par[names(par) == "predation.predPrey.stage.threshold.sp0"] * (Linf.sp0.per)
  
  
  # # Starvation rate max
  # modelConfig[modelConfig[,1] == "mortality.starvation.rate.max.sp0", 2]         = par[names(par) == "mortality.starvation.rate.max.sp0"]
  
  
  # # vonBertalanffy threshold
  # amax.sp0 = as.numeric(modelConfig[modelConfig[,1] == "species.lifespan.sp0", 2])
  # modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp0", 2]  = par[names(par) == "species.vonbertalanffy.threshold.age.sp0"] * (amax.sp0)
  
  
  # Manually changes about egg SIZE AND WEIGHT
  # eggSize.sp0   = as.numeric(modelConfig[modelConfig[,1] == "species.egg.size.sp0", 2])
  # eggWeight.sp0 = as.numeric(modelConfig[modelConfig[,1] == "species.egg.weight.sp0", 2])
  # meanDensity.sp0 = eggWeight.sp0 / ((4/3 * pi) * (eggSize.sp0/2)^3) 
  # eggSize     = par[names(par) == "species.egg.size.sp0"]
  # eggWeight   = (4/3 * pi) * (as.numeric(eggSize)/2)^3 * meanDensity.sp0
  # modelConfig[modelConfig[,1] == "species.egg.weight.sp0", 2] = eggWeight
  # modelConfig[modelConfig[,1] == "species.egg.size.sp0", 2]   = eggSize
  
  
  # Critical efficiency and predation ingestion rate 
  # modelConfig[modelConfig[,1] == "predation.efficiency.critical.sp0", 2]  = par[names(par) == "predation.efficiency.critical.sp0"]
  # modelConfig[modelConfig[,1] == "predation.ingestion.rate.max.sp0", 2]   = par[names(par) == "predation.ingestion.rate.max.sp0"]
  # 
  
  # Natural mortality
  # modelConfig[modelConfig[,1] == "mortality.natural.rate.sp0", 2]  = par[names(par) == "mortality.natural.rate.sp0"]
  
  
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

  
  # Sex ratio
  # modelConfig[modelConfig[,1] == "species.sexratio.sp0", 2]  = par[names(par) == "species.sexratio.sp0"]
  
  
  # Von Bertalanffy parameters: l0 perturbed instead of t0
  # K.sp0    = par[names(par) == "species.K.sp0"]
  conf[grepl("species\\.k\\.sp", names(conf))] <- par[grepl("species\\.k\\.sp", names(par))]
  conf[grepl("species\\.linf\\.sp", names(conf))] <- par[grepl("species\\.linf\\.sp", names(par))]
  # Linf.sp0 = par[names(par) == "species.lInf.sp0"]
  # 
  # l0.sp0      = par[names(par) == "species.l0.sp0"]
  # newl0.sp0   = l0.sp0 * (Linf.sp0)
  # newt0.sp0   = (1 / K.sp0) * (log(1 - (newl0.sp0 / Linf.sp0)))
  # modelConfig[modelConfig[,1] == "species.t0.sp0", 2]  = newt0.sp0
  
  # Von Bertalanffy parameters: K and lInf
  # modelConfig[modelConfig[,1] == "species.K.sp0", 2]              = par[names(par) == "species.K.sp0"]
  # modelConfig[modelConfig[,1] == "species.lInf.sp0", 2]           = par[names(par) == "species.lInf.sp0"]
  
  # maturity size
  # sx.sp0   = par[names(par) == "species.maturity.size.sp0"]
  # smat.sp0 = ((sx.sp0)*(Linf.sp0.per - newl0.sp0)) + newl0.sp0
  # modelConfig[modelConfig[,1] == "species.maturity.size.sp0", 2]  = smat.sp0
  
  # Length to weight relationship: condition factor perturbed
  # modelConfig[modelConfig[,1] == "species.length2weight.condition.factor.sp0", 2]  =  par[names(par) == "species.length2weight.condition.factor.sp0"]
  
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
