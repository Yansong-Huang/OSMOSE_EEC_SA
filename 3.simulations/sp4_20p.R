
# Packages ----------------------------------------------------------------
rm(list = ls())
require(osmose)

# Source of scripts -------------------------------------------------------

setwd("/home1/datahome/clujanpa/sensitivity_osmose")

source("internal-functions.R")
source("random-sampling.R")
source("elementary-effects.R")
source("methods.R")
source("auxiliar.R")

# 1. Doe (design of experiments) ------------------------------------------
# Building the matrix with the design of experiments (doe)

setwd("/home/datawork-marbec-scenlab/OSMOSE/Criscely/ua_osmose_paper/outputs_uncertainty/sp4_20p")

doe = readRDS(file = "doe_sp4_20p.rds")

# 2. run function ---------------------------------------------------------
# The user has to provide a function to evaluate for each parameter vector

run_model = function(par, names, ...) {
  
  # set parameter names
  names(par) = names
  
  # define the java and osmose executables
  configDir  = "osmose-hum_v3"
  jarFile    = file.path(configDir, "osmose.jar")
  
  # initial configuration file
  modelConfig = read.csv(file = file.path(configDir, "osmose_hum.csv"), stringsAsFactors = FALSE, na.strings = c(""))
  
  # output directory
  outputDir = "output"
  
  sp = (4+1) #species4
 
  
  # Manually changes about PREDATION ACCESSIBILITY
  predationAccessibility = read.csv(file.path(configDir, "input/predation/predation-accessibility.csv"), stringsAsFactors = FALSE, sep = ";")
  pred = as.matrix(predationAccessibility[,-1])
  
  predx = (pred[sp, ]/pred[1:9, sp])[-sp]
  
  predationAccessibility$anchovy[sp]          = pmin(predx[1]*par[1], 1)
  predationAccessibility$hake[sp]             = pmin(predx[2]*par[2], 1)
  predationAccessibility$sardine[sp]          = pmin(predx[3]*par[3], 1)
  predationAccessibility$jurel[sp]            = pmin(predx[4]*par[4], 1)
  predationAccessibility$caballa[c(1:9)[-sp]] = pmin(par[c(1:8)], 1)
  predationAccessibility$meso[sp]             = pmin(predx[5]*par[5], 1)
  predationAccessibility$munida[sp]           = pmin(predx[6]*par[6], 1)
  predationAccessibility$pota[sp]             = pmin(predx[7]*par[7], 1)
  predationAccessibility$euphausidos[sp]      = pmin(predx[8]*par[8], 1)
  colnames(predationAccessibility)[1] = ""
  write.table(predationAccessibility, file = file.path(configDir, "newPredationAccessibility.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "predation.accessibility.file", 2] = "newPredationAccessibility.csv"
  
  
  # Manually changes about PREDATION SIZE RATIOS
  theta.sp4.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp4.stage1"]) * (pi/2)
  alpha.sp4.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp4.stage1"]) * ((pi/2)-theta.sp4.stage1)
  min.sp4.stage1 = 1/maxSlope(angle = theta.sp4.stage1, m_min = 0)
  max.sp4.stage1 = 1/maxSlope(angle = alpha.sp4.stage1, m_min = 1/min.sp4.stage1)
  
  theta.sp4.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp4.stage2"]) * (pi/2)
  alpha.sp4.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp4.stage2"]) * ((pi/2)-theta.sp4.stage2)
  min.sp4.stage2 = 1/maxSlope(angle = theta.sp4.stage2, m_min = 0)
  max.sp4.stage2 = 1/maxSlope(angle = alpha.sp4.stage2, m_min = 1/min.sp4.stage2)
  
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.max.sp4", c(2,3)] = c(max.sp4.stage1, max.sp4.stage2)
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.min.sp4", c(2,3)] = c(min.sp4.stage1, min.sp4.stage2)
  
  
  # PredPrey stage threshold
  Linf.sp4.per = par[names(par) == "species.lInf.sp4"]
  modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp4", 2]    = par[names(par) == "predation.predPrey.stage.threshold.sp4"] * (Linf.sp4.per)
  
  
  # Starvation rate max
  modelConfig[modelConfig[,1] == "mortality.starvation.rate.max.sp4", 2]         = par[names(par) == "mortality.starvation.rate.max.sp4"]
  
  
  # vonBertalanffy threshold
  amax.sp4 = as.numeric(modelConfig[modelConfig[,1] == "species.lifespan.sp4", 2])
  modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp4", 2]  = par[names(par) == "species.vonbertalanffy.threshold.age.sp4"] * (amax.sp4)
  
  
  # Manually changes about egg SIZE AND WEIGHT
  eggSize.sp4   = as.numeric(modelConfig[modelConfig[,1] == "species.egg.size.sp4", 2])
  eggWeight.sp4 = as.numeric(modelConfig[modelConfig[,1] == "species.egg.weight.sp4", 2])
  meanDensity.sp4 = eggWeight.sp4 / ((4/3 * pi) * (eggSize.sp4/2)^3) 
  eggSize     = par[names(par) == "species.egg.size.sp4"]
  eggWeight   = (4/3 * pi) * (as.numeric(eggSize)/2)^3 * meanDensity.sp4
  modelConfig[modelConfig[,1] == "species.egg.weight.sp4", 2] = eggWeight
  modelConfig[modelConfig[,1] == "species.egg.size.sp4", 2]   = eggSize
  
  
  # Critical efficiency and predation ingestion rate 
  modelConfig[modelConfig[,1] == "predation.efficiency.critical.sp4", 2]  = par[names(par) == "predation.efficiency.critical.sp4"]
  modelConfig[modelConfig[,1] == "predation.ingestion.rate.max.sp4", 2]   = par[names(par) == "predation.ingestion.rate.max.sp4"]
  
  
  # Natural mortality
  modelConfig[modelConfig[,1] == "mortality.natural.rate.sp4", 2]  = par[names(par) == "mortality.natural.rate.sp4"]
 
   
  # Manually changes about larval mortality: 19 par but perturbing the mean
  larvalMortality.sp4 = read.csv(file.path(configDir, "input/larval/larval_mortality-caballa.csv"), stringsAsFactors = FALSE, sep = ";")
  lx  = log(larvalMortality.sp4$x)
  mlx = mean(lx) # perturbation using mlx: lx = exp()
  dlx = lx - mlx
  
  Lx = par[names(par) == "mortality.natural.larva.rate.Lx.sp4"]
  new_lx = dlx + log(Lx)
  new_x = exp(new_lx)
  
  newLarvalMortality.sp = larvalMortality.sp4
  newLarvalMortality.sp$x = new_x
  colnames(newLarvalMortality.sp)[1] = ""
  write.table(newLarvalMortality.sp, file = file.path(configDir, "newLavalMortality-caballa.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.natural.larva.rate.bytDt.file.sp4", 2] = "newLavalMortality-caballa.csv"
  
  
  # Manually changes about fishing mortality #### TO CHECK
  # 224 parameters: 1 (f media) + T (between years) + 12T (distribution of the fishing between years) + 2 (fishing selectivity)
  fishing_multiplier    = par[names(par) == "fishing.multiplier.sp4"]
  fishingMortality.sp4  = read.csv(file.path(configDir, "input/fishing/F-caballa.csv"), stringsAsFactors = FALSE, sep = ";")
  fishingMortality.sp4[, c(2:dim(fishingMortality.sp4)[2])] = fishingMortality.sp4[, c(2:dim(fishingMortality.sp4)[2])] * fishing_multiplier
  colnames(fishingMortality.sp4) = c("", as.numeric(substring(colnames(fishingMortality.sp4)[-1], 2)))
  write.table(fishingMortality.sp4, file = file.path(configDir, "newFishingMortality-caballa.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.fishing.rate.byDt.bySize.file.sp4", 2] = "newFishingMortality-caballa.csv"
  
  
  # Sex ratio
  modelConfig[modelConfig[,1] == "species.sexratio.sp4", 2]  = par[names(par) == "species.sexratio.sp4"]
  
  
  # Von Bertalanffy parameters: l0 perturbed instead of t0
  K.sp4    = par[names(par) == "species.K.sp4"]
  Linf.sp4 = par[names(par) == "species.lInf.sp4"]
  
  l0.sp4      = par[names(par) == "species.l0.sp4"]
  newl0.sp4   = l0.sp4 * (Linf.sp4)
  newt0.sp4   = (1 / K.sp4) * (log(1 - (newl0.sp4 / Linf.sp4)))
  modelConfig[modelConfig[,1] == "species.t0.sp4", 2]  = newt0.sp4
  
  
  # Von Bertalanffy parameters: K and lInf
  modelConfig[modelConfig[,1] == "species.K.sp4", 2]              = par[names(par) == "species.K.sp4"]
  modelConfig[modelConfig[,1] == "species.lInf.sp4", 2]           = par[names(par) == "species.lInf.sp4"]
  
  # maturity size
  sx.sp4   = par[names(par) == "species.maturity.size.sp4"]
  smat.sp4 = ((sx.sp4)*(Linf.sp4.per - newl0.sp4)) + newl0.sp4
  modelConfig[modelConfig[,1] == "species.maturity.size.sp4", 2]  = smat.sp4
  
  # Length to weight relationship: condition factor perturbed
  modelConfig[modelConfig[,1] == "species.length2weight.condition.factor.sp4", 2]  =  par[names(par) == "species.length2weight.condition.factor.sp4"]
  
  # NEW configuration file
  write.table(modelConfig, file = file.path(configDir, "config.csv"), na = "", sep = ",",
              quote = FALSE, row.names = FALSE, col.names = c(colnames(modelConfig)[c(1:2)], "", ""))
  
  # run Osmose Model
  run_osmose(input = file.path(configDir, "config.csv"), output = outputDir, osmose = jarFile, version = 3)
  
  # read Osmose outputs 
  data = read_osmose(path = file.path(outputDir), version = 3)
  unlink(outputDir, recursive = TRUE) # remove outputs after read the results of simulation
  
  # extract the biomass and yields variables (monthly data)
  output = list(osmose.biomass        = get_var(data, what = "biomass", expected = FALSE),
                osmose.abundance      = get_var(data, what = "abundance", expected = FALSE),
                osmose.yield          = get_var(data, what = "yield", expected = FALSE),
                osmose.yieldN         = get_var(data, what = "yieldN", expected = FALSE),
                osmose.meanTL         = get_var(data, what = "meanTL", type="trophic", expected=FALSE),
                osmose.meanLength     = get_var(data, what = "meanSize", expected = FALSE),
                osmose.sizeSpectrum   = get_var(data, what = "SizeSpectrum", expected=FALSE),
                osmose.sizeSpectrumN  = get_var(data, what = "SizeSpectrumN", expected=FALSE),
                osmose.sizeSpectrumB  = get_var(data, what = "SizeSpectrumB", expected=FALSE),
                osmose.sizeSpectrumC  = get_var(data, what = "SizeSpectrumC", expected=FALSE),
                osmose.sizeSpectrumY  = get_var(data, what = "SizeSpectrumY", expected=FALSE))
  
  return(output)
}

# 3. save outputs ---------------------------------------------------------

start = date()
fixed20p_sp4 = run_experiments(X = doe, FUN = run_model, names=doe$parameter, parallel=TRUE)
end   = date()

saveRDS(object = fixed20p_sp4, file = "fixed20p_sp4.rds")
