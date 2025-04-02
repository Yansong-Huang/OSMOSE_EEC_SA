
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

setwd("/home/datawork-marbec-scenlab/OSMOSE/Criscely/ua_osmose_paper/outputs_uncertainty/sp7_10p")

doe = readRDS(file = "doe_sp7_10p.rds")

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
  
  sp = (7+1) #species7
  
  
  # Manually changes about PREDATION ACCESSIBILITY
  predationAccessibility = read.csv(file.path(configDir, "input/predation/predation-accessibility.csv"), stringsAsFactors = FALSE, sep = ";")
  pred = as.matrix(predationAccessibility[,-1])
  
  predx = (pred[sp, ]/pred[1:9, sp])[-sp]
  
  predationAccessibility$anchovy[sp]          = pmin(predx[1]*par[1], 1)
  predationAccessibility$hake[sp]             = pmin(predx[2]*par[2], 1)
  predationAccessibility$sardine[sp]          = pmin(predx[3]*par[3], 1)
  predationAccessibility$jurel[sp]            = pmin(predx[4]*par[4], 1)
  predationAccessibility$caballa[sp]          = pmin(predx[5]*par[5], 1)
  predationAccessibility$meso[sp]             = pmin(predx[6]*par[6], 1)
  predationAccessibility$munida[sp]           = pmin(predx[7]*par[7], 1)
  predationAccessibility$pota[c(1:9)[-sp]]    = pmin(par[c(1:8)], 1)
  predationAccessibility$euphausidos[sp]      = pmin(predx[8]*par[8], 1)
  colnames(predationAccessibility)[1] = ""
  write.table(predationAccessibility, file = file.path(configDir, "newPredationAccessibility.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "predation.accessibility.file", 2] = "newPredationAccessibility.csv"
  
  
  # Manually changes about PREDATION SIZE RATIOS
  theta.sp7.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp7.stage1"]) * (pi/2)
  alpha.sp7.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp7.stage1"]) * ((pi/2)-theta.sp7.stage1)
  min.sp7.stage1 = 1/maxSlope(angle = theta.sp7.stage1, m_min = 0)
  max.sp7.stage1 = 1/maxSlope(angle = alpha.sp7.stage1, m_min = 1/min.sp7.stage1)
  
  theta.sp7.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp7.stage2"]) * (pi/2)
  alpha.sp7.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp7.stage2"]) * ((pi/2)-theta.sp7.stage2)
  min.sp7.stage2 = 1/maxSlope(angle = theta.sp7.stage2, m_min = 0)
  max.sp7.stage2 = 1/maxSlope(angle = alpha.sp7.stage2, m_min = 1/min.sp7.stage2)
  
  theta.sp7.stage3   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp7.stage3"]) * (pi/2)
  alpha.sp7.stage3   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp7.stage3"]) * ((pi/2)-theta.sp7.stage3)
  min.sp7.stage3 = 1/maxSlope(angle = theta.sp7.stage3, m_min = 0)
  max.sp7.stage3 = 1/maxSlope(angle = alpha.sp7.stage3, m_min = 1/min.sp7.stage3)
  
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.max.sp7", c(2,3,4)] = c(max.sp7.stage1, max.sp7.stage2, max.sp7.stage3)
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.min.sp7", c(2,3,4)] = c(min.sp7.stage1, min.sp7.stage2, min.sp7.stage3)
  
  
  # PredPrey stage threshold
  Linf.sp7.per = par[names(par) == "species.lInf.sp7"]
  sp7.stage2   = par[names(par) == "predation.predPrey.stage.threshold.sp7.stage2.frac"] * (Linf.sp7.per)
  sp7.ratio    = par[names(par) == "predation.predPrey.stage.threshold.sp7.ratio"]
  sp7.stage1   = sp7.ratio * sp7.stage2
  modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp7", c(2,3)]    = c(sp7.stage1, sp7.stage2)
 
   
  # Starvation rate max
  modelConfig[modelConfig[,1] == "mortality.starvation.rate.max.sp7", 2]         = par[names(par) == "mortality.starvation.rate.max.sp7"]
  
  
  # vonBertalanffy threshold
  amax.sp7 = as.numeric(modelConfig[modelConfig[,1] == "species.lifespan.sp7", 2])
  modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp7", 2]  = par[names(par) == "species.vonbertalanffy.threshold.age.sp7"] * (amax.sp7)
  
  
  # Manually changes about egg SIZE AND WEIGHT
  eggSize.sp7   = as.numeric(modelConfig[modelConfig[,1] == "species.egg.size.sp7", 2])
  eggWeight.sp7 = as.numeric(modelConfig[modelConfig[,1] == "species.egg.weight.sp7", 2])
  meanDensity.sp7 = eggWeight.sp7 / ((4/3 * pi) * (eggSize.sp7/2)^3) 
  eggSize     = par[names(par) == "species.egg.size.sp7"]
  eggWeight   = (4/3 * pi) * (as.numeric(eggSize)/2)^3 * meanDensity.sp7
  modelConfig[modelConfig[,1] == "species.egg.weight.sp7", 2] = eggWeight
  modelConfig[modelConfig[,1] == "species.egg.size.sp7", 2]   = eggSize
 
   
  # Critical efficiency and predation ingestion rate 
  modelConfig[modelConfig[,1] == "predation.efficiency.critical.sp7", 2]  = par[names(par) == "predation.efficiency.critical.sp7"]
  modelConfig[modelConfig[,1] == "predation.ingestion.rate.max.sp7", 2]   = par[names(par) == "predation.ingestion.rate.max.sp7"]
  
  
  # Natural mortality
  modelConfig[modelConfig[,1] == "mortality.natural.rate.sp7", 2]  = par[names(par) == "mortality.natural.rate.sp7"]
  
  
  # Manually changes about larval mortality: 19 par but perturbing the mean
  larvalMortality.sp7 = read.csv(file.path(configDir, "input/larval/larval_mortality-pota.csv"), stringsAsFactors = FALSE, sep = ";")
  lx  = log(larvalMortality.sp7$x)
  mlx = mean(lx) # perturbation using mlx: lx = exp()
  dlx = lx - mlx
  
  Lx = par[names(par) == "mortality.natural.larva.rate.Lx.sp7"]
  new_lx = dlx + log(Lx)
  new_x = exp(new_lx)
  
  newLarvalMortality.sp = larvalMortality.sp7
  newLarvalMortality.sp$x = new_x
  colnames(newLarvalMortality.sp)[1] = ""
  write.table(newLarvalMortality.sp, file = file.path(configDir, "newLavalMortality-pota.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.natural.larva.rate.bytDt.file.sp7", 2] = "newLavalMortality-pota.csv"
  
  
  # Manually changes about fishing mortality #### TO CHECK
  # 224 parameters: 1 (f media) + T (between years) + 12T (distribution of the fishing between years) + 2 (fishing selectivity)
  fishing_multiplier    = par[names(par) == "fishing.multiplier.sp7"]
  fishingMortality.sp7  = read.csv(file.path(configDir, "input/fishing/F-pota.csv"), stringsAsFactors = FALSE, sep = ";")
  fishingMortality.sp7[, c(2:dim(fishingMortality.sp7)[2])] = fishingMortality.sp7[, c(2:dim(fishingMortality.sp7)[2])] * fishing_multiplier
  colnames(fishingMortality.sp7) = c("", as.numeric(substring(colnames(fishingMortality.sp7)[-1], 2)))
  write.table(fishingMortality.sp7, file = file.path(configDir, "newFishingMortality-pota.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.fishing.rate.byDt.bySize.file.sp7", 2] = "newFishingMortality-pota.csv"
  
  
  # Sex ratio
  modelConfig[modelConfig[,1] == "species.sexratio.sp7", 2]  = par[names(par) == "species.sexratio.sp7"]
  
  
  # Von Bertalanffy parameters: l0 perturbed instead of t0
  K.sp7    = par[names(par) == "species.K.sp7"]
  Linf.sp7 = par[names(par) == "species.lInf.sp7"]
  
  l0.sp7      = par[names(par) == "species.l0.sp7"]
  newl0.sp7   = l0.sp7 * (Linf.sp7)
  newt0.sp7   = (1 / K.sp7) * (log(1 - (newl0.sp7 / Linf.sp7)))
  modelConfig[modelConfig[,1] == "species.t0.sp7", 2]  = newt0.sp7
  
  
  # Von Bertalanffy parameters: K and lInf
  modelConfig[modelConfig[,1] == "species.K.sp7", 2]              = par[names(par) == "species.K.sp7"]
  modelConfig[modelConfig[,1] == "species.lInf.sp7", 2]           = par[names(par) == "species.lInf.sp7"]
  
  # maturity size
  sx.sp7   = par[names(par) == "species.maturity.size.sp7"]
  smat.sp7 = ((sx.sp7)*(Linf.sp7.per - newl0.sp7)) + newl0.sp7
  modelConfig[modelConfig[,1] == "species.maturity.size.sp7", 2]  = smat.sp7
  
  # Length to weight relationship: condition factor perturbed
  modelConfig[modelConfig[,1] == "species.length2weight.condition.factor.sp7", 2]  =  par[names(par) == "species.length2weight.condition.factor.sp7"]
  
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
fixed10p_sp7 = run_experiments(X = doe, FUN = run_model, names=doe$parameter, parallel=TRUE)
end   = date()

saveRDS(object = fixed10p_sp7, file = "fixed10p_sp7.rds")
