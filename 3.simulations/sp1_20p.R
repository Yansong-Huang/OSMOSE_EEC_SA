
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

setwd("/home/datawork-marbec-scenlab/OSMOSE/Criscely/ua_osmose_paper/outputs_uncertainty/sp1_20p")

doe = readRDS(file = "doe_sp1_20p.rds")

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
  
  sp = (1+1) #species1 + 1
 
  
  # Manually changes about PREDATION ACCESSIBILITY
  predationAccessibility = read.csv(file.path(configDir, "input/predation/predation-accessibility.csv"), stringsAsFactors = FALSE, sep = ";")
  pred = as.matrix(predationAccessibility[,-1])
  
  predx = (pred[sp, ]/pred[1:9, sp])[-sp]
  
  predationAccessibility$anchovy[sp]          = pmin(predx[1]*par[1], 1)
  predationAccessibility$hake[c(1:9)[-sp]]    = pmin(par[c(1:8)], 1)
  predationAccessibility$sardine[sp]          = pmin(predx[2]*par[2], 1)
  predationAccessibility$jurel[sp]            = pmin(predx[3]*par[3], 1)
  predationAccessibility$caballa[sp]          = pmin(predx[4]*par[4], 1)
  predationAccessibility$meso[sp]             = pmin(predx[5]*par[5], 1)
  predationAccessibility$munida[sp]           = pmin(predx[6]*par[6], 1)
  predationAccessibility$pota[sp]             = pmin(predx[7]*par[7], 1)
  predationAccessibility$euphausidos[sp]      = pmin(predx[8]*par[8], 1)
  colnames(predationAccessibility)[1] = ""
  write.table(predationAccessibility, file = file.path(configDir, "newPredationAccessibility.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "predation.accessibility.file", 2] = "newPredationAccessibility.csv"
 
   
  # Manually changes about PREDATION SIZE RATIOS
  theta.sp1.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp1.stage1"]) * (pi/2)
  alpha.sp1.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp1.stage1"]) * ((pi/2)-theta.sp1.stage1)
  min.sp1.stage1 = 1/maxSlope(angle = theta.sp1.stage1, m_min = 0)
  max.sp1.stage1 = 1/maxSlope(angle = alpha.sp1.stage1, m_min = 1/min.sp1.stage1)
  
  theta.sp1.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp1.stage2"]) * (pi/2)
  alpha.sp1.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp1.stage2"]) * ((pi/2)-theta.sp1.stage2)
  min.sp1.stage2 = 1/maxSlope(angle = theta.sp1.stage2, m_min = 0)
  max.sp1.stage2 = 1/maxSlope(angle = alpha.sp1.stage2, m_min = 1/min.sp1.stage2)
  
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.max.sp1", c(2,3)] = c(max.sp1.stage1, max.sp1.stage2)
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.min.sp1", c(2,3)] = c(min.sp1.stage1, min.sp1.stage2)
  
  
  # PredPrey stage threshold
  Linf.sp1.per = par[names(par) == "species.lInf.sp1"]
  modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp1", 2]    = par[names(par) == "predation.predPrey.stage.threshold.sp1"] * (Linf.sp1.per)
  
  
  # Starvation rate max
  modelConfig[modelConfig[,1] == "mortality.starvation.rate.max.sp1", 2]         = par[names(par) == "mortality.starvation.rate.max.sp1"]
  
  
  # vonBertalanffy threshold
  amax.sp1 = as.numeric(modelConfig[modelConfig[,1] == "species.lifespan.sp1", 2])
  modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp1", 2]  = par[names(par) == "species.vonbertalanffy.threshold.age.sp1"] * (amax.sp1)
  
  
  # Manually changes about egg SIZE AND WEIGHT
  eggSize.sp1   = as.numeric(modelConfig[modelConfig[,1] == "species.egg.size.sp1", 2])
  eggWeight.sp1 = as.numeric(modelConfig[modelConfig[,1] == "species.egg.weight.sp1", 2])
  meanDensity.sp1 = eggWeight.sp1 / ((4/3 * pi) * (eggSize.sp1/2)^3) 
  eggSize     = par[names(par) == "species.egg.size.sp1"]
  eggWeight   = (4/3 * pi) * (as.numeric(eggSize)/2)^3 * meanDensity.sp1
  modelConfig[modelConfig[,1] == "species.egg.weight.sp1", 2] = eggWeight
  modelConfig[modelConfig[,1] == "species.egg.size.sp1", 2]   = eggSize
  
  
  # Critical efficiency and predation ingestion rate 
  modelConfig[modelConfig[,1] == "predation.efficiency.critical.sp1", 2]  = par[names(par) == "predation.efficiency.critical.sp1"]
  modelConfig[modelConfig[,1] == "predation.ingestion.rate.max.sp1", 2]   = par[names(par) == "predation.ingestion.rate.max.sp1"]
  
  
  # Natural mortality
  modelConfig[modelConfig[,1] == "mortality.natural.rate.sp1", 2]  = par[names(par) == "mortality.natural.rate.sp1"]
  
  
  # Manually changes about larval mortality: 19 par but perturbing the mean
  larvalMortality.sp1 = read.csv(file.path(configDir, "input/larval/larval_mortality-hake.csv"), stringsAsFactors = FALSE, sep = ";")
  lx  = log(larvalMortality.sp1$x)
  mlx = mean(lx) # perturbation using mlx: lx = exp()
  dlx = lx - mlx
  
  Lx = par[names(par) == "mortality.natural.larva.rate.Lx.sp1"]
  new_lx = dlx + log(Lx)
  new_x = exp(new_lx)
  
  newLarvalMortality.sp = larvalMortality.sp1
  newLarvalMortality.sp$x = new_x
  colnames(newLarvalMortality.sp)[1] = ""
  write.table(newLarvalMortality.sp, file = file.path(configDir, "newLavalMortality-hake.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.natural.larva.rate.bytDt.file.sp1", 2] = "newLavalMortality-hake.csv"
  
  
  # Manually changes about fishing mortality #### TO CHECK
  # 224 parameters: 1 (f media) + T (between years) + 12T (distribution of the fishing between years) + 2 (fishing selectivity)
  fishing_multiplier    = par[names(par) == "fishing.multiplier.sp1"]
  fishingMortality.sp1  = read.csv(file.path(configDir, "input/fishing/F-hake.csv"), stringsAsFactors = FALSE, sep = ";")
  fishingMortality.sp1[, c(2:dim(fishingMortality.sp1)[2])] = fishingMortality.sp1[, c(2:dim(fishingMortality.sp1)[2])] * fishing_multiplier
  colnames(fishingMortality.sp1) = c("", as.numeric(substring(colnames(fishingMortality.sp1)[-1], 2)))
  write.table(fishingMortality.sp1, file = file.path(configDir, "newFishingMortality-hake.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.fishing.rate.byDt.bySize.file.sp1", 2] = "newFishingMortality-hake.csv"
  
  
  # Sex ratio
  modelConfig[modelConfig[,1] == "species.sexratio.sp1", 2]  = par[names(par) == "species.sexratio.sp1"]
  
  
  # Von Bertalanffy parameters: l0 perturbed instead of t0
  K.sp1    = par[names(par) == "species.K.sp1"]
  Linf.sp1 = par[names(par) == "species.lInf.sp1"]
  
  l0.sp1      = par[names(par) == "species.l0.sp1"]
  newl0.sp1   = l0.sp1 * (Linf.sp1)
  newt0.sp1   = (1 / K.sp1) * (log(1 - (newl0.sp1 / Linf.sp1)))
  modelConfig[modelConfig[,1] == "species.t0.sp1", 2]  = newt0.sp1
  
  
  # Von Bertalanffy parameters: K and lInf
  modelConfig[modelConfig[,1] == "species.K.sp1", 2]              = par[names(par) == "species.K.sp1"]
  modelConfig[modelConfig[,1] == "species.lInf.sp1", 2]           = par[names(par) == "species.lInf.sp1"]
  
  # maturity size
  sx.sp1   = par[names(par) == "species.maturity.size.sp1"]
  smat.sp1 = ((sx.sp1)*(Linf.sp1.per - newl0.sp1)) + newl0.sp1
  modelConfig[modelConfig[,1] == "species.maturity.size.sp1", 2]  = smat.sp1
  
  # Length to weight relationship: condition factor perturbed
  modelConfig[modelConfig[,1] == "species.length2weight.condition.factor.sp1", 2]  =  par[names(par) == "species.length2weight.condition.factor.sp1"]
  
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
fixed20p_sp1 = run_experiments(X = doe, FUN = run_model, names=doe$parameter, parallel=TRUE)
end   = date()

saveRDS(object = fixed20p_sp1, file = "fixed20p_sp1.rds")
