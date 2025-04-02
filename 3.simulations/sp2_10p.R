
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

setwd("/home/datawork-marbec-scenlab/OSMOSE/Criscely/ua_osmose_paper/outputs_uncertainty/sp2_10p")

doe = readRDS(file = "doe_sp2_10p.rds")

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
  
  sp = (2+1) #species2
  
  
  # Manually changes about PREDATION ACCESSIBILITY
  predationAccessibility = read.csv(file.path(configDir, "input/predation/predation-accessibility.csv"), stringsAsFactors = FALSE, sep = ";")
  pred = as.matrix(predationAccessibility[,-1])
  
  predx = (pred[sp, ]/pred[1:9, sp])[-sp]
  
  predationAccessibility$anchovy[sp]          = pmin(predx[1]*par[1], 1)
  predationAccessibility$hake[sp]             = pmin(predx[2]*par[2], 1)
  predationAccessibility$sardine[c(1:9)[-sp]] = pmin(par[c(1:8)], 1)
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
  theta.sp2.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp2.stage1"]) * (pi/2)
  alpha.sp2.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp2.stage1"]) * ((pi/2)-theta.sp2.stage1)
  min.sp2.stage1 = 1/maxSlope(angle = theta.sp2.stage1, m_min = 0)
  max.sp2.stage1 = 1/maxSlope(angle = alpha.sp2.stage1, m_min = 1/min.sp2.stage1)
  
  theta.sp2.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp2.stage2"]) * (pi/2)
  alpha.sp2.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp2.stage2"]) * ((pi/2)-theta.sp2.stage2)
  min.sp2.stage2 = 1/maxSlope(angle = theta.sp2.stage2, m_min = 0)
  max.sp2.stage2 = 1/maxSlope(angle = alpha.sp2.stage2, m_min = 1/min.sp2.stage2)
  
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.max.sp2", c(2,3)] = c(max.sp2.stage1, max.sp2.stage2)
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.min.sp2", c(2,3)] = c(min.sp2.stage1, min.sp2.stage2)
  
  
  # PredPrey stage threshold
  Linf.sp2.per = par[names(par) == "species.lInf.sp2"]
  modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp2", 2]    = par[names(par) == "predation.predPrey.stage.threshold.sp2"] * (Linf.sp2.per)
  
  
  # Starvation rate max
  modelConfig[modelConfig[,1] == "mortality.starvation.rate.max.sp2", 2]         = par[names(par) == "mortality.starvation.rate.max.sp2"]
  
  
  # vonBertalanffy threshold
  amax.sp2 = as.numeric(modelConfig[modelConfig[,1] == "species.lifespan.sp2", 2])
  modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp2", 2]  = par[names(par) == "species.vonbertalanffy.threshold.age.sp2"] * (amax.sp2)
 
   
  # Manually changes about egg SIZE AND WEIGHT
  eggSize.sp2   = as.numeric(modelConfig[modelConfig[,1] == "species.egg.size.sp2", 2])
  eggWeight.sp2 = as.numeric(modelConfig[modelConfig[,1] == "species.egg.weight.sp2", 2])
  meanDensity.sp2 = eggWeight.sp2 / ((4/3 * pi) * (eggSize.sp2/2)^3) 
  eggSize     = par[names(par) == "species.egg.size.sp2"]
  eggWeight   = (4/3 * pi) * (as.numeric(eggSize)/2)^3 * meanDensity.sp2
  modelConfig[modelConfig[,1] == "species.egg.weight.sp2", 2] = eggWeight
  modelConfig[modelConfig[,1] == "species.egg.size.sp2", 2]   = eggSize
  
  
  # Critical efficiency and predation ingestion rate 
  modelConfig[modelConfig[,1] == "predation.efficiency.critical.sp2", 2]  = par[names(par) == "predation.efficiency.critical.sp2"]
  modelConfig[modelConfig[,1] == "predation.ingestion.rate.max.sp2", 2]   = par[names(par) == "predation.ingestion.rate.max.sp2"]
  
  
  # Natural mortality
  modelConfig[modelConfig[,1] == "mortality.natural.rate.sp2", 2]  = par[names(par) == "mortality.natural.rate.sp2"]
  
  
  # Manually changes about larval mortality: 19 par but perturbing the mean
  larvalMortality.sp2 = read.csv(file.path(configDir, "input/larval/larval_mortality-sardine.csv"), stringsAsFactors = FALSE, sep = ";")
  lx  = log(larvalMortality.sp2$x)
  mlx = mean(lx) # perturbation using mlx: lx = exp()
  dlx = lx - mlx
  
  Lx = par[names(par) == "mortality.natural.larva.rate.Lx.sp2"]
  new_lx = dlx + log(Lx)
  new_x = exp(new_lx)
  
  newLarvalMortality.sp = larvalMortality.sp2
  newLarvalMortality.sp$x = new_x
  colnames(newLarvalMortality.sp)[1] = ""
  write.table(newLarvalMortality.sp, file = file.path(configDir, "newLavalMortality-sardine.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.natural.larva.rate.bytDt.file.sp2", 2] = "newLavalMortality-sardine.csv"
  
  
  # Manually changes about fishing mortality #### TO CHECK
  # 224 parameters: 1 (f media) + T (between years) + 12T (distribution of the fishing between years) + 2 (fishing selectivity)
  fishing_multiplier    = par[names(par) == "fishing.multiplier.sp2"]
  fishingMortality.sp2  = read.csv(file.path(configDir, "input/fishing/F-sardine.csv"), stringsAsFactors = FALSE, sep = ";")
  fishingMortality.sp2[, c(2:dim(fishingMortality.sp2)[2])] = fishingMortality.sp2[, c(2:dim(fishingMortality.sp2)[2])] * fishing_multiplier
  colnames(fishingMortality.sp2) = c("", as.numeric(substring(colnames(fishingMortality.sp2)[-1], 2)))
  write.table(fishingMortality.sp2, file = file.path(configDir, "newFishingMortality-sardine.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.fishing.rate.byDt.bySize.file.sp2", 2] = "newFishingMortality-sardine.csv"
  
  
  # Sex ratio
  modelConfig[modelConfig[,1] == "species.sexratio.sp2", 2]  = par[names(par) == "species.sexratio.sp2"]
 
   
  # Von Bertalanffy parameters: l0 perturbed instead of t0
  K.sp2    = par[names(par) == "species.K.sp2"]
  Linf.sp2 = par[names(par) == "species.lInf.sp2"]
  
  l0.sp2      = par[names(par) == "species.l0.sp2"]
  newl0.sp2   = l0.sp2 * (Linf.sp2)
  newt0.sp2   = (1 / K.sp2) * (log(1 - (newl0.sp2 / Linf.sp2)))
  modelConfig[modelConfig[,1] == "species.t0.sp2", 2]  = newt0.sp2
  
  
  # Von Bertalanffy parameters: K and lInf
  modelConfig[modelConfig[,1] == "species.K.sp2", 2]              = par[names(par) == "species.K.sp2"]
  modelConfig[modelConfig[,1] == "species.lInf.sp2", 2]           = par[names(par) == "species.lInf.sp2"]
  
  # maturity size
  sx.sp2   = par[names(par) == "species.maturity.size.sp2"]
  smat.sp2 = ((sx.sp2)*(Linf.sp2.per - newl0.sp2)) + newl0.sp2
  modelConfig[modelConfig[,1] == "species.maturity.size.sp2", 2]  = smat.sp2
  
  # Length to weight relationship: condition factor perturbed
  modelConfig[modelConfig[,1] == "species.length2weight.condition.factor.sp2", 2]  =  par[names(par) == "species.length2weight.condition.factor.sp2"]
  
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
fixed10p_sp2 = run_experiments(X = doe, FUN = run_model, names=doe$parameter, parallel=TRUE)
end   = date()

saveRDS(object = fixed10p_sp2, file = "fixed10p_sp2.rds")
