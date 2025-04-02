
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

setwd("/home/datawork-marbec-scenlab/OSMOSE/Criscely/ua_osmose_paper/outputs_uncertainty/sp8_30p")

doe = readRDS(file = "doe_sp8_30p.rds")

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
  
  sp = (8+1) #species8
  
  
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
  predationAccessibility$pota[sp]             = pmin(predx[8]*par[8], 1)
  predationAccessibility$euphausidos[c(1:9)[-sp]] = pmin(par[c(1:8)], 1)
  colnames(predationAccessibility)[1] = ""
  write.table(predationAccessibility, file = file.path(configDir, "newPredationAccessibility.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "predation.accessibility.file", 2] = "newPredationAccessibility.csv"
  
  
  # Manually changes about PREDATION SIZE RATIOS
  theta.sp8.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp8.stage1"]) * (pi/2)
  alpha.sp8.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp8.stage1"]) * ((pi/2)-theta.sp8.stage1)
  min.sp8.stage1 = 1/maxSlope(angle = theta.sp8.stage1, m_min = 0)
  max.sp8.stage1 = 1/maxSlope(angle = alpha.sp8.stage1, m_min = 1/min.sp8.stage1)
  
  theta.sp8.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp8.stage2"]) * (pi/2)
  alpha.sp8.stage2   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp8.stage2"]) * ((pi/2)-theta.sp8.stage2)
  min.sp8.stage2 = 1/maxSlope(angle = theta.sp8.stage2, m_min = 0)
  max.sp8.stage2 = 1/maxSlope(angle = alpha.sp8.stage2, m_min = 1/min.sp8.stage2)
  
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.max.sp8", c(2,3)] = c(max.sp8.stage1, max.sp8.stage2)
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.min.sp8", c(2,3)] = c(min.sp8.stage1, min.sp8.stage2)
 
   
  # PredPrey stage threshold
  Linf.sp8.per = par[names(par) == "species.lInf.sp8"]
  modelConfig[modelConfig[,1] == "predation.predPrey.stage.threshold.sp8", 2]    = par[names(par) == "predation.predPrey.stage.threshold.sp8"] * (Linf.sp8.per)
 
   
  # Starvation rate max
  modelConfig[modelConfig[,1] == "mortality.starvation.rate.max.sp8", 2]         = par[names(par) == "mortality.starvation.rate.max.sp8"]
  
  
  # vonBertalanffy threshold
  amax.sp8 = as.numeric(modelConfig[modelConfig[,1] == "species.lifespan.sp8", 2])
  modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp8", 2]  = par[names(par) == "species.vonbertalanffy.threshold.age.sp8"] * (amax.sp8)
 
   
  # Manually changes about egg SIZE AND WEIGHT
  eggSize.sp8   = as.numeric(modelConfig[modelConfig[,1] == "species.egg.size.sp8", 2])
  eggWeight.sp8 = as.numeric(modelConfig[modelConfig[,1] == "species.egg.weight.sp8", 2])
  meanDensity.sp8 = eggWeight.sp8 / ((4/3 * pi) * (eggSize.sp8/2)^3) 
  eggSize     = par[names(par) == "species.egg.size.sp8"]
  eggWeight   = (4/3 * pi) * (as.numeric(eggSize)/2)^3 * meanDensity.sp8
  modelConfig[modelConfig[,1] == "species.egg.weight.sp8", 2] = eggWeight
  modelConfig[modelConfig[,1] == "species.egg.size.sp8", 2]   = eggSize
  
  
  # Critical efficiency and predation ingestion rate 
  modelConfig[modelConfig[,1] == "predation.efficiency.critical.sp8", 2]  = par[names(par) == "predation.efficiency.critical.sp8"]
  modelConfig[modelConfig[,1] == "predation.ingestion.rate.max.sp8", 2]   = par[names(par) == "predation.ingestion.rate.max.sp8"]
  
  
  # Natural mortality
  modelConfig[modelConfig[,1] == "mortality.natural.rate.sp8", 2]  = par[names(par) == "mortality.natural.rate.sp8"]
 
   
  # Manually changes about larval mortality: 19 par but perturbing the mean
  larvalMortality.sp8 = read.csv(file.path(configDir, "input/larval/larval_mortality-euphausidos.csv"), stringsAsFactors = FALSE, sep = ";")
  lx  = log(larvalMortality.sp8$x)
  mlx = mean(lx) # perturbation using mlx: lx = exp()
  dlx = lx - mlx
  
  Lx = par[names(par) == "mortality.natural.larva.rate.Lx.sp8"]
  new_lx = dlx + log(Lx)
  new_x = exp(new_lx)
  
  newLarvalMortality.sp = larvalMortality.sp8
  newLarvalMortality.sp$x = new_x
  colnames(newLarvalMortality.sp)[1] = ""
  write.table(newLarvalMortality.sp, file = file.path(configDir, "newLavalMortality-euphausidos.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.natural.larva.rate.bytDt.file.sp8", 2] = "newLavalMortality-euphausidos.csv"
  
  
  # Sex ratio
  modelConfig[modelConfig[,1] == "species.sexratio.sp8", 2]  = par[names(par) == "species.sexratio.sp8"]
  
  
  # Von Bertalanffy parameters: l0 perturbed instead of t0
  K.sp8    = par[names(par) == "species.K.sp8"]
  Linf.sp8 = par[names(par) == "species.lInf.sp8"]
  
  l0.sp8      = par[names(par) == "species.l0.sp8"]
  newl0.sp8   = l0.sp8 * (Linf.sp8)
  newt0.sp8   = (1 / K.sp8) * (log(1 - (newl0.sp8 / Linf.sp8)))
  modelConfig[modelConfig[,1] == "species.t0.sp8", 2]  = newt0.sp8

    
  # Von Bertalanffy parameters: K and lInf
  modelConfig[modelConfig[,1] == "species.K.sp8", 2]              = par[names(par) == "species.K.sp8"]
  modelConfig[modelConfig[,1] == "species.lInf.sp8", 2]           = par[names(par) == "species.lInf.sp8"]
  
  # maturity size
  sx.sp8   = par[names(par) == "species.maturity.size.sp8"]
  smat.sp8 = ((sx.sp8)*(Linf.sp8.per - newl0.sp8)) + newl0.sp8
  modelConfig[modelConfig[,1] == "species.maturity.size.sp8", 2]  = smat.sp8
  
  # Length to weight relationship: condition factor perturbed
  modelConfig[modelConfig[,1] == "species.length2weight.condition.factor.sp8", 2]  =  par[names(par) == "species.length2weight.condition.factor.sp8"]
  
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
fixed30p_sp8 = run_experiments(X = doe, FUN = run_model, names=doe$parameter, parallel=TRUE)
end   = date()

saveRDS(object = fixed30p_sp8, file = "fixed30p_sp8.rds")
