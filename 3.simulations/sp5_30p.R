
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

setwd("/home/datawork-marbec-scenlab/OSMOSE/Criscely/ua_osmose_paper/outputs_uncertainty/sp5_30p")

doe = readRDS(file = "doe_sp5_30p.rds")

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
  
  sp = (5+1) #species5
  
  
  # Manually changes about PREDATION ACCESSIBILITY
  predationAccessibility = read.csv(file.path(configDir, "input/predation/predation-accessibility.csv"), stringsAsFactors = FALSE, sep = ";")
  pred = as.matrix(predationAccessibility[,-1])
  
  predx = (pred[sp, ]/pred[1:9, sp])[-sp]
  
  predationAccessibility$anchovy[sp]          = pmin(predx[1]*par[1], 1)
  predationAccessibility$hake[sp]             = pmin(predx[2]*par[2], 1)
  predationAccessibility$sardine[sp]          = pmin(predx[3]*par[3], 1)
  predationAccessibility$jurel[sp]            = pmin(predx[4]*par[4], 1)
  predationAccessibility$caballa[sp]          = pmin(predx[5]*par[5], 1)
  predationAccessibility$meso[c(1:9)[-sp]]   = pmin(par[c(1:8)], 1)
  predationAccessibility$munida[sp]           = pmin(predx[6]*par[6], 1)
  predationAccessibility$pota[sp]             = pmin(predx[7]*par[7], 1)
  predationAccessibility$euphausidos[sp]      = pmin(predx[8]*par[8], 1)
  colnames(predationAccessibility)[1] = ""
  write.table(predationAccessibility, file = file.path(configDir, "newPredationAccessibility.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "predation.accessibility.file", 2] = "newPredationAccessibility.csv"
  
  
  # Manually changes about PREDATION SIZE RATIOS
  theta.sp5.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.theta.sp5.stage1"]) * (pi/2)
  alpha.sp5.stage1   = as.numeric(par[names(par) == "predation.predPrey.sizeRatio.alpha.sp5.stage1"]) * ((pi/2)-theta.sp5.stage1)
  min.sp5.stage1 = 1/maxSlope(angle = theta.sp5.stage1, m_min = 0)
  max.sp5.stage1 = 1/maxSlope(angle = alpha.sp5.stage1, m_min = 1/min.sp5.stage1)
  
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.max.sp5", c(2)] = c(max.sp5.stage1)
  modelConfig[modelConfig[,1] == "predation.predPrey.sizeRatio.min.sp5", c(2)] = c(min.sp5.stage1)
  
  
  # Starvation rate max
  modelConfig[modelConfig[,1] == "mortality.starvation.rate.max.sp5", 2]         = par[names(par) == "mortality.starvation.rate.max.sp5"]
  
  
  # vonBertalanffy threshold
  amax.sp5 = as.numeric(modelConfig[modelConfig[,1] == "species.lifespan.sp5", 2])
  modelConfig[modelConfig[,1] == "species.vonbertalanffy.threshold.age.sp5", 2]  = par[names(par) == "species.vonbertalanffy.threshold.age.sp5"] * (amax.sp5)
  
  
  # Manually changes about egg SIZE AND WEIGHT
  eggSize.sp5   = as.numeric(modelConfig[modelConfig[,1] == "species.egg.size.sp5", 2])
  eggWeight.sp5 = as.numeric(modelConfig[modelConfig[,1] == "species.egg.weight.sp5", 2])
  meanDensity.sp5 = eggWeight.sp5 / ((4/3 * pi) * (eggSize.sp5/2)^3) 
  eggSize     = par[names(par) == "species.egg.size.sp5"]
  eggWeight   = (4/3 * pi) * (as.numeric(eggSize)/2)^3 * meanDensity.sp5
  modelConfig[modelConfig[,1] == "species.egg.weight.sp5", 2] = eggWeight
  modelConfig[modelConfig[,1] == "species.egg.size.sp5", 2]   = eggSize
 
   
  # Critical efficiency and predation ingestion rate 
  modelConfig[modelConfig[,1] == "predation.efficiency.critical.sp5", 2]  = par[names(par) == "predation.efficiency.critical.sp5"]
  modelConfig[modelConfig[,1] == "predation.ingestion.rate.max.sp5", 2]   = par[names(par) == "predation.ingestion.rate.max.sp5"]
  
  
  # Natural mortality
  modelConfig[modelConfig[,1] == "mortality.natural.rate.sp5", 2]  = par[names(par) == "mortality.natural.rate.sp5"]
  
  
  # Manually changes about larval mortality: 19 par but perturbing the mean
  larvalMortality.sp5 = read.csv(file.path(configDir, "input/larval/larval_mortality-meso.csv"), stringsAsFactors = FALSE, sep = ";")
  lx  = log(larvalMortality.sp5$x)
  mlx = mean(lx) # perturbation using mlx: lx = exp()
  dlx = lx - mlx
  
  Lx = par[names(par) == "mortality.natural.larva.rate.Lx.sp5"]
  new_lx = dlx + log(Lx)
  new_x = exp(new_lx)
  
  newLarvalMortality.sp = larvalMortality.sp5
  newLarvalMortality.sp$x = new_x
  colnames(newLarvalMortality.sp)[1] = ""
  write.table(newLarvalMortality.sp, file = file.path(configDir, "newLavalMortality-meso.csv"), row.names = FALSE, sep = ";")
  modelConfig[modelConfig[,1] == "mortality.natural.larva.rate.bytDt.file.sp5", 2] = "newLavalMortality-meso.csv"
  
  
  # Sex ratio
  modelConfig[modelConfig[,1] == "species.sexratio.sp5", 2]  = par[names(par) == "species.sexratio.sp5"]
  
  
  # Von Bertalanffy parameters: l0 perturbed instead of t0
  K.sp5    = par[names(par) == "species.K.sp5"]
  Linf.sp5 = par[names(par) == "species.lInf.sp5"]
  
  l0.sp5      = par[names(par) == "species.l0.sp5"]
  newl0.sp5   = l0.sp5 * (Linf.sp5)
  newt0.sp5   = (1 / K.sp5) * (log(1 - (newl0.sp5 / Linf.sp5)))
  modelConfig[modelConfig[,1] == "species.t0.sp5", 2]  = newt0.sp5
  
  
  # Von Bertalanffy parameters: K and lInf
  modelConfig[modelConfig[,1] == "species.K.sp5", 2]              = par[names(par) == "species.K.sp5"]
  modelConfig[modelConfig[,1] == "species.lInf.sp5", 2]           = par[names(par) == "species.lInf.sp5"]
  
  # maturity size
  Linf.sp5.per= par[names(par) == "species.lInf.sp5"]
  sx.sp5   = par[names(par) == "species.maturity.size.sp5"]
  smat.sp5 = ((sx.sp5)*(Linf.sp5.per - newl0.sp5)) + newl0.sp5
  modelConfig[modelConfig[,1] == "species.maturity.size.sp5", 2]  = smat.sp5
  
  # Length to weight relationship: condition factor perturbed
  modelConfig[modelConfig[,1] == "species.length2weight.condition.factor.sp5", 2]  =  par[names(par) == "species.length2weight.condition.factor.sp5"]
  
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
fixed30p_sp5 = run_experiments(X = doe, FUN = run_model, names=doe$parameter, parallel=TRUE)
end   = date()

saveRDS(object = fixed30p_sp5, file = "fixed30p_sp5.rds")
