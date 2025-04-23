# Creation of data perturbation files (except predation accessibility)
# Creation date : 2025-03-12 (Yansong Huang)
# Author : Criscely Lujan, Yansong Huang

# Scripts -----------------------------------------------------------------------
library(osmose)
source("1.preliminary-calculations/auxiliar.R")

# Config ------------------------------------------------------------------------

# 读取配置数据
config_dir  = "osmose-eec"
main_file = "initial_config.csv"
config_file = file.path(config_dir, main_file)
conf = read_osmose(input=config_file)
# # 读取配置数据
# predation_access_matrix <- read.csv("2.get-doe/config_osmose_eec/predation-accessibility.csv", header = TRUE, row.names = 1)



# Process -----------------------------------------------------------------------
# 1 predation accessibility
# see another script "" 


# 2-3. Predation size ratios (min, max) ----------------------------------
# list of ratios for all species
pred_ratio_max_all_sp <- get_par(conf, "predation.predprey.sizeratio.max")
pred_ratio_min_all_sp <- get_par(conf, "predation.predprey.sizeratio.min")


getAngles = function(sp){
  
  max = get_par(pred_ratio_max_all_sp,sp=sp)
  min = get_par(pred_ratio_min_all_sp,sp=sp)
  
  theta.stage1 = angleEstimation(m_min = 0, m_max = 1/min[1])
  alpha.stage1 = angleEstimation(m_min = 0, m_max = 1/max[1]) - theta.stage1
  
  if(length(max) == 1){
    return(c( theta.stage1/(pi/2),
              alpha.stage1/((pi/2)-theta.stage1) ))
  }
  
  if(length(max) == 2){
    theta.stage2 = angleEstimation(m_min = 0, m_max = 1/min[2])
    alpha.stage2 = angleEstimation(m_min = 0, m_max = 1/max[2]) - theta.stage2
    
    return(c( theta.stage1/(pi/2),
              alpha.stage1/((pi/2)-theta.stage1),
              theta.stage2/(pi/2),
              alpha.stage2/((pi/2)-theta.stage2) ))
  }
  
}

# 使用 lapply 获取角度
angles_list <- lapply(0:15, getAngles)

# 构造矩阵
sizeRatios <- matrix(unlist(angles_list), ncol = 1, byrow = TRUE)
sizeRatios = round(sizeRatios, 8)

# 生成参数名
pred_ratio_names <- unlist(lapply(0:15, function(sp) {
  stages <- if (sp == 5) c("stage1", "stage2") else c("stage1")
  paste0("predation.predPrey.sizeRatio.",rep(c("teta", "alpha"), each = length(stages)),".sp",
    sp,".",rep(stages, times = 2))
}))

# 加入其他信息，整理表格
pred_ratio_table <- data.frame(
  parameter = pred_ratio_names,
  value = sizeRatios,
  scale = rep("logit", 34)
)


write.table(
  pred_ratio_table,
  "1.preliminary-calculations/initial_parameters/sizeRatios.csv",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  sep = ","
)


# 10. Additional natural mortality rate --------------------------------------------
additional_mortality_all_sp <- get_par(conf,"mortality.additional.rate")

# 生成参数名
additional_mortality_names <- unlist(lapply(0:15, function(sp) {
  paste0("mortality.additional.rate.sp",sp)
}))

# 加入其他信息，整理表格
additional_mortality_table <- data.frame(
  parameter = additional_mortality_names,
  value = as.numeric(additional_mortality_all_sp),
  scale = rep("log", 16)
)


write.table(
  additional_mortality_table,
  "1.preliminary-calculations/initial_parameters/additionalMortality.csv",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  sep = ","
)


# 11.  Larval mortality rate ---------------------------------

# larval mortalities of all species but sole and plaice
larval_mortality_all_sp <- get_par(conf,"mortality.additional.larva.rate")[1:14]

# larval mortalities of sole and plaice are provided in files
larval_mortality_dir <- file.path(config_dir,"mortality")
larval_mortality_file_sp7 <- file.path(larval_mortality_dir, "larval_mortality-sole.csv")
larval_mortality_file_sp8 <- file.path(larval_mortality_dir, "larval_mortality-plaice.csv")
larval_mortality_sp7 = read.csv(larval_mortality_file_sp7,col.names = c("time","value"))
larval_mortality_sp8 = read.csv(larval_mortality_file_sp8,col.names = c("time","value"))

# 在对数空间计算几何均值
get_mlx = function(larvalVector){
  
  lx  = log(larvalVector)
  mlx = mean(lx)
  Lx = exp(mlx)
  
  return(Lx)
}

Lx_sp7 = get_mlx(larval_mortality_sp7$value)
Lx_sp8 = get_mlx(larval_mortality_sp8$value)

larvalMortality = c(unlist(larval_mortality_all_sp),Lx_sp7,Lx_sp8)
larvalMortality = round(larvalMortality, 8)

# 生成参数名
# sole (sp7) and plaice (sp8) were placed at last
larvalMortality_names_1 <- unlist(lapply(c(0:6,9:15), function(sp) {
  paste0("mortality.additional.larva.rate.sp",sp)
}))
larvalMortality_names_2 <- unlist(lapply(7:8, function(sp) {
  paste0("mortality.additional.larva.rate.mean.sp",sp)
}))
larvalMortality_names <- c(larvalMortality_names_1,larvalMortality_names_2)

# 加入其他信息，整理表格
larvalMortality_table <- data.frame(
  parameter = larvalMortality_names,
  value = larvalMortality,
  scale = rep("log", 16)
)

# 写入文档
write.table(
  larvalMortality_table,
  "1.preliminary-calculations/initial_parameters/larvalMortality.csv",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  sep = ","
)


# 12. catchability -----------------------------------------------------------------------
catchability_all_sp <- get_par(conf, par="osmose.user.catchability")
catchability_all_fl <- get_par(conf, par="fisheries.rate.base")
fishing_dir <- file.path(config_dir,"fishing")
catchability_file <- file.path(fishing_dir, "eec_fisheries_catchability.csv")

catchability.matrix = read.csv(catchability_file, check.names = FALSE, row.names = 1)
# 生成参数名
catchability_species_names <- names(catchability_all_sp)
catchability_species_names <- gsub("osmose\\.user", "species", catchability_species_names)
catchability_fleet_names <- unlist(lapply(0:3, function(fsh) {
  paste0("fisheries.rate.base.fsh",fsh)
}))

catchability_species_values <- c(catchability.matrix[c(1:4,6,8:11,14:16),1],
                                 catchability.matrix[10:13,2],
                                 catchability.matrix[c(1,6,8,9),1],
                                 catchability.matrix[setdiff(1:16, c(5, 7)),4])

# 把两类可捕捞性参数整理为一个表格
catchability_table <- data.frame(
  parameter = c(catchability_fleet_names,catchability_species_names),
  value = c(as.numeric(catchability_all_fl),catchability_species_values),
  scale = rep("log", 38) # chose log because catchability could be greater than 1
)


write.table(
  catchability_table,
  "1.preliminary-calculations/initial_parameters/catchability.csv",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  sep = ","
)


# 14.  L0 -----------------------------------------------------------------------
# 读取配置数据
t0_all_sp <- get_par(conf,"species.t0")
K_all_sp <- get_par(conf,"species.k")
Linf_all_sp <- get_par(conf,"species.linf")

# 方程计算vb生长起始体长
getL0 = function(sp){

  t0    = get_par(t0_all_sp,sp=sp)
  k     = get_par(K_all_sp,sp=sp)
  Linf  = get_par(Linf_all_sp,sp=sp)
  t = 0

  l0 = Linf*(1 - exp(-k * (t - t0)))

  return(l0)
}

# 调用方程计算vb生长起始体长，并除以最大体长

L0_list <- lapply(0:15, getL0)
L0_reparam = as.numeric(L0_list) / as.numeric(Linf_all_sp)
L0_reparam = round(L0_reparam, 8)
# might exclude squids (sp13) and cuttlefish (sp14), for which t0=0
# L0_reparam <- L0_reparam[-c(14:15)]

# 生成参数名
# might exclude squids (sp13) and cuttlefish (sp14), for which t0=0
L0_names <- unlist(lapply(c(0:15), function(sp) {
  paste0("species.l0.sp",sp)
}))


# 加入其他信息，整理表格
L0_table <- data.frame(
  parameter = L0_names,
  value = L0_reparam,
  scale = rep("logit", 16)
)

# 写入文档
write.table(
  L0_table,
  "1.preliminary-calculations/initial_parameters/L0.csv",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  sep = ","
)

# 15.K (von bertalanffy)                       :  --------------------------
# K is not re-parametrized 
# 配置数据读取
K_all_sp <- get_par(conf,"species.k")

# 生成参数名
K_names <- unlist(lapply(c(0:15), function(sp) {
  paste0("species.k.sp",sp) 
}))


# 加入其他信息，整理表格
K_table <- data.frame(
  parameter = K_names,
  value = as.numeric(K_all_sp),
  scale = rep("log", 16)
)

# 写入文档
write.table(
  K_table,
  "1.preliminary-calculations/initial_parameters/K.csv",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  sep = ","
)

# 16.  Linf (von bertalanffy)                       :  --------------------------
# Linf is not re-parametrized 
# 配置数据读取
Linf_all_sp <- get_par(conf,"species.linf")
# 生成参数名
Linf_names <- unlist(lapply(c(0:15), function(sp) {
  paste0("species.linf.sp",sp) 
}))


# 加入其他信息，整理表格
Linf_table <- data.frame(
  parameter = Linf_names,
  value = as.numeric(Linf_all_sp),
  scale = rep("log", 16)
)

# 写入文档
write.table(
  Linf_table,
  "1.preliminary-calculations/initial_parameters/Linf.csv",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  sep = ","
)


# 17. Size at maturity -----------------------------------------------------
# 读取配置数据
maturity_size_all_species <- get_par(conf,"species.maturity.size")

# 重参数化
maturity_size_reparam   = (as.numeric(maturity_size_all_species) - as.numeric(L0_list))/(as.numeric(Linf_all_sp) - as.numeric(L0_list))
maturity_size_reparam = round(maturity_size_reparam, 8)


# 生成参数名
maturity_size_names <- unlist(lapply(c(0:15), function(sp) {
  paste0("species.maturity.size.ratio.sp",sp) # Yansong: modified lower case l to L
}))

# 加入其他信息，整理表格
maturity_size_table <- data.frame(
  parameter = maturity_size_names,
  value = as.numeric(maturity_size_reparam),
  scale = rep("logit", 16)
)

# 写入文档
write.table(
  maturity_size_table,
  "1.preliminary-calculations/initial_parameters/maturitySize.csv",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  sep = ","
)



# 18. Constant of proportionality of the allometric length−weight relationship --------------------------------------------
constant_allometric_all_sp <- get_par(conf,"species.length2weight.condition.factor")

# 生成参数名
constant_allometric_names <- unlist(lapply(0:15, function(sp) {
  paste0("species.length2weight.condition.factor.sp",sp)
}))

# 加入其他信息，整理表格
constant_allometric_table <- data.frame(
  parameter = constant_allometric_names,
  value = as.numeric(constant_allometric_all_sp),
  scale = rep("log", 16)
)


write.table(
  constant_allometric_table,
  "1.preliminary-calculations/initial_parameters/constantAllometric.csv",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  sep = ","
)

# 19. LTL ressources accessibility to fish --------------------------------------------
ltl_access_all_sp <- get_par(conf,"species.accessibility2fish")

# 生成参数名
ltl_access_names <- unlist(lapply(16:26, function(sp) {
  paste0("species.accessibility2fish.sp",sp)
}))

# 加入其他信息，整理表格
ltl_access_table <- data.frame(
  parameter = ltl_access_names,
  value = as.numeric(ltl_access_all_sp),
  scale = rep("logit", 11)
)


write.table(
  ltl_access_table,
  "1.preliminary-calculations/initial_parameters/ltlAccess.csv",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  sep = ","
)

