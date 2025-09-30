# 对照检查脚本

library(data.table)

# ========== 路径 ==========
sp_dir <- "4.indicators/indicators_output"
comm_dir <- "4.indicators/indicators_output"

# ========== 读取物种层面的数据 ==========
MeanTL_sp <- readRDS(file.path(sp_dir, "meanTL_by_sp.rds"))      # 结构: (sim, time, species)
LFI_sp    <- readRDS(file.path(sp_dir, "lfi_sp.rds")) # 结构: (sim, time, species)
biomass_sp   <- readRDS(file.path(sp_dir, "biomass.rds"))        # 生物量 (sim, time, species)
meanLength_sp  <- readRDS(file.path(sp_dir, "meanLength_by_sp.rds"))

# ========== 读取群落层面的数据 ==========
MeanTL_comm <- readRDS(file.path(comm_dir, "meanTL.rds"))   
meanLength_comm <- readRDS(file.path(comm_dir, "meanLength.rds"))     
LFI_comm    <- readRDS(file.path(comm_dir, "lfi40.rds"))  


# -----------------------------
# 2. 辅助函数：提取数组
# -----------------------------
build_sp_array <- function(biomass_lst, ind_lst = NULL){
  
  n_sim <- length(biomass_lst)
  time_len <- dim(biomass_lst[[1]])[1]
  n_species <- dim(biomass_lst[[1]])[2]
  n_repl <- dim(biomass_lst[[1]])[3]
  
  biomass_arr <- array(NA, dim = c(n_sim, time_len, n_species, n_repl))
  
  if(!is.null(ind_lst)){
    ind_arr <- array(NA, dim = c(n_sim, time_len, n_species, n_repl))
  } else {
    ind_arr <- NULL
  }
  
  for(i in seq_len(n_sim)){
    biomass_arr[i,,,] <- biomass_lst[[i]][[1]]
    if(!is.null(ind_lst)){
      ind_arr[i,,,] <- ind_lst[[i]][[1]]
    }
  }
  
  list(biomass = biomass_arr, ind = ind_arr)
}

 

# -----------------------------
# 5. 物种加权群落指标
# -----------------------------
calc_weighted_ind <- function(biomass_arr, ind_arr){
  weighted_ind <- apply(ind_arr * biomass_arr, c(1,2,4), sum) /
    apply(biomass_arr, c(1,2,4), sum)
  # 结果维度： sim × time × replicate
  return(weighted_ind)
}


# 1) 构建数组
arrays <- build_sp_array(biomass_sp, LFI_sp)
biomass_arr <- arrays$biomass
LFI_arr <- arrays$ind

# 2) 物种层面加权
weighted_meanTL_sp <- calc_weighted_ind(biomass_arr, MeanTL_arr)
weighted_LFI_sp <- calc_weighted_ind(biomass_arr, LFI_arr)

# 3. 计算群落层面指标
# ================================
# 返回：array，dim = sim × time × replicate
build_comm_array <- function(lst){
  
  n_sim <- length(lst)
  time_len <- dim(lst[[1]])[1]
  n_repl   <- dim(lst[[1]])[2]
  
  arr <- array(NA, dim = c(n_sim, time_len, n_repl))
  
  for(i in seq_len(n_sim)){
    arr[i,,] <- lst[[i]][[1]]
  }
  
  return(arr)
}


# ================================
# 使用示例
# ================================
MeanTL_comm_arr <- build_comm_array(MeanTL_comm)
LFI_comm_arr <- build_comm_array(LFI_comm)
dim(LFI_comm_arr)
# 应该得到: 196000 × 20 × 16 × 10
