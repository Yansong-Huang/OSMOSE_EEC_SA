# script to split doe matrix
# Yansong Huang
#13/04/2025

split_doe <- function(X, n_splits = 4) {
  r_total <- dim(X$doe)[3]  # 总轨迹数量，比如20
  split_size <- ceiling(r_total / n_splits)
  
  split_list <- vector("list", n_splits)
  
  for (k in 1:n_splits) {
    idx_start <- (k - 1) * split_size + 1
    idx_end   <- min(k * split_size, r_total)
    
    new_X <- X
    
    new_X$doe      <- X$doe[,,idx_start:idx_end]
    new_X$unscaled <- X$unscaled[,,idx_start:idx_end]
    new_X$jumps    <- X$jumps[,,idx_start:idx_end]
    new_X$delta    <- X$delta[,idx_start:idx_end]
    new_X$i        <- X$i[,idx_start:idx_end]
    new_X$r        <- idx_end - idx_start + 1  # 更新数量
    
    split_list[[k]] <- new_X
  }
  
  return(split_list)
}

# 读取原始列表
doe_complete = readRDS(file = "2.get-doe/doe/test_10p_0410.rds")
# 执行拆分
splits <- split_doe(doe_complete, n_split=10)
# 分别保存
for (j in 1:4) {
  saveRDS(splits[[j]],file.path("2.get-doe/doe",paste0("split_", j, "_0413.rds"))) 
}
