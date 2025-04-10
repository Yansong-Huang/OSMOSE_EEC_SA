# Creation of data perturbation files (only for predation accessibility)
# Creation date : 2025-03-12 
# 2025-04-10 we decided to not include these parameters.
# Author : Yansong Huang
# library(osmose)
# 
# # 物种列表
# species_list <- c(
#   "lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole",
#   "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay"
# )
# 
# # 读取捕食可达性配置数据
# predation_access_matrix <- read.csv("2.get-doe/config_osmose_eec/predation-accessibility.csv", header = TRUE, row.names = 1)
# 
# # 目标路径
# folder_path <- "1.preliminary-calculations/csv" 
# 
# # 确保文件夹存在
# if (!dir.exists(folder_path)) {
#   dir.create(folder_path, recursive = TRUE)  # recursive = TRUE 允许创建多级目录
# }
# 
# file_name <- file.path(folder_path, "predAccess.csv")
# # file_name <- file.path(folder_path, "osm_parameters_initial.csv")
# file.create(file_name) 
# 
# 
# # 写入所有参数
# # 统一计数方式
# pred_index <- 1 # 该参数用于捕食可达性表格读取
# for (i in c(0:15)) {  # predator
#   # 捕食可达性
#   pred_access_param_name_list <- unlist(lapply(0:15, function(x) {
#     if (x %in% c(9, 10, 14, 15)) {
#       return(paste0("predation.accessibility.",species_list[i+1],".stage1.prey.sp", x, ".stage1"))
#     } else {
#       return(c(
#         paste0("predation.accessibility.",species_list[i+1],".stage1.prey.sp", x, ".stage1"),
#         paste0("predation.accessibility.",species_list[i+1],".stage1.prey.sp", x,".stage2")
#       ))
#     }
#   }))
#   # 生成表格
#   df_access <- data.frame(
#     parameter = pred_access_param_name_list,
#     value = predation_access_matrix[1:28,pred_index],
#     scale = rep("logit", 28)
#   )
#   # 该参数用于控制表格读取
#   pred_index <- pred_index+1
#   # 在原有基础上写入
#   write.table(
#     df_access,
#     file.path(folder_path, "predAccess.csv"),
#     sep = ",",
#     row.names = FALSE,
#     col.names = FALSE,
#     append = TRUE,
#     quote = FALSE
#   )
#   # 捕食可达性 第二部分
#   if (i %in% c(0:8,11:13)){
#     pred_access_param_name_list <- unlist(lapply(0:15, function(x) {
#       if (x %in% c(9, 10, 14, 15)) {
#         return(paste0("predation.accessibility.",species_list[i+1],".stage2.prey.sp", x, ".stage1"))
#       } else {
#         return(c(
#           paste0("predation.accessibility.",species_list[i+1],".stage2.prey.sp", x, ".stage1"),
#           paste0("predation.accessibility.",species_list[i+1],".stage2.prey.sp", x,".stage2")
#         ))
#       }
#     }))
#     # 生成表格
#     df_access <- data.frame(
#       parameter = pred_access_param_name_list,
#       value = predation_access_matrix[1:28,pred_index],
#       scale = rep("logit", 28)
#     )
#     # 该参数用于控制捕食可达性表格读取
#     pred_index <- pred_index+1
#     # 在原有基础上写入
#     write.table(
#       df_access,
#       file.path(folder_path, "predAccess.csv"),
#       sep = ",",
#       row.names = FALSE,
#       col.names = FALSE,
#       append = TRUE,
#       quote = FALSE
#     )
#   }
# 
# }
# 

