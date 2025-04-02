# Creation of data perturbation files
# Creation date : 2025-03-12 
# Author : Yansong Huang

# 物种列表
species_list <- c(
  "lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole",
  "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay"
)

# 读取配置数据
predation_access_matrix <- read.csv("2.get-doe/config_osmose_eec/predation-accessibility.csv", header = TRUE, row.names = 1)

# 目标路径
folder_path <- "2.get-doe/data_perturbation" 

# 确保文件夹存在
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)  # recursive = TRUE 允许创建多级目录
}

# 生成 CSV 文件
for (i in 0:15) {
  file_name <- file.path(folder_path, paste0("osm_parameters_sp", i, ".csv"))
  file.create(file_name)  
}

# 写入所有参数

pred_index <- 1 # 该参数用于捕食表格读取
for (i in c(0:15)) {  # predator
  # 捕食可达性
  pred_access_param_name_list <- unlist(lapply(0:15, function(x) {
    if (x %in% c(9, 10, 14, 15)) {
      return(paste0("predation.accessibility.",species_list[i+1],".stage0.prey.sp", x, ".stage0"))
    } else {
      return(c(
        paste0("predation.accessibility.",species_list[i+1],".stage0.prey.sp", x, ".stage0"),
        paste0("predation.accessibility.",species_list[i+1],".stage0.prey.sp", x,".stage1")
      ))
    }
  }))
  # 生成表格
  df_access <- data.frame(
    parameter = pred_access_param_name_list,
    hierarchy = rep("gs", 28),
    value = predation_access_matrix[1:28,pred_index],
    scale = rep("logit", 28)
  )
  # 该参数用于控制表格读取
  pred_index <- pred_index+1
  # 在原有表格基础上写入
  write.table(
    df_access,
    file.path(folder_path, paste0("osm_parameters_sp", i, ".csv")),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    append = TRUE,
    quote = FALSE
  )
  # 捕食可达性 第二部分
  if (i %in% c(0:8,11:13)){
    pred_access_param_name_list <- unlist(lapply(0:15, function(x) {
      if (x %in% c(9, 10, 14, 15)) {
        return(paste0("predation.accessibility.",species_list[i+1],".stage1.prey.sp", x, ".stage0"))
      } else {
        return(c(
          paste0("predation.accessibility.",species_list[i+1],".stage1.prey.sp", x, ".stage0"),
          paste0("predation.accessibility.",species_list[i+1],".stage1.prey.sp", x,".stage1")
        ))
      }
    }))
    # 生成表格
    df_access <- data.frame(
      parameter = pred_access_param_name_list,
      hierarchy = rep("gs", 28),
      value = predation_access_matrix[1:28,pred_index],
      scale = rep("logit", 28)
    )
    # 该参数用于控制表格读取
    pred_index <- pred_index+1
    # 在原有表格基础上写入
    write.table(
      df_access,
      file.path(folder_path, paste0("osm_parameters_sp", i, ".csv")),
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE,
      quote = FALSE
    )
  }
  
  # 其他参数
  other_param_name_list <- c(paste0("predation.predPrey.sizeRatio.theta.sp",i,".stage0"),
                             paste0("predation.predPrey.sizeRatio.theta.sp",i,".stage0"),
                             paste0("mortality.starvation.rate.max.sp",i),
                             paste0("species.vonbertalanffy.threshold.age.sp",i),
                             paste0("species.egg.size.sp",i),
                             paste0("predation.efficiency.critical.sp",i),
                             paste0("predation.ingestion.rate.max.sp",i),
                             paste0("mortality.natural.rate.sp",i),
                             paste0("mortality.natural.larva.rate.Lx.sp",i),
                             paste0("species.sexratio.sp",i),
                             paste0("species.l0.sp",i),
                             paste0("species.K.sp",i),
                             paste0("species.lInf.sp",i),
                             paste0("species.maturity.size.sp",i),
                             paste0("species.length2weight.condition.factor.sp",i)
                             )
  df_access <- data.frame(
    parameter = pred_access_param_name_list,
    hierarchy = c(rep("gs", 4),rep("er",3),rep("mc",2),rep("hqd",6)),
    value = ,
    scale = c("logit","logit","log","logit","log","logit","log",
              "log","log","logit","logit","log","log","logit","log")
  )
}


