library(data.table)
library(dplyr)
library(reshape2)
library(ComplexHeatmap)
library(circlize)
library(viridis)

# ----------准备---------- 
#物种映射 
sp_names <- c( "sp0" = "SYC", "sp1" = "MUR", "sp2" = "BIB", "sp3" = "WHG", "sp4" = "POD", "sp5" = "COD", "sp6" = "LYY", "sp7" = "SOL", "sp8" = "PLE", "sp9" = "HOM", "sp10" = "MAC", "sp11" = "HER", "sp12" = "PIL", "sp13" = "SQZ", "sp14" = "CTC", "sp15" = "RJC" ) 
# 指标 
indicators <- c("biomass", "yield", "mean_TL", "LFI", "mean_length")

plots <- list()

for(ind in indicators){
  # ---------- 数据读取 ----------
  EE_stats <- fread(paste0("5.elementary_effect/EE_outputs/EE_",ind,"_by_species_stats.csv"))
  if(ind %in% c("LFI","yield")) EE_stats <- filter(EE_stats, !(species %in% c("sp4","sp6")))
  
  mapping <- fread("5.elementary_effect/param_name_map.csv")
  EE_stats <- merge(EE_stats, mapping, by="param_name", all.x=TRUE)
  EE_stats$param_species2 <- EE_stats$param_species
  
  # 修正 fleet/resource/species_catchability
  is_fleet <- EE_stats$param_species=="fleet"
  EE_stats$param_species2[is_fleet] <- sub(".*catchability\\.","",EE_stats$param_label[is_fleet])
  is_resource <- EE_stats$param_species=="resource"
  EE_stats$param_species2[is_resource] <- sub(".*accessibility2fish\\.","",EE_stats$param_label[is_resource])
  is_spcat <- EE_stats$param_process=="species_catchability"
  catch_part <- sub(".*catchability\\.([^\\.]+)\\..*","\\1",EE_stats$param_label[is_spcat])
  EE_stats$param_species2[is_spcat] <- paste0(EE_stats$param_species[is_spcat],".",catch_part)
  
  # 得到不带物种名的参数名
  EE_stats$param_clean <- gsub("\\.[A-Z]{2,}", "", EE_stats$param_label)


  # 拼接新的行标签
  EE_stats$param_newlabel <- paste0(
    EE_stats$param_species2, ".", EE_stats$param_clean
  )
  
  # 构建热图矩阵
  heat_mat <- dcast(EE_stats, param_newlabel ~ species, value.var="mu_star")
  rownames(heat_mat) <- heat_mat$param_newlabel
  heat_mat <- as.matrix(heat_mat[,-1])
  colnames(heat_mat) <- sp_names[colnames(heat_mat)]
  heat_mat <- heat_mat[, sort(colnames(heat_mat))]
  heat_mat_log <- log1p(heat_mat)
  
  # 判断是否最后一个子图
  show_rows_flag <- ind == tail(indicators, 1)
  
  ht <- Heatmap(
    heat_mat_log,
    name = ind,               
    col = viridis(100),
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_row_names = show_rows_flag,   # 只在最后一个图显示行标签
    show_column_names = TRUE,          
    column_names_side = "bottom",
    column_names_rot = 90,
    column_title = ind,                # 指标名放在上方
    column_title_side = "top",         
    heatmap_legend_param = list(
      direction = "horizontal",
      title_position = "topcenter"
    )
  )
  
  plots[[ind]] <- ht
}

# 合并五张 heatmap
ht_list <- Reduce("+", plots)

# 绘图并保存
png("figures/heatmap/combined_heatmap_test.png",
    width = 6000, height = 12000, res = 300)
draw(ht_list, heatmap_legend_side = "bottom", merge_legends = FALSE,
     padding = unit(c(2, 8, 2, 2), "cm"))
dev.off()
