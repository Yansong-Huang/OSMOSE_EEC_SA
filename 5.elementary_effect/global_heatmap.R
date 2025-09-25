library(data.table)
library(dplyr)
library(reshape2)
library(ComplexHeatmap)
library(circlize)
library(viridis)

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
  
  EE_stats$param_newlabel <- paste0(
    EE_stats$param_species2,"-",substr(EE_stats$param_type,1,4),"-",EE_stats$param_order
  )
  
  # 构建热图矩阵
  heat_mat <- dcast(EE_stats, param_newlabel ~ species, value.var="mu_star")
  rownames(heat_mat) <- heat_mat$param_newlabel
  heat_mat <- as.matrix(heat_mat[,-1])
  colnames(heat_mat) <- sp_names[colnames(heat_mat)]
  heat_mat <- heat_mat[, sort(colnames(heat_mat))]
  heat_mat_log <- log1p(heat_mat)
  
  # 每张子图独立 heatmap
  ht <- Heatmap(
    heat_mat_log,
    name = ind,               # 🔑 用指标名保证图例独立
    col = viridis(100),
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_row_names = FALSE,   # 隐藏行标签
    show_column_names = TRUE, # 显示列标签
    column_names_side = "bottom",
    column_names_rot = 90,
    heatmap_legend_param = list(
      direction = "horizontal",  # 横向显示
      title_position = "topcenter"
    ),
    row_title = ind
  )
  
  plots[[ind]] <- ht
}

# 合并五张 heatmap
ht_list <- Reduce("+", plots)

# 绘图并保存
png("figures/heatmap/combined_heatmap_complex_individual_legends.png",
    width = 4000, height = 8000, res = 300)
draw(ht_list, heatmap_legend_side = "bottom", merge_legends = FALSE)
dev.off()
