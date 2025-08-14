rm(list = ls())

library(data.table)
library(pheatmap)
library(viridis)
library(stringr)
library(dendextend)
library(dplyr)

# ----------准备----------
#物种映射
sp_names <- c(
  "sp0" = "SYC",  "sp1" = "MUR",  "sp2" = "BIB",  "sp3" = "WHG",
  "sp4" = "POD",  "sp5" = "COD",  "sp6" = "LYY",  "sp7" = "SOL",
  "sp8" = "PLE",  "sp9" = "HOM",  "sp10" = "MAC", "sp11" = "HER",
  "sp12" = "PIL", "sp13" = "SQZ", "sp14" = "CTC", "sp15" = "RJC"
)
# 指标
indicators <- c("biomass_rel", "mean_length", "LFI", "yield_rel", "mean_TL")

for(ind in indicators)
{
  # ---------- 读取数据 ----------
  EE_stats <- fread(paste0("5.elementary_effect/EE_outputs/EE_",ind,"_by_species_stats.csv"))
  
  # non-exploited species 
  if(ind %in% c("LFI", "yield_rel"))
    EE_stats <- filter(EE_stats, !(species %in% c("sp4", "sp6")))
  
  # ---------- 合并 param_type 和 param_label ----------
  mapping <- fread("5.elementary_effect/param_name_map.csv")
  EE_stats <- merge(EE_stats, mapping, by = "param_name", all.x = TRUE)
  
  
  # 设定物种顺序
  species_all <- paste0("sp", 0:15)
  EE_stats[, species := factor(species, levels = species_all)]
  
  
  # ---------- 构建 heatmap 数据矩阵 ----------
  heat_mat <- dcast(EE_stats, param_label ~ species, value.var = "mu_star")
  heat_mat <- as.data.frame(heat_mat)
  rownames(heat_mat) <- heat_mat$param_label
  heat_mat <- as.matrix(heat_mat[, -1])
  
  # 替换列名为物种缩写
  colnames(heat_mat) <- sp_names[colnames(heat_mat)]
  
  heat_mat_log <- log1p(heat_mat)
  
  # ---------- 聚类计算 ----------
  # 行聚类（参数）
  row_dist <- dist(heat_mat_log)
  row_hclust <- hclust(row_dist, method = "ward.D")
  row_dend <- as.dendrogram(row_hclust)
  
  # 列聚类（物种）
  col_dist <- dist(t(heat_mat_log))
  col_hclust <- hclust(col_dist, method = "ward.D")
  col_dend <- as.dendrogram(col_hclust)
  
  # ---------- 主热图 ----------
  pheatmap(
    mat = heat_mat_log,
    cluster_rows = row_hclust,
    cluster_cols = col_hclust,
    color = viridis(100),
    fontsize_row = 6,          # 显示参数名
    fontsize_col = 10,
    border_color = NA,
    angle_col = 45,   # 横坐标文字旋转 45 度
    main = "Clustered Heatmap of μ* (log1p)",
    filename = paste0("figures/hierarchical_heatmap/EE_",ind,"_hierarchical_heatmap_mu_star_ward.D_method.png"),
    width = 10,
    height = 12
  )
  
  # ---------- 单独保存行聚类树图（参数） ----------
  png(paste0("figures/hierarchical_heatmap/EE_",ind,"_row_dendrogram_ward.D_method.png"), width = 2200, height = 4000, res = 150)
  par(mar = c(5, 5, 4, 20))  # 试着留更大的左边距
  plot(row_dend, horiz = TRUE, main = "Parameter Clustering Dendrogram", cex = 0.7)
  dev.off()
  
}