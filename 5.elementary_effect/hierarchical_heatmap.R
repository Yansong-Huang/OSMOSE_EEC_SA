library(data.table)
library(pheatmap)
library(viridis)
library(stringr)
library(dendextend)

# ---------- 读取数据 ----------
EE_stats <- fread("5.elementary_effect/EE_outputs/EE_biomass_by_species_stats.csv")

# 设定物种顺序
species_all <- paste0("sp", 0:15)
EE_stats[, species := factor(species, levels = species_all)]


# ---------- 构建 heatmap 数据矩阵 ----------
heat_mat <- dcast(EE_stats, param_name ~ species, value.var = "mu_star")
heat_mat <- as.data.frame(heat_mat)
rownames(heat_mat) <- heat_mat$param_name
heat_mat <- as.matrix(heat_mat[, -1])
heat_mat_log <- log1p(heat_mat)

# ---------- 聚类计算 ----------
# 行聚类（参数）
row_dist <- dist(heat_mat_log)
row_hclust <- hclust(row_dist, method = "complete")
row_dend <- as.dendrogram(row_hclust)

# 列聚类（物种）
col_dist <- dist(t(heat_mat_log))
col_hclust <- hclust(col_dist, method = "complete")
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
  main = "Clustered Heatmap of μ* (log1p)",
  filename = "figures/EE_clustered_heatmap_mu_star.png",
  width = 10,
  height = 12
)

# ---------- 单独保存行聚类树图（参数） ----------
# png("figures/EE_row_dendrogram_params.png", width = 2000, height = 1000)
# plot(row_dend, horiz = TRUE, main = "Parameter Clustering Dendrogram")
# dev.off()
# 
# ---------- 单独保存列聚类树图（物种） ----------
# png("figures/EE_col_dendrogram_species.png", width = 800, height = 400)
# plot(col_dend, main = "Species Clustering Dendrogram")
# dev.off()
