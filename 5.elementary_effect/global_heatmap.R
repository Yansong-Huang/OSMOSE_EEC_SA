rm(list = ls())

library(data.table)
library(pheatmap)
library(viridis)
library(stringr)
library(dendextend)
library(dplyr)
library(gridExtra)


# ----------准备----------
#物种映射
sp_names <- c(
  "sp0" = "SYC",  "sp1" = "MUR",  "sp2" = "BIB",  "sp3" = "WHG",
  "sp4" = "POD",  "sp5" = "COD",  "sp6" = "LYY",  "sp7" = "SOL",
  "sp8" = "PLE",  "sp9" = "HOM",  "sp10" = "MAC", "sp11" = "HER",
  "sp12" = "PIL", "sp13" = "SQZ", "sp14" = "CTC", "sp15" = "RJC"
)
# 指标
indicators <- c("biomass", "yield", "mean_TL", "LFI", "mean_length")

plots <- list()

for(ind in indicators)
{
  # ---------- 读取数据 ----------
  EE_stats <- fread(paste0("5.elementary_effect/EE_outputs/EE_",ind,"_by_species_stats.csv"))
  
  # non-exploited species 
  if(ind %in% c("LFI", "yield"))
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
  
  # ---------- 主热图 ----------
  p <- pheatmap(
    mat = heat_mat_log,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    labels_row = "",        # 隐藏参数名标签
    color = viridis(100),
    fontsize_row = 6,
    fontsize_col = 10,
    border_color = NA,
    angle_col = 90,
    main = ind
  )
  plots[[ind]] <- p[[4]]  # pheatmap返回一个list，第4项是gtable对象
}

# 把5张拼接成一张
png("figures/heatmap/combined_heatmap.png", width = 5000, height = 3000, res = 500)
grid.arrange(grobs = plots, ncol = 5)  # 两列排版
dev.off()
  