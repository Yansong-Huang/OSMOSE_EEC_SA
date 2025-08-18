rm(list = ls())

library(data.table)
library(pheatmap)
library(viridis)
library(stringr)
library(dendextend)
library(dplyr)
library(gridExtra)
library(gtable)


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
  #  param_species 大部分情况下直接用.特殊值时要补丁处理
  EE_stats$param_species2 <- EE_stats$param_species
  
  # 1) fleet → catchability 后的字符串
  is_fleet <- EE_stats$param_species == "fleet"
  EE_stats$param_species2[is_fleet] <- sub(".*catchability\\.", "", EE_stats$param_label[is_fleet])
  
  # 2) resource → accessibility2fish.. 后的字符串
  is_resource <- EE_stats$param_species == "resource"
  EE_stats$param_species2[is_resource] <- sub(".*accessibility2fish\\.", "", EE_stats$param_label[is_resource])
  
  # 3) species_catchability → 原物种 + "." + catchability后第一个点之前的部分
  is_spcat <- EE_stats$param_process == "species_catchability"
  catch_part <- sub(".*catchability\\.([^\\.]+)\\..*", "\\1", EE_stats$param_label[is_spcat])
  EE_stats$param_species2[is_spcat] <- paste0(EE_stats$param_species[is_spcat], ".", catch_part)
  
  # 最终新标签
  EE_stats$param_newlabel <- paste0(
    EE_stats$param_species2, "-",
    substr(EE_stats$param_type, 1, 4), "-",
    EE_stats$param_order
  )
  
  
  # ---------- 构建 heatmap 数据矩阵 ----------
  heat_mat <- dcast(EE_stats, param_newlabel ~ species, value.var = "mu_star")
  heat_mat <- as.data.frame(heat_mat)
  rownames(heat_mat) <- heat_mat$param_newlabel
  heat_mat <- as.matrix(heat_mat[, -1])
  
  # 替换列名为物种缩写
  colnames(heat_mat) <- sp_names[colnames(heat_mat)]
  # 列按字母顺序排序
  col_order <- sort(colnames(heat_mat))
  heat_mat <- heat_mat[, col_order]
  heat_mat_log <- log1p(heat_mat)
  
  # ---------- 主热图 ----------
  labels_rows_to_use <- if (ind == "mean_length") rownames(heat_mat_log) else rep("", nrow(heat_mat_log))
  
  p <- pheatmap(
    mat = heat_mat_log,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    labels_row = labels_rows_to_use,  
    color = viridis(100),
    fontsize_row = 6,
    fontsize_col = 10,
    border_color = NA,
    angle_col = 90,
    main = ind
  )
  
  
  plots[[ind]] <- p[[4]]  # pheatmap返回一个list，第4项是gtable对象
}

# 对齐宽度：找到所有子图的列宽，然后取最大值
# 这一步会隐藏图例

# 假设 plots[[1]] 是最左子图
# 给行名列增加宽度
plots[[1]]$widths[1] <- unit(2, "cm")  # 根据需要调整宽度

# 假设想要每个子图总宽度相等，先计算其它子图热图区总宽度平均值
heatBodyWidth <- plots[[2]]$widths[2:length(plots[[2]]$widths)]

# 左侧子图：行名列加宽，但热图区宽度减小保持总宽度一致
plots[[1]]$widths[2:length(plots[[1]]$widths)] <- heatBodyWidth - (plots[[1]]$widths[1] - unit(1, "cm"))

# 找到热图主体列（从第2列开始）对齐
maxWidthBody <- do.call(grid::unit.pmax, lapply(plots, function(x) x$widths[2:length(x$widths)]))

for (i in seq_along(plots)) {
  plots[[i]]$widths[2:length(plots[[i]]$widths)] <- maxWidthBody
}


# 把5张拼接成一张
png("figures/heatmap/combined_heatmap_no_legend.png", width = 4000, height = 4000, res = 300)
grid.arrange(grobs = plots, ncol = 5)  # 两列排版
dev.off()
  