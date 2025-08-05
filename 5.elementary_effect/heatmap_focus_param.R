rm(list = ls())

library(data.table)
library(stringr)
library(dplyr)
library(rlang)
require(ggplot2)
require(viridis)
require(reshape2)

  
plot_heatmap_by_species <- function(data,
                                    species_col = "species",
                                    param_col = "param_label",
                                    value_col = "mu_star",
                                    sp_names,
                                    species_order = NULL,
                                    scale_trans = "log1p",
                                    fill_label = NULL,
                                    plot_title = NULL) {
  
  
  # 使用缺省值或自动生成物种顺序
  if (is.null(species_order)) {
    species_order <- unique(data[[species_col]])
  }
  
  # 确保物种为 factor 并映射缩写
  data[[species_col]] <- factor(data[[species_col]], levels = species_order)
  species_abbr <- sp_names[as.character(data[[species_col]])]
  data[[species_col]] <- factor(species_abbr, levels = sp_names[species_order])
  
  # 创建绘图对象
  p <- ggplot(data, aes_string(x = species_col, y = param_col, fill = value_col)) +
    geom_tile(color = "NA", size = 0.3) +
    # scale_fill_viridis(
    #   option = "C",
    #   direction = 1,
    #   trans = scale_trans,
    #   name = fill_label %||% value_col
    # ) +
    labs(
      title = plot_title %||% paste("Heatmap of", value_col),
      x = "Species",
      y = "Parameter"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  if (value_col == "mu") {
    fill_scale <- scale_fill_gradient2(
      low = "#482777", mid = "white", high = "#FDE725",
      midpoint = 0,
      name = fill_label %||% value_col
    )
  } else {
    fill_scale <- scale_fill_viridis(
      option = "C",
      direction = 1,
      trans = scale_trans,
      name = fill_label %||% value_col
    )
  }
  p <- p + fill_scale
  
  return(p)
}

# 物种缩写映射
sp_names <- c(
  "sp0" = "SYC",  "sp1" = "MUR",  "sp2" = "BIB",  "sp3" = "WHG",
  "sp4" = "POD",  "sp5" = "COD",  "sp6" = "LYY",  "sp7" = "SOL",
  "sp8" = "PLE",  "sp9" = "HOM",  "sp10" = "MAC", "sp11" = "HER",
  "sp12" = "PIL", "sp13" = "SQZ", "sp14" = "CTC", "sp15" = "RJC"
)

# 设置物种顺序
species_all <- paste0("sp", 0:15)

# ---------- 读取数据 ---------- 
EE_stats <- fread("5.elementary_effect/EE_outputs/EE_LFI_by_species_stats.csv")

# 去掉非捕捞物种
EE_stats <- filter(EE_stats, !(species %in% c("sp4", "sp6")))


# ---------- 合并参数名称映射 ----------
mapping <- fread("5.elementary_effect/param_name_map.csv")
EE_stats <- merge(EE_stats, mapping, by = "param_name", all.x = TRUE)

# ---------- 筛选：仅相关参数 ----------
sel_param_sp <- "HER"
EE_stats <- filter(EE_stats, str_detect(param_species, sel_param_sp))

# 指标列名称，mu mu_star sigma
metric_col <- "sigma"

# 只保留想要画的指标列，比如 mu_star、sigma 等
heat_data <- melt(
  dcast(EE_stats, param_label ~ species, value.var = metric_col),
  id.vars = "param_label",
  variable.name = "species",
  value.name = metric_col
)

# 绘图
plot_heatmap_by_species(
  data = heat_data,
  species_col = "species",
  param_col = "param_label",
  value_col = metric_col,
  sp_names = sp_names,
  species_order = species_all,
  scale_trans = "log1p",
  fill_label = paste(metric_col, "(log1p)"),
  plot_title = paste("EE of", sel_param_sp, "parameters on LFI:", metric_col)
)

  