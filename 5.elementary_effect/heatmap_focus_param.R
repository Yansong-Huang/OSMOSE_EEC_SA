# =========================================
#  批量绘制 EE 热图脚本
# =========================================
rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(viridis)
library(rlang)

# ---------- 物种映射 ----------
sp_names <- c(
  "sp0" = "SYC",  "sp1" = "MUR",  "sp2" = "BIB",  "sp3" = "WHG",
  "sp4" = "POD",  "sp5" = "COD",  "sp6" = "LYY",  "sp7" = "SOL",
  "sp8" = "PLE",  "sp9" = "HOM",  "sp10" = "MAC", "sp11" = "HER",
  "sp12" = "PIL", "sp13" = "SQZ", "sp14" = "CTC", "sp15" = "RJC"
)
species_all <- paste0("sp", 0:15)
# TODO 将指标和过滤物种关联（部分指标需要过滤非捕捞物种）
# species_keep <- setdiff(species_all, c("sp4", "sp6"))
species_keep <- species_all
# ---------- 绘图函数 ----------
plot_heatmap_by_species <- function(data,
                                    species_col = "species",
                                    param_col = "param_label",
                                    value_col = "mu_star",
                                    sp_names,
                                    species_order = NULL,
                                    scale_trans = "identity",
                                    fill_label = NULL,
                                    plot_title = NULL) {
  
  # 自动物种顺序
  if (is.null(species_order)) {
    species_order <- unique(data[[species_col]])
  }
  
  # 将物种列改为缩写
  data[[species_col]] <- factor(data[[species_col]], levels = species_order)
  species_abbr <- sp_names[as.character(data[[species_col]])]
  data[[species_col]] <- factor(species_abbr, levels = sp_names[species_order])
  
  # 绘图
  p <- ggplot(data, aes_string(x = species_col, y = param_col, fill = value_col)) +
    geom_tile(color = NA) +
    labs(
      title = plot_title %||% paste("Heatmap of", value_col),
      x = "Species",
      y = "Parameter"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # 填色方案
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
  p + fill_scale
}

# ---------- 循环参数 ----------
# TODO 将指标和过滤物种关联（部分指标需要过滤非捕捞物种）
indicators <- c("biomass_rel", "mean_length", "mean_TL") # indicators including poor cod and dragonet
# "LFI", "yield_rel" # indicators without poor cod and dragonet
metrics <- c("mu", "mu_star", "sigma")
param_species <- unique(sp_names)  # 缩写

# 所有组合
combinations <- tidyr::crossing(
  plot_indicator = indicators,
  metric_col     = metrics,
  sel_param_sp   = param_species
)

# 一次性读 mapping
mapping <- fread("5.elementary_effect/param_name_map.csv")

# ---------- 批量绘图函数 ----------
plot_one <- function(plot_indicator, metric_col, sel_param_sp) {
  
  # 数据
  EE_stats <- fread(
    paste0("5.elementary_effect/EE_outputs/EE_", plot_indicator, "_by_species_stats.csv")
  ) %>% 
    filter(species %in% species_keep) %>%
    merge(mapping, by = "param_name", all.x = TRUE) %>%
    filter(str_detect(param_species, sel_param_sp))
  
  if (nrow(EE_stats) == 0) return(NULL)
  
  # 转宽再转长
  heat_data <- reshape2::melt(
    reshape2::dcast(EE_stats, param_label ~ species, value.var = metric_col),
    id.vars = "param_label",
    variable.name = "species",
    value.name = metric_col
  )
  
  # 绘图
  p <- plot_heatmap_by_species(
    data = heat_data,
    species_col = "species",
    param_col = "param_label",
    value_col = metric_col,
    sp_names = sp_names,
    species_order = species_all,
    scale_trans = "identity",
    fill_label = metric_col,
    plot_title = paste("EE", metric_col, "of", sel_param_sp, "parameters on", plot_indicator)
  )
  
  # 保存
  out_dir <- file.path("figures", plot_indicator, metric_col)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path(out_dir, paste0(sel_param_sp, "_", metric_col, ".png")),
    plot = p,
    width = 10, height = 6
  )
}

# ---------- 批量运行 ----------
pwalk(combinations, plot_one)

message("plots exported!")
