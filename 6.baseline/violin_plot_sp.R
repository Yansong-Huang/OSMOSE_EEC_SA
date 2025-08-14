rm(list = ls())
library(data.table)
library(ggplot2)

# 物种缩写映射
# sp_names <- c(
#   "sp0" = "SYC",  "sp1" = "MUR",  "sp2" = "BIB",  "sp3" = "WHG",
#   "sp4" = "POD",  "sp5" = "COD",  "sp6" = "LYY",  "sp7" = "SOL",
#   "sp8" = "PLE",  "sp9" = "HOM",  "sp10" = "MAC", "sp11" = "HER",
#   "sp12" = "PIL", "sp13" = "SQZ", "sp14" = "CTC", "sp15" = "RJC"
# )

sp_names <- c("lesserSpottedDogfish","redMullet","pouting","whiting",  
              "poorCod","cod","dragonet","sole","plaice","horseMackerel",
              "mackerel","herring","sardine","squids","cuttlefish","thornbackRay")
species_all <- names(sp_names)

biomass_list <- readRDS("4.indicators/indicators_output/biomass.rds")
baseline_biomass <- readRDS("6.baseline/mean_baseline/baseline_biomass_sp.rds")

# 循环每个物种
for (sp in species_all) {
  
  message("Processing ", sp, " ...")
  
  # --- 收集该物种的所有实验数据 ---
  biomass_dt_list <- vector("list", length(biomass_list))
  
  for (i in seq_along(biomass_list)) {
    x <- biomass_list[[i]][ , sp , ]  # 取出该物种的 20×10 矩阵
    
    dt <- as.data.table(as.table(x))
    setnames(dt, c("year", "replicate", "biomass"))
    
    dt[, `:=`(
      year = as.integer(year),
      replicate = as.integer(replicate),
      scenario = paste0("exp", i)
    )]
    
    biomass_dt_list[[i]] <- dt
  }
  
  biomass_all <- rbindlist(biomass_dt_list)
  rm(biomass_dt_list); gc()
  
  # --- 基线数据 ---
  baseline_dt <- data.table(
    year = 1:20,
    biomass = baseline_biomass[, sp],
    scenario = "baseline"
  )
  
  # --- 合并数据绘图 ---
  plot_data <- rbind(biomass_all, baseline_dt, fill = TRUE)
  
  violin_plot <- ggplot(plot_data[scenario != "baseline"], aes(x = factor(year), y = biomass)) +
    geom_violin(fill = "skyblue", alpha = 0.5, draw_quantiles = 0.5) +
    geom_line(data = baseline_dt, aes(x = factor(year), y = biomass, color = "baseline"), size = 1) +
    labs(
      x = "Year", y = "Biomass (t)",
      title = paste("Morris simulation:", sp_names[sp], "biomass over time"),
      color = "Scenario"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.background = element_rect(fill = "white", color = NA))
  
  # 保存
  dir.create("figures/species_biomass", recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path("figures/species_biomass", paste0(sp_names[sp], "_biomass.png")),
    plot = violin_plot,
    width = 10, height = 5, dpi = 300
  )
  
  rm(biomass_all, baseline_dt, plot_data, violin_plot); gc()
}

message("✅ 所有物种绘图完成")
