rm(list = ls())

library(data.table)
library(ggplot2)

biomass_list <- readRDS("4.indicators/indicators_output/biomass.rds")

# 合并所有实验，得到一个长格式的 data.table
biomass_dt_list <- lapply(seq_along(biomass_list), function(i) {
  x <- biomass_list[[i]]  # 20 × 16 × 10
  
  # 先对物种求和，得到 20 × 10 的矩阵
  total_biomass <- apply(x, c(1, 3), sum)  # 年 × 重复
  
  # 转换为 long 格式 data.table
  dt <- as.data.table(as.table(total_biomass))
  setnames(dt, c("year", "replicate", "biomass"))
  
  dt[, `:=`(year = as.integer(year),
            replicate = as.integer(replicate),
            scenario = paste0("exp", i))]
  
  return(dt)
})

biomass_all <- rbindlist(biomass_dt_list)

# baseline total biomass 
baseline_biomass <- readRDS("6.baseline/mean_baseline/baseline_biomass_sp.rds")

# 对物种求和，得到 20 年 × 1 的向量
baseline_total <- rowSums(baseline_biomass)  

baseline_dt <- data.table(
  year = 1:20,
  biomass = baseline_total,
  scenario = "baseline"
)

# combine data
plot_data <- rbind(biomass_all, baseline_dt, fill = TRUE)


violin_plot <- ggplot(plot_data[scenario != "baseline"], aes(x = factor(year), y = biomass)) +
  geom_violin(fill = "skyblue", alpha = 0.5, draw_quantiles = 0.5) +
  geom_line(data = plot_data[scenario == "baseline"],
            aes(x = factor(year), y = biomass, group = 1),
            color = "darkred", size = 1.2) +
  labs(x = "Year", y = "Total Biomass (t)",
       title = "Morris simulation: total biomass over time") +
  theme_minimal(base_size = 14)+
  theme(plot.background = element_rect(fill = "white", color = NA))
                

ggsave("figures/violin_total_biomass_over_time.png", plot = violin_plot, width = 12, height = 5, dpi = 300)

#------ 2 violin plot by species-----

# ----------------- 准备实验模拟数据 -----------------
# 每个元素：20年 × 16物种 × 10重复 → reshape成长格式
biomass_species_list <- lapply(seq_along(biomass_list), function(i) {
  x <- biomass_list[[i]]  # 20 × 16 × 10
  
  dt <- as.data.table(as.table(x))  # 三维转长格式
  setnames(dt, c("year", "species", "replicate", "biomass"))
  
  dt[, `:=`(
    year = as.integer(year),
    species = paste0("sp", as.integer(species)),
    replicate = as.integer(replicate),
    scenario = paste0("exp", i)
  )]
  
  return(dt)
})

biomass_species_all <- rbindlist(biomass_species_list)

# ----------------- 准备基线数据 -----------------
# baseline_biomass <- readRDS("6.baseline/mean_baseline/baseline_biomass_sp.rds")


# 转为 data.table 再 melt，确保兼容性和未来稳定性
baseline_dt <- melt(
  as.data.table(baseline_biomass),
  measure.vars = names(baseline_biomass),
  variable.name = "species",
  value.name = "biomass"
)

# 添加年份（1到20）与标签
baseline_dt[, `:=`(
  year = 1:.N,
  species = as.character(species),
  scenario = "baseline"
)]



# ----------------- 合并数据 -----------------
plot_species_dt <- rbind(biomass_species_all, baseline_dt, fill = TRUE)

# 示例：只保留重复 == 1 的数据
plot_species_small <- plot_species_dt[replicate == 1]


violin_by_species <- ggplot(plot_species_small[scenario != "baseline"], aes(x = factor(year), y = biomass)) +
  geom_violin(fill = "skyblue", alpha = 0.4, draw_quantiles = 0.5) +
  geom_line(data = plot_species_small[scenario == "baseline"],
            aes(x = factor(year), y = biomass, group = 1),
            color = "red", size = 0.8) +
  facet_wrap(~species, scales = "free_y") +
  labs(x = "Year", y = "Biomass (t)", title = "Morris simulation: Species-wise biomass over time") +
  theme_minimal(base_size = 12)

ggsave("figures/violin_total_biomass_by_species_over_time.png", plot = violin_by_species, width = 20, height = 10, dpi = 300)
