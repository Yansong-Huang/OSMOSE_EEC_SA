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


# 计算每年的平均 biomass（不包括 baseline）
mean_dt <- biomass_all[,.(mean_biomass = mean(biomass)),
                     by = year]

# 平均值差异
mean_dt$mean_biomass - baseline_dt$biomass
# [1] -496115.03 -628063.79 -268535.80 -163020.84 -134879.33
# [6] -173908.25 -105861.99 -122698.12 -174418.31 -233002.43
# [11] -203859.85 -171334.12 -176435.78 -123327.97 -130574.62
# [16] -128972.61 -115368.62  -93810.88  -97415.27 -108765.56

# combine data
plot_data <- rbind(biomass_all, baseline_dt, fill = TRUE)

# 准备图例
baseline_dt[, type := "baseline average"]
mean_dt[, type := "test average"]

baseline_dt[, year := as.integer(year)]
mean_dt[, year := as.integer(year)]

# 统一列名以便合并
setnames(baseline_dt, "biomass", "value")
setnames(mean_dt, "mean_biomass", "value")

# 合并两条线的数据
line_dt <- rbind(baseline_dt[, .(year, value, type)],
                 mean_dt[, .(year, value, type)])

violin_plot <- ggplot(plot_data[scenario != "baseline"], aes(x = factor(year), y = biomass)) +
  geom_violin(fill = "skyblue", alpha = 0.5, draw_quantiles = 0.5) +
  
  # 添加两条线（自动带图例）
  geom_line(data = line_dt,
            aes(x = factor(year), y = value, color = type, linetype = type, group = type),
            size = 1.1) +
  
  scale_color_manual(values = c("baseline average" = "darkred",
                                "test average" = "blue")) +
  scale_linetype_manual(values = c("baseline average" = "solid",
                                   "test average" = "dashed")) +
  
  labs(x = "Year", y = "Total Biomass (t)",
       title = "Morris simulation: total biomass over time",
       color = "Scenario", linetype = "Scenario") +
  
  theme_minimal(base_size = 14) +
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave("figures/violin_total_biomass_over_time.png", plot = violin_plot, width = 12, height = 5, dpi = 300)

