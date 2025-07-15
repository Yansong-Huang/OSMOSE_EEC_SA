
library(osmose)
library(ggplot2)

# 设置路径和参数
biomass_dir <- "biomass_output"
yield_dir <- "yield_output"
n_rep_values <- c(5, 10, 15, 20, 25, 30, 40, 50)

for (n_rep in n_rep_values){
  outputDir  = paste0("output_replicate_test_",n_rep)
  
  output_simu = read_osmose(path = file.path(outputDir), version = "4.4.0")

}


# 存储结果
cv_results <- data.frame(n_rep = integer(), cv_total_biomass = numeric())

for (n in n_rep_values) {
  file_path <- file.path(biomass_dir, paste0("biomass_n_rep", n, ".rds"))
  biomass_array <- readRDS(file_path)  # [time, species, replicate]
  
  total_biomass_per_replicate <- apply(biomass_array[,,1:n], 3, function(slice) {
    mean(colSums(slice, na.rm = TRUE))  # 每个时间步的总量 → 求均值
  })
  
  # 计算 CV
  cv <- sd(total_biomass_per_replicate) / mean(total_biomass_per_replicate)
  
  cv_results <- rbind(cv_results, data.frame(n_rep = n, cv_total_biomass = cv))
}

# 绘图
ggplot(cv_results, aes(x = n_rep, y = cv_total_biomass)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(size = 2, color = "blue") +
  labs(x = "number of replicate",
       y = "CV of total biomass") +
  theme_minimal()
