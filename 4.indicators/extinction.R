rm(list = ls())
library(data.table)

biomass_list <- readRDS("4.indicators/indicators_output/biomass.rds")

# 记录每个模拟中灭绝物种的编号（sp0 ~ sp15）
extinct_species_list_soft <- lapply(biomass_list, function(arr) {
  extinct_soft <- apply(arr, 2, function(mat) {
    mean(mat <= 1e-6, na.rm = TRUE) >= 0.1  # 超过10%为近零
  })
  ids <- which(extinct_soft)
  if (length(ids) == 0) character(0) else paste0("sp", ids)
})

# 统计每个物种被判定为“经常接近灭绝”的次数
table(unlist(extinct_species_list_soft))

# 转为物种编号向量
extinct_counts <- table(unlist(extinct_species_list_soft))

# sp1  sp14   sp3 
# 836 93383   331 

# ------ 大西洋鳕竟然没有灭绝？------

# 提取所有模拟中 sp5 的值（sp5 是第6个物种，因 R 索引从1开始）
# 维度：20 × 10，共196000个模拟
cod_values <- unlist(lapply(biomass_list, function(arr) {
  arr[, 6, ]  # 6是sp5在R中的索引（物种编号5 + 1）
}))

summary(cod_values)
hist(cod_values, breaks = 100, main = "biomass distribution of cuttlefish", 
     xlab = "biomass (t)", col = "skyblue", border = "white")

# 最后一年所有物种生物量在不同实验中的分布情况
par(mfrow = c(4, 4), mar = c(3, 3, 2, 1))
for (i in 1:16) {
  values <- unlist(lapply(biomass_list, function(arr) arr[20, i, ]))
  hist(values, main = paste0("sp", i - 1), xlab = "", ylab = "", col = "gray", border = "white")
}

# ------ 为什么墨鱼常常灭绝？------

# 提取所有模拟中 sp14 的值
# 维度：20 × 10，共196000个模拟
ctc_values <- unlist(lapply(biomass_list, function(arr) {
  arr[, 15, ]  # 15是sp14在R中的索引
}))

summary(ctc_values)
hist(ctc_values, breaks = 100, main = "biomass distribution of cuttlefish", 
     xlab = "biomass (t)", col = "skyblue", border = "white")
