library(data.table)
library(ggplot2)

## --------------------------------
## 1. 读取三类指标的 μ* 结果
## --------------------------------
EE_lfi      <- fread("5.elementary_effect/EE_LFI40_stats.csv")[, .(param_name, mu_star)]
# EE_length   <- fread("5.elementary_effect/EE_meanLength_stats.csv")[, .(param_name, mu_star)]
EE_trophic  <- fread("5.elementary_effect/EE_meanTL_stats.csv")[, .(param_name, mu_star)]

## 给列加标签
EE_lfi[,      indicator := "LFI40"]
# EE_length[,   indicator := "MeanLength"]
EE_trophic[,  indicator := "MeanTL"]

## --------------------------------
## 2. 合并成长格式表
## --------------------------------
# EE_all <- rbindlist(list(EE_lfi, EE_length, EE_trophic))
EE_all <- rbindlist(list(EE_lfi, EE_trophic))
EE_all <- EE_all[!is.na(mu_star)]  # 移除NA（有些参数可能对某些指标无效）

## --------------------------------
## 3. 可选：对参数名排序（按总μ*影响力）
## --------------------------------
param_order <- EE_all[, .(total_mu_star = sum(mu_star, na.rm = TRUE)), by = param_name][
  order(-total_mu_star), param_name]

EE_all[, param_name := factor(param_name, levels = param_order)]
# EE_all[, indicator := factor(indicator, levels = c("LFI40", "MeanLength", "MeanTL"))]
EE_all[, indicator := factor(indicator, levels = c("LFI40", "MeanTL"))]

## --------------------------------
## 4. 画热图
## --------------------------------

top_params <- head(param_order, 20)
EE_all <- EE_all[param_name %in% top_params]

ggplot(EE_all, aes(x = indicator, y = param_name, fill = mu_star)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C", name = expression(mu^"*")) +
  labs(x = NULL, y = NULL, title = "Elementary Effects (μ*) by Parameter and Indicator") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
