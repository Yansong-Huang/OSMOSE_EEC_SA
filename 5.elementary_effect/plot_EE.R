library(data.table)
library(ggplot2)

## 读取统计结果
EE <- fread("5.elementary_effect/EE_LFI40_stats.csv")  # 列有 param_name, mu_star, sigma

## 选取阈值（简单示例：μ* 的中位数）
thresh <- median(EE$mu_star)

## μ*–σ 散点图
ggplot(EE, aes(x = mu_star, y = sigma)) +
  geom_hline(yintercept = thresh, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = thresh, linetype = "dashed", colour = "grey50") +
  geom_point(size = 3, alpha = 0.8) +
  ggrepel::geom_text_repel(aes(label = param_name), max.overlaps = 20) +
  labs(
    title = "Morris Elementary Effects: LFI catch 40",
    x = expression(mu["*"]~""),
    y = expression(sigma~"")
  ) +
  theme_minimal(base_size = 13)


##  μ* 条形图（影响强度排序）
EE[, param_name := factor(param_name, levels = param_name[order(-mu_star)])]

ggplot(EE[order(-mu_star)][1:25],         # 先看前 25 个最敏感参数
       aes(x = param_name, y = mu_star)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top-25 sensitive parameters (LFI40)",
    x = NULL,
    y = expression(mu["*"])
  ) +
  theme_minimal(base_size = 13)
