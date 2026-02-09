library(data.table)
library(ggplot2)
library(dplyr)

read_add_indicator <- function(path, indicator_name) {
  dt <- fread(path)
  dt[, indicator := indicator_name]
  return(dt)
}

EE_all <- rbindlist(list(
  read_add_indicator("5.elementary_effect/EE_outputs/EE_biomass_total_biomass.csv",    "Total Biomass"),
  read_add_indicator("5.elementary_effect/EE_outputs/EE_yield_total_yield.csv",        "Total Yield"),
  read_add_indicator("5.elementary_effect/EE_outputs/EE_LFI40_stats.csv",              "LFI40"),
  read_add_indicator("5.elementary_effect/EE_outputs/EE_meanLength_stats.csv",         "Mean Length"),
  read_add_indicator("5.elementary_effect/EE_outputs/EE_meanTL_stats.csv",             "Mean Trophic Level")
), use.names = TRUE, fill = TRUE)

# ---------- 合并 param_type 和 param_label ----------
mapping <- fread("5.elementary_effect/param_name_map.csv")
EE_all <- merge(EE_all, mapping, by = "param_name", all.x = TRUE)


# 确保是 data.table 或 data.frame
dt <- as.data.table(EE_all)

dt <- dt %>%
  filter(indicator=="Total Biomass")


# ========== 1️⃣ 排除 fleet / resource ==========
dt_sp <- dt[!(param_species %in% c("fleet", "resource"))]

# ========== 2️⃣ 读取 baseline 生物量 ==========
baseline <- readRDS("6.baseline/mean_baseline/baseline_biomass_sp.rds")
baseline_dt <- as.data.table(baseline)

# baseline 是 20 行（年份）× 16 列（物种）
# 去掉前两年（假设是前两行）
baseline_dt <- baseline_dt[-c(1, 2), ]

# ========== 3️⃣ 求每个物种平均生物量 ==========
mean_biomass <- baseline_dt[, lapply(.SD, mean, na.rm = TRUE)]

# 转为长格式方便合并
mean_biomass_long <- melt(mean_biomass, variable.name = "species_full", value.name = "mean_biomass")

# ========== 4️⃣ 建立物种代码与全名的对应 ==========
sp_names <- c(
  "lesserSpottedDogfish","redMullet","pouting","whiting",
  "poorCod","cod","dragonet","sole","plaice","horseMackerel",
  "mackerel","herring","sardine","squids","cuttlefish","thornbackRay"
)

sp_codes <- c("SYC", "MUR", "BIB", "WHG", "POD", "COD", "LYY", "SOL",
              "PLE", "HOM", "MAC", "HER", "PIL", "SQZ", "CTC", "RJC")

sp_map <- data.table(
  param_species = sp_codes,
  species_full  = sp_names
)

# 合并平均生物量
mean_biomass_long <- merge(mean_biomass_long, sp_map, by = "species_full")

# ========== 5️⃣ 计算每个物种 mu* 平均值 ==========
mean_mu_star <- dt_sp[, .(mean_mu_star = mean(mu_star, na.rm = TRUE)), by = param_species]

# ========== 6️⃣ 合并两类信息 ==========
plot_dt <- merge(mean_mu_star, mean_biomass_long[, .(param_species, mean_biomass)], by = "param_species", all.x = TRUE)

# ========== 7️⃣ 绘图 ==========
ggplot(plot_dt, aes(x = mean_biomass, y = mean_mu_star, label = param_species)) +
  geom_point(size = 3, color = "#377EB8") +
  geom_text(vjust = -0.6, size = 3) +
  scale_x_log10(name = "Mean species biomass (log scale)") +
  scale_y_log10(name = "Effect magnitude (log scale)") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank()
  )

