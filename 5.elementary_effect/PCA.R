rm(list = ls())
library(tidyverse)

indicators <- c("biomass_rel", "mean_length", "LFI", "yield_rel", "mean_TL")

# 读数据
EE_list <- lapply(indicators, function(ind){
  read_csv(paste0("5.elementary_effect/EE_outputs/EE_", ind, "_by_species_stats.csv"))
})

names(EE_list) <- indicators

# 过滤LFI和yield_rel中排除sp4和sp6
EE_list$LFI <- EE_list$LFI %>% filter(!species %in% c("sp4", "sp6"))
EE_list$yield_rel <- EE_list$yield_rel %>% filter(!species %in% c("sp4", "sp6"))

# 创建参数唯一ID
# 这里用 param_name 作为参数的唯一标识，假如同一个param_name对应多个species无问题
# 因为参数是固定的，只是对不同species敏感度不同

# 构建数据框，行是参数，列是 species_indicator
all_params <- unique(unlist(lapply(EE_list, function(df) unique(df$param_name))))

# 生成空矩阵：行=参数，列=物种_指标组合
# 先收集所有 species 和 indicator 组合
species_all <- unique(unlist(lapply(EE_list, function(df) unique(df$species))))

# 组合列名
cols <- c()
for (ind in indicators){
  spc <- if(ind %in% c("LFI", "yield_rel")) setdiff(species_all, c("sp4","sp6")) else species_all
  cols <- c(cols, paste(spc, ind, sep="_"))
}

# 初始化矩阵
mu_star_mat <- matrix(NA, nrow=length(all_params), ncol=length(cols),
                      dimnames = list(all_params, cols))

# 填充矩阵
for(ind in indicators){
  df <- EE_list[[ind]]
  spc_vec <- if(ind %in% c("LFI", "yield_rel")) setdiff(species_all, c("sp4","sp6")) else species_all
  
  for(sp in spc_vec){
    # 找对应列名
    colname <- paste(sp, ind, sep="_")
    # 筛数据
    sub_df <- df %>% filter(species == sp)
    # 用参数名匹配填充 mu_star
    for(pn in all_params){
      val <- sub_df %>% filter(param_name == pn) %>% pull(mu_star)
      if(length(val)==1) {
        mu_star_mat[pn, colname] <- val
      }
    }
  }
}

# 处理NA（可以选择替换为0或者行/列均值，视情况而定）
mu_star_mat[is.na(mu_star_mat)] <- 0


# 按列做 z-score 标准化
mu_star_z <- scale(mu_star_mat, center=TRUE, scale=TRUE)

# 计算每列标准差（scale后其实就是1或0）
col_sd <- apply(mu_star_mat, 2, sd)

# 找出非零方差列
zero_cols <- which(col_sd == 0)
non_zero_cols <- which(col_sd > 0)


# 只保留有变异的列
mu_star_mat_filtered <- mu_star_mat[, non_zero_cols]

# 再做一次标准化
mu_star_z_filtered <- scale(mu_star_mat_filtered, center=TRUE, scale=TRUE)

# PCA
pca_res <- prcomp(mu_star_z_filtered, center=FALSE, scale.=FALSE)
# 因为scale()已经标准化，所以这里不需要重复

# 查看结果
summary(pca_res)
biplot(pca_res, cex=0.5)
