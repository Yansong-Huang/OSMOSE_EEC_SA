#indicator 1
.MeanLength = function(abundance, meanLength){
  
  total_abundance = apply(abundance, c(1,3), sum, na.rm=TRUE)
  weighted_length = apply(abundance*meanLength, c(1,3), sum, na.rm=TRUE)
  meanLength      = weighted_length / total_abundance
  
  return(meanLength)
}

#indicator 2
.MeanTL = function(biomass, meanTL){
  
  total_biomass = apply(biomass, c(1,3), sum, na.rm=TRUE)
  weighted_TL   = apply(biomass*meanTL, c(1,3), sum, na.rm = TRUE)
  meanTL        = weighted_TL / total_biomass
  
  return(meanTL)
}

#indicator 3
.MeanLifespan = function(biomass){
  
  lifespan          = c(3,12,8,8,10,2,4,1.5,1)
  names(lifespan)   = paste0("sp",c(0:8))
  
  total_biomass  = apply(biomass, c(1,3), sum, na.rm=TRUE)
  weighted_lspan = aperm(apply(biomass, c(1, 3), FUN="*", y=lifespan), c(2,1,3))
  weighted_lspan = apply(weighted_lspan, c(1,3), sum, na.rm = TRUE)
  meanLS         = weighted_lspan / total_biomass
  
  return(meanLS)
}

#indicator 4
.BiomassOverYield = function(biomass, yield){
  
  totalYield        = apply(yield, c(1, 3), sum, na.rm=TRUE)
  totalBiomass      = apply(biomass, c(1, 3), sum, na.rm=TRUE)
  biomassOverYield  = totalBiomass/totalYield
  
  return(biomassOverYield)
}

#indicator 5
.MTI = function(meanTL, yield){

  TL         = apply(meanTL, c(1,2,3), function(x) x > 3.25)
  new_yield  = yield*TL
  new_TL     = meanTL*TL
  
  totalYield = apply(new_yield, c(1,3), sum, na.rm = TRUE)
  weighted_TL= apply(new_TL*new_yield, c(1,3), sum, na.rm = TRUE)
  MTI        = weighted_TL / totalYield
 
  return(MTI)
}

#indicator 6
.FitSizeSpectrum = function(sizeSpectrum, each = 10, output = "slope"){
  
  #Size spectrum aggregation
  by = (dim(sizeSpectrum)[2])/each
  x = var_aggregation(x = sizeSpectrum, each = each)
  colnames(x) = seq(from = each, by = each, length.out = by)
  sizes = as.numeric(colnames(x))
  marks = sizes - 0.5*mean(diff(sizes)) #correct????
  
  # aggregation by replicates and time
  sizeTotal = apply(apply(x, c(1,2), mean, na.rm = TRUE), 2, mean, na.rm = TRUE)
  names(sizeTotal) = marks
  
  # linear regression
  ln_length = log(as.numeric(names(sizeTotal))) #x
  ln_number = log(sizeTotal) #y
  ln_number[is.na(ln_number) | ln_number == "-Inf"] = NA #is it ok?
  fit = lm(formula = ln_number ~ ln_length, na.action = na.omit)
  #plot(x = ln_number, y = ln_length)
  
  if(output == "slope"){return(fit$coefficients[2])}
  if(output == "intercept"){return(fit$coefficients[1])}

}

#indicator 7
.LFI = function(biomass, sizeSpectrumB, thr){
  
  sizes = as.numeric(colnames(sizeSpectrumB[[1]]))
  marks = sizes + 0.5*mean(diff(sizes))
  
  new_sizes = sizeSpectrumB
  for (i in seq_along(new_sizes)){
    colnames(new_sizes[[i]]) <- marks
  }
  
  new_sizes = lapply(new_sizes, FUN = function(x) x[, marks > thr, ] )
  new_sizes = lapply(new_sizes, FUN = function(x) apply(x, c(1,3), sum, na.rm = TRUE))
  
  new_sizes = Reduce(`+`, new_sizes)
  totalBiomass = apply(biomass, c(1, 3), sum, na.rm = TRUE)
  
  lfi = new_sizes/totalBiomass
  
  return(lfi)
}

.LFI_by_species <- function(biomass, sizeSpectrumB, thr) {
  
  sizes <- as.numeric(colnames(sizeSpectrumB[[1]]))
  marks <- sizes + 0.5 * mean(diff(sizes))
  
  # 统一 colnames
  for (i in seq_along(sizeSpectrumB)) {
    colnames(sizeSpectrumB[[i]]) <- marks
  }
  
  time_steps <- dim(biomass)[1]
  n_species <- dim(biomass)[2]   # 第二维是物种
  replicates <- dim(biomass)[3]
  
  # 初始化结果数组：时间 × 物种 × 重复
  result <- array(NA, dim = c( time_steps, n_species, replicates))
  
  for (sp in seq_len(n_species)) {
    bio <- biomass[ ,sp , ]  # time x replicate 矩阵
    sizeB <- sizeSpectrumB[[sp]]  # time x size x replicate
    
    # 非捕捞物种过滤
    if (all(is.na(bio)) || sum(bio, na.rm = TRUE) == 0) {
      next
    }
    
    # 体长超过阈值部分的捕捞量求和
    large_bio <- sizeB[, marks > thr, , drop = FALSE]  # time x size_filtered x replicate
    large_bio_sum <- apply(large_bio, c(1, 3), sum, na.rm = TRUE)  # time x replicate
    
    # 总捕捞量，直接用 bio，因为已经是 time x replicate
    total_bio <- bio  # time x replicate
    
    # 计算比例，注意除数为0时处理
    ratio <- large_bio_sum / total_bio
    ratio[is.nan(ratio)] <- NA
    
    result[,sp , ] <- ratio
  }
  
  dimnames(result) <- list(
    time = dimnames(biomass)[[1]],
    species = dimnames(biomass)[[2]],
    replicate = dimnames(biomass)[[3]]
  )
  
  return(result)
}

.YieldBySize_to_df <- function(yield_by_size) {
  out <- list()
  for (sp in seq_along(yield_by_size)) {
    arr <- yield_by_size[[sp]]  # 20 × 26 × 10 array
    dimnames(arr) <- list(
      year = 1:dim(arr)[1],
      length = 1:dim(arr)[2],
      rep = 1:dim(arr)[3]
    )
    df <- as.data.frame.table(arr, responseName = "yield")
    df$species <- sp
    out[[sp]] <- df
  }
  do.call(rbind, out)
}
