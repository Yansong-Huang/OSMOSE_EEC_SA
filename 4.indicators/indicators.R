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
