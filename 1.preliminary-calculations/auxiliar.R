
angleEstimation = function(m_min, m_max){
  
  angle = atan((m_max - m_min) / (1 + m_max * m_min))
  
  return(angle)
}

maxSlope = function(angle, m_min){
  
  m_max = (tan(angle) + m_min) / (1 - tan(angle) * m_min)
  
  return(m_max)
}

rangeEstimation = function(par, x = "x", scale = "scale", percentage = "percentage") {
  
  names = colnames(par)
  
  if(! x %in% names ) stop("A column with the parameters values should be indicated")
  if(! scale %in% names ) stop("A scale should be provided for the limits estimation")
  if(! percentage %in% names ) stop("A column with percentage values should be provided")
  
  x          = par[, x]
  scale      = par[, scale]
  percentage = par[, percentage]
  
  n = nrow(par)
  
  range_min = rep(NA_real_, n)
  range_max = rep(NA_real_, n)
  
  for(i in seq_len(n)){
    ranges = .rangeEstimation(x=x[i], scale=scale[i], percentage=percentage[i])
    range_min[i] = ranges$range_min
    range_max[i] = ranges$range_max
  }
  
  par$range_min = range_min
  par$range_max = range_max
  
  return(par)
}

.rangeEstimation = function(x, scale, percentage){
  
  if(percentage > 1){
    message("automatic conversion of percentage (value / 100)")
    percentage = percentage/100
  }
  
  if(scale == "linear") ranges = linear_range(x, percentage)
  if(scale == "log")    ranges = log_range(x, percentage)
  if(scale == "logit")  ranges = logit_range(x, percentage)
  
  output = list(range_min = ranges$range_min, range_max = ranges$range_max)
  
  return(output)
  
}

linear_range = function(x, percentage){
  
  range_min = x*percentage
  range_max = x*percentage
  
  return(list(range_min = range_min, range_max = range_max))
}

log_range = function(x, percentage){
  
  range_min = log(1/(1 - percentage))
  range_max = log(1+percentage)
  
  return(list(range_min = range_min, range_max = range_max))
}

logit_range = function(x, percentage){
  
  p = percentage
  pcrit = (1/x - 1) 
  if(p > pcrit) warning(sprintf("Upper percentage %g exceeds critical value of %g, upper limit will exceed 1. Setting upper range to 10.", p, pcrit))
  range_min = +log((1/(1-p) - x)/(1-x))
  range_max = suppressWarnings(-log((1/(1+p) - x)/(1-x)))
  if(any(is.nan(range_max))) range_max[is.nan(range_max)] = 10

  # range_min = - log(1-x) + log(1 - percentage*x) - log(percentage)
  # range_max =   log(1-x) - log(1 - percentage*x) + log(percentage)
  
  return(list(range_min = range_min, range_max = range_max))
}