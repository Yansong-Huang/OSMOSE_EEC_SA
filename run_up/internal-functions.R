# Functions ---------------------------------------------------------------

# Functions to estimate the limits of the parameters
get_limits = function(par, x = "x", scale = "scale", range = NULL,
                      range_min = NULL, range_max = NULL) {
  
  names = colnames(par)
  
  if(! x %in% names ) {stop("A column with the parameters values should be indicated")}
  if(! scale %in% names ) {stop("A scale should be provided for the limits estimation")}
  if(is.null(range) & is.null(range_min) & is.null(range_max)) {
    stop("Ranges should be provided for the limits estimation")}
  
  par$x     = par[, x]
  par$scale = par[, scale]
  
  if(!is.null(range_min)) {
    par$range_min = par[, range_min]
  } else {
    par$range_min = par[, range]
  }
  
  if(!is.null(range_max)){
    par$range_max = par[, range_max]
  } else {
    par$range_max = par[, range]
  }
  
  n = nrow(par)
  
  min = rep(NA_real_, n)
  max = rep(NA_real_, n)
  min_lin = rep(NA_real_, n)
  max_lin = rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    limits     = .create_limits(par[i, ])
    limits_lin = .create_limits(par[i, ], linear=TRUE)
    min[i] = limits$min
    max[i] = limits$max
    min_lin[i] = limits_lin$min
    max_lin[i] = limits_lin$max
  }
  
  par$min = min
  par$max = max
  par$min_lin = min_lin
  par$max_lin = max_lin
  
  return(par)
}

.create_limits = function(par, linear=FALSE){
  
  x = par$x
  scale = par$scale
  range_min = par$range_min
  range_max = par$range_max
  
  if(range_min<=0) stop("range must be positive.")
  if(range_max<=0) stop("range must be positive.")
  
  if(scale == "linear") limits = linear_perturbation(x, range_min, range_max, linear)
  if(scale == "log")    limits = log_perturbation(x, range_min, range_max, linear)
  if(scale == "logit")  limits = logit_perturbation(x, range_min, range_max, linear)
  
  output = list(min = limits$min, max = limits$max, scale = scale)
  
  return(output)
}

linear_perturbation = function(x, range_min, range_max, linear=FALSE){
  
  min  = x - range_min
  max  = x + range_max
  
  return(list(min = min, max = max))
}

log_perturbation = function(x, range_min, range_max, linear=FALSE) {
  
  if(x<=0) stop("parameters in log scale must be positive.")
  
  min = log(x) - range_min
  max = log(x) + range_max
  
  if(isTRUE(linear)) return(list(min = min, max = max))
  
  min = exp(min)
  max = exp(max)
  
  return(list(min = min, max = max))
}

logit_perturbation = function(x, range_min, range_max, linear=FALSE) {
  
  if(x>1 | x<0) stop("parameters in logit scale must be between 0 and 1.")
  
  tiny = 1e-10
  x = pmin(pmax(tiny, x), 1-tiny)
  
  min = log(x/(1-x)) - range_min
  max = log(x/(1-x)) + range_max

  if(isTRUE(linear)) return(list(min = min, max = max))
  
  min = 1/(1+exp(-min))
  max = 1/(1+exp(-max))
  
  return(list(min = min, max = max))
  
}

# Functions to rescale parameter values
transform = function(x, scale, min, max, inverse = FALSE) {
  
  # rescale transformed parameters (in the linear space)
  x = linear_scale(x=x, min=min, max=max, rescale=FALSE)
  
  # invert the transformation (return to original scale)
  
  if(isTRUE(inverse)){
    if(scale == "linear") return( x )
    if(scale == "log")    return( exp(x) )
    if(scale == "logit")  return( 1/(1+exp(-x)) )
  } else {
    if(scale == "linear") return( x )
    if(scale == "log")    return( log(x) )
    if(scale == "logit")  return( log(x/(1-x)) )
  }
  
}

# escale_fake = function(x, scale, min, max, rescale){
#   
#   if(scale == "linear") x = linear_escale(x = x, min = min, max = max, rescale = rescale)
#   if(scale == "log")    x = linear_escale(x = x, min = min, max = max, rescale = rescale)
#   if(scale == "logit")  x = linear_escale(x = x, min = min, max = max, rescale = rescale)
#   
#   return(x)
# }

linear_scale = function(x, min, max, rescale=FALSE) {
  
  if(isTRUE(rescale)) return((x - min) / (max - min)) # to scale from 0 to 1

  # to scale in parameter scale
  return(x*(max - min) + min)
  
}

log_scale = function(x, min, max, rescale) {
  
  if(isFALSE(rescale)) { out = log(x)*(max - min) + min }     # to scale in parameter scale
  
  if(isTRUE(rescale))  { out = (log(x) - min) / (max - min) } # to scale from 0 to 1
  
  output = exp(out)
  return(output)
}

logit_scale = function(x, min, max, rescale) {
  
  if(isFALSE(rescale)) { out = log(x/(1-x))*(max - min) + min }     # to scale in parameter scale
  
  if(isTRUE(rescale))  { out = (log(x/(1-x)) - min) / (max - min) } # to scale from 0 to 1
  
  output = 1/(1+exp(-out))
  return(output)
}


# Running the experiments -------------------------------------------------


# run_experiments = function(X, FUN, ..., parallel=FALSE, control=list()) {
#   UseMethod("run_experiments")
# }
# 
# run_experiments.doe = function(X, FUN, ..., parallel=FALSE, control=list()) {
#   # TO DO:
#   # parallelization
#   # optimization of duplicates 
# 
#   out = if(isTRUE(parallel)) {
#     .run_experiments_doe_parallel(X=X, FUN=FUN, ..., control=control)    
#   } else {
#     .run_experiments_doe_seq(X=X, FUN=FUN, ..., control=control)
#   }
#   
#   class(out) = c("doe_output", class(out))
#   attr(out, "doe") = X
#   
#   return(out)
# }
# 
# 
# .run_experiments_doe_seq = function(X, FUN, ..., control=list()) {
#   # TO DO:
#   # parallelization
#   # optimization of duplicates 
#   
#   # wrap FUN and ...
#   FUN = match.fun(FUN)
#   fn  = function(par, .i=0) FUN(par, ...) # non-parallel
#   
#   # matrix of parameters
#   par = aperm(X$doe, c(2,1,3))
#   par = matrix(par, nrow=prod(dim(par)[-1]), ncol=dim(par)[1], byrow=TRUE)
#   
#   r = dim(X$doe)[3]
#   
#   # store outputs in a list
#   out = vector(mode = "list", length = nrow(par))
#   for(i in seq_len(nrow(par))) out[[i]] = fn(par[i, , drop=FALSE], .i=i-1)
#   
#   return(invisible(out))
# }
# 
# .run_experiments_doe_parallel = function(X, FUN, ..., control=list()) {
#   # TO DO:
#   # parallelization
#   # optimization of duplicates 
#   
#   if(is.null(control$output)) control$output = "doe"
#   if(is.null(control$output.dir)) control$output.dir = getwd()
#   # if(is.null(control$restart)) control$restart = "restart.txt"
#   
#   # wrap FUN and ...
#   FUN = match.fun(FUN)
#   fn  = function(par, .i=0) FUN(par, ...) # non-parallel
#   
#   # matrix of parameters
#   par = aperm(X$doe, c(2,1,3))
#   par = matrix(par, nrow=prod(dim(par)[-1]), ncol=dim(par)[1], byrow=TRUE)
#   
#   r = dim(X$doe)[3]
#   
#   # store outputs in a list
#   out = vector(mode = "list", length = nrow(par))
#   Nmax = floor(log10(nrow(par))) + 1
#   patt = sprintf("%s_%%0%dd.rds", control$output, Nmax)
#   files = file.path(control$output.dir, sprintf(patt, seq_len(nrow(par))))
#   
#   # last.i = 0
#   # if(file.exists(control$restart)) last.i = as.numeric(readLines(control$restart)[1])
#   
#   for(i in seq_len(nrow(par))) {
#   
#     # if(i <= last.i) next # skip up to last.i  
#     tmp = fn(par[i, , drop=FALSE], .i=i-1)
#     saveRDS(tmp, file=files[i])
#     
#     # writeLines(text=as.character(i), control$restart) # update last.i
#     
#   }
#   
#   out = lapply(files, FUN = readRDS) 
#   
#   return(invisible(out))
# }

