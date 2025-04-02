
# Functions ---------------------------------------------------------------

# Principal function
random_sampling = function(par, r, levels, grid.jump, method="morris") {
  
  p        = nrow(par)
  parNames = par$parameter
  scale    = par$scale
  min      = par$min
  min_lin  = par$min_lin
  max      = par$max
  max_lin  = par$max_lin
  
  if(length(levels) == 1) { levels = rep(levels, p) }
  if(length(grid.jump) == 1) {grid.jump = rep(grid.jump, p)}
  
  X = .random_sampling(p = p, scale = scale, min = min, max = max,
                       min_lin = min_lin, max_lin = max_lin,
                       r = r, levels = levels, grid.jump = grid.jump)
  
  x_pars = aperm(array(data = t(X$X), dim = c(p, p+1, r)), c(2,1,3))
  x = aperm(array(data = t(X$unscaled), dim = c(p, p+1, r)), c(2,1,3))
  
  jumps = x[2:(p+1), , ,drop=FALSE] - x[1:p, , ,drop=FALSE] # p+1 x p x r
  delta = apply(jumps, c(1,3), sum) # p x r
  i_par = apply(jumps, c(1,3), function(x) which(x!=0)) # p x r
  
  # 
  # x  = matrix(x.unique, ncol = p, byrow = TRUE)
  
  dimNames = list(sample=1:(p+1), parameters=parNames, r=1:r)
  dimnames(x) = dimnames(x_pars) = dimNames
  
  output = list(doe = x_pars, parameter = parNames, scale = scale,
                min = min, max = max, min_lin = min_lin, max_lin = max_lin,
                r = r, levels = levels, grid.jump = grid.jump, 
                unscaled = x, jumps=jumps, delta=delta, i=i_par, method=method)
  class(output) = "doe"
  
  return(output)
}

# Internal functions
.random_sampling = function(p, scale, min, max, min_lin, max_lin, r, levels, grid.jump) {
  
  # B: orientation matrix
  B = matrix(-1, nrow = p + 1, ncol = p)
  B[lower.tri(B)] = 1
  
  # grid step
  delta = grid.jump / (levels - 1)
  X = matrix(nrow = r * (p + 1), ncol = p)
  
  for(j in seq_len(r)) {
    
    # D: direction matrix
    D = diag(sample(c(-1, 1), size = p, replace = TRUE), nrow = p)
    
    # P: permutation matrix
    perm = sample(p)
    P    = matrix(0, nrow = p, ncol = p)
    for(i in seq_len(p)) {P[i, perm[i]] = 1 }
    
    # x.base: starting point
    x.base = matrix(nrow = p + 1, ncol = p)
    for(i in seq_len(p)) {
      # random values from 0 to 1
      x.base[,i] = ((sample(levels[i] - grid.jump[i], size = 1) - 1) / (levels[i] - 1))
    }
    # cuts in linear scale
    X[ind.rep(j,p),] = 0.5 * (B %*% P %*% D + 1) %*% diag(delta, nrow = p) + x.base
  
  }
  
  # unscaled: points in linear scale
  unscaled = X
  
  # X: points in parameter scale
  X = sapply(1:p, function(i) 
    transform(x=X[, i], scale=scale[i], min=min_lin[i], max=max_lin[i], inverse=TRUE))
  
  return(list(X=X, unscaled=unscaled))
}
  



ind.rep = function(i, p) {
  x = (1 : (p + 1)) + (i - 1) * (p + 1)
  return(x)
}
