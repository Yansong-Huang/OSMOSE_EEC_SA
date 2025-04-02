ee.morris = function(x) {
  
  doe = x$doe
  y   = x$y
  min_lin = x$min_lin
  max_lin = x$max_lin
    
  # The input design of experiments is scaled after building the design
  # and before computing the elementary effects so that all factors vary within
  # the range [0,1]
  n = ncol(doe)
  doe = sapply(1:n, function(i){transform(x = doe[, i], scale = x$scale[i])})
  doe = sapply(1:n, function(i){linear_escale(x = doe[,i], min = min_lin[i], max = max_lin[i], rescale = TRUE)})
  
  x$ee = .ee(doe, y)

  return(x)
}

.ee = function(doe, y) {
  
  p = ncol(doe)
  r = nrow(doe) / (p+1)
  
  # if y is numeric
  if(class(y) == "numeric"){
    
    i_vector = function(i){
      j = ind.rep(i, p)
      j1 = j[1 : p ]
      j2 = j[2 : (p+1) ]
      
      return(solve(doe[j2, ] - doe[j1, ], y[j2] - y[j1]))
    }
    ee = vapply(seq_len(r), i_vector, FUN.VALUE = numeric(p))
    ee = t(ee)
  }
  
  # if y is matrix
  if(class(y) == "matrix"){
    
    i_matrix = function(i){
      j = ind.rep(i, p)
      j1 = j[1 : p ]
      j2 = j[2 : (p+1) ]
      
      return(solve(doe[j2, ] - doe[j1, ], y[j2, , drop = FALSE] - y[j1, , drop = FALSE]))
    }
    ee = vapply(seq_len(r), i_matrix, FUN.VALUE = matrix(0, nrow = p, ncol = dim(y)[2]))
    ee = aperm(ee, perm = c(3, 1, 2))
  }
  
  # pending: if y is array
  
  return(ee)
}


# new ---------------------------------------------------------------------

sensitivity = function(X, Y, method) {

  msg = sprintf("Sensitivity method %s is not implemented yet", method)
  
  x = switch (method,
              elementary_effects = elementary_effects(X = X, Y = Y),
              stop(msg))
  
  return(x)
  
}

elementary_effects = function(X, Y) {
  
  delta = X$delta
  i = X$i
  
  # manipular Y para que sea una lista con matrices (p x r)
  out = lapply(Y, unlist) # unlist each component
  out = do.call(rbind, lapply(out, as.numeric)) # each output to a vector
  # reshape list: one list element per output
  Y = lapply(seq_len(ncol(out)), FUN=function(i) matrix(out[, i], ncol=X$r))
  dimNames = list(sample=seq_len(nrow(Y[[1]])), r=seq_len(ncol(Y[[1]])))
  Y = lapply(Y, FUN='dimnames<-', value=dimNames)
  
  di = lapply(lapply(Y, diff), FUN="/", e2=delta)
  di = lapply(di, FUN=.sort_i, i=i)
  
  class(di) = "ee"
  return(di)
} 

.sort_i = function(y, i) {
  out = data.frame(i=as.numeric(i), di=as.numeric(y))
  out = out[order(out$i), ]
  return(out)
}

# this function defines what indicators are calculated over the ee.
.metrics = function(x) c(mu=mean(x), sd=sd(x), mu_star=mean(abs(x)))

.ee_metrics = function(x) {
  out = aggregate(x$di, by=list(x$i), .metrics)
  out = cbind(out[,1], out[,2])
  colnames(out)[1] = "parameter"
  out = as.data.frame(out)
  return(out)
}


