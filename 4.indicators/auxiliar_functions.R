
var_aggregation = function(x, each = 4) {
  
  breaks = (dim(x)[2])/each
  new_x = array(NA, dim = c(dim(x)[1], breaks, dim(x)[3]))
  
  for(i in seq_len(dim(x)[3])){
    
    new_x[,,i] = .var_aggregation(x = x[,,i], each = each, breaks = breaks)
    colnames(new_x) = c(0:(breaks-1)) #make it more flexible
  }
  
  return(new_x)
}

.var_aggregation = function(x, each, breaks){
  
  x = sapply(split.default(as.data.frame(x), rep(paste0("x", 1:breaks), each = each)), Reduce, f = '+')
  
  # order data names
  colnames(x) = as.numeric(substring(colnames(x),2))
  vectorPos   = as.numeric(colnames(x))
  x           = x[, order(vectorPos)]
  colnames(x) = c(0:(breaks-1)) 
  
  return(x)
}
