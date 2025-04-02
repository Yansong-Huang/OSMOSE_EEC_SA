print.doe = function(x, ...) {
  cat("DOE:\n")
  print(x$doe)
  cat("\n unscaled DOE:\n")
  print(x$unscaled)
}

print.ee = function(x, ...) {
  print.default(x[])
}

summary.ee = function(object, ...) {
  
  metrics = lapply(object, FUN = .ee_metrics)
  class(metrics) = "summary.ee"
  return(metrics)  
}

print.summary.ee = function(x, ...) {
  cat("Summary of elementary effects:\n")
  print.default(x[])
}
