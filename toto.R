


# CODE Criscely internal-functions.R
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
#     .run_experiments_doe_parallel(X=X, FUN=FUN, ..., control=list())    
#   } else {
#     .run_experiments_doe_seq(X=X, FUN=FUN, ..., control=list())
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
#   if(is.null(control$restart)) control$restart = "restart.txt"
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
#   last.i = 0
#   if(file.exists(control$restart)) last.i = as.numeric(readLines(control$restart)[1])
#   
#   for(i in seq_len(nrow(par))) {
#     
#     if(i <= last.i) next # skip up to last.i  
#     tmp = fn(par[i, , drop=FALSE], .i=i-1)
#     saveRDS(tmp, file=files[i])
#     
#     writeLines(text=as.character(i), control$restart) # update last.i
#     
#   }
#   
#   out = lapply(files, FUN = readRDS) 
#   
#   return(invisible(out))
# }




### Pour compiler l'ensemble des simu dans un fichier list output
files = file.path(control$output.dir, sprintf(patt, seq_len(nrow(par))))
out = lapply(files, FUN = readRDS) 
class(out) = c("doe_output", class(out))
attr(out, "doe") = X

return(out)