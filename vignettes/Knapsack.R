## ----include=FALSE, eval=TRUE, message=FALSE, warning=FALSE--------------

set.seed(42)
n <- 1000000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


## ----include=FALSE, eval=TRUE, message=FALSE, warning=FALSE--------------

library(parallel)

brute_force_knapsack <- function(x, W, parallel=FALSE)
{
  stopifnot(is.data.frame(x) == T)
  stopifnot(ncol(x)==2)
  stopifnot(colnames(x)== list("w","v"))
  stopifnot(all(x$v>0))
  stopifnot(all(x$w>0))
  stopifnot(W>=0)
  ideal_value <- 0
  n = nrow(x)
  x$label <- 1:n
  x <- x[which(x$w < W), ]
  n = nrow(x)
  if (parallel == TRUE) {
    cluster = makeCluster(makeCluster(detectCores() - 1))
    permutation <- parSapply(cluster, 1:(2^n-1), function(i) {intToBits(i)[1:n]}, simplify="array")
    weight <- parSapply(cluster, 1:(2^n-1), function(i) {sum(as.numeric(permutation[,i])*x$w) }, simplify="array")
    value <- parSapply(cluster, 1:(2^n-1), function(i) {sum(as.numeric(permutation[,i])*x$v) }, simplify="array")
    stopCluster(cluster)
  }
  else
  {
  permutation <- sapply(1:(2^n-1), function(i) { intToBits(i)[1:n]}, simplify="array")
  weight <- sapply(1:(2^n-1), function(i) { sum(as.numeric(permutation[,i])*x$w) }, simplify="array")
  value <- sapply(1:(2^n-1), function(i) { sum(as.numeric(permutation[,i])*x$v) }, simplify="array")
  }
  for (i in 1:(2^n-1))  
  {
    if (ideal_value<value[[i]] && weight[[i]] < W)
    {
      ideal_value <- value[[i]]
      elements <- as.numeric(permutation[,i])*x$label
    }
  }
  return(list(value=round(ideal_value,0), elements=elements[which(elements>0)])) 
  
}



## ----message=FALSE, warning=FALSE----------------------------------------
system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))

system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))


## ----message=FALSE, warning=FALSE----------------------------------------

system.time(brute_force_knapsack(x = knapsack_objects[1:30,], W = 3000)) 


## ----message=FALSE, warning=FALSE----------------------------------------

system.time(brute_force_knapsack(x = knapsack_objects[1:30,], W = 3000, parallel = TRUE))


## ----include=TRUE, eval=FALSE, message=FALSE, warning=FALSE--------------
#  
#  knapsack_dynamic <- function(x,W)
#  {
#    stopifnot(is.data.frame(x) == T)
#    stopifnot(ncol(x)==2)
#    stopifnot(colnames(x)== list("w","v"))
#    stopifnot(all(x$v>0))
#    stopifnot(all(x$w>0))
#    stopifnot(W>=0)
#    n = nrow(x)
#    x$ord <- 1:n
#    x <- x[which(x$w < W), ]
#    n = nrow(x)
#    m <- matrix(0, nrow=n, ncol=W+1)
#    path <- as.data.frame(matrix(0,nrow = n, ncol = W+1))
#  
#    for(i in 1:n)
#    {
#      for(j in 1:W+1)
#      {
#        if (x$w[[i]] >j-1 && i==1) {path[[1,j]] <- list(0)}
#        else if (x$w[[i]] >j-1)
#        {
#          m[[i,j]] <- m[[i-1,j]]
#          path[[i,j]] <- path[[i-1,j]]
#        }
#        else
#        {
#          if(i==1)
#          {
#            m[[i,j]] <- x$v[[i]]
#            path[[i,j]] <- list(x$ord[[i]])
#          }
#          else
#          {
#            m[[i,j]] <- max(m[[i-1,j]],m[[i-1,j-x$w[[i]]]] + x$v[[i]])
#            if(m[[i-1,j]] < m[[i-1,j-x$w[[i]]]] + x$v[[i]])
#              { path[[i,j]] <- rlist::list.append(path[[i-1,j-x$w[[i]]]],x$ord[[i]]) }
#            else
#              path[[i,j]] <- path[[i-1,j]]
#          }
#        }
#      }
#    }
#    elements <- path[[n,W+1]]
#    elements = elements[which(elements>0)]
#    return(list(value= round(m[[n,W+1]],0), elements=unlist(elements, use.names = FALSE) ))
#  }
#  

