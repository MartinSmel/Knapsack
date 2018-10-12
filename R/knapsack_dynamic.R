#'Dynamic
#'
#'Solving Knapsack problem with dynamic programming
#'
#'@param x data.frame
#'@param W integer
#'@return list of ideal value and elements that solves the problem
#'@export


knapsack_dynamic <- function(x,W)
{
  stopifnot(is.data.frame(x) == T)
  stopifnot(ncol(x)==2)
  stopifnot(colnames(x)== list("w","v"))
  stopifnot(all(x$v>0))
  stopifnot(all(x$w>0))
  stopifnot(W>=0)
  n = nrow(x)
  m <- matrix(0, nrow=n, ncol=W+1)
  path <- as.data.frame(matrix(0,nrow = n, ncol = W+1))
  for(i in 1:W)
  {
    path[[1,i]] <- list(0)
  }

  for(i in 1:n)
  {
    for(j in 1:W+1)
    {
      if (x$w[[i]] >j-1 && i==1) {m[[i,j]] <- 0}
      else if (x$w[[i]] >j-1) 
      {
        m[[i,j]] <- m[[i-1,j]]
        path[[i,j]] <- path[[i-1,j]]
      }
      else {
        m[[i,j]] <- max(m[[i-1,j]],m[[i-1,j-x$w[[i]]]] + x$v[[i]])
        if(m[[i-1,j]] < m[[i-1,j-x$w[[i]]]] + x$v[[i]])
      #    path[[i,j]] <- 10*path[[i-1,j-x$w[[i]]]]+i
           path[[i,j]] <- list(list(unlist(path[[i-1,j-x$w[[i]]]], use.names = FALSE),i))
        else
          path[[i,j]] <- path[[i-1,j]]
      }
    }
  }
  elements = suppressWarnings(as.numeric(strsplit(as.character(path[[n,W+1]]), "")[[1]]))
  return(list(value= round(m[[n,W+1]],0), elements = elements[which(elements>0)]))
}
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)


# system.time(knapsack_dynamic(knapsack_objects[1:8,], 3500))
# user  system elapsed 
# 6.12    0.02    6.14 