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
  x$ord <- 1:n
  x <- x[which(x$w < W), ]
  n = nrow(x)
  m <- matrix(0, nrow=n, ncol=W+1)
  path <- as.data.frame(matrix(0,nrow = n, ncol = W+1))

  for(i in 1:n)
  {
    for(j in 1:W+1)
    {
      if (x$w[[i]] >j-1 && i==1) {path[[1,j]] <- list(0)}
      else if (x$w[[i]] >j-1) 
      {
        m[[i,j]] <- m[[i-1,j]]
        path[[i,j]] <- path[[i-1,j]]
      }
      else 
      {
        if(i==1) 
        {
          m[[i,j]] <- x$v[[i]]
          path[[i,j]] <- list(x$ord[[i]])
        }
        else
        {
          m[[i,j]] <- max(m[[i-1,j]],m[[i-1,j-x$w[[i]]]] + x$v[[i]])
          if(m[[i-1,j]] < m[[i-1,j-x$w[[i]]]] + x$v[[i]])
            { path[[i,j]] <- rlist::list.append(path[[i-1,j-x$w[[i]]]],x$ord[[i]]) }
          else
            path[[i,j]] <- path[[i-1,j]]
        }
      }
    }
  }
  elements <- path[[n,W+1]]
  elements = elements[which(elements>0)]
  return(list(value= round(m[[n,W+1]],0), elements=unlist(elements, use.names = FALSE) ))
}
