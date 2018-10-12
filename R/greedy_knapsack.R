#'Greedy
#'
#'Solving Knapsack problem with Greedy heuristic
#'
#'@param x data.frame
#'@param W integer
#'@return list of ideal value and elements that solves the problem
#'@export


greedy_knapsack <- function(x,W)
{
  stopifnot(is.data.frame(x) == T)
  stopifnot(ncol(x)==2)
  stopifnot(colnames(x)== list("w","v"))
  stopifnot(all(x$v>0))
  stopifnot(all(x$w>0))
  stopifnot(W>=0)
  n = nrow(x)
  x$z <- x$v/x$w
  x$ord <- 1:n
  x <-x [with(x, order(-z)),]
  i<- 1
  weight <- 0
  value <- 0
  elements <- list()
  while (weight + x$w[[i]]<W)
  {
    weight <- weight + x$w[[i]] 
    value <- value + x$v[[i]]
    elements[[i]] <- x$ord[[i]]
    i<- i+1
  }
  return(list(value=round(value,0),elements=unlist(elements, use.names = FALSE)))
}

