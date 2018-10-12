#'Brute force
#'
#'Solving Knapsack problem with brute force approach
#'
#'@param x data.frame
#'@param W integer
#'@return list of ideal value and elements that solves the problem
#'@export

brute_force_knapsack <- function(x, W)
{
  stopifnot(is.data.frame(x) == T)
  stopifnot(ncol(x)==2)
  stopifnot(colnames(x)== list("w","v"))
  stopifnot(all(x$v>0))
  stopifnot(all(x$w>0))
  stopifnot(W>=0)
  ideal_value <- 0
  n = nrow(x)
  for (i in 1:(2^n-1)) 
  {
    permutation <- intToBits(i)
    weight <- sum(as.numeric(permutation[1:n])*x$w)
    value <- sum(as.numeric(permutation[1:n])*x$v)
    if (ideal_value<value && weight < W)
    {
      ideal_value <- value
      elements <- as.numeric(permutation[1:n])*1:n
    }
  }
  return(list(value=round(ideal_value,0), elements=which(elements>0)))
}



