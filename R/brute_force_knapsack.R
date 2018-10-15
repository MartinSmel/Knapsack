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
  x$label <- 1:n
  x <- x[which(x$w < W), ]
  n = nrow(x)
 
  permutation <- sapply(1:(2^n-1), function(i) { intToBits(i)[1:n]}, simplify="array")
  weight <- sapply(1:(2^n-1), function(i) { sum(as.numeric(permutation[,i])*x$w) }, simplify="array")
  value <- sapply(1:(2^n-1), function(i) { sum(as.numeric(permutation[,i])*x$v) }, simplify="array")
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




