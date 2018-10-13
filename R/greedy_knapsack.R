#'Greedy
#'
#'Solving Knapsack problem with Greedy heuristic also adding Rcpp code
#'
#'@param x data.frame
#'@param W integer
#'@param fast logical
#'@return list of ideal value and elements that solves the problem
#'@import Rcpp
#'@export


greedy_knapsack <- function(x,W, fast = FALSE)
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
  
  if(fast == TRUE)
  {
    Rcpp::sourceCpp("src/division.cpp")
    z <- divisionCpp(x$v, x$w, n)
    
  x$z <- z
  }
  else
  {    }
    
    x$z <- x$v/x$w
  cppFunction('double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')
  
  
  x$z <- x$v/x$w
  x <-x [with(x, order(-z)),]
  i<- 1
  weight <- 0
  value <- 0
  elements <- list()
  while (i<n+1 && weight + x$w[[i]]<W  )
    {
      weight <- weight + x$w[[i]] 
      value <- value + x$v[[i]]
      elements[[i]] <- x$ord[[i]]
      i<- i+1
    }
  
  return(list(value=round(value,0), elements=unlist(elements, use.names = FALSE) ))
}

