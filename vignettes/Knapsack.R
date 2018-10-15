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


## ----message=FALSE, warning=FALSE, eval= FALSE---------------------------
#  
#  system.time(brute_force_knapsack(x = knapsack_objects[1:30,], W = 3000))
#  

## ------------------------------------------------------------------------

#user    system   elapsed 
#11.72     0.05     12.08 
  


## ----message=FALSE, warning=FALSE, eval = FALSE--------------------------
#  
#  system.time(brute_force_knapsack(x = knapsack_objects[1:30,], W = 3000, parallel = TRUE))
#  

## ------------------------------------------------------------------------

#user    system   elapsed 
#1.94     1.24     11.79 
  


## ----include=FALSE, eval=FALSE, message=FALSE, warning=FALSE-------------
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

## ----message=FALSE, warning=FALSE, eval=FALSE----------------------------
#  system.time(knapsack_dynamic(x = knapsack_objects[1:500,], W = 2000))
#  
#  system.time(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500))
#  

## ------------------------------------------------------------------------
#For W=2000
#user    system   elapsed 
#84.57     0.02     84.69 
  
#For W=3500

#user    system   elapsed 
#391.42     0.16    392.93 

## ----include=FALSE, eval=FALSE, message=FALSE, warning=FALSE-------------
#  
#  greedy_knapsack <- function(x,W)
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
#    x$z <- x$v/x$w
#    x <-x [with(x, order(-z)),]
#    i<- 1
#    weight <- 0
#    value <- 0
#    elements <- list()
#    while (i<n+1 && weight + x$w[[i]]<W  )
#    {
#      weight <- weight + x$w[[i]]
#      value <- value + x$v[[i]]
#      elements[[i]] <- x$ord[[i]]
#      i<- i+1
#    }
#  
#    return(list(value=round(value,0), elements=unlist(elements, use.names = FALSE) ))
#  }
#  

## ----message=FALSE, warning=FALSE, eval=FALSE----------------------------
#  system.time(greedy_knapsack(x = knapsack_objects[1: 1000000,], W = 2000))
#  
#  system.time(greedy_knapsack(x = knapsack_objects[1: 1000000,], W = 3500))
#  

## ------------------------------------------------------------------------
#For W=2000
#user    system   elapsed 
#0.18      0.01      0.20 
   
#For W=3500
#user    system   elapsed 
#0.36      0.00      0.38

## ----eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-------------
#  
#  # context("brute_force_knapsack")
#  #
#  # set.seed(42)
#  # n <- 2000
#  # knapsack_objects <- data.frame(
#  #   w=sample(1:4000, size = n, replace = TRUE),
#  #   v=runif(n = n, 0, 10000)
#  # )
#  #
#  # test_that("Correct object is returned", {
#  #   expect_silent(bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
#  #   expect_named(bfk, c("value", "elements"))
#  # })
#  #
#  #
#  # test_that("functions rejects errounous input.", {
#  #   expect_error(brute_force_knapsack("hej", 3500))
#  #   expect_error(brute_force_knapsack(x = knapsack_objects[1:8,], W = -3500))
#  # })
#  #
#  # test_that("Function return correct results.", {
#  #   bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#  #   expect_equal(round(bfk$value), 16770)
#  #   expect_true(all(round(bfk$elements) %in% c(5, 8)))
#  #
#  #   bfk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#  #   expect_equal(round(bfk$value), 16770)
#  #   expect_true(all(round(bfk$elements) %in% c(5, 8)))
#  #
#  #   bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#  #   expect_equal(round(bfk$value), 15428)
#  #   expect_true(all(round(bfk$elements) %in% c(3, 8)))
#  #
#  #   bfk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#  #   expect_equal(round(bfk$value), 15428)
#  #   expect_true(all(round(bfk$elements) %in% c(3, 8)))
#  #
#  #   st <- system.time(bfk <- brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))
#  #   expect_true(as.numeric(st)[2] >= 0.00)
#  # })
#  

## ----eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-------------
#  
#  # context("greedy_knapsack")
#  #
#  # set.seed(42)
#  # n <- 2000
#  # knapsack_objects <- data.frame(
#  #   w=sample(1:4000, size = n, replace = TRUE),
#  #   v=runif(n = n, 0, 10000)
#  # )
#  #
#  # test_that("Correct object is returned", {
#  #   expect_silent(gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500))
#  #   expect_named(gk, c("value", "elements"))
#  # })
#  #
#  # test_that("functions rejects errounous input.", {
#  #   expect_error(greedy_knapsack("hej", 3500))
#  #   expect_error(greedy_knapsack(x = knapsack_objects[1:8,], W = -3500))
#  # })
#  #
#  # test_that("Function return correct results.", {
#  #   gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
#  #   expect_equal(round(gk$value), 15428)
#  #   expect_true(all(round(gk$elements) %in% c(3, 8)))
#  #
#  #   gk <- greedy_knapsack(x = knapsack_objects[1:12,], W = 3500)
#  #   expect_equal(round(gk$value), 15428)
#  #   expect_true(all(round(gk$elements) %in% c(3, 8)))
#  #
#  #   gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 2000)
#  #   expect_equal(round(gk$value), 15428)
#  #   expect_true(all(round(gk$elements) %in% c(3, 8)))
#  #
#  #   gk <- greedy_knapsack(x = knapsack_objects[1:12,], W = 2000)
#  #   expect_equal(round(gk$value), 15428)
#  #   expect_true(all(round(gk$elements) %in% c(3, 8)))
#  #
#  #   st <- system.time(gk <- greedy_knapsack(x = knapsack_objects[1:16,], W = 2000))
#  #   expect_true(as.numeric(st)[2] <= 0.01)
#  #
#  #   gk <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#  #   expect_equal(round(gk$value), 192647)
#  #
#  #   gk <- greedy_knapsack(x = knapsack_objects[1:1200,], W = 3500)
#  #   expect_equal(round(gk$value), 270290)
#  # })
#  

## ----eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-------------
#  
#  # context("knapsack_dynamic")
#  #
#  #
#  # set.seed(42)
#  # n <- 2000
#  # knapsack_objects <- data.frame(
#  #   w=sample(1:4000, size = n, replace = TRUE),
#  #   v=runif(n = n, 0, 10000)
#  # )
#  #
#  # test_that("Correct object is returned", {
#  #   expect_silent(bfk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
#  #   expect_named(bfk, c("value", "elements"))
#  # })
#  #
#  #
#  # test_that("functions rejects errounous input.", {
#  #   expect_error(knapsack_dynamic("hej", 3500))
#  #   expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -3500))
#  # })
#  #
#  # test_that("Function return correct results.", {
#  #   bfk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#  #   expect_equal(round(bfk$value), 16770)
#  #   expect_true(all(round(bfk$elements) %in% c(5, 8)))
#  #
#  #   bfk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#  #   expect_equal(round(bfk$value), 16770)
#  #   expect_true(all(round(bfk$elements) %in% c(5, 8)))
#  #
#  #   bfk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#  #   expect_equal(round(bfk$value), 15428)
#  #   expect_true(all(round(bfk$elements) %in% c(3, 8)))
#  #
#  #   bfk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#  #   expect_equal(round(bfk$value), 15428)
#  #   expect_true(all(round(bfk$elements) %in% c(3, 8)))
#  #
#  #   st <- system.time(bfk <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 2000))
#  #   expect_true(as.numeric(st)[2] >= 0.00)
#  # })
#  

