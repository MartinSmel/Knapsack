#' Knapsack_objects
#'
#' Creating a data set
#'
#' @docType data
#'
#' @keywords datasets
#'
#'
#' @export
"knapsack_objects"

set.seed(42)
n <- 1000000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
save(knapsack_objects, file="data/knapsack_objects.RData",compress='xz')