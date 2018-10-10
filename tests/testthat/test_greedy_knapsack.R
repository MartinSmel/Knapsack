context("greedy_knapsack")



test_that("Correct result", {
  expect_equal(greedy_knapsack(x = knapsack_objects[1:8,], W = 3500),list(value=15428, elements = list(8,3)))
  
})


test_that("correct input", {
  expect_error(greedy_knapsack(x = knapsack_objects[1:8,]))
  expect_error(greedy_knapsack(x = list(v=1000,w=500), W = 3500))
  expect_error(greedy_knapsack(x = data.frame(v=c(1000,-1000),w=c(1,500)), W = 3500))
  expect_error(greedy_knapsack(x = list(v=1000,w=500), W = "one"))
}
  )