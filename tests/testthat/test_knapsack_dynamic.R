context("knapsack_dynamic")



test_that("Correct result", {
  expect_equal(knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500),list(value=16670, elements = list(5,8)))

})


test_that("correct input", {
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,]))
  expect_error(knapsack_dynamic(x = list(v=1000,w=500), W = 3500))
  expect_error(knapsack_dynamic(x = data.frame(v=c(1000,-1000),w=c(1,500)), W = 3500))
  expect_error(knapsack_dynamic(x = list(v=1000,w=500), W = "one"))
  }
  )