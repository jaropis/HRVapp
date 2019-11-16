### tests for runs.R
test_that("Testing if various runs are counted and interpreted correctly",{
  expect_equal(countRuns(c(1,2,2,1)), 
               list(list(c(1,2), c(2), c(1)), c("dec", "noChange", "acc")))
  expect_equal(countRuns(c(1,2)), list(list(c(1,2)), c("dec")))
  expect_equal(countRuns(c(1,1,1,1,1,1),list(list(1,1,1,1,1,1), c("noChange"))))
})
