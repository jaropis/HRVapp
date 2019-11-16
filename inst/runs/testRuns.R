### tests for runs.R
library(testthat)
source("runs.R")
context("The Runs application")
test_that("Testing if various runs are counted and interpreted correctly",{
  expect_equal(countRuns(c(1,2,2,1)), 
               list(allRuns = list(c(1,2), c(2), c(1)), directions=c("dec", "noChange", "acc")))
  expect_equal(countRuns(c(1,2)), list(allRuns = list(c(1,2)), directions = c("dec")))
  expect_equal(countRuns(c(1,1,1,1,1,1)),list(allRuns = list(c(1,1,1,1,1,1)), directions = c("noChange")))
  expect_error(countRuns(c(0)), "the lenght of the time series must be greater than 1")
  
  expect_equal(splitOnAnnot(c(1,1,1), c(0,0,0)), list(c(1,1,1)))
  expect_equal(splitOnAnnot(c(1,1,1), c(0,1,0)), list(c(1), c(1)))
  expect_equal(splitOnAnnot(c(1,1,1), c(1,1,1)), list())
  expect_equal(splitOnAnnot(c(1,1,1,1,1), c(0,0,1,0,0)), list(c(1,1), c(1,1)))
  
  expect_equal(splitAllIntoRuns(c(1,1,1,1,1), c(0,0,1,0,0)), 
               list(allRuns = list(c(1,1), c(1,1)), directions = c("noChange", "noChange")))
  expect_equal(splitAllIntoRuns(c(1,2,3,1,2,3), c(0,0,0,0,0,0)), 
               list(allRuns = list(c(1,2,3), c(1),c(2,3)), directions = c("dec", "acc", "dec")))
  expect_equal(splitAllIntoRuns(c(1,2,3,3,2,1), c(0,0,0,0,0,0)), 
               list(allRuns = list(c(1,2,3), c(3), c(2,1)), directions = c("dec", "noChange", "acc")))
  expect_equal(splitAllIntoRuns(c(1,2,3,8,3,2,1), c(0,0,0,1,0,0,0)), 
               list(allRuns = list(c(1,2,3), c(3,2,1)), directions = c("dec", "acc")))
  expect_equal(countForAll(c(1,2,4,3,2,1,2,3,4,4), c(0,0,0,0,0,0,0,0,0,0)), 
               list(decelerations = c(0,0,2), accelerations = c(0,0,1), noChange = c(1)))
  expect_equal(countForAll(c(1,2,4,3,2,1,2,3,4,5), c(0,0,0,0,0,0,0,0,0,0)), 
               list(decelerations = c(0,0,1,1), accelerations = c(0,0,1), noChange = c(NULL)))
  expect_equal(countForAll(c(1,2,4,3,2,1,2,3,4,5), c(0,0,0,1,0,0,0,0,0,0)), 
               list(decelerations = c(0,0,1,1), accelerations = c(0,1), noChange = c(NULL)))
  expect_equal(countForAll(c(4,3,2,1,2,3,4,5), c(0,0,0,0,0,0,0,0,0,0)), 
               list(decelerations = c(0,0,0,1), accelerations = c(0,0,0,1), noChange = c(NULL)))
})
