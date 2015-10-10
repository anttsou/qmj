context("Test Tidy Functions")

load(system.file("extdata/true_balancesheets.RData", package = "qmj"))
load(system.file("extdata/true_incomestatements.RData", package = "qmj"))
load(system.file("extdata/true_cashflows.RData", package = "qmj"))
load(system.file("extdata/true_info.RData", package = "qmj"))
load(system.file("extdata/raw_financials.RData", package = "qmj"))

test_that("balance sheets have expected output", { 
  expect_equal(tidy_balancesheets(raw_financials[[3]]), true_balancesheets)
})

test_that("income statements have expected output", {
  expect_equal(tidy_incomestatements(raw_financials[[2]]), true_incomestatements)
})

test_that("cash flows have expected output", {
  expect_equal(tidy_cashflows(raw_financials[[1]]), true_cashflows)
})

test_that("all financial statements have merge correctly", {
  expect_equal(tidyinfo(raw_financials), true_info)
})