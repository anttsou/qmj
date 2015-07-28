context("Test Tidy Functions")

companies <- qmjdata::companies[1:2,]
load(system.file("extdata/true_balancesheets.RData", package = "qmj"))
load(system.file("extdata/true_incomestatements.RData", package = "qmj"))
load(system.file("extdata/true_cashflows.RData", package = "qmj"))
raw_data <- get_info(companies)

test_that("balance sheets have expected output", { 
  expect_equal(tidy_balancesheets(raw_data[[3]]), true_balancesheets)
})

test_that("income statements have expected output", {
  expect_equal(tidy_incomestatements(raw_data[[2]]), true_incomestatements)
})

test_that("cash flows have expected output", {
  expect_equal(tidy_cashflows(raw_data[[1]]), true_cashflows)
})