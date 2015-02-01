test_that("financials are of the same length", {
  data(tidybalance)
  data(tidyincome)
  data(tidycash)
  expect_true(length(tidybalance$ticker) == length(tidyincome$ticker))
  expect_true(length(tidybalance$ticker) == length(tidycash$ticker))   
  expect_true(length(tidyincome$ticker) == length(tidycash$ticker))
})