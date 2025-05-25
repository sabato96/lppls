# Unit tests for LPPLS estimation functions

test_that("prepare_lppls_data works correctly", {
  # Create test data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
  prices <- 100 + cumsum(rnorm(100, 0, 1))
  test_data <- data.frame(Date = dates, Close = prices)

  # Test basic functionality
  result <- prepare_lppls_data(test_data)

  expect_true(is.data.frame(result))
  expect_true(all(c("Date", "Close", "t") %in% names(result)))
  expect_equal(nrow(result), 100)
  expect_true(all(result$Close > 0))
})

test_that("validate_lppls_data catches errors", {
  # Test missing columns
  bad_data1 <- data.frame(x = 1:10, y = 1:10)
  result1 <- validate_lppls_data(bad_data1)
  expect_false(result1)
  expect_true(length(attr(result1, "issues")) > 0)

  # Test insufficient data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 10)
  prices <- rep(100, 10)
  bad_data2 <- data.frame(Date = dates, Close = prices, t = 1:10)
  result2 <- validate_lppls_data(bad_data2)
  expect_false(result2)

  # Test good data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 50)
  prices <- rep(100, 50)
  good_data <- data.frame(Date = dates, Close = prices, t = 1:50)
  result3 <- validate_lppls_data(good_data)
  expect_true(result3)
})

test_that("lppls_model function works", {
  # Create simple test data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 50)
  prices <- 100 * exp(cumsum(rnorm(50, 0.001, 0.01)))
  test_data <- data.frame(Date = dates, Close = prices, t = 1:50)

  # Test model function with reasonable parameters
  result <- lppls_model(test_data, tc = 55, m = 0.5, w = 5,
                        a = 5, b = -0.1, c1 = 0.01, c2 = 0.01)

  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(test_data))
  expect_true(all(is.finite(result)))
})

test_that("calculate_linear_parameters works", {
  # Create test data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 50)
  prices <- 100 * exp(cumsum(rnorm(50, 0.001, 0.01)))
  test_data <- data.frame(Date = dates, Close = prices, t = 1:50)

  # Calculate linear parameters
  result <- calculate_linear_parameters(test_data, tc = 55, m = 0.5, w = 5)

  expect_true(is.numeric(result))
  expect_equal(length(result), 4)  # Should return [A, B, C1, C2]
  expect_true(all(is.finite(result)))
})

test_that("lppls_objective_function returns valid output", {
  # Create test data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 50)
  prices <- 100 * exp(cumsum(rnorm(50, 0.001, 0.01)))
  test_data <- data.frame(Date = dates, Close = prices, t = 1:50)

  # Test objective function
  params <- c(55, 0.5, 5)  # tc, m, w
  result <- lppls_objective_function(params, test_data)

  expect_true(is.numeric(result))
  expect_equal(length(result), 1)
  expect_true(result >= 0)  # RSS should be non-negative
})

test_that("generate_synthetic_data creates valid output", {
  # Test bubble data generation
  bubble_data <- generate_synthetic_data("bubble", n = 100)
  expect_true(is.data.frame(bubble_data))
  expect_equal(nrow(bubble_data), 100)
  expect_true(all(c("Date", "Close") %in% names(bubble_data)))
  expect_true(all(bubble_data$Close > 0))

  # Test regular data generation
  regular_data <- generate_synthetic_data("sp500", n = 100)
  expect_true(is.data.frame(regular_data))
  expect_equal(nrow(regular_data), 100)
})
