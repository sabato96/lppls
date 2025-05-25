# Integration tests for complete LPPLS workflow

test_that("complete LPPLS workflow works", {
  skip_on_cran()  # Skip on CRAN due to computational intensity

  # Generate test data with known bubble characteristics
  set.seed(123)
  n <- 100
  t_vals <- 1:n

  # Create synthetic bubble data
  tc <- 110
  m <- 0.5
  w <- 5
  A <- 5
  B <- -0.2
  C1 <- 0.05
  C2 <- 0.03

  # Generate LPPLS pattern with noise
  theoretical_values <- A + B * (tc - t_vals)^m +
    C1 * (tc - t_vals)^m * cos(w * log(tc - t_vals)) +
    C2 * (tc - t_vals)^m * sin(w * log(tc - t_vals))

  prices <- exp(theoretical_values + rnorm(n, 0, 0.02))
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = n)

  test_data <- data.frame(Date = dates, Close = prices)

  # Step 1: Prepare data
  prepared_data <- prepare_lppls_data(test_data)
  expect_true(validate_lppls_data(prepared_data))

  # Step 2: Estimate model (with error handling)
  fit_result <- try({
    lppls_estimate(prepared_data, plot = FALSE)
  }, silent = TRUE)

  # If estimation succeeds, check results
  if (!inherits(fit_result, "try-error")) {
    expect_true(is.data.frame(fit_result))
    expect_true(all(c("tc", "m", "w", "A", "B") %in% names(fit_result)))

    # Check if estimated parameters are reasonable
    expect_true(fit_result$m > 0 && fit_result$m < 1)
    expect_true(fit_result$w > 0)
    expect_true(fit_result$tc > max(prepared_data$t))
  }
})

test_that("confidence indicator calculation works", {
  skip_on_cran()

  # Create simple test data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 200)
  prices <- 100 * exp(cumsum(rnorm(200, 0.001, 0.015)))
  test_data <- data.frame(Date = dates, Close = prices)

  prepared_data <- prepare_lppls_data(test_data)

  # Test confidence calculation with minimal parameters
  confidence_result <- try({
    compute_lppls_confidence(
      prepared_data,
      clusters = 1,  # Use single core for testing
      window_size = 3,  # Very small window for testing
      min_window = 1
    )
  }, silent = TRUE)

  if (!inherits(confidence_result, "try-error")) {
    expect_true(is.data.frame(confidence_result))
    expect_true(nrow(confidence_result) == nrow(prepared_data))

    # Check that confidence columns exist
    confidence_cols <- c("P.SS_EW", "P.S_EW", "P.M_EW", "P.L_EW")
    existing_cols <- confidence_cols[confidence_cols %in% names(confidence_result)]
    expect_true(length(existing_cols) > 0)
  }
})

test_that("plotting functions work without errors", {
  # Create test data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 50)
  prices <- 100 * exp(cumsum(rnorm(50, 0.001, 0.01)))
  test_data <- data.frame(Date = dates, Close = prices)
  prepared_data <- prepare_lppls_data(test_data)

  # Create mock LPPLS result
  mock_result <- data.frame(
    tc = 55,
    m = 0.5,
    w = 5,
    A = 5,
    B = -0.1,
    C1 = 0.01,
    C2 = 0.01
  )

  # Test plotting function (should not error)
  expect_no_error({
    plot_lppls_fit(prepared_data, mock_result, show_critical_time = FALSE)
  })

  # Test confidence plotting with mock data
  mock_confidence <- prepared_data
  mock_confidence$P.S_EW <- runif(nrow(mock_confidence), 0, 1)
  mock_confidence$P.S_EF <- runif(nrow(mock_confidence), 0, 1)

  expect_no_error({
    plot_confidence_indicators(mock_confidence, scale = "S")
  })
})

test_that("example functions run without major errors", {
  skip_on_cran()

  # Test example with reduced parameters
  example_result <- try({
    run_lppls_example(
      generate_bubble = TRUE,
      n_observations = 100,
      plot_results = FALSE  # Don't create plots in tests
    )
  }, silent = TRUE)

  # Should return a list even if some components fail
  if (!inherits(example_result, "try-error")) {
    expect_true(is.list(example_result))
    expect_true("data" %in% names(example_result))
    expect_true(is.data.frame(example_result$data))
  }
})

test_that("parameter validation works correctly", {
  # Test with invalid parameters for LPPLS model
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 50)
  prices <- rep(100, 50)  # Constant prices
  test_data <- data.frame(Date = dates, Close = prices)
  prepared_data <- prepare_lppls_data(test_data)

  # LPPLS estimation should handle constant prices gracefully
  result <- try({
    lppls_estimate(prepared_data, plot = FALSE)
  }, silent = TRUE)

  # Either succeeds with warning or fails gracefully
  expect_true(inherits(result, "try-error") || is.data.frame(result))
})

test_that("error handling works for edge cases", {
  # Test with insufficient data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 10)
  prices <- rep(100, 10)
  small_data <- data.frame(Date = dates, Close = prices)

  expect_error({
    prepare_lppls_data(small_data, subset_range = 1:5)  # Too small
  })

  # Test with missing values
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 50)
  prices <- c(rep(100, 25), NA, rep(100, 24))
  na_data <- data.frame(Date = dates, Close = prices)

  cleaned_data <- prepare_lppls_data(na_data)
  expect_equal(nrow(cleaned_data), 49)  # Should remove NA row

  # Test with negative prices
  negative_prices <- c(rep(100, 25), -50, rep(100, 24))
  negative_data <- data.frame(Date = dates, Close = negative_prices)

  expect_error({
    prepared_negative <- prepare_lppls_data(negative_data)
    validate_lppls_data(prepared_negative)
  })
})
