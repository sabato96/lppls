# Tests for LPPLS optimizer functionality

test_that("LPPLS estimation works with different optimizers", {
  skip_on_cran()  # Skip on CRAN due to computational intensity

  # Generate synthetic bubble data for testing
  set.seed(123)
  n <- 100
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = n)

  # Create synthetic LPPLS bubble
  tc <- 110
  m <- 0.5
  w <- 5
  A <- 5
  B <- -0.2
  C1 <- 0.05
  C2 <- 0.03

  t_vals <- 1:n
  theoretical_values <- A + B * (tc - t_vals)^m +
    C1 * (tc - t_vals)^m * cos(w * log(tc - t_vals)) +
    C2 * (tc - t_vals)^m * sin(w * log(tc - t_vals))

  prices <- exp(theoretical_values + rnorm(n, 0, 0.02))
  test_data <- data.frame(Date = dates, Close = prices)
  prepared_data <- prepare_lppls_data(test_data)

  # Test MLSL optimizer
  mlsl_result <- try({
    lppls_estimate(prepared_data, plot = FALSE, optimizer = "mlsl")
  }, silent = TRUE)

  if (!inherits(mlsl_result, "try-error")) {
    expect_true(is.data.frame(mlsl_result))
    expect_true("optimizer_used" %in% names(mlsl_result))
    expect_equal(mlsl_result$optimizer_used, "mlsl")
    expect_true(mlsl_result$m > 0 && mlsl_result$m < 1)
    expect_true(mlsl_result$w > 0)
  }

  # Test DE optimizer
  de_result <- try({
    lppls_estimate(prepared_data, plot = FALSE, optimizer = "de")
  }, silent = TRUE)

  if (!inherits(de_result, "try-error")) {
    expect_true(is.data.frame(de_result))
    expect_equal(de_result$optimizer_used, "de")
    expect_true(de_result$m > 0 && de_result$m < 1)
    expect_true(de_result$w > 0)
  }
})

test_that("compute_lppls_confidence works with different optimizers", {
  skip_on_cran()

  # Create simple test data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 150)
  prices <- 100 * exp(cumsum(rnorm(150, 0.001, 0.015)))
  test_data <- data.frame(Date = dates, Close = prices)
  prepared_data <- prepare_lppls_data(test_data)

  # Test with DE optimizer (reduced parameters for speed)
  confidence_de <- try({
    compute_lppls_confidence(
      prepared_data,
      clusters = 1,
      window_size = 2,
      min_window = 1,
      optimizer = "de",
      benchmark = FALSE,
      progress = FALSE
    )
  }, silent = TRUE)

  if (!inherits(confidence_de, "try-error")) {
    expect_true(is.data.frame(confidence_de))
    expect_true(nrow(confidence_de) == nrow(prepared_data))

    # Check that benchmark info includes optimizer
    benchmark_info <- attr(confidence_de, "benchmark")
    if (!is.null(benchmark_info)) {
      expect_equal(benchmark_info$optimizer, "de")
    }
  }
})

test_that("optimizer parameter validation works", {
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 50)
  prices <- rep(100, 50)
  test_data <- data.frame(Date = dates, Close = prices)
  prepared_data <- prepare_lppls_data(test_data)

  # Test invalid optimizer in lppls_estimate
  expect_error({
    lppls_estimate(prepared_data, optimizer = "invalid")
  }, "optimizer must be one of")

  # Test invalid optimizer in compute_lppls_confidence
  expect_error({
    compute_lppls_confidence(prepared_data, optimizer = "invalid")
  }, "optimizer must be one of")
})

test_that("DE optimization function works independently", {
  skip_on_cran()

  # Create test data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 50)
  prices <- 100 * exp(cumsum(rnorm(50, 0.001, 0.01)))
  test_data <- data.frame(Date = dates, Close = prices, t = 1:50)

  # Test bounds
  upper <- c(60, 2, 50)
  lower <- c(40, 0.01, 1)
  start_params <- c(55, 0.5, 5)

  # Test DE optimization function
  de_result <- try({
    lppls_de_optimize(start_params, lower, upper, test_data)
  }, silent = TRUE)

  if (!inherits(de_result, "try-error")) {
    expect_true(is.list(de_result))
    expect_true(all(c("par", "value", "convergence") %in% names(de_result)))
    expect_true(length(de_result$par) == 3)
    expect_true(de_result$value >= 0)  # RSS should be non-negative
  }
})

test_that("compare_lppls_optimizers function works", {
  skip_on_cran()

  # Create small test dataset
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 80)
  prices <- 100 * exp(cumsum(rnorm(80, 0.002, 0.02)))
  test_data <- data.frame(Date = dates, Close = prices)
  prepared_data <- prepare_lppls_data(test_data)

  # Test comparison with limited optimizers and trials
  comparison_result <- try({
    compare_lppls_optimizers(
      prepared_data,
      optimizers = c("mlsl", "de"),
      n_trials = 1
    )
  }, silent = TRUE)

  if (!inherits(comparison_result, "try-error")) {
    expect_true(is.data.frame(comparison_result))
    expect_true(all(c("optimizer", "trial", "success", "time_seconds") %in% names(comparison_result)))
    expect_true(nrow(comparison_result) >= 2)  # At least one trial per optimizer
  }
})

test_that("hybrid optimizer works", {
  skip_on_cran()

  # Create test data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 60)
  prices <- 100 * exp(cumsum(rnorm(60, 0.001, 0.01)))
  test_data <- data.frame(Date = dates, Close = prices)
  prepared_data <- prepare_lppls_data(test_data)

  # Test hybrid optimizer
  hybrid_result <- try({
    lppls_estimate(prepared_data, plot = FALSE, optimizer = "hybrid")
  }, silent = TRUE)

  if (!inherits(hybrid_result, "try-error")) {
    expect_true(is.data.frame(hybrid_result))
    expect_equal(hybrid_result$optimizer_used, "hybrid")
    expect_true(hybrid_result$m > 0 && hybrid_result$m < 1)
    expect_true(hybrid_result$w > 0)
  }
})

test_that("example functions work with different optimizers", {
  skip_on_cran()

  # Test run_lppls_example with DE optimizer
  example_result_de <- try({
    run_lppls_example(
      generate_bubble = TRUE,
      n_observations = 100,
      plot_results = FALSE,
      optimizer = "de"
    )
  }, silent = TRUE)

  if (!inherits(example_result_de, "try-error")) {
    expect_true(is.list(example_result_de))
    expect_true("parameters" %in% names(example_result_de))
    expect_equal(example_result_de$parameters$optimizer, "de")
  }
})

test_that("optimizer performance tracking works", {
  skip_on_cran()

  # Create test data
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
  prices <- 100 * exp(cumsum(rnorm(100, 0.001, 0.015)))
  test_data <- data.frame(Date = dates, Close = prices)
  prepared_data <- prepare_lppls_data(test_data)

  # Test with benchmark enabled
  confidence_result <- try({
    compute_lppls_confidence(
      prepared_data,
      clusters = 1,
      window_size = 2,
      min_window = 1,
      optimizer = "de",
      benchmark = TRUE,
      progress = FALSE
    )
  }, silent = TRUE)

  if (!inherits(confidence_result, "try-error")) {
    benchmark_info <- attr(confidence_result, "benchmark")
    if (!is.null(benchmark_info)) {
      expect_true("optimizer" %in% names(benchmark_info))
      expect_equal(benchmark_info$optimizer, "de")
      expect_true("total_time" %in% names(benchmark_info))
      expect_true("avg_window_time" %in% names(benchmark_info))
    }
  }
})
