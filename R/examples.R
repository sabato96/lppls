#' Run LPPLS Analysis Example
#'
#' This function demonstrates a complete LPPLS analysis workflow using synthetic data.
#' It shows how to prepare data, estimate models, calculate confidence indicators,
#' and create visualizations.
#'
#' @param generate_bubble Logical, whether to generate data with bubble characteristics
#' @param n_observations Number of observations to generate (default: 500)
#' @param plot_results Logical, whether to create plots (default: TRUE)
#' @param save_results Logical, whether to save results to files (default: FALSE)
#' @param optimizer Character, optimization method to use ("mlsl", "de", or "hybrid")
#' @return List containing all analysis results
#' @export
#' @examples
#' \dontrun{
#' # Run basic example with MLSL
#' results <- run_lppls_example()
#'
#' # Run example with DE optimizer and bubble data
#' bubble_results <- run_lppls_example(generate_bubble = TRUE, optimizer = "de")
#'
#' # Run example with hybrid optimizer
#' hybrid_results <- run_lppls_example(optimizer = "hybrid")
#'
#' # Access different components
#' print(bubble_results$single_fit)
#' head(bubble_results$confidence_indicators)
#' }
run_lppls_example <- function(generate_bubble = TRUE, n_observations = 500,
                              plot_results = TRUE, save_results = FALSE,
                              optimizer = "mlsl") {

  # Validate optimizer parameter
  valid_optimizers <- c("mlsl", "de", "hybrid")
  if (!optimizer %in% valid_optimizers) {
    stop("optimizer must be one of: ", paste(valid_optimizers, collapse = ", "))
  }

  cat("Running LPPLS Analysis Example...\n")
  cat("Optimizer:", toupper(optimizer), "\n\n")

  # Step 1: Generate or load data
  cat("Step 1: Generating synthetic data...\n")
  if (generate_bubble) {
    data <- generate_synthetic_data("bubble", n = n_observations)
    cat("Generated data with bubble characteristics\n")
  } else {
    data <- generate_synthetic_data("sp500", n = n_observations)
    cat("Generated regular financial data\n")
  }

  # Step 2: Prepare data for LPPLS analysis
  cat("Step 2: Preparing data for analysis...\n")
  prepared_data <- prepare_lppls_data(data)

  # Validate data
  is_valid <- validate_lppls_data(prepared_data)
  if (!is_valid) {
    stop("Data validation failed: ", paste(attr(is_valid, "issues"), collapse = "; "))
  }
  cat("Data validation passed\n")

  # Step 3: Estimate single LPPLS model
  cat("Step 3: Estimating LPPLS model using", toupper(optimizer), "optimizer...\n")
  single_fit <- try({
    lppls_estimate(prepared_data, plot = FALSE, optimizer = optimizer)
  }, silent = TRUE)

  if (inherits(single_fit, "try-error")) {
    cat("Single model estimation failed, continuing with confidence analysis\n")
    single_fit <- NULL
  } else {
    cat("Single model estimation completed\n")
    cat("Critical time (tc):", single_fit$tc, "\n")
    cat("Power law exponent (m):", single_fit$m, "\n")
    cat("Log-periodic frequency (ω):", single_fit$w, "\n")
    cat("Optimizer used:", single_fit$optimizer_used, "\n\n")
  }

  # Step 4: Calculate confidence indicators with progress tracking
  cat("Step 4: Calculating confidence indicators...\n")
  cat("Note: Using reduced parameters for faster computation in example\n")

  confidence_results <- try({
    compute_lppls_confidence(
      prepared_data,
      clusters = 2,        # Reduced for example
      window_size = 5,     # Reduced for example
      min_window = 1,
      save = save_results,
      progress = TRUE,     # Show progress bar
      benchmark = TRUE,    # Show detailed timing
      optimizer = optimizer # Use specified optimizer
    )
  }, silent = TRUE)

  if (inherits(confidence_results, "try-error")) {
    cat("Confidence indicator calculation failed\n")
    confidence_results <- NULL
  } else {
    cat("Confidence indicator calculation completed\n\n")
  }

  # Step 5: Create visualizations
  if (plot_results) {
    cat("Step 5: Creating visualizations...\n")

    # Plot 1: Original price data
    plot(prepared_data$Date, prepared_data$Close,
         type = "l", col = "blue", lwd = 2,
         main = "Price Time Series",
         xlab = "Date", ylab = "Price")

    # Plot 2: Single LPPLS fit if available
    if (!is.null(single_fit)) {
      plot_lppls_fit(prepared_data, single_fit,
                     title = paste("LPPLS Model Fit -", toupper(optimizer), "Optimizer"),
                     show_critical_time = TRUE)
    }

    # Plot 3: Confidence indicators if available
    if (!is.null(confidence_results)) {
      # Check if we have confidence columns
      if ("P.S_EW" %in% names(confidence_results)) {
        plot_confidence_indicators(confidence_results, scale = "S",
                                   title = paste("LPPLS Confidence Indicators -", toupper(optimizer)))
      }
    }

    cat("Visualizations created\n\n")
  }

  # Step 6: Summary
  cat("Step 6: Analysis Summary\n")
  cat("======================\n")
  cat("Optimizer used:", toupper(optimizer), "\n")
  cat("Data points analyzed:", nrow(prepared_data), "\n")
  cat("Date range:", format(min(prepared_data$Date)), "to", format(max(prepared_data$Date)), "\n")

  if (!is.null(single_fit)) {
    cat("LPPLS model successfully fitted\n")
    cat("Estimated critical time:", format(as.Date("1970-01-01") + (single_fit$tc - 1970) * 365.25), "\n")
    cat("Model quality indicators:\n")
    cat("  - Relative error:", round(single_fit$rel_err, 4), "\n")
    cat("  - Oscillations count:", round(single_fit$oscillations, 2), "\n")
    cat("  - Damping ratio:", round(single_fit$damping, 2), "\n")
  }

  if (!is.null(confidence_results)) {
    cat("Confidence indicators calculated successfully\n")

    # Show optimizer performance comparison if benchmark data available
    benchmark_info <- attr(confidence_results, "benchmark")
    if (!is.null(benchmark_info)) {
      cat("Performance metrics:\n")
      cat("  - Total time:", round(benchmark_info$total_time, 2), "seconds\n")
      cat("  - Average window time:", round(benchmark_info$avg_window_time, 2), "seconds\n")
      cat("  - Throughput:", round(benchmark_info$total_windows / (benchmark_info$total_time / 60), 2), "windows/minute\n")
    }
  }

  # Return results
  results <- list(
    data = prepared_data,
    single_fit = single_fit,
    confidence_indicators = confidence_results,
    parameters = list(
      generate_bubble = generate_bubble,
      n_observations = n_observations,
      optimizer = optimizer,
      analysis_date = Sys.Date()
    )
  )

  cat("\nExample completed successfully!\n")
  cat("Access results using: results$data, results$single_fit, results$confidence_indicators\n")

  return(invisible(results))
}

#' Quick LPPLS Analysis
#'
#' Simplified function for quick LPPLS analysis of user data
#'
#' @param file_path Path to CSV file with financial data
#' @param date_col Name of date column (default: "Date")
#' @param price_col Name of price column (default: "Close")
#' @param plot_results Logical, whether to create plots (default: TRUE)
#' @param optimizer Character, optimization method to use ("mlsl", "de", or "hybrid")
#' @return List with analysis results
#' @export
#' @examples
#' \dontrun{
#' # Analyze your data file with default MLSL
#' results <- quick_lppls_analysis("my_data.csv")
#'
#' # Use DE optimizer for faster analysis
#' results <- quick_lppls_analysis("my_data.csv", optimizer = "de")
#'
#' # Use hybrid optimizer for best quality
#' results <- quick_lppls_analysis("my_data.csv", optimizer = "hybrid")
#'
#' # With custom column names
#' results <- quick_lppls_analysis("my_data.csv",
#'                                date_col = "date",
#'                                price_col = "price",
#'                                optimizer = "de")
#' }
quick_lppls_analysis <- function(file_path, date_col = "Date", price_col = "Close",
                                 plot_results = TRUE, optimizer = "mlsl") {

  # Validate optimizer parameter
  valid_optimizers <- c("mlsl", "de", "hybrid")
  if (!optimizer %in% valid_optimizers) {
    stop("optimizer must be one of: ", paste(valid_optimizers, collapse = ", "))
  }

  cat("Quick LPPLS Analysis\n")
  cat("===================\n")
  cat("Optimizer:", toupper(optimizer), "\n\n")

  # Load and prepare data
  cat("Loading data from:", file_path, "\n")
  raw_data <- read.csv(file_path, stringsAsFactors = FALSE)

  # Rename columns if necessary
  if (date_col != "Date") {
    names(raw_data)[names(raw_data) == date_col] <- "Date"
  }
  if (price_col != "Close") {
    names(raw_data)[names(raw_data) == price_col] <- "Close"
  }

  # Prepare data
  data <- prepare_lppls_data(raw_data)
  cat("Prepared", nrow(data), "observations\n")

  # Quick validation
  if (!validate_lppls_data(data)) {
    stop("Data validation failed")
  }

  # Estimate model
  cat("Estimating LPPLS model using", toupper(optimizer), "optimizer...\n")
  fit <- try(lppls_estimate(data, plot = plot_results, optimizer = optimizer), silent = TRUE)

  if (inherits(fit, "try-error")) {
    cat("Model estimation failed\n")
    fit <- NULL
  } else {
    cat("Model estimation successful\n")
  }

  # Quick confidence check (limited scope)
  cat("Calculating basic confidence indicators...\n")
  confidence <- try({
    compute_lppls_confidence(data, clusters = 2, window_size = 3, optimizer = optimizer)
  }, silent = TRUE)

  if (inherits(confidence, "try-error")) {
    confidence <- NULL
  }

  # Results summary
  cat("\nAnalysis Results:\n")
  if (!is.null(fit)) {
    cat("Optimizer used:", fit$optimizer_used, "\n")
    cat("Critical time estimated at:", format(as.Date("1970-01-01") + (fit$tc - 1970) * 365.25), "\n")
    cat("Days until critical time:", fit$days_to_tc, "\n")
    cat("Power law exponent (m):", round(fit$m, 3), "\n")
    cat("Log-periodic frequency (ω):", round(fit$w, 3), "\n")

    # Interpretation
    if (fit$B < 0) {
      cat("Pattern suggests: POSITIVE bubble (price acceleration upward)\n")
    } else {
      cat("Pattern suggests: NEGATIVE bubble (price acceleration downward)\n")
    }

    if (fit$rel_err < 0.05) {
      cat("Model quality: GOOD (low relative error)\n")
    } else {
      cat("Model quality: MODERATE (higher relative error)\n")
    }
  }

  # Show performance comparison hint
  if (!is.null(confidence)) {
    benchmark_info <- attr(confidence, "benchmark")
    if (!is.null(benchmark_info)) {
      cat("\nPerformance Note:\n")
      cat("Try different optimizers for speed comparison:\n")
      cat("  - 'mlsl': Most robust (default)\n")
      cat("  - 'de': Typically 2-5x faster\n")
      cat("  - 'hybrid': Best quality, moderate speed\n")
    }
  }

  return(list(
    data = data,
    fit = fit,
    confidence = confidence
  ))
}

#' Compare LPPLS Optimizers
#'
#' Benchmark different optimizers on the same dataset
#'
#' @param data Prepared LPPLS data frame
#' @param optimizers Vector of optimizers to compare (default: c("mlsl", "de", "hybrid"))
#' @param n_trials Number of trials per optimizer (default: 3)
#' @return Data frame with comparison results
#' @export
#' @examples
#' \dontrun{
#' # Generate test data
#' data <- generate_synthetic_data("bubble", n = 200)
#' prepared_data <- prepare_lppls_data(data)
#'
#' # Compare optimizers
#' comparison <- compare_lppls_optimizers(prepared_data)
#' print(comparison)
#' }
compare_lppls_optimizers <- function(data, optimizers = c("mlsl", "de", "hybrid"), n_trials = 3) {

  cat("Comparing LPPLS Optimizers\n")
  cat("==========================\n")
  cat("Data points:", nrow(data), "\n")
  cat("Optimizers:", paste(optimizers, collapse = ", "), "\n")
  cat("Trials per optimizer:", n_trials, "\n\n")

  results <- data.frame()

  for (optimizer in optimizers) {
    cat("Testing", toupper(optimizer), "optimizer...\n")

    for (trial in 1:n_trials) {
      cat("  Trial", trial, "of", n_trials, "...")

      start_time <- Sys.time()

      fit_result <- try({
        lppls_estimate(data, plot = FALSE, optimizer = optimizer)
      }, silent = TRUE)

      end_time <- Sys.time()
      elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

      if (!inherits(fit_result, "try-error")) {
        trial_result <- data.frame(
          optimizer = optimizer,
          trial = trial,
          success = TRUE,
          time_seconds = elapsed_time,
          objective_value = fit_result$resid_sum_sq,
          tc = fit_result$tc,
          m = fit_result$m,
          w = fit_result$w,
          rel_err = fit_result$rel_err,
          oscillations = fit_result$oscillations,
          damping = fit_result$damping,
          stringsAsFactors = FALSE
        )
        cat(" Success (", round(elapsed_time, 2), "s)\n")
      } else {
        trial_result <- data.frame(
          optimizer = optimizer,
          trial = trial,
          success = FALSE,
          time_seconds = elapsed_time,
          objective_value = NA,
          tc = NA,
          m = NA,
          w = NA,
          rel_err = NA,
          oscillations = NA,
          damping = NA,
          stringsAsFactors = FALSE
        )
        cat(" Failed\n")
      }

      results <- rbind(results, trial_result)
    }
    cat("\n")
  }

  # Summary statistics
  cat("Summary Statistics:\n")
  cat("==================\n")

  for (optimizer in optimizers) {
    opt_results <- results[results$optimizer == optimizer, ]
    success_rate <- mean(opt_results$success) * 100
    avg_time <- mean(opt_results$time_seconds, na.rm = TRUE)

    if (any(opt_results$success)) {
      successful_results <- opt_results[opt_results$success, ]
      avg_objective <- mean(successful_results$objective_value, na.rm = TRUE)
      avg_rel_err <- mean(successful_results$rel_err, na.rm = TRUE)

      cat(sprintf("%s: Success rate: %.0f%%, Avg time: %.2fs, Avg RSS: %.4f, Avg rel_err: %.4f\n",
                  toupper(optimizer), success_rate, avg_time, avg_objective, avg_rel_err))
    } else {
      cat(sprintf("%s: Success rate: %.0f%%, Avg time: %.2fs\n",
                  toupper(optimizer), success_rate, avg_time))
    }
  }

  return(results)
}
