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
#' @return List containing all analysis results
#' @export
#' @examples
#' \dontrun{
#' # Run basic example
#' results <- run_lppls_example()
#'
#' # Run example with bubble data
#' bubble_results <- run_lppls_example(generate_bubble = TRUE)
#'
#' # Access different components
#' print(bubble_results$single_fit)
#' head(bubble_results$confidence_indicators)
#' }
run_lppls_example <- function(generate_bubble = TRUE, n_observations = 500,
                              plot_results = TRUE, save_results = FALSE) {

  cat("Running LPPLS Analysis Example...\n\n")

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
  cat("Step 3: Estimating LPPLS model...\n")
  single_fit <- try({
    lppls_estimate(prepared_data, plot = FALSE)
  }, silent = TRUE)

  if (inherits(single_fit, "try-error")) {
    cat("Single model estimation failed, continuing with confidence analysis\n")
    single_fit <- NULL
  } else {
    cat("Single model estimation completed\n")
    cat("Critical time (tc):", single_fit$tc, "\n")
    cat("Power law exponent (m):", single_fit$m, "\n")
    cat("Log-periodic frequency (ω):", single_fit$w, "\n\n")
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
      benchmark = TRUE     # Show detailed timing
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
                     title = "LPPLS Model Fit Example",
                     show_critical_time = TRUE)
    }

    # Plot 3: Confidence indicators if available
    if (!is.null(confidence_results)) {
      # Check if we have confidence columns
      if ("P.S_EW" %in% names(confidence_results)) {
        plot_confidence_indicators(confidence_results, scale = "S",
                                   title = "LPPLS Confidence Indicators Example")
      }
    }

    cat("Visualizations created\n\n")
  }

  # Step 6: Summary
  cat("Step 6: Analysis Summary\n")
  cat("======================\n")
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
  }

  # Return results
  results <- list(
    data = prepared_data,
    single_fit = single_fit,
    confidence_indicators = confidence_results,
    parameters = list(
      generate_bubble = generate_bubble,
      n_observations = n_observations,
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
#' @return List with analysis results
#' @export
#' @examples
#' \dontrun{
#' # Analyze your data file
#' results <- quick_lppls_analysis("my_data.csv")
#'
#' # With custom column names
#' results <- quick_lppls_analysis("my_data.csv",
#'                                date_col = "date",
#'                                price_col = "price")
#' }
quick_lppls_analysis <- function(file_path, date_col = "Date", price_col = "Close",
                                 plot_results = TRUE) {

  cat("Quick LPPLS Analysis\n")
  cat("===================\n\n")

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
  cat("Estimating LPPLS model...\n")
  fit <- try(lppls_estimate(data, plot = plot_results), silent = TRUE)

  if (inherits(fit, "try-error")) {
    cat("Model estimation failed\n")
    fit <- NULL
  } else {
    cat("Model estimation successful\n")
  }

  # Quick confidence check (limited scope)
  cat("Calculating basic confidence indicators...\n")
  confidence <- try({
    compute_lppls_confidence(data, clusters = 2, window_size = 3)
  }, silent = TRUE)

  if (inherits(confidence, "try-error")) {
    confidence <- NULL
  }

  # Results summary
  cat("\nAnalysis Results:\n")
  if (!is.null(fit)) {
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

  return(list(
    data = data,
    fit = fit,
    confidence = confidence
  ))
}
