#' Compute Daily DS LPPLS Confidence Indicators Using Original Methodology
#'
#' This function implements the exact DS LPPLS Confidence methodology from the thesis,
#' calculating confidence indicators for recent trading days using multiple window sizes
#' and proper filtering criteria for each time scale.
#'
#' @param data A data frame with columns Date, Close, and t (decimal time)
#' @param recent_days Number of recent trading days to calculate indicators for (default: 30)
#' @param window_sizes Vector of window sizes to use (default: creates ~980 windows like original)
#' @param clusters Number of parallel clusters to use (default: 4)
#' @param progress Logical, whether to show progress tracking (default: TRUE)
#' @param benchmark Logical, whether to show detailed timing information (default: TRUE)
#' @param save Logical, whether to save intermediate results (default: FALSE)
#' @param folder Output folder path (default: "./ds_lppls_results/")
#' @return Data frame with DS LPPLS Confidence indicators for each recent day
#' @export
#' @examples
#' \dontrun{
#' # Calculate DS LPPLS Confidence for last 30 days (original methodology)
#' daily_confidence <- compute_ds_lppls_confidence(data, recent_days = 30)
#'
#' # Calculate for last week only
#' daily_confidence <- compute_ds_lppls_confidence(data, recent_days = 5)
#' }
compute_ds_lppls_confidence <- function(data, recent_days = 30,
                                        window_sizes = NULL,
                                        clusters = 4, progress = TRUE,
                                        benchmark = TRUE, save = FALSE,
                                        folder = "./ds_lppls_results/") {

  # Initialize timer if benchmarking is enabled
  if (benchmark) {
    timer <- create_timer("DS LPPLS Confidence Calculation")
  }

  # Validate inputs
  if (!all(c("Date", "Close", "t") %in% names(data))) {
    stop("Data must contain columns: Date, Close, t")
  }

  # Sort data by date
  data <- data[order(data$Date), ]
  total_obs <- nrow(data)

  # Create window sizes (like original: from 1460 to 40 days)
  if (is.null(window_sizes)) {
    window_sizes <- seq(from = 1460, to = 40, by = -1)
    # Filter to available data
    window_sizes <- window_sizes[window_sizes <= (total_obs - recent_days)]
  }

  n_windows <- length(window_sizes)

  if (benchmark) {
    timer$checkpoint("Setup",
                     sprintf("Analyzing %d recent days using %d window sizes (like original ~980 windows)",
                             recent_days, n_windows))
  }

  # Initialize results storage for each day
  daily_results <- list()

  # Setup parallel processing
  cl <- parallel::makeForkCluster(clusters)
  doParallel::registerDoParallel(cl)

  if (benchmark) {
    timer$checkpoint("Parallel Setup", paste("Initialized", clusters, "worker processes"))
  }

  # Initialize progress tracking
  if (progress) {
    cat("Computing DS LPPLS Confidence indicators (original methodology)...\n")
    cat(sprintf("Analyzing %d recent days with %d windows each\n", recent_days, n_windows))
    cat(paste(rep("=", 70), collapse = ""), "\n")

    if (requireNamespace("progress", quietly = TRUE)) {
      pb <- progress::progress_bar$new(
        format = "[:bar] :percent | Day :current/:total | Elapsed: :elapsed | ETA: :eta",
        total = recent_days,
        clear = FALSE,
        width = 70
      )
      use_fancy_progress <- TRUE
    } else {
      pb <- NULL
      use_fancy_progress <- FALSE
    }
  }

  # Process each recent day
  day_times <- numeric(recent_days)

  for (day_idx in 1:recent_days) {

    day_start_time <- Sys.time()

    # Current analysis day (counting backwards from end)
    current_day_idx <- total_obs - recent_days + day_idx
    current_date <- data$Date[current_day_idx]

    if (progress && !use_fancy_progress) {
      cat(sprintf("Processing day %d/%d: %s\n", day_idx, recent_days, current_date))
    }

    # Initialize confidence indicators for this day
    day_confidence <- initialize_ds_confidence_structure(current_date)

    # For this day, run LPPLS estimation on all window sizes
    # This replicates the original approach: for each day, estimate LPPLS on ~980 windows

    window_results <- foreach(w_idx = 1:n_windows,
                              .combine = rbind,
                              .packages = c("nloptr", "tseries", "dplyr"),
                              .errorhandling = "remove") %dopar% {

                                window_size <- window_sizes[w_idx]

                                # Define window: ending at current day, going back window_size days
                                window_start_idx <- max(1, current_day_idx - window_size + 1)
                                window_end_idx <- current_day_idx

                                if (window_end_idx - window_start_idx + 1 < 40) {
                                  return(NULL)  # Skip if window too small
                                }

                                window_data <- data[window_start_idx:window_end_idx, ]

                                # Attempt LPPLS estimation with retry mechanism
                                result <- NULL
                                attempts <- 0
                                max_attempts <- 3

                                while (is.null(result) && attempts < max_attempts) {
                                  attempts <- attempts + 1
                                  try({
                                    result <- lppls_estimate(window_data)
                                  }, silent = TRUE)
                                }

                                if (!is.null(result)) {
                                  result$window_size <- window_size
                                  result$window_idx <- w_idx
                                }

                                return(result)
                              }

    # Apply DS LPPLS filtering and calculate confidence indicators
    if (!is.null(window_results) && nrow(window_results) > 0) {
      day_confidence <- calculate_ds_confidence_indicators(window_results, current_date)
    }

    # Store results for this day
    daily_results[[day_idx]] <- day_confidence

    # Save intermediate results if requested
    if (save) {
      if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
      filename <- file.path(folder, paste0("ds_confidence_", format(current_date, "%Y%m%d"), ".csv"))
      write.csv(day_confidence, filename, row.names = FALSE)
    }

    # Update progress
    day_end_time <- Sys.time()
    day_times[day_idx] <- as.numeric(difftime(day_end_time, day_start_time, units = "secs"))

    if (progress) {
      if (use_fancy_progress && !is.null(pb)) {
        pb$tick()
      } else {
        cat(sprintf("  Completed in %.1fs | Windows processed: %d\n",
                    day_times[day_idx], ifelse(is.null(window_results), 0, nrow(window_results))))
      }
    }

    # Memory management
    if (day_idx %% 5 == 0) {
      gc(verbose = FALSE)
    }
  }

  # Cleanup parallel processing
  try({
    parallel::stopCluster(cl)
  }, silent = TRUE)

  if (benchmark) {
    timer$checkpoint("Daily Processing",
                     sprintf("Processed %d days (avg: %.2fs per day)",
                             recent_days, mean(day_times, na.rm = TRUE)))
  }

  # Combine all daily results
  final_results <- do.call(rbind, daily_results)

  if (progress) {
    cat("\n", paste(rep("=", 70), collapse = ""), "\n")
    cat("✓ DS LPPLS Confidence calculation completed!\n")
  }

  if (benchmark) {
    total_time <- timer$finish("DS LPPLS Confidence Calculation")

    # Performance statistics
    cat("\nPerformance Statistics:\n")
    cat(sprintf("  Days processed: %d\n", recent_days))
    cat(sprintf("  Windows per day: %d\n", n_windows))
    cat(sprintf("  Total LPPLS estimations: %d\n", recent_days * n_windows))
    cat(sprintf("  Average time per day: %.2fs\n", mean(day_times, na.rm = TRUE)))
    cat(sprintf("  Parallel workers used: %d\n", clusters))
    cat(sprintf("  Throughput: %.2f days/minute\n", recent_days / (total_time / 60)))
  }

  return(final_results)
}

#' Initialize DS Confidence Structure for a Single Day
#' @keywords internal
initialize_ds_confidence_structure <- function(current_date) {
  data.frame(
    Date = current_date,

    # Early Warning Indicators
    P.SS_EW = 0, P.S_EW = 0, P.M_EW = 0, P.L_EW = 0,
    N.SS_EW = 0, N.S_EW = 0, N.M_EW = 0, N.L_EW = 0,

    # End Flag Indicators
    P.SS_EF = 0, P.S_EF = 0, P.M_EF = 0, P.L_EF = 0,
    N.SS_EF = 0, N.S_EF = 0, N.M_EF = 0, N.L_EF = 0,

    # Critical Time Medians
    P.SS_tc = NA, P.S_tc = NA, P.M_tc = NA, P.L_tc = NA,
    N.SS_tc = NA, N.S_tc = NA, N.M_tc = NA, N.L_tc = NA,

    stringsAsFactors = FALSE
  )
}

#' Calculate DS LPPLS Confidence Indicators Following Original Methodology
#' @keywords internal
calculate_ds_confidence_indicators <- function(window_results, current_date) {

  # Apply initial quality filtering (from Table 3.1 in thesis)
  filtered_results <- window_results[
    !is.na(window_results$dt) &
      window_results$dt >= 40 & window_results$dt <= 1460 &
      !is.na(window_results$m) & window_results$m >= 0.01 & window_results$m <= 1.2 &
      !is.na(window_results$w) & window_results$w >= 2 & window_results$w <= 25 &
      !is.na(window_results$oscillations) & window_results$oscillations >= 2.5 &
      !is.na(window_results$rel_err) & window_results$rel_err >= 0 & window_results$rel_err <= 0.2 &
      !is.na(window_results$test_resid) & window_results$test_resid < 0.463,
  ]

  if (nrow(filtered_results) == 0) {
    return(initialize_ds_confidence_structure(current_date))
  }

  # Time scale definitions (from thesis)
  scales <- list(
    SS = c(40, 183),    # Super-Short
    S = c(40, 365),     # Short
    M = c(365, 730),    # Medium
    L = c(730, 1460)    # Long
  )

  # Initialize result structure
  confidence_result <- initialize_ds_confidence_structure(current_date)

  # Calculate confidence for each time scale
  for (scale_name in names(scales)) {
    scale_range <- scales[[scale_name]]

    # Filter results for this time scale
    scale_results <- filtered_results[
      filtered_results$dt >= scale_range[1] & filtered_results$dt <= scale_range[2],
    ]

    if (nrow(scale_results) == 0) next

    # Calculate for Early Warning filters
    ew_positive <- apply_ds_filters(scale_results, "positive", "early_warning")
    ew_negative <- apply_ds_filters(scale_results, "negative", "early_warning")
    ew_total <- nrow(scale_results[scale_results$B < 0 | scale_results$B > 0, ])

    # Calculate for End Flag filters
    ef_positive <- apply_ds_filters(scale_results, "positive", "end_flag")
    ef_negative <- apply_ds_filters(scale_results, "negative", "end_flag")
    ef_total <- nrow(scale_results[scale_results$B < 0 | scale_results$B > 0, ])

    # Update confidence indicators using DS LPPLS formula: (npos - nneg) / ntot
    if (ew_total > 0) {
      confidence_result[[paste0("P.", scale_name, "_EW")]] <- round(ew_positive / ew_total, 5)
      confidence_result[[paste0("N.", scale_name, "_EW")]] <- round(ew_negative / ew_total, 5)
    }

    if (ef_total > 0) {
      confidence_result[[paste0("P.", scale_name, "_EF")]] <- round(ef_positive / ef_total, 5)
      confidence_result[[paste0("N.", scale_name, "_EF")]] <- round(ef_negative / ef_total, 5)
    }

    # Calculate median critical times
    pos_bubbles <- scale_results[scale_results$B < 0 & !is.na(scale_results$tc), ]
    neg_bubbles <- scale_results[scale_results$B > 0 & !is.na(scale_results$tc), ]

    if (nrow(pos_bubbles) > 0) {
      confidence_result[[paste0("P.", scale_name, "_tc")]] <- median(pos_bubbles$tc, na.rm = TRUE)
    }
    if (nrow(neg_bubbles) > 0) {
      confidence_result[[paste0("N.", scale_name, "_tc")]] <- median(neg_bubbles$tc, na.rm = TRUE)
    }
  }

  return(confidence_result)
}

#' Apply DS LPPLS Filtering Criteria (Table 3.1 from thesis)
#' @keywords internal
apply_ds_filters <- function(scale_results, bubble_type, filter_type) {

  # Define filtering parameters based on thesis Table 3.1
  if (filter_type == "early_warning") {
    m_range <- c(0.01, 1.2)
    damping_min <- 0.8
    rel_err_max <- 0.05
  } else {  # end_flag
    m_range <- c(0.01, 0.99)
    damping_min <- 1.0
    rel_err_max <- 0.2
  }

  # Apply additional filters specific to Early Warning vs End Flag
  filtered_results <- scale_results[
    scale_results$m >= m_range[1] & scale_results$m <= m_range[2] &
      scale_results$damping >= damping_min &
      scale_results$rel_err <= rel_err_max,
  ]

  if (nrow(filtered_results) == 0) {
    return(0)
  }

  # Count bubbles by type (B < 0 = positive bubble, B > 0 = negative bubble)
  if (bubble_type == "positive") {
    bubble_count <- sum(filtered_results$B < 0 & filtered_results$hazard > 0)
  } else {  # negative
    bubble_count <- sum(filtered_results$B > 0 & filtered_results$hazard < 0)
  }

  return(bubble_count)
}
