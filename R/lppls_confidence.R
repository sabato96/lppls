#' Compute LPPLS Confidence Indicators
#'
#' @param data A data frame with columns Date, Close, and t (decimal time)
#' @param clusters Number of parallel clusters to use (default: 4)
#' @param window_size Maximum window size in days (default: 10)
#' @param min_window Minimum window size in days (default: 1)
#' @param save Logical, whether to save results (default: FALSE)
#' @param folder Output folder path (default: "./results/")
#' @param progress Logical, whether to show progress tracking (default: TRUE)
#' @param benchmark Logical, whether to show detailed timing information (default: TRUE)
#' @return Data frame with confidence indicators
#' @export
#' @importFrom parallel makeForkCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom dplyr filter as_tibble
#' @importFrom magrittr %>%
compute_lppls_confidence <- function(data, clusters = 4, window_size = 10,
                                     min_window = 1, save = FALSE,
                                     folder = "./results/", progress = TRUE,
                                     benchmark = TRUE) {

  # Initialize timer if benchmarking is enabled

  # Always show completion message
  cat("\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("✓ Window processing completed!\n")

  if (benchmark) {
    timer <- create_timer("LPPLS Confidence Indicator Calculation")
  }

  ticker <- data

  # Initialize confidence indicator columns
  conf_columns <- c(
    "P.SS_EW", "P.SS_EF", "P.S_EW", "P.S_EF",
    "P.M_EW", "P.M_EF", "P.L_EW", "P.L_EF",
    "N.SS_EW", "N.SS_EF", "N.S_EW", "N.S_EF",
    "N.M_EW", "N.M_EF", "N.L_EW", "N.L_EF",
    "P.SS_tc", "P.S_tc", "P.M_tc", "P.L_tc",
    "N.SS_tc", "N.S_tc", "N.M_tc", "N.L_tc"
  )

  conf_ind <- as.data.frame(matrix(0, nrow = nrow(ticker), ncol = length(conf_columns)))
  names(conf_ind) <- conf_columns

  ticker <- cbind(ticker, conf_ind)

  if (benchmark) {
    timer$checkpoint("Data Preparation",
                     paste("Prepared", nrow(ticker), "observations with", length(conf_columns), "indicators"))
  }

  # Setup parallel processing
  cl <- parallel::makeForkCluster(clusters)
  doParallel::registerDoParallel(cl)

  if (benchmark) {
    timer$checkpoint("Parallel Setup", paste("Initialized", clusters, "worker processes"))
  }

  # Calculate total iterations for progress tracking
  total_iterations <- window_size

  # Always show progress - make it more visible
  cat("Starting LPPLS Confidence Calculation...\n")
  cat(sprintf("Processing %d windows with %d parallel workers\n", total_iterations, clusters))
  cat(paste(rep("=", 50), collapse = ""), "\n")

  # Install and load progress package if not available
  if (!requireNamespace("progress", quietly = TRUE)) {
    cat("Installing progress package for better progress bars...\n")
    install.packages("progress")
  }

  # Initialize progress tracking (always enabled)
  pb <- NULL
  use_fancy_progress <- FALSE

  if (requireNamespace("progress", quietly = TRUE)) {
    actual_total <- length(min_window:(window_size + min_window))
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent | :current/:total | Elapsed: :elapsed | ETA: :eta",
      total = actual_total,
      clear = FALSE,
      width = 60
    )
    use_fancy_progress <- TRUE
    cat("Using enhanced progress bar\n")
  } else {
    cat("Using simple progress tracking\n")
  }

  # Track timing for each window
  window_times <- numeric(total_iterations)

  # Main computation loop
  for (j in min_window:(window_size + min_window)) {

    window_start_time <- Sys.time()

    # Define sub-window
    window_start <- max(1, nrow(ticker) - 1350 - j)
    window_end <- nrow(ticker) - j
    sub_ticker <- ticker[window_start:window_end, 1:3]

    # Parallel estimation across multiple time windows
    df_result <- foreach(i = seq(1, min(1437, nrow(sub_ticker) - 40), 1),
                         .combine = rbind, .packages = c("nloptr", "tseries")) %dopar% {

                           # Select estimation window
                           r_ticker <- sub_ticker[i:nrow(sub_ticker), ]

                           # Ensure minimum window size
                           if (nrow(r_ticker) < 40) return(NULL)

                           result <- NULL
                           attempts <- 0
                           max_attempts <- 3

                           # Retry mechanism for robust estimation
                           while (is.null(result) && attempts < max_attempts) {
                             attempts <- attempts + 1
                             try({
                               result <- lppls_estimate(r_ticker)
                             }, silent = TRUE)
                           }

                           return(result)
                         }

    # Filter results based on quality criteria
    if (!is.null(df_result) && nrow(df_result) > 0) {
      df_result <- as_tibble(df_result) %>%
        filter(dt >= 40 & dt <= 1460)

      # Save individual results if requested
      if (save) {
        if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
        filename <- file.path(folder, paste0("df_result_", j, ".csv"))
        write.csv(df_result, filename, row.names = FALSE)
      }

      # Calculate confidence indicators for different time scales
      current_date_idx <- which(ticker$Date == sub_ticker$Date[nrow(sub_ticker)])

      if (length(current_date_idx) > 0) {
        ticker[current_date_idx, ] <- calculate_scale_indicators(ticker[current_date_idx, ], df_result)
      }
    }

    # ALWAYS show progress - force visibility
    window_end_time <- Sys.time()
    window_times[j - min_window + 1] <- as.numeric(difftime(window_end_time, window_start_time, units = "secs"))

    completed <- j - min_window + 1

    # Try fancy progress bar first
    if (use_fancy_progress && !is.null(pb)) {
      pb$tick()
    }

    # ALWAYS show simple progress as backup/additional info
    percent_complete <- 100 * completed / total_iterations
    elapsed_this_window <- window_times[completed]

    # Calculate ETA
    if (completed > 0) {
      elapsed_total <- sum(window_times[1:completed], na.rm = TRUE)
      avg_time <- elapsed_total / completed
      eta <- avg_time * (total_iterations - completed)
    } else {
      eta <- 0
    }

    # Simple text progress that ALWAYS shows
    cat(sprintf("Window %d/%d (%.1f%%) completed in %.2fs | ETA: %.1fs\n",
                completed, total_iterations, percent_complete, elapsed_this_window, eta))
    flush.console()
  }

  if (benchmark) {
    avg_window_time <- mean(window_times, na.rm = TRUE)
    timer$checkpoint("Window Processing",
                     sprintf("Processed %d windows (avg: %.2fs per window)",
                             total_iterations, avg_window_time))
  }

  # Cleanup parallel processing
  try({
    parallel::stopCluster(cl)
  }, silent = TRUE)

  if (benchmark) {
    timer$checkpoint("Cleanup", "Stopped parallel workers")
  }

  # Save final results if requested
  if (save) {
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    filename <- file.path(folder, "LPPLS_confidence_indicators.csv")
    write.csv(ticker, filename, row.names = FALSE)

    if (benchmark) {
      timer$checkpoint("File Output", paste("Saved results to", filename))
    }
  }

  # Finish timing and show summary
  if (benchmark) {
    total_time <- timer$finish("LPPLS Confidence Calculation")

    # Add performance statistics
    cat("\nPerformance Statistics:\n")
    cat(sprintf("  Windows processed: %d\n", total_iterations))
    cat(sprintf("  Average time per window: %.2fs\n", mean(window_times, na.rm = TRUE)))
    cat(sprintf("  Fastest window: %.2fs\n", min(window_times, na.rm = TRUE)))
    cat(sprintf("  Slowest window: %.2fs\n", max(window_times, na.rm = TRUE)))
    cat(sprintf("  Parallel workers used: %d\n", clusters))
    cat(sprintf("  Throughput: %.2f windows/minute\n", total_iterations / (total_time / 60)))

    # Store timing info in result
    attr(ticker, "benchmark") <- list(
      total_time = total_time,
      window_times = window_times,
      avg_window_time = mean(window_times, na.rm = TRUE),
      clusters = clusters,
      total_windows = total_iterations
    )
  }

  return(ticker)

  # Save final results if requested
  if (save) {
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    filename <- file.path(folder, "LPPLS_confidence_indicators.csv")
    write.csv(ticker, filename, row.names = FALSE)
  }

  return(ticker)
}

#' Calculate Scale-Specific Indicators
#'
#' @param ticker_row Single row of ticker data
#' @param df_result Results from LPPLS estimation
#' @return Updated ticker row with calculated indicators
calculate_scale_indicators <- function(ticker_row, df_result) {

  # Define filtering conditions for Early Warning and End Flag
  filters <- list(
    early_warning = list(
      m_range = c(0.01, 1.2),
      w_range = c(2, 25),
      oscill_min = 2.5,
      damp_min = 0.8,
      rel_err_max = 0.05,
      kpss_max = 0.463
    ),
    end_flag = list(
      m_range = c(0.01, 0.99),
      w_range = c(2, 25),
      oscill_min = 2.5,
      damp_min = 1.0,
      rel_err_max = 0.2,
      kpss_max = 0.463
    )
  )

  # Time scale definitions
  scales <- list(
    SS = c(40, 183),    # Super Short
    S = c(40, 360),     # Short
    M = c(365, 730),    # Medium
    L = c(730, 1460)    # Long
  )

  for (scale_name in names(scales)) {
    scale_range <- scales[[scale_name]]

    for (filter_type in names(filters)) {
      filter_params <- filters[[filter_type]]

      # Apply filters for positive bubbles (B < 0)
      positive_bubbles <- df_result %>%
        filter(
          dt >= scale_range[1] & dt <= scale_range[2],
          B < 0,
          m >= filter_params$m_range[1] & m <= filter_params$m_range[2],
          w >= filter_params$w_range[1] & w <= filter_params$w_range[2],
          oscillations >= filter_params$oscill_min,
          damping >= filter_params$damp_min,
          rel_err >= 0 & rel_err <= filter_params$rel_err_max,
          test_resid < filter_params$kpss_max,
          hazard > 0
        )

      # Apply filters for negative bubbles (B > 0)
      negative_bubbles <- df_result %>%
        filter(
          dt >= scale_range[1] & dt <= scale_range[2],
          B > 0,
          m >= filter_params$m_range[1] & m <= filter_params$m_range[2],
          w >= filter_params$w_range[1] & w <= filter_params$w_range[2],
          oscillations >= filter_params$oscill_min,
          damping >= filter_params$damp_min,
          rel_err >= 0 & rel_err <= filter_params$rel_err_max,
          test_resid < filter_params$kpss_max,
          hazard < 0
        )

      # Count total eligible windows for this scale
      total_windows <- df_result %>%
        filter(dt >= scale_range[1] & dt <= scale_range[2]) %>%
        nrow()

      # Calculate confidence indicators
      if (total_windows > 0) {
        pos_ratio <- nrow(positive_bubbles) / total_windows
        neg_ratio <- nrow(negative_bubbles) / total_windows

        # Update ticker row with calculated ratios
        filter_suffix <- ifelse(filter_type == "early_warning", "EW", "EF")

        pos_col <- paste0("P.", scale_name, "_", filter_suffix)
        neg_col <- paste0("N.", scale_name, "_", filter_suffix)

        if (pos_col %in% names(ticker_row)) {
          ticker_row[[pos_col]] <- round(pos_ratio, 5)
        }
        if (neg_col %in% names(ticker_row)) {
          ticker_row[[neg_col]] <- round(neg_ratio, 5)
        }

        # Calculate median critical times
        tc_col <- paste0(ifelse(nrow(positive_bubbles) > nrow(negative_bubbles), "P", "N"),
                         ".", scale_name, "_tc")

        if (tc_col %in% names(ticker_row)) {
          if (nrow(positive_bubbles) > 0) {
            ticker_row[[tc_col]] <- median(positive_bubbles$tc, na.rm = TRUE)
          } else if (nrow(negative_bubbles) > 0) {
            ticker_row[[tc_col]] <- median(negative_bubbles$tc, na.rm = TRUE)
          }
        }
      }
    }
  }

  return(ticker_row)
}
