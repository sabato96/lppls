#' Progress Tracking Utilities for LPPLS Analysis
#'
#' Functions for tracking progress and benchmarking LPPLS computations
#' @keywords internal

#' Create Progress Bar
#'
#' @param total Total number of iterations
#' @param format Progress bar format
#' @return Progress bar object or simple counter
create_progress_bar <- function(total, format = "[:bar] :percent (:current/:total) ETA: :eta Elapsed: :elapsed") {
  if (requireNamespace("progress", quietly = TRUE)) {
    progress::progress_bar$new(
      format = format,
      total = total,
      clear = FALSE,
      width = 80
    )
  } else {
    # Simple fallback progress tracker
    list(
      total = total,
      current = 0,
      start_time = Sys.time(),
      tick = function() {
        current <<- current + 1
        if (current %% max(1, floor(total / 20)) == 0) {
          elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
          eta <- elapsed * (total - current) / current
          cat(sprintf("\rProgress: %d/%d (%.1f%%) - ETA: %.1fs",
                      current, total, 100 * current / total, eta))
          flush.console()
        }
      },
      terminate = function() cat("\n")
    )
  }
}

#' Create Benchmark Timer
#'
#' @param description Description of the benchmark
#' @return List with timer functions
#' @export
create_timer <- function(description = "LPPLS Analysis") {
  start_time <- Sys.time()
  checkpoints <- list()

  cat("Starting", description, "at", format(start_time), "\n")
  cat("==================================================\n")

  timer <- list()

  timer$checkpoint <- function(name, message = NULL) {
    current_time <- Sys.time()
    elapsed <- as.numeric(difftime(current_time, start_time, units = "secs"))

    # Calculate stage time
    if (length(checkpoints) > 0) {
      last_checkpoint <- checkpoints[[length(checkpoints)]]
      last_time <- last_checkpoint$time
      stage_elapsed <- as.numeric(difftime(current_time, last_time, units = "secs"))
    } else {
      stage_elapsed <- elapsed
    }

    checkpoint_info <- list(
      time = current_time,
      elapsed_total = elapsed,
      elapsed_stage = stage_elapsed
    )

    checkpoints[[name]] <<- checkpoint_info

    if (!is.null(message)) {
      cat(sprintf("✓ %s (%.2fs) - %s\n", name, stage_elapsed, message))
    } else {
      cat(sprintf("✓ %s (%.2fs)\n", name, stage_elapsed))
    }
  }

  timer$finish <- function(message = "Analysis completed") {
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    cat("\n==================================================\n")
    cat(sprintf("%s in %.2f seconds (%.2f minutes)\n",
                message, total_time, total_time / 60))

    if (length(checkpoints) > 0) {
      cat("\nStage Breakdown:\n")
      for (name in names(checkpoints)) {
        checkpoint <- checkpoints[[name]]
        percentage <- (checkpoint$elapsed_stage / total_time) * 100
        cat(sprintf("  %s: %.2fs (%.1f%%)\n",
                    name, checkpoint$elapsed_stage, percentage))
      }
    }

    invisible(total_time)
  }

  timer$get_summary <- function() {
    if (length(checkpoints) == 0) {
      return(data.frame())
    }

    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    summary_df <- data.frame(
      stage = names(checkpoints),
      elapsed_seconds = sapply(checkpoints, function(x) x$elapsed_stage),
      cumulative_seconds = sapply(checkpoints, function(x) x$elapsed_total),
      percentage = sapply(checkpoints, function(x) (x$elapsed_stage / total_time) * 100),
      stringsAsFactors = FALSE
    )

    return(summary_df)
  }

  return(timer)
}

#' Extract Benchmark Results
#'
#' @param lppls_result Result from compute_lppls_confidence with benchmark=TRUE
#' @return Data frame with benchmark statistics
#' @export
extract_benchmark <- function(lppls_result) {
  benchmark_info <- attr(lppls_result, "benchmark")

  if (is.null(benchmark_info)) {
    stop("No benchmark information found. Run compute_lppls_confidence with benchmark=TRUE")
  }

  data.frame(
    metric = c("Total Time (seconds)", "Total Time (minutes)", "Average Window Time",
               "Fastest Window", "Slowest Window", "Clusters Used", "Total Windows",
               "Throughput (windows/minute)"),
    value = c(
      round(benchmark_info$total_time, 2),
      round(benchmark_info$total_time / 60, 2),
      round(benchmark_info$avg_window_time, 2),
      round(min(benchmark_info$window_times, na.rm = TRUE), 2),
      round(max(benchmark_info$window_times, na.rm = TRUE), 2),
      benchmark_info$clusters,
      benchmark_info$total_windows,
      round(benchmark_info$total_windows / (benchmark_info$total_time / 60), 2)
    ),
    stringsAsFactors = FALSE
  )
}

#' Plot Window Processing Times
#'
#' @param lppls_result Result from compute_lppls_confidence with benchmark=TRUE
#' @return Plot of processing times per window
#' @export
plot_window_times <- function(lppls_result) {
  benchmark_info <- attr(lppls_result, "benchmark")

  if (is.null(benchmark_info)) {
    stop("No benchmark information found. Run compute_lppls_confidence with benchmark=TRUE")
  }

  window_times <- benchmark_info$window_times
  window_numbers <- seq_along(window_times)

  plot(window_numbers, window_times,
       type = "b", pch = 16, col = "blue",
       main = "Processing Time per Window",
       xlab = "Window Number",
       ylab = "Processing Time (seconds)")

  # Add average line
  avg_time <- mean(window_times, na.rm = TRUE)
  abline(h = avg_time, col = "red", lty = 2)

  # Add legend
  legend("topright",
         legend = c("Window Times", paste("Average:", round(avg_time, 2), "s")),
         col = c("blue", "red"),
         lty = c(1, 2),
         pch = c(16, NA))
}
