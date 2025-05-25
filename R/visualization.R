#' Plot LPPLS Model Fit
#'
#' @param data Original time series data
#' @param lppls_result Result from lppls_estimate function
#' @param title Plot title (optional)
#' @param show_critical_time Logical, whether to show critical time line
#' @return ggplot object if ggplot2 is available, otherwise base plot
#' @export
plot_lppls_fit <- function(data, lppls_result, title = "LPPLS Model Fit",
                           show_critical_time = TRUE) {

  # Check if ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plot_lppls_ggplot(data, lppls_result, title, show_critical_time)
  } else {
    plot_lppls_base(data, lppls_result, title, show_critical_time)
  }
}

#' Plot LPPLS Confidence Indicators
#'
#' @param confidence_data Data frame with confidence indicators
#' @param scale Time scale to plot ("SS", "S", "M", "L")
#' @param date_col Name of date column (default: "Date")
#' @param title Plot title (optional)
#' @return ggplot object if ggplot2 is available, otherwise base plot
#' @export
plot_confidence_indicators <- function(confidence_data, scale = "S",
                                       date_col = "Date",
                                       title = paste("LPPLS Confidence Indicators -", scale, "Scale")) {

  # Validate scale parameter
  valid_scales <- c("SS", "S", "M", "L")
  if (!scale %in% valid_scales) {
    stop("Scale must be one of: ", paste(valid_scales, collapse = ", "))
  }

  # Define column names for the specified scale
  ew_col <- paste0("P.", scale, "_EW")
  ef_col <- paste0("P.", scale, "_EF")

  # Check if columns exist
  if (!all(c(ew_col, ef_col) %in% names(confidence_data))) {
    stop("Required confidence indicator columns not found")
  }

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plot_confidence_ggplot(confidence_data, scale, date_col, title)
  } else {
    plot_confidence_base(confidence_data, scale, date_col, title)
  }
}

#' Internal function for ggplot2-based LPPLS fit plotting
#' @keywords internal
plot_lppls_ggplot <- function(data, lppls_result, title, show_critical_time) {

  # Generate fitted values
  fitted_values <- lppls_model(
    data,
    lppls_result$tc,
    lppls_result$m,
    lppls_result$w,
    lppls_result$A,
    lppls_result$B,
    lppls_result$C1,
    lppls_result$C2
  )

  plot_data <- data.frame(
    Date = data$Date,
    Actual = log(data$Close),
    Fitted = fitted_values
  )

  # Reshape for ggplot
  plot_data_long <- tidyr::pivot_longer(plot_data,
                                        cols = c("Actual", "Fitted"),
                                        names_to = "Series",
                                        values_to = "LogPrice")

  p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = Date, y = LogPrice, color = Series)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_color_manual(values = c("Actual" = "red", "Fitted" = "blue")) +
    ggplot2::labs(title = title, x = "Date", y = "Log Price") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  # Add critical time line if requested
  if (show_critical_time) {
    # Convert decimal time to date
    tc_date <- as.Date("1970-01-01") + (lppls_result$tc - 1970) * 365.25
    p <- p + ggplot2::geom_vline(xintercept = tc_date,
                                 linetype = "dashed",
                                 color = "orange",
                                 size = 1) +
      ggplot2::annotate("text", x = tc_date, y = max(plot_data_long$LogPrice),
                        label = paste("Critical Time:", format(tc_date, "%Y-%m-%d")),
                        angle = 90, vjust = -0.5)
  }

  return(p)
}

#' Internal function for base R LPPLS fit plotting
#' @keywords internal
plot_lppls_base <- function(data, lppls_result, title, show_critical_time) {

  # Generate fitted values
  fitted_values <- lppls_model(
    data,
    lppls_result$tc,
    lppls_result$m,
    lppls_result$w,
    lppls_result$A,
    lppls_result$B,
    lppls_result$C1,
    lppls_result$C2
  )

  # Create plot
  plot(data$Date, log(data$Close),
       type = "l", col = "red", lwd = 2,
       main = title,
       xlab = "Date",
       ylab = "Log Price")

  lines(data$Date, fitted_values, col = "blue", lwd = 2)

  # Add critical time line if requested
  if (show_critical_time) {
    tc_date <- as.Date("1970-01-01") + (lppls_result$tc - 1970) * 365.25
    abline(v = tc_date, lty = 2, col = "orange", lwd = 2)
    text(tc_date, max(log(data$Close)),
         paste("Critical Time:", format(tc_date, "%Y-%m-%d")),
         srt = 90, adj = c(-0.1, 0.5))
  }

  legend("topleft",
         legend = c("Actual", "LPPLS Fit"),
         col = c("red", "blue"),
         lty = 1, lwd = 2)
}

#' Internal function for ggplot2-based confidence indicator plotting
#' @keywords internal
plot_confidence_ggplot <- function(confidence_data, scale, date_col, title) {

  # Define column names
  ew_col <- paste0("P.", scale, "_EW")
  ef_col <- paste0("P.", scale, "_EF")

  # Prepare data for plotting
  plot_data <- data.frame(
    Date = confidence_data[[date_col]],
    EarlyWarning = confidence_data[[ew_col]],
    EndFlag = confidence_data[[ef_col]]
  )

  # Remove rows with missing dates
  plot_data <- plot_data[!is.na(plot_data$Date), ]

  # Reshape for ggplot
  if (requireNamespace("tidyr", quietly = TRUE)) {
    plot_data_long <- tidyr::pivot_longer(plot_data,
                                          cols = c("EarlyWarning", "EndFlag"),
                                          names_to = "Indicator",
                                          values_to = "Value")
  } else {
    # Manual reshape if tidyr not available
    plot_data_long <- rbind(
      data.frame(Date = plot_data$Date,
                 Indicator = "EarlyWarning",
                 Value = plot_data$EarlyWarning),
      data.frame(Date = plot_data$Date,
                 Indicator = "EndFlag",
                 Value = plot_data$EndFlag)
    )
  }

  p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = Date, y = Value, color = Indicator)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_color_manual(values = c("EarlyWarning" = "red", "EndFlag" = "green")) +
    ggplot2::labs(title = title, x = "Date", y = "Confidence Level") +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  return(p)
}

#' Internal function for base R confidence indicator plotting
#' @keywords internal
plot_confidence_base <- function(confidence_data, scale, date_col, title) {

  # Define column names
  ew_col <- paste0("P.", scale, "_EW")
  ef_col <- paste0("P.", scale, "_EF")

  # Prepare data
  dates <- confidence_data[[date_col]]
  ew_values <- confidence_data[[ew_col]]
  ef_values <- confidence_data[[ef_col]]

  # Remove missing dates
  valid_idx <- !is.na(dates)
  dates <- dates[valid_idx]
  ew_values <- ew_values[valid_idx]
  ef_values <- ef_values[valid_idx]

  # Create plot
  plot(dates, ew_values,
       type = "l", col = "red", lwd = 2,
       main = title,
       xlab = "Date",
       ylab = "Confidence Level",
       ylim = c(0, 1))

  lines(dates, ef_values, col = "green", lwd = 2)

  legend("topleft",
         legend = c("Early Warning", "End Flag"),
         col = c("red", "green"),
         lty = 1, lwd = 2)
}

#' Plot Multiple LPPLS Models
#'
#' @param data Original time series data
#' @param models_list List of LPPLS model results
#' @param title Plot title (optional)
#' @param max_models Maximum number of models to plot (default: 15)
#' @return Plot showing multiple LPPLS fits
#' @export
plot_multiple_lppls <- function(data, models_list, title = "Multiple LPPLS Models",
                                max_models = 15) {

  if (length(models_list) > max_models) {
    models_list <- models_list[1:max_models]
    warning("Only plotting first ", max_models, " models")
  }

  # Start with base plot
  plot(data$Date, log(data$Close),
       type = "l", col = "black", lwd = 2,
       main = title,
       xlab = "Date",
       ylab = "Log Price")

  # Add each model
  colors <- rainbow(length(models_list))

  for (i in seq_along(models_list)) {
    model <- models_list[[i]]

    # Generate fitted values for this model
    fitted_values <- lppls_model(
      data,
      model$tc,
      model$m,
      model$w,
      model$A,
      model$B,
      model$C1,
      model$C2
    )

    lines(data$Date, fitted_values, col = colors[i], lwd = 1.5, alpha = 0.7)
  }

  legend("topleft",
         legend = c("Actual", paste("Model", 1:min(3, length(models_list)))),
         col = c("black", colors[1:min(3, length(colors))]),
         lty = 1, lwd = c(2, rep(1.5, min(3, length(models_list)))))
}

#' Create Summary Dashboard Plot
#'
#' @param data Time series data
#' @param confidence_results Confidence indicator results
#' @param lppls_fit Single LPPLS fit result (optional)
#' @return Combined dashboard plot
#' @export
create_lppls_dashboard <- function(data, confidence_results, lppls_fit = NULL) {

  # Set up layout for multiple plots
  old_par <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  on.exit(par(old_par))

  # Plot 1: Price time series
  plot(data$Date, data$Close,
       type = "l", col = "blue", lwd = 2,
       main = "Price Time Series",
       xlab = "Date",
       ylab = "Price")

  # Plot 2: LPPLS fit if provided
  if (!is.null(lppls_fit)) {
    plot_lppls_base(data, lppls_fit, "LPPLS Model Fit", TRUE)
  } else {
    plot(data$Date, log(data$Close),
         type = "l", col = "red", lwd = 2,
         main = "Log Price",
         xlab = "Date",
         ylab = "Log Price")
  }

  # Plot 3: Short-term confidence indicators
  plot_confidence_base(confidence_results, "S", "Date", "Short-term Confidence")

  # Plot 4: Long-term confidence indicators
  plot_confidence_base(confidence_results, "L", "Date", "Long-term Confidence")
}
