#' Prepare Time Series Data for LPPLS Analysis
#'
#' @param data A data frame with Date and Close columns, or path to CSV file
#' @param date_format Format of the date column (default: "%Y-%m-%d")
#' @param subset_range Optional vector of row indices to subset data
#' @return Data frame with Date, Close, and t (decimal time) columns
#' @export
#' @importFrom lubridate decimal_date
prepare_lppls_data <- function(data, date_format = "%Y-%m-%d", subset_range = NULL) {

  # Handle file input
  if (is.character(data) && length(data) == 1) {
    if (!file.exists(data)) {
      stop("File not found: ", data)
    }
    ticker <- read.csv(data, stringsAsFactors = FALSE)
  } else {
    ticker <- as.data.frame(data)
  }

  # Validate required columns
  if (!"Date" %in% names(ticker)) {
    stop("Data must contain a 'Date' column")
  }

  # Handle different price column names
  price_cols <- c("Close", "Adj.Close", "close", "price", "Close.Price")
  price_col <- price_cols[price_cols %in% names(ticker)][1]

  if (is.na(price_col)) {
    stop("Data must contain a price column (Close, Adj.Close, close, price, or Close.Price)")
  }

  # Standardize column names
  names(ticker)[names(ticker) == price_col] <- "Close"

  # Convert date column
  ticker$Date <- as.Date(ticker$Date, format = date_format)

  # Remove rows with invalid dates or prices
  ticker <- ticker[!is.na(ticker$Date) & !is.na(ticker$Close), ]

  # Convert Close to numeric and handle "null" values
  ticker$Close <- ifelse(ticker$Close == "null", NA, ticker$Close)
  ticker$Close <- as.numeric(ticker$Close)
  ticker <- ticker[!is.na(ticker$Close), ]

  # Add decimal time column
  ticker$t <- decimal_date(ticker$Date)

  # Select only required columns
  ticker <- ticker[, c("Date", "Close", "t")]

  # Apply subset if specified
  if (!is.null(subset_range)) {
    if (length(subset_range) == 2) {
      ticker <- ticker[subset_range[1]:subset_range[2], ]
    } else {
      ticker <- ticker[subset_range, ]
    }
  }

  # Sort by date
  ticker <- ticker[order(ticker$Date), ]

  # Reset row names
  rownames(ticker) <- NULL

  return(ticker)
}

#' Load Sample Data
#'
#' @param dataset Name of sample dataset to load
#' @return Data frame with sample financial data
#' @export
load_sample_data <- function(dataset = "sp500") {

  available_datasets <- c("sp500", "bitcoin", "oil", "gold")

  if (!dataset %in% available_datasets) {
    stop("Available datasets: ", paste(available_datasets, collapse = ", "))
  }

  # This is a placeholder - in a real package you would include sample data files
  # For now, generate synthetic data
  generate_synthetic_data(dataset)
}

#' Generate Synthetic Financial Data
#'
#' @param type Type of data to generate
#' @param n Number of observations (default: 1000)
#' @param start_date Start date for the series (default: "2020-01-01")
#' @return Data frame with synthetic financial data
#' @export
generate_synthetic_data <- function(type = "sp500", n = 1000, start_date = "2020-01-01") {

  dates <- seq(as.Date(start_date), by = "day", length.out = n)

  # Generate price series with different characteristics based on type
  set.seed(123)  # For reproducibility

  if (type == "bubble") {
    # Generate data with bubble characteristics
    t <- 1:n
    trend <- 0.0005 * t
    bubble_component <- ifelse(t > n/2, 0.002 * (t - n/2)^1.5, 0)
    noise <- rnorm(n, 0, 0.01)
    log_returns <- trend + bubble_component + noise
    prices <- 100 * exp(cumsum(log_returns))
  } else {
    # Generate regular financial data
    log_returns <- rnorm(n, 0.0002, 0.015)
    prices <- 100 * exp(cumsum(log_returns))
  }

  data.frame(
    Date = dates,
    Close = prices,
    stringsAsFactors = FALSE
  )
}

#' Validate LPPLS Data
#'
#' @param data Data frame to validate
#' @return Logical indicating if data is valid, with attributes for any issues
validate_lppls_data <- function(data) {

  issues <- character(0)

  # Check required columns
  required_cols <- c("Date", "Close", "t")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    issues <- c(issues, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check data types
  if ("Date" %in% names(data) && !inherits(data$Date, "Date")) {
    issues <- c(issues, "Date column must be of Date type")
  }

  if ("Close" %in% names(data) && !is.numeric(data$Close)) {
    issues <- c(issues, "Close column must be numeric")
  }

  if ("t" %in% names(data) && !is.numeric(data$t)) {
    issues <- c(issues, "t column must be numeric")
  }

  # Check for missing values
  if (any(is.na(data$Close))) {
    issues <- c(issues, "Close column contains missing values")
  }

  # Check minimum data requirements
  if (nrow(data) < 40) {
    issues <- c(issues, "Data must contain at least 40 observations")
  }

  # Check for non-positive prices
  if (any(data$Close <= 0, na.rm = TRUE)) {
    issues <- c(issues, "Close prices must be positive")
  }

  is_valid <- length(issues) == 0
  attr(is_valid, "issues") <- issues

  return(is_valid)
}
