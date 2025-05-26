#' LPPLS Model Estimation Function
#'
#' @param data A data frame with columns Date, Close, and t (decimal time)
#' @param plot Logical, whether to plot the fitted model
#' @param optimizer Character, optimization method to use ("mlsl" or "de")
#' @return A data frame with estimation results and parameters
#' @export
#' @importFrom nloptr mlsl
#' @importFrom DEoptim DEoptim
#' @importFrom tseries kpss.test
#' @importFrom lubridate decimal_date
lppls_estimate <- function(data, plot = FALSE, optimizer = "mlsl") {

  # Validate input data
  if (!all(c("Date", "Close", "t") %in% names(data))) {
    stop("Data must contain columns: Date, Close, t")
  }

  # Validate optimizer parameter
  valid_optimizers <- c("mlsl", "de")
  if (!optimizer %in% valid_optimizers) {
    stop("optimizer must be one of: ", paste(valid_optimizers, collapse = ", "))
  }

  ticker <- data

  # Calculate time window properties
  last_row <- tail(ticker, 1)
  first_row <- head(ticker, 1)
  dt <- last_row$t - first_row$t

  # Set optimization bounds
  start_search <- c(
    runif(1, max(ticker$t) - 0.2 * dt, max(ticker$t) + 0.2 * dt),
    runif(1, 0.01, 1.99),
    runif(1, 1, 50)
  )

  upper <- c(max(ticker$t) + 0.2 * dt, 2, 50)
  lower <- c(max(ticker$t) - 0.2 * dt, 0.01, 1)

  # Choose optimization method
  if (optimizer == "de") {
    test <- lppls_de_optimize(start_search, lower, upper, ticker)
  } else if (optimizer == "mlsl") {
    # Current MLSL implementation
    test <- mlsl(
      start_search,
      lppls_objective_function,
      lower = lower,
      upper = upper,
      local.method = "LBFGS",
      data = ticker
    )
  }

  # Calculate linear parameters
  linear_param <- calculate_linear_parameters(ticker, test$par[1], test$par[2], test$par[3])

  # Generate fitted values
  fitted <- lppls_model(
    ticker,
    test$par[1], test$par[2], test$par[3],
    linear_param[1], linear_param[2], linear_param[3], linear_param[4]
  )

  # Plot if requested
  if (plot) {
    plot(log(ticker$Close), type = "l", col = "red",
         main = paste("LPPLS Model Fit (", toupper(optimizer), ")", sep = ""),
         ylab = "Log Price", xlab = "Time")
    lines(fitted, col = "blue")
    legend("topleft", c("Actual", "LPPLS Fit"), col = c("red", "blue"), lty = 1)
  }

  # Calculate residuals and diagnostics
  residual <- log(ticker$Close) - fitted
  test_resid <- suppressWarnings(tseries::kpss.test(residual)[1])

  # Prepare results
  results <- data.frame(
    start_date = first_row$Date,
    end_date = last_row$Date,
    last_price = last_row$Close,
    dt = as.integer(dt / (1/365)),
    LPPL_max = exp(max(fitted)),
    tc_minus_end = test$par[1] - last_row$t,
    days_to_tc = as.integer((test$par[1] - last_row$t) / (1/365)),
    m = test$par[2],
    w = test$par[3],
    tc = test$par[1],
    A = linear_param[1],
    B = linear_param[2],
    C1 = linear_param[3],
    C2 = linear_param[4],
    oscillations = (test$par[3] / (2 * pi)) * log(abs(test$par[1] / (test$par[1] - last_row$t))),
    damping = (test$par[2] * abs(linear_param[2])) / (test$par[3] * abs((linear_param[3]^2 + linear_param[4]^2)^0.5)),
    rel_err = abs((log(last_row$Close) - fitted[length(fitted)]) / fitted[length(fitted)]),
    dt_filter_low = last_row$t - 0.05 * dt,
    dt_filter_high = last_row$t + 0.1 * dt,
    hazard = -linear_param[2] * test$par[2] - abs((linear_param[3]^2 + linear_param[4]^2)^0.5) * sqrt(test$par[2]^2 + test$par[3]^2),
    test_resid = as.numeric(test_resid),
    resid_sum_sq = sum(residual^2),
    optimizer_used = optimizer
  )

  rownames(results) <- NULL
  return(results)
}

#' Differential Evolution Optimization for LPPLS
#'
#' @param start_params Initial parameter vector [tc, m, w]
#' @param lower Lower bounds vector
#' @param upper Upper bounds vector
#' @param data Time series data frame
#' @return Optimization result compatible with MLSL output format
#' @importFrom DEoptim DEoptim
lppls_de_optimize <- function(start_params, lower, upper, data) {

  # Set DE control parameters optimized for LPPLS
  de_control <- list(
    itermax = 200,           # Maximum iterations
    CR = 0.9,               # Crossover probability
    F = 0.8,                # Differential weight
    strategy = 2,           # DE/rand/1/bin strategy
    storepopfrom = 1,       # Store population for analysis
    trace = FALSE,          # No verbose output
    parallelType = 0,       # Use sequential processing (parallel handled at higher level)
    packages = c("LPPLS"), # Required packages
    parVar = c("data")      # Variables to export to workers
  )

  # Run differential evolution
  de_result <- DEoptim(
    fn = lppls_objective_function,
    lower = lower,
    upper = upper,
    data = data,
    control = de_control
  )

  # Convert DEoptim result to MLSL-compatible format
  mlsl_compatible_result <- list(
    par = as.vector(de_result$optim$bestmem),
    value = de_result$optim$bestval,
    iter = de_result$optim$iter,
    convergence = ifelse(de_result$optim$iter < de_control$itermax, 0, 1),
    message = paste("DE optimization completed in", de_result$optim$iter, "iterations")
  )

  return(mlsl_compatible_result)
}

#' Hybrid DE + Local Search Optimization for LPPLS
#'
#' @param start_params Initial parameter vector [tc, m, w]
#' @param lower Lower bounds vector
#' @param upper Upper bounds vector
#' @param data Time series data frame
#' @return Optimization result compatible with MLSL output format
lppls_hybrid_optimize <- function(start_params, lower, upper, data) {

  # Phase 1: Global search with DE (reduced iterations)
  de_control <- list(
    itermax = 100,    # Reduced for global phase
    CR = 0.9,
    F = 0.8,
    strategy = 2,
    trace = FALSE,
    parallelType = 0
  )

  de_result <- DEoptim(
    fn = lppls_objective_function,
    lower = lower,
    upper = upper,
    data = data,
    control = de_control
  )

  # Phase 2: Local refinement with L-BFGS-B
  local_result <- optim(
    par = as.vector(de_result$optim$bestmem),
    fn = lppls_objective_function,
    data = data,
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    control = list(maxit = 100)
  )

  # Return best result from either phase
  if (local_result$value < de_result$optim$bestval) {
    return(list(
      par = local_result$par,
      value = local_result$value,
      convergence = local_result$convergence,
      message = "Hybrid optimization: Local search improved DE result"
    ))
  } else {
    return(list(
      par = as.vector(de_result$optim$bestmem),
      value = de_result$optim$bestval,
      convergence = 0,
      message = "Hybrid optimization: DE result was optimal"
    ))
  }
}

#' LPPLS Model Function
#'
#' @param data Data frame with time series data
#' @param tc Critical time parameter
#' @param m Power law exponent
#' @param w Log-periodic frequency
#' @param a Constant term
#' @param b Linear coefficient
#' @param c1 Cosine coefficient
#' @param c2 Sine coefficient
#' @return Vector of fitted values
lppls_model <- function(data, tc, m, w, a, b, c1, c2) {
  dif_time <- abs(tc - data$t)
  est <- a + dif_time^m * (b + ((c1 * cos(w * log(dif_time))) + (c2 * sin(w * log(dif_time)))))
  return(est)
}

#' Calculate Linear Parameters for LPPLS Model
#'
#' @param data Data frame with time series data
#' @param tc Critical time parameter
#' @param m Power law exponent
#' @param w Log-periodic frequency
#' @return Vector of linear parameters [A, B, C1, C2]
calculate_linear_parameters <- function(data, tc, m, w) {
  ti <- abs(tc - data$t)
  fi <- ti^m
  gi <- ti^m * cos(w * log(ti))
  hi <- ti^m * sin(w * log(ti))
  yi <- log(data$Close)

  # Design matrix
  X <- cbind(1, fi, gi, hi)

  # Solve normal equations
  coef <- solve(t(X) %*% X, t(X) %*% yi)

  return(as.vector(coef))
}

#' LPPLS Objective Function
#'
#' @param x Vector of parameters [tc, m, w]
#' @param data Data frame with time series data
#' @return Residual sum of squares
lppls_objective_function <- function(x, data) {
  tc <- x[1]
  m <- x[2]
  w <- x[3]

  lin_par <- calculate_linear_parameters(data, tc, m, w)

  # Calculate residuals
  fitted_values <- lppls_model(data, tc, m, w, lin_par[1], lin_par[2], lin_par[3], lin_par[4])
  residuals <- log(data$Close) - fitted_values

  RSS <- sum(residuals^2)
  return(RSS)
}
