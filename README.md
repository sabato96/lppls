# LPPLS: Log Periodic Power Law Singularity Model for Financial Bubbles

[![R-CMD-check](https://github.com/yourusername/LPPLS/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/LPPLS/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/LPPLS)](https://CRAN.R-project.org/package=LPPLS)

## Overview

The LPPLS package provides a comprehensive implementation of the Log Periodic Power Law Singularity model for detecting and analyzing financial bubbles. The model was originally developed by Johansen, Ledoit, and Sornette to identify critical points in financial time series that may precede market crashes.

## Key Features

- **LPPLS Model Estimation**: Fit the LPPLS model to financial time series data
- **Confidence Indicators**: Calculate multi-scale confidence indicators for bubble detection
- **Robust Optimization**: Uses the MLSL (Multi-Level Single-Linkage) algorithm for reliable parameter estimation
- **Multiple Time Scales**: Analyze bubbles across different time horizons (short, medium, long-term)
- **Visualization Tools**: Create plots for model fits and confidence indicators
- **Parallel Processing**: Leverage multiple CPU cores for faster computation

## Installation

### From GitHub (Development Version)

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install LPPLS package
devtools::install_github("yourusername/LPPLS")
```

### Dependencies

The package requires the following R packages:
- `nloptr` - for optimization
- `zoo`, `lubridate` - for time series handling
- `dplyr` - for data manipulation
- `parallel`, `doParallel`, `foreach` - for parallel processing
- `tseries` - for time series analysis

Optional packages for enhanced functionality:
- `ggplot2`, `tidyr` - for advanced plotting
- `quantmod` - for financial data import

## Quick Start

### Basic Usage

```r
library(LPPLS)

# Load your financial data (CSV with Date and Close columns)
data <- read.csv("your_financial_data.csv")

# Quick analysis
results <- quick_lppls_analysis("your_financial_data.csv", plot_results = TRUE)

# Or step by step:

# 1. Prepare data
prepared_data <- prepare_lppls_data(data)

# 2. Estimate single LPPLS model
fit <- lppls_estimate(prepared_data, plot = TRUE)

# 3. Calculate confidence indicators across multiple time scales
confidence <- compute_lppls_confidence(prepared_data, clusters = 4)

# 4. Visualize results
plot_confidence_indicators(confidence, scale = "S")
```

### Example with Synthetic Data

```r
# Run example with synthetic bubble data
results <- run_lppls_example(generate_bubble = TRUE, plot_results = TRUE)

# Access different components
print(results$single_fit)
head(results$confidence_indicators)
```

## The LPPLS Model

The LPPLS model describes the expected log-price evolution during a bubble as:

```
E[ln p(t)] = A + B|tc - t|^m + C|tc - t|^m * cos[ω ln|tc - t| - φ]
```

Where:
- **A**: Log price at critical time
- **B**: Amplitude of power law growth  
- **C**: Amplitude of log-periodic oscillations
- **m**: Power law exponent (0 < m < 1)
- **ω**: Log-periodic frequency
- **tc**: Critical time (estimated crash time)
- **φ**: Phase parameter

### Key Assumptions

1. **Super-exponential growth**: Price growth accelerates (m < 1)
2. **Log-periodic oscillations**: Prices exhibit oscillatory behavior with increasing frequency
3. **Finite-time singularity**: The model predicts a critical time tc where the bubble becomes unsustainable

## Confidence Indicators

The package calculates confidence indicators across four time scales:

- **Super-Short (SS)**: 40-183 days
- **Short (S)**: 40-365 days  
- **Medium (M)**: 365-730 days
- **Long (L)**: 730-1460 days

Each scale provides:
- **Early Warning (EW)**: Indicates bubble formation
- **End Flag (EF)**: Suggests bubble is reaching critical phase

## Advanced Usage

### Parallel Processing

```r
# Use multiple cores for faster computation
confidence <- compute_lppls_confidence(
  data, 
  clusters = 8,  # Use 8 CPU cores
  window_size = 10,
  save = TRUE,
  folder = "./results/"
)
```

### Custom Visualization

```r
# Plot specific time scale
plot_confidence_indicators(confidence, scale = "L", 
                          title = "Long-term Bubble Indicators")

# Create dashboard
create_lppls_dashboard(data, confidence, single_fit)

# Plot multiple model fits
models_list <- list(fit1, fit2, fit3)
plot_multiple_lppls(data, models_list)
```

### Data Validation

```r
# Validate your data before analysis
is_valid <- validate_lppls_data(prepared_data)
if (!is_valid) {
  print(attr(is_valid, "issues"))
}
```

## Model Interpretation

### Positive vs Negative Bubbles

- **B < 0**: Positive bubble (upward price acceleration)
- **B > 0**: Negative bubble (downward price acceleration)

### Quality Indicators

- **Relative Error < 0.05**: Good model fit
- **Oscillations > 2.5**: Sufficient log-periodic pattern
- **Damping > 0.8**: Stable oscillatory behavior

### Critical Time Interpretation

- **tc > current time**: Bubble may continue growing
- **tc ≈ current time**: Bubble may be reaching critical phase
- **Multiple models converge on similar tc**: Higher confidence in prediction

## Computational Considerations

LPPLS analysis is computationally intensive. For large datasets:

1. **Use parallel processing**: Set `clusters` parameter appropriately
2. **Reduce window sizes**: For testing, use smaller `window_size` and `min_window`
3. **Save intermediate results**: Set `save = TRUE` to avoid losing progress
4. **Monitor memory usage**: Large datasets may require substantial RAM

## References

1. Johansen, A., Ledoit, O., & Sornette, D. (2000). Crashes as critical points. *International Journal of Theoretical and Applied Finance*, 3(02), 219-255.

2. Sornette, D. (2009). *Why stock markets crash: critical events in complex financial systems*. Princeton University Press.

3. Filimonov, V., & Sornette, D. (2013). A stable and robust calibration scheme of the log-periodic power law model. *Physica A: Statistical Mechanics and its Applications*, 392(17), 3698-3707.

4. Zhang, Q., Sornette, D., Balcilar, M., Gupta, R., Ozdemir, Z. A., & Yetkiner, H. (2016). LPPLS bubble indicators over two centuries of the S&P 500 index. *Physica A: Statistical Mechanics and its Applications*, 458, 126-139.

## Contributing

Contributions are welcome! Please feel free to submit issues, feature requests, or pull requests.

## License

This package is licensed under the MIT License. See LICENSE file for details.

## Citation

If you use this package in your research, please cite:

```bibtex
@Manual{LPPLS,
  title = {LPPLS: Log Periodic Power Law Singularity Model for Financial Bubbles},
  author = {Your Name},
  year = {2024},
  note = {R package version 0.1.0},
  url = {https://github.com/yourusername/LPPLS}
}
```
