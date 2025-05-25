# Complete Package Build Script for LPPLS
# Run this script to create and build the complete package

library(usethis)
library(devtools)
library(roxygen2)

# Set working directory to package root
# setwd("path/to/your/LPPLSpackage")

cat("Building LPPLS Package...\n")
cat("========================\n\n")

# Step 1: Update documentation
cat("Step 1: Updating documentation...\n")
document()

# Step 2: Check package
cat("Step 2: Checking package...\n")
check_results <- check()

if (length(check_results$errors) > 0) {
  cat("ERRORS found during check:\n")
  print(check_results$errors)
} else {
  cat("No errors found!\n")
}

if (length(check_results$warnings) > 0) {
  cat("WARNINGS found during check:\n")
  print(check_results$warnings)
}

# Step 3: Run tests
cat("Step 3: Running tests...\n")
test_results <- test()

# Step 4: Install package
cat("Step 4: Installing package...\n")
install()

# Step 5: Create vignette (optional)
cat("Step 5: Creating vignette...\n")
use_vignette("lppls-introduction", "Introduction to LPPLS Analysis")

# Step 6: Build package
cat("Step 6: Building package...\n")
build()

cat("\nPackage build completed!\n")
cat("========================\n")

# Step 7: Quick functionality test
cat("Step 7: Testing basic functionality...\n")

# Test that package loads
library(LPPLS)

# Test basic functions
test_data <- generate_synthetic_data("bubble", n = 100)
prepared_data <- prepare_lppls_data(test_data)

cat("Data preparation: OK\n")

# Test validation
is_valid <- validate_lppls_data(prepared_data)
cat("Data validation:", ifelse(is_valid, "OK", "FAILED"), "\n")

# Test example function
try({
  example_result <- run_lppls_example(
    generate_bubble = TRUE,
    n_observations = 50,
    plot_results = FALSE
  )
  cat("Example function: OK\n")
}, silent = TRUE)

cat("\nPackage is ready for use!\n")
cat("========================\n")
cat("Try running:\n")
cat("library(LPPLS)\n")
cat("?LPPLS  # for package help\n")
cat("run_lppls_example()  # for a demo\n")

# Optional: Create package website
# if (requireNamespace("pkgdown", quietly = TRUE)) {
#   cat("Creating package website...\n")
#   pkgdown::build_site()
# }

# Summary of what was created
cat("\nPackage Structure Created:\n")
cat("==========================\n")
cat("R/\n")
cat("  ├── lppls_estimation.R     # Core LPPLS model functions\n")
cat("  ├── lppls_confidence.R     # Confidence indicator calculation\n")
cat("  ├── data_processing.R      # Data preparation and validation\n")
cat("  ├── visualization.R        # Plotting functions\n")
cat("  ├── examples.R            # Example and demo functions\n")
cat("  └── LPPLS-package.R       # Package documentation\n")
cat("\n")
cat("tests/\n")
cat("  └── testthat/\n")
cat("      ├── test-lppls-estimation.R  # Unit tests\n")
cat("      └── test-integration.R       # Integration tests\n")
cat("\n")
cat("Other files:\n")
cat("  ├── DESCRIPTION            # Package metadata\n")
cat("  ├── README.md             # Package documentation\n")
cat("  ├── LICENSE               # MIT license\n")
cat("  └── man/                  # Auto-generated help files\n")

cat("\nMain Functions Available:\n")
cat("=========================\n")
cat("• prepare_lppls_data()       - Prepare time series data\n")
cat("• lppls_estimate()           - Estimate LPPLS model\n")
cat("• compute_lppls_confidence() - Calculate confidence indicators\n")
cat("• plot_lppls_fit()          - Visualize model fits\n")
cat("• plot_confidence_indicators() - Plot confidence over time\n")
cat("• run_lppls_example()       - Complete example workflow\n")
cat("• quick_lppls_analysis()    - Simplified analysis function\n")

cat("\nNext Steps:\n")
cat("===========\n")
cat("1. Test the package with your own data\n")
cat("2. Customize parameters for your specific use case\n")
cat("3. Consider contributing improvements to the codebase\n")
cat("4. Cite the package if used in research\n")

