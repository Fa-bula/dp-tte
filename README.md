# Differential Privacy for time-to-event endpoints

This R project is designed to explore and calculate differential private time-to-event endpoints.

## Project Structure

-   **`DP.Rproj`**: The RStudio project file, used to manage the project environment and settings in RStudio.
-   **`find_global_sensitivity.R`**: Script for calculating global sensitivity.
-   **`functions.R`**: A script that defines functions for time-to-event endpoints calculation (KM estimates, median survival time etc.).
-   **`generate_results.R`**: A script for generating visualizations and tables. Those figures and tables are meant to assist with the interpretation of results.
-   **`generate_data.R`**: Function for simulating survival data, generating survival times and event indicators with optional censoring based on trial duration and censoring probability.
-   **`calculate_dp_values.R`**: Function for calculating differentially private values by applying the Laplace mechanism with varying privacy budgets, sensitivities, and repeated simulations, outputting a matrix of private values for analysis.
-   **`plot_results.R`**: Function that visualizes statistical metrics (median, quartiles) against a parameter (e.g., epsilon) with customizable reference lines and highlights.
-   **`create_summary_table.R`**: Function for summarizing differentially private results by calculating quartiles, medians, and relative ranges, and identifying the first epsilon value meeting a specified range threshold, returning a summary table and index.

## Getting Started

### Prerequisites

Ensure that you have the following installed: - **R** (version 4.0 or later) - **RStudio** (optional but recommended for running `DP.Rproj`)

You will also need to install the following R packages:

``` r
install.packages(c("survival", "DPpack"))
```
