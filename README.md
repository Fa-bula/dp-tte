# Differential Privacy for time-to-event endpoints

This R project is designed to explore and calculate differential private time-to-event endpoints.

## Project Structure
- **`DP.Rproj`**: The RStudio project file, used to manage the project environment and settings in RStudio.
- **`find_global_sensitivity.R`**: Script for calculating global sensitivity.
- **`functions.R`**: A script that defines functions for time-to-event endpoints calculation (KM estimates, median survival time etc.).
- **`generate_figures.R`**: A script for generating visualizations and plots. These figures are meant to assist with the interpretation of results.

## Getting Started

### Prerequisites

Ensure that you have the following installed:
- **R** (version 4.0 or later)
- **RStudio** (optional but recommended for running `DP.Rproj`)

You will also need to install the following R packages:
```R
install.packages(c("survival", "DPpack"))
```
