# Inference in Regression Discontinuity Designs

This repository contains the code and data for the master's thesis *"Inference in Regression Discontinuity Designs"*. Below, you will find an overview of the repository structure and its contents.

## Repository Structure

- **`lib.R`**: This script loads all required R packages necessary to run the analyses.
- **`analysis.R`**: This script performs the analyses, including Monte Carlo simulations and the real data application. It reads data from the following directories:
  - `data/simulations_results/`: Contains the results of the Monte Carlo simulations.
  - `data/application/`: Contains the data used for the real data application.
- **`functions/`**: Contains all functions required for Monte Carlo simulations and graphical visualizations. The key scripts in this directory include:
  - `dgp.R`: Generates the data generating process (DGP).
  - `mrot.R`: Computes rule-of-thumb estimates for the smoothness constant.
  - `plots.R`: Handles the creation of plots.
  - `rd.R`: Estimates the sharp regression discontinuity (RD) local treatment effects.
  - `simulations/`: A subdirectory that contains all necessary scripts for running the Monte Carlo simulations.