First Version: May 2019
Updated Version: October 2025

This folder contains the codes and data needed to replicate the results of the article **Weather Shocks**.

The sources of the data are available in the online appendix of the article, as well as in the file `data/02_2_data_import.R`.

# Data processing

The folder untitled `data` contains the materials needed to manipulate the data used both in the VAR and the DSGE estimations. It contains two folders:

1. `map/`: this folder contains the codes needed to create a map of NZ using R

2. `climate_data/`: this folder contains the climate/weather data for New Zealand (source: NIWA) and the R codes to scrape them

	1. `climate_data.Rproj`: RStudio project file to launch first (this opens RStudio and set the working directory correctly).
	2. `functions_weather_stations.R`:  functions to download the weather data (no longer works, the data provider no longer exists) and to create the droight metric (SMDI).
	3. `01_download_weather_data.R`: script to download the weather station data (again, no longer works).
	4. `02_weather_metrics.R`: script to build the SMDI.
	5. `03_explanations-for-NGFS`: (optional script) script to illustrate, step-by-step, how to build the SMDI.
	6. `04_projection.R`: script to compute the average growth rate of the standard error of quarterly precipitation under different climate scenarios.


3. `economic_data/`: this folder contains the `Excel` file with all downloaded data


The folder `data` contains a RStudio project file, named `data.Rproj`. This file needs to be launched first, to initate the R session. Then, the following `R` files can be opened and executed:

1. `01_data_world_new_oecd`: code to download and gather data from OECD 
	**Update 2025**: the raw data are now in a Excel files: `economic_data/rest-world/`
2. `02_1_seasonality`: functions to remove seasonal component of time-series
3. `02_2_data_import`: gather all data to a single table, and then creates the variables used in the estimation
4. `variables_names`: useful functions and definitions for aesthetics.

# VAR estimation

To replicate the VAR estimation, you need to go to the folder untitled `var_estimation`. This folder contains a RStudio project file `estimation.Rproj` that need to be launched first to initate the `R` session.

Then, the estimation is done thanks to the code from the file `01_restricted_var.R`.

The folder `assets` contains some more code to compute the IRFs.

# DSGE estimation

The estimation of the DSGE model was performed using the [dynare](https://www.dynare.org/) package, in Matlab. Please use a version 4.5 (or above) as our model use Weibull priors which are only available on recent versions of Dynare.

The folder `dsge_estimation` containes the following files:

1. `RBC_q0.mod`: dynare code to load the estimated DSGE model at the posterior mode with no damage from weather shocks.
2. `RBC_q1.mod`: dynare code to load the estimated DSGE model at the posterior mean with contemporaneous effects of weather shocks in the damage function. This file also provides table 3, 4, 5 and 6 and figures 7, 9, 10, 11, 12.
3. `COMPARE.mod`: dynare code to compare marginal densities between models using Laplace approximation given in table 2 of the paper.
4. `dataNZ_cubic_trend.m`: MATLAB script that provides data of the VAR's model detrended using a quadratic trend.
5. `climate_IRF.m`: MATLAB function that computes IRFs for 3 alternative calibrations expressed in percentage deviations from a benchmark model.
6. `RBC_q1/metropolis`: Folder containing the Metropolis-Hastings sampling for 4 MCMC parallel chains. This folder is necessary to compute the same posterior mean as in the paper.



