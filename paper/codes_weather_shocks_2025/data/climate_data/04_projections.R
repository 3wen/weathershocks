# Climate Projections
# In this script, we estimate the average growth rate of the standard
# error of quarterly precipitation under different climate scenarios.

# In the paper, we used monthly precipitation (pr) 
# simulated by the NCAR Community Climate System Model, version 4 (CCSM4) 
# under four different scenarios: RCP 2.5, RCP 4.5, RCP 6.0, and RCP 8.5.
# The data can be downloaded here: https://aims2.llnl.gov/search/cmip5/
#
# The name of the files are build as follows:
# `pr_Amon_CCSM4_rcp60_r1i1p1_200601-210012.nc`
#   * pr: precipitation, in kg/m2/s
#   * Amon: Atmospheric variable, monthly frequency
#   * CCSM4: Community Climate System Model version 4 (model)
#   * rcp60: Experiment, here, for RCP 6.0 scenario
#   * r1i1p1: Ensemble member = realization 1, initialization 1, physics 1
#   * 200601-210012: Time period, from Jan. 2006 to Dec. 2100


library(tidyverse)
library(terra)
library(sf)
library(exactextractr)
library(slider) # for rolling windows


colour_scenarios <- c(
  "RCP 2.6" = "#27377A",
  "RCP 4.5" = "#709FC8",
  "RCP 6.0" = "#DE632B",
  "RCP 8.5" = "#CD1020"
)


load("../map/nz_df_regions.rda")
map_country <- nz_df_regions


# Load NCDF Data----

#' Compute area-weighted regional means of precipitation from CMIP5 NetCDF files
#' 
#' @description
#' This function reads monthly precipitation data (in kg/m2/s) from a CMIP5
#' NetCDF file and computes area-weighted averages for each region of a country.
#' The results are expressed in millimeters per month.
#' 
#' @param nc_path Path to the NetCDF file containing the precipitation data.
#' @param map_country An `sf` object defining the regions of the country.
#' @param region_id Name of the column in `map_country` identifying the regions.
#' @param start_date Optional. A `Date` object indicating the first date to
#'   include in the output. If `NULL`, all available dates are kept.
#' 
#' @returns A tibble with the following columns:
#' * `region_id`: Region identifier
#' * `date`: Date (corresponding to a month, even if given as a Date object).
#' * `pr_mean`: Area-weighted mean precipitation (in millimeters).
#'   
get_netcfd_ccsm <- function(nc_path, 
                            map_country, 
                            region_id,
                            start_date = NULL) {
  pr <- rast(nc_path)
  # Extract time
  dates <- terra::time(pr)
  
  if (!is.null(start_date)) {
    idx <- which(dates >= start_date)
    pr <- pr[[idx]]
    dates <- dates[idx]
    time(pr) <- dates
  }
  
  # Focus on the country of interest
  # Reproject shapefile to match NetCDF raster CRS if necessary
  map_country <- st_transform(map_country, crs(pr))
  # Cropping
  pr_country <- crop(pr, vect(map_country))
  # Compute cell areas (used as weights)
  cell_area <- cellSize(pr_country, unit = "km")
  
  # Area-weighted means for each region
  res_mat <- exact_extract(
    pr,
    map_country,
    fun = "weighted_mean",
    weights = cell_area,
    progress = FALSE
  )
  
  # Bind region IDs
  res_mat <- dplyr::bind_cols(
    map_country |> 
      st_drop_geometry() |> 
      dplyr::select(!!region_id),
    res_mat
  )
  
  # Tidy to long format, with dates
  names(res_mat)[-1] <- as.character(dates)
  
  region_daily_pr <- res_mat |>
    tidyr::pivot_longer(
      cols = -!!region_id,
      names_to = "date",
      values_to = "pr_mean"
    ) |>
    dplyr::mutate(date = as.Date(date)) |> 
    # Express in mm
    mutate(pr_mean = pr_mean * 86400 * 30) |> 
    rename(region_id = !!region_id)
  
  region_daily_pr
}



precip_hist <- get_netcfd_ccsm(
  nc_path = "projections//pr_Amon_CCSM4_historical_r1i1p1_185001-200512.nc", 
  map_country = map_country, 
  region_id = "region", 
  start_date = as.Date("1960-01-01")
)

precip_rcp26 <- get_netcfd_ccsm(
  nc_path = "projections//pr_Amon_CCSM4_rcp26_r1i1p1_200601-210012.nc", 
  map_country = map_country, 
  region_id = "region", 
  start_date = NULL
)
precip_rcp45 <- get_netcfd_ccsm(
  nc_path = "projections//pr_Amon_CCSM4_rcp45_r1i1p1_200601-210012.nc", 
  map_country = map_country, 
  region_id = "region", 
  start_date = NULL
)
precip_rcp60 <- get_netcfd_ccsm(
  nc_path = "projections//pr_Amon_CCSM4_rcp60_r1i1p1_200601-210012.nc", 
  map_country = map_country, 
  region_id = "region", 
  start_date = NULL
)
precip_rcp85 <- get_netcfd_ccsm(
  nc_path = "projections//pr_Amon_CCSM4_rcp85_r1i1p1_200601-210012.nc", 
  map_country = map_country, 
  region_id = "region", 
  start_date = NULL
)


# Regional Weights Depending on Agricultural Production----

# Weights
cultures <- readxl::read_excel(
  path = "../economic_data/matrix_pond_agriculture.xls", 
  sheet="RNA434201_20150707_091730_92", skip = 2
)
colnames(cultures)[1] <- "year"
cultures <- cultures[-1,]
cultures <- cultures[, seq(1, which(colnames(cultures) == "New Zealand")-1)]
cultures <- cultures[seq(1, which(str_detect(cultures$year, "Table information:"))-1),]
cultures <- apply(cultures, 2, as.numeric) |> data.frame() |> as_tibble()
cultures <-
  cultures |> 
  pivot_longer(cols = -year, names_to = "region", values_to = "gdp") |> 
  mutate(
    year = as.numeric(year),
    region = str_replace_all(region, "\\.", " "),
    region = ifelse(region == "Hawke s Bay", yes = "Hawke's Bay", no = region),
    region = ifelse(region == "Manawatu Wanganui", yes = "Manawatu-Wanganui", no = region),
    region = ifelse(region == "Tasman Nelson", yes = "Tasman/Nelson", no = region)
  )

# Check if some regions are not present 
setdiff(cultures$region, map_country$region)

cultures_weights <- 
  cultures |> 
  group_by(region) |> 
  summarise(gdp = mean(gdp)) |> 
  mutate(weight = gdp / sum(gdp)) |> 
  select(-gdp) |> 
  rename(region_id = region)

# National Aggregation----

#' Computes quarterly aggregation at the national level of precipitation.
#' 
#' @param x Tibble with monthly precipitation (see details).
#' @param regional_weights Tibble witht the regional weights to use.
#' 
#' @details
#' The tibble `x` must contain the following columns:
#' * `region_id`: Region identifier.
#' * `date`: Date (corresponding to a month, even if given as a Date object).
#' * `pr_mean`: Area-weighted mean precipitation (in millimeters).
#' The tibble `regional_weights` must contain the following columns:
#' * `region_id`: Region identifier.
#' * `weight`: The weight to use for national aggregation.
#' 
national_quaterly_aggreg <- function(x, regional_weights) {
  x |> 
    left_join(regional_weights, by = c("region_id")) |> 
    group_by(date) |> 
    summarise(
      pr_mean = sum(pr_mean * weight, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    mutate(
      year = year(date),
      quarter = quarter(date)
    ) |> 
    group_by(year, quarter) |> 
    summarise(
      pr_mean = sum(pr_mean, na.rm = TRUE),
      .groups = "drop"
    )
}

precip_nat_q <- list(
  `Historical` = precip_hist,
  `RCP 2.6` = precip_rcp26,
  `RCP 4.5` = precip_rcp45,
  `RCP 6.0` = precip_rcp60,
  `RCP 8.5` = precip_rcp85
) |> 
  map(
    .f = ~national_quaterly_aggreg(x = .x, regional_weights = cultures_weights)
  )


# Change in the Variance of the Weather----

## Helper Functions----

#' Make a Date at the end of each quarter
#' @param y Year (integer).
#' @param q Quarter (integer).
#' @returns The date at the end of the quarter.
#' 
as_quarter_end <- function(y, q) {
  # first day of the quarter
  yq <- yq(paste(y, q))
  # last day of the quarter
  (yq %m+% months(3)) - days(1)
}

#' Build a continuous series for one scenario
#' 
#' @description
#' concatenates Historical data (up to 2005Q4) and data from a scenario (from 
#' 2006Q1).
#' 
#' @param hist_tb Tibble with precipitation data for the historical period.
#' @param rcp_tb Tibble with precipitation data for a scenario.
bind_hist_and_rcp <- function(hist_tb, 
                              rcp_tb) {
  bind_rows(
    hist_tb,
    rcp_tb
  ) |> 
    arrange(year, quarter) |> 
    mutate(date = as_quarter_end(year, quarter))
}


#' Fit AR(1) in a window and return sd of residuals
#' 
#' @param x Series with quarterly precipitation.
#' 
#' @returns Returns the standard deviation of the residuals of the AR(1), or 
#' `NA` if the number of observation is lower than 102 (window smaller than 
#' that used in the DSGE model).
sd_resid_ar1 <- function(x) {
  if (length(x) < 102) {
    return(NA)
  } else {
    fit <- stats::arima(x, order = c(1, 0, 0), include.mean = TRUE)
    sd_resid <- stats::sd(stats::residuals(fit), na.rm = TRUE)
    return(sd_resid)
  }
}


#' Compute rolling standard deviations of AR(1) residuals for scenario scaling
#' 
#' @description
#' This function computes the time-varying volatility of precipitation by
#' estimating, within each rolling window, an AR(1) model and extracting the 
#' standard deviation of its residuals. We will use the resulting series to
#' derive scenario multipliers (RCP-based scaling factors) for weather shock 
#' variances.
#'
#' @param tb A tibble (ordered by increasing dates) with precipitation for 
#' historical values and projected values under a climate scenario. The tibble 
#' must contain at least the following column:
#'  * `pr_mean`: precipitation values.
#' @param reg_start
#' @param reg_end
#' 
#' @return The initial tibble with an addition column:
#'  * `sd_ar1`: the rolling-window standard deviation of AR(1) residuals.
#' 
sd_resid_scenario <- function(tb) {
  
  # rolling window std of AR(1) residuals (window size = 102 quarters)
  # This size corresponds to the size of our sample in the DSGE model.
  roll_sd <- slide(
    .x = tb$pr_mean, 
    .f = ~ sd_resid_ar1(.x), 
    .before = 102, 
    .complete = FALSE # we return NAs if incomplete
  )
  
  tb |> 
    mutate(sd_ar1 = unlist(roll_sd)) |> 
    filter(!is.na(sd_ar1))
  
}

## Rolling Windows----

# Let us compute the standard deviations of the AR(1) residuals on a rolling
# window of 25.5 years for each scenario.
tb_sd_scenarios <- map(
  precip_nat_q[names(precip_nat_q) != "Historical"], 
  ~sd_resid_scenario(
    tb = bind_hist_and_rcp(hist_tb = precip_nat_q$Historical, rcp_tb = .x)
  ), 
  .progress = TRUE
)

# Visualization of the estimates sd of the residuals computed on the rolling 
# windows.
p <- ggplot(
  data = list_rbind(tb_sd_scenarios, names_to = "scenario") |> 
    filter(year >= 2014),
  mapping = aes(x = date, y = sd_ar1, colour = scenario)
) +
  geom_line() +
  scale_colour_manual(NULL, values = colour_scenarios) +
  labs(x = NULL, y = "sd of the AR(1) residuals")

p

# source("../variables_names.R")
# ggsave(p + theme_paper(), file = "sd_scenarios.pdf", width = 7, height = 5)

## Growth Rate of the Standard Deviation----

#' Compute instantaneous, compound, and total growth rates of rolling residual 
#' volatility
#'
#' @description
#' This function summarizes the time trend in the rolling standard deviations 
#' of AR(1) residuals from the precipitation data under a scenario. 
#' It estimates how the volatility of the underlying process evolves over time 
#' by regressing the (log of) fitted standard deviations on time.
#'
#' @param tb_sd A tibble containing the estimated rolling standard deviations 
#'  of residuals from AR(1) models (as produced by `sd_resid_scenario()`). It 
#'  must include the following columns:
#'  * `year`: Calendar year.
#'  * `sd_ar1`: Standard deviation of AR(1) residuals.
#'  
#' @returns A tibble with three summary statistics:
#'   * `instant_growth`: Estimated instantaneous quarterly growth rate of 
#'      volatility (log-linear).
#'   * `compound_growth`: Equivalent compound quarterly growth rate, computed 
#'   as \eqn{e^{r} - 1}.
#'   * `tot_growth`: Total cumulative growth (%) over the entire sample period 
#'   since 2014.
#' 
compute_growth_stats <- function(tb_sd) {
  
  tb_sd <- tb_sd |> filter(year >= 2014) |> 
    mutate(t = row_number())
  
  q_total <- nrow(tb_sd)
  # regress ln(sd) on time
  fit <- lm(sd_ar1 ~ t, data = tb_sd)
  tb_sd$fitted <- fitted(fit)
  
  instant_growth <- coef(lm(log(fitted)~ 1 + t, data = tb_sd))[2]
  # Equivalently
  compound_growth <- exp(instant_growth) - 1
  tot_growth <- ((1 + compound_growth)^q_total - 1) * 100
  
  tibble(
    instant_growth = instant_growth,
    compound_growth = compound_growth,
    tot_growth = tot_growth
  )
}

# Quarterly rate of growth of the standard deviation of the weather measure
# and the corresponding average growth rate over the whole 1989–2100 period
growth_rates <- map(tb_sd_scenarios, compute_growth_stats) |> 
  bind_rows()

growth_rates
1+growth_rates$tot_growth/100
