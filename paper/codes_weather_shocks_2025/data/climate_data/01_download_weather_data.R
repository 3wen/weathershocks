# This files downloads raw climatic measures from whether stations into R.

# IMPORTANT
# Note (2025): the "CliFlo" service is no longer available.
# The replacement tool is DataHub: https://data.niwa.co.nz/collections/all
# The following codes no longer work and should be adapted.

library(tidyverse)
library(lubridate)
library(stringr)
library(RCurl)
library(httr)


source("functions_weather_stations.R")

# Settings----

# We will download data for two metrics from NIWA, using CliFlo.

# 00	MTHLY: TOTAL RAIN	Mthly_Stats: Total Rainfall
# 66 	MTHLY: MEAN DEFICIT (WBAL) 	Mthly_Stats: Mean Deficit Of Soil Moisture (Wbal Awc=150mm)
codes <- data.frame(
  code = c("00", "66"),
  variable = c("rainfall", "sm_mean")
)

# Information about the stations

# We downloaded, manually, the list of stations for each region, for the desired
# metrics.
# The station listings are in separate folder for the different metrics:
# - `data/climate_data/stations_00`: for total rainfall;
# - `data/climate_data/stations_66`: for Mean Deficit Of Soil Moisture.

# We format the station listing by metric, using the `stations_to_df()` function.

# Total Rainfall
stations_00 <- stations_to_df("stations_00")
# Mean Deficit Of Soil Moisture
stations_66 <- stations_to_df("stations_66")

stations_all <- list(
  stations_00 = stations_00,
  stations_66 = stations_66
)

save(stations_all, file = "stations_all.rda")

# Download Data----

# A (free) account is needed to download the data
lien <- "https://cliflo.niwa.co.nz/pls/niwp/wa.logindb"

# Login ID and password from http://cliflo.niwa.co.nz/
user_name <- "" # Set your own username here
password <- "" # Set your password here

curl = getCurlHandle()
curlSetOpt(cookiejar="cookies.txt",  useragent = "Mozilla/5.0", followlocation = TRUE, curl=curl)
login <- postForm(
  uri = lien, .opts = curlOptions(followlocation=TRUE), curl = curl,
  cusername=user_name,
  cpwd=password,
  submit="login",
  ispopup="false",
  ispopup="false"
)

# Looping over years to download the weather data, tidy them up, and save the 
# resulting table in `./data_*/year_ii.rda` where * is replaced by the 
# code of the metric (e.g., 00 for total rainfall) and `ii` is replaced by an
# index (the number of stations can be too high, multiple calls are made to
# the API, resulting in multiple temporary saved files).
for(a in 1980:2015) get_data_year(a, "stations_00")
for(a in 1980:2015) get_data_year(a, "stations_66")

for(a in 2015:2017) get_data_year(a, "stations_66")
for(a in 2015:2017) get_data_year(a, "stations_00")