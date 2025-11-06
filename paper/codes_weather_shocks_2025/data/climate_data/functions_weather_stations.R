# Required packages:
# stringr, RCurl, httr,

# Download Stations----

#' Help function to download data from The National Climate Database
#' 
#' @description
#' Sends a form to the Clifo Climate Database to retrieve data
#' given station ids and years
#' 
#' @param stations Character vector of stations id.
#' @param year Year for which we want to download data.
#' @param prm1 Code of the variable (see in 00_stations.R for codes)
#' stations <- c("18468", "15876") ; year <- 1980
#' 
data_stations <- function(stations, year, prm1) {
  lien_requete <- "https://cliflo.niwa.co.nz/pls/niwp/wgenf.genform1_proc"
  
  p_stations <- str_c(stations, collapse=",")
  postForm(
    uri = lien_requete, 
    .opts = curlOptions(followlocation=FALSE, verbose = TRUE), 
    curl = curl,
    cselect = "wgenf.genform1?fset=defdtype",
    prm1 = prm1,
    dt1 = "ms,1",
    auswahl = "wgenf.genform1?fset=defagent",
    agents = p_stations,
    dateauswahl = "wgenf.genform1?fset=defdate",
    date1_1 = year,
    date1_2 = "12",
    date1_3 = "15",
    date1_4 = "00",
    date2_1 = year,
    date2_2 = "12",
    date2_3 = "16",
    date2_4 = "00",
    formatselection = "wgenf.genform1?fset=deffmt",
    TSselection = "UTC",
    dateformat = "0",
    Splitdate = "N",
    mimeselection = "csvplain",
    cstn_id = "A",
    cdata_order = "DSC",
    submit_sq = "Send+Query"
  )
}

#' Identify all the stations that operate in a given year.
#' 
#' @param year Year of interest (integer).
#' @param stations Name of the tibble (e.g., "stations_00")
#' 
#' @returns A vector of strings with the IDs (`agent`) of the weather stations.
#' @examples
#' year <- 2015 ; stations <-  "stations_02"
#' stations_year(year = year, stations = stations)
#' 
stations_year <- function(year, stations) {
  stations <- get(stations)
  stations |> 
    filter(year(start_date) <= !!year, year(end_date) >= !!year) |> 
    pull("agent") |> 
    unique()
}

#' Splits a vector in subsets of desired length
#' 
#' @param x Vector of observations to split
#' @param n Maximum length of the subsets.
#' @details
#' The last subset may be smaller.
#' 
split_max_groupsize <- function(x, n) {
  split(x, gl(ceiling(length(x) / n), n, length(x)))
}

#' Obtain data for the stations, from NIWA, for a specific year. (no longer works)
#' 
#' @param year Year (int).
#' @param stations Name of the weather station tibble (string).
#' 
#' @description
#' This function downloads the data for one metric for multiple stations, in a
#' given year. It then saves them in a .rda file in a folder:
#' `./data_*/raw/year_ii.rda`, where `*` is extracted from the name of the
#' weather station listing tibble (e.g., 00 for total rainfall) and `ii` is a
#' index because the station IDs are split into smaller chunks to be able to
#' correctly call the API.
#' The raw data are then tidied up and saved in `./data_*/year_ii.rda`.
#' 
#' year <- 1980 ; stations <- "stations_00"
get_data_year <- function(year, stations) {
  
  stations_name <- stations
  stations <- get(stations_name) # get the tibble of that name
  prm1 <- str_split(stations_name, "_")[[1]][2] # variable code
  # Identify the stations that operate that year (IDs)
  stations_cour <- stations_year(year = year, stations = stations_name)
  # Split the vector of IDs in subsets of 50.
  a_parcourir <- split_max_groupsize(stations_cour, 50)
  # i <- 1
  for (i in 1:length(a_parcourir)) {
    # Download weather data
    df_tmp <- data_stations(stations = a_parcourir[[i]], year = year, prm1 = prm1)
    if(!dir.exists(str_c("data_", prm1))) 
      dir.create(str_c("data_", prm1))
    if(!dir.exists(str_c("data_", prm1, "/raw"))) 
      dir.create(str_c("data_", prm1, "/raw"))
    
    save(
      df_tmp, 
      file = str_c("data_", prm1, "/raw/", annee, "_", sprintf("%02s", i), ".rda")
    )
    # Tidy the results
    res <- tidy_weather_data(df_tmp)
    # And export them
    save(
      res, 
      file = str_c("data_", prm1, "/", annee, "_", sprintf("%02s", i), ".rda")
    )
    rm(df_tmp, res)
  }
}

# Weather Station Information----

# Functions to get information about weather stations, such as operating dates.

#' Import weather data from a CSV file (helper function for `stations_to_df()`).
#' 
#' @param file Full path to the CSV file to import.
#' @returns A tibble with the data:
#" * `agent`: the identifier of the weather station.
#" * `network`: network.
#" * `start_date`: Start date of the weather station.
#" * `end_date`: End date of the weather station.
#" * `pct_complete`: Percentage of days with observations.
#" * `name`: Name of the weather station.
#" * `lat`: Latitude of the weather station (in degrees).
#" * `long`: Longitude of the weather station (in degrees).
#' @md
#' 
import_station_info <- function(file) {
  
  df <- read.csv(file, skip = 6, sep = "\t")
  df <- df[-which(str_detect(df$Agent, "Numbe")), ]
  df <- as_tibble(df)
  
  df <- 
    df |> 
    rename(
      agent = Agent,
      network = Network,
      start_date = Start_Date,
      end_date = End_Date,
      pct_complete = Percent_Complete,
      name = Name,
      lat = `Lat.dec_deg.`,
      long = `Long.dec_deg.`
    ) |> 
    mutate(
      start_date = dmy(start_date, locale = "en_us"),
      end_date = dmy(end_date, locale = "en_us")
    )
  
  df
}

#' Import weather station informations from all the files contained in a folder.
#' 
#' @param folder Name of the folder tga contains the weather data (string).
#' @param save If `TRUE` (default), the data table is saved as an rda file in 
#'  the current working directory (which is assumed to be `data/climate_data`).
#'  The file name is set as the name of the folder that contains the imported
#'  data.
#' 
#' @returns A tibble with the information about the stations that are listed in
#' the folder:
#' * `agent`: ID of the weather station
#' * `network`: Network
#' * `start_date`: Start date of the weather station.
#' * `end_date`: End date of the weather station.
#' * `pct_complete`: Percentage of completeness of the weather station data.
#' * `name`: Name of the weather station.
#' * `lat`: Latitude (degree) of the weather station.
#' * `long`: Longitude (degree) of the weather station.
#' 
#' @examples
#' stations_66 <- stations_to_df("stations_66")
#' 
stations_to_df <- function(folder, save = TRUE) {
  
  # The files to loop over
  N <- list.files(folder, full.names = TRUE)
  
  stations <- lapply(N, import_station_info)
  stations <- bind_rows(stations)
  stations <- unique(stations)
  
  # Remove stations that closed prior to 1980
  stations <- 
    stations |> 
    filter(year(end_date) >= 1980)
  save(stations, file = str_c(folder, ".rda"))
  
  stations
}

# Tidy Weather Data from Weather Stations----


#' Tidy the downloaded data from the result obtained after a POST request
#' @returns A tibble with the information received.
#' @param df_tmp
#' 
tidy_weather_data <- function(df_tmp) {
  df_tmp <- str_split(df_tmp, "\n")[[1]]
  
  if (any(str_detect(df_tmp, "No rows\\? See this help link"))) {
    # Nothing to return
    res <- NULL
  } else {
    ind_deb <- which(str_detect(df_tmp, "^Station,Mon-YYYY"))
    ind_fin <- which(str_detect(df_tmp, "^UserName is = "))-2
    df_tmp <- df_tmp[ind_deb:ind_fin]
    df_tmp <- str_replace(df_tmp, "\r$", "")
    
    noms_variables <- str_split(df_tmp[1], ",")[[1]]
    
    head(do.call("rbind", str_split(df_tmp[-1], ",")))
    
    res <- do.call("rbind", str_split(df_tmp[-1], ",")) |> 
      data.frame() |> 
      as_tibble() |> 
      select(-X6)
    colnames(res) <- noms_variables[-6]
  }
  
  res
}


# Handle Weather Data----

#' Load weather station data for a single metric
#' 
#' @param folder  Folder containing data of a given weather measure
#' @returns 
#' dossier <- "data_66"
load_data_metrics <- function(folder) {
  
  N <- list.files(folder, full.names = TRUE)
  N <- N[str_detect(N, "rda$")]
  # Loop over each file (one file per year)
  df <- lapply(N, function(x) {
    load(x)
    res
  })
  
  df <- 
    df |> bind_rows()
  
  # Keep only observations from stations with at least 20 years of recording
  stations_to_keep <- 
    df |> 
    count(Station) |> 
    filter(n >= 12 * 20) |>  
    pull("Station")
  
  df |> 
    filter(Station %in% stations_to_keep)
}


#' Reshape Metrics
#' 
#' @param code Code number of the metric (e.g., "00" for rainfall)
#' 
#' @returns A list of two elements:
#' * `weighted_values`: weighted values of the measure by month and year
#' * `weighted_values_q`: weighted values of the measure by month
#' @details
#' Weights are computed as the share of agricultural gdp in the region
#' 
#' code <- "66"
shape_metric <- function(code) {
  df <- get(str_c("df_", code))
  # Focus on the stations that give values for that metric
  stations_mesure <- stations_all[[str_c("stations_", code)]]$agent
  df <- 
    df |> 
    rename(
      agent = Station, date = `Mon-YYYY(local)`,
      code = `Stat Code`,
      value = `Stat Value `
    ) |> 
    mutate(
      value = as.numeric(value),
      year = str_sub(date, -4),
      month = str_sub(date, 1, 3),
      code = as.character(code),
      agent = as.character(agent)
    ) |> 
    left_join(codes) |> 
    left_join(
      stations |> 
        filter(agent %in% stations_mesure) |> 
        select(agent, long, lat, region) |> 
        mutate(agent = as.character(agent))
    ) |> 
    select(-`Day of Extr`, -code, -date) |> 
    mutate(year = as.numeric(year))
  
  # Weights
  cultures <- read_excel("../economic_data/matrix_pond_agriculture.xls", sheet="RNA434201_20150707_091730_92", skip = 2)
  colnames(cultures)[1] <- "year"
  cultures <- cultures[-1,]
  cultures <- cultures[, seq(1, which(colnames(cultures) == "New Zealand")-1)]
  cultures <- cultures[seq(1, which(str_detect(cultures$year, "Table information:"))-1),]
  cultures <- apply(cultures, 2, as.numeric) %>% data.frame() %>% as_tibble()
  cultures <-
    cultures |> 
    gather(region, gdp, -year) |> 
    mutate(year = as.numeric(year))
  
  cultures <- 
    cultures |> 
    mutate(
      region = str_replace_all(region, "\\.", " "),
      region = ifelse(region == "Hawke s Bay", yes = "Hawke's Bay", no = region),
      region = ifelse(region == "Manawatu Wanganui", yes = "Manawatu-Wanganui", no = region),
      region = ifelse(region == "Tasman Nelson", yes = "Tasman/Nelson", no = region)
    )
  
  
  cultures_weights <- 
    cultures |> 
    filter(region %in% df$region) |> 
    group_by(year) |> 
    mutate(weight = gdp / sum(gdp)) |> 
    select(-gdp) |> 
    mutate(region = as.character(region))
  
  # Average of the last 5 years
  years_weights <- unique(cultures_weights$year) |> sort()
  years_weights <- years_weights[(length(years_weights)-4):length(years_weights)]
  cultures_weights_last <- cultures_weights |> 
    filter(year %in% years_weights) |> 
    group_by(region) |> 
    summarise(weight = mean(weight))
  
  # Missing years
  a_completer <- unique(df$year)[!unique(df$year) %in% cultures_weights$year]
  a_completer <- a_completer[a_completer >= 2015]
  
  # Region names
  regions_cult <- unique(cultures_weights$region)
  
  cultures_weights <- 
    cultures_weights |> 
    bind_rows(
      expand.grid(year = a_completer, region = regions_cult) |> 
        left_join(cultures_weights_last)
    )
  
  df <- 
    df |> 
    left_join(cultures_weights)
  
  df |> 
    left_join(quarters) |> 
    mutate(month = factor(month, levels = month.abb))
  
}

# Build Weather Metrics----

# The following function (`valeur_c()`) is used afterwards to create an index 
# of drought. It is from the following paper:
# Narasimhan, B., & Srinivasan, R. (2005). 
# Development and evaluation of Soil Moisture Deficit Index (SMDI) 
# and Evapotranspiration Deficit Index (ETDI) for agricultural drought monitoring.
# Agricultural and Forest Meteorology, 133(1), 69-88.

valeur_c <- function(t) -25 / (25 * t + 25)

#' Compute Weather/Climate Variables
#' 
#' @param code_variable Code of the weather variable (e.g., "00" for rainfall).
#' @param stations_mean (Logical) Computation of the SMDI at the station level
#'  (if `TRUE`) or at the region level (if `FALSE`, default value).
#' @param val_T Temporal scaling or smoothing constant (default to `1`).
#' 
#' code_variable <- "66" ; stations_mean = FALSE
climate_variables <- function(code_variable, 
                              stations_mean = FALSE, 
                              val_T = 1) {
  # Retrieve data
  df <- shape_metric(code_variable)
  # Remove observation from weather stations too close to the coastal area
  df <- df |> filter(!is.na(region))
  
  # Perform region-wise aggregations: 
  # the value from monthly data at station are averaged at the 
  # region x year x month level
  df <-
    df |> 
    group_by(variable, region, weight, year, quarter, month) |> 
    summarize(value = mean(value, na.rm = TRUE)) |> 
    ungroup() |> 
    arrange(year, month)
  
  # Long term statistics for each region
  df_lt <- 
    df |> 
    group_by(variable, region, month) |> 
    summarise(
      value_mean = mean(value),
      value_med = median(value),
      value_min = min(value),
      value_max = max(value),
      value_sd = sd(value)
    )
  
  # Standardization
  df <- 
    df |> 
    left_join(df_lt) |> 
    mutate(
      value_s = ifelse(
        value <= value_med,
        yes = (value - value_med)/(value_med - value_min) * 100,
        no = (value - value_med)/(value_max - value_med) * 100
      ),
      value_demeaned = value / value_mean,
      value_pct = 100*(value - value_mean) / value_mean,
      value_norm = (value - value_mean) / value_sd
    )
  
  # Index creation
  # SMDI
  # SD_{ij} = \frac{SW_{ij} - Med(SW_{j})}{Med(SW_{j})}, j = month, i = year
  # Init:
  # SMDI_{1} = SD_1 / 50
  # Reason:
  # SMDI_{j} = 0.5 * SMDI_{j-1} + \frac{SD_j}/50
  
  # Help function to compute the index for a station or a region
  # @x: a station id if stations_mean, a region id otherwise
  # x <- unique(df$region)[1]
  calcul_smdi <- function(x) {
    
    if (stations_mean) {
      df_tmp <- 
        df |> 
        filter(agent == x)
    } else {
      df_tmp <- 
        df |>  
        filter(region == x)
    }
    
    index <- rep(NA, nrow(df_tmp))
    # Init the recursive formula
    index[1] <- df_tmp$value_s[1] / (25 * val_T + 25)
    # i <- 1
    for (i in 2:nrow(df_tmp)) {
      index[i] <- df_tmp$value_s[i]/(25*val_T + 25)  - valeur_c(val_T)*index[i-1]
    }
    df_tmp$index <- index
    df_tmp
  }
  
  if (stations_mean) {
    df <- lapply(unique(df$agent), calcul_smdi) |> 
      bind_rows()
  } else {
    df <- lapply(unique(df$region), calcul_smdi) |>  
      bind_rows()
  }
  
  
  # SPI index for each region
  calcul_spi <- function(x) {
    
    if (stations_mean) {
      df_tmp <- 
        df |> 
        filter(agent == x)
    } else {
      df_tmp <- 
        df |> 
        filter(region == x)
    }
    
    
    # Build a ts object with frequency = 12
    df_tmp <- df_tmp |> arrange(year, month)
    start_year  <- df_tmp$year[1]
    start_month <- df_tmp$month[1]
    
    smdi_val <- ts(
      df_tmp$value, start = c(start_year, start_month), frequency = 12
    )
    spei_fit <- SPEI::spi(data = smdi_val, scale = 3, verbose = FALSE)
    spei_val <- as.numeric(spei_fit$fitted)
    
    df_tmp_spi <- df_tmp |> 
      select(year, month) |> 
      mutate(
        spi = spei_val
      )
    
    if (stations_mean) {
      df_tmp_spi <- 
        df_tmp_spi |> 
        mutate(agent = x)
    } else {
      df_tmp_spi <- 
        df_tmp_spi |> 
        mutate(region = x)
    }
    
    df_tmp_spi
  }
  
  
  if (stations_mean) {
    df_smpi <- lapply(unique(df$agent), calcul_spi) |> 
      bind_rows() |> 
      as_tibble()
  } else {
    df_smpi <- lapply(unique(df$region), calcul_spi) |> 
      bind_rows() |> 
      as_tibble()
  }
  
  df <- 
    df |> 
    left_join(df_smpi)
  
  # Removing data where no weight is available
  df <- df |> filter(!is.na(weight))
  
  # Weighted values
  df <- 
    df |> 
    ungroup() |> 
    mutate(
      value = value * weight,
      value_spi = spi * weight,
      index = index * weight,
      value_demeaned = value_demeaned * weight,
      value_pct = value_pct * weight,
      value_norm = value_norm * weight
    ) |> 
    group_by(variable, year, quarter, month) |> 
    summarize(
      value = sum(value),
      spi = sum(value_spi),
      index = sum(index),
      value_demeaned = sum(value_demeaned),
      value_pct = sum(value_pct),
      value_norm = sum(value_norm)
    ) |> 
    ungroup()
  
  
  df
}
