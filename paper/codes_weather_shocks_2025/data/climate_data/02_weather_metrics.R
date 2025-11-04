# This file first identify the region associated with each station from its 
# (lon,lat) coordinates.
# Then, it associates a weight to give to each region each year according to 
# the share of agricultural GDP.

library(tidyverse)
library(sf)
library(readxl)

# Set to TRUE to save the graphs
if (!exists("save_graphs")) {
  save_graphs <- FALSE
}
# If save_graphs is TRUE, you will need to have this package installed:
# remotes::install_github("3wen/plotRTex")

source("functions_weather_stations.R")

# 1. Load Data----

# Map
load("../map/nz_df_regions.rda")
nz_df_regions

# Codes of weather variables
# 00	MTHLY: TOTAL RAIN	Mthly_Stats: Total Rainfall
# 66 	MTHLY: MEAN DEFICIT (WBAL) 	Mthly_Stats: Mean Deficit Of Soil Moisture (Wbal Awc=150mm)
codes <- data.frame(
  code = c("00", "66"),
  variable = c("rainfall", "sm_mean")
)

load("stations_all.rda")
stations <- stations_all |> 
  bind_rows() |> 
  select(agent, long, lat) |> 
  unique()

stations <- st_as_sf(
  stations, coords = c("long", "lat"), remove = FALSE, crs = 4326
)
stations <- st_transform(stations, st_crs(nz_df_regions))


## 1.1 Identify Stations in Each Region----

stations <- st_join(
  stations,
  nz_df_regions[, "region"],
  join = st_within,
  left = TRUE
)

ggplot() +
  geom_sf(
    data = stations,
    mapping = aes(colour = region),
    size = .1
  ) +
  geom_sf(
    data = nz_df_regions,
    fill = NA, colour = "black"
  )

## 1.2 Weather Station Data ----

df_00 <- load_data_metrics("data_00")
df_66 <- load_data_metrics("data_66")

### Number of Stations for Each Month----

#' Quick reshaping of the data
#' 
#' @param code Measure code (e.g. "00" for rainfall) (string)
#' 
quick_reshape_weather <- function(code) {
  
  df <- get(str_c("df_", code))
  stations_mesure <- stations_all[[str_c("stations_", code)]]$agent
  df |> 
    rename(
      agent = Station, date = `Mon-YYYY(local)`,
      code = `Stat Code`, value = `Stat Value `
    ) |> 
    mutate(
      value = as.numeric(value),
      year = str_sub(date, -4),
      month = str_sub(date, 1, 3),
      agent = as.character(agent),
      code = as.character(code)
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
}


df_plot_stations <- 
  quick_reshape_weather("00") |> 
  bind_rows(quick_reshape_weather("66"))

df_plot_stations <- 
  df_plot_stations |> 
  mutate(
    date = str_c(
      year,
      "-",
      sprintf("%02s", match(month, month.abb)),
      "-15"
    ),
    date = as.Date(date)
  )

df_plot_stations <- 
  df_plot_stations |> 
  filter(date < as.Date("2017-01-01"))


library(scales)
source("../variables_names.R")

if (save_graphs) {
  
  p <- 
    ggplot(
      data = 
        df_plot_stations |> 
        group_by(date, variable) |> 
        summarize(nb_obs = n()) |> 
        ungroup(),
      mapping = aes(x = date, y = nb_obs, linetype = variable, colour = variable)
    ) +
    geom_line() +
    scale_x_date(breaks = date_breaks("2 year"), labels = date_format("%Y")) +
    labs(x = NULL, y = NULL) +
    scale_linetype_discrete(
      "",
      labels = c("rainfall" = "Total Rainfall",
                 "sm_mean" = "Mean Deficit Of Soil Moisture")
    ) +
    scale_colour_discrete(
      "",
      labels = c("rainfall" = "Total Rainfall",
                 "sm_mean" = "Mean Deficit Of Soil Moisture")
    ) +
    theme_paper()
  
  p_1 <- p +
    theme(
      legend.direction = "vertical",
      legend.key.height	= unit(2, "line"),
      legend.key.width	= unit(3, "line")
    )
  
  width <- 20
  ggsave(
    p_1,
    file = "../../images/nb_stations_date.pdf", width = width, height=.5*width
  )
  
  # Only with Soil Moisture deficit
  p <- 
    ggplot(
      data = 
        df_plot_stations |> 
        filter(variable == "sm_mean") |> 
        group_by(date) |> 
        summarize(nb_obs = n()) |> 
        ungroup(),
      aes(x = date, y = nb_obs)) +
    geom_line(colour = "#0081BC", linewidth = 1.2) +
    scale_x_date(breaks = date_breaks("4 year"), labels = date_format("%Y")) +
    labs(x = NULL, y = NULL) +
    theme_paper()
  
  width <- 20
  # ggsave(
  #   p,
  #   file = "../../images/nb_stations_date_sm.pdf", width = width, height=.4*width
  # )
  
  # remotes::install_github("3wen/plotRTex")
  library(plotRTeX)
  #source("../export_tex.R")
  width <- 7
  ggplot2_to_pdf(
    plot = p, path = "../../images/",
    filename = "nb_stations_date_sm",
    width = width, height = width
  )
  
}

### Station Locations----

if (save_graphs) {
  p <- 
    ggplot() +
    geom_sf(
      data = nz_df_regions
    ) +
    geom_sf(
      data = stations,
      mapping = aes(colour = region),
      size = .1
    ) +
    scale_colour_discrete("") +
    scale_y_continuous(breaks = seq(-90, 90, by = 5)) +
    scale_x_continuous(breaks = seq(-180, 180, by = 5)) +
    theme(
      text = element_text(size = 10),
      panel.background = element_rect(fill = NA),
      panel.border = element_rect(fill = NA, colour = "grey60", linewidth = 1),
      axis.line = element_line(colour = "grey60"),
      axis.title = element_blank(),
      axis.text = element_text(), 
      legend.text = element_text(size = rel(1.4)),
      legend.title = element_text(size = rel(1.4)),
      legend.background = element_rect(),
      legend.key.height	= unit(2, "line"),
      legend.key.width	= unit(3, "line"),
      panel.spacing = unit(1, "lines"),
      panel.grid.major = element_line(colour = "grey90"), 
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      strip.text = element_text(size = rel(1.2))
    )
  p
  
  # width <- 20
  # ggsave(
  #   p,
  #   file = "../../images/stations_loc.pdf", width = width, height=0.6*width
  # )
  
  library(plotRTeX)
  #source("../export_tex.R")
  width <- 7
  ggplot2_to_pdf(
    plot = p, path = "../../images/",
    filename = "stations_loc",
    width = width, height = width
  )
  
}

### Value for Each Region----

quarters <- 
  tibble(
    month = c("Jan", "Feb", "Mar",
              "Apr", "May", "Jun",
              "Jul", "Aug", "Sep",
              "Oct", "Nov", "Dec"),
    quarter = rep(1:4, each = 3)
  )

# 2. Create Metrics----

# Soil Moisture
df_sm <- climate_variables("66")
# Rainfall
df_rainfall <- climate_variables("00")

# Gathering different climatic variables in a single data.frame
df_meteo_month <- 
  df_sm |> 
  bind_rows(df_rainfall) |> 
  left_join(codes)

# 3. Quarterly aggregation----
df_meteo <- 
  df_meteo_month |> 
  group_by(variable, year, quarter) |> 
  summarize(
    value = mean(value, na.rm = TRUE),
    spi = mean(spi, na.rm = TRUE),
    index = mean(index, na.rm = TRUE),
    value_demeaned = mean(value_demeaned, na.rm = TRUE),
    value_pct = mean(value_pct, na.rm = TRUE),
    value_norm = mean(value_norm, na.rm = TRUE)
  )

# 4. Export Results----


meteo <- 
  df_meteo |> 
  mutate(
    value = round(value, 4),
    index = round(index, 4),
    spi = round(spi, 4),
    valuedemeaned = round(value_demeaned, 4),
    valuepct = round(value_pct, 4),
    valuenorm = round(value_norm, 4)
  ) |> 
  select(-value_demeaned, -value_pct, -value_norm) |> 
  unite(val_ind, value, index, spi, valuedemeaned, valuepct, valuenorm, sep = "_") |> 
  spread(variable, val_ind) |> 
  gather(variable, val_ind, -year, -quarter) |> 
  separate(
    val_ind, 
    c("value", "index", "spi", "valuedemeaned", "valuepct", "valuenorm"), 
    sep = "_"
  ) |> 
  gather(type, value, value, index, spi, valuedemeaned, valuepct, valuenorm) |> 
  mutate(
    date = year + as.numeric(as.character(quarter))/4 - 0.25,
    value = as.numeric(value)
  ) |> 
  unite(var, variable, type, sep = "_") |> 
  spread(var, value) |> 
  rename(
    YEARS = date,
    rain_val = rainfall_valuedemeaned,
    soil_moist = sm_mean_valuedemeaned,
    soil_moist_norm = sm_mean_valuenorm,
    crfi = rainfall_index,
    smdi = sm_mean_index,
    spi = rainfall_spi
  ) |> 
  select(-sm_mean_spi)


save(meteo, file = "meteo.rda")