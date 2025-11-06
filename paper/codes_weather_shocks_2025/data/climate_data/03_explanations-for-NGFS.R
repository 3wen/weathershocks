# Weather Data for Session 1 of the NGFS workshop
# This code explains, step-by-step, how the SMDI is computed
# using the `climate_variables()` function from `functions_weather_stations.R`
# script.

library(tidyverse)
library(sf)
library(readxl)

source("functions_weather_stations.R")
source("../variables_names.R")

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


## 1.1. Identify Stations in Each Region----

stations <- st_join(
  stations,
  nz_df_regions[, "region"],
  join = st_within,
  left = TRUE
)

p_map_stations <- ggplot() +
  geom_sf(
    data = stations,
    mapping = aes(colour = region),
    size = .1
  ) +
  geom_sf(
    data = nz_df_regions,
    fill = NA, colour = "black"
  )

p_map_stations

# ggsave(p_map_stations, file = "stations-map.pdf", width = 8, height = 4.5)

## 1.2. Weather Station Data ----

df_66 <- load_data_metrics("data_66")

quarters <- 
  tibble(
    month = c("Jan", "Feb", "Mar",
              "Apr", "May", "Jun",
              "Jul", "Aug", "Sep",
              "Oct", "Nov", "Dec"),
    quarter = rep(1:4, each = 3)
  )

# 2. Creation of the SMDI index----

# "manual" run of climate_variables() from `functions_weather_stations.R`
code_variable <- "66"  # soild moisture deficit
stations_mean <- FALSE # If False, computation of the SMDI at the region level
                       # Otherwise, at the station level.

# Retrieve data: monthly observation at the station level
df <- shape_metric(code_variable)
# Remove observation from weather stations too close to the coastal area
df <- df |> filter(!is.na(region))

## 2.1. Region-Wise Aggregations----

# Perform region-wise aggregations: 
# the value from monthly data at station are averaged at the 
# region x year x month level
df <-
  df |> 
  group_by(variable, region, weight, year, quarter, month) |> 
  summarize(value = mean(value, na.rm = TRUE)) |> 
  ungroup() |> 
  arrange(year, month)

# We can have a look at a specific day, using a choropleth map
ggplot() +
  geom_sf(
    data = nz_df_regions |> 
      left_join(
        df |> filter(year == 2000, month == "Jan")
      ),
    mapping = aes(fill = value)
  ) +
  facet_wrap(~ month) +
  scale_fill_gradient("SMD", low = "lightblue", high = "darkblue")

## 2.2. Long-Term Statistics----

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

# Tibble to create a map with long-term statistics
tb_map_lt <- map(
  unique(df_lt$month), # iterate over months
  ~{
    nz_df_regions |> left_join(
      # Focus on current month
      df_lt |> filter(month == .x),
      by = "region"
    )
  }
) |> 
  list_rbind()

ggplot() +
  geom_sf(
    data = st_as_sf(tb_map_lt),
    mapping = aes(fill = value_med)
  ) +
  facet_wrap(~ month) +
  scale_fill_gradient("Med(SMD)", low = "lightblue", high = "darkblue")

## 2.3. SMDI----

# Standardization
df <- 
  df |> 
  left_join(df_lt) |> 
  mutate(
    # THIS is the variable of interest
    value_s = ifelse(
      value <= value_med,
      yes = (value - value_med)/(value_med - value_min) * 100,
      no = (value - value_med)/(value_max - value_med) * 100
    ),
    # Some by products not used afterwards
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

val_T <- 1

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

# We now have monthly SPEI values at the region level
# For example, for Auckland:
df |> filter(region == "Auckland") |> select(year, month, index)
ggplot(
  data = df |> filter(region == "Auckland") |> 
    mutate(years = year + as.numeric(month)/12),
  mapping = aes(x = years, y = index)
) +
  geom_line() +
  labs(x = NULL, y = "SMDI", title = "Auckland")

# Let us also plot the series for all regions.
# We will add a little bit of colours to reflect the values
# of the SMDI.

spei_bands <- tibble(
  ymin = c(-Inf, -2.0, -1.5, -1.0, -0.5,  0.5,  1.0,  1.5,  2.0),
  ymax = c(-2.0, -1.5, -1.0, -0.5,  0.5,  1.0,  1.5,  2.0,  Inf),
  label = c(
    "Extremely dry", "Severely dry", "Moderately dry", "Mildly dry",
    "Normal",
    "Mildly wet", "Moderately wet", "Severely wet", "Extremely wet"
  ),
  fill = c(
    "#67001f", "#b2182b", "#d6604d", "#f4a582", # dry shades
    "#f7f7f7",
    "#92c5de", "#4393c3", "#2166ac", "#053061" # wet shades
  )
) |> 
  mutate(label = factor(label, levels = label))

ggplot(
  data = df |> mutate(years = year + as.numeric(month)/12),
  mapping = aes(x = years, y = index)
) +
  geom_line() +
  facet_wrap(~ region) +
  labs(x = NULL, y = NULL, title = "SMDI") +
  geom_rect(
    data = spei_bands,
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = ymin, ymax = ymax,
      fill = label
    ),
    inherit.aes = FALSE,
    alpha = 0.25
  ) +
  scale_fill_manual(
    name = "Climate condition",
    values = setNames(spei_bands$fill, spei_bands$label)
  ) 


# Let us visualize on choropleth maps (one for each month), the SMDI values
# for 2013.

# Tibble for the map
tb_map_smdi <- map(
  unique(df$month), # iterate over months
  ~{
    nz_df_regions |> left_join(
      # Focus on current month, for 2013
      df |> filter(month == .x, year == 2013),
      by = "region"
    )
  }
) |> 
  list_rbind()

cols_smdi <- c(
  "#67001f", "#b2182b", "#d6604d", "#f4a582", # dry shades
  "#f7f7f7",
  "#92c5de", "#4393c3", "#2166ac", "#053061" # wet shades
)

p <- ggplot() +
  geom_sf(
    data = st_as_sf(tb_map_smdi),
    mapping = aes(fill = index)
  ) +
  facet_wrap(~ month, nrow = 2) +
  scale_fill_gradientn(
    name    = NULL,
    colours = cols_smdi,
    limits  = c(-4, 4),
    oob     = scales::squish
  ) +
  labs(title = "SMDI in 2013") +
  theme_map_paper() +
  theme(
    legend.key.width	= unit(1, "line"),
    legend.key.height	= unit(3, "line")
  )

p
# ggsave(p, file = "smdi-2013-map.pdf", width = 10, height = 5)

# 3. Quarterly Aggregation----

df_meteo <- 
  df |> 
  group_by(variable, year, quarter) |> 
  summarize(
    value = mean(value, na.rm = TRUE),
    index = mean(index, na.rm = TRUE)
  )

p_smdi <- ggplot(
  data = df_meteo |> 
    mutate(years = year + quarter / 4),
  mapping = aes(x = years, y = index)
) +
  geom_line() +
  labs(x = NULL, y = NULL, title = "SMDI") +
  geom_rect(
    data = spei_bands,
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = ymin, ymax = ymax,
      fill = label
    ),
    inherit.aes = FALSE,
    alpha = 0.25
  ) +
  scale_fill_manual(
    name = NULL,
    values = setNames(spei_bands$fill, spei_bands$label)
  ) +
  theme_paper() +
  theme(
    legend.position = "right", 
    legend.direction = "vertical"
  )

p_smdi

# ggsave(p_smdi, file = "smdi-nz.pdf", width = 10, height = 5)
