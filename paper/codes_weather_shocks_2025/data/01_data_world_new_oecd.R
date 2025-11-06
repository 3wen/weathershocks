library(tidyverse)
library(readxl)


# Setting the reference year for index creation
ref_year <- 2010

source("variables_names.R")

# 1. Gross Domestic Product----
# Source: OECD
# Quarterly GDP and components - expenditure approach, US Dollars
# Frequency of observation: Quarterly
# Price base: Current prices
# Combined transaction: Gross domestic product, Total economy

df_gdp <- read_excel(
  "economic_data/rest-world/GDP.xlsx", skip = 5,
  n_max = 6
) |> 
  select(-`Time period...2`,-`Time period...3`) |> 
  rename(country = `Time period...1`) |> 
  filter(country != "Reference area") |> 
  pivot_longer(cols = -country, values_to = "gdp") |> 
  mutate(
    year = str_sub(name, 1, 4) |> as.numeric(),
    quarter = str_sub(name, -1) |> as.numeric(),
    YEARS = year + (quarter-1)/4
  ) |> 
  select(-name)

p <- ggplot(
  data = df_gdp, 
  mapping = aes(x = YEARS, y = gdp, colour = country)
) +
  geom_line() +
  xlab("") +
  ylab("") +
  ggtitle("GDP (PPP, millions $, constant 2010 prices)") +
  scale_colour_discrete("") +
  theme_paper()
p

df_gdp |> 
  group_by(country) |> 
  summarise(min_y = min(YEARS), max_y = max(YEARS), n = n())

# 2. Interest Rate----

# Source: OECD
# Economic Outlook No 100 - November 2016
# Variable: Short-term interest rate
# Frequency: Quarterly
# https://stats.oecd.org/Index.aspx?DataSetCode=EO100_INTERNET
df_r <- read_excel(
  "economic_data/rest-world/interest-rate.xlsx",
  skip = 4, n_max = 6
) |> 
  select(-`Time...2`) |> 
  rename(country = `Time...1`) |> 
  filter(country != "Country") |> 
  pivot_longer(cols = -country, values_to = "r") |> 
  mutate(
    year = str_sub(name, 1, 4) |> as.numeric(),
    quarter = str_sub(name, -1) |> as.numeric(),
    YEARS = year + (quarter-1)/4
  ) |> 
  select(-name)

p <- ggplot(
  data = df_r, 
  mapping = aes(x = YEARS, y = r, colour = country)
) +
  geom_line() +
  labs(x = NULL, y = NULL, title ="Short-Term Interest Rate") +
  scale_colour_discrete("") +
  theme_paper()
p

df_r <- df_r |>  filter(!is.na(r))

df_r |> 
  group_by(country) |> 
  summarise(min_y = min(YEARS), max_y = max(YEARS), n = n())

## ======================= ##
# 2. Consumer Price Index----
## ======================= ##

# Source: OECD
# Consumer price indices (CPIs, HICPs), COICOP 1999
# Frequency of observation: Quarterly
# Measure: Consumer price index, National
# Unit of measure: Index, 2015
df_cpi <- read_excel(
  "economic_data/rest-world/CPI.xlsx",
  skip = 5, n_max = 6
) |> 
  select(-`Time period...2`) |> 
  rename(country = `Time period...1`) |> 
  filter(country != "Reference area") |> 
  pivot_longer(cols = -country, values_to = "cpi") |> 
  mutate(
    year = str_sub(name, 1, 4) |> as.numeric(),
    quarter = str_sub(name, -1) |> as.numeric(),
    YEARS = year + (quarter-1)/4
  ) |> 
  select(-name)

# Change base year
ref_cpi <- df_cpi |>  
  filter(YEARS == !!ref_year) |> 
  select(country, cpi_ref = cpi)

df_cpi <- df_cpi |> 
  left_join(ref_cpi, by = c("country")) |> 
  mutate(
    cpi = cpi / cpi_ref * 100
  ) |> 
  select(-cpi_ref)


p <- ggplot(
  data = df_cpi, 
  mapping = aes(x = YEARS, y = cpi, colour = country)
) +
  geom_line() +
  labs(x = NULL, y = NULL, title = "CPI") +
  scale_colour_discrete("") +
  theme_paper()
p

df_cpi <- df_cpi |> filter(!is.na(cpi))

df_cpi |> 
  group_by(country) |> 
  summarise(min_y = min(YEARS), max_y = max(YEARS), n = n())

## =============== ##
# 3. GDP Deflator----
## =============== ##

# Source: OECD
# QNA – Archive before 2019 benchmark revisions
# Subject: Gross domestic product - expenditure approach
# Measure: Deflator, OECD reference year, seasonally adjusted
# Frequency: Quarterly

df_gdp_defl <- read_excel(
  "economic_data/rest-world/GDP_deflator.xlsx",
  skip = 4, n_max = 7
) |> 
  select(-`Period...2`) |> 
  rename(country = `Period...1`) |> 
  filter(country != "Country") |> 
  pivot_longer(cols = -country, values_to = "gdp_defl") |> 
  mutate(
    year = str_sub(name, 1, 4) |> as.numeric(),
    quarter = str_sub(name, -1) |> as.numeric(),
    YEARS = year + (quarter-1)/4
  ) |> 
  select(-name)

# Change base year
# ref_gdp_defl <- df_gdp_defl |>  
#   filter(YEARS == !!ref_year) |> 
#   select(country, gdp_defl_ref = gdp_defl)
# 
# df_gdp_defl <- df_gdp_defl |> 
#   left_join(ref_gdp_defl, by = c("country")) |> 
#   mutate(
#     gdp_defl = gdp_defl / gdp_defl_ref * 100
#   ) |> 
#   select(-gdp_defl_ref)

p <- ggplot(
  data = df_gdp_defl, 
  mapping = aes(x = YEARS, y = gdp_defl, colour = country)
) +
  geom_line() +
  labs(x = NULL, y = NULL, title = "GDP Deflator") +
  scale_colour_discrete("") +
  theme_paper()
p


df_gdp_defl <- df_gdp_defl |>  filter(!is.na(gdp_defl))

df_gdp_defl |> 
  group_by(country) |> 
  summarise(min_y = min(YEARS), max_y = max(YEARS), n = n())


## ============= ##
# 4. Population----
## ============= ##

# Source: OECD
# Historical population data
# Measure: Population
# Age: From 15 to 64 years
# Time horizon: Historical
# Combined unit of measure: Persons

pop <- read_excel(
  "economic_data/rest-world/pop.xlsx", skip = 5,
  n_max = 7
) |> 
  select(-`Time period...2`) |> 
  rename(country = `Time period...1`) |> 
  filter(country != "Reference area") |> 
  pivot_longer(cols = -country, values_to = "pop") |> 
  mutate(
    year = str_sub(name, 1, 4) |> as.numeric()
  ) |> 
  select(-name)

library(tempdisagg)

# Estimate quarterly data from annual data
# Method: denton-cholette

{
  # This function is actually not used
  
  
  #' Convert annual data to quarterly serie.
  #' It uses the variations of the quarterly serie x_quarterly to help fitting.
  #' 
  #' @param y_yearly Annual data.
  #' @param x_quarterly Quarterly data.
  #' @param y_year_start Start year for annual data.
  #' @param x_year_start Start year for quarterly data.
  #' @param x_quarter_start Start quarter for quarterly data.
  #' @param nom_y Name of annual data.
  #' 
  yearly_to_quarterly <- function(y_yearly, 
                                  x_quarterly,
                                  y_year_start, 
                                  x_year_start, 
                                  x_quarter_start, 
                                  nom_y) {
    
    y <- ts(y_yearly, start = y_year_start, frequency = 1)
    x <- ts(x_quarterly, start = c(x_year_start, x_quarter_start), frequency = 4)
    modele <- td(y ~ x)
    y_pred <- predict(modele)
    res <- tibble(
      years = as.vector(time(y_pred)), 
      !!sym(nom_y) := as.vector(y_pred)
    )
    
    res
  }
}


library(tempdisagg)

#' Estimate quarterly population from annual values
#' 
#' @param country Name of the country in the dataset `pop`.
#' 
pop_quarterly <- function(country) {
  x <- pop |> 
    filter(country %in% !!country)
  x_ts <- ts(x$pop, start = as.numeric(x$year[1]))
  # Forecast 3 years ahead
  x_f <- forecast::forecast(forecast::auto.arima(x_ts), h = 3)
  # Disaggregate
  x_ts <- ts(c(x_ts, x_f$mean), start = start(x_ts))
  res <- predict(td(x_ts ~ 1, method = "denton-cholette", conversion = "average"))
  res <- tibble(
    YEARS = as.vector(time(res)), 
    country = country, 
    pop = as.vector(res)
  )
  
  # Change base year
  ref_pop <- res |> filter(YEARS == ref_year) |> pull("pop")
  
  res |> mutate(pop = pop / ref_pop * 100)
}


df_pop <- 
  map(unique(pop$country), ~pop_quarterly(.x)) |> 
  list_rbind()


p <- ggplot(
  data = df_pop, 
  mapping = aes(x = YEARS, y = pop, colour = country)
) +
  geom_line() +
  labs(x = NULL, y = NULL, title = "Population") +
  scale_colour_discrete("") +
  theme_paper()
p

df_pop <- df_pop |>  filter(!is.na(pop))

df_pop |> 
  group_by(country) |> 
  summarise(min_y = min(YEARS), max_y = max(YEARS), n = n())

# ================================= #
# 5. GDP, Inflation rate, and CPI----
# ================================= #

# Quarterly share of each country's GDP
shares <- 
  df_gdp |> 
  group_by(year, quarter, YEARS) |> 
  mutate(total_gdp = sum(gdp)) |> 
  ungroup() |> 
  filter(YEARS >= 1987, YEARS < 2017) |> 
  mutate(share = gdp / total_gdp) |> 
  select(YEARS, country, share)

# Merge all the information in a single table
df <- 
  df_gdp |> 
  left_join(df_r, by = c("country", "year", "quarter", "YEARS")) |> 
  left_join(df_cpi, by = c("country", "year", "quarter", "YEARS")) |> 
  left_join(df_gdp_defl, by = c("country", "year", "quarter", "YEARS")) |> 
  left_join(df_pop, by = c("country", "YEARS")) |> 
  left_join(shares, by = c("country", "YEARS")) |> 
  rename(years = YEARS) |> 
  filter(years >= 1987, years < 2017)


df <- 
  df |>
  mutate(prices = gdp_defl)

df_w <- 
  df |> 
  mutate(r_y = gdp / pop / prices) |> 
  mutate(
    r_y = r_y * share,
    cpi = cpi * share,
    gdp_defl = gdp_defl * share,
    r = r * share
  ) |> 
  group_by(years) |> 
  summarise(
    wgdp = sum(r_y),
    wcpi = sum(cpi),
    wgdp_def = sum(gdp_defl),
    wr = sum(r)
  )

df_w

save(df_w, file = "economic_data/df_w.rda")