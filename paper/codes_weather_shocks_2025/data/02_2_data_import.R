# Codes to replicate the content of the article:
# "Weather Shocks
# 2019

# This package is very useful: when you try to load another package
# if it is missing, pacman will try to install it.
# install.packages("pacman")
library(pacman)

p_load(vars, dplyr, ggplot2, stringr, gdata, ggthemes, lubridate, readxl, tidyr)



# Setting the reference year for index creation
ref_year <- 2010

# 1. Functions----

# Seasonal decomposition
# Seasonal Adjustment with X-13ARIMA-SEATS
source("02_1_seasonality.R")


# 2. Load Data----

# ======================================================== #
# ======================================================== #
#
# FIRST STEP: LOAD DATA FROM THE DIFFERENT SOURCES
#
# ======================================================== #
# ======================================================== #

## ======================================================== #
## GDP (All industries & Agriculture)----
## Seasonnaly adjusted
## Source: OECD
## Frequency: quarterly
## ======================================================== #

gdp <- readxl::read_excel("economic_data/data_nz.xls", sheet = "Y", skip = 2)

gdp <- gdp |> 
  dplyr::select(
    date, 
    `NZNTAGCL Index`, # Agriculture Chain Volume
    `NZNTPRAS Index`, # GDP Chain Volume
    `NZNTNOM Index`   # NZ Expenditure Based GDP (Nominal, SA, NZD)
  ) |> 
  mutate(
    agri_share = `NZNTAGCL Index` / `NZNTPRAS Index`,
    gdp_a = agri_share * `NZNTNOM Index`
  ) |> 
  rename(gdp_tot = `NZNTNOM Index`) |> 
  mutate(
    gdp = gdp_tot - gdp_a,
    year = as.numeric(str_sub(date, 1, 4)),
    quarter = as.numeric(str_sub(date, 6, 7)),
    quarter = ceiling(quarter / 3),
    YEARS = year + quarter / 4 - 0.25
  ) |> 
  select(YEARS, gdp, gdp_a, gdp_tot)



## ======================================================== #
## Consumption----
## Source: Statistics New Zealand
## Frequency: quarterly
## ======================================================== #

consum <- readxl::read_excel("economic_data/data_nz.xls", sheet = "C", skip = 3) |> 
  rename(
    YEARS = `...1`,
    c = `Households...2`,
    c_a = `Households...3`
  )

consum <- 
  consum |> 
  select(YEARS, c, c_a) |> 
  filter(YEARS %>% str_detect("^[[:alnum:]]{4}Q[[:alnum:]]$")) |> 
  mutate(
    year = as.numeric(str_sub(YEARS, 1, 4)),
    quarter = c(0, 0.25, 0.5, 0.75)[as.numeric(str_sub(YEARS, -1))],
    YEARS = year + quarter,
    c = c |> as.character() |> as.numeric(),
    c_a = c_a |> as.character() |> as.numeric()
  ) |> 
  select(-year, -quarter)

head(consum)
tail(consum)


## ======================================================== #
## Investment----
## Source: Statistics New Zealand
## Frequency: quarterly
## ======================================================== #

invest <- readxl::read_excel("economic_data/data_nz.xls", sheet = "inv") |> 
  rename(
    YEARS = Series,
    I = `GDP(E)`,
    I_private = Nominal
  )


invest <- 
  invest |> 
  select(YEARS, I,I_private) |> 
  filter(YEARS %>% str_detect("^[[:alnum:]]{4}Q[[:alnum:]]$")) |> 
  mutate(
    year = as.numeric(str_sub(YEARS, 1, 4)),
    quarter = c(0, 0.25, 0.5, 0.75)[as.numeric(str_sub(YEARS, -1))],
    YEARS = year + quarter,
    I = I |>  as.character() |>  as.numeric(),
    I_private = I_private |> as.character() |>  as.numeric()
  ) |> 
  select(-year, -quarter)

head(invest)
tail(invest)


## ======================================================== #
## Consumer price index----
## Source: Statistics New Zealand
## Frequency: quarterly
## ======================================================== #

cpi <- readxl::read_excel(
  "economic_data/data_nz.xls", sheet = "CPI", skip = 1, na = ",,"
) |> 
  rename(
    YEARS = `...1` ,
    P = `All groups`
  )

cpi <- 
  cpi |> 
  select(YEARS, P) |> 
  filter(YEARS |> str_detect("^[[:digit:]]{4}Q[[:digit:]]$")) |> 
  mutate(
    year = as.numeric(str_sub(YEARS, 1, 4)),
    quarter = c(0, 0.25, 0.5, 0.75)[as.numeric(str_sub(YEARS, -1))],
    YEARS = year + quarter
  ) |> 
  select(-year, -quarter) |> 
  mutate(P = P |>  as.character() |> as.numeric())

# Change base year
ref_cpi <- cpi |>  filter(YEARS == ref_year) |>  pull("P")

cpi <- 
  cpi |> 
  mutate(P = P / ref_cpi * 100)

head(cpi)
tail(cpi)


## ======================================================== #
## GDP Deflator----
## Source: Statistics New Zealand
## Frequency: quarterly
## ======================================================== #

gdp_defl <- readxl::read_excel(
  "economic_data/data_nz.xls", sheet = "CPI", skip = 1, na = ",,"
) |> 
  select(years, gdp_defl)


gdp_defl <- 
  gdp_defl |> 
  filter(years %>% str_detect("^[[:digit:]]{4}Q[[:digit:]]$")) |> 
  mutate(
    year = as.numeric(str_sub(years, 1, 4)),
    quarter = c(0, 0.25, 0.5, 0.75)[as.numeric(str_sub(years, -1))],
    years = year + quarter
  ) |> 
  select(-year, -quarter) |> 
  rename(YEARS = years, PP = gdp_defl) |> 
  mutate(PP = PP |> as.character() |> as.numeric())

# Change base year
ref_pp <- gdp_defl |> filter(YEARS == ref_year) |> pull("PP")

gdp_defl <- 
  gdp_defl |> 
  mutate(PP = PP / ref_pp * 100)

head(gdp_defl)
tail(gdp_defl)



## ======================================================== #
## Exports----
## Source: OECD
## Frequency: quarterly
## ======================================================== #

trade <- readxl::read_excel(
  "economic_data/data_nz.xls", sheet = "imports_exports", skip = 3, na = ",,"
)


trade <- 
  trade |> 
  filter(YEARS %>% str_detect("^[[:digit:]]{4}Q[[:digit:]]$")) |> 
  mutate(
    year = as.numeric(str_sub(YEARS, 1, 4)),
    quarter = c(0, 0.25, 0.5, 0.75)[as.numeric(str_sub(YEARS, -1))],
    YEARS = year + quarter
  ) |> 
  select(-year, -quarter) |> 
  mutate(
    exports_a = as.numeric(as.character(exports_a)),
    imports = as.numeric(as.character(imports_sa_vfd)),
    exports = as.numeric(as.character(exports_sa))
  )

head(trade)
tail(trade)

## ======================================================== #
## Paid hours----
## Source: Statistics New Zealand
## Frequency: quarterly
## ======================================================== #

paid_hours <- readxl::read_excel(
  "economic_data/data_nz.xls", sheet = "Hours(FTE)", skip = 1, na = ",,"
) |> 
  rename(
    YEARS = `...1`,
    weekly = `Total All Industries`
  )

paid_hours <- 
  paid_hours |> 
  select(YEARS, weekly) |> 
  filter(YEARS |> str_detect("^[[:digit:]]{4}Q[[:digit:]]$")) |> 
  mutate(
    year = as.numeric(str_sub(YEARS, 1, 4)),
    quarter = c(0, 0.25, 0.5, 0.75)[as.numeric(str_sub(YEARS, -1))],
    YEARS = year + quarter
  ) |> 
  select(-year, -quarter) |> 
  rename(H = weekly) |> 
  mutate(H = as.numeric(as.character(H)))

# Index
ref_hours <- paid_hours |> filter(YEARS == ref_year) |> pull("H")

paid_hours <- 
  paid_hours |> 
  mutate(H = H / ref_hours * 100)

head(paid_hours)
tail(paid_hours)


## ======================================================== #
## Employment----
## Source: Statistics New Zealand
## Frequency: quarterly
## ======================================================== #

employment <- readxl::read_excel(
  "economic_data/data_nz.xls", sheet = "employment", skip = 2, na = ",,"
) |> 
  rename(employment = `Employment Rate`)


employment <- 
  employment |> 
  select(YEARS, employment) |> 
  filter(YEARS %>% str_detect("^[[:digit:]]{4}Q[[:digit:]]$")) |> 
  mutate(
    year = as.numeric(str_sub(YEARS, 1, 4)),
    quarter = c(0, 0.25, 0.5, 0.75)[as.numeric(str_sub(YEARS, -1))],
    YEARS = year + quarter
  ) |> 
  select(-year, -quarter) |> 
  rename(E = employment) |> 
  mutate(E = as.numeric(as.character(E)))


## ======================================================== #
## Wages----
## Source: Statistics New Zealand
## Frequency: quarterly
## ======================================================== #

wages <- readxl::read_excel(
  "economic_data/data_nz.xls", sheet = "Wages", skip = 3, na = ",,"
) |> 
  rename(
    YEARS = `...1`,
    Wage = `Total - Seasonally Adjusted`
  )

wages <- 
  wages |> 
  select(YEARS, Wage) |> 
  filter(YEARS |> str_detect("^[[:digit:]]{4}Q[[:digit:]]$")) |> 
  mutate(
    year = as.numeric(str_sub(YEARS, 1, 4)),
    quarter = c(0, 0.25, 0.5, 0.75)[as.numeric(str_sub(YEARS, -1))],
    YEARS = year + quarter
  ) |> 
  select(-year, -quarter) |> 
  rename(W = Wage) |> 
  mutate(W = as.numeric(as.character(W)))

# Change base year
ref_wages <- wages |> filter(YEARS == ref_year) |> pull("W")

wages <- 
  wages |> 
  mutate(W = W / ref_wages * 100)

head(wages)
tail(wages)

## ======================================================== #
## Population----
## Source: Statistics New Zealand
## Frequency: quarterly
## ======================================================== #

pop <- readxl::read_excel("economic_data/data_nz.xls", sheet = "POP", skip = 1)
colnames(pop)[c(1,2)] <- c("YEARS", "pop")

pop <- 
  pop |> 
  select(YEARS, pop) |> 
  mutate(POP = pop |> str_replace_all(",", "")) |> 
  select(-pop) |> 
  filter(YEARS |> str_detect("^[[:digit:]]{4}Q[[:digit:]]$")) |> 
  mutate(
    year = as.numeric(str_sub(YEARS, 1, 4)),
    quarter = c(0, 0.25, 0.5, 0.75)[as.numeric(str_sub(YEARS, -1))],
    YEARS = year + quarter,
    POP = POP |> as.character() |> as.numeric()
  ) |> 
  select(-year, -quarter)


# Now let us create an index
pop_ref <- pop |>  filter(YEARS == ref_year) |> pull("POP")

pop <- 
  pop |> 
  mutate(POP = POP / pop_ref)

head(pop)
tail(pop)


## ======================================================== #
## Production prices (All industrie & agriculture)----
## Source: Statistics New Zealand
## Frequency: quarterly
## ======================================================== #

prices <- readxl::read_excel(
  "economic_data/data_nz.xls", sheet = "productionPrice", skip = 1, na = ",,"
) |> 
  rename(
    YEARS = `...1`,
    P = `All Industries`,
    P_A = `Agriculture, Forestry and Fishing`
  )

prices <-
  prices |> 
  select(YEARS, P_A) |> 
  filter(YEARS |> str_detect("^[[:alnum:]]{4}Q[[:alnum:]]")) |> 
  mutate(
    year = as.numeric(str_sub(YEARS, 1, 4)),
    quarter = c(0, 0.25, 0.5, 0.75)[as.numeric(str_sub(YEARS, -1))],
    YEARS = year + quarter,
    P_A = P_A |> as.character() |> as.numeric()
  ) |> 
  select(-year, -quarter)


# Create price indices
p_a_ref <- prices %>% filter(YEARS == ref_year) |> pull("P_A")

# Join CPI
prices <- 
  cpi |> 
  left_join(prices)

prices <- 
  prices |> 
  mutate(P_A = P_A / p_a_ref * 100)

head(prices)
tail(prices)


# Join CPI
prices <- 
  cpi |> 
  left_join(prices)

head(prices)
tail(prices)

## ======================================================== #
## Interest rate----
## Source: OECD stat
## Frequency: quarterly
## ======================================================== #

interest <- readxl::read_excel(
  "economic_data//data_nz.xls", sheet = "R", na = ",,", skip = 10
) |> 
  select(YEARS = observation_date, R = IR3TBB01NZQ156N)

interest <- 
  interest |> 
  mutate(YEARS = YEARS |> ymd()) |> 
  mutate(
    year = year(YEARS),
    quarter = YEARS |> quarter(),
    YEARS = year + c(0, 0.25, 0.5, 0.75)[quarter]
  ) |> 
  select(-year, -quarter) |> 
  filter(!is.na(YEARS), str_length(YEARS) > 0)

str_length(interest$YEARS)

tail(interest)


## =============== #
## EXCHANGE RATE----
## Source: FRED    #
## =============== #

ex_rate <- readxl::read_excel(
  "economic_data/data_nz.xls", sheet = "ExchangeRate", skip = 0
)
head(ex_rate)          

ex_rate <- ex_rate[, which(colnames(ex_rate) %in% c("DATE", "RTWI"))]

ex_rate <- 
  ex_rate |> 
  mutate(
    quarter = str_extract(DATE, "M(.*?)$"),
    quarter = str_sub(quarter, 2),
    quarter = match(quarter, c(1,4, 7, 10)),
    year = str_sub(DATE, 1, 4) |> as.numeric(),
    YEARS = year + quarter/4 - 0.25
  ) |> 
  select(-quarter, -year, -DATE)

# Index
ex_rate_ref <- ex_rate |> filter(YEARS == ref_year) |>  pull("RTWI")

ex_rate <-
  ex_rate |> 
  mutate(ex_rate = RTWI / ex_rate_ref * 100) |> 
  select(YEARS, ex_rate)

head(ex_rate)
tail(ex_rate)


## ============================== #
## REAL EFFECTIVE EXCHANGE RATE----
## Source: FRED                   #
## ============================== #

# Real Broad Effective Exchange Rate for New Zealand (RBNZBIS)
# Source: Bank for International Settlements
# Not seasonally adjusted, monthly

reer <- readxl::read_excel("economic_data/data_nz.xls", sheet = "Reer", skip = 10)
head(reer)

# Seasonal adjustment
debut_reer <- reer$observation_date[1]

reer <- 
  reer |> 
  select(RBNZBIS) |> 
  ts(start = c(year(debut_reer),month(debut_reer)), freq = 12) |> 
  seasonal::seas() |> 
  seas_to_df(name = "reer") |> 
  group_by(year, quarter) |> 
  summarise(reer = mean(reer), .groups = "drop")


reer <-
  reer |> 
  mutate(YEARS = as.numeric(year) + as.numeric(quarter)/4 - 0.25) |> 
  select(-quarter, -year)


# Index
reer_ref <- reer |> filter(YEARS == ref_year) |> pull("reer")

reer <-
  reer |> 
  mutate(reer = reer / reer_ref * 100) |> 
  select(YEARS, reer)

head(reer)
tail(reer)



## ======================================================== #
## Weather data----
## Source: National Climate Database, 
#          National Institute of Water and Atmospheric Research
## Frequency: quarterly
## ======================================================== #

# See R codes from data/climate_data/

load("climate_data/meteo.rda")

climate_trim <- 
  meteo |> 
  mutate(quarter = as.numeric(as.character(quarter)))


## ============== #
## SHARE PRICE----
## Source : OECD  #
## ============== #

share_price <- readxl::read_excel(
  "economic_data/data_nz.xls", sheet = "Share_Prices(OECD)", skip = 1
)

share_price <- 
  share_price |> 
  mutate(
    year = as.numeric(str_sub(YEARS, -4)),
    quarter = c(0, 0.25, 0.5, 0.75)[as.numeric(str_sub(YEARS, 2,2))],
    YEARS = year + quarter,
    share_price = share_prices_index |> as.character() |> as.numeric()
  ) |> 
  select(-year, -quarter)

# Index
share_price_ref <- share_price |> filter(YEARS == ref_year) |> pull("share_price")

share_price <-
  share_price |> 
  mutate(share_price = share_price / share_price_ref * 100)


## ================== #
## SHARE PRICE----
## Source : BLOOMBERG #
## ================== #

share_price <- readxl::read_excel(
  "economic_data//data_nz.xls", sheet = "NZSE_Index", skip = 1
)

share_price <-
  share_price |> 
  rename(date = Date, share_price = PX_LAST) |> 
  mutate(
    year = year(date),
    month = month(date),
    quarter = match(month, c(3,6,9,12)),
    YEARS = year + quarter/4 - 0.25
  ) |> 
  select(YEARS, share_price)


# Index
share_price_ref <- share_price |> filter(YEARS == ref_year) |>  pull("share_price")

share_price <-
  share_price |> 
  mutate(share_price = share_price / share_price_ref * 100)

## ========================== #
## CRUDE OIL PRICES (Dubai)----
## Source : FRED              #
## ========================== #

# Global price of Dubai Crude©,
# U.S. Dollars per Barrell, Quarterly, Not Seasonally Adjusted

crude_oil <- readxl::read_excel(
  "economic_data//data_nz.xls", sheet = "Crude Oil", skip = 10
)
head(crude_oil)

crude_oil <- 
  crude_oil |> 
  rename(date = observation_date, crude_oil = POILDUBUSDQ) |> 
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    quarter = match(month, c(1,4,7,10)),
    YEARS = year + quarter/4 - 0.25
  )

# Unseason
crude_oil_desais <- 
  remove_seasonnality(
    x = crude_oil, 
    variable = "crude_oil", 
    start = c(crude_oil$year[1], crude_oil$quarter[1])
  )

crude_oil <- 
  crude_oil |> 
  select(-crude_oil) |> 
  left_join(crude_oil_desais, by = "YEARS") |> 
  select(YEARS, crude_oil)


# Index
crude_oil_ref <- crude_oil |> filter(YEARS == ref_year) |>  pull("crude_oil")

crude_oil <-
  crude_oil |> 
  mutate(crude_oil = crude_oil / crude_oil_ref * 100)


## ======================================================== #
## TRADING PARTNERS----
## Source: OECD and Oxford Economics
## Frequency: quarterly
## ======================================================== #

# See 01_data_world_new_oecd.R for computations
load("economic_data/df_w.rda")

df_w <- df_w |> 
  ungroup() |> 
  rename(YEARS = years)

# 3. Merge----

# ======================================================== #
# ======================================================== #
#
# SECOND STEP: MERGE ALL DATA IN A SINGLE TABLE
#
# ======================================================== #
# ======================================================== #

head(share_price)

donnees_brutes_nz <- 
  climate_trim |> 
  full_join(gdp) |>  
  full_join(consum) |>  
  full_join(trade) |>  
  full_join(invest) |>   
  full_join(prices) |>   
  full_join(gdp_defl) |>  
  full_join(interest) |>  
  full_join(ex_rate) |>  
  full_join(reer) |>  
  full_join(pop) |>  
  full_join(paid_hours) |>  
  full_join(employment) |>  
  full_join(wages) |>  
  full_join(df_w) |>  
  full_join(share_price) |>  
  full_join(crude_oil) |>  
  filter(YEARS >= 1994.25, YEARS < 2017)


df <- 
  donnees_brutes_nz |> 
  rename(Y = gdp, Y_TOT = gdp_tot, Y_A = gdp_a, C_A = c_a, C = c) |> 
  select(
    YEARS, Y, Y_TOT, C, I, I_private, exports, imports, exports_a, P, PP,
    POP, H, E, W, Y_A, P_A, C_A, R,
    crude_oil,
    ex_rate, reer, share_price,
    rain_val, soil_moist,
    crfi, smdi, spi,
    wgdp, wr, wcpi, wgdp_def) |> 
  ungroup()

df |> select(-YEARS) |> cor(use = "pairwise.complete.obs")

df |> select(
  rain_val, soil_moist,
  crfi, smdi, spi,
  Y, P, Y_A, P_A
) |> 
  mutate(lead(P_A)) |> 
  cor(use = "pairwise.complete.obs")


p_load(mFilter)

# 4. Detrending----

# Filter type to use here:
type <- "hp"

# We will use the GDP deflator as the inflation time serie
# rather than CPI.

df <- 
  df |> 
  mutate(
    prices = PP,
    world_prices = wgdp_def,
    invest = I_private,
    ratio_p = P_A / prices
  )


library(TTR)
df_finale <- 
  df |> 
  # Transformting in per capita / real terms
  mutate(
    r_y = Y / POP / prices,
    r_y_tot = Y_TOT / POP / prices,
    r_y_a = Y_A / POP / P_A,
    r_wy = wgdp,
    r_q = share_price / prices,
    r_c = C / POP / prices,
    r_x = exports / POP / prices,
    r_x_a = exports_a / POP / prices,
    r_im = imports / POP / prices,
    r_tb = r_x - r_im,
    r_c_a = C_A / POP / P_A,
    r_i = invest / POP / prices,
    r_h = H * E,
    r_w = W / prices
  ) |> 
  select(-wgdp) |> 
  mutate(
    y_obs = myfilter(r_y, type),
    y_tot_obs = myfilter(r_y_tot, type),
    wy_obs = myfilter(r_wy, type),
    y_a_obs = myfilter(r_y_a, type),
    c_obs = myfilter(r_c, type),
    c_a_obs = myfilter(r_c_a, type),
    x_obs = myfilter(r_x, type),
    x_a_obs = myfilter(r_x_a, type),
    im_obs = myfilter(r_im, type),
    q_obs = c(NA, diff(log(r_q))*100),
    q_obs = q_obs - hp_filter(q_obs),
    i_obs = myfilter(r_i, type),
    h_obs = log(r_h / mean(r_h, na.rm=T)) * 100,
    h_obs2 = log(E / mean(E, na.rm=T)) * 100,
    e_obs = E,
    w_obs = myfilter(r_w, type),
    p_obs = c(NA, diff(log(prices))*100),
    ratio_p_obs = myfilter(ratio_p, type),
    wp_obs = c(NA, diff(log(world_prices))*100),
    p_a_obs = c(NA, diff(log(P_A))*100),
    oil_obs = c(NA, diff(log(crude_oil))*100),
    r_obs = R / 4,
    wr_obs = wr / 4,
    ex_rate_obs = myfilter(ex_rate, type),
    reer_obs = c(NA, diff(log(reer))*100),
    crfi_obs = crfi,
    smdi_obs = smdi,
    rain_obs = rain_val,
    sm_obs = soil_moist,
    spi_obs = spi
  )

# 5. Export----

save(df_finale, file = "df_finale.rda")
load("df_finale.rda")



# ===================#
# EXPORT TO CSV FILE #
# ===================#


df_excel <- 
  df_finale |> 
  mutate(
    vide = NA,
    YEARS_2 = YEARS
  ) |> 
  select(
    "YEARS", "smdi_obs",
    "wy_obs", "wp_obs", "wr_obs",
    "y_obs", "p_obs", "y_a_obs", "p_a_obs",
    "c_obs", "i_obs", "q_obs",
    "h_obs",
    "r_obs", "ex_rate_obs", "reer_obs"
  )

p_load(readr)
write_delim(df_excel, path = "df_finale.csv", delim = ";")

# 6. Graphs----

source("variables_names.R")


tb_var_exo <- tribble(
  ~variable, ~label,
  "smdi_obs", "Weather (SMDI)",
  "wy_obs", "Foreign Output",
  "y_obs", "Production",
  "y_a_obs",  "Agriculture",
  "h_obs", "Hours Worked",
  "c_obs", "Consumption",
  "i_obs", "Investment",
  "reer_obs", "Delta Exchange Rate"
)



# Plotting all the variables of interest
ggplot(
  data = df_finale |> 
    select(YEARS, !!tb_var_exo$variable) |> 
    pivot_longer(cols = -YEARS) |> 
    mutate(
      name = factor(
        name, levels = tb_var_exo$variable, labels = tb_var_exo$label
      )
    ),
  mapping = aes(x = YEARS, y = value)
) +
  geom_line(colour = "dodgerblue") +
  facet_wrap(~ name, scales = "free") +
  geom_hline(yintercept = 0, linetype = "solid", colour = "gray") +
  geom_vline(
    xintercept = c(1998, 2002, 2008, 2013), 
    linetype = "dotted", col = "red"
  ) +
  theme_paper()
    


# Only the drought index
ggplot(
  data = df_finale |> 
    mutate(y_a_hp = hp_filter(r_y_a)) |> 
    select(YEARS, smdi) |> 
    gather(key, value, -YEARS),
  mapping = aes(x = YEARS, y = value)
) +
  geom_line() +
  facet_wrap(~ key, scales = "free") +
  geom_vline(
    xintercept = c(2007.75), 
    linetype = "dotted", col = "blue"
  )+
  geom_vline(
    xintercept = c(1998, 2003, 2008, 2013), 
    linetype = "dotted", col = "red"
  ) +
  theme_paper()


# Openess
df_finale |> 
  mutate(openess = (exports  + imports) / (2*Y_TOT*1000)) |> 
  select(YEARS, exports, imports, exports_a, Y_TOT, openess) |> 
  summarise(
    openess = mean(openess),
    share_exports = mean(exports / (Y_TOT*1000)),
    share_imports = mean(imports / (Y_TOT*1000)),
    exports_a = mean(exports_a*1000 / exports)
  )
