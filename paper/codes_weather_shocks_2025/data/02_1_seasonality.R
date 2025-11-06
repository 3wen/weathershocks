# Codes to replicate the content of the article:
# "Weather Shocks
# 2019


# Objective : remove seasonal component of time-series

#' Remove seasonality from a variable
#' 
#' @param x A data frame.
#' @param nom Name of the variable in `df`.
#' @param start Start date.
#' @param freq Frequency (default to 4, quarterly).
#' 
#' @importFrom seasonal seas
remove_season <- function(x, name, start, freq = 4){
  x |> dplyr::pull(!!name) |> 
    ts(start = start, frequency = freq) |> 
    seasonal::seas()
}

# Extraire les composantes de temps de l'objet ts
#' Extract components of ts object
#' 
#' @param x An object of class ts.
#' 
extract_temps <- function(x) {
  years <- 
    x |> 
    time() |> 
    zoo::as.yearqtr() |> 
    format("%Y")
  
  quarters <-
    x |> 
    time() |> 
    zoo::as.yearqtr() |> 
    format("%q")
  
  tibble::tibble(year = years, quarter = quarters)
}

# Transformer l'objet seas en data.frame
#' Transform an object of class seas
#' 
#' @param x An object of class seas.
#' @param name Desired name for the variable column
seas_to_df <- function(x, name) {
  res <- 
    seasonal::final(x) |> 
    extract_temps() |> 
    bind_cols(tibble(x = seasonal::final(x) |>  as.vector()))
  colnames(res)[3] <- name
  res
}

#' Remove seasonality in a time series
#' 
#' @param x A data frame.
#' @param name Name (string) of the variable in the data frame for wich one 
#'  wants to remove seasonality.
#' @param start Start date.
#' 
unseason <- function(x, name, start) {
  seas_to_df(
    x = remove_season(x = x, name = name, start = start), 
    name = name
  )
}

#' Remove seasonnality
#' 
#' @param x A data frame.
#' @param variable Name of the variable for which to remove seasonnality.
#' @param start Start date.
remove_seasonnality <- function(x, variable, start){
  unseason(x = x, name = variable, start = start) |> 
    dplyr::mutate(
      YEARS = as.numeric(year) + c(0, 0.25, 0.5, 0.75)[as.numeric(quarter)]
    ) |> 
    dplyr::select(-year, -quarter)
}

#' Detrends a time serie
#' 
#' @param x Serie to be detrended (numeric).
#' @param type Type of the method applied: Hodrick-Prescott (hp) of OLS (ols).
myfilter <- function(x, type = c("hp", "ols")) {

  if (type == "hp") {
    res <- hp_filter(x)
    res <- log(x / res) * 100
  } else if (type == "ols") {
    x_log <- log(x)*100
    trend_x <- lin_trend(x_log)
    res <- x_log - trend_x
  } else {
    stop("Not the right filter")
  }
  res
}



#' Applies the HP filter on a quarterly time serie
#' 
#' @param x Series for which to retrieve 
hp_filter <- function(x){
  serie <- x
  if(any(is.na(x))){
    serie <- x[!is.na(x)]
  }
  
  res <- hpfilter(serie, freq=1600, type = "lambda")$trend %>% as.vector()
  if(any(is.na(x))){
    x[!is.na(x)] <- res
    res <- x
  }
  res
}


#' Returns the linear trend of `x`
#' 
lin_trend <- function(x, trend_val = df$YEARS) {
  
  serie <- x
  if (any(is.na(x))) {
    serie <- x[!is.na(x)]
    trend_val <- trend_val[!is.na(x)]
  }
  
  df_tmp <- data.frame(x = serie) |> 
    mutate(cste = 1, trend = trend_val)
  
  reg <- lm(x ~ 1 + trend, data = df_tmp)
  resul <- coef(reg)[["trend"]]*df_tmp$trend + coef(reg)[["(Intercept)"]]
  
  
  if (any(is.na(x))) {
    x[!is.na(x)] <- resul
    resul <- x
  }
  
  resul
}