# Codes for replication of the article:
# "Weather Shocks"
# 2019

# This package is very useful: when you try to load another package
# if it is missing, pacman will try to install it.
# install.packages("pacman")
library(pacman)

# Loading packages that are needed to run the code
p_load(vars, dplyr, ggplot2, tidyr, stringr, readr)
rm(list=ls())

# If TRUE, graphs are saved
save_graphs <- TRUE

# 1. Settings----

# ---------------------------------------- #
# Picking the variables for the estimation #
# ---------------------------------------- #

# Table with variables' names and types
# Type of variables: in the weather block, the foreign block, or the domestic block
variables_types <-
  matrix(
    c("smdi_obs", "climate",
      "wy_obs", "world",
      "y_obs", "domestic",
      "y_a_obs", "domestic",
      "h_obs", "domestic",
      "c_obs", "domestic",
      "i_obs", "domestic",
      "reer_obs", "domestic"),
    ncol = 2, byrow = T
  )

# Variables
variables_exo <- variables_types[,1]

# number lags
L <- 1

# number of variables
N <- length(variables_exo)
# 
# # number of endogenous variables
# K <- length(variables_exo)


#' Returns the corresponding name of the block for a variable
#' 
#' @param name Variable name (string).
#' name <- "wp_obs"
find_block <- function(name) {
  variables_types[variables_types[, 1] == name, 2]
}

# Let us create a matrix with the constraints on coefficients to ensure the 
# exogeneity of blocks.
# The equations will be written in columns in this matrix.
# Hence, at element [i,j], the coefficient indicates whether
# the j^th variable can affect the i^th one. If so, the element [i,j]
# will take the value 1; 0 otherwise.

A_l_restrictions <- 
  matrix(NA, ncol = N, nrow = N)
colnames(A_l_restrictions) <- rownames(A_l_restrictions) <- variables_exo


# i_column <- 1 ; i_row <- 2
for (i_column in 1:nrow(A_l_restrictions)) {
  name_eq <- colnames(A_l_restrictions)[i_column]
  
  for (i_row in 1:ncol(A_l_restrictions)) {
    nom_expl <- rownames(A_l_restrictions)[i_row]
    value_to_set <- 1
    # Climate block
    if (find_block(name_eq) == "climate") {
      if(find_block(nom_expl) != "climate") value_to_set <- 0
    }
    
    # Rest of the world block
    if (find_block(name_eq) == "world") {
      if(find_block(nom_expl) != "world") value_to_set <- 0
    }
    
    A_l_restrictions[i_row, i_column] <- value_to_set
  }
}

A_l_restrictions

# Now, let us write the equations in lines instead of rows
A_l_restrictions <- t(A_l_restrictions)


# Let us replicate this matrix L times
# so that all the constraints imposed apply to each lag
# Equations are written in lines.

A_l_restrict <- parse(
  text = str_c(
    "cbind(",
    str_c(rep("A_l_restrictions", L), collapse = ", "),
    ")"
  )
) |> 
  eval()

A_l_restrict <- do.call(cbind, rep(list(A_l_restrictions), L))
base_nms <- colnames(A_l_restrictions)
colnames(A_l_restrict) <- paste0(
  base_nms, "_", rep(seq_len(L), each = length(base_nms))
)

# Let us add an intercept (const)
A_l_restrict <- cbind(A_l_restrict, const = rep(1, nrow(A_l_restrict)))

# 2. Data----

# --------------------------- #
# Data used in the estimation #
# --------------------------- #

load('../data/df_finale.rda')

# Total production is denoted y_tot_obs in the base we just loaded
df_finale <-
  df_finale |> 
  mutate(y_obs = y_tot_obs)

# Restrain to the variables of interest
df_finale <- 
  df_finale |> 
  dplyr::select(YEARS, !!variables_exo)

# And to the time period of interest
df_finale <- df_finale |> 
  filter(YEARS >= 1994.25)

# We know there is no missing values between different dates
# so, we can use na.omit without risk.
# (there is only one missing value for the reer)
df_finale <- na.omit(df_finale)


# Now, we need to turn the data into a time series (ts) object
start_date_raw_data <- c(
  df_finale$YEARS[1] %/% 1,
  df_finale$YEARS[1] %% 1 * 4 + 1
)
end_date_raw_data <- c(
  df_finale$YEARS[nrow(df_finale)] %/% 1,
  df_finale$YEARS[nrow(df_finale)] %% 1 * 4 + 1
)
frequency_raw_data <- 4 # quarterly

start_date_sample <- c(
  df_finale$YEARS[1] %/% 1,
  df_finale$YEARS[1] %% 1 * 4 + 1
)
end_date_sample <- end_date_raw_data

# Finally turning it to a ts object
raw_data <- 
  df_finale |> 
  dplyr::select(!!variables_exo) |> 
  ts(frequency = frequency_raw_data, start = start_date_raw_data)

# Let us call the data used in the estimation: data
data <- raw_data

library(vars)
# data <- window(data,start=start_date_sample,end=end_date_sample)

cor(data)

# 3. Estimation----

# --------------------- #
# Estimation of the VAR #
# --------------------- #

VARselect(data, lag.max = 4, type = "const")
VARselect(data, lag.max = 4, type = "trend")

# Estimation without the constraints
var_1 <- VAR(data, p = L, type = "const")

# Now let us add the constraints
# and estimate the restricted VAR
var_res <- restrict(var_1, method = "manual", resmat = A_l_restrict)

# Then the SVAR model can be estimated, by specifying the A matrix
# Each row of this matrix sets the constraints for an equation of the previously estimated VAR
# For example, the line `y_a_obs` states that the agricultural output can contemporaneously depend
# only on the weather, foreign GDP and domestic output
amat <- A_l_restrictions
amat[amat == 1] <- NA
amat[upper.tri(amat)] <- 0
diag(amat) <- 1
amat

svar_est <- SVAR(x = var_res, Amat = amat, Bmat = NULL, estmethod = "direct")

# Estimation results
summary(svar_est)


# We can export the data for use in the DSGE
write_delim(df_finale, file = "../data/df_finale_var.csv", delim = ";")


plot(var_res$datamat$smdi_obs, t="l")
lines(fitted(var_res$varresult$smdi_obs), col = "red", lty = 2)

# ----------- #
# Fisher Test #
# ----------- #

fisher_test <- function(variables_rest, 
                        variables_interest, 
                        print_res = TRUE, 
                        contemp = TRUE) {
  
  variables <- c(variables_rest, variables_interest)
  
  # Complete model
  data_tmp <- as_tibble(data)
  data_tmp <- 
    data_tmp |> 
    dplyr::select(!!variables)
  
  form <- str_c(variables_interest, " ~ 1")
  
  # i <- 1
  for (i in 1:length(variables)) {
    var_name <- variables[i]
    var_name_new <- str_c(variables[i], "_1")
    
    # Contemporaneous effect
    if (contemp) {
      ind <- which(rownames(amat) == variables_interest)
      if (is.na(amat[ind, which(colnames(amat) == var_name)])) {
        form <- str_c(form, var_name, sep = " + ")
      }
    }
    
    # Lagged effect
    ind <- which(rownames(A_l_restrict) == variables_interest)
    
    if (A_l_restrict[ind, which(colnames(A_l_restrict) == var_name_new)] == 1) {
      data_tmp <- 
        data_tmp |> 
        mutate(!!var_name_new := dplyr::lag(!!sym(var_name),1))
      
      form <- str_c(form, var_name_new, sep = " + ")
    }
  }
  
  mod_complete <- lm(as.formula(form), data = data_tmp)
  
  # Restricted model
  variables_rest_r <- variables_rest[-which(variables_rest == "smdi_obs")]
  variables_r <- c(variables_rest_r, variables_interest)
  
  data_tmp_r <- as_tibble(data) |> 
    dplyr::select(c(!!variables_rest_r, !!variables_interest))
  
  form_r <- str_c(variables_interest, " ~ 1")
  
  # i <- 1
  for (i in 1:length(variables_r)) {
    var_name <- variables_r[i]
    var_name_new <- str_c(variables_r[i], "_1")
    
    # Contemporaneous effect
    if (contemp) {
      ind <- which(rownames(amat) == variables_interest)
      if (is.na(amat[ind, which(colnames(amat) == var_name)])) {
        form_r <- str_c(form_r, var_name, sep = " + ")
      }
    }
    
    # Lagged effect
    if (A_l_restrict[ind, which(colnames(A_l_restrict) == var_name_new)] == 1) {
      data_tmp_r <- 
        data_tmp_r |> 
        mutate(!!var_name_new := dplyr::lag(!!sym(var_name),1))
      
      form_r <- str_c(form_r, var_name_new, sep = " + ")
    }
  }
  
  mod_restricted <- lm(as.formula(form_r), data = data_tmp_r)
  
  rss_r <- sum(mod_restricted$residuals^2)
  rss_c <- sum(mod_complete$residuals^2)
  const <- 0
  p <- mod_restricted$rank-const
  k <- mod_complete$rank - const - p
  n <- length(mod_restricted$residuals)
  F_obs <- ((rss_r - rss_c) / k) / (rss_c / (n - (k+p+const)))
  (F_tab <- qf(p = 1-0.05, df1 = p, df2 = n - (k+p+1)))
  p_value <- (1 - pf(q = F_obs, df1 = p, df2 = n - (k+p+const)))
  
  cat("Complete model:\n---------------\n")
  cat(form)
  cat("\n\nRestricted model:\n---------------\n")
  cat(form_r)
  cat("\n")
  if(print_res)
    texreg::screenreg(
      l = list(mod_complete, mod_restricted),
      custom.model.names = c("Complete", "Restricted")
    ) |> 
    print()
  
  tibble(var = !!variables_interest, F = F_obs, `Pr(>F)` = p_value)
}

variables_rest <- c("smdi_obs")

f_test <- NULL
for (v in c("y_obs", "y_a_obs", "h_obs", "c_obs", "i_obs", "reer_obs")) {
  f_test_tmp <- fisher_test(
    variables_rest = variables_rest,
    variables_interest = v, print_res = F
  )
  f_test <- f_test |> 
    bind_rows(f_test_tmp)
}


variables_rest <- c(
  "smdi_obs", "wy_obs", "y_obs", "y_a_obs", "h_obs", "c_obs", "i_obs", "reer_obs"
)


f_test_2 <- NULL
for (v in c("y_obs", "y_a_obs", "h_obs", "c_obs", "i_obs", "reer_obs")) {
  f_test_tmp <- fisher_test(
    variables_rest = variables_rest,
    variables_interest = v, print_res = F, contemp = F
  )
  f_test_2 <- f_test_2 |> bind_rows(f_test_tmp)
}

variables_rest <- c(
  "smdi_obs", "wy_obs", "y_obs", "y_a_obs", "h_obs", "c_obs", "i_obs", "reer_obs"
)
f_test_3 <- NULL
for (v in c("y_obs", "y_a_obs", "h_obs", "c_obs", "i_obs", "reer_obs")) {
  f_test_tmp <- fisher_test(
    variables_rest = variables_rest,
    variables_interest = v, print_res = F
  )
  f_test_3 <- f_test_3 |> bind_rows(f_test_tmp)
}


f_test |> 
  left_join(
    f_test_2 |> 
      rename(F_2 = F, `Pr(>F)_2` = `Pr(>F)`)
  ) |> 
  left_join(
    f_test_3 |> 
      rename(F_3 = F, `Pr(>F)_3` = `Pr(>F)`)
  )

# 4. IRF----

# -------------------------- #
# Impulse Response Functions #
# -------------------------- #

# Number of lags for the Impulse Response Analysis
last_lag <- 30

# Number of random draws
runs <- 10000

source("assets/irf_varest.R")
# svar_est: class svarest. Phi() function is used 
irfs <- irf(
  svar_est, 
  n.ahead = last_lag, 
  boot = FALSE, 
  cumulative = FALSE, 
  ortho = TRUE, 
  seed = 1
)


#' Returns a tibble with the IRFs values for a shocked variable
#' and the credible intervals as computed with Monte-Carlo simulations
#' 
#' @param irfs IRFs obtained with `irf_varest()`
#' @param var_shocked Name of the variable shocked in `irfs$irf`
#' @param runs No. draws for the Monte-Carlo simulations (default: 10,000)
#' 
irfs_mc <- function(var_shocked, 
                    irfs, 
                    runs = 10000) {
  sim <- vector("list", runs)
  n_variables <- ncol(irfs$irf[[var_shocked]])
  
  for (i in 1:length(sim)) {
    x <- matrix(0, ncol = n_variables, nrow = nrow(irfs$irf[[var_shocked]]))
    x[1,1] <- sqrt(abs(rnorm(n = 1, mean = 0, sd = 1))) # Generate a random shock
    sim[[i]] <- x[1,1] * irfs$irf[[var_shocked]] |> as_tibble()
    sim[[i]]$horizon <- 1:nrow(irfs$irf[[var_shocked]])
  }
  
  sim <- 
    sim |> bind_rows() |> 
    gather(var_response, value, -horizon) |> 
    group_by(horizon, var_response) |> 
    summarise(
      mean = mean(value),
      sd = sd(value),
      lower_95 = quantile(value, probs = .05),
      upper_95 = quantile(value, probs = .95),
      lower_68 = quantile(value, probs = .32),
      upper_68 = quantile(value, probs = .68),
      .groups = "drop"
    ) |> 
    mutate(var_shocked = !!var_shocked) |> 
    rename(value = mean)
  
  sim
}


# Get the IRFs and the 0.95% and 68% credible intervals
df_irfs <- 
  pbapply::pblapply(names(irfs$irf), irfs_mc, irfs = irfs) |> 
  bind_rows()

# Let us have a look at the beginning of these IRFs
df_irfs |> filter(var_shocked == "smdi_obs", var_response == "wy_obs") |> head()
df_irfs |> filter(var_shocked == "smdi_obs", var_response == "y_obs") |> head()
df_irfs |> filter(var_shocked == "smdi_obs", var_response == "h_obs") |> head()

# Some fancy names for the variables
source("../data/variables_names.R")

#' Reshape IRF table
get_df_plot <- function(df_irfs) {
  
  df_plot <- 
    df_irfs |> 
    mutate(
      title = str_c(var_shocked, "-", var_response),
      title2 = str_c(
        c_name(var_shocked, type = "pm"), " %->% ",
        c_name(var_response, type = "pm")
      ),
      title = factor(title, levels = possibilites$title)
    ) |> 
    arrange(title) |> 
    ungroup() |> 
    rename(lower = lower_95, upper = upper_95) |> 
    mutate(
      lower = as.numeric(lower),
      upper = as.numeric(upper),
      var_shocked = factor(var_shocked, levels = corresp_names$name_r),
      var_response = factor(var_response, levels = corresp_names$name_r)
    ) |> 
    arrange(var_shocked, var_response, horizon)
  
  ordered_titles <- df_plot |> pull("title2") |> unique()
  
  df_plot <- 
    df_plot |> 
    mutate(title2 = factor(title2, levels = ordered_titles))
}

df_irfs_plot <- get_df_plot(df_irfs)



# IRFs with all the shocked variables
# **Beware**: this may take a lot of time to be displayed
# And requires a very large screen
p <- ggplot(
  data = df_irfs_plot |> 
    unite(values_95, lower, upper, sep = "@") |> 
    unite(values_68, lower_68, upper_68, sep = "@") |> 
    gather(band, value_bounds, values_95, values_68) |> 
    separate(value_bounds, into = c("lower", "upper"), sep = "@", convert = T),
  mapping = aes(x = horizon, y = value)
) +
  geom_ribbon(
    mapping = aes(
      ymin = lower, ymax = upper, 
      fill = band, group = band), alpha = .5
  ) +
  geom_line(colour = "blue") +
  geom_line(aes(x = horizon, y = lower, group = band, linetype = band)) +
  geom_line(aes(x = horizon, y = upper, group = band, linetype = band)) +
  facet_wrap(~ title2, scales="free_y", labeller = label_parsed) +
  geom_hline(yintercept = 0, col = "black") +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(
    name="C.I. Level", 
    values = c("values_95" = "#1E90FF", "values_68" = "#005A9C"),
    labels = c("values_95" = "95%", "values_68" = "68%")
  ) +
  scale_linetype_manual(
    name="C.I. Level", 
    values = c("values_95" = "dashed", "values_68" = "dotted"),
    labels = c("values_95" = "95%", "values_68" = "68%")
  ) +
  theme(
    strip.background = element_rect(fill=NA),
    legend.position = "bottom",
    strip.text.x = element_text(size = 25)
  )


# It is more useful to create a function that will only display the IRFs
# for one equation

#' Displays the IRFs for one equation
#' 
#' @param df_plot (data.frame) the data containing the IRFs (obtained from get_df_plot())
#' @param shocked variable shocked
#' df_plot <- df_irfs_plot ; shocked <- "smdi_obs" ; ci <- 95
print_irf <- function(df_plot, shocked) {
  
  df_tmp <- 
    df_plot |> 
    filter(var_shocked == !!shocked) |> 
    mutate(title2 = c_name(var_response, type = "pm")) |> 
    arrange(var_shocked, var_response, horizon)
  
  ordered_titles <- df_tmp |> pull("title2") |> unique()
  
  df_tmp <- 
    df_tmp |> 
    mutate(title2 = factor(title2, levels = ordered_titles))
  
  ggplot(
    data = df_tmp |> 
      unite(values_95, lower, upper, sep = "@") |> 
      unite(values_68, lower_68, upper_68, sep = "@") |> 
      gather(band, value_bounds, values_95, values_68) |> 
      separate(value_bounds, into = c("lower", "upper"), sep = "@", convert = T),
    mapping = aes(x = horizon, y = value)
  ) +
    geom_ribbon(
      mapping = aes(ymin = lower, ymax = upper, fill = band, group = band), 
      alpha=.5
    ) +
    geom_line(colour = "blue") +
    geom_line(aes(x = horizon, y = lower, group = band, linetype = band)) +
    geom_line(aes(x = horizon, y = upper, group = band, linetype = band)) +
    facet_wrap(~title2, scales="free", labeller = label_parsed) +
    geom_hline(yintercept = 0, col = "black") +
    labs(x = NULL, y = NULL) +
    scale_fill_manual(
      name = "C.I. Level", 
      values = c("values_95" = "#1E90FF", "values_68" = "#005A9C"),
      labels = c("values_95" = "95%", "values_68" = "68%")
    ) +
    scale_linetype_manual(
      name="C.I. Level", 
      values = c("values_95" = "dashed", "values_68" = "dotted"),
      labels = c("values_95" = "95%", "values_68" = "68%")
    ) +
    theme(
      strip.background = element_rect(fill = NA),
      legend.position = "bottom",
      strip.text.x = element_text(size = 25)
    )
}


print_irf(shocked = "smdi_obs", df_plot = df_irfs_plot) + 
  labs(title = "Weather shock") +
  xlim(c(0, 20))

print_irf("i_obs", df_plot = df_irfs_plot) +
  labs(title = "Investment shock")

# Export IRFs
readr::write_csv(df_irfs, file = "var_irfs.csv")
