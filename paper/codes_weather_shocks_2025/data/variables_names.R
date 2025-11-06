library(latex2exp)
library(dplyr)


corresp_names <- tribble(
  ~name_r, ~long_name, ~short_name, ~pm_name,
  "wy_obs", "Foreign Output", "$\\Delta log\\left(Y_t^*\\right)$", "hat(y)[t]^F",
  "wp_obs", "Foreign CPI Inflation", "$\\pi_t^*$", "hat(pi)[t]^F",
  "wr_obs", "Foreign Interest Rate", "r_t^*", "hat(r)[t]^F",
  "wp_a_obs", "Foreign Ag. Price Infl.", "\\Delta log \\left(p_t^{A*}\\right)$", "p[t]^D",
  "wy_a_obs", "Foreign Ag. Output", "$\\Delta log \\left(Y_t^{A*}\\right)$", "Y[t]^D",
  "oil_obs", "Crude Oil Inflation", "$oil_t$", "hat(oil)[t]",
  "y_obs", "Output", "$\\Delta log \\left(Y_t^d\\right)$", "hat(y)[t]",
  "y_a_obs", "Ag. Output", "$\\Delta log \\left(X_t^A\\right)$", "hat(y)[t]^A",
  "p_obs", "CPI Inflation", "$\\pi_t^C$", "hat(pi)[t]",
  "ratio_p_obs", "Rel. Prices", "$\\pi_{x,t}^A / \\pi_t^C$", "hat(pi)[t] / hat(pi)[t]^A",
  "p_a_obs", "Ag. Inflation", "$log \\left(\\pi_{x,t}^A\\right)$", "hat(pi)[t]^A",
  "c_obs", "Consumption", "$log \\left(c_{t}\\right)$", "hat(c)[t]",
  "h_obs", "Hours Worked", "\\Delta log \\left($h_t\\right)$", "hat(h)[t]",
  "i_obs", "Investment", "$\\Delta log \\left(i_t\\right)$", "hat(i)[t]",
  "q_obs", "Stock Prices", "q_t", "hat(q)[t]",
  "im_obs", "Imports", "\\Delta log \\left(im_{t}\\right)$", "hat(im)[t]",
  "x_obs", "Exports", "$\\Delta log \\left(x_{t}\\right)$", "hat(x)[t]",
  "tb_obs", "Trade Balance", "$tb_{t}$", "hat(tb)[t]",
  "ex_rate_obs", "Real Ex. Rate", "$rer_t$", "hat(e)[t]",
  "reer_obs", "Real Eff. Ex. Rate", "$reer_$", "hat(e)[t]",
  "r_obs", "Interest Rate", "$r_t$", "hat(r)[t]",
  "smdi_obs", "Weather", "$\\varepsilon_{t}^{W}$", "hat(s)[t]",
  "r_y_hp", "GDP Deviation From HP Filter Trend", "$y_t$", "y[t]",
  "r_y_a_hp", "agricultural GDP Deviation From HP Filter Trend", "$y_t^A$", "y[t]^A",
  "smdi", "Wheater", "$\\varepsilon_{t}^{W}$", "hat(s)[t]",
  "y_w", "Foreign Output", "$\\Delta log\\left(Y_t^*\\right)$", "hat(y)[t]^F",
  "y", "Output", "$\\Delta log \\left(Y_t^d\\right)$", "hat(y)[t]"
)

# We put the variables in a specific order when creating `corresp_names`.
# Let us make sure, by casting column `name_r` as a factor, that the order is
# preserved.
noms_ordonnees <- corresp_names$name_r
corresp_names <- 
  corresp_names |> 
  mutate(name_r = factor(name_r, levels = noms_ordonnees))

#' Returns the label of a variable by looking it up in `corresp_names`
#' @param x Variable name (column `name_r` in `corresp_names`)
#' @param type Type of desired label (`"pm"`, `"short"`, `"long"`). See details.
#' 
#' @returns A character vector with the label of the variable.
#' @details
#' The desired type corresponds to the prefix of the column names of 
#' `corresp_names`: `"pm"` for R formulas, `"short"` for LaTeX formulas, and
#' `"long"` for a string describing the variable.
#' 
#' @examples
#' c_name("wy_obs", "pm")
#' 
c_name <- function(x, type = c("pm", "short", "long")){
  type <- match.arg(type)
  type <- type %>% str_c("_name")
  ind <- match(x, corresp_names$name_r)
  corresp_names[ind, ] |> pull(type)
}

# Listing all possible subsets of 2 variables
possibilites <- expand.grid(
  Var1 = levels(corresp_names$name_r),
  Var2 = levels(corresp_names$name_r)
) |> 
  arrange(Var1, Var2) |> 
  mutate(title = str_c(Var1, "-", Var2)) |> 
  as_tibble()

size_text <- 20
library(grid)
theme_paper <- function(...)
  theme(
    text = element_text(size = size_text),
    panel.background = element_rect(fill = NA),
    axis.line = element_line(colour = "grey60"),
    axis.title = element_blank(), axis.text = element_text(), 
    legend.text = element_text(size = rel(1.1)),
    legend.title = element_text(size = rel(1.1)),
    legend.background = element_rect(), legend.position = "bottom", 
    legend.direction = "horizontal", legend.box = "vertical",
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_line(colour = "grey90"), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, size = rel(1.3), face = "bold"),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    strip.background = element_rect(fill=NA, colour = NA),
    strip.text = element_text(size = rel(1.1))
  )


theme_map_paper <- function(...)
  theme(
    text = element_text(size = size_text),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), axis.line = element_blank(),
    legend.text = element_text(size = rel(1.2)),
    legend.title = element_text(size = rel(1.2)),
    legend.background = element_rect(),
    legend.key.height	= unit(2, "line"),
    legend.key.width	= unit(3, "line"),
    strip.background = element_rect(fill=NA),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    strip.text = element_text(size = rel(1.2))
  )
