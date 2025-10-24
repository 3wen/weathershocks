This repository provides all materials needed to reproduce the analyses from the paper

Gallic, E. and Vermandel, G. "*Weather Shocks*" (2020)
_European Economic Review_, 124, 103409.
<https://doi.org/10.1016/j.euroecorev.2020.103409>

It also contains **complementary materials** developed for a hands-on session of the
[Network of Central Banks and Supervisors for Greening the Financial System (NGFS)](https://www.ngfs.net/en)
Macro Modelling Initiative, organized by Benjamin Alford and Helena Herber 
(Banque de France).


# Replication Package

- [📕 Author's version of _Weather Shocks_](paper/Gallic_Vermandel-Weather Shocks_2020.pdf)
- [📝 Technical Appendix](paper/Technical-Appendix.pdf)
- [📦 Replication codes](paper/codes_weather_shocks.zip) (Zip archive)

Software requirements:

- R (≥ 4.0.0) with a few packages (tidyverse, lubridate, dplyrExtras)
- Dynare (≥ 4.6) interfaced with MATLAB

# Session 1: Reproducing the paper "Weather Shocks"

The objective of the first session is to walk participants through the main 
methodology used in the paper and to show how to reproduce each component of the 
analysis step by step.

- Slides: coming soon,
- Code: download the [replication codes](paper/codes_weather_shocks.zip) to
  follow along.

Topics covered (broadly):

- Construction of weather shock variables from weather station data
- Econometric estimation with a structural VAR
- Integration of weather shocks into a dynamic stochastic general equilibrium 
  (DSGE) framework

# Session 2: Extending the framework

The second session is designed as a broader training exercise.
Its goal is to help participants adapt the methodology to new contexts or datasets.

Provided tutorials cover:

- How to download and process gridded weather data to build relevant climate indicators
- How to develop DSGE models including:

  - An agricultural sector,
  - An exogenous weather shock affecting the production function of that sector

- [Notebooks for weather data](https://3wen.github.io/weathershocks).
