# This file is used to create an R data object representing NZ maps:
# - nz_df_regions.rda
# It is a tibble that contains coordinates of points
# to delimit polygons for each NZ region.

# Last update : October 2025

library(tidyverse)
library(sf)


# Source: Statistics NZ
# Place the extracted .gdb folder in in the current working directory.
# http://www3.stats.govt.nz/digitalboundaries/annual/ESRI_Geodatabase_2015_Digital_Boundaries_Generalised_Clipped.zip
nz_df <- st_read(
  dsn = "2015 Digital Boundaries Generalised Clipped.gdb/", 
  layer = "REGC2015_GV_Clipped"
)
nz_df <- st_transform(nz_df, 4167)  # EPSG:4167

nz_df_simplified <- nz_df |> st_simplify(dTolerance = 5e2) # 5000m

# Bounding box
bbox <- st_bbox(c(
  xmin = 162,
  xmax = 180,
  ymin = -50,
  ymax = -32
), crs = 4167) |> st_as_sfc()

# Clip map to keep regions within the bounding box
nz_clipped <- st_intersection(nz_df_simplified, bbox)


regions <- 
  c("Northland", "Auckland", "Waikato", "Bay of Plenty", "Gisborne",
    "Hawke's Bay", "Taranaki", "Manawatu-Wanganui", "Wellington",
    "West Coast", "Canterbury", "Otago", "Southland", "Marlborough", "Tasman/Nelson"
  )


nz_df_regions <- 
  nz_clipped |> 
  mutate(
    region = str_replace(REGC2015_NAME, " Region$", ""),
    region = ifelse(
      # In the agricultural data, these two are merged
      region %in% c("Tasman", "Nelson"),
      yes = "Tasman/Nelson", no = region
    )
  )

ggplot(data = nz_df_regions) + 
  geom_sf(mapping = aes(fill = region))

# Export the data
save(nz_df_regions, file = "nz_df_regions.rda")