# Cooking dataset for the mode --------------------------------------------
library(tidyverse)
library(rgee)
library(sf)
source("utils.R")

ee_Initialize()

region <- st_read("../spdata/hydrobasin.gpkg") %>%
  sf_as_ee()

# List of variables:
# 1- Maximum temperature  (tmax)
# 2- Soil moisture (hsoil)
# 3- Runoff (esco)
# 4- Precipitation (Pp)
# 5- Evapotranspitation( ETP)
# 6- NDVI
# 7- EVI
# 8- SAVI

terraclim <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")
modis <- ee$ImageCollection("MODIS/006/MOD13Q1")
chips <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")
etp <- ee$ImageCollection("MODIS/006/MOD16A2")

# Data extract  by month
period <- seq(as.Date("2009-01-01"), as.Date("2018-12-31"), by = "1 month")

# Runoff (Escorrentía)
esco <- lapply(period,
  FUN = ts_extract_esco,
  images = terraclim,
  roi = region
)

# Evapotranspitarion
etp <- lapply(period,
  FUN = ts_extract_etp,
  images = etp,
  roi = region
)
# EVI
evi <- lapply(period,
  FUN = ts_extract_evi,
  images = modis,
  roi = region
)
# NDVI
ndvi <- lapply(period,
  FUN = ts_extract_ndvi,
  images = modis,
  roi = region
)

# Soil moisture (humedad del suelo)
hsoil <- lapply(period,
  FUN = ts_extract_hsoil,
  images = terraclim,
  roi = region
)

# Precipitation
pp <- lapply(period,
  FUN = ts_extract_pp,
  images = chips,
  roi = region
)

# Temperature
temp <- lapply(period,
  FUN = ts_extract_temp,
  images = terraclim,
  roi = region
)


# SAVI

savi <- lapply(period,
  FUN = extract_savi_index,
  images = modis,
  roi = region
)

# New list
dataset_lista <- list(esco, etp, evi, ndvi, hsoil, pp, temp, savi)

# Export dataset in rds
saveRDS(object = dataset_lista, file = "../cuencas/alldaset_hydrobasin.rds")
