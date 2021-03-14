# Cooking dataset for INLA ------------------------------------------------
library(tidyverse)
library(sf)

remove_geom <- function(x) {
  data <- x %>% st_set_geometry(NULL)
  return(data)
}

# 1. Reading of data  -----------------------------------------------------
esco <- read_rds("../cuencas/variables/esco.rds") %>% lapply(remove_geom)
etp <- read_rds("../cuencas/variables/etp.rds") %>% lapply(remove_geom)
evi <- read_rds("../cuencas/variables/evi.rds") %>% lapply(remove_geom)
hsoil <- read_rds("../cuencas/variables/hsoil.rds") %>% lapply(remove_geom)
ndvi <- read_rds("../cuencas/variables/ndvi.rds") %>% lapply(remove_geom)
pp <- read_rds("../cuencas/variables/pp.rds") %>% lapply(remove_geom)
savi <- read_rds("../cuencas/variables/savi.rds") %>% lapply(remove_geom)
temp <- read_rds("../cuencas/variables/temp.rds") %>% lapply(remove_geom)


# 2. Cooking data ---------------------------------------------------------
esco <- esco %>%
  bind_rows() %>%
  gather(key = variable, valor, X2009Jan:X2018Dec) %>%
  filter(valor != "NA") %>%
  mutate(
    year = extract_numeric(variable),
    month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
    variable = "esco"
  )

etp <- etp %>%
  bind_rows() %>%
  gather(key = variable, valor, X2009Jan:X2018Dec) %>%
  filter(valor != "NA") %>%
  mutate(
    year = extract_numeric(variable),
    month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
    variable = "etp"
  )

evi <- evi %>%
  bind_rows() %>%
  gather(key = variable, valor, X2009Jan:X2018Dec) %>%
  filter(valor != "NA") %>%
  mutate(
    year = extract_numeric(variable),
    month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
    variable = "evi"
  )

hsoil <- hsoil %>%
  bind_rows() %>%
  gather(key = variable, valor, X2009Jan:X2018Dec) %>%
  filter(valor != "NA") %>%
  mutate(
    year = extract_numeric(variable),
    month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
    variable = "hsoil"
  )

ndvi <- ndvi %>%
  bind_rows() %>%
  gather(key = variable, valor, X2009Jan:X2018Dec) %>%
  filter(valor != "NA") %>%
  mutate(
    year = extract_numeric(variable),
    month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
    variable = "ndvi"
  )

pp <- pp %>%
  bind_rows() %>%
  gather(key = variable, valor, X2009Jan:X2018Dec) %>%
  filter(valor != "NA") %>%
  mutate(
    year = extract_numeric(variable),
    month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
    variable = "pp"
  )

savi <- savi %>%
  bind_rows() %>%
  gather(key = variable, valor, savi2009Jan:savi2018Dec) %>%
  filter(valor != "NA") %>%
  mutate(
    year = extract_numeric(variable),
    month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
    variable = "savi"
  )

temp <- temp %>%
  bind_rows() %>%
  gather(key = variable, valor, X2009Jan:X2018Dec) %>%
  filter(valor != "NA") %>%
  mutate(
    year = extract_numeric(variable),
    month = as.character(str_extract_all(variable, "[A-Z][a-z]+")),
    variable = "temp"
  )

malaria <- read_rds("../cuencas/variables/malaria_hydrobasin.rds") %>%
  gather(variable, valor, nrohab:fal:viv) %>%
  select(-codigo)
malaria <- malaria %>% select(id, Hydroname, variable, valor, year, month)
# 4. New dataset ----------------------------------------------------------
predictores <- rbind(esco, etp, evi, hsoil, ndvi, pp, savi, temp)
predictores <- predictores %>%
  mutate(month = as.integer(factor(month, levels = month.abb, labels = 1:12))) %>%
  select(-ubigeo)

tabla_a <- malaria %>%
  spread(variable, valor)

tabla_a <- tabla_a %>% mutate(
  esco = 0, etp = 0, evi = 0, hsoil = 0, ndvi = 0,
  pp = 0, savi = 0, temp = 0
)

tabla_b <- predictores %>%
  spread(variable, valor)
tabla_b <- tabla_b %>% mutate(fal = 0, nrohab = 0, viv = 0)

tabla_b <- tabla_b %>% select(
  id, Hydroname, year, month, fal, nrohab, viv,
  esco, etp, evi, hsoil, ndvi, pp, savi, temp
)

alldataset <- rbind(tabla_a, tabla_b)

alldataset <- alldataset %>%
  group_by(Hydroname, year, month) %>%
  summarise(
    fal = sum(fal, na.rm = T),
    nrohab = sum(nrohab, na.rm = T),
    viv = sum(viv, na.rm = T),
    esco = sum(esco, na.rm = T),
    etp = sum(etp, na.rm = T),
    evi = sum(evi, na.rm = T),
    hsoil = sum(hsoil, na.rm = T),
    ndvi = sum(ndvi, na.rm = T),
    pp = sum(pp, na.rm = T),
    savi = sum(savi, na.rm = T),
    temp = sum(temp, na.rm = T)
  ) %>%
  drop_na()


# 5.New id ----------------------------------------------------------------

hydrobasin <- st_read("../spdata/hydrobasin.gpkg") %>%
  mutate(new_id = 1:n())
id_unico <- unique(hydrobasin$Hydroname)
alldataset <- alldataset %>%
  mutate(new_id = as.integer(factor(Hydroname, levels = id_unico, labels = 1:19)))


# 6. Export in rds --------------------------------------------------------

saveRDS(object = alldataset, "../cuencas/final/alldataset_hydrobasin.rds")
saveRDS(object = hydrobasin, "../cuencas/final/sp_hydrobasin.rds")