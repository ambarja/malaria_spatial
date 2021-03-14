# Points to regions -------------------------------------------------------#
library(tidyverse)
library(qgisprocess)
library(sf)


# 1. Reading of Malaria-dataset -------------------------------------------

malaria <- read_rds("../spdata/malaria_basins.rds") %>% 
  st_set_geometry(NULL) %>% 
  group_by(district,year,month) %>%
  summarise(viv = sum(viv),
            fal = sum(fal),
            nrohab = sum(nrohab)
            )
district <- st_read('../spdata/district.gpkg')
hydrobasin <- st_read('../spdata/hydrobasin.gpkg')

# Malaria-district
malaria_district <- left_join(x = district,y = malaria,by = 'district') 

# Malaria-hydrobasin
point_to_region <- qgis_function(algorithm = 'native:joinattributesbylocation')
malaria_hydrobasin <- left_join(x = district,y = malaria,by = 'district') %>% 
  st_centroid()

malaria_hydrobasin <- malaria_hydrobasin %>% 
  point_to_region(
    INPUT = hydrobasin ,
    JOIN = . ,
    PREDICATE = 1,
    METHOD = 0,
    OUTPUT = qgis_tmp_vector()
    ) %>% 
  qgis_output('OUTPUT') %>% 
  st_read()
malaria_hydrobasin <- malaria_hydrobasin %>% mutate(id = 1:n())
malaria_hydrobasin <- malaria_hydrobasin %>% select(-fid_2) 
# 2. Final Malaria-dataset ------------------------------------------------

malaria_district <- malaria_district %>% st_set_geometry(NULL)
malaria_hydrobasin <- malaria_hydrobasin %>%  st_set_geometry(NULL)


# 3. Export dataset in rds ------------------------------------------------
saveRDS(object = malaria_district,
        file = '../distritos/variables/malaria_district.rds')
saveRDS(object = malaria_hydrobasin,
        file = '../cuencas/variables/malaria_hydrobasin.rds')
