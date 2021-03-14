#-----------------------------------------------------------------------
#'Essential functions in R for working malaria database 
#'@author:Antony-Barja 
#'@Lab:InnoLab-UPCH
#-----------------------------------------------------------------------

# Functions for extract the sum of pixels of a rasterdata
extract_value_sum <- function(x,y,by = 1000){
  y_len <- y$size()$getInfo()
  for(i in seq(1,y_len,by)) { 
    index <-  i - 1
    print(sprintf("Extracting information [%s/%s]...",index,y_len))
    ee_value_layer <- ee$FeatureCollection(y) %>% 
      ee$FeatureCollection$toList(by, index) %>% 
      ee$FeatureCollection()
    if(i ==1){
      dataset <- ee_extract(
        x = x,
        fun = ee$Reducer$sum(),
        y = ee_value_layer,
        sf = T)
    } else {
      db_local <- ee_extract(x = x ,
                             y = ee_value_layer,
                             fun = ee$Reducer$sum(),
                             sf =T)
      dataset <- rbind(dataset,db_local)
    }
  }
  return(dataset)
}  

# Functions for extract the mean of pixels of a rasterdata
extract_value_mean <- function(x,y,by = 1000){
  y_len <- y$size()$getInfo()
  for(i in seq(1,y_len,by)) { 
    index <-  i - 1
    print(sprintf("Extracting information [%s/%s]...",index,y_len))
    ee_value_layer <- ee$FeatureCollection(y) %>% 
      ee$FeatureCollection$toList(by, index) %>% 
      ee$FeatureCollection()
    if(i ==1){
      dataset <- ee_extract(
        x = x,
        fun = ee$Reducer$mean(),
        y = ee_value_layer,
        sf = T)
    } else {
      db_local <- ee_extract(x = x ,
                             y = ee_value_layer,
                             fun = ee$Reducer$mean(),
                             sf =T)
      dataset <- rbind(dataset,db_local)
    }
  }
  return(dataset)
}  

# Functions for extract NDVI 
ts_extract_ndvi <- function(date, images, roi) {
  print(date)
  year <- str_sub(date, 1, 4) %>% as.numeric()
  month <- str_sub(date, 6, 7) %>% as.numeric()
  ndvi <- images$select('NDVI')$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    filter(ee$Filter$calendarRange(month, month, "month"))$
    median()$multiply(0.0001) %>% 
    ee$ImageCollection$toBands() %>% 
    ee$Image$rename(sprintf("%s%s",year,month.abb[month]))
  
  data <- extract_value_mean(ndvi, roi)
  return(data)
}


# Functions for calculate EVI

ts_extract_evi <- function(date, images, roi) {
  print(date)
  year <- str_sub(date, 1, 4) %>% as.numeric()
  month <- str_sub(date, 6, 7) %>% as.numeric()
  ndvi <- images$select('NDVI')$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    filter(ee$Filter$calendarRange(month, month, "month"))$
    median()$multiply(0.0001) %>% 
    ee$ImageCollection$toBands() %>% 
    ee$Image$rename(sprintf("%s%s",year,month.abb[month]))
  
  data <- extract_value_mean(ndvi, roi)
  return(data)
}

# SAVI
extract_savi_index <- function(date,images,roi){
  print(date)
  year <- str_sub(date, 1, 4) %>% as.numeric()
  month <- str_sub(date, 6, 7) %>% as.numeric()
  ndvi <- images$select('sur_refl_b02','sur_refl_b01')$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    filter(ee$Filter$calendarRange(month, month, "month"))$
    median()$multiply(0.0001) %>% 
    ee$ImageCollection$toBands()
  
  savi = ndvi$expression(
    '(1 + L) * float(nir - red)/ (nir + red + L)',
    list(
      'nir'= ndvi$select('0_sur_refl_b02'),
      'red'= ndvi$select('0_sur_refl_b01'),
      'L'= 0.5
    ))$rename(sprintf("%s%s%s",'savi',year,month.abb[month]))
  
  value <- extract_value_mean(savi,roi)
  return(value)
}

# ETP
ts_extract_etp <- function(date, images, roi) {
  print(date)
  year <- str_sub(date, 1, 4) %>% as.numeric()
  month <- str_sub(date, 6, 7) %>% as.numeric()
  ndvi <- images$select('PET')$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    filter(ee$Filter$calendarRange(month, month, "month"))$
    mean()$multiply(0.1) %>% 
    ee$ImageCollection$toBands() %>% 
    ee$Image$rename(sprintf("%s%s",year,month.abb[month]))
  
  data <- extract_value_mean(ndvi, roi)
  return(data)
}


# Escorrentía

ts_extract_esco <- function(date, images, roi) {
  print(date)
  year <- str_sub(date, 1, 4) %>% as.numeric()
  month <- str_sub(date, 6, 7) %>% as.numeric()
  ndvi <- images$select('ro')$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    filter(ee$Filter$calendarRange(month, month, "month"))$
    mean() %>% 
    ee$ImageCollection$toBands() %>% 
    ee$Image$rename(sprintf("%s%s",year,month.abb[month]))
  
  data <- extract_value_mean(ndvi, roi)
  return(data)
}


# Soil humid
ts_extract_hsoil <- function(date, images, roi) {
  print(date)
  year <- str_sub(date, 1, 4) %>% as.numeric()
  month <- str_sub(date, 6, 7) %>% as.numeric()
  ndvi <- images$select('soil')$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    filter(ee$Filter$calendarRange(month, month, "month"))$
    mean()$multiply(0.1) %>% 
    ee$ImageCollection$toBands() %>% 
    ee$Image$rename(sprintf("%s%s",year,month.abb[month]))
  
  data <- extract_value_mean(ndvi, roi)
  return(data)
}


# Function for extract temperature
ts_extract_temp <- function(date, images, roi) {
  print(date)
  year <- str_sub(date, 1, 4) %>% as.numeric()
  month <- str_sub(date, 6, 7) %>% as.numeric()
  ndvi <- images$select('tmmx')$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    filter(ee$Filter$calendarRange(month, month, "month"))$
    mean()$multiply(0.1) %>% 
    ee$ImageCollection$toBands() %>% 
    ee$Image$rename(sprintf("%s%s",year,month.abb[month]))
  
  data <- extract_value_mean(ndvi, roi)
  return(data)
}


# CHIPRS  by month
ts_extract_pp <- function(date, images, roi) {
  print(date)
  year <- str_sub(date, 1, 4) %>% as.numeric()
  month <- str_sub(date, 6, 7) %>% as.numeric()
  ndvi <- images$select('precipitation')$
    filter(ee$Filter$calendarRange(year, year, "year"))$
    filter(ee$Filter$calendarRange(month, month, "month"))$
    mean()%>% 
    ee$ImageCollection$toBands() %>% 
    ee$Image$rename(sprintf("%s%s",year,month.abb[month]))
  
  data <- extract_value_mean(ndvi, roi)
  return(data)
}

# Remove geometry 
remove_geometry <- function(x){
  a = st_set_geometry(x = x,value = NULL)
  return(a)
}
