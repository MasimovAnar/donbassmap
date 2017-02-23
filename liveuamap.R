#Getting polygon data from liveuamap.com
#devtools::install_github("briatte/tidykml")
library(tidyverse)
library(tidykml)

jan <- paste0("2015-january-", 1:31)
feb <- paste0("2015-february-", 1:28)
date <- c(jan, feb)
data <- data.frame()
#dowloading kml files for each day 
for(i in date){
  download.file(paste0("http://liveuamap.com/mapdata/en/", i, "/kml"), paste0(i, ".kml"))
  polygon <- kml_polygons(paste0(i, ".kml")) %>%
    filter(name == "russiainvadedukraine") %>%
    mutate(data = as.Date(i, format = "%Y-%B-%d"))
  data <- rbind.data.frame(data, polygon)
}

data <- data %>%
  select(longitude, latitude, "date" = data)

data_s <- split(data, data$date)
