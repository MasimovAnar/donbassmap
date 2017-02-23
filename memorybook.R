#Getting and processing data from memorybook.org.ua
library(tidyverse)
library(tidykml)
library(rvest)
library(httr)

#getting links for each soldier profile
links <- character()
for(i in c(1:12, 14:31)){
  tmpr <- read_html(paste0("http://memorybook.org.ua/letters/", i, ".htm")) %>%
    html_nodes("td td div p") %>% 
    html_nodes("a") %>% 
    html_attrs() %>% 
    do.call(rbind, .)
  links <- rbind(links, tmpr)
}


memorybook <- data.frame(unique(links), stringsAsFactors = F) %>%
  mutate(href = paste0("http://memorybook.org.ua", gsub("^..", "", href )))

#getting place and date of death
memorybook$dataplace <- NA
for(i in 1:nrow(memorybook)){
  memorybook$dataplace[i] <- read_html(memorybook$href[i]) %>% 
    html_nodes("td td+ td font") %>% 
    html_text() %>% .[2]
}

tidymemorybook <- memorybook %>%
  mutate(date =  sub("^.*: (.* [0-9]{4}).*", "\\1", dataplace),
         place = sub("^.*[0-9]{4}.{0,4}(.*).$", "\\1", dataplace)) %>%
  separate(date, c("day", "month", "year"), sep = " ") %>%
  filter(month %in% c("січня", "лютого")  & year == "2015") 

start <- as.Date( "01-01-2015", format = "%d-%m-%Y" )
end <- as.Date( "28-02-2015", format = "%d-%m-%Y" )
dt <- data.frame(date = seq(start, end, by = "day"))


tidymemorybook2 <- tidymemorybook %>%
  filter(!(place %in% c("(помер від поранень)", "", "(помер від поранення)", "(помер від поранень)."))) %>%
  mutate(month = recode(month, `лютого` = "02", січня = "01" ),
         place = recode(place, ` Донецький аеропорт` =  "аеропорт Донецьк"),
         date = as.Date( paste0(day,"-",month,"-2015"), format = "%d-%m-%Y" )) %>%
  dplyr::select(date, place)

#geocoding place
#code from https://github.com/andriy-gazin/yandex-geocodeR
geocode2 <- function(i){
  request <- GET("https://geocode-maps.yandex.ru/1.x/?format=json&", 
                 query = list(geocode = i, lang="uk_UA", results = 1)) %>% content()  
  return(request$response$GeoObjectCollection$featureMember[[1]]$GeoObject$Point$pos)
}

tidymemorybook2$coord <- NA
for(i in 1:nrow(tidymemorybook2)){
  tidymemorybook2$coord[i] <- geocode2(tidymemorybook2$place[i])
}

#adding jitter
tidymemorybook3 <- tidymemorybook2 %>%
  dplyr::select(-place) %>% separate(coord, c("lon", "lat"), sep = " " ) %>%
  right_join(dt) %>%
  mutate(lat = jitter(as.numeric(lat), amount = 0.075), lon = jitter(as.numeric(lon), amount = 0.075)) %>%
  arrange(date)

#splitting data by date
tidymemorybook_s <- split(tidymemorybook3, tidymemorybook3$date)

tidymemorybook_s[["2015-02-22"]]$lat <- tidymemorybook_s[["2015-02-22"]]$lat + 0.015
tidymemorybook_s[["2015-02-10"]]$lat <- tidymemorybook_s[["2015-02-10"]]$lat + 0.015

tidymemorybook_s2 <- list()
for(i in 1:length(tidymemorybook_s)){
  tidymemorybook_s2[[i]] <- do.call(rbind, tidymemorybook_s[1:i]) 
}
