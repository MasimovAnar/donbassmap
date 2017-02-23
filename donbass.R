#devtools::install_github("briatte/tidykml")
library(ggplot2)
library(tidykml)
library(dplyr)
library(animation)
library(sp)
library(ggmap)
library(httr)
library(rvest)
library(ggrepel)
library(tidyr)
library(showtext)
library(raster)

luh <- c(	"Luhansk", "Alchevsk", "Lysychansk", "Krasnyi Luch", "Stakhanov", "Sverdlovs'k", "Rubizhne", "Rovenky") 
luhp <- c(425, 114, 103, 82, 76, 64, 63, 47)
don <- c("Donetsk", "Mariupol", "Makiivka", "Horlivka", "Kramatorsk", "Sloviansk", "Yenakiieve", "Bakhmut", "Kostiantynivka", "Krasnoarmiisk", "Debaltseve", "Volnovakha")
donp <- c(975, 461, 353, 256, 164, 117, 82, 77, 77, 64, 25, 23)
cities <- data.frame(city = c(luh, don), population = c(luhp, donp), stringsAsFactors = F) 
coord <- ggmap::geocode(c(luh, don))
cities <- cbind(cities, coord) 

polygon <- kml_polygons("/Users/Masimov/Downloads/MarineRegions-iho.kml")
#date <- as.Date("2015-January-1", format = "%Y-%B_%y")
#date2 <- as.Date("2015-February-28", format = "%d_%m_%y")
#a <- seq(date, date2, "days")

#getting death data 
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

daydata <- tidymemorybook %>% 
  group_by(day, month) %>%
  tally() %>%
  mutate(month = recode(month, `лютого` = "02", січня = "01" ),
         date = as.Date( paste0(day,"-",month,"-2015"), format = "%d-%m-%Y" )) %>%
  ungroup() %>%
  dplyr::select(date, n) 

daydata <- left_join(dt,daydata ) %>%  
  arrange(date) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  mutate(n2 = cumsum(n))


tidymemorybook2 <- tidymemorybook %>%
  filter(!(place %in% c("(помер від поранень)", "", "(помер від поранення)", "(помер від поранень)."))) %>%
  mutate(month = recode(month, `лютого` = "02", січня = "01" ),
         place = recode(place, ` Донецький аеропорт` =  "аеропорт Донецьк"),
    date = as.Date( paste0(day,"-",month,"-2015"), format = "%d-%m-%Y" )) %>%
  dplyr::select(date, place)

geocode2 <- function(i){
  request <- GET("https://geocode-maps.yandex.ru/1.x/?format=json&", 
                 query = list(geocode = i, lang="uk_UA", results = 1)) %>% content()  
  return(request$response$GeoObjectCollection$featureMember[[1]]$GeoObject$Point$pos)
}

tidymemorybook2$coord <- NA
for(i in 1:nrow(tidymemorybook2)){
  tidymemorybook2$coord[i] <- geocode2(tidymemorybook2$place[i])
}

tidymemorybook3 <- tidymemorybook2 %>%
  dplyr::select(-place) %>% separate(coord, c("lon", "lat"), sep = " " ) %>%
  right_join(dt) %>%
  mutate(lat = jitter(as.numeric(lat), amount = 0.075), lon = jitter(as.numeric(lon), amount = 0.075)) %>%
  arrange(date)


  
tidymemorybook_s <- split(tidymemorybook3, tidymemorybook3$date)


daydata_s <- split(daydata, daydata$date)

tidymemorybook_s[["2015-02-22"]]$lat <- tidymemorybook_s[["2015-02-22"]]$lat + 0.015
  
jan <- paste0("2015-january-", 1:31)
feb <- paste0("2015-february-", 1:28)
date <- c(jan, feb)
data <- data.frame()
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

ukraine <-  kml_polygons("/Users/Masimov/Downloads/doc.kml")
ukraine2 <- ukraine %>%
  filter(name == "12" | name == "44" | name == "14" | name == "63"|name == "23") %>%
  select(longitude, latitude, "region" = name)
attach(ukraine2)
donb <- ukraine %>%
  filter( name == "44" | name == "14")
S <- SpatialPoints(cbind(donb$longitude, donb$latitude))
b <- bbox(S)


l <- unique(data$date)
dd <- seq(min(ukraine2$longitude) + 4.75, max(ukraine2$longitude) + 0.8, length.out = length(l))
timeline_y <- 50.28
timeline <- data.frame(
  x = dd,
  y = timeline_y,
  day = l
)
time <- split(timeline, timeline$day)


timeline_t <- timeline %>%
  filter(day %in% as.Date(c("2015-01-13", "2015-02-12"), format("%Y-%m-%d"))) %>%
  mutate(text = c(
    "Volnovakha\nbus attack",
    "Minsk Two\nagreement"
    ))

timeline_b <- timeline %>%
  filter(day %in% as.Date(c("2015-01-21","2015-02-10","2015-02-18"), format("%Y-%m-%d"))) %>%
  mutate(text = c(
    "Retreat from\nDonetsk airport",
    "Kramatorsk\nshelling",
    "Retreat from\nDebaltseve"
  ))

plane <- readPNG('donetsk.png')
tidymemorybook_s2 <- list()
for(i in 1:length(tidymemorybook_s)){
  tidymemorybook_s2[[i]] <- do.call(rbind, tidymemorybook_s[1:i]) 
}



legend <- data.frame(x = c(40.45, 40.45,40.45,40.45), y = c(49.5, 49.3, 49.09, 48.9 ), text = c("Ukraine","Occupied\nterritories", "Donetsk\nAirport", "Place of\ndeath"))
legend2 <- data.frame(x = c(40.6, 40.8), y = c(48.9, 48.9))


  
frames <- list()
for( i in 1:length(l)){
frames[[i]] <- ggplot(data = ukraine2, aes(x = longitude, y = latitude)) +
  geom_polygon(aes(group = region), fill = "#f0ede5", color = "white", size = 0.8) +
 # geom_polygon(data = polygon, aes(x = longitude, y = latitude), fill = "skyblue") +
  geom_point(data = tidymemorybook_s2[[i]], aes(x = lon, y = lat), col = "red", size = 4, alpha = 0.2, shape = 4) +
  geom_polygon(data = data_s[[i]], aes(x = longitude, y = latitude), fill = "black", alpha = 0.25, size = 0.5) +
  coord_equal(xlim = c(b[1,1], 41), ylim = c(47.05, 50.4)) +
  geom_segment(aes(x = min(x), xend = max(x), y = min(y), yend = max(y)), data = timeline, colour = "black", size = 2) +
  geom_segment(aes(x = min(x), y = min(y), yend = max(y)), xend = time[[i]]$x,alpha=0.015, data = timeline, colour = "white", size = 3) +
  #geom_point(aes(x = x, y = y), data = time[[i]], col = "black", size = 4) +
  
  geom_point(aes(x = x, y = y), data = timeline_t, fill = "black", size = 3, shape = 25, position = position_nudge(y = c(0.05,0.05)) ) +
  geom_point(aes(x = x, y = y), data = timeline_b, fill = "black", size = 3, shape = 24, position = position_nudge(y = c(-0.05,-0.05, -0.05)) ) +
  
  geom_text(aes(x = x, y = y,label = text), data = timeline_t, size = 5.5,  nudge_y = 0.18, nudge_x = -0.04,hjust=0, family = "Merriweather") +
  geom_text(aes(x = x, y = y, label = text), data = timeline_b, size = 5.5,  position = position_nudge(y = c(-0.165, -0.165,-0.165 ), x = c(-0.46,-0.43,-0.039)), hjust=0, family = "Merriweather") +
  
  
  #geom_text((aes(x = x, y = y, label = text)), size = 6, family = "Merriweather", data = timeline2, position = position_nudge(y = c(0.16,-0.16,-0.16,0.16,-0.16))) +
  geom_text(aes(label = format(date, "%B %d,\n%Y")), data = data_s[[i]][1,],family = "Merriweather", x = 37.1, y = 49.55, size = 9 ) +
  geom_point(data = cities, aes(x = lon, y = lat, size = 2*population), shape = 1) +
  #geom_text(data = cities, aes(x = lon, y = lat, label = city), family = "Trebuchet MS",size = 5, nudge_y = 0.1, check_overlap = T ) +
  geom_text(data = cities, aes(x = lon, y = lat, label = city),
            position = position_nudge(y = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,-0.1,-0.12,0.1,0.1,0.1,0.08,0.1,-0.08,0.1,0.1,0.1,0.11,0.1), x = c(0,0.1,0.25,0,0,0,0,-0.05,0,0,0,0,-0.3,0,0.2,0,-0.1,0,0.02,0))
            ,  family = "Merriweather",size = 5.5) + 
  geom_point(data = tidymemorybook_s[[i]], aes(x = lon, y = lat), col = "black", size = 4,stroke = 0.8, shape = 4) + 
  scale_size(range = c(5, 12)) + 
  annotation_raster(plane, ymin = 48.0,ymax= 48.125,xmin = 37.629709,xmax = 37.75471) + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#f0f0f0"),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#f0f0f0"),
        plot.title = element_text(family = "Merriweather", size = 45,hjust = 0.08),
        plot.subtitle = element_text(family = "Merriweather", size = 27,hjust = 0.065),
        legend.position = "none",
        plot.margin = margin(15,0,-2,-2.5)) +
  geom_rect(aes(xmin=40.3, xmax=41.1, ymin=48.55,ymax=49.65),col = "black", alpha=1, fill="white") + 
 geom_point(data = legend[1,], aes(x,y), shape = 22, fill = "#f0ede5",stroke = 0, size = 15) +
  geom_text(data = legend[1,], aes(x+0.1,y, label = text),  size = 5, family = "Merriweather", hjust = 0 ) + 
geom_point(data = legend[2,], aes(x,y), shape = 22, fill = "black", alpha = 0.25, stroke = 0, size = 15) +
  geom_text(data = legend[2,], aes(x+0.1,y, label = text),  size = 5, family = "Merriweather",hjust = 0) +
  geom_text(data = legend[3,], aes(x+0.1,y, label = text),  size = 5, family = "Merriweather",hjust = 0) +
  annotation_raster(plane, ymin = 49.0,ymax= 49.18,xmin = 40.37,xmax = 40.54) + 
  geom_point(data = legend2[1,], aes(x,y), col = "black", size = 7,stroke = 0.8, shape = 4) +
  geom_point(data = legend2[2,], aes(x,y), col = "red", size = 7, shape = 4) +
  geom_text(data = data.frame(x = 40.365, y= 48.75), aes(x,y), label = "Place of death of one\nUkrainian soldier",  size = 4.6, family = "Merriweather",hjust = 0) +
  labs( title = "War in Eastern Ukraine", subtitle = "January - February 2015")
frames[[i]]
} 


map <- ggplot() +
  geom_polygon(data = ukraine, aes(longitude, latitude, group = name), fill = "black", col = "black") + theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#f0ede5"),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#f0ede5"),
    legend.position = "none"
  ) + 
  geom_rect(aes(xmin=b[1,1], xmax=b[1,2], ymin=b[2,1],ymax=b[2,2] + 0.4),col = "black", alpha=0.25, fill="white") +
  geom_point(data = data.frame(x = 30.52, y = 50.4501),aes(x,y), shape = 22, size = 3.5, fill = "white", col = "#f0ede5") +
  geom_text(data = data.frame(x = 30.52, y = 50.4501),aes(x,y), label = "Kyiv", size = 4.5, nudge_y = 0.4, nudge_x = -0.55, family = "Merriweather", col = "white")


saveGIF(
  {lapply(frames, print)}
  , "animationTest.gif",
  ani.width = 954, ani.height = 800
)



#A viewport taking up a fraction of the plot area

#Just draw the plot twice

