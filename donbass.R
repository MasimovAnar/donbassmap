library(tidyverse)
library(animation)
library(ggmap)
library(sp)
library(raster)
library(png)

#Donbass cities
luh <- c(	"Luhansk", "Alchevsk", "Lysychansk", "Krasnyi Luch", "Stakhanov", "Sverdlovs'k", "Rubizhne", "Rovenky") 
luhp <- c(425, 114, 103, 82, 76, 64, 63, 47)
don <- c("Donetsk", "Mariupol", "Makiivka", "Horlivka", "Kramatorsk", "Sloviansk", "Yenakiieve", "Bakhmut", "Kostiantynivka", "Krasnoarmiisk", "Debaltseve", "Volnovakha")
donp <- c(975, 461, 353, 256, 164, 117, 82, 77, 77, 64, 25, 23)
cities <- data.frame(city = c(luh, don), population = c(luhp, donp), stringsAsFactors = F) 
coord <- ggmap::geocode(c(luh, don))
cities <- cbind(cities, coord) 

#polygon of Azov sea
polygon <- kml_polygons("https://raw.githubusercontent.com/MasimovAnar/donbassmap/master/MarineRegions-iho.kml")
#polygon of Ukraine
ukraine <-  kml_polygons("https://raw.githubusercontent.com/MasimovAnar/donbassmap/master/doc.kml")
ukraine2 <- ukraine %>%
  filter(name == "12" | name == "44" | name == "14" | name == "63"|name == "23") %>%
  select(longitude, latitude, "region" = name)
donb <- ukraine %>%
  filter( name == "44" | name == "14")
S <- SpatialPoints(cbind(donb$longitude, donb$latitude))
b <- bbox(S)

#creating timeline 
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

legend <- data.frame(x = c(40.45, 40.45,40.45,40.45), y = c(49.5, 49.3, 49.09, 48.9 ), text = c("Ukraine","Occupied\nterritories", "Donetsk\nAirport", "Place of\ndeath"))
legend2 <- data.frame(x = c(40.6, 40.8), y = c(48.9, 48.9))

png <- tempfile()
download.file('http://raw.githubusercontent.com/MasimovAnar/donbassmap/master/donetsk.png', png)
plane <- readPNG(png)

#small map of Ukraine
map <- ggplot() +
  geom_polygon(data = ukraine, aes(longitude, latitude, group = name), fill = "grey50", col = "grey50") + theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    legend.position = "none",
    plot.margin = margin(0,-5,0,-3)) + 
  geom_rect(aes(xmin=b[1,1], xmax=b[1,2], ymin=b[2,1],ymax=b[2,2] + 0.4),col = "black", alpha=0.25, fill="white") +
  geom_point(data = data.frame(x = 30.52, y = 50.4501),aes(x,y), shape = 22, size = 3.5, fill = "white", col = "#f0ede5") +
  geom_text(data = data.frame(x = 30.52, y = 50.4501),aes(x,y), label = "Kyiv", size = 4.5, nudge_y = 0.6, nudge_x = -0.55, family = "Merriweather", col = "white")

#creating animation
frames <- list()
for( i in 1:length(l)){
frames[[i]] <- ggplot(data = ukraine2, aes(x = longitude, y = latitude)) +
  geom_polygon(aes(group = region), fill = "#f0ede5", color = "white", size = 0.8) +
  geom_point(data = tidymemorybook_s2[[i]], aes(x = lon, y = lat), col = "red", size = 4, alpha = 0.2, shape = 4) +
  geom_polygon(data = data_s[[i]], aes(x = longitude, y = latitude), fill = "black", alpha = 0.25, size = 0.5) +
  coord_equal(xlim = c(b[1,1], 41), ylim = c(47.05, 50.4)) +
  geom_segment(aes(x = min(x), xend = max(x), y = min(y), yend = max(y)), data = timeline, colour = "black", size = 2) +
  geom_segment(aes(x = min(x), y = min(y), yend = max(y)), xend = time[[i]]$x,alpha=0.015, data = timeline, colour = "white", size = 3) +
  geom_point(aes(x = x, y = y), data = timeline_t, fill = "black", size = 3, shape = 25, position = position_nudge(y = c(0.05,0.05)) ) +
  geom_point(aes(x = x, y = y), data = timeline_b, fill = "black", size = 3, shape = 24, position = position_nudge(y = c(-0.05,-0.05, -0.05)) ) +
  geom_text(aes(x = x, y = y,label = text), data = timeline_t, size = 5.5,  nudge_y = 0.18, nudge_x = -0.04,hjust=0, family = "Merriweather") +
  geom_text(aes(x = x, y = y, label = text), data = timeline_b, size = 5.5,  position = position_nudge(y = c(-0.165, -0.165,-0.165 ), x = c(-0.46,-0.43,-0.039)), hjust=0, family = "Merriweather") +
  geom_text(aes(label = format(date, "%B %d,\n%Y")), data = data_s[[i]][1,],family = "Merriweather", x = 37.1, y = 49.55, size = 9 ) +
  geom_point(data = cities, aes(x = lon, y = lat, size = 2*population), shape = 1) +
  geom_text(data = cities, aes(x = lon, y = lat, label = city),
            position = position_nudge(y = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,-0.1,-0.12,0.1,0.1,0.1,0.08,0.1,-0.08,0.1,0.1,0.1,0.11,0.1), x = c(0,0.1,0.25,0,0,0,0,-0.05,0,0,0,0,-0.3,0,0.2,0,-0.1,0,0.02,0)),
            family = "Merriweather",size = 5.5) + 
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
  labs( title = "War in Eastern Ukraine", subtitle = "January - February 2015") +
  annotation_custom(grob = ggplotGrob(map), xmin = 39.85, xmax = 41.18, 
                    ymin = 46.9, ymax = 47.9)
} 


saveGIF( {lapply(frames, print)} , "donbass.gif", ani.width = 954, ani.height = 800)


