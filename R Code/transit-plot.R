stations <- read.csv('/Users/vidit/Desktop/Masters/Sem_6/Advance Data Science/Project/R Code/transit.csv')

# split stations by elevation
ground <- subset(stations, GRD_ELEV == 'GROUND')
below <- subset(stations, GRD_ELEV == 'BELOW')
elevated <- subset(stations, GRD_ELEV == 'ELEVATED')
other <- subset(stations, GRD_ELEV != 'ELEVATED' & GRD_ELEV != 'BELOW' & GRD_ELEV != 'GROUND')

# map on usa
library(ggplot2)
usa <- map_data("usa")

# map each elevation
ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  coord_fixed(1.3) + geom_point(data = ground, aes(x = LONGITUDE, y = LATITUDE), color = "yellow")

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  coord_fixed(1.3) + geom_point(data = below, aes(x = LONGITUDE, y = LATITUDE), color = "red")

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  coord_fixed(1.3) + geom_point(data = elevated, aes(x = LONGITUDE, y = LATITUDE), color = "blue")

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  coord_fixed(1.3) + geom_point(data = below, aes(x = LONGITUDE, y = LATITUDE), color = "red") +
  coord_fixed(1.3) + geom_point(data = elevated, aes(x = LONGITUDE, y = LATITUDE), color = "blue")

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  coord_fixed(1.3) + geom_point(data = other, aes(x = LONGITUDE, y = LATITUDE), color = "green")

ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  coord_fixed(1.3) + geom_point(data = stations, aes(x = LONGITUDE, y = LATITUDE), color = "black")

plotRoute <- function(routeIdSys) {
  stationIds <- routes[routes$RTE_ID_SYS == routeIdSys, ]$STA_ID
  View(stationIds)
  routeStations <- apply(stationIds, 1, function(sta_id) stations[stations$STA_ID == sta_id, ])
  View(routeStations)
  ggplot() +
    geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
    coord_fixed(1.3) + geom_point(data = routeStations, aes(x = LONGITUDE, y = LATITUDE), color = "green")
}
plotRoute('BNSF - Metra')
