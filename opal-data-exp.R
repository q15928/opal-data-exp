library(data.table)
library(stringr)
library(lubridate)
library(leaflet)
library(leaflet.extras)

wd <- "/Users/jasonfeng/jasonf/src/opal-data-exp/"
setwd(wd)

station_loc_file_path <- paste0(wd, "data/StationEntrances2018.csv")

station_loc_dt <- fread(station_loc_file_path)

idx <- station_loc_dt[, .I[1], by=Train_Station]

station_loc_dt <- station_loc_dt[idx$V1, c(1, 5, 6)]
names(station_loc_dt) <- c("station", "lat", "long")

opal_tap_file_path <- paste0(wd, "data/opal_train_tap.csv")
opal_tap_dt <- fread(opal_tap_file_path, header = FALSE)
names(opal_tap_dt) <- c("mode", "timestamp_str", "station", "count")

# remove "Station"
p <- "(.+) Station"
opal_tap_dt$station <- str_match(opal_tap_dt$station, p)[, 2]

opal_dt <- merge(opal_tap_dt, station_loc_dt, by=c("station"))
opal_dt$timestamp <- ymd_hms(opal_dt$timestamp_str, tz="Australia/Sydney")

# plot all the counts on a map
yy <- merge(opal_tap_dt, station_loc_dt, by=c("station"))
yy <- yy[, list(count = sum(count)), by=c("station", "lat", "long")]
yy <- opal_dt[timestamp == ymd_hms("2016-12-28 17:00:00", tz="Australia/Sydney"),]
yy <- opal_dt[timestamp >= ymd_hms("2016-12-28 00:00:00", tz="Australia/Sydney") &
                timestamp <= ymd_hms("2016-12-28 23:00:00", tz="Australia/Sydney")]
yy <- yy[(lat < -33.63) & (lat > -34.08) & (long > 150.8) & (long < 153.1),]

#################################
leaflet(yy) %>% 
  addProviderTiles(providers$CartoDB.Positron)  %>%
  addHeatmap(lng = ~long, lat = ~lat, 
             intensity = ~count/10000, blur = 20, max=0.05, radius = 15)

pal <- colorNumeric("RdYlBu", domain = yy$count)
leaflet(yy) %>%
  addProviderTiles(providers$CartoDB.Positron)  %>%
  # addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
  addCircles(col = ~pal(count), opacity = 0.9) %>%
  addLegend(pal = pal, values = ~count)


##############################
library(sp)
library(tmaptools)
library(tmap)
library(OpenStreetMap)

coordinates(yy) <- ~long + lat
yy <- set_projection(yy, current.projection = "longlat")
bb_config <- st_bbox(yy)
tmap_mode("view")
tm_shape(yy, bbox = bb_config) + 
  tm_symbols(size = 0.07, col = "count", n = 6)

### animation - not working
carto <- "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}{r}.png"
facet_anim = tm_basemap(carto) + tm_shape(yy, bbox = bb_config) +
  tm_symbols(size = 0.07, col = "count", n = 6) +
  tm_facets(free.scales.fill = FALSE, ncol = 1, nrow = 1, along = "timestamp") +
  tm_layout(legend.position = c("right", "top"))
tmap_animation(tm = facet_anim, delay = 80, filename = "opal_train_2016-12-28.gif")

#### qtm
qtm(yy, symbols.col = "count", symbols.size = 0.3, n=12)
yy_osm <- read_osm(yy, ext=1.2)
qtm(yy_osm) + qtm(yy, symbols.col = "count", symbols.size = 0.07, n=6)
# tm_shape(yy_osm) + tm_symbols(col="count", n=10)

#############################
library(ggplot2)
library(maps)
library(ggthemes)

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 
