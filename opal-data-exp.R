library(data.table)
library(stringr)
library(lubridate)

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

# plot all the counts on a map
yy <- merge(opal_tap_dt, station_loc_dt, by=c("station"))

yy <- yy[, list(count = sum(count)), by=c("station", "lat", "long")]
