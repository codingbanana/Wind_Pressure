# option 1: download raw data from NCDC API using rnoaa package

install.packages("rnoaa")
library(rnoaa)
options("noaakey"="forXKnBisbvtNnFgCRBaNzINpZgSCmGe")
library(lubridate)

SetInternet2()
httr::set_config(httr::use_proxy(url="webproxy.phila.gov", port=8080))

stations <- isd_stations()
df <- stations[complete.cases(stations$lat, stations$lon), ]
df <- df[df$lat != 0, ]
# search stations within 100mi radius of philadelphia
phl <- isd_stations_search(lat = 39.938245, lon = -75.176862, radius = 160)
View(phl)

#  download the raw data, this step takes very long time to finish
#  station_Name: PHILADELPHIA INTERNATIONAL AIRPORT, PA US
# 	USAF: 999999 (1941-1972); 724080 (1973-present)
# 	WBAN: 13739
sapply(1940:1972, function (x) isd("999999","13739",x))
sapply(1973:2015, function (x) isd("724080","13739",x))

#  station Name: WILMINGTON NEW CASTLE CO AIRPORT, DE US
# 	USAF:724089 (1942/8/1-2010/7/31); 724180 (2010/8/1-present)
# 	NCDC: 13781
sapply(1942:2010, function (x) isd("724089","13781",x))
sapply(2010:2015, function (x) isd("724180","13781",x))

#  station Name: ATLANTIC CITY INTERNATIONAL AIRPORT, NJ US
# 	USAF:724070 (1947/1/1-present)
#  WBAN: 93730
sapply(1947:2015, function (x) isd("724070","93730",x))

#  process data
df <- data.frame()
# conceptual, actual data structure changes over the time therefore requires more detail adjustment
for (yr in 1940:2015){
    if (yr >1972) usaf=724080
    else usaf=999999
    wban=13739
    try({
        tmp2 <- isd(usaf,wban,yr)
        tmp2 <- as.data.frame(tmp2[1])
        df <- dplyr::rbind_all(list(df,tmp2))
        }, silent=F)
}

