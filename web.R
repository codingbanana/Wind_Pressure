# option2: download the data from NOAA website

# request data from NOAA NCEI ISD website
# download the requested data from the FTP
# open the data in text editor, fix any anomalies in the format, such as:
# non-numeric characters, missing space, NA strings, etc
# fix any data import errors first, then save the file as '_hourly.csv'
# use na.strings as one argument of read.csv to replace NA values, e.g.,
# na.strings = c("******","***","**"); na.strings = c(999,999.9,9999.9)
# use stringsAsFactors = F to aviod coersion
# use ymd_hm(paste0(wim$Date,sprintf("%04d",wim$HrMn))) to parse numbers
# to dtime e.g., 35 -> 0035; 105 -> 0105

library(lubridate)
library(dplyr)

files.list <- list.files("raw data\\",pattern="hourly.*.csv",full.names=T)
for (x in c("atl","wil")){
    tmp <- read.csv(grep(x,files.list,value=T),stringsAsFactors = F)
    tmp <- mutate(tmp,datetime=as.POSIXct(paste(tmp$Date,sprintf("%04d",tmp$HrMn)),format="%Y%m%d %H%M"))
    assign(x,tmp)
}
#pia has special format, which is handled manually
pia <- read.csv(grep('pia',files.list,value=T),stringsAsFactors = F)

