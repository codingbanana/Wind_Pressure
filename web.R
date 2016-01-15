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
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

rm(list=ls())

# # import atl and wil data (temporarily suspended)
# files.list <- list.files("raw data\\",pattern="hourly.*.csv",full.names=T)
# for (x in c("atl","wil")){
#     tmp <- read.csv(grep(x,files.list,value=T),stringsAsFactors = F)
#     tmp <- mutate(tmp,datetime=as.POSIXct(paste(tmp$Date,sprintf("%04d",tmp$HrMn)),format="%Y%m%d %H%M"))
#     assign(x,tmp)
# }

if("PIA_raw_parsed.RData" %in% list.files()){
        load("PIA_raw_parsed.RData")
    } else {
        # parse raw data from downloaded file(space delimited)
        pia.file <- "raw data\\PIA_1940_Present_Hourly.txt"
        pia <- read.fwf(pia.file, widths=c(6,-1,5,-1,12,-1,3,-1,3,-1,3,-1,3,-1,3,-1,1,-1,1,-1,1,-1,4,-1,11,-1,11,-1,1,-1,9,-1,6,-1,5,-1,6,-1,3,-1,3,-1,5,-1,5,-1,5,-1,5,-1,2),buffersize = 100000,as.is=T)
        # add table header
        pia.colnames <- as.character(unlist(pia[1,]))
        pia.colnames <- gsub("^ *","",pia.colnames)
        colnames(pia) <- pia.colnames
        pia <- pia[-1,]
        # save import data as 'PIA_raw_parsed.RData' for quicker processing
        save(pia,file = "PIA_raw_parsed.RData")
    }
#remove non-numeric values, coerce data type, split wind and pressure data
t <- pia %>%
    mutate(datetime=lubridate::ymd_hm(`YR--MODAHRMN`))%>%
    select(datetime,DIR,SPD,SLP,STP)
# wind data: datetime, DIR, SPD
t.wd <- t%>%
    filter(!(DIR %in% c("990","***")|SPD =="***"))%>%
    select(datetime,DIR,SPD)%>%
    mutate(DIR=as.numeric(DIR),SPD=as.numeric(SPD))%>%
    arrange(datetime)
# pressure data: datetime, SLP, SPD
t.pr <- t%>%
    select(datetime,SLP,STP)%>%
    #NAs introduced by coercion, not important
    mutate(SLP=as.numeric(SLP),STP=as.numeric(STP))%>%
    filter(!((is.na(SLP))&(is.na(STP))))%>%
    arrange(datetime)
# check slp vs. stp data, remove outliers
## first, plot SLP vs. STP as a fitted scatter plot
# slp.vs.stp <- ggplot(data = t.pr,mapping = aes(STP,SLP))+
#     geom_point()+stat_smooth(method = 'lm',formula=y~x,size=1)
## second, get the regression coefficients
lm.coef <- coef(lm(formula = SLP~STP,data = t.pr))
## third, remove significant outliers (visually)
t.pr.clean <- t.pr%>%
    filter(SLP < lm.coef[2]*STP+lm.coef[1]*3)
t.pr.outlier <- t.pr%>%
    filter(SLP > lm.coef[2]*STP+lm.coef[1]*3)
## the plot is moved to plots.R for better logic
# p.slp.vs.stp <- ggplot(data = t.pr.clean,mapping = aes(STP,SLP))+
#     geom_point()+stat_smooth(method = 'lm',formula=y~x,size=1)+
#     geom_point(mapping = aes(color='red'),data=t.pr.outlier)+
#     xlab("station-level pressure (milibar)")+ylab("sea-level pressure(milibar)")+
#     scale_fill_discrete(name="",labels="outlier")

# aggregate hourly data to daily data
## first, add a column 'day' as a grouping factor
## next, summarize grouped data:
### method1: base:: cut->split->lapply->rbind
#do.call(rbind,lapply(split(t.wd,t.wd$day),function(x)mean(x$DIR,na.rm = T)))
### method2: base:: aggregate
#aggregate(t.wd,list(t.wd$day),mean,na.rm=T)
### method3: dplyr:: group_by + summarize, summarize_each

# add wedges (45degree each, compass direction): since DIR is a circular degree, and the north is (-22.5,22.5], we need to group the largest half wedge (DIR>337.5) with the smallest half wedge (DIR<22.5). Alternatively, we can assign two temp names for north, e.g., n1, n2, then subseting the columns and assign a value to those subsets, and R will treat the levels indifferently
t.wd.clean <- t.wd%>%
    mutate(wedge=cut(ifelse(DIR>=337.5,DIR*-1,DIR),c(-Inf,seq(22.5,360,45)),
                     labels = c('N','NE','E','SE','S','SW','W','NW')))%>%
    arrange(datetime)

t.wd.daily <- t.wd%>%
    mutate(date=cut(datetime,breaks="1 day"))%>%
    group_by(date)%>%
    summarize(DIR=mean(DIR),SPD=mean(SPD))%>%
    mutate(date=as.POSIXct(date))%>%
    #categorize DIR into 8 wedges
    mutate(wedge=cut(ifelse(DIR>=337.5,DIR*-1,DIR),c(-Inf,seq(22.5,360,45)),
                     labels = c('N','NE','E','SE','S','SW','W','NW')))%>%
    arrange(date)

t.pr.daily <- t.pr.clean%>%
    mutate(date=cut(datetime,breaks="1 day"))%>%
    group_by(date)%>%
    summarize(SLP=mean(SLP,na.rm=T),STP=mean(STP,na.rm=T))%>%
    mutate(date=as.POSIXct(date))%>%
    arrange(date)

# import synthetic-filled tide hourly data (from victoria)
## high frequency
t.td <- read.csv('raw data\\tide.csv',as.is = T)
t.td.clean <- t.td %>%
    select(datetime=`Date.Time..EST.`,lvl=`Water.Level..ft.from.NAVD88.`)%>%
    mutate(datetime=lubridate::mdy_hm(datetime),lvl=as.numeric(lvl))
t.td.daily <- t.td.clean%>%
    mutate(date=cut(datetime,breaks="1 day"))%>%
    group_by(date)%>%
    summarise(lvl=mean(lvl,na.rm=T))%>%
    mutate(date=as.POSIXct(date))
## low frequency
t.td.lf <- read.csv('raw data\\Fill_LP_old.csv', as.is = T)
t.td.lf.clean <- t.td.lf %>%
    select(5:6)%>%
    rename(datetime=Datetime,lvl_lf=LP_SyntheticData)%>%
    mutate(datetime=lubridate::mdy_hm(datetime),
           lvl_lf=as.numeric(lvl_lf))%>%
    arrange(datetime)
t.td.lf.daily <- t.td.lf.clean%>%
    mutate(date=cut(datetime,breaks="1 day"))%>%
    group_by(date)%>%
    summarize(lvl_lf=mean(lvl_lf,na.rm=T))%>%
    mutate(date=as.POSIXct(date))

## calculate the tide difference as compared to the day before
t.td.diff.daily <- t.td.daily%>%
    transmute(date=date,lvl_diff=lvl-lag(lvl))%>%
    filter(!is.na(lvl_diff))
t.td.lf.diff.daily <- t.td.lf.daily%>%
    transmute(date=date,lvl_lf_diff=lvl_lf-lag(lvl_lf))%>%
    filter(!is.na(lvl_lf_diff))

# save the cleaned, hourly/daily, wind, pressure, tide data(low/high frequency)
save(list=grep("(^t\\.)",ls(),value=T),file="PIA_data.RData")
