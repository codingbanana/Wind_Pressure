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
library(ggplot2)
library(lubridate)

# # import atl and wil data (temporarily suspended)
# files.list <- list.files("raw data\\",pattern="hourly.*.csv",full.names=T)
# for (x in c("atl","wil")){
#     tmp <- read.csv(grep(x,files.list,value=T),stringsAsFactors = F)
#     tmp <- mutate(tmp,datetime=as.POSIXct(paste(tmp$Date,sprintf("%04d",tmp$HrMn)),format="%Y%m%d %H%M"))
#     assign(x,tmp)
# }
pia.obj<- "PIA_raw_parsed.RData"
if ("pia" %in% ls()){
} else if(pia.obj %in% list.files()){
        load(pia.obj)
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
    mutate(SLP=as.numeric(SLP),STP=as.numeric(STP))%>%
    filter(!((is.na(SLP))&(is.na(STP))))%>%
    arrange(datetime)
# check slp vs. stp data, remove outliers
## first, plot SLP vs. STP as a fitted scatter plot
slp.vs.stp <- ggplot(data = t.pr,mapping = aes(STP,SLP))+
    geom_point()+stat_smooth(method = 'lm',formula=y~x,size=1)
## second, get the regression coefficients
lm.coef <- coef(lm(formula = SLP~STP,data = t.pr))

## third, remove significant outliers
t.pr.clean <- t.pr%>%
    filter(SLP < lm.coef[2]*STP+lm.coef[1]*3)
slp.vs.stp.clean <- ggplot(data = t.pr.clean,mapping = aes(STP,SLP))+
    geom_point()+stat_smooth(method = 'lm',formula=y~x,size=1)

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
t.td <- read.csv('tide.csv',as.is = T)
t.td.clean <- t.td %>%
    select(datetime=`Date.Time..EST.`,lvl=`Water.Level..ft.from.NAVD88.`)%>%
    mutate(datetime=lubridate::mdy_hm(datetime),lvl=as.numeric(lvl))
t.td.daily <- t.td.clean%>%
    mutate(date=cut(datetime,breaks="1 day"))%>%
    group_by(date)%>%
    summarise(lvl=mean(lvl,na.rm=T))%>%
    mutate(date=as.POSIXct(date))

wd.vs.td.clean <- inner_join(t.wd.clean,t.td.clean)
wd.vs.td.daily <- inner_join(t.wd.daily,t.td.daily)
pr.vs.td.clean <- inner_join(t.pr.clean,t.td.clean)
pr.vs.td.daily <- inner_join(t.pr.daily,t.td.daily)

wd.td.hourly.plot <-
    ggplot(data = wd.vs.td.daily,mapping = aes(SPD , lvl))+geom_point(mapping = aes(color=wedge))+geom_smooth()

lattice::xyplot(SPD ~ lvl | wedge, data=wd.vs.td.daily,
       grid=T,
       type = c("p", "smooth"),
       col.line = "darkorange",
       # group = wedge,
       auto.key = list(columns=nlevels(wd.vs.td.daily$wedge)),
       lwd = 3)

pres_tide <- qplot(sea_pressure, tide, data= wind_tide_all,geom= c("point", "smooth"))
qplot(wind_spd, data=wind_tide_all, group=wedge, color=wedge, binwidth=0.1, geom = c('freqpoly'))
ggsave("prs_td.svg",plot=pres_tide)

p <- ggplot(wind_tide_all,aes(wind_spd,tide))+
    facet_wrap(~wedge,nrow=4)+
    geom_point(aes(color=wedge))+
    geom_smooth(method='lm')+
    coord_cartesian()
p+theme_grey()
p+theme_minimal()
p+theme_void()
autoplot(wind_tide_all)
