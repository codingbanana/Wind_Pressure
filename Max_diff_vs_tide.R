### This script addresses Victoria's request on 2016.1.29, which includes:
### 1. explore the correlation between daily maximum atmospheric pressure changes and daily maximum tide level changes. The expectation is to show more significant trend since the amplitudes are used instead of the daily average.
### 2. explore the correlation between daily wind speed and daily tide level difference at a finer grade (5 degree per wedge, 72 wedges in total). The expectation is to show more clear picture of the correlation for each wind direction.

library(dplyr)
library(ggplot2)
if ("PIA_data.RData" %in% list.files()){
    load("PIA_data.RData")
} else {
    source("web.R")
}
## pressure vs. tide difference:
t.pr.max.diff.dy <- t.pr.clean %>%
    mutate(date=cut(datetime,breaks="1 day"))%>%
    group_by(date)%>%
    summarize(SLP.diff=max(SLP)-min(SLP))%>%
    mutate(date=as.POSIXct(date))%>%
    arrange(date)

# note the daily maximum difference is different than the daily difference, which is the daily averaged tide level difference between the day and the previous day.
t.td.max.diff.dy <- t.td.clean %>%
    mutate(date=cut(datetime,breaks="1 day"))%>%
    group_by(date)%>%
    summarize(lvl.diff=max(lvl)-min(lvl))%>%
    mutate(date=as.POSIXct(date))%>%
    arrange(date)

pr.vs.td.max.diff.dy <- inner_join(t.pr.max.diff.dy,t.td.max.diff.dy)
p.pr.td.max.diff.scatter <- ggplot(data = pr.vs.td.max.diff.dy,aes(x=SLP.diff,y=lvl.diff))+
    geom_point(alpha=0.1)+geom_smooth()+labs(x="daily max sea-level pressure change(milibar)",y="daily max tide level change (ft, NAVD88)")+stat_density2d(size=0.8, geom="contour")

## wind speed (72 wedges) vs tide difference
t.wd.daily.72wedges <- t.wd.daily %>%
    mutate(wedge=cut(ifelse(DIR>=357.5,DIR*-1,DIR),c(-Inf,seq(2.5,360,5))))%>%
    # cut() automatically included empty levels, to enforce this, specify labels to force fill empty bins.
    arrange(date)

wd.vs.td.diff.daily.72wedges <- inner_join(t.wd.daily.72wedges,t.td.diff.daily)
p.wd.td.diff.dy.scatter.72wedges <- ggplot(data = wd.vs.td.diff.daily.72wedges,aes(x=SPD,y=lvl_diff))+ facet_wrap(~wedge,ncol=4,drop = F)+geom_point(mapping = aes(color=wedge),alpha=0.3)+geom_smooth()+theme(legend.position="none")+labs(x="daily wind speed(MPH)",y="daily tide difference (ft)") # note the drop=F in facet_wrap

save(list=grep("(^p\\.)",ls(),value=T),file="PIA_max_diff.RData")
