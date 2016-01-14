# plotting results:
library(ggplot2)
library(reshape2)

if ("PIA_all.RData" %in% list.files()){
    if (length(ls())==0) load("PIA_all.RData")
} else {
    source("web.R")
}

## wind:
library(openair)
## make the wind rose plot and polar frequency plot
## (spd unit is converted to m/s by *0.447)
t.wd.hr <- t.wd.clean%>%
    transmute(datetime=datetime,wd=DIR,ws=SPD*0.447)
png(filename = "wind rose_hourly.png",width = 7,height = 7,units = "inch")
plot(windRose(t.wd.hr))
dev.off()
png(filename = "wind polar_hourly.png",width = 7,height = 7,units = "inch")
plot(polarFreq(t.wd.hr))
dev.off()

t.wd.dy.rose <- t.wd.daily%>%
    transmute(datetime=date,wd=DIR,ws=SPD*0.447)
png(filename = "wind rose_daily.png",width = 7,height = 7,units = "inch")
plot(windRose(t.wd.dy.rose))
dev.off()
png(filename = "wind polar_daily.png",width = 7,height = 7,units = "inch")
plot(polarFreq(t.wd.dy.rose))
dev.off()

#plot wind speed pdf by wedges
## hourly
t.wd.mean.spd <- t.wd.clean%>%
    group_by(wedge)%>%
    summarize(m.spd=mean(SPD,na.rm=T))
p.wd.pdf <- ggplot(data=t.wd.daily,
                   mapping=aes(x = SPD,fill=wedge,color=wedge))+
    geom_freqpoly()+
    geom_vline(data=t.wd.mean.spd,
               mapping = aes(xintercept=m.spd,color=wedge),
               linetype='dashed',size=1)+
    annotation_custom(grob=gridExtra::tableGrob(t.wd.mean.spd),xmin=25,xmax=35,ymin=100,ymax=1200)
ggsave("wind speed distribution by wedge.png")

## sea level vs. station level pressure
p.slp.vs.stp <- ggplot(data = t.pr.clean,mapping = aes(STP,SLP))+
    geom_point()+stat_smooth(method = 'lm',formula=y~x,size=1)+
    geom_point(mapping = aes(color='red'),data=t.pr.outlier)+
    xlab("station-level pressure (milibar)")+ylab("sea-level pressure(milibar)")+
    scale_fill_discrete(name="",labels="outlier")
ggsave("SLP vs STP.png")

## pressure pdf
t.pr.melt <- melt(data = t.pr.clean,id.vars = "datetime",variable.name = "type",value.name = "milibar")
p.pr.pdf <- ggplot(data=t.pr.melt,mapping = aes(x=milibar,color=type))+geom_freqpoly()
ggsave("pressure distribution.png")

##tide high frequency vs. low frequency
t.td.lf.hf <- inner_join(t.td.clean,t.td.lf.clean)
t.td.lf.hf.melt <- melt(t.td.lf.hf,id.vars = "datetime",variable.name = "type",value.name = "ft")
p.td.lf.hf.scatter <- ggplot(t.td.lf.hf.melt,aes(x=datetime,y=ft, color=type,group=type))+geom_line()+labs(x="date time",y="tide level (ft)")
ggsave("tide level high vs. low frequency.png")

#wind vs. tide:
wd.vs.td.clean <- inner_join(t.wd.clean,t.td.clean)
p.wd.td.hr.scatter <- ggplot(data = wd.vs.td.clean,aes(x=SPD,y=lvl))+ facet_wrap(~wedge)+geom_point(mapping = aes(color=wedge))+geom_smooth()
ggsave("wind speed vs. tide by wedge-hourly.png")

wd.vs.td.daily <- inner_join(t.wd.daily,t.td.daily)
p.wd.td.dy.scatter <- ggplot(data = wd.vs.td.daily,aes(x=SPD,y=lvl))+ facet_wrap(~wedge)+geom_point(mapping = aes(color=wedge))+geom_smooth()
ggsave("wind speed vs. tide by wedge-daily.png")

wd.vs.td.lf.clean <- inner_join(t.wd.clean,t.td.lf.clean)
p.wd.td.lf.hr.scatter <- ggplot(data = wd.vs.td.lf.clean,aes(x=SPD,y=lvl))+ facet_wrap(~wedge)+geom_point(mapping = aes(color=wedge))+geom_smooth()
ggsave("wind speed vs. tide_low_freq by wedge-hourly.png")

wd.vs.td.lf.daily <- inner_join(t.wd.daily,t.td.lf.daily)
p.wd.td.lf.dy.scatter <- ggplot(data = wd.vs.td.lf.daily,aes(x=SPD,y=lvl))+ facet_wrap(~wedge)+geom_point(mapping = aes(color=wedge))+geom_smooth()
ggsave("wind speed vs. tide_low_freq by wedge-daily.png")

pr.vs.td.clean <- inner_join(t.pr.clean,t.td.clean)
p.pr.td.hr.scatter <- ggplot(data = pr.vs.td.clean,aes(x=SLP,y=lvl))+
    geom_point()+geom_smooth()
ggsave(" Sea-level Pressure vs. tide-hourly.png")

pr.vs.td.lf.clean <- inner_join(t.pr.clean,t.td.lf.clean)
p.pr.td.lf.hr.scatter <- ggplot(data = pr.vs.td.lf.clean,aes(x=SLP,y=lvl))+geom_point()+geom_smooth()
ggsave(" Sea-level Pressure vs. tide_low_freq-hourly.png")

pr.vs.td.daily <- inner_join(t.pr.daily,t.td.daily)
p.pr.td.dy.scatter <- ggplot(data = pr.vs.td.daily,aes(x=SLP,y=lvl))+
    geom_point()+geom_smooth()
ggsave(" Sea-level Pressure vs. tide-daily.png")

pr.vs.td.lf.daily <- inner_join(t.pr.daily,t.td.lf.daily)
p.pr.td.lf.dy.scatter <- ggplot(data = pr.vs.td.lf.daily,aes(x=SLP,y=lvl))+geom_point()+geom_smooth()
ggsave(" Sea-level Pressure vs. tide_low_freq-daily.png")

wd.vs.pr.hourly <- inner_join(t.wd.clean,t.pr.clean)
p.wd.pr.hr.scatter <- ggplot(data=wd.vs.pr.hourly,aes(x=SPD,y=SLP))+facet_wrap(~wedge)+geom_point(aes(color=wedge))+geom_smooth()
ggsave("wind speed vs. sea-level pressure-hourly.png")

wd.vs.pr.daily <- inner_join(t.wd.daily,t.pr.daily)
p.wd.pr.dy.scatter <- ggplot(data=wd.vs.pr.daily,aes(x=SPD,y=SLP))+facet_wrap(~wedge)+geom_point(aes(color=wedge))+geom_smooth()
ggsave("wind speed vs. sea-level pressure-daily.png")

# wd.pr.td.daily <- inner_join(inner_join(t.wd.daily,t.pr.daily),t.td.lf.daily)
# p.wd.pr.td.tile <- ggplot(wd.pr.td.daily, aes(SPD, SLP)) +
#     geom_point(aes(color=lvl),alpha=I(0.1))+scale_color_gradient(low="blue",high="red")

# lattice::xyplot(SPD ~ lvl | wedge, data=wd.vs.td.daily,
#        grid=T,
#        type = c("p", "smooth"),
#        col.line = "darkorange",
#        # group = wedge,
#        auto.key = list(columns=nlevels(wd.vs.td.daily$wedge)),
#        lwd = 3)
#
# pres_tide <- qplot(sea_pressure, tide, data= wind_tide_all,geom= c("point", "smooth"))
# qplot(wind_spd, data=wind_tide_all, group=wedge, color=wedge, binwidth=0.1, geom = c('freqpoly'))

# ggsave("prs_td.svg",plot=pres_tide)
#
# p <- ggplot(wind_tide_all,aes(wind_spd,tide))+
#     facet_wrap(~wedge,nrow=4)+
#     geom_point(aes(color=wedge))+
#     geom_smooth(method='lm')+
#     coord_cartesian()
