# plotting results:
library(ggplot2)
library(reshape2)
library(dplyr)

if ("PIA_all.RData" %in% list.files()){
    load("PIA_all.RData")
} else {
    source("web.R")
}
fl_name_gen <- function(x){
    tmp <- gsub("\\.","_",x)
    tmp <- gsub("wd","wind",tmp)
    tmp <- gsub("td","tide",tmp)
    tmp <- gsub("pr","pressure",tmp)
    tmp <- gsub("SLP","sea-level pressure",tmp)
    tmp <- gsub("STP","station pressure",tmp)
    tmp <- gsub("lf","low_freq",tmp)
    tmp <- gsub("^[t|p]_","",tmp)
    tmp <- gsub("(clean)|(hr)","hourly",tmp)
    tmp <- gsub("dy","daily",tmp)
    tmp <- gsub("dist","distribution",tmp)
    paste0(tmp,".png")
}

png_plot <- function(x){
    fl.name=fl_name_gen(deparse(substitute(x)))
    png(filename = fl.name,width = 12,height = 8,units="in",res=140)
    plot.new()
    plot(x)
    par(oma=c(0,0,2,0),cex.main=1)
    title(main=fl.name,outer = T,font.main=4,col.main="blue")
    dev.off()
}

## wind:
library(openair)
## make the wind rose plot and polar frequency plot
## (spd unit is converted to m/s by *0.447)
t.wd.hr.rose <- t.wd.clean%>%
    transmute(datetime=datetime,wd=DIR,ws=SPD*0.447)
p.wd.hr.rose <- windRose(t.wd.hr.rose)
png_plot(p.wd.hr.rose)

p.wd.hr.polar <-polarFreq(t.wd.hr.rose)
png_plot(p.wd.hr.polar)

t.wd.dy.rose <- t.wd.daily%>%
    transmute(datetime=date,wd=DIR,ws=SPD*0.447)
p.wd.dy.rose <- windRose(t.wd.dy.rose)
png_plot(p.wd.dy.rose)

p.wd.dy.polar <-polarFreq(t.wd.dy.rose)
png_plot(p.wd.dy.polar)

# wind speed pdf by wedges
## hourly
t.wd.mean.spd <- t.wd.clean%>%
    group_by(wedge)%>%
    summarize(m.spd=mean(SPD,na.rm=T))
p.wd.dist.by.wedge <- ggplot(data=t.wd.clean,
                   mapping=aes(x = SPD,color=wedge))+
    geom_freqpoly(binwidth=4)+
# the mean line is removed to aviod confusion (the dist is not normal)
#     geom_vline(data=t.wd.mean.spd,
#                mapping = aes(xintercept=m.spd,color=wedge),
#                linetype='dashed',size=1)+
    coord_cartesian(xlim=c(0,60))+
    annotation_custom(grob=gridExtra::tableGrob(t.wd.mean.spd),xmin=40,xmax=60,ymin=10000,ymax=40000)
#ggsave("wind speed distribution by wedge.png")
## daily
t.wd.dy.mean.spd <- t.wd.daily%>%
    group_by(wedge)%>%
    summarize(m.spd=mean(SPD,na.rm=T))
p.wd.dist.by.wedge.dy <- ggplot(data=t.wd.daily,
                             mapping=aes(x = SPD,color=wedge))+
    geom_freqpoly(binwidth=1)+
#     geom_vline(data=t.wd.mean.spd,
#                mapping = aes(xintercept=m.spd,color=wedge),
#                linetype='dashed',size=1)+
    annotation_custom(grob=gridExtra::tableGrob(t.wd.dy.mean.spd),xmin=25,xmax=35,ymin=100,ymax=1200)
png_plot(p.wd.dist.by.wedge.dy)
#ggsave("wind speed distribution by wedge.png")

## sea level vs. station level pressure
p.slp.vs.stp <- ggplot(data = t.pr.clean,mapping = aes(STP,SLP))+
    geom_point(aes(alpha=0.5),show.legend = F)+stat_smooth(method = 'lm',formula=y~x,size=1)+
    geom_point(mapping = aes(color='red'),data=t.pr.outlier,show.legend = F)+
    xlab("station-level pressure (milibar)")+ylab("sea-level pressure(milibar)")+
    scale_fill_discrete(name="",labels="outlier")
#ggsave("SLP vs STP.png")

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
