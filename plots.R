# plotting results:
library(ggplot2)
library(reshape2)
library(dplyr)
library(openair)

if ("PIA_data.RData" %in% list.files()){
    load("PIA_data.RData")
} else {
    source("web.R")
}

# wind:
## make the wind rose plot and polar frequency plot
## (spd unit is converted to m/s by *0.447)
t.wd.hr.rose <- t.wd.clean%>%
    transmute(datetime=datetime,wd=DIR,ws=SPD*0.447)
p.wd.hr.rose <- windRose(t.wd.hr.rose)
p.wd.hr.polar <-polarFreq(t.wd.hr.rose)

t.wd.dy.rose <- t.wd.daily%>%
    transmute(datetime=date,wd=DIR,ws=SPD*0.447)
p.wd.dy.rose <- windRose(t.wd.dy.rose)
p.wd.dy.polar <-polarFreq(t.wd.dy.rose)

# wind speed pdf by wedges
## hourly
t.wd.mean.spd <- t.wd.clean%>%
    group_by(wedge)%>%
    summarize(m.spd=mean(SPD,na.rm=T))
p.wd.dist.by.wedge <- ggplot(data=t.wd.clean,
                   mapping=aes(x = SPD,color=wedge))+
    geom_freqpoly(binwidth=4)+
    coord_cartesian(xlim=c(0,60))+
    annotation_custom(grob=gridExtra::tableGrob(t.wd.mean.spd),xmin=40,xmax=60,ymin=10000,ymax=40000)
## daily
t.wd.dy.mean.spd <- t.wd.daily%>%
    group_by(wedge)%>%
    summarize(m.spd=mean(SPD,na.rm=T))
p.wd.dist.by.wedge.dy <- ggplot(data=t.wd.daily,
                             mapping=aes(x = SPD,color=wedge))+
    geom_freqpoly(binwidth=1)+
    annotation_custom(grob=gridExtra::tableGrob(t.wd.dy.mean.spd),xmin=25,xmax=35,ymin=100,ymax=1200)

## sea level vs. station level pressure
p.slp.vs.stp <- ggplot(data = t.pr.clean,mapping = aes(STP,SLP))+
    geom_point(aes(alpha=0.5),show.legend = F)+stat_smooth(method = 'lm',formula=y~x,size=1)+
    geom_point(mapping = aes(color='red'),data=t.pr.outlier,show.legend = F)+
    xlab("station-level pressure (milibar)")+ylab("sea-level pressure(milibar)")+
    scale_fill_discrete(name="",labels="outlier")

# pressure pdf
## hourly
t.pr.hr.melt <- melt(data = t.pr.clean,id.vars = "datetime",variable.name = "type",value.name = "milibar")
p.pr.dist.hr <- ggplot(data=t.pr.hr.melt,mapping = aes(x=milibar,color=type))+geom_freqpoly(binwidth=4)
## daily
t.pr.dy.melt <- melt(data = t.pr.daily,id.vars = "date",variable.name = "type",value.name = "milibar")
p.pr.dist.dy <- ggplot(data=t.pr.dy.melt,mapping = aes(x=milibar,color=type))+geom_freqpoly(binwidth=4)

# tide high frequency vs. low frequency
## hourly
t.td.hr <- inner_join(t.td.clean,t.td.lf.clean)
t.td.hr.melt <- melt(t.td.hr,id.vars = "datetime",variable.name = "type",value.name = "ft")
p.td.hr.line <- ggplot(t.td.hr.melt,aes(x=datetime,y=ft, color=type,group=type))+geom_line()+labs(x="date time",y="tide level (ft)")
## daily
t.td.dy <- inner_join(t.td.daily,t.td.lf.daily)
t.td.dy.melt <- melt(t.td.dy,id.vars = "date",variable.name = "type",value.name = "ft")
p.td.dy.line <- ggplot(t.td.dy.melt,aes(x=date,y=ft, color=type,group=type))+geom_line()+labs(x="date",y="tide level (ft)")

## tide diff-daily
t.td.diff.dy <- inner_join(t.td.diff.daily,t.td.lf.diff.daily)
t.td.diff.dy.melt <- melt(t.td.diff.dy,id.vars = "date",variable.name = "type",value.name = "ft")
p.td.diff.dy.line <- ggplot(t.td.diff.dy.melt,aes(x=date,y=ft, color=type,group=type))+geom_line()+labs(x="date",y="tide level difference (ft)")

# wind vs. tide:
wd.vs.td.clean <- inner_join(t.wd.clean,t.td.clean)
p.wd.td.hr.scatter <- ggplot(data = wd.vs.td.clean,aes(x=SPD,y=lvl))+ facet_wrap(~wedge,ncol=4)+geom_point(mapping = aes(color=wedge),alpha=0.1)+geom_smooth()

wd.vs.td.daily <- inner_join(t.wd.daily,t.td.daily)
p.wd.td.dy.scatter <- ggplot(data = wd.vs.td.daily,aes(x=SPD,y=lvl))+ facet_wrap(~wedge,ncol=4)+geom_point(mapping = aes(color=wedge),alpha=0.1)+geom_smooth()
## low frequency:
wd.vs.td.lf.clean <- inner_join(t.wd.clean,t.td.lf.clean)
p.wd.td.lf.hr.scatter <- ggplot(data = wd.vs.td.lf.clean,aes(x=SPD,y=lvl_lf))+ facet_wrap(~wedge,ncol=4)+geom_point(mapping = aes(color=wedge),alpha=0.1)+geom_smooth()

wd.vs.td.lf.daily <- inner_join(t.wd.daily,t.td.lf.daily)
p.wd.td.lf.dy.scatter <- ggplot(data = wd.vs.td.lf.daily,aes(x=SPD,y=lvl_lf))+ facet_wrap(~wedge,ncol=4)+geom_point(mapping = aes(color=wedge),alpha=0.1)+geom_smooth()
# pressure vs. tide:
pr.vs.td.clean <- inner_join(t.pr.clean,t.td.clean)
p.pr.td.hr.scatter <- ggplot(data = pr.vs.td.clean,aes(x=SLP,y=lvl))+
    geom_point(alpha=0.1)+geom_smooth()

pr.vs.td.lf.clean <- inner_join(t.pr.clean,t.td.lf.clean)
p.pr.td.lf.hr.scatter <- ggplot(data = pr.vs.td.lf.clean,aes(x=SLP,y=lvl_lf))+geom_point(alpha=0.1)+geom_smooth()

pr.vs.td.daily <- inner_join(t.pr.daily,t.td.daily)
p.pr.td.dy.scatter <- ggplot(data = pr.vs.td.daily,aes(x=SLP,y=lvl))+
    geom_point(alpha=0.1)+geom_smooth()

pr.vs.td.lf.daily <- inner_join(t.pr.daily,t.td.lf.daily)
p.pr.td.lf.dy.scatter <- ggplot(data = pr.vs.td.lf.daily,aes(x=SLP,y=lvl_lf))+geom_point(alpha=0.1)+geom_smooth()

#wind vs. pressure:
wd.vs.pr.hourly <- inner_join(t.wd.clean,t.pr.clean)
p.wd.pr.hr.scatter <- ggplot(data=wd.vs.pr.hourly,aes(x=SPD,y=SLP))+facet_wrap(~wedge,ncol=4)+geom_point(aes(color=wedge),alpha=0.1)+geom_smooth()

wd.vs.pr.daily <- inner_join(t.wd.daily,t.pr.daily)
p.wd.pr.dy.scatter <- ggplot(data=wd.vs.pr.daily,aes(x=SPD,y=SLP))+facet_wrap(~wedge,ncol=4)+geom_point(aes(color=wedge),alpha=0.1)+geom_smooth()

#wind vs. tide diff-daily:
wd.vs.td.diff.daily <- inner_join(t.wd.daily,t.td.diff.daily)
p.wd.td.diff.dy.scatter <- ggplot(data = wd.vs.td.diff.daily,aes(x=SPD,y=lvl_diff))+ facet_wrap(~wedge,ncol=4)+geom_point(mapping = aes(color=wedge),alpha=0.1)+geom_smooth()

wd.vs.td.lf.diff.daily <- inner_join(t.wd.daily,t.td.lf.diff.daily)
p.wd.td.lf.diff.dy.scatter <- ggplot(data = wd.vs.td.lf.diff.daily,aes(x=SPD,y=lvl_lf_diff))+ facet_wrap(~wedge,ncol=4)+geom_point(mapping = aes(color=wedge),alpha=0.1)+geom_smooth()

pr.vs.td.diff.daily <- inner_join(t.pr.daily,t.td.diff.daily)
p.pr.td.diff.dy.scatter <- ggplot(data = pr.vs.td.diff.daily,aes(x=SLP,y=lvl_diff))+geom_point(alpha=0.1)+geom_smooth()

pr.vs.td.lf.diff.daily <- inner_join(t.pr.daily,t.td.lf.diff.daily)
p.pr.td.lf.diff.dy.scatter <- ggplot(data = pr.vs.td.lf.diff.daily,aes(x=SLP,y=lvl_lf_diff))+geom_point(alpha=0.1)+geom_smooth()

save(list=grep("(^p\\.)",ls(),value=T),file="PIA_plots.RData")
