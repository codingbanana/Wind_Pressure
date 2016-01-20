## relationship between pressure difference and wind speed
## suppose to be positively correlated, result dosen't prove it.
library(dplyr)
library(ggplot2)
t.pr.diff.hr <- t.pr.clean %>%
    mutate(diff=SLP-lag(SLP))
t.wd.pr.diff.hr <- inner_join(t.wd.clean,t.pr.diff.hr)
p.wd.pr.diff.hr <- ggplot(t.wd.pr.diff.hr,aes(SPD,diff,color=wedge))+
    facet_wrap(~wedge,nrow = 2)+
    geom_point()+geom_smooth(color="blue")

t.pr.diff.dy <- t.pr.daily %>%
    mutate(diff=SLP-lag(SLP))
t.wd.pr.diff.dy <- inner_join(t.wd.daily,t.pr.diff.dy)
p.wd.pr.diff.dy <- ggplot(t.wd.pr.diff.dy,aes(SPD,diff,color=wedge))+
    facet_wrap(~wedge,nrow = 2)+
    geom_point()+geom_smooth(color="blue")