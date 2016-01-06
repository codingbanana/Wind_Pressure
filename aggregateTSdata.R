library(lattice)
library(plyr)

## Create a record and some random data for every 5 seconds
## over two days for two hosts.
dates <- seq(as.POSIXct("2011-01-01 00:00:00", tz = "GMT"),
             as.POSIXct("2011-01-02 23:59:55", tz = "GMT"),
             by = 5)
hosts <- c(rep("host1", length(dates)), rep("host2",
                                            length(dates)))
x1    <- sample(0:20, 2*length(dates), replace = TRUE)
x2    <- rpois(2*length(dates), 2)
Data  <- data.frame(dates = dates, hosts = hosts, x1 = x1,
                    x2 = x2)

## Calculate the mean for every hour using cut() to define
## the factors and ddply() to calculate the means.
## getmeans() is applied for each unique combination of the
## hosts and hour factors.
getmeans  <- function(Df) c(x1 = mean(Df$x1),
                            x2 = mean(Df$x2))
Data$hour <- cut(Data$dates, breaks = "hour")
Means <- ddply(Data, .(hosts, hour), getmeans)
Means$hour <- as.POSIXct(Means$hour, tz = "GMT")

## A plot for each host.
xyplot(x1 ~ hour | hosts, data = Means, type = "o",
       scales = list(x = list(relation = "free", rot = 90)))
