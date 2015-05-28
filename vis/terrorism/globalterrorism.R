setwd("/Users/chengjun/百度云同步盘/复旦大学计算新闻培训/data/")

gc()
# rm(list=ls())

data = read.csv("./globalterrorismdb_0814small.csv", sep = ';', quote = '"', dec = ".",
                  header = T, stringsAsFactors = T)
data$num = 1
#year country, nkill, nwound
dat = aggregate(data$num, 
                    by=list(data$iyear, data$country_txt, data$nkill, 
                            data$nwound, data$attacktype1_txt), 
                    FUN=sum)
dat$nwound = dat$nwound+1
names(dat) = c("year", "country", "nkill", "nwound", "type", "num")

library(googleVis)

dat = aggregate(data$num, 
                by=list(data$iyear, data$country_txt ), 
                FUN=sum)


dkill = aggregate(data$nkill, 
                by=list(data$iyear, data$country_txt  ), 
                FUN=sum)

dwound = aggregate(data$nwound, 
                  by=list(data$iyear, data$country_txt ), 
                  FUN=sum)

names(dat) = c("year", "country", "num")
names(dkill) = c("year", "country", "nkill")
names(dwound) = c("year", "country", "nwound")

dat = merge(dat, dkill, by=c("year","country"))
dat = merge(dat, dwound, by=c("year","country"))

names(dat) = c("Year", "Country", "Events", "Kills", "Wounds")
dat$Wounds.Log = log(dat$Wounds + 1)
#  The data must have rows with unique combinations of idvar and timevar.
GM <- gvisMotionChart(dat,
                     idvar="Country", timevar="Year",
                     xvar="Kills", yvar="Events",
                     colorvar="Wounds.Log", sizevar="Wounds.Log",
                     options=list(width=700, height=600),
                     chartid="GlobalTerrorism")
## Display the chart in the browser
plot(GM)


write.csv(dat, './globalterrismdata2013.csv', row.names =)
####################
#
####################
library(googleVis)
demo(WorldBank)


library(WDI)

inds <- c('SP.DYN.TFRT.IN','SP.DYN.LE00.IN', 'SP.POP.TOTL',
                      'NY.GDP.PCAP.CD', 'SE.ADT.1524.LT.FE.ZS')

indnams <- c("fertility.rate", "life.expectancy", "population",
                             "GDP.per.capita.Current.USD", "15.to.25.yr.female.literacy")

wdiData <- WDI(country="all", indicator=inds,
                                 start=1960, end=format(Sys.Date(), "%Y"), extra=TRUE)

colnum <- match(inds, names(wdiData))

names(wdiData)[colnum] <- indnams

## Create a motion chart
library(googleVis)

WorldBank <- droplevels(subset(wdiData, !region %in% "Aggregates"))

M <- gvisMotionChart(WorldBank,
                     idvar="country", timevar="year",
                     xvar="life.expectancy", yvar="fertility.rate",
                     colorvar="region", sizevar="population",
                     options=list(width=700, height=600),
                     chartid="WorldBank")

## Display the chart in the browser
plot(M)