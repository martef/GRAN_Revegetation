#Packages needed
``` 
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes) #usikker
library(hydroTSM)
library(zoo)
library(lubridate)

## Visualize local weather data

Data retrieved from aklima.no from weather stations around Namsos and Åfjord (two near Namsos and 1 near Åfjord)

#Upload datasets

klima_afjord <- readr::read_delim('C:/Users/martef/DokumenterIntern/GitHub/GRAN_Revegetation/Data/klimadata_2019-2023_afjord.csv', 
                                  delim = ';',
                                  locale = locale('se', encoding = 'ISO8859-1'),
                                  col_names = TRUE)
klima_namsos <- readr::read_delim('C:/Users/martef/DokumenterIntern/GitHub/GRAN_Revegetation/Data/klimadata_2019-2023_namsos.csv', 
                                  delim = ';',
                                  locale = locale('se', encoding = 'ISO8859-1'),
                                  col_names = TRUE)                          

#Denne funksjonen får til å laste inn datasettene med spesialkarakterer

#Alle tall er registert som characters i Namsos-settet. Disse må gjøres om.

klima_afjord <- klima_afjord %>% mutate_at(c('Mean_temp'), as.numeric)
klima_namsos <- klima_namsos %>% mutate_at(c('Mean_temp', 'Max_temp', 'Min_temp', 'Precipitation'), as.numeric)

#Det er ingen nedbørsdata registrert i Namsos-datasettet. Må se på klimadataene på seklima.no på nytt

###Plot daily precipitation and temperature##
afjord <- ggplot(klima_afjord, aes(x = Time, y = Precipitation))
afjord + geom_col()

plot(klima_afjord$Time, klima_afjord$Precipitation, type="l") #works, but have one strange zigzagging line going through it

afjord.ts = zoo(klima_afjord$Precipitation, order.by= klima_afjord$Time)
hydroplot(afjord.ts, var.type="Precipitation", var.unit="mm", xlab="Year", ylab="Precipitation (mm)")

#Plot monthly precipitation
afjord.monthly <- aggregate(afjord.ts, as.yearmon(index(afjord.ts)), sum)
afjord.monthly
df_afjord.monthly <-fortify.zoo(afjord.monthly, names=c("Month")) #How to convert a zoo file to a data frame with normal columns and headers
df_afjord.monthly <-rename(df_afjord.monthly, Precipitation=afjord.monthly)
plot_afjord.monthly <- ggplot(df_afjord.monthly, aes(x = Month, y = Precipitation))
plot_afjord.monthly + geom_col()


#monthly mean precipitation
klima_afjord$month <- month(klima_afjord$Time)
klima_afjord %>%
  group_by(month) %>%
  summarise_at(vars(Precipitation), list(name = mean)) #gives only the daily mean per month

klima_afjord$Month <- floor_date(klima_afjord$Time, "month")
klima_afjord %>%
  group_by(Month) %>%
  summarize(sum=sum(Precipitation))
#This does the same as the yearmon-aggregation

df_afjord.monthly$Year <-as.numeric(format(df_afjord.monthly$Month, "%Y"))
df_afjord.monthly$Month2 <-as.numeric(format(df_afjord.monthly$Month, "%m"))
df_afjord.monthly %>%
  group_by(Month2) %>%
  summarise_at(vars(Precipitation), list(name = mean))

#Month2  name
#<dbl> <dbl>
#1      1 209. 
#2      2 156. 
#3      3 170. 
#4      4  98.0
#5      5 110. 
#6      6  97.1
#7      7 137. 
#8      8 156. 
#9      9 216. 
#10    10 187. 
#11    11 143. 
#12    12 156. 

#Mean per month over the period Jan 2019 to August 2023

#Plot quartlerly precipitation
afjord.qtr.sum <- aggregate(afjord.ts, as.yearqtr(index(afjord.ts)), sum)
afjord.qtr.sum
df_afjord.qtr.sum <-fortify.zoo(afjord.qtr.sum, names=c("Month")) #How to convert a zoo file to a data frame with normal columns and headers
df_afjord.qtr.sum <-rename(df_afjord.qtr.sum, Precipitation=afjord.qtr.sum)
plot_afjord.qtr.sum <- ggplot(df_afjord.qtr.sum, aes(x = Month, y = Precipitation))
plot_afjord.qtr.sum + geom_col()

#yearly precipitation
klima_afjord$Year <- floor_date(klima_afjord$Time, "year")
klima_afjord %>%
  group_by(Year) %>%
  summarize(sum=sum(Precipitation))

# Year         sum
#<date>     <dbl>
#  1 2019-01-01 1639 
#  2 2020-01-01 1827.
#  3 2021-01-01 2025.
#  4 2022-01-01 1984.
#  5 2023-01-01  839.
