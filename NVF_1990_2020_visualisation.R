"""
This script will take the shares presented in T&E '21 and the NVF I calculated from 2020-2050
"""

df <- read.csv("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/openENTRANCE projection/ACEA - New Vehicle Registration/Historical Vehicle Registration 1990-2020V2.csv")
countries = df[1]
remove= c(              'efta',
            'estonia',
 'eu (new members)2',
         'eu + efta',
    'eu + efta + uk',
            'eu-151',
       'eu15 + efta',
    'european union',
   'european union3')
df =  df[ ! df$country %in% remove, ]

#rownames(df) = df$country
#df = subset(df, select= -c(country))
colnames(df) = c("country","region",seq(1990, 2020))

df.long =reshape(df, 
        direction = "long",
        varying = list(names(df)[3:33]),
        v.names = "NVF",
        idvar = c("country"),
        timevar = "Year",
        times = 1990:2020)
df.long$NVF = df.long$NVF/1000000

west= subset(df.long, df.long$region == "west")
south = subset(df.long, df.long$region == "south")
east = subset(df.long, df.long$region == "east")
nordic = subset(df.long, df.long$region == "nordic")
library(ggplot2)

wp<-ggplot(west, aes(x=Year, y=NVF, group=country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country))
wp 

ep<-ggplot(east, aes(x=Year, y=NVF, group=country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country))
ep 

np<-ggplot(nordic, aes(x=Year, y=NVF, group=country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country))
np 

sp<-ggplot(south, aes(x=Year, y=NVF, group=country)) +
  geom_line(aes(color=country))+
  geom_point(aes(color=country))
sp 
