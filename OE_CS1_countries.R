library(readxl)
library(psych)
library(knitr)
library(mlr)
install.packages('rgeos')
library(rgeos)
library(rnaturalearth)
install.packages('cowplot') #makes inset maps
library(cowplot)
install.packages('magick')
library(magick) #for png image
library(plyr)
install.packages('devtools')
install.packages('ggplot2')
install.packages('viridis')
install.packages('plyr')
library(ggplot2)
library(devtools)
library(tidyr)
library(dplyr)
library(plyr) #used to rename factors
devtools::install_github("ropenscilabs/rnaturalearth")
devtools::install_github("ropenscilabs/rnaturalearthdata")
install.packages("rnaturalearthhires",
                 repos = "http://packages.ropensci.org",
                 type = "source")
install.packages("rnatrualearthdata")
library(rnaturalearth)
install.packages('png') #for png images
library(png)
install.packages('installr')
library(installr)
install.Rtools()
########################
nhh <- read_csv("Desktop/Energy Institute/openEntrance/data/openENTRANCE final data/nhh.csv")
est18=read_csv("Desktop/Energy Institute/openEntrance/OE Twitter image/yr_2018.csv")
# the following Eurostat dataset is the annual electricity demand for all households (kWh)
HH_kWH_a_Eurostat <- read_excel("Desktop/Energy Institute/openEntrance/OE Twitter image/HH_kWH.a_Eurostat.xlsx") # does not have TR or CH
alpha3_to_alpha2 <- read_excel("Desktop/Energy Institute/openEntrance/OE Twitter image/alpha3_to_alpha2.xlsx") # for conversion of alpha 2 to alpha 3
final = merge(est18,HH_kWH_a_Eurostat)
final = as.data.frame(unique(final$country))
colnames(final)=c('country')
# change to alpha 3 country codes
for (i in 1:29){if (final$country[i]=="UK")(final$country[i]="GB")} #UK
for (i in 1:29){if (final$country[i]=="EL")(final$country[i]="GR")} #EL
final = merge(alpha3_to_alpha2,final,by.x =,'alpha-2', by.y ='country')
final = rbind.data.frame(final, c("GB",'United Kingdom',"GBR",826))
#####################
#################
# data manipulation
#shapefile
world<-ne_countries(scale="medium",returnclass = "sf") #feature class multipoly
world$partner<-0 # recode full consortium partners
world$p.names<-NA # names for labeling, might be too busy with labels

# data from project
#df1.ss<-subset(df1,df1$`Partner Type (dt)` =="(Regional-) Regierung & Beh?rde"|df1$`Partner Type (dt)` =="Forschung"|df1$`Partner Type (dt)`=="NGO"|df1$`Partner Type (dt)`=="Unternehmen") 
#df1.ss$Type[df1.ss$Type=="Lighthouse"]<-"Lighthouse City" ### IF YOU WANT TO RENAME
#df1.ss$Type[df1.ss$Type=="Follower"]<-"Follower City"

#partner
for(i in 1:241){ 
  for(j in 1:length(final$country)){
    if (world$sovereignt[i]==final$country[j]){world$partner[i]=1}
  }
}
for(i in 1:241){ 
  for(j in 1:length(final$country)){
    if (world$sovereignt[i]=="United Kingdom"){world$partner[i]=1}
  }
}

# MAKE PARTNER COUNTRIES FACTORS TO MAKE LABELING EASIER
partner<-as.factor(world$partner) # factor and name levels
partner<-revalue(partner, c("1"="EI-JKU Partnerländer","0"="EI JKU Non-Partner Countries"))
partner<-factor(partner, levels=c("EI-JKU Partnerländer","EI JKU Non-Partner Countries")) # reorder levels to set which one is viewed first

world$partner2<-partner #add too world


#BEGIN MAP OF EUROPE
map<-ggplot(data = test) + 
  geom_sf(aes(fill = partner2), alpha=.7)+#,show.legend = FALSE)+
  labs(fill="Legend")+
  scale_fill_manual(aesthetics = "fill",values = c("darkolivegreen3","grey92")) + #sets the fill color
  coord_sf(xlim = c(-15.0, 43.0),
           ylim = c(35.0, 72)) +
  scale_size_area(guide = FALSE)+
  ggtitle("Partnernetzwerk der Arbeitsgruppe")

map  +theme_classic()





