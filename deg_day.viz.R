library(sf)
library(tidyverse)
library(viridis)

NUTS2 = st_read("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/shapefiles/NUTS_RG_01M_2021_3857.shp")
NUTS2 = NUTS2[NUTS2$LEVL_CODE == 2 , ]
NUTS2 = NUTS2[!NUTS2$CNTR_CODE %in% c("IS","LI"), ]

CDD = read.csv("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/CDD_.1deg_11-21_V2.csv")
HDD = read.csv("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/HDD_.1deg_11-21_V2.csv")
colnames(CDD)[2] = "NUTS_ID"
CDD_ss = CDD[,c(2,15)]
CDD_ss$yr_CDD = as.numeric(CDD_ss$yr_CDD)

CDD_merged = NUTS2 %>% left_join(CDD_ss, by= 'NUTS_ID')
CDD_merged = CDD_merged[complete.cases(CDD_merged$yr_CDD),]

CDD_merged = st_transform(CDD_merged,"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#NUTS2_pro = spTransform(CDD_merged, CRS.new)
#NUTS2_pro = NUTS2_pro[complete.cases(NUTS2_pro@data$yr_CDD),]

ggplot(CDD_merged)+
  geom_sf(aes(fill= yr_CDD))+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")+
  theme_bw()

View(CDD_merged@data)
