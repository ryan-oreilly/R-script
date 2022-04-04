#obj: create index for eHighway cluster
install.packages('yaml')
library(yaml)
library(stringr)
library(rgdal)
eHigh = yaml.load_file('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/Modeling Team Data/eHighway_clusters.yaml.txt')

df = as.data.frame(matrix(NA,1,4))
colnames(df) = c("country","NUTS_ID", "eHighway","info")
for (i in 1:118){
  if (grepl("NULL",eHigh[[1]]$`e-highway2050`[[i]]) ==TRUE){next}
  else{df[i,1:4] = as.data.frame(eHigh[[1]]$`e-highway2050`[[i]])}
}
df[88,'eHighway'] = '89_SE' #error found in eHighway cluster definition
#final df with eHighway clusters
df = df[complete.cases(df),]
row.names(df) = NULL
df = df[1:95,]
#luxembourg doesnt work as it only has nuts2 regions
df[30,"NUTS3"]='LU000'

df2 = as.data.frame(matrix(NA,1,4))
colnames(df2) = c( "eHighway","country","NUTS_ID","countNUTS3")
for (row in 1:dim(df)[1]){
  temp=chartr(" + ", '   ',df$NUTS_ID[row])
  temp2 = as.data.frame(str_split(temp, "   "))[1]
  colnames(temp2)= "NUTS_ID"
  temp2$eHighway = df$eHighway[row]
  temp2$country = df$country[row]
  temp2$countNUTS3 = length(temp2[,1])
  df2 = rbind.data.frame(df2, temp2)}

#eHighway 04_ES has error because of missing space character in string with nuts3 regions
# remove row with error and add missing regions ES211  ES212
df2 = df2
df2 = df2[-c(18,1156),]
df3 = as.data.frame(matrix(NA,4,4))
colnames(df3) = colnames(df2)
df3$eHighway = c('04_ES','04_ES','75_FI','75_FI')
df3$country = c('Spain','Spain','Finland','Finland')
df3$NUTS_ID = c('ES211', 'ES212','FI197',  'FI1B1')
df3$countNUTS3 = c(5,5,16,16)
df2 = rbind.data.frame(df2, df3)
df2 = df2[complete.cases(df2),]
#find duplicates: DED52 DEF0F EL622
temp = as.data.frame(table(df2$NUTS_ID))
rm(temp)
df2 = df2[!duplicated(df2$NUTS_ID), ]

#read in shapefile
NUTS2013 = readOGR('C:/Users/AK194059/Desktop/NUTS_RG_01M_2013_3035.shp') # notice this is a 2013 version which coincides with runtime of eHighway project
NUTS2016 = readOGR('C:/Users/AK194059/Desktop/NUTS_RG_01M_2016_3035.shp')
# the 2013 version does not coincide with the FRENCH eHighway definitions
NUTS2021 = readOGR('C:/Users/Default/Desktop/NUTS_RG_01M_2021_3035.shp')

NUTS3merged2013 = merge(NUTS2013@data, df2, by= "NUTS_ID",duplicateGeoms=FALSE)
NUTS3merged2016 = merge(NUTS2016@data, df2, by= "NUTS_ID",duplicateGeoms=FALSE)
NUTS3merged2021 = merge(NUTS2021@data, df2, by= "NUTS_ID",duplicateGeoms=FALSE)

NUTS3merged2013 = NUTS3merged2013[complete.cases(NUTS3merged2013$eHighway ),]
NUTS3merged2016 = NUTS3merged2016[complete.cases(NUTS3merged2016$eHighway ),]
NUTS3merged2021 = NUTS3merged2021[complete.cases(NUTS3merged2021$eHighway ),]

setdiff(NUTS3merged2013$eHighway,NUTS3merged2016$eHighway)
setdiff(NUTS3merged2016$eHighway,NUTS3merged2013$eHighway)
#setdiff(NUTS3merged2016$eHighway,NUTS3merged2021$eHighway)
setdiff(NUTS3merged2021$eHighway,NUTS3merged2016$eHighway)

View(NUTS3merged2013@data )

plot(NUTS3merged2013,col = 'blue')
plot(NUTS3merged2016,col ='red',add = TRUE)
plot(NUTS3me )
# highest matching comes from 2021





NUTS@data$nchar = nchar(NUTS@data$NUTS_ID)
NUTS3 = subset(NUTS, NUTS@data$nchar == 5)

NUTS3merged = merge(NUTS3, df2, by= "NUTS_ID",duplicateGeoms=TRUE)

# create eHighway clusters
NUTS3.union2013 <- unionSpatialPolygons(NUTS3merged, NUTS3merged$eHighway)
#convert to a Spatial polygon dataframe and save
NUTS3merged.df <- as(NUTS3merged, "data.frame")
NUTS3merged.df = NUTS3merged.df[,c(3,8)]
NUTS3merged.df = NUTS3merged.df[!duplicated(NUTS3merged.df$eHighway), ]
NUTS3merged.df = NUTS3merged.df[complete.cases(NUTS3merged.df),]
rownames(NUTS3merged.df) = NUTS3merged.df$eHighway

NUTS3.union2013SPD = SpatialPolygonsDataFrame( NUTS3.union2013,NUTS3merged.df)

writeOGR(NUTS3.union2013SPD, ".", "I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/R script/eHighway.shp", 
         driver = "ESRI Shapefile")
plot(NUTS3.union2013SPD)

# IDENTIFY WHICH NUTS REGIONS DID NOT GET CLASSIFIED
plot(NUTS, col = 'blue')
plot(NUTS3.union2013, add = TRUE)
no_eHigh = subset(NUTS3, is.na(NUTS3merged$eHighway)==TRUE)
plot(no_eHigh)
table(no_eHigh$CNTR_CODE)
View(no_eHigh@data)






# read in nuts3 shapefile
library(rgdal)
library(stringr)
NUTS = readOGR('C:/Users/Default/Desktop/NUTS_RG_01M_2021_3035.shp')
NUTS@data$nchar = nchar(NUTS@data$NUTS_ID)
NUTS3 = subset(NUTS, NUTS@data$nchar == 5)
rownames(NUTS3@data) = NULL

#final df with eHighway clusters
df = df[complete.cases(df),]
row.names(df) = NULL
df = df[1:95,]
#luxembourg doesnt work as it only has nuts2 regions
df[30,"NUTS3"]='LU000'

#create initial polygons dataframe
temp=chartr(" + ", '   ',df$NUTS3[1])

nuts3_eHigh = as.data.frame(str_split(temp, "   "))[1]
colnames(nuts3_eHigh) =c('names')
ssNUTS3 = subset(NUTS3, NUTS3@data$NUTS_ID %in% nuts3_eHigh$names)

final =aggregate(ssNUTS3)
final3 = SpatialPolygonsDataFrame(final,
                                  data.frame(id =df$dHighway[1],
                                             row.names = 1))

#####################
#https://gis.stackexchange.com/questions/63577/joining-polygons-in-r
libs <- c("rgdal", "maptools", "gridExtra")
install.packages('maptools')
lapply(libs, require, character.only = TRUE)
unionSpatialPolygons()
#######################
for (row in 2:95){
  temp=chartr(" + ", '   ',df$NUTS3[row])
  
  nuts3_eHigh = as.data.frame(str_split(temp, "   "))[1]
  colnames(nuts3_eHigh) =c('names')
  ssNUTS3 = subset(nuts3, nuts3@data$NUTS_ID %in% nuts3_eHigh$names)
  
  final =aggregate(ssNUTS3)
  final2 = SpatialPolygonsDataFrame(final,
                                    data.frame(id =df$dHighway[row],
                                               row.names = 1))
  final3 = rbind.SpatialPolygonsDataFrame(final3,final2)
}
rownames(final3@data) = NULL
View(final3@data)
plot(final3)



#calculate the percentage of NUTS2 region that falls within eHighway cluster
nuts2 = readOGR('C:/Users/AK194059/Desktop/ref-nuts-2021-01m.shp/NUTS_RG_01M_2021_3035_LEVL_2.shp')
plot(nuts2)

library(sf)
library(dplyr)
intersect_pct <- raster::intersect(final3, nuts2) 
intersect_pct$area <- area(intersect_pct) / 1000000

plot(final3, axes=T); plot(nuts2, add=T); plot(intersect_pct, add=T, col='red')
