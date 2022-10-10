# calculate average monthly temperature for each nuts 2 location
# source data: https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles
# Version 24.0e TG 2011-2021
library(rgdal)
library(raster)
library(stringr)
temp<-  brick("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/tg_ens_mean_0.1deg_reg_2011-2021_v24.0e.nc")

nlayers(temp)
CDD_day = temp
HDD_day = temp


########################
# functions
#######################
CDD = function(x){
  if(is.na(x)==TRUE){x=x}
  else if((as.numeric(x)>=24) == TRUE){x=(as.numeric(x)-21)}
  else{x=as.numeric(0)}
}


HDD = function(x){
  if(is.na(x)==TRUE){x=x}
  else if((as.numeric(x)<=15) ==TRUE){x=(18-as.numeric(x))}
  else{x=as.numeric(0)}
}


#########################
# CDD
#########################
for(day in 1:nlayers(temp)){
  print(day)
  temp_rast = apply(array(CDD_day[[day]]),c(1),CDD)
  CDD_day[[day]] = temp_rast
}


save(CDD_day, "I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/CDD_EOBS.1deg.rdata")
#load("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/CDD_EOBS.1deg.rdata")
#########################
# HDD
#########################

for(day in 1:nlayers(HDD_day)){
  print(day)
  temp_rast = apply(array(temp[[day]]),c(1),HDD)
  HDD_day[[day]] = temp_rast
}


save(HDD_day, file = "I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/HDD_EOBS.1deg.rdata")
#load("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/HDD_EOBS.1deg.rdata")
#########################
# extract time component from raster stack
########################
dates <- getZ(temp)
#month <-  as.integer(format(dates, "%m"))
month_day <-  as.integer(format(dates, "%m%d"))


#########################
# average daily dd
#########################
CDD_ave_day <- stackApply(CDD_day, month_day, fun=mean)

rm(CDD_day)

HDD_ave_day <- stackApply(HDD_day, month_day, fun=mean)

rm(HDD_day)

#########################
# average monthly dd
#########################

############
# create index for month
############

  month = names(HDD_ave_day@data)
  month = as.data.frame(t(as.data.frame(strsplit(month,'_', fixed =TRUE))))
  month$V3 = str_sub(month$V2,1,nchar(month$V2)-2)

CDD_ave_month <- stackApply(CDD_ave_day, month$V3, fun=sum)

HDD_ave_month <- stackApply(HDD_ave_day, month$V3, fun=sum)

rm(CDD_ave_day)
rm(HDD_ave_day)

###########################
# bring in external data on NUTS regions
###########################
rm(temp)
nhh= read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/openENTRANCE final data/nhhV2.csv')
nhh = nhh[complete.cases(nhh),]
nhh_nuts2 = nhh$nutscode
rm(nhh)

NUTS2 = readOGR("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/shapefiles/NUTS_RG_01M_2021_3857.shp")
nuts_shp = NUTS2@data$NUTS_ID

setdiff(nhh_nuts2, nuts_shp) # "UKI1" "UKI2" "UKM2" "UKM3" = not included in NUTS shape file = they will use country average



# adjust projections
CRS.new = CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
CDD_pro = projectRaster(CDD_ave_month, crs = CRS.new)
HDD_pro = projectRaster(HDD_ave_month, crs = CRS.new)

NUTS2_pro = spTransform(NUTS2, CRS.new)

plot(HDD_pro[[1]])
plot(NUTS2_pro, add = TRUE)


# intersection identification
CDD_temp_NUTS = extract(CDD_pro,NUTS2_pro)
HDD_temp_NUTS = extract(HDD_pro,NUTS2_pro)

CDD_final_df = as.data.frame(matrix(NA,2010,13))
HDD_final_df = as.data.frame(matrix(NA,2010,13))

colnames(CDD_final_df) = c("NUTS","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
colnames(HDD_final_df) = c("NUTS","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
CDD_final_df$NUTS = nuts_shp
HDD_final_df$NUTS = nuts_shp

# CDD nuts region averages
for (i in 1:nrow(CDD_final_df)){
  #print(i)
  if(is.null(dim(CDD_temp_NUTS[[i]]))==TRUE){
    print(CDD_final_df$NUTS[i]) 
          next}
  else{CDD_final_df[i,2:13] = colMeans(as.data.frame(CDD_temp_NUTS[[i]]),na.rm = TRUE)}
}



# HDD nuts region averages
for (i in 1:nrow(HDD_final_df)){
  #print(i)
  if(is.null(dim(HDD_temp_NUTS[[i]]))==TRUE){
    print(HDD_final_df$NUTS[i]) 
    next}
  else{HDD_final_df[i,2:13] = colMeans(as.data.frame(HDD_temp_NUTS[[i]]),na.rm = TRUE)}
}


#######
# make adjustments for nuts regions that did not have temperature observations
#######
#EL30 = EL41
HDD_final_df[3:ncol(HDD_final_df)-1][HDD_final_df$NUTS == "EL30",] = HDD_final_df[3:ncol(HDD_final_df)-1][HDD_final_df$NUTS == "EL41",]
CDD_final_df[3:ncol(CDD_final_df)-1][CDD_final_df$NUTS == "EL30",] = CDD_final_df[3:ncol(CDD_final_df)-1][CDD_final_df$NUTS == "EL41",]

#EL43 = EL421
HDD_final_df[3:ncol(HDD_final_df)-1][HDD_final_df$NUTS == "EL43",] = HDD_final_df[3:ncol(HDD_final_df)-1][HDD_final_df$NUTS == "EL421",]
CDD_final_df[3:ncol(CDD_final_df)-1][CDD_final_df$NUTS == "EL43",] = CDD_final_df[3:ncol(CDD_final_df)-1][CDD_final_df$NUTS == "EL421",]
#TR31 = TR32
HDD_final_df[3:ncol(HDD_final_df)-1][HDD_final_df$NUTS == "TR31",] = HDD_final_df[3:ncol(HDD_final_df)-1][HDD_final_df$NUTS == "TR32",]
CDD_final_df[3:ncol(CDD_final_df)-1][CDD_final_df$NUTS == "TR31",] = CDD_final_df[3:ncol(CDD_final_df)-1][CDD_final_df$NUTS == "TR32",]

# UK - UKI1, UKI2, UKM2, UKM3 will be set to their NUTS1 region

UKI1 = HDD_final_df[HDD_final_df$NUTS == 'UKI',]
UKI2 = HDD_final_df[HDD_final_df$NUTS == 'UKI',]
UKM2 = HDD_final_df[HDD_final_df$NUTS == 'UKM',]
UKM3 = HDD_final_df[HDD_final_df$NUTS == 'UKM',]

temp = rbind.data.frame(UKI1,UKI2, UKM2,UKM3)
temp$NUTS = c('UKI1','UKI2','UKM2','UKM3')
HDD_final_df = rbind.data.frame(HDD_final_df, temp)


UKI1 = CDD_final_df[CDD_final_df$NUTS == 'UKI',]
UKI2 = CDD_final_df[CDD_final_df$NUTS == 'UKI',]
UKM2 = CDD_final_df[CDD_final_df$NUTS == 'UKM',]
UKM3 = CDD_final_df[CDD_final_df$NUTS == 'UKM',]

temp = rbind.data.frame(UKI1,UKI2, UKM2,UKM3)
temp$NUTS = c('UKI1','UKI2','UKM2','UKM3')
CDD_final_df = rbind.data.frame(CDD_final_df, temp)


# the islands of ES64, FRY1-FRY5, PT20,  will be dropped

#create annual totals
HDD_final_df$yr_HDD = round(rowSums(HDD_final_df[2:ncol(HDD_final_df)]))
CDD_final_df$yr_CDD = round(rowSums(CDD_final_df[2:ncol(CDD_final_df)]))
# write to file
write.csv(CDD_final_df,"I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/CDD_.1deg_11-21_V2.csv")
write.csv(HDD_final_df,"I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/HDD_.1deg_11-21_V2.csv")




