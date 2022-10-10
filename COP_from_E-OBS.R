# objective calculate COP for heat pumps from historic temperature for each nuts 2 location
# source data: https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles
# Version 24.0e TG 2011-2021
library(rgdal)
library(raster)
library(stringr)
temp<-  brick("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/tg_ens_mean_0.1deg_reg_2011-2021_v24.0e.nc")

nlayers(temp)

COP_df = temp

lift = tg_mean - 30
6.81+0.121*lift+0.000630*lift^2
########################
# functions
#######################
COP = function(x){
  if(is.na(x)==TRUE){x=x}
  else{lift = as.numeric(x) - 30
       x = 6.81+0.121*lift+0.000630*lift^2}
}

######################
for(day in 1:nlayers(COP_df)){
  print(day)
  temp_rast = apply(array(COP_df[[day]]),c(1),COP)
  COP_df[[day]] = temp_rast
}

#########################
# extract time component from raster stack
########################
dates <- getZ(temp)
#month <-  as.integer(format(dates, "%m"))
month_day <-  as.integer(format(dates, "%m%d"))


#########################
# average daily COP
#########################
COP_df <- stackApply(COP_df, month_day, fun=mean)

rm(CDD_day)


#########################
# average monthly COP
#########################

############
# create index for month
############

month = names(COP_df@data)
month = as.data.frame(t(as.data.frame(strsplit(month,'_', fixed =TRUE))))
month$V3 = str_sub(month$V2,1,nchar(month$V2)-2)

COP_ave_month <- stackApply(COP_df, month$V3, fun=mean)


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
COP_ave_month_pro = projectRaster(COP_ave_month, crs = CRS.new)

NUTS2_pro = spTransform(NUTS2, CRS.new)

# intersection identification
CDD_temp_NUTS = extract(COP_ave_month_pro,NUTS2_pro)


COP_final_df = as.data.frame(matrix(NA,2010,13))


colnames(COP_final_df) = c("NUTS","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
COP_final_df$NUTS = nuts_shp

# COP nuts region averages
for (i in 1:nrow(COP_final_df)){
  #print(i)
  if(is.null(dim(CDD_temp_NUTS[[i]]))==TRUE){
    print(COP_final_df$NUTS[i]) 
    next}
  else{COP_final_df[i,2:13] = colMeans(as.data.frame(CDD_temp_NUTS[[i]]),na.rm = TRUE)}
}


#######
# make adjustments for nuts regions that did not have temperature observations
#######
#EL30 = EL41
COP_final_df[2:ncol(COP_final_df)][COP_final_df$NUTS == "EL30",] = COP_final_df[2:ncol(COP_final_df)][COP_final_df$NUTS == "EL41",]

#EL43 = EL421
COP_final_df[2:ncol(COP_final_df)][COP_final_df$NUTS == "EL43",] = COP_final_df[2:ncol(COP_final_df)][COP_final_df$NUTS == "EL421",]
#TR31 = TR32
COP_final_df[2:ncol(COP_final_df)][COP_final_df$NUTS == "TR31",] = COP_final_df[2:ncol(COP_final_df)][COP_final_df$NUTS == "TR32",]

# UK - UKI1, UKI2, UKM2, UKM3 will be set to their NUTS1 region

UKI1 = COP_final_df[COP_final_df$NUTS == 'UKI',]
UKI2 = COP_final_df[COP_final_df$NUTS == 'UKI',]
UKM2 = COP_final_df[COP_final_df$NUTS == 'UKM',]
UKM3 = COP_final_df[COP_final_df$NUTS == 'UKM',]

temp = rbind.data.frame(UKI1,UKI2, UKM2,UKM3)
temp$NUTS = c('UKI1','UKI2','UKM2','UKM3')
COP_final_df = rbind.data.frame(COP_final_df, temp)


# the islands of ES64, FRY1-FRY5, PT20,  will be dropped

#create annual totals
COP_final_df$yr_CCOP = rowMeans(COP_final_df[2:ncol(COP_final_df)])

save.image("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/COP_from_EOBS.rdata")

# write to file
write.csv(COP_final_df,"I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/openENTRANCE final data/COP_.1deg_11-21_V1.csv")


