# calculate average monthly temperature for each nuts 2 location
# source data: https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles
# Version 24.0e TG 2011-2021

rm(list=ls())
library(chron)
library(lattice)
library(ncdf4)
library(RColorBrewer)
#package for time series ncdf time extract
#install.packages('ncdf4.helpers')
library(ncdf4.helpers)

#ncin<-nc_open("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/tg_ens_mean_0.25deg_reg_2011-2021_v24.0e.nc")
ncin<-nc_open("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/tg_ens_mean_0.1deg_reg_2011-2021_v24.0e.nc")

print(ncin)


lon<-ncvar_get(ncin,"longitude")
nlon<-dim(lon)
head(lon)

lat<-ncvar_get(ncin,"latitude")
nlat<-dim(lat)
head(lat)

print(c(nlon,nlat))

time<-ncvar_get(ncin,"time")
tunits<-ncatt_get(ncin,"time","units")

nt<-dim(time)
nt
tunits


tas_time = nc.get.time.series(ncin, v ="tas",time.dim.name = "time")


#get the temperature
dname<-"tg"
tmp_array<-ncvar_get(ncin,dname)
dlname<-ncatt_get(ncin,dname,"long_name")
dunits<-ncatt_get(ncin,dname,"units")
fillvalue<-ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array) #longitude*latitude*time=464*201*2922
head(tmp_array)

#convert the time variable
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(time, origin = c(tmonth, tday, tyear))

#replace the netCDF fillvaules with R NAs
tmp_array[tmp_array == fillvalue$value]<-NA
#length(na.omit(as.vector(tmp_array[,,1])))

"""
not used

leap = as.data.frame(matrix(NA,11,2))
colnames(leap) = c("year","leap")
leap$year = seq(2011,2021)

leap$leap=leap.year(leap$year)
leap$days = 365
for(i in 1:11){
  if(leap$leap[i] ==TRUE){leap$days[i]=366}
}
leap$start = ""
leap$end = ""
for(i in 1:11){
  if(i ==1){leap$start[i]=as.numeric(1)
            leap$end[i] = as.numeric(leap$days[i]*i*nlat*nlon)}
  else{leap$start[i] = as.numeric(leap$end[i-1])+1
        leap$end[i] = as.numeric(leap$days[i]*i*nlat*nlon)}

}
leap$end[leap$year == 2021] = length(tmp_array)
leap$end = as.numeric(leap$end)
leap$start = as.numeric(leap$start)
(leap$end[11]-leap$start[11])/(nlat*nlon)
"""

"""
#simple plot example
d<-1
tmp_slice<-tmp_array[,,2] #get a single time slice of the data
image(lon,lat,tmp_slice,col = rev(brewer.pal(10,"RdBu")))
#levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

#create dataframe1 for tmp_slice with 1 day

lonlat<-as.matrix(expand.grid(lon,lat))
dim(lonlat)

tmp_vec<-as.vector(tmp_slice)
length(tmp_vec)

tmp_df01<-data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01)<-c("lon","lat",paste(dname,as.character(m),sep = "_"))
head(na.omit(tmp_df01),10)
"""
#create dataframe1 for tmp_slice with 365 days
#tmp_slice<-tmp_array[,,2558:2922]
#tmp_slice_vec_long<-as.vector(tmp_slice)

temp_all<-as.vector(tmp_array)
rm(tmp_array)

#stack where each df is a day
#tmp_slice_vec_long = array(tmp_slice_vec_long,dim = c(nlat*nlon,3834))

#yr_2011 = tmp_slice_vec_long[,,1:365]

temp_all<-matrix(temp_all,nrow = nlon*nlat,ncol = nt)

#tmp_mat<-matrix(tmp_slice_vec_long,nrow = nlon*nlat,ncol = 365)
lonlat <- as.matrix(expand.grid(lon,lat))
temp_all <- data.frame(cbind(lonlat,temp_all))

#create a dataframe without missing values
temp_all<-na.omit(temp_all)


save(temp_all,file = "I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/historic_tg_2011_2021.RData")

#create month index

month = as.data.frame(time)
month$date = nc.get.time.series(ncin, v ="tas",time.dim.name = "time")
month2=as.data.frame(strsplit(as.character(month$date),"-"))

month_sample = as.data.frame(t(month2[2,]))
month$month = month_sample$`2`

colnames(temp_all) = c("lon","lat",tas_time)
temp_all = as.data.frame(t(temp_all))
row.names(temp_all)=NULL#c("lon","lat",tas_time)
colnames(temp_all)=NULL
temp_all$month = c("","",month$month)
Jan = temp_all[temp_all$month == '01']
Feb = temp_all[temp_all$month == '02']
Mar = temp_all[temp_all$month == '03']
Apr = temp_all[temp_all$month == '04']
May = temp_all[temp_all$month == '05']
Jun = temp_all[temp_all$month == '06']
Jul = temp_all[temp_all$month == '07']
Aug = temp_all[temp_all$month == '08']
Sep = temp_all[temp_all$month == '09']
Oct = temp_all[temp_all$month == '10']
Nov = temp_all[temp_all$month == '11']
Dec = temp_all[temp_all$month == '12']

#issues transfering correct lat and longs to final dataset
#each month has different number of unique lat and longs --- look at columns
Jan2 = as.data.frame(colMeans(Jan[3:nrow(Jan),]))
Jan2 = as.data.frame(t(Jan2))
Jan2$lon =Jan[1,]
Jan2$lat = 
col
final_df = as.data.frame(matrix(NA,nt,14))
colnames(final_df)<-c("longitude","latitude","Jan","Feb","March","April",
                                  "May","June","July","August","Septmber","October","November","December")
final_df$lon = temp_all['lon',]
#######################################################################################
#######################################################################################
#clear all previous data to save space 
library(openxlsx)

load("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/tg_mean025_2011_2021.RData")

coordiate<-tmp_df03[,1:2]

dailytemp<-tmp_df03[,3:367]

Jan<-dailytemp[,1:31]
Jan_mean<-as.data.frame(rowMeans(Jan))

Feb<-dailytemp[,32:59]
Feb_mean<-as.data.frame(rowMeans(Feb))

March<-dailytemp[,60:90]
March_mean<-as.data.frame(rowMeans(March))

April<-dailytemp[,91:120]
April_mean<-as.data.frame(rowMeans(April))

May<-dailytemp[,121:151]
May_mean<-as.data.frame(rowMeans(May))

June<-dailytemp[,152:181]
June_mean<-as.data.frame(rowMeans(June))

July<-dailytemp[,182:212]
July_mean<-as.data.frame(rowMeans(July))

August<-dailytemp[,213:243]
August_mean<-as.data.frame(rowMeans(August))

Septmber<-dailytemp[,244:273]
Septmber_mean<-as.data.frame(rowMeans(Septmber))

October<-dailytemp[,274:304]
October_mean<-as.data.frame(rowMeans(October))

November<-dailytemp[,305:334]
November_mean<-as.data.frame(rowMeans(November))

December<-dailytemp[,335:365]
December_mean<-as.data.frame(rowMeans(December))

df_list<-mget(ls(pattern = "\\_mean"))

temp_2011_2021_month<-cbind.data.frame(df_list)

temp_2011_2021_month$row_ID<-seq.int(nrow(temp_2011_2021_month))

temp_2011_2021_month<-cbind.data.frame(coordiate,temp_2011_2021_month)

temp_2011_2021_month<-temp_2011_2021_month[c("row_ID","Var1","Var2","rowMeans(Jan)","rowMeans(Feb)","rowMeans(March)",
                                 "rowMeans(April)","rowMeans(May)","rowMeans(June)","rowMeans(July)",
                                 "rowMeans(August)","rowMeans(Septmber)","rowMeans(October)",
                                 "rowMeans(November)","rowMeans(December)")]

colnames(temp_2011_2021_month)<-c("row_ID","longitude","latitude","Jan","Feb","March","April",
                            "May","June","July","August","Septmber","October","November","December")

write.xlsx(temp_2011_2021_month,file = "I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/temp_2011_2021_month_average.xlsx")

######################################################################################
######################################################################################
############################
# compare shapefile nuts 2 2021 with OE nuts
###########################
library(rgdal)
nhh= read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/openENTRANCE final data/nhhV2.csv')
nhh_nuts2 = nhh$nutscode
rm(nhh)

NUTS2 = readOGR("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/shapefiles/NUTS_RG_01M_2021_3857.shp")
nuts_shp = NUTS2@data$NUTS_ID

setdiff(nhh_nuts2, nuts_shp) # "UKI1" "UKI2" "UKM2" "UKM3" = not included in NUTS shape file = they will use country average
temp = as.data.frame(nhh_nuts2)
View(temp)

NUTS2_ss = subset(NUTS2, NUTS2@data$NUTS_ID %in% nhh_nuts2)
plot(NUTS2_ss)

##############################
# add temperature data for TR GR and CY
##############################

temp_2011_2021_month = read_excel("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/temp_2011_2021_month_average.xlsx")

# read in new temperature data from RoR
TR_GR_CY_temp = read.csv("I:/Projekte/SMARTEES H2020 - WV0139/Work Phase/Papers/Biking/data/spatial/temperature/TR_GR_CYtemperature.csv")

# Convert from decimal degree min sec to decimal degree

chg_lat = TR_GR_CY_temp[,'LAT']
chg_lat = as.data.frame(t(as.data.frame(strsplit(chg_lat,"[+]"))[2,]))
chg_lat = as.data.frame(t(as.data.frame(strsplit(chg_lat[1:42,],"[:]"))))
rownames(chg_lat) = NULL

chg_lat2 = measurements::conv_unit(paste0(chg_lat[,1], " ", chg_lat[,2],".", chg_lat[,3]), from = 'deg_dec_min', to = 'dec_deg') 

chg_lon = TR_GR_CY_temp[,'LON']
chg_lon = as.data.frame(t(as.data.frame(strsplit(chg_lon,"[+]"))[2,]))
chg_lon = as.data.frame(t(as.data.frame(strsplit(chg_lon[1:42,],"[:]"))))
rownames(chg_lon) = NULL

chg_lon2 = measurements::conv_unit(paste0(chg_lon[,1], " ", chg_lon[,2],".", chg_lon[,3]), from = 'deg_dec_min', to = 'dec_deg') 
TR_GR_CY_temp$longitude = as.numeric(chg_lon2)
TR_GR_CY_temp$latitude = as.numeric(chg_lat2)

# combine E-OBS data with GR_TR_CY
TR_GR_CY_temp = TR_GR_CY_temp[,c(1,18,19,6:17)]
colnames(TR_GR_CY_temp) = colnames(temp_2011_2021_month)
temp_2011_2021_month_2 = rbind.data.frame(temp_2011_2021_month, TR_GR_CY_temp)
temp_2011_2021_month_2$row_ID = seq(1,dim(temp_2011_2021_month_2)[1])
rownames(temp_2011_2021_month_2) = NULL

temp_spatial = temp_2011_2021_month_2

CRS.new = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs +type=crs")

coordinates(temp_spatial) = ~longitude + latitude
proj4string(temp_spatial) = CRS("+proj=longlat")
temp_spatial = spTransform(temp_spatial, proj4string(NUTS2_ss))

temp_spatial@bbox
NUTS2_ss@bbox

plot(temp_spatial)
plot(NUTS2_ss, col = "red", add = TRUE)

proj4string(temp_spatial) = CRS.new
proj4string(NUTS2_ss) = CRS.new




final_temp_obv = over(temp_spatial,NUTS2_ss)
nuts_w_temp = unique(final_temp_obv2$NUTS_ID)

# use nearest neighbor to calculate average temperatures for NUTS2 with missing temp values
# setdiff(nhh_nuts2,nuts_w_temp)
# all of the nuts regions without temp stations 
# "AT13" "ES63" "ES64" "FI20" "FRY1" "FRY2" "FRY3" "FRY4" "FRY5" "IS00" "MT00" "PT30" "PT20" "TR31" "TR32" "TR33" "TR41" "TR51" "TR52"

# the row names in the following df indicate the temp observations for the respective NUTS2 region
final_temp_obv2=final_temp_obv[complete.cases(final_temp_obv[ , c('NUTS_ID')]), ] 

final_df = as.data.frame(matrix(NA,length(nhh_nuts2), 12))
colnames(final_df) = c(colnames(temp_2011_2021_month)[4:15])
rownames(final_df) = nhh_nuts2

for (nuts in rownames(final_df)){
  print(nuts)
  temp = subset(final_temp_obv2, final_temp_obv2$NUTS_ID == nuts)
  temp2 = subset(temp, temp$NUTS_ID == nuts)
  temp3 = temp_spatial@data[rownames(temp2),]
  temp_mean = colSums(temp3)[2:13]/dim(temp3)[1]
  final_df[nuts,] = temp_mean
}
library(measurements)
missing = setdiff(nhh_nuts2,nuts_w_temp)
missing_nuts = subset(NUTS2_ss, NUTS2_ss@data$NUTS_ID %in% missing)

library(rgeos)
centr =gCentroid(missing_nuts, byid = TRUE, id= missing_nuts$NUTS_ID)
centr_coord = as.data.frame(centr)

library(yaImpute)
temp_spatial2 = as.data.frame(temp_spatial@coords)

knn_out<-ann(as.matrix(temp_spatial2[,c("latitude","longitude")]),as.matrix(centr_coord[,c("y","x")]), k=3)

nearest_temp_point<-as.data.frame(knn_out$knnIndexDist)

colnames(nearest_temp_point)<-c("nearest1","nearest2","nearest3","ndist1","ndist2","ndist3")

nearest_temp_point<-cbind(missing_nuts$NUTS_ID,nearest_temp_point)

#check results for AT13
NUTS_test = subset(NUTS2,NUTS2@data$NUTS_ID == "UKI3")
plot(NUTS_test)
plot(temp_spatial[c(5967,6165,5968),], col = "blue", add = TRUE, size = 1000)

# calculate missing observations based on nn id
rownames(nearest_temp_point) = nearest_temp_point$`missing_nuts$NUTS_ID`
for (nuts in missing){
  station_id = unlist(nearest_temp_point[nuts,][2:4])
  final_df[nuts, ] = colSums(temp_2011_2021_month_2[station_id,4:15])/3
}

write.csv(final_df, file = "I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/NUTS2_2011_2021_tgmean.csv")
