library(raster)
library(rgdal)

temp = brick("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/tg_ens_mean_0.25deg_reg_2011-2021_v24.0e.nc")

dates <- getZ(temp)
month <-  as.integer(format(dates, "%m"))
#month_day <-  as.integer(format(dates, "%m%d"))

s <- stackApply(temp, month, fun=mean)


nhh= read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/openENTRANCE final data/nhhV2.csv')
nhh_nuts2 = nhh$nutscode
rm(nhh)

NUTS2 = readOGR("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/shapefiles/NUTS_RG_01M_2021_3857.shp")
nuts_shp = NUTS2@data$NUTS_ID

setdiff(nhh_nuts2, nuts_shp) # "UKI1" "UKI2" "UKM2" "UKM3" = not included in NUTS shape file = they will use country average



# adjust projections
CRS.new = CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
s_pro = projectRaster(s, crs = CRS.new)
NUTS2_pro = spTransform(NUTS2, CRS.new)

plot(s_pro[[1]])
plot(NUTS2_pro, add = TRUE)

Jan = s[[1]]
# intersection identification
temp_NUTS = extract(s_pro,NUTS2_pro)

final_df = as.data.frame(matrix(NA,2010,13))
colnames(final_df) = c("NUTS","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
final_df$NUTS = nuts_shp
for (i in 1:nrow(final_df)){
  #print(i)
  if(is.null(dim(temp_NUTS[[i]]))==TRUE){
    print(final_df$NUTS[i]) 
          next}
  else{final_df[i,2:13] = colMeans(as.data.frame(temp_NUTS[[i]]),na.rm = TRUE)}
}

write.csv(final_df,"I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/tg_mean.25deg_11-21.csv")
