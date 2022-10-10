###########################################
# format data to visualize potential for reduction and potential for increase using the NUTS2 shapefile
# read in potential for reduction
# aggregate devices by hour and region
# adjust using achievable participation rates
# select years 2022 2030 2040 2050
###
# repeat for p increase
###
###########################################
library(countrycode)
library(dplyr)
full_potential = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/Full_potential.V9.csv')
head(full_potential)
full_potential = full_potential[,c(3,4,6:39)]
#remove max dispatch
key = "Maximum Dispatch"
library(ggrepel)
full_potential$type = grepl(key,full_potential$variable)
full_potential = full_potential[full_potential$type!= TRUE,]
# select 2020, 2022, 2030, 2040,2050
head(full_potential)
full_potential = full_potential[,c(1:3, 6,8,16,26,36)]

full_potential$iso2c = substr(full_potential$region, start = 1, stop = 2)
full_potential$countryname = countrycode(full_potential$iso2c,origin = 'iso2c',destination = 'country.name')
full_potential$countryname[full_potential$iso2c=='EL'] = 'Greece'
full_potential$countryname[full_potential$iso2c=='UK'] = 'United Kingdom'
head(full_potential)

# DR_part from wide to long
library(data.table)
full_potential = melt(full_potential,id.vars = c("region","variable","countryname","iso2c", "subannual") )
colnames(full_potential)[7] = "MW_red"

# ADJUST DEVICE NAMES
full_potential$device = ""
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Air Conditioning"] = 'AC'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Circulation Pump"] = 'CP'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Dish Washer"]     ='DW'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Dryer"]           ='TD'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Electric Vehicle"]='EV'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Heat Pump"]       ='HP'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Refrigeration"]   ='FRRF'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Storage Heater"]  ='SH'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Washing Machine"] ='WM'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Water Heater"]   = 'WH'
#drop EV fit for 55
full_potential = full_potential[full_potential$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Electric Vehicle_55",]
#drop devices that can not be delayed
full_potential = full_potential[full_potential$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Storage Heater",]
full_potential = full_potential[full_potential$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Heat Pump",]
full_potential = full_potential[full_potential$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Water Heater",]
unique(full_potential$variable)

full_potential$device2 = ""
full_potential$device2[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Air Conditioning"] = 'AC'
full_potential$device2[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Circulation Pump"] = 'SH'
full_potential$device2[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Dish Washer"]     ='Wash'
full_potential$device2[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Dryer"]           ='Wash'
full_potential$device2[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Electric Vehicle"]='EV'
full_potential$device2[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Heat Pump"]       ='SH'
full_potential$device2[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Refrigeration"]   ='FRRF'
full_potential$device2[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Storage Heater"]  ='SH'
full_potential$device2[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Washing Machine"] ='Wash'
full_potential$device2[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Water Heater"]   = 'WH'


# READ IN PARTICIPATION RATES
DR_part = read.csv("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/participation_rates_country.csv")
head(DR_part)
DR_part = DR_part[,c(1,6:12)]

DR_part = melt(DR_part,id.vars = c("X") )
colnames(DR_part) = c("countryname","device2","DR_rate")
DR_part$iso2c = countrycode(DR_part$countryname,origin = 'country.name',destination = 'iso2c')
DR_part$iso2c[DR_part$iso2c=='GR'] = 'EL'
DR_part$iso2c[DR_part$iso2c=='GB'] = 'UK'
head(DR_part)

full_potential = merge(full_potential, DR_part, by = c("device2","iso2c"))

full_potential$MW_acheiv = full_potential$MW_red*full_potential$DR_rate/100
head(full_potential)

full_potential = full_potential[,c(1,2,3,5,6,7,8,9,11,12)]
head(test)
nuts2_acheive= full_potential %>%
  group_by(iso2c, region, variable, device) %>%
  summarize(acheive_ave = mean(MW_acheiv))
head(nuts2_acheive)
# set EL30 CP for 2050 using the % change in neighboring region
nuts2_acheive$acheive_ave[nuts2_acheive$device=="CP" &nuts2_acheive$region=="EL30"] = 3.334117- 3.334117 * (0.34390336-0.21669611)/0.34390336

nuts2_yr_tot= nuts2_acheive %>%
  group_by(region, variable) %>%
  summarize(acheive_tot = sum(acheive_ave))

nuts2_acheive$id_col = paste0("RED_",nuts2_acheive$device,"_",nuts2_acheive$variable)
nuts2_acheive = nuts2_acheive[,c(2,5,6)]


library(tidyr)

data_wide <- spread(nuts2_acheive, id_col, acheive_ave)

data_wide2 = spread(nuts2_yr_tot, variable, acheive_tot)
colnames(data_wide2) = c("region","RED_tot2020","RED_tot2022","RED_tot2030","RED_tot2040","RED_tot2050")

final_red = merge(data_wide,data_wide2)
#####################################
# potential for increase
#read in p_inc
pinc_SH = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/P_inc/P_inc_SH.csv')
pinc_WH = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/P_inc/P_inc_WH.csv')
pinc_HP = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/P_inc/P_inc_HP.csv')
pinc_DW = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/P_inc/P_inc_DW.csv')
pinc_WM = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/P_inc/P_inc_WM.csv')
pinc_TD = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/P_inc/P_inc_TD.csv')

# adjust pinc_SH EL30
pinc_SH$X2049[pinc_SH$X =='EL30'] = pinc_SH$X2048[pinc_SH$X =='EL30'] - pinc_SH$X2048[pinc_SH$X =='EL30']*(pinc_SH$X2048[pinc_SH$X =='EL30']-pinc_SH$X2047[pinc_SH$X =='EL30'])/pinc_SH$X2048[pinc_SH$X =='EL30']
pinc_SH$X2050[pinc_SH$X =='EL30'] = pinc_SH$X2049[pinc_SH$X =='EL30'] - pinc_SH$X2049[pinc_SH$X =='EL30']*(pinc_SH$X2049[pinc_SH$X =='EL30']-pinc_SH$X2048[pinc_SH$X =='EL30'])/pinc_SH$X2049[pinc_SH$X =='EL30']


# insert devices
pinc_SH$device = 'SH'
pinc_WH$device = "WH"
pinc_HP$device = "HP"
pinc_DW$device = "DW"
pinc_WM$device = "WM"
pinc_TD$device = "TD"

# insert DR  COLUMN TO MERGE ON
pinc_SH$DR_merge[pinc_SH$device == 'SH'] = 'SH'
pinc_WH$DR_merge[pinc_WH$device == 'WH'] = 'WH'
pinc_HP$DR_merge[pinc_HP$device == 'HP'] = 'SH'
pinc_DW$DR_merge[pinc_DW$device == 'DW'] = 'Wash'
pinc_WM$DR_merge[pinc_WM$device == 'WM'] = 'Wash'
pinc_TD$DR_merge[pinc_TD$device == 'TD'] = 'Wash'


list = ls(pattern = "pinc")
list_dev = c('SH', 'WH','HP','DW','WM','TD')
cols = c("X2020","X2022","X2030","X2040","X2050")

list_of_objects <- mget(ls(.GlobalEnv, pattern = "pinc"), envir = .GlobalEnv)

final_inc = pinc_DW

for(i in list_of_objects[2:6]){
final_inc = rbind.data.frame(final_inc, i)
}
final_inc = final_inc[,c(1,4,6,14,24,34:37)]
colnames(final_inc)[1] = "region"
colnames(final_inc)[7] = "iso2c"
colnames(DR_part)[2] = "DR_merge" 

head(final_inc)
head(DR_part)
final_inc = merge(final_inc, DR_part, by = c("DR_merge", "iso2c"))
final_inc[,4:8] = final_inc[,4:8]*final_inc$DR_rate/100

test = melt(final_inc,id.vars = c("region","iso2c","device","DR_merge","countryname","DR_rate") )

INC_nuts2_yr_tot= test %>%
  group_by(region, variable) %>%
  summarize(INC_tot = sum(value))

INC_tot_wide = spread(INC_nuts2_yr_tot, variable, INC_tot)
colnames(INC_tot_wide) = c("region","INC_tot2020","INC_tot2022","INC_tot2030","INC_tot2040","INC_tot2050")

test$id_col = paste0("INC_",test$device,"_",test$variable)
head(test)
test = test[,c(1,2,8,9)]
final_inc_wide <- spread(test, id_col, value)
final_inc_wide = merge(final_inc_wide,INC_tot_wide)
final = merge(final_red, final_inc_wide, by = "region")
save.image('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/acheivable_NUTS2_summary.rdata')
write.csv(final,'I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/acheivable_NUTS2_summary.csv')
