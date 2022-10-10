library(xtable)
library(countrycode)
######################################
df.table=read_excel("/Users/ryanoreilly/Desktop/Energy Institute/openEntrance/data/EV/Images from data ELECTRIC AVE/EV.shares.count.xlsx")

table.hours=cbind.data.frame(hourlytotal, round(hourlyshare,digits = 4))
colnames(table.hours)=c("count","share")
rownames(table.hours)=c(seq(0,23,1))
xtable(table.hours,caption = "Charging count and share of daily energy demand (n= 133673)",digits = 4)
Charging count and share of daily energy demand (n= 133673)
Notes: Shares were determined by dividing the count for hour by the total observations.
"""
df.table=read.csv2("/Users/ryanoreilly/Desktop/Energy Institute/openEntrance/data/flexibilities file reference/Gils2014_tbl7_4python.csv", sep = ",")

# issues with dplyr made me do this summary table manually
df =  read.csv2('/Users/ryanoreilly/Desktop/Energy Institute/openEntrance/data/df.11.23.csv', sep=",")
variables=c("CDD", "HDD", "nflh_ac", "nflh_cp", "NHH", "rrf", "rfr", "rwm", "rtd", "rdw", "rac", "rwh", "rcp", "rsh", "rev")
countries = sort(unique(df$country))
df_sum =as.data.frame(matrix(NA, nrow =length(countries), ncol= length(variables)))
rownames(df_sum)=countries
colnames(df_sum)=variables

for (i in 1:length(countries)){
  tempdf=subset(df, df$country==countries[i] & df$hour==1)
  df_sum$CDD[i] = round(sum(as.numeric(tempdf$cdd)),digits = 0) 
  df_sum$HDD[i]=round(mean(as.numeric(unique(tempdf$Yr_HDD))),digits = 0) 
  df_sum$nflh_ac[i]=mean(as.numeric(tempdf$nflh_ac)) 
  df_sum$nflh_cp[i]=mean(as.numeric(tempdf$nflh_cp))
  df_sum$NHH[i] = round(sum(as.numeric(unique(tempdf$nhh)))/1000000, digits =1) #in millions
  df_sum$rrf[i] = mean(as.numeric(tempdf$rrf))*100
  df_sum$rfr[i] = mean(as.numeric(tempdf$rfr))*100 
  df_sum$rwm[i] = mean(as.numeric(tempdf$rwm)) *100
  df_sum$rtd[i] = mean(as.numeric(tempdf$rtd))*100
  df_sum$rdw[i] = mean(as.numeric(tempdf$rdw)) *100
  df_sum$rac[i] = mean(as.numeric(tempdf$rac))*100
  df_sum$rwh[i] = mean(as.numeric(tempdf$rwh)) *100
  df_sum$rcp[i] = mean(as.numeric(tempdf$rcp))*100
  df_sum$rsh[i] = mean(as.numeric(tempdf$rsh)) *100
  df_sum$rev[i] = round(mean(as.numeric(tempdf$rev))*100,digits = 2)
}

countries=c("Austria","Belgium","Bulgaria","Switzerland","Cyprus","Czech Republic","Germany","Denmark","Estonia", "Greece", "Spain", "Finland", "France",
            "Croatia","Hungary", "Ireland","Italy","Lithuania", "Luxembourg","Latvia","Malta","Netherlands","Norway","Poland","Portugal","Romania","Sweden",
            "Slovenia", "Slovakia","United Kingdom")
rownames(df_sum)= countries
#fill in nflh for CH
df_sum[4,3:4] =c(243,5910)
xtable(df_sum, display=rep("s",ncol(df_sum)+1))

"""




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Belgium	(BE)	Greece	(EL)	Lithuania	(LT)	Portugal	(PT)
Bulgaria	(BG)	Spain	(ES)	Luxembourg	(LU)	Romania	(RO)
Czechia	(CZ)	France	(FR)	Hungary	(HU)	Slovenia	(SI)
Denmark	(DK)	Croatia	(HR)	Malta	(MT)	Slovakia	(SK)
Germany	(DE)	Italy	(IT)	Netherlands	(NL)	Finland	(FI)
Estonia	(EE)	Cyprus	(CY)	Austria	(AT)	Sweden	(SE)
Ireland	(IE)	Latvia	(LV)	Poland	(PL)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
####################################
# EV
########################################

library(readxl)
library(dplyr)
library(xtable)
# data tables for EV
r_EV = read_excel('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/EV_NVF_EV_path.xlsx', sheet = '2021 T&E final')
r_EVfit55 = read_excel('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/EV_NVF_EV_path.xlsx', sheet = 'fit_for_55')

r_EV2 = r_EV[,c(2,3,5,15,25,35)]
r_EV2[,2:6]= r_EV2[,2:6]*100
r_EV3 = r_EV2 %>% 
  mutate_if(is.numeric, round,digits = 1)

NVF = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/New Vehicle Fleet projectionsV2.csv')
NVF = NVF[,c(1,5)]
colnames(NVF) = c('country', 'NVF')
NVF$NVF = round(NVF$NVF)

# read in Wunit EV and usable life
EV_param = read_excel('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/EV_parameters.xlsx')
EV_param = EV_param[, c(1,2,4)]
colnames(EV_param) = c('country','wunitEV','UL')
EV_param = EV_param %>% 
  mutate_if(is.numeric, round)

EV_kmannual = read_excel('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/Ev lit review.xlsx',sheet = 'km per year')
EV_kmannual =EV_kmannual[,c(1,2,4)]

EV_kmannual = EV_kmannual %>% 
  mutate_if(is.numeric, round)

# merge info together
finalEV = merge(r_EV3,NVF)

finalEV = finalEV[,c(1,7,2,3,4,5,6)]

finalEV = merge(finalEV,EV_param)
finalEV = merge(finalEV,EV_kmannual)
head(finalEV)
temp = c("","count","%","%","%","%","%","kWh","years","km","")
finalEV = rbind.data.frame(temp, finalEV)

print(xtable(finalEV, display=rep("s",ncol(finalEV)+1)), include.rownames = FALSE)


######################
# paramaters
#####################

param = as.data.frame(matrix(NA, 21, 2))
colnames(param) = c('parameter',"definition")
param$definition = c("duration of one run of device type i",
                     "heating degree days in month m",
                     "heating degree days in year y",
                     "annual full load hours of device i",
                     "annual number of runs per unit of device i",
                     "number of households",
                     "potential load increase of device i, MW",
                     "potential load reduction of device i, MW",
                     "installed capacity of device i, MW",
                     "installed capacity per unit of device i",
                     "average unit load during one run of appliance i, kW",
                     "demand on hour t, MWh",
                     "demand on day d, MWh",
                     "annual demand, MWh",
                     "equipemnt ownership rate of household device i",
                     "share of Q_{h} that can be activated for DR, % ",
                     "time, h",
                     "maximum time until the shifted load has to be balanced, h",
                     "annual electricity demand of process/device i, MWh",
                     "specific electricity demand per output unit of process i, kWh/t",
                     "annual electricity demand per unit of appliance i, kWh"
                     )
param$parameter = c('d_{cycle,i}',
                    'HDD_{m}',
                    "HDD_{y}",
                    "n_{FLH,i}",
                    "N_{cycle,i}",
                    "N_{HH}",
                    "P_{increase,i}",
                    "P_{reduction,i}",
                    "P_{installed,i}",
                    "P_{installed,i}^{unit}",
                    "P_{cycle,i}",
                    "Q_t",
                    "Q_d",
                    "Q_{year}",
                    "r_i",
                    "s_DR",
                    "t",
                    "t_{shift}",
                    "W_{i}",
                    "W_{i}^{spec}",
                    "W_{i}^{unit}"                    )

print(xtable(param, display=rep("s",ncol(param)+1)), include.rownames = FALSE)

###########################
# contry dd projections
###########################
df = readxl::read_excel('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/country dd projections.xlsx')
print(xtable(df, display=rep("s",ncol(df)+1)), include.rownames = FALSE)

##########################
# hourly profiles
##########################
df = read.csv2('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/device_shares.csv', sep =",")
df2 = read.csv2('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/stamminger_11.23.csv', sep =",")
df3 = subset(df2,df2$country == 'EU')
dffinal = cbind.data.frame(df,df3[,4:6])
colnames(dffinal) = c("hour","WH","SH","AC","CP","EV","RF/FR","WM","DW","TD")
s_hp = read.csv2('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/heat_pump_hourly_share.csv',sep=",") 
colnames(s_hp) = c("hour","HP")

for (col in 2:ncol(dffinal)){
  print(col)
  dffinal[,col] = round(as.numeric(dffinal[,col])*100,digits = 2)
}
dffinal[,2:ncol(dffinal)] = as.numeric(dffinal[,2:ncol(dffinal)])*100
colSums(dffinal)
as.numeric(dffinal[,2:ncol(dffinal)])*100

###############################
# Heat pump transition
################################

df = read.csv("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/HP_transitionV2.csv")
cop = read.csv("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/openENTRANCE final data/COP_.1deg_11-21_V1.csv")
cop$length = nchar(cop$NUTS)
cop = cop[cop$length==2,]
cop = cop[,c(2,15)]
colnames(cop) = c("country","cop")

df2 = merge(df,cop)
df2$HP_final_2018 = df2$HP_thermal_2018/df2$cop
df2$HP_final_2019 = df2$HP_thermal_2019/df2$cop
#these are removed because they are confusing. Dividing electrical_2050 by the COP would result in the final energy met by HP in 2050
#df2$electrical_2050 = df2$final_energy_15.19_nonEE+df2$HP_final_2018 !!! 
#df2$electrical_share_2050 = round(df2$electrical_2050/df2$final_energy_15.19,digits = 2)

df_final = df2[,c(1,3,6,11,10)]#,13,14)]
df_final$final_energy_15.19_nonEE_share = round(df_final$final_energy_15.19_nonEE_share/1000,digits = 2)
colnames(df_final) = c("country","total","nonEE_share_total","HP_total","COP") #,"electric_total_2050","electric_share_total_2050")
df_final[,c(2,4)]=round(df_final[,c(2,4)]/1000,digits =0)
df_final[,c(3)]=round(df_final[,c(3)]*100,digits =0)
df_final[,5] = round(df_final[,5],digits = 2)
names = c("","GWh","//%","GWh","")#,"GWh","//%")
df_final2 = rbind.data.frame(names,df_final)
library(xtable)
print(xtable(df_final2, display=rep("s",ncol(df_final2)+1)), include.rownames = FALSE)

#####################################
########################################
# reduction potential 2022, 2030, 2040, 2050 by country
######################################
######################################

d_red = read.csv("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/OE_validation/OE_data/Ave_red_device_yearV4.csv")
head(d_red)
d_red$device2 = ""
d_red$device2[d_red$device == 'Air Conditioning'] = 'AC'
d_red$device2[d_red$device == 'Circulation Pump'] = 'CP'
d_red$device2[d_red$device == 'Dish Washer'] = 'DW'
d_red$device2[d_red$device == 'Dryer'] = 'TD'
d_red$device2[d_red$device == 'Electric Vehicle'] = 'EV'
d_red$device2[d_red$device == 'Electric Vehicle_55'] = 'EV_fit'
d_red$device2[d_red$device == 'Heat Pump'] = 'SH'
d_red$device2[d_red$device == 'Refrigeration'] = 'FRRF'
d_red$device2[d_red$device == 'Storage Heater'] = 'SH'
d_red$device2[d_red$device == 'Washing Machine'] = 'WM'
d_red$device2[d_red$device == 'Water Heater'] = 'WH'


#device3 is used to convert theoretical potentials to feasible - CP, SH, and HP all use SH which is the vector representing participation rates for the heating domain
d_red$device3 = ""
d_red$device3[d_red$device == 'Air Conditioning'] = 'AC'
d_red$device3[d_red$device == 'Circulation Pump'] = 'SH'
d_red$device3[d_red$device == 'Dish Washer'] = 'Wash'
d_red$device3[d_red$device == 'Dryer'] = 'Wash'
d_red$device3[d_red$device == 'Electric Vehicle'] = 'EV'
d_red$device3[d_red$device == 'Electric Vehicle_55'] = 'EV'
d_red$device3[d_red$device == 'Heat Pump'] = 'SH'
d_red$device3[d_red$device == 'Refrigeration'] = 'FRRF'
d_red$device3[d_red$device == 'Storage Heater'] = 'SH'
d_red$device3[d_red$device == 'Washing Machine'] = 'Wash'
d_red$device3[d_red$device == 'Water Heater'] = 'WH'

d_red$countryname = countrycode(d_red$region,origin = 'iso2c',destination = 'country.name')
d_red$countryname[d_red$region=='EL'] = 'Greece'
d_red$countryname[d_red$region=='UK'] = 'United Kingdom'

# read in country participation rates and adjust to feasible reduction potentials

DR_part = read.csv("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/participation_rates_country.csv")
head(DR_part)
d_red2 = d_red

for(row in 1:nrow(d_red2)){
  print(row)
  nuts0 = d_red2$countryname[row]
  device = d_red2$device3[row]
  d_red2[row,4:c(ncol(d_red2)-3)] = round(d_red2[row,4:c(ncol(d_red2)-3)]*(as.numeric(DR_part[which(DR_part$X == nuts0),which(colnames(DR_part) == device)])/100))
}


d_red2 = d_red2[,c(2,8,16,26,36,37)]
head(d_red2)
d_red2 = subset(d_red2, !d_red2$device2 %in% c('HP', 'SH','WH','EV_fit'))

d_red3 = reshape(d_red2, idvar = "region", timevar = "device2", direction = "wide")
d_red3[,c(2:ncol(d_red3))] = round(d_red3[,c(2:ncol(d_red3))])
head(d_red3)
d_red4 = d_red3[,c(1,22:25,26:29,14:17,10:13,2:5,6:9,18:21)]
head(d_red4)
"""
d_red3[,6:9]   # CP
d_red3[,2:5]   # AC
d_red3[,10:13] # DW
d_red3[,14:17] # TD
d_red3[,18:21] # EV
d_red3[,22:25] #FRRF
d_red3[,26:29] # WM
"""
temp =c('country',rep('FRRF',4),rep('WM',4),rep('TD',4),rep('DW',4),rep('AC',4),rep('CP',4),rep('EV',4))

d_red4$countryname = countrycode(d_red4$region,origin = 'iso2c',destination = 'country.name')
d_red4$countryname[d_red4$region=='EL'] = 'Greece'
d_red4$countryname[d_red4$region=='UK'] = 'United Kingdom'
d_red4 = d_red4[,c(ncol(d_red4),2:c(ncol(d_red4)-1))]
d_red4 = rbind.data.frame(temp, d_red4)
print(xtable(d_red4, display=rep("s",ncol(d_red4)+1)), include.rownames = FALSE)
write.csv(d_red4, 'I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/achievable potential/d_red.csv' )

# ref info
head(d_red4)
ref = d_red4[2:nrow(d_red4),c(1,2,3,4,5)]
head(ref)
ref$share_2030 = as.numeric(ref$X2030.FRRF)/as.numeric(ref$X2022.FRRF)
ref$share_2040 = as.numeric(ref$X2040.FRRF)/as.numeric(ref$X2022.FRRF)
ref$share_2050 = as.numeric(ref$X2050.FRRF)/as.numeric(ref$X2022.FRRF)

#####################################
########################################
# increase potential 2022, 2030, 2040, 2050 by country
######################################
######################################

setwd('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/P_inc/country_summary')
pinc_SH = read.csv('./NUTS0_p_incSH.csv')
pinc_WH = read.csv('./NUTS0_p_incWH.csv')
pinc_HP = read.csv('./NUTS0_p_incHP.csv')
pinc_DW = read.csv('./NUTS0_p_incDW.csv')
pinc_WM = read.csv('./NUTS0_p_incWM.csv')
pinc_TD = read.csv('./NUTS0_p_incTD.csv')


pinc_SH = pinc_SH[,c(2,7,15,25,35)]
pinc_WH = pinc_WH[,c(2,7,15,25,35)]
pinc_HP = pinc_HP[c(2,7,15,25,35)]
pinc_DW = pinc_DW[,c(2,7,15,25,35)]
pinc_WM = pinc_WM[c(2,7,15,25,35)]
pinc_TD = pinc_TD[,c(2,7,15,25,35)]




SH_HP = merge(pinc_SH, pinc_HP,suffixes = c('SH','HP'), by = 'country')
WH_WM = merge(pinc_WH,pinc_WM,suffixes = c('WH','WM'), by = 'country')
TD_DW = merge(pinc_TD,pinc_DW,suffixes = c('TD','DW'), by = 'country')
templst = c(SH_HP,WH_WM)
final_inc = merge(SH_HP, WH_WM,by = 'country')
final_inc = merge(final_inc, TD_DW,by = 'country')
head(final_inc)


final_inc$countryname = countrycode(final_inc$country,origin = 'iso2c',destination = 'country.name')
final_inc$countryname[final_inc$country=='EL'] = 'Greece'
final_inc$countryname[final_inc$country=='UK'] = 'United Kingdom'


# read in country participation rates and adjust to feasible reduction potentials

DR_part = read.csv("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/participation_rates_country.csv")
head(DR_part)
final_inc2 = final_inc

library(reshape2)

final_inc2 = melt(final_inc, idvar = "country")
library(stringi)

final_inc2$device = stri_sub(final_inc2$variable,-2)
#convert washing equipment labels to 'Wash' used as reference in DR_part
final_inc2$device2 = final_inc2$device
final_inc2$device2[final_inc2$device2 =='TD'] ="Wash"
final_inc2$device2[final_inc2$device2 =='DW'] ="Wash"
final_inc2$device2[final_inc2$device2 =='WM'] ="Wash"
final_inc2$device2[final_inc2$device2 =='HP'] ="SH"
unique(final_inc2$device2)

for(row in 1:nrow(final_inc2)){
  print(row)
  nuts0 = final_inc2$countryname[row]
  device = final_inc2$device2[row]
  final_inc2[row,4] = round(final_inc2[row,4]*(as.numeric(DR_part[which(DR_part$X == nuts0),which(colnames(DR_part) == device)])/100))
}


head(final_inc2)
final_inc3 = final_inc2[,c(2,3,4)]

final_inc4 = reshape(final_inc3, idvar = "countryname", timevar = "variable", direction = "wide")
head(final_inc4)
colnames(final_inc4)[2:21] = stri_sub(colnames(final_inc4)[2:21],-6)

print(xtable(final_inc4, display=rep("s",ncol(final_inc4)+1)), include.rownames = FALSE)
write.csv(final_inc4, 'I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/achievable potential/d_inc.csv' )


#####################################
########################################
# sample level q reduction 2022, 2030, 2040, 2050
######################################
######################################
full_potential = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/Full_potential.V9.country.csv')
head(full_potential)
full_potential = full_potential[,c(3,4,6:39)]
#remove max dispatch
key = "Maximum Dispatch"
library(ggrepel)
full_potential$type = grepl(key,full_potential$variable)
full_potential = full_potential[full_potential$type!= TRUE,]
full_potential$countryname = countrycode(full_potential$region,origin = 'iso2c',destination = 'country.name')
full_potential$countryname[full_potential$region=='EL'] = 'Greece'
full_potential$countryname[full_potential$region=='UK'] = 'United Kingdom'
head(DR_part)

# DR_part from wide to long
library(data.table)
DR_part = DR_part[,c(1,6:12)]
DR_part2 = melt(DR_part,id.vars = c("X") )
# convert from demand to realistic demand response

unique(full_potential$variable)
full_potential$device = ""
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Air Conditioning"] = 'AC'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Circulation Pump"] = 'SH'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Dish Washer"]     ='Wash'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Dryer"]           ='Wash'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Electric Vehicle"]='EV'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Heat Pump"]       ='SH'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Refrigeration"]   ='FRRF'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Storage Heater"]  ='SH'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Washing Machine"] ='Wash'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Water Heater"]   = 'WH'
#drop EV fit for 55
full_potential = full_potential[full_potential$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Electric Vehicle_55",]
#drop devices that can not be delayed
full_potential = full_potential[full_potential$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Storage Heater",]
full_potential = full_potential[full_potential$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Heat Pump",]
full_potential = full_potential[full_potential$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Water Heater",]
unique(full_potential$variable)

# merge DR_part with full potential

full_potential2 = merge(full_potential,DR_part2, by.x = c("countryname","device"),by.y = c("X","variable") )
# adjust to realistic potentials
full_potential2[,c(6:38)] = full_potential2[,c(6:38)]*(full_potential2$value/100)
# sort dfs
full_potential  = full_potential[order(full_potential$region,full_potential$device),]
full_potential2 = full_potential2[order(full_potential2$region,full_potential2$device),]
head(full_potential2)
full_potential3 = full_potential2[,c(1,2,5,10,18,28,38)]
head(full_potential3)


sample_sum = full_potential3 %>%
  group_by(subannual) %>%
  summarize(x2022sum = sum(X2022),
            x2030sum = sum(X2030),
            x2040sum = sum(X2040),
            x2050sum = sum(X2050))

sample_sum2 =  as.data.frame(stat.desc(sample_sum))
sample_sum3 = round(sample_sum2[,c(2:5)],digits = 0)
sample_sum4 = sample_sum3[c(4,5,8,9),]
"""
# device specific sample averages

sample_sum_device = full_potential3 %>%
  group_by(subannual, device) %>%
  summarize(x2022sum = sum(X2022),
            x2030sum = sum(X2030),
            x2040sum = sum(X2040),
            x2050sum = sum(X2050))

sample_sum_device2 = full_potential3 %>%
  group_by(device) %>%
  summarize(Var_mean2022 = mean(X2022),
            Var_max2022 = max(X2022),
            Var_min2022 = min(X2022),
            Var_mean2030 = mean(X2030),
            Var_max2030 = max(X2030),
            Var_min2030 = min(X2030),
            Var_mean2040 = mean(X2040),
            Var_max2040 = max(X2040),
            Var_min2040 = min(X2040),
            Var_mean2050 = mean(X2050),
            Var_max2050 = max(X2050),
            Var_min2050 = min(X2050))


sample_sum_device2 = rbind.data.frame(sample_sum_device2,c("col_sum",colSums(sample_sum_device2[2:13])))
sample_sum_device_asShare = as.data.frame(matrix(NA,5,13))
sample_sum_device_asShare$V1 = sample_sum_device2$device[1:5]
colnames(sample_sum_device_asShare) = colnames(sample_sum_device2)
for(col in 2:ncol(sample_sum_device_asShare)){
  print(col)
  for (row in 1:5){
    print(row)
  sample_sum_device_asShare[row,col] = round(as.numeric(sample_sum_device2[row,col])/as.numeric(sample_sum_device2[6,col]),digits = 2)
  }
  }
"""
sample_q_red = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/OE_validation/OE_data/sample_q_red_by_device.csv')


sample_q_red = sample_q_red[,c(2,3,8,16,26,36)]
head(sample_q_red)

#drop EV fit for 55
sample_q_red = sample_q_red[sample_q_red$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Electric Vehicle_55",]
#drop devices that can not be delayed
sample_q_red = sample_q_red[sample_q_red$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Storage Heater",]
sample_q_red = sample_q_red[sample_q_red$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Heat Pump",]
sample_q_red = sample_q_red[sample_q_red$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Water Heater",]
unique(sample_q_red$variable)

library(dplyr)
library(pastecs)

sample_sumNODR = sample_q_red %>%
  group_by(subannual) %>%
  summarize(x2022sum = sum(X2022),
            x2030sum = sum(X2030),
            x2040sum = sum(X2040),
            x2050sum = sum(X2050))

sample_sumNODR2 =  as.data.frame(stat.desc(sample_sumNODR))
sample_sumNODR3 = round(sample_sumNODR2[,c(2:5)],digits = 0)
sample_sumNODR4 = sample_sumNODR3[c(4,5,8,9),]

Q_red_2022 = sample_q_red %>%
  group_by(variable) %>%
  summarize(Var_mean = mean(X2022),
            Var_max = max(X2022),
            Var_min = min(X2022))

Q_red_2030 = sample_q_red %>%
  group_by(variable) %>%
  summarize(Var_mean = mean(X2030),
            Var_max = max(X2022),
            Var_min = min(X2022))

Q_red_2030 = sample_q_red %>%
  group_by(variable) %>%
  summarize(Var_mean = mean(X2030),
            Var_max = max(X2030),
            Var_min = min(X2030))

Q_red_2040 = sample_q_red %>%
  group_by(variable) %>%
  summarize(Var_mean = mean(X2040),
            Var_max = max(X2040),
            Var_min = min(X2040))

Q_red_2050 = sample_q_red %>%
  group_by(variable) %>%
  summarize(Var_mean = mean(X2050),
            Var_max = max(X2050),
            Var_min = min(X2050))



#####################################
#####################################
# sample level p increase  2022, 2030, 2040, 2050
######################################
######################################

sample_p_inc = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/P_inc/SAMPLE_PINC.csv')
sample_p_inc$device2 = sample_p_inc$device

sample_p_inc$device2[sample_p_inc$device== "HP"] = 'SH'
sample_p_inc$device2[sample_p_inc$device== "DW"]  ='Wash'
sample_p_inc$device2[sample_p_inc$device== "TD"]  ='Wash'
sample_p_inc$device2[sample_p_inc$device== "WM"]  ='Wash'
test = merge()


#####################################
#####################################
# Annual energy demand
######################################
######################################
Q_NUTS0_device = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/OE_validation/OE_data/Q_red_device_yearV4.csv')

NUTS0 = c('DE','FR','NO','IT','ES')
Q_NUTS_ss = subset(Q_NUTS0_device,Q_NUTS0_device$region %in% NUTS0)
Q_NUTS_SS_select = Q_NUTS_ss[,c(2,3,8,16,26,36)]
Q_NUTS_SS_select[,c(3:6)] = round(Q_NUTS_SS_select[,c(3:6)]/1000,digits = 0)

Q_NUTS_SS_select$device[Q_NUTS_SS_select$device == 'Air Conditioning'] = 'AC'
Q_NUTS_SS_select$device[Q_NUTS_SS_select$device == 'Circulation Pump'] = 'CP'
Q_NUTS_SS_select$device[Q_NUTS_SS_select$device == 'Dish Washer'] = 'DW'
Q_NUTS_SS_select$device[Q_NUTS_SS_select$device == 'Dryer'] = 'TD'
Q_NUTS_SS_select$device[Q_NUTS_SS_select$device == 'Electric Vehicle'] = 'EV'
Q_NUTS_SS_select$device[Q_NUTS_SS_select$device == 'Electric Vehicle_55'] = 'EV_fit'
Q_NUTS_SS_select$device[Q_NUTS_SS_select$device == 'Heat Pump'] = 'HP'
Q_NUTS_SS_select$device[Q_NUTS_SS_select$device == 'Refrigeration'] = 'FRRF'
Q_NUTS_SS_select$device[Q_NUTS_SS_select$device == 'Storage Heater'] = 'SH'
Q_NUTS_SS_select$device[Q_NUTS_SS_select$device == 'Washing Machine'] = 'WM'
Q_NUTS_SS_select$device[Q_NUTS_SS_select$device == 'Water Heater'] = 'WH'

Q_NUTS_SS_select = Q_NUTS_SS_select[Q_NUTS_SS_select$device != 'EV_fit',] 


Q_NUTSfinal = reshape(Q_NUTS_SS_select, idvar = "device", timevar = "region", direction = "wide")
Q_NUTSfinal = Q_NUTSfinal[c(3,4,9,8,2,10,6,1,7,5),]
print(xtable(Q_NUTSfinal, display=rep("s",ncol(Q_NUTSfinal)+1)), include.rownames = FALSE)

#####################################
#####################################
# Annual energy demand sample
######################################
######################################
full_potential = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/Full_potential.V9.country.csv')
full_potential = full_potential[,c(3,4,6:39)]
#remove max dispatch
key = "Maximum Dispatch"
library(ggrepel)
full_potential$type = grepl(key,full_potential$variable)
full_potential = full_potential[full_potential$type!= TRUE,]

full_potential$device = ""
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Air Conditioning"] = 'AC'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Circulation Pump"] = 'CP'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Dish Washer"]     ='DW'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Dryer"]           ='TD'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Electric Vehicle"]='EV'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Heat Pump"]       ='HP'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Refrigeration"]   ='Ref'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Storage Heater"]  ='SH'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Washing Machine"] ='WM'
full_potential$device[full_potential$variable== "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Water Heater"]   = 'WH'
#drop EV fit for 55
full_potential = full_potential[full_potential$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Electric Vehicle_55",]


full_potential2 = full_potential[,c(1,3:36,38)]
full_potential3 = full_potential2[,c(1,36,2,7,15,25,35)]
rm(full_potential)
rm(full_potential2)
library(dplyr)
Q_sample2022 = full_potential3 %>%
  group_by(subannual, device) %>%
  summarize(Sum_2022 = sum(X2022))

Q_NUTSfinal = reshape(Q_sample2022, idvar = "subannual", timevar = "device",v.names = 'Sum_2022', direction = "wide")


Q_sample = full_potential3 %>%
  group_by(subannual,device) %>%
  summarize(Sum_2022 = sum(X2022),
            Sum_2030 = sum(X2030),
            Sum_2040 = sum(X2040),
            Sum_2050 = sum(X2050))


#####################################
#####################################
# shares by device
######################################
######################################
library(xtable) 
setwd('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/openENTRANCE final data')
s_ev=read.csv('./hourlyEVshares.csv') 
# add shares for CP, WH, SH, AC
s_allelse = read.csv('./stamminger_2009.csv') 
s_hp = read.csv('./heat_pump_hourly_share.csv') 

shares_all = cbind.data.frame(s_allelse,s_ev$S_EV,s_hp$S_HP)
for(col in 2:ncol(shares_all)){
  shares_all[,col] = round(as.numeric(shares_all[,col])*100,digits = 2)
}

print(xtable(shares_all, display=rep("s",ncol(shares_all)+1)), include.rownames = FALSE)
#convert from nuts to country level
s_wash = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/stamminger_to_latex.csv', stringsAsFactors = FALSE)
temp = as.character(s_wash[1,])
temp[2:40] = countrycode(temp[2:40],origin = 'iso2c',destination = 'country.name')
temp[c(5,18,31)] = "EU Average"
temp[c(14,27,40)] = "United Kingdom"

s_wash2 = data.frame(matrix(NA,24,40))
for(col in 1:ncol(s_wash)){
  s_wash2[,col] = round(as.numeric(s_wash[3:26,col])*100,digits = 2)
}
colnames(s_wash2) = colnames(s_wash)
s_washfinal = rbind.data.frame(temp,s_wash2)
s_washfinal[,1] = c("",seq(0,23))
head(s_washfinal)

WM = s_washfinal[,1:14]
DW = s_washfinal[,c(1,15:27)]
head(DW)
TD = s_washfinal[,c(1,28:40)]
 
print(xtable(WM, display=rep("s",ncol(WM)+1)), include.rownames = FALSE)

print(xtable(DW, display=rep("s",ncol(DW)+1)), include.rownames = FALSE)
print(xtable(TD, display=rep("s",ncol(DW)+1)), include.rownames = FALSE)




##################################
# shift parameters
##################################
library(readxl)
library(dplyr)
library(xtable)
constraints = read_excel('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/metaData.TheoreticalV8.xlsx', sheet = 'energy shift constraints')

df =  constraints%>% select('Appliance','Demand Response Measure','t.shift')
df = df[1:10,]
colnames(df)[2] = 'DR Measure'
df$Reference = "cite{gils2014a}"

print(xtable(df, display=rep("s",ncol(df)+1)), include.rownames = FALSE)


