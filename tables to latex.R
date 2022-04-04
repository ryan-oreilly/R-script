df.table=read_excel("/Users/ryanoreilly/Desktop/Energy Institute/openEntrance/data/EV/Images from data ELECTRIC AVE/EV.shares.count.xlsx")
library(xtable)

table.hours=cbind.data.frame(hourlytotal, round(hourlyshare,digits = 4))
colnames(table.hours)=c("count","share")
rownames(table.hours)=c(seq(0,23,1))
xtable(table.hours,caption = "Charging count and share of daily energy demand (n= 133673)",digits = 4)
Charging count and share of daily energy demand (n= 133673)
Notes: Shares were determined by dividing the count for hour by the total observations.

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
