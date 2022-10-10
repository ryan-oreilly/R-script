library(dplyr)
library(data.table)
library(ggplot2)
#install.packages('viridis')
library(viridis)
#install.packages('countrycode')
library(countrycode)
### Set options
setwd('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/')
#setwd('Desktop')
### END - global options
### Load in data
df = read.csv('./Full_potential.V9.country.csv')
head(df)

demand=df[df$variable %like% 'Maximum Reduction', ]
demand = subset(demand, demand$variable != "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Electric Vehicle_55")


demand$variable[demand$variable == "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Air Conditioning"] = 'AC'
demand$variable[demand$variable == "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Circulation Pump"] = 'CP'
demand$variable[demand$variable == "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Dish Washer"     ] = 'DW'
demand$variable[demand$variable == "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Dryer"           ] = 'TD'
demand$variable[demand$variable == "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Electric Vehicle"] = 'EV'
demand$variable[demand$variable == "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Heat Pump"       ] = 'HP'
demand$variable[demand$variable == "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Refrigeration"   ] = 'RF'
demand$variable[demand$variable == "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Storage Heater"  ] = 'SH'
demand$variable[demand$variable == "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Washing Machine" ] = 'WM'
demand$variable[demand$variable == "Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Water Heater"    ] = 'WH'


# 2022 aggregated hourly demand
yr_2022_agg = demand %>% group_by(subannual, region)  %>%
  summarise(hour_sum = sum(X2022))
# 2022 peak demand
yr_2022_max = yr_2022_agg %>% group_by(region)  %>%
  summarise(max2022 = max(hour_sum))

peakTiming = yr_2022_agg %>%
  group_by(region) %>%
  filter(hour_sum == max(hour_sum, na.rm=TRUE))

# convert from iso to countrynames
yr_2022_max$countryname = countrycode(yr_2022_max$region, origin = 'iso2c',destination ='country.name')#,origin = 'country.name', destination = 'iso2c')
yr_2022_max$countryname[yr_2022_max$region =='EL'] = 'Greece'
yr_2022_max$countryname[yr_2022_max$region =='UK'] = 'United Kingdom'

ggplot(yr_2022_max, aes(y=max2022, x=countryname)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_viridis(discrete = T) +
  ggtitle("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("") +
  ylab("MW")

###################################
# 2022
####################################
head(demand)
yr_2022 = demand[,c(3,4,5,6,11,19,29,39)]
head(yr_2022)
unique(yr_2022$variable)
yr_2022$test = yr_2022$X2022*(365/12)
yr_sum_2022 = yr_2022 %>% group_by(region)  %>%
  summarise(total = sum(test))
head(yr_sum_2022)

##################################
# annual sums
##################################
yr_select = demand[,c(3,4,5,6,11,19,29,39)]
yr_select[,5:ncol(yr_select)] = yr_select[,5:ncol(yr_select)]*(365/12)
head(yr_select)
yr_select_sum = yr_select %>% group_by(region)  %>%
  summarise(total2022 = sum(X2022),
            total2030 = sum(X2030),
            total2040 = sum(X2040),
            total2050 = sum(X2050),
            )

yr_select2 = merge(yr_select,yr_select_sum)
yr_select2$share2022 = yr_select2$X2022/yr_select2$total2022
yr_select2$share2030 = yr_select2$X2030/yr_select2$total2030
yr_select2$share2040 = yr_select2$X2040/yr_select2$total2040
yr_select2$share2050 = yr_select2$X2050/yr_select2$total2050

colnames(yr_select2)[2] = 'Device'
######################################
# stackplot annual total by country and device
######################################

# 2022


p2022 = ggplot(yr_select2, aes(y=share2022, x=region, fill = Device)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_discrete(limits=rev)+
  scale_y_continuous(labels = scales::percent)+
  theme_bw() +
  ylab("")+
  xlab("")+
  coord_flip()
p2022
Device = c(rep('AC',30))
temp2022 = cbind.data.frame(unique(yr_select2[,c(1,9)]),Device)
temp2022$total = round(temp2022$total2022)

p2022 + geom_text(data = temp2022,
              aes(y=1, x=region,label = format(round(as.numeric(temp2022$total), 1), nsmall=0, big.mark=",")),nudge_y = .05)


# 2030


p2030 = ggplot(yr_select2, aes(y=share2030, x=region, fill = Device)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_discrete(limits=rev)+
  scale_y_continuous(labels = scales::percent)+
  theme_bw() +
  ylab("")+
  xlab("")+
  coord_flip()
p2030
Device = c(rep('AC',30))
temp2030 = cbind.data.frame(unique(yr_select2[,c(1,10)]),Device)
temp2030$total = round(temp2030$total2030)

p2030 + geom_text(data = temp2030,
              aes(y=1, x=region,label = format(round(as.numeric(temp2030$total), 1), nsmall=0, big.mark=",")),nudge_y = .05)

# 2040


p2040 = ggplot(yr_select2, aes(y=share2040, x=region, fill = Device)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_discrete(limits=rev)+
  scale_y_continuous(labels = scales::percent)+
  theme_bw() +
  ylab("")+
  xlab("")+
  coord_flip()
p2040
Device = c(rep('AC',30))
temp2040 = cbind.data.frame(unique(yr_select2[,c(1,11)]),Device)
temp2040$total = round(temp2040$total2040)

p2040 + geom_text(data = temp2040,
                  aes(y=1, x=region,label = format(round(as.numeric(temp2040$total), 1), nsmall=0, big.mark=",")),nudge_y = .05)

# 2050

p2050 = ggplot(yr_select2, aes(y=share2050, x=region, fill = Device)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_discrete(limits=rev)+
  scale_y_continuous(labels = scales::percent)+
  theme_bw() +
  ylab("")+
  xlab("")+
  coord_flip()
p2050
Device = c(rep('AC',30))
temp2050 = cbind.data.frame(unique(yr_select2[,c(1,12)]),Device)
temp2050$total = round(temp2050$total2050)

p2050 + geom_text(data = temp2050,
                  aes(y=1, x=region,label = format(round(as.numeric(temp2050$total), 1), nsmall=0, big.mark=",")),nudge_y = .05)

#################################
library(tidyr)
data_long <- gather(demand, value = MW, key= yr_tmp, 7:39 )
data_long = data_long[,c(3,4,6,7,8)]
head(data_long)

data_long = data_long %>% group_by(subannual, yr_tmp)  %>%
  summarise(GW_sum = sum(MW)/1000)

data_long$date=strptime(data_long$subannual, "%m-%d %H:%M")
data_long$yr=strptime(data_long$yr_tmp, "X%Y")
data_long$month =month(data_long$date)
data_long$hour =hour(data_long$date)
data_long$year = year(data_long$yr)
#extract years 2020-2050
data_long = data_long[data_long$year>2019,]

data_long = data_long %>% select(year, month, hour, GW_sum)
data_long$season = ''
data_long$season[data_long$month %in% c(12,1,2)] ="winter"
data_long$season[data_long$month %in% c(3,4,5)] ="spring"
data_long$season[data_long$month %in% c(6,7,8)] ="summer"
data_long$season[data_long$month %in% c(9,10,11)] ="fall"

data_long$season2 = factor(data_long$season, levels = c("winter", "spring","summer","fall"))

data_long$index = 0
data_long$index[data_long$month == 12] = "first month"
data_long$index[data_long$month == 1] ="second month"
data_long$index[data_long$month == 2] ="third month"

data_long$index[data_long$month == 3] ="first month"
data_long$index[data_long$month == 4] ="second month"
data_long$index[data_long$month == 5] ="third month"

data_long$index[data_long$month == 6] ="first month"
data_long$index[data_long$month == 7] ="second month"
data_long$index[data_long$month == 8] ="third month"

data_long$index[data_long$month == 9] ="first month"
data_long$index[data_long$month == 10] ="second month"
data_long$index[data_long$month == 11] ="third month"
data_long$index = factor(data_long$index, levels = c("first month","second month","third month"))


rm(df)
#jan_Jun = subset(data_long, data_long$month <=6)
#jul_dec = subset(data_long, data_long$month >=7)


p<-ggplot(data_long,aes(year,hour,fill=GW_sum))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="GW",option ="C")
p<-p+ facet_grid(season2~index)
p<-p+ scale_y_continuous(trans = "reverse", breaks = unique(data_long$hour))
p<-p+ scale_x_continuous(breaks =c(2020,2050))
p<-p+ theme_minimal(base_size = 20)
#p<-p+ labs(title= paste("Residential electricity demand for an average day in a month for the 2022-2050 (GW)"), x="year", y="hour")
p<-p+ theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y.left=element_text(size=6)) + # hour
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=10))+ # years
  theme(legend.title=element_text(size=16))+ # legend
  theme(legend.text=element_text(size=9))+ # legend text
  removeGrid()#ggExtra+
p

# extract rows with peak hours
years = seq(2020, 2050)
for (yr in years){
  temp_df = data_long[data_long$year == yr ,]
  temp = temp_df[temp_df$GW_sum == max(temp_df$GW_sum) ,]
  print(temp)
}
# show change in peak load hours
temp_df = data_long[data_long$hour >15 & data_long$hour <20,]

df2 <- temp_df%>%
  group_by(hour, month) %>%
  arrange(year) %>%
  mutate(pct.chg = 100*(GW_sum - lag(GW_sum))/lag(GW_sum))
# get rid of NAs
df2 = df2 %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))

df2$season2 = factor(df2$season, levels = c("winter", "spring","summer","fall"))

df2$index = 0
df2$index[df2$month == 12] = "first month"
df2$index[df2$month == 1] ="second month"
df2$index[df2$month == 2] ="third month"

df2$index[df2$month == 3] ="first month"
df2$index[df2$month == 4] ="second month"
df2$index[df2$month == 5] ="third month"

df2$index[df2$month == 6] ="first month"
df2$index[df2$month == 7] ="second month"
df2$index[df2$month == 8] ="third month"

df2$index[df2$month == 9] ="first month"
df2$index[df2$month == 10] ="second month"
df2$index[df2$month == 11] ="third month"
df2$index = factor(df2$index, levels = c("first month","second month","third month"))

p<-ggplot(df2,aes(year,hour,fill=pct.chg))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="% change",option ="C")
p<-p+ facet_grid(season2~index)
p<-p+ scale_y_continuous(trans = "reverse", breaks = unique(data_long$hour))
p<-p+ scale_x_continuous(breaks =c(2020,2050))
p<-p+ theme_minimal(base_size = 20)
#p<-p+ labs(title= paste("Residential electricity demand for an average day in a month for the 2022-2050 (GW)"), x="year", y="hour")
p<-p+ theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y.left=element_text(size=15)) + # hour
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=10))+ # years
  theme(legend.title=element_text(size=16))+ # legend
  theme(legend.text=element_text(size=9))+ # legend text
  removeGrid()#ggExtra+
p

temp_17
years = seq(2020, 2050)
for (yr in years){
  temp_df = data_long[data_long$year == yr ,]
  temp = temp_df[temp_df$GW_sum == max(temp_df$GW_sum) ,]
  print(temp)
}

plt_7_12<-ggplot(jul_dec,aes(year,hour,fill=GW_sum))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Hourly energy demand in GW",option ="C")
plt_7_12<-plt_7_12+ facet_grid(index~month)
plt_7_12<-plt_7_12+ scale_y_continuous(trans = "reverse", breaks = unique(data_long$hour))
plt_7_12<-plt_7_12+ scale_x_continuous(breaks =c(2022,2050))
plt_7_12<-plt_7_12+ theme_minimal(base_size = 8)
plt_7_12<-plt_7_12+ labs(title= paste("Residential energy 2022-2050 (GW)"), x="year", y="hour")
plt_7_12<-plt_7_12+ theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra

plt_7_12
