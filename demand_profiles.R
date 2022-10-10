library(ggplot2)
install.packages('hrbrthemes')
library(hrbrthemes)
library(viridis)
library(scales)
df = read.csv2('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/device_shares.csv', sep =",")
df2 = read.csv2('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/stamminger_11.23.csv', sep =",")
df3 = subset(df2,df2$country == 'EU')
dffinal = cbind.data.frame(df,df3[,4:6])
colnames(dffinal) = c("hour","WH","SH","AC","CP","EV","RF/FR","WM","DW","TD")
s_hp = read.csv2('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/heat_pump_hourly_share.csv',sep=",") 
colnames(s_hp) = c("hour","HP")
dffinal = merge(dffinal, s_hp)

dffinal = as.data.frame(t(dffinal))
colnames(dffinal) = dffinal[1,]

dffinal = dffinal[2:11,1:24]
dffinal$device = rownames(dffinal)
rownames(dffinal) = NULL

dffinal2 =reshape(dffinal, 
        direction = "long",
        varying = list(names(dffinal)[1:24]),
        v.names = "hour_share",
        timevar = "hour",
        idvar = c("device"))
dffinal2$hour = dffinal2$hour-1
dffinal2$hour_share = round(as.numeric(dffinal2$hour_share), 4)

wash = subset(dffinal2,dffinal2$device == "WM"|dffinal2$device == "TD"|dffinal2$device == "DW")
temp = subset(dffinal2,dffinal2$device == "CP"|dffinal2$device == "AC"|dffinal2$device == "SH"|dffinal2$device == "WH"|dffinal2$device == "HP")
ev = subset(dffinal2,dffinal2$device=="EV")


pwash<-ggplot(wash, aes(x=hour, y=hour_share, color = device)) +
  geom_line(aes(color=device),size =1.5)+
  ggtitle("Demand profile of household washing devices")+
  theme_minimal()+
  ylab("Hour's share of daily demand")+
  xlim(0,23)
pwash + scale_y_continuous(labels =percent)


ptemp<-ggplot(temp, aes(x=hour, y=hour_share, color = device)) +
  geom_line(aes(linetype=device),size =.5)+
  geom_point(aes(shape=device),size =2)+
  ggtitle("Demand profile of household heating and cooling devices")+
  theme_minimal()+
  ylab("Hour's share of daily demand")+
  xlim(0,23)
ptemp + scale_x_continuous(breaks=seq(0, 23, 1)) + scale_y_continuous(labels =percent)
#ptemp + scale_y_continuous(labels =percent)



pev<-ggplot(ev, aes(x=hour, y=hour_share, color = device)) +
  geom_line(aes(color=device),size =1.5)+
  ggtitle("Demand profile of EV charging")+
  theme_minimal()+
  ylab("Hour's share of daily demand")+
  xlim(0,23)
pev + scale_y_continuous(labels =percent)


pall<-ggplot(dffinal2, aes(x=hour, y=hour_share, color = device)) +
  geom_line(aes(color=device),size =1.5)+
  ggtitle("Demand profile of household electrical devices")+
  theme_minimal()+
  ylab("Hour's share of daily demand")+
  xlim(0,23)
pall + scale_y_continuous(labels =percent)

