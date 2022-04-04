""" 
The calculation of willingness to participate in controlled DSM
# questions relating to DSM
# 45 many people would support me if I allowed my grid operator to remotely switch on and off non critical appliances in my home.
# 46 A growing number of people in...have allowed their grid operator to remotely switch on and off non critical appliances in their home.
# 47 In my opinion, people will allow grid operators to remotely switch on and off their non critical appliances as soon as the current obstacles will be dealt with.
# 52 Suppose you allow your grid operator to remotely switch on and off non critical appliances in your home. Would you say that would benefit the energy transition?
# 53 I feel a personal obligation to allow my grid operator to remotely switch on and off non-critical appliances in my home.
# 54 How would you feel if you allowed your grid operator to remotely switch on and off non-critical appliances in your home?
# 65. I would accept energy policies that protect the environment even when these induce some loss in comfort (e.g., policies that restrict car traffic in cities).
# 68 I intend to decrease my energy consumption for heating and cooling my dwelling.describeBy(df$nationality,df$Q45_1)
102. How often do you disconnect electric appliances from the power supply when you are currently not using them? (Specifically TV, PC, Notebook, DVD-Player etc.)
# !!!!!!!!!!!!

question 70 is the one most relevant to the study

# 70 Would you allow your grid operator to remotely switch on and off non critical appliances in your home if you were offered an annual discount of [assign the national bid price from list "Bid Values in nat currencies.xlsx", store used bid value as variable "bVremote" in dataset]

"""
library(psych)
library(readxl)
df <- read_excel("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/ECHOESdata/International survey/ECHOES_raw_data_int_survey.xlsx")


df2 = read.csv2("I:/Projekte/SMARTEES H2020 - WV0139/Work Phase/Papers/Biking/BikingEstimationDataV1.csv",sep = ";") # used for country index



#
ctrycode = unique(cbind.data.frame(df2$country.x,df2$country_sample))
#ctrycode = order(ctrycode)
ctrycode = ctrycode[order(ctrycode$`df2$country.x` ),]
# raw data
DSM = cbind.data.frame(df$country_sample, df$Q70_1, df$bVremote)
DSM = DSM[complete.cases(DSM), ]
colnames(DSM) = c("cntrycode","Q70","price")
total_cntry = table(DSM$cntrycode)
q_70 = subset(DSM, DSM$Q70 > 3)
WTP_DSM = table(q_70$cntrycode)

WTP_DSM_final = cbind.data.frame(ctrycode,WTP_DSM, total_cntry)
colnames(WTP_DSM_final) = c("ctrycode","country","ctrycode2","WTP_DSM","ctrycode3","Total")

WTP_DSM_final = WTP_DSM_final[,c(2,4,6)]
WTP_DSM_final$WTP = round(as.numeric(WTP_DSM_final$WTP_DSM/WTP_DSM_final$Total)*100,digits = 2)
annual_discount = unique(DSM[,c(1,3)])[2]
WTP_DSM_final$annual_discount = as.numeric(unlist(annual_discount))
colnames(WTP_DSM_final) = c("country","WTP_DSM","Total",  "WTP",  "annual_discount")


WTP_DSM_final$mean_reference = WTP_DSM_final$WTP/mean(WTP_DSM_final$WTP)
########################
# bring in info from lit review
########################
library(data.table)
df_lit = read_excel("C:/Users/AK194059/Desktop/DR summaryV2.xlsx")
df_lit$pubID = rownames(df)

dflong = melt(setDT(df_lit), id.vars = c("...1","continent", "payment", "country","Type of DR","hypothetical"), 
              measure.vars = c("WM/TD/DW","RF/FR","AC","WH","SH","EV","other"),
              value.name = c("Participation"),variable.name = c("device"))
dflong = na.omit(dflong)





#########################
# adjust for 'warm glow' factor of 0.45
#########################
dflong$participation_adj= dflong$Participation

#adjustment
for (i in 1:dim(dflong)[1]){
  if (dflong$hypothetical[i] == 'yes') {dflong$participation_adj[i] = dflong$Participation[i]*.45}}#round(dflong$Participation[i]*.45,digits = 2) }}

library(dplyr)
#means
gd = dflong %>%
  group_by(device)%>%
  summarise(participation_adj = mean(participation_adj))

gd$group2 = "mean"  

rownames(gd) = gd$device
############################
# combine ECHOES results and lit review
###########################
WTP_DSM_final$AC   = as.numeric(gd['AC',][2])*WTP_DSM_final$mean_reference
WTP_DSM_final$SH   = as.numeric(gd['SH',][2])*WTP_DSM_final$mean_reference
WTP_DSM_final$WH   = as.numeric(gd['WH',][2])*WTP_DSM_final$mean_reference
WTP_DSM_final$EV   = as.numeric(gd['EV',][2])*WTP_DSM_final$mean_reference
WTP_DSM_final$Wash = as.numeric(gd['WM/TD/DW',][2])*WTP_DSM_final$mean_reference
WTP_DSM_final$FRRF = as.numeric(gd['RF/FR',][2])*WTP_DSM_final$mean_reference
WTP_DSM_final$Other = as.numeric(gd['other',][2])*WTP_DSM_final$mean_reference


# adjust foreign currencies to 2018 Euro
#https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=ert_bil_eur_a&lang=en

rownames(WTP_DSM_final) = WTP_DSM_final$country
WTP_DSM_final = WTP_DSM_final
WTP_DSM_final['Bulgaria',][5] = WTP_DSM_final['Bulgaria',][5]/1.9558 
WTP_DSM_final['Croatia',][5] = WTP_DSM_final['Croatia',][5]/7.41820  
WTP_DSM_final['Czech_Republic',][5] = WTP_DSM_final['Czech_Republic',][5]/25.647  
WTP_DSM_final['Denmark',][5] = WTP_DSM_final['Denmark',][5]/7.4532  
WTP_DSM_final['Hungary',][5] = WTP_DSM_final['Hungary',][5]/318.89  
WTP_DSM_final['Poland',][5] = WTP_DSM_final['Poland',][5]/4.2615  
WTP_DSM_final['Romania',][5] = WTP_DSM_final['Romania',][5]/4.6540  
WTP_DSM_final['Sweden',][5] = WTP_DSM_final['Sweden',][5]/10.2583  
WTP_DSM_final['United_Kingdom',][5] = WTP_DSM_final['United_Kingdom',][5]/0.88471 
WTP_DSM_final['Norway',][5] = WTP_DSM_final['Norway',][5]/9.5975  
WTP_DSM_final['Turkey',][5] = WTP_DSM_final['Turkey',][5]/5.7077  
WTP_DSM_final['Switzerland',][5] = WTP_DSM_final['Switzerland',][5]/1.1550  

WTP_DSM_final$annual_discount = round(WTP_DSM_final$annual_discount,digits = 1)

final = WTP_DSM_final[, c(1,3,4,5,6,7:13)]
final[,5:12] = final[,5:12]*100
final[,2:12] = round(final[,2:12], digits = 1)
colnames(final) = c("country","N",  "pct.yes",  "bid value", "mean_reference" , "AC",     "SH",     "WH","EV",     "Wash",   "FRRF",   "Other")          
final = final[,2:12]
final = final[, c(1:4,9,10,5,7,6,8,11)]

library(xtable)
xtable(final, display=rep("s",ncol(final)+1))

temp  = as.data.frame(colSums(final)/31)
colnames(temp) = 'mean'
temp$mean
