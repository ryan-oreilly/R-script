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
df = ECHOES_raw_data_int_survey
rm(ECHOES_raw_data_int_survey)
table(df$Q45_1)
df$country_sample


df2 = read.csv2("I:/Projekte/SMARTEES H2020/Work Phase/Papers/Biking/BikingEstimationDataV1.csv",sep = ";")

ctrycode = unique(cbind.data.frame(df2$country.x,df2$country_sample))
ctrycode = order(ctrycode)
DSM = cbind.data.frame(df$country_sample, df$Q70_1, df$bVremote)
DSM = DSM[complete.cases(DSM), ]
colnames(DSM) = c("cntrycode","Q70","price")
total_cntry = table(DSM$cntrycode) # total complete observations per country
q_70 = subset(DSM, DSM$Q70 > 3)
WTP_DSM = table(q_70$Q70)

WTP_DSM_final = cbind.data.frame(sort(ctrycode$`df2$country_sample`),WTP_DSM,total_cntry)
colnames(WTP_DSM_final) = c("ctrycode","sadf","WTP_DSM","qwer","Total")
WTP_DSM_final = cbind.data.frame(WTP_DSM_final$ctrycode,WTP_DSM_final$WTP_DSM,WTP_DSM_final$Total)
colnames(WTP_DSM_final) = c("ctrycode","WTP_DSM","Total")      
WTP_DSM_final$WTP = round(as.numeric(WTP_DSM_final$WTP_DSM/WTP_DSM_final$Total)*100,digits = 2)

#price of participation
costDSM = cbind.data.frame(df$country_sample,df$bVremote)
costDSM = unique(costDSM)

WTP_DSM_final$annual_discount = costDSM$`df$bVremote`

write.csv(WTP_DSM_final,"I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/WTP_DSM_echoes.csv",sep = ";",row.names = FALSE)
