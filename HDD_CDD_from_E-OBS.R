library(readxl)
load("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/tg_mean025_2011_2021.RData")
rownames(tmp_df03) = NULL
coordiate<-tmp_df03[,1:2]

dailytemp<-tmp_df03[,3:367]

CDD= dailytemp

#calc CDD
for (row in 1:nrow(CDD)){
  print(row)
  for (col in 1:ncol(CDD)){
    if(as.numeric(CDD[row,col]>=24) ==TRUE){CDD[row,col]=(as.numeric(CDD[row,col])-21)}
    else{CDD[row,col]=as.numeric(0)}}
}

HDD= dailytemp
#calc HDD
for (row in 1:nrow(HDD)){
  print(row)
  for (col in 1:ncol(HDD)){
    if((as.numeric(HDD[row,col])<=15) ==TRUE){HDD[row,col]=(18-as.numeric(HDD[row,col]))}
    else{HDD[row,col]=as.numeric(0)}}
}


#CDD

Jan<-CDD[,1:31]
Jan_sum<-as.data.frame(rowSums(Jan))

Feb<-CDD[,32:59]
Feb_sum<-as.data.frame(rowSums(Feb))

March<-CDD[,60:90]
March_sum<-as.data.frame(rowSums(March))

April<-CDD[,91:120]
April_sum<-as.data.frame(rowSums(April))

May<-CDD[,121:151]
May_sum<-as.data.frame(rowSums(May))

June<-CDD[,152:181]
June_sum<-as.data.frame(rowSums(June))

July<-CDD[,182:212]
July_sum<-as.data.frame(rowSums(July))

August<-CDD[,213:243]
August_sum<-as.data.frame(rowSums(August))

Septmber<-CDD[,244:273]
Septmber_sum<-as.data.frame(rowSums(Septmber))

October<-CDD[,274:304]
October_sum<-as.data.frame(rowSums(October))

November<-CDD[,305:334]
November_sum<-as.data.frame(rowSums(November))

December<-CDD[,335:365]
December_sum<-as.data.frame(rowSums(December))

df_list<-mget(ls(pattern = "\\_sum"))

temp_2011_2021_month<-cbind.data.frame(df_list)

temp_2011_2021_month$row_ID<-seq.int(nrow(temp_2011_2021_month))

temp_2011_2021_month<-cbind.data.frame(coordiate,temp_2011_2021_month)

temp_2011_2021_month<-temp_2011_2021_month[c("row_ID","Var1","Var2","rowSums(Jan)","rowSums(Feb)","rowSums(March)",
                                             "rowSums(April)","rowSums(May)","rowSums(June)","rowSums(July)",
                                             "rowSums(August)","rowSums(Septmber)","rowSums(October)",
                                             "rowSums(November)","rowSums(December)")]

colnames(temp_2011_2021_month)<-c("row_ID","longitude","latitude","Jan","Feb","March","April",
                                  "May","June","July","August","Septmber","October","November","December")

write.csv(temp_2011_2021_month,file = "I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/temp_2011_2021_month_CDD.xlsx")


#HDD

Jan<-HDD[,1:31]
Jan_sum<-as.data.frame(rowSums(Jan))

Feb<-HDD[,32:59]
Feb_sum<-as.data.frame(rowSums(Feb))

March<-HDD[,60:90]
March_sum<-as.data.frame(rowSums(March))

April<-HDD[,91:120]
April_sum<-as.data.frame(rowSums(April))

May<-HDD[,121:151]
May_sum<-as.data.frame(rowSums(May))

June<-HDD[,152:181]
June_sum<-as.data.frame(rowSums(June))

July<-HDD[,182:212]
July_sum<-as.data.frame(rowSums(July))

August<-HDD[,213:243]
August_sum<-as.data.frame(rowSums(August))

Septmber<-HDD[,244:273]
Septmber_sum<-as.data.frame(rowSums(Septmber))

October<-HDD[,274:304]
October_sum<-as.data.frame(rowSums(October))

November<-HDD[,305:334]
November_sum<-as.data.frame(rowSums(November))

December<-HDD[,335:365]
December_sum<-as.data.frame(rowSums(December))

df_list<-mget(ls(pattern = "\\_sum"))

temp_2011_2021_month<-cbind.data.frame(df_list)

temp_2011_2021_month$row_ID<-seq.int(nrow(temp_2011_2021_month))

temp_2011_2021_month<-cbind.data.frame(coordiate,temp_2011_2021_month)

temp_2011_2021_month<-temp_2011_2021_month[c("row_ID","Var1","Var2","rowSums(Jan)","rowSums(Feb)","rowSums(March)",
                                             "rowSums(April)","rowSums(May)","rowSums(June)","rowSums(July)",
                                             "rowSums(August)","rowSums(Septmber)","rowSums(October)",
                                             "rowSums(November)","rowSums(December)")]

colnames(temp_2011_2021_month)<-c("row_ID","longitude","latitude","Jan","Feb","March","April",
                                  "May","June","July","August","Septmber","October","November","December")

write.csv(temp_2011_2021_month,file = "I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/temperature/temp_2011_2021_month_HDD.xlsx")
