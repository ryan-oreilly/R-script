library(psych)
df = read.csv2("/Users/ryanoreilly/Desktop/Energy Institute/openEntrance/data/theoretical potential/theoretical_potential.V2.csv",sep =",")
head(df)
dfEV = subset(df, df$variable =="Demand Response|Maximum Reduction|Load Shifting|Electricity|Residential|Electric Vehicle")
describeBy(test,group = test$region)

nuts2 = unique(dfEV$region)
finaldf = as.data.frame(matrix(NA, nrow = 323, ncol = c(4)))
rownames(finaldf) =nuts2
colnames(finaldf) = c("2020","2030","2040","2050")
for (nuts in nuts2){
  print(nuts)
  temp = subset(dfEV, dfEV$region == nuts)
  temp = cbind.data.frame(temp$region, as.numeric(temp$X2020),as.numeric(temp$X2030),as.numeric(temp$X2040),as.numeric(temp$X2050) )
  colnames(temp) = c("region","2020","2030","2040","2050")
  finaldf[nuts,]  = colSums(temp[,2:5])*30.42
}
DE11 = subset(dfEV, dfEV$region =="DE11")
DE11 = cbind.data.frame(DE11$region, as.numeric(DE11$X2020),as.numeric(DE11$X2030),as.numeric(DE11$X2040),as.numeric(DE11$X2050) )
colnames(DE11) = c("region","2020","2030","2040","2050")
colSums(DE11[,2:5])*30.42


write.csv(finaldf, "/Users/ryanoreilly/Desktop/Energy Institute/openEntrance/data/theoretical potential/EV_summary.csv", sep =";")

