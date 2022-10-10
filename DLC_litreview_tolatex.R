# objective of this script is to make the latex table that summarized DLC lit review

#############################
# table to latex
#############################
library(readxl)
library(xtable)
df_latx = read_excel("C:/Users/AK194059/Desktop/DR summaryV2.xlsx", sheet = "final simplified-for overleaf")
df_latx2 = df_latx[,2:11]
df_latx2[,4:10] = round(df_latx2[,4:10]*100,digits = 0)

xtable(df_latx2)
