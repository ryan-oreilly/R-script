library(dplyr)
EV = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/d_EVV7.csv')
HP = read.csv('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/data/theoretical potential/d_HPV4.csv')

head(EV)
EV = EV[,c(6,14,24,34,35,36,38)]

EV2 = EV
EV2[,1:4] = EV2[,1:4]*(365/12)
EV2_sum= EV2 %>%
  group_by(nutscode) %>%
  summarize(EV_2022GW = sum(X2022)/1000,
            EV_2030GW = sum(X2030)/1000,
            EV_2040GW = sum(X2040)/1000,
            EV_2050GW = sum(X2050)/1000)


HP = HP[,c(6,14,24,34,35,36,38)]
HP2 = HP
HP2[,1:4] = HP2[,1:4]*(365/12)
HP2_sum= HP2 %>%
  group_by(nutscode) %>%
  summarize(HP_2022GW = sum(X2022)/1000,
            HP_2030GW = sum(X2030)/1000,
            HP_2040GW = sum(X2040)/1000,
            HP_2050GW = sum(X2050)/1000)
final = merge(EV2_sum,HP2_sum)
write.csv(final,'I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/OE_validation/OE_data/HP_EV_annual.csv')
