library(ggplot2)
library(readxl)
library(data.table)
install.packages('scales')
library(scales)
df = read_excel("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/OE_data_analysis/openEntrance/Lit Review/DR summaryV2.xlsx")
df$pubID = rownames(df)
colnames(df)[11] ="SH-HP"
colnames(df)[7] = "Wash"
colnames(df)[8] = 'Ref'
dflong = melt(setDT(df), id.vars = c("...1","continent", "payment", "country","Type of DR","hypothetical","pubID"), 
              measure.vars = c("Wash","Ref","AC","WH","SH-HP","EV","other"),
              value.name = c("Participation"),variable.name = c("device"))
#dflong$Participation = round(dflong$Participation, digits = 2)
dflong = na.omit(dflong)
dflong$payment = as.factor(dflong$payment)
dflong$Participation = dflong$Participation#*100
dflong$payYN = "yes"
for (row in 1:dim(dflong)[1]){
  if (dflong$payment[row]== 0) {dflong$payYN[row] = "no"}}

#create index for grouping
dflong$group2 = NA
for (i in 1:dim(dflong)[1]){
  if (dflong$hypothetical[i]== 'yes' & dflong$payment[i]== 1) {dflong$group2[i] = "HWP"}
  if (dflong$hypothetical[i]== 'yes' & dflong$payment[i]== 0) {dflong$group2[i] = "HNP"}
  if (dflong$hypothetical[i]== 'no' & dflong$payment[i]== 1) {dflong$group2[i] = "NHWP"}
  if (dflong$hypothetical[i]== 'no' & dflong$payment[i]== 0) {dflong$group2[i] = "NHNP"}}

dflong$group2 =ordered(dflong$group2, levels= c("HWP","HNP","NHWP","NHNP","mean"))
base = ggplot(data = dflong, aes(x =device,y=Participation, shape = group2, colour = group2))+
  geom_point(size = 4)+
  scale_colour_manual(name = "Type of DLC participation result",
                      labels = c("Hypothetical        | payment offered", "Hypothetical        | no payment", "Not hypothetical | payment offered", "Not hypothetical | no payment"),
                      values = c("indianred2", "lightseagreen","indianred2", "lightseagreen")) +   
  scale_shape_manual(name = "Type of DLC participation result",
                      labels = c("Hypothetical        | payment offered", "Hypothetical        | no payment", "Not hypothetical | payment offered", "Not hypothetical | no payment"),
                      values = c(19, 19,17, 17))#+
  #ggrepel::geom_text_repel(data = dflong, aes(label = pubID), color = "black", size = 3,show.legend = F)

base
base = base + scale_y_continuous(labels = percent)

labelled = base +
  labs(
    x = "Household Devices",
    y = "Participation %"#,
    #title = "Participation Rates of DLC program by household device and result type"
  ) #+
  #scale_colour_brewer(type = "seq", palette = "Spectral")
labelled


styled <- labelled +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.justification = c(0, 1),
#    legend.position = c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank()
  )
styled


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
gd$pubID = NA
#gd$participation_adj = round(gd$participation_adj,digits = 2)
#consolidate into one dataframe
dflong2 = cbind.data.frame(dflong$device,dflong$participation_adj,dflong$group2, dflong$pubID)
colnames(dflong2) = c(colnames(gd))
dflong2 = rbind.data.frame(dflong2, gd)

base2 = ggplot(data = dflong2, aes(x =device,y=participation_adj, shape = group2, colour = group2))+
  geom_point(size = 4)+
  scale_colour_manual(name = "Type of DLC participation result",
                      labels = c("Hypothetical adj.| payment offered", "Hypothetical adj.| no payment", "Not hypothetical | payment offered", "Not hypothetical | no payment","Average participation"),
                      values = c("indianred2", "lightseagreen","indianred2", "lightseagreen","black")) +   
  scale_shape_manual(name = "Type of DLC participation result",
                     labels = c("Hypothetical adj.| payment offered", "Hypothetical adj.| no payment", "Not hypothetical | payment offered", "Not hypothetical | no payment","Average participation"),
                     values = c(19, 19,17, 17, 18))+ scale_y_continuous(labels = percent, limits =c(0,1)) 
base2

labelled2 = base2 +
  labs(
    x = "Household Devices",
    y = "Participation %"#,
    #title = "Adjusted Participation Rates of DLC program by household device and result type"
  ) #+
#scale_colour_brewer(type = "seq", palette = "Spectral")
labelled2


styled2 <- labelled2 +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.justification = c(0, 1),
    #    legend.position = c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank()
  )
styled2


#############################
#create country and device specific participation rates
#############################

echoes <- read_excel("I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/WTP_DSM_echoes.xlsx")
echoes$AC =echoes$`mean reference`*gd
echoes$SH
echoes$WH
echoes$EV
echoes$wash
echoes$FRRF
gd[]
