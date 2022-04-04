library(rgdal)


nuts2=readOGR('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/NUTS_RG_60M_2021_3035_LEVL_2.shp/NUTS_RG_60M_2021_3035_LEVL_2.shp')
plot(nuts2)
View(nuts2@data)

dfEV = read.csv2('I:/Projekte/OpenEntrance - WV0173/Durchführungsphase/WP6/CS1/EV_summary.csv', sep = ";")
colnames(dfEV) = c("NUTS_ID", seq(2018,2050,1))



dfEVmap = merge(nuts2, dfEV, by = intersect("NUTS_ID","NUTS_ID"))

dfEVmap$FID = as.numeric(dfEVmap$FID)
dfEVmap$NUTS_ID = as.numeric(dfEVmap$NUTS_ID)
dfEVmap = fortify(nuts2)
install.packages('viridis')
library(viridis)

p <- ggplot() +
  geom_polygon(data = dfEVmap, aes(fill = '2018', x = long, y = lat) , size=0, alpha=0.9) +
  theme_void()


+
  scale_fill_viridis(name="MW",
    guide = guide_legend( keyheight = unit(3, units = "mm"), 
    keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "EV annual electricity demand (MW)",
    subtitle = "208",
    caption = "Data: INSEE | Creation: Yan Holtz | r-graph-gallery.com"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09)
  ) +
  coord_map()
p
