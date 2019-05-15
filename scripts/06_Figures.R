### MAP FIGURES #### 

#Figure 1 a
library(mapdata)
library(ggplot2)
library(ggsn) #add scale bar and north arrow 
library(dplyr)

usa<-map_data("usa")  #pull out the usa map
p<-ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 

points<-select(allecos, LON_DD83, LAT_DD83, Type)
points$Type<-ordered(points$Type, levels=c("Lake", "Wetland", "Stream"))

#add points to the US map
Fig1a<-p+geom_point(data=points, size = 1, aes(x = LON_DD83, y = LAT_DD83, colour=Type, shape=Type)) + 
  scale_colour_manual(values = c("#7570b3", "#d95f02", "#1b9e77" ), name = "Freshwater Type") + 
  scale_shape_manual(values=c(0, 2, 1),  name = "Freshwater Type") +
  north(data = usa, symbol=3, scale=.1, location = "bottomright", 
        anchor = c(x = -120, y = 27)) +
  ggsn::scalebar(data = usa, dist = 500,dist_unit ="km", transform = TRUE, model = "WGS84", st.size = 2, location = "bottomleft") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA)) +
  theme(legend.position = c(0.88, 0.20), legend.text = element_text(size=9) ) +
  theme(legend.text=element_text(size=9) ) 

cowplot::save_plot("Figure 1a.png", Fig1a, base_width = 7,
                   base_aspect_ratio = 1.1)
                  
#Fig 1 B
library(rgdal)
eco9<- readOGR(dsn = "Data/Ecoregion9", layer = "Export_Output")
prj.new <-CRS("+proj=longlat +datum=NAD83 +ellps=GRS80") 
eco9.ll <- spTransform(eco9, prj.new)

## using ggplot 
#add a dataframe  
pid = sapply(slot(eco9.ll, "polygons"), 
             function(x) slot(x, "ID"))

p.df = data.frame(ID=1:length(eco9.ll), row.names = pid)
eco9_p = SpatialPolygonsDataFrame(eco9.ll, p.df)
eco_df = fortify(eco9.ll) #fortify is a ggplot function that scales coordinates to fit the plot area
names(eco_df)[which(names(eco_df)=="id")]="WSA9"
ecor_df <- plyr::join(eco_df, eco9.ll@data, by="WSA9")

  Fig1b<-ggplot(eco_df, aes(long,lat, group=group, fill=WSA9)) + 
  geom_polygon(col="black") + 
  scale_fill_manual(name="Region", values=c('#f7fbff','#deebf7','#c6dbef', '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#08519c', '#08306b' ), labels = c("CPL", "NAP", "NPL", "SAP", "SPL" , "TPL", "UMW", "WMT" , "XER") ) +
    north(data = eco_df, symbol=3, scale=.1, location = "bottomright", anchor = c(x = -120, y = 27)) +
    ggsn::scalebar(data = eco_df, dist = 500,dist_unit ="km", transform = TRUE, model = "WGS84", st.size = 2, location = "bottomleft") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA)) + theme( legend.position = c(0.94, 0.34)) + theme(legend.text=element_text(size=8) ) 


cowplot::save_plot("Figure 1b.png", Fig1b, base_width = 7, base_aspect_ratio = 1.1)

#using TMAP package
#set colors 
library(tmap)
mypalette<-c('#f7fbff', '#deebf7', '#c6dbef', '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#08519c', '#08306b')
eco<-tm_shape(eco9.ll)+
  tm_polygons('WSA9', title = 'Ecoregion', palette = mypalette ) + 
  tm_borders("black") +
  tm_layout(legend.text.size = .9, legend.title.size=1, legend.position = c("right", "bottom") ) +
  tm_compass(north = 0, type = 'arrow', position =c("left", "bottom")) + 
  tm_scale_bar(position=c("center", "bottom"))

save_tmap(eco, filename="Figure 1b.png", width = 7, asp=0)

#Figure 5 - 4-panel map
noNAs<-allecos[!(is.na(allecos$TP)),]
mid<-mean(log(noNAs$TP+1))  
TPmap<-p+ geom_point(data=noNAs, aes(x = LON_DD83, y = LAT_DD83, colour=log(TP+1), shape=Type)) + 
  scale_shape_manual(values=c(0, 1, 2),  name = "Freshwater Type")+
  scale_color_gradient2(midpoint=mid, low="darkgreen", mid="yellow2",
                        high="red", space = "Lab", name = "ln(TP) ") +
  north(data = usa, symbol=3, scale=.1, location = "bottomleft") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.92, 0.25), legend.text = element_text(size=10) ) + 
  guides(shape=FALSE)

noNAs<-allecos[!(is.na(allecos$TN)),]
mid<-mean(log(noNAs$TN+1)) 
TNmap<-p+ geom_point(data=noNAs, aes(x = LON_DD83, y = LAT_DD83, colour=log(TN+1), shape=Type)) + 
  scale_shape_manual(values=c(0, 1, 2),  name = "Freshwater Type")+
  scale_color_gradient2(midpoint=mid, low="darkgreen", mid="yellow2",
                        high="red", space = "Lab", name = "ln(TN) ") +
  north(data = usa, symbol=3, scale=.1, location = "bottomleft") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.92, 0.25), legend.text = element_text(size=10) ) + 
  guides(shape=FALSE)

noNAs<-allecos[!(is.na(allecos$CHLA)),]
mid<-mean(log(noNAs$CHLA+1))  #mean 1.9 and med 1.6 
CHLmap<-p+ geom_point(data=noNAs, aes(x = LON_DD83, y = LAT_DD83, colour=log(CHLA+1), shape=Type)) + 
  scale_shape_manual(values=c(0, 1, 2),  name = "Freshwater Type")+
  scale_color_gradient2(midpoint=mid, low="darkgreen", mid="yellow2",
                        high="red", space = "Lab", name = "ln(CHL)") +
  north(data = usa, symbol=3, scale=.1, location = "bottomleft") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.92, 0.25), legend.text = element_text(size=10) ) + 
  guides(shape=FALSE)

noNAs<-allecos[!(is.na(allecos$aqveg)),]
mid<-mean(log(noNAs$aqveg+1)) # mean 1.8 and med 1.7 
vegmap<-p+ geom_point(data=noNAs, aes(x = LON_DD83, y = LAT_DD83, colour=log(aqveg+1), shape=Type)) + 
  scale_shape_manual(values=c(0, 1, 2),  name = "Freshwater Type")+
  scale_color_gradient2(midpoint=mid, low="darkgreen", mid="yellow2",
                        high="red", space = "Lab", name = "ln(AqVeg)") +
  north(data = usa, symbol=3, scale=.1, location = "bottomleft") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.92, 0.25), legend.text = element_text(size=10) ) + 
  guides(shape=FALSE)

Fig3<-cowplot::plot_grid(TPmap, TNmap, CHLmap, vegmap, labels = c('A', 'B', "C", "D"))

cowplot::save_plot("Figure 5.png", Fig3, ncol = 2, nrow = 2, base_width = 7,
                   base_aspect_ratio = 1.1)
