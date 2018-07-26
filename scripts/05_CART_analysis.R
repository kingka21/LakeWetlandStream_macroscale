### Written by Katelyn King 
#### Start Date: March 


### Download libraries 
install.packages("rpart")
install.packages("dplyr")
install.packages("proj.4")
install.packages("dplyr")
library(rpart)
library(dplyr)
library(gtools)

#ecoregion map 
library(rgdal)
library(maptools)
library(sp)
library(tmap)
library(ggplot2)

###  Load in the data 
lake <- read.csv("Data/NLA2012_data.csv", header=T)
stream<-read.csv("Data/NRSA0809_data.csv", header=T)
wetland<-read.csv("Data/NWCA2011_data.csv", header=T)

### Comine all 3 ecosystems 
allecos<-gtools::smartbind(lake, stream, wetland)

### CART analysis 
# For each independent variable, the data is split at several split points. 
#At each split point, the "error" between the predicted value and the actual values is squared to get a "Sum of Squared Errors (SSE)". 
# the variable/point yielding the lowest SSE is chosen as the root node/split point. This process is recursively continued.
       

##### TP ########################
hist(allecos$TP)
allecos$logTP <- log(1+allecos$TP)
hist(allecos$logTP) #normal 

fitall<-rpart(logTP ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + WS_AREA + POPDEN + ROADDEN + NDEP + 
                TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83,
              method="anova", data=allecos, control =  rpart.control(xval=10))

summary(fitall)

# Prune based on 1SE rule  (xerror + xstd of the minimum and then pruning at the row with xerror just below that)
plotcp(fitall, main="TP pruning") #chose the CP value that is furthest left under the line (1SE above the min error)
printcp(fitall) #shows a table of CP values and xerror/xstd
fitall1<-prune(fitall, cp=0.012552)  #use the CP value from the row where pruning should occur
post(fitall1, title = "Drivers of TP in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE)  #horizontal = TRUE

par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fitall) # visualize cross-validation results  (two graphs)

## extract data from any node for investigation!! 
install.packages('partykit')
library(partykit)
library(car)
pfit <- as.party(fitall1)  #change the format to fit this package
plot(pfit)
data<-data_party(pfit)
data$class<-data$'(fitted)'
data$class<-as.factor(data$class)
data$newclass<-recode(data$class,"3=1;5=2;6=3;9=4;10=5;12=6;14=7;16=8;17=9")
data2 <- data_party(pfit, 2) 
data7 <- data_party(pfit, 7) 
data8 <- data_party(pfit, 8) 
data11 <- data_party(pfit, 11) 
data16 <- data_party(pfit, 16)
data17 <- data_party(pfit, 17)


library(ggmap)
library(stringr)
library(devtools)
library(mapdata)
library(RColorBrewer)
display.brewer.all()
devtools::install_github("dkahle/ggmap")
usa<-map_data("usa")  #pull out the usa map
p<-ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 


#terminal class map 
p1<-p+ geom_point(data=data, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(1, 16, 1, 16, 1, 16, 1, 16, 16),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class")
#scale_color_manual(values=c('skyblue3','darkblue', 'lightgreen', 'palegreen2', 'darkorange', 'darkorange', "plum3", "plum3", "khaki1"))

#terminal map without last two classes 
datasub<-filter(data, newclass== '1' | newclass== '2' | newclass== '3'| newclass== '4' | newclass== '5' | newclass== '6' | newclass== '7'   )
p+ geom_point(data=datasub, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(1, 16, 1, 16, 16, 1, 1),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class")
#% Forest map 
p+ geom_point(data=data2, aes(x = LON_DD83, y = LAT_DD83, shape=Type, colour=(">=30% Forest") )) + 
  geom_point(data=data7, aes(x = LON_DD83, y = LAT_DD83, shape=Type, colour=("<30% Forest") ))
#region map
p+ geom_point(data=data8, aes(x = LON_DD83, y = LAT_DD83, shape=Type, colour=("Region 1") )) + 
  geom_point(data=data11, aes(x = LON_DD83, y = LAT_DD83, shape=Type, colour=("Region 2") ))
#Type split map 
p+ geom_point(data=data16, aes(x = LON_DD83, y = LAT_DD83, colour=("Lake/Stream") )) + 
  geom_point(data=data17, aes(x = LON_DD83, y = LAT_DD83, colour=("Wetland") )) +
  scale_colour_manual(values = c("darkorange2", 'plum3'), name = "Ecosystem Type")

#scale_shape_manual(values = c(3, 17), name = "Ecosystem Type")

anova(lm(data$logTP~data$class))
out<-LSD.test(data$logTP,data$class, 3638, 1.157, alpha=0.05)  #specify the DF and MSE of the residuals
boxplot(logTP~newclass, data=data, xlab="class", ylab="logTP", col=brewer.pal(9,'Paired'))
text(1, 2.8, labels='a', col='black')
text(2, 2.6, labels='b', col='black')
text(3, 3.7, labels='c', col='black')
text(4, 3.15, labels='a', col='black')
text(5, 4.1, labels='d', col='black')
text(6, 4, labels='d', col='black')
text(7, 4.1, labels='d', col='black')
text(8, 4.75, labels='e', col='black')
text(9, 5.65, labels='f', col='black')

library(gridExtra)
grid.arrange(p1, p2, nrow=1, ncol=2)
?grid.arrange
dev.off()
######## TN ######### 
hist(allecos$TN)
allecos$logTN <- log(1+allecos$TN)
hist(allecos$logTN) #normal 
TNall<-rpart(logTN ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + BASIN_AREA + POPDEN + ROADDEN + NDEP + 
               TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
               WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83,
             method="anova", data=allecos)
printcp(TNall)
post(TNall, title = "Drivers of TN in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

plotcp(TNall)
printcp(TNall)
TNall1<-prune(TNall, cp= 0.013142)
post(TNall1, title = "Drivers of TN in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

pfit <- as.party(TNall1)  #change the format to fit this package
plot(pfit)
data2 <- data_party(pfit, 2) 
data9 <- data_party(pfit, 9)
TNdata<-data_party(pfit)
TNdata$class<-TNdata$'(fitted)'
TNdata$class<-as.factor(TNdata$class)
TNdata$newclass<-recode(TNdata$class,"4=1;5=2;7=3;8=4;11=5;12=6;14=7;15=8")
p+ geom_point(data=TNdata, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_colour_brewer(palette = 'Paired',  name = "Class") +
  scale_shape_manual(values=c(1, 16, 1, 16, 16, 1, 1, 16), name = "Class")

#forest map 
forestlow<-dplyr::filter(allecos, FOREST_PCT <20.6)
forest20<-dplyr::filter(allecos, FOREST_PCT >= 20.6 & FOREST_PCT < 48.4)
forest48<-dplyr::filter(allecos, FOREST_PCT >= 48.4 & FOREST_PCT < 80)
forest80<-dplyr::filter(allecos, FOREST_PCT >= 80)
p+ geom_point(data=forestlow, aes(x = LON_DD83, y = LAT_DD83, shape=("0-20.6"))) + 
  geom_point(data=forest20, aes(x = LON_DD83, y = LAT_DD83, shape=("20.6-48.4"))) +
  geom_point(data=forest48, aes(x = LON_DD83, y = LAT_DD83, shape=("48.4-80"))) +
  geom_point(data=forest80, aes(x = LON_DD83, y = LAT_DD83, shape=("80-100"))) +
  scale_shape_manual(values=c(1, 16, 17, 4), name = "% Forest")

#ANOVA
anova(lm(TNdata$logTN~TNdata$class))
out<-LSD.test(TNdata$logTN,TNdata$class, 3629, 0.8, alpha=0.05)  #specify the DF and MSE of the residuals
boxplot(logTN~newclass, data=TNdata, xlab="class", ylab="logTN", col=brewer.pal(8,'Paired'))
text(1, 4.25, labels='a', col='black')
text(2, 5.25, labels='b', col='black')
text(3, 5.3, labels='b', col='black')
text(4, 5.85, labels='c', col='black')
text(5, 4.5, labels='d', col='black')
text(6, 5.85, labels='c', col='black')
text(7, 6.5, labels='e', col='black')
text(8, 7, labels='f', col='black')

############ CHLA #####################
hist(allecos$CHLA) 
allecos$logCHLA <- log(1+allecos$CHLA)
hist(allecos$logCHLA) # sort of normal 
CHLAall<-rpart(logCHLA ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                 TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                 WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83,
               method="anova", data=allecos)
printcp(CHLAall)
post(CHLAall, title = "Drivers of Chla in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

# prune Chla
plotcp(CHLAall)
printcp(CHLAall)
CHLA1<-prune(CHLAall, cp=0.012924)
post(CHLA1, title = "Drivers of Chla in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

chlafit <- as.party(CHLA1)  #change the format to fit this package
plot(chlafit)
data2 <- data_party(chlafit, 2) #select data from the 10th node
data7 <- data_party(chlafit, 7)
data3 <- data_party(chlafit, 3)
data6 <- data_party(chlafit, 6)
data10 <- data_party(chlafit, 10)
data13 <- data_party(chlafit, 13)
chladata<-data_party(chlafit)
chladata$class<-chladata$'(fitted)'
chladata$class<-as.factor(chladata$class)
chladata$newclass<-recode(chladata$class,"4=1;5=2;6=3;8=4;11=5;12=6;14=7;15=8")
p+ geom_point(data=chladata, aes(x = LON_DD83, y = LAT_DD83, colour=newclass)) + 
  scale_colour_brewer(palette = 'Paired') 

#subset maps 
chlaL<-filter(chladata, newclass== '1' | newclass== '2' | newclass== '3')
p+ geom_point(data=chlaL, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 1, 1),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class")

chlaR<-filter(chladata, newclass== '4' | newclass== '5' | newclass== '6' | newclass== '7' | newclass== '8' )
p+ geom_point(data=chlaR, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 16, 1, 1, 16),  name = "Class")+
  scale_colour_manual(values=c('green4', 'lightcoral', "red2", 'sandybrown', 'orange'), name = "Class")

#forest map
p+ geom_point(data=data2, aes(x = LON_DD83, y = LAT_DD83, colour=(">=35% forest"), shape=Type )) + 
  geom_point(data=data7, aes(x = LON_DD83, y = LAT_DD83, colour=("<35% forest"), shape=Type )) 
#type split
p+ geom_point(data=data3, aes(x = LON_DD83, y = LAT_DD83, colour=("stream/lake") )) + 
  geom_point(data=data6, aes(x = LON_DD83, y = LAT_DD83, colour=("wetland") )) 
#type split
p+ geom_point(data=data2, aes(x = LON_DD83, y = LAT_DD83, colour=("Stream/Wetland")  )) + 
  geom_point(data=data7, aes(x = LON_DD83, y = LAT_DD83, colour=("Lake"))) 
#final boxplot 
boxplot(logCHLA~newclass,
        data=chladata,
        xlab="class", 
        ylab="logChla", col=brewer.pal(8,'Paired'))
text(1, .5, labels='a', col='black')
text(2, 1, labels='b', col='black')
text(3, 1.35, labels='c', col='black')
text(4, 1.15, labels='d', col='black')
text(5, 1.25, labels='c,d', col='black')
text(6, 2.1, labels='e', col='black')
text(7, 2.15, labels='e', col='black')
text(8, 3.1, labels='f', col='black')
#ANOVA test
library(agricolae)
anova(lm(chladata$logCHLA~chladata$class))
out<-LSD.test(chladata$logCHLA,chladata$class, 3431, 1.166, alpha=0.05)  #specify the DF and MSE of the residuals
#class 14 and 12 are not sig diff, and 6,11 and 11, 8

#install.packages('ggpubr')
#library(ggpubr)

################# AQM  ##############
hist(allecos$AQM)
allecos$logAQM <- log(1+allecos$AQM)
hist(allecos$logAQM) #sort of normal 
AQMall<-rpart(logAQM ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83,
              method="anova", data=allecos)

#no pruning needed 
printcp(AQMall)
plotcp(AQMall)
post(AQMall, title = "Drivers of Aquatic Vegetation in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

AQMfit <- as.party(AQMall)  #change the format to fit this package
plot(AQMfit)
AQMdata <- data_party(AQMfit) 
AQMdata$class<-AQMdata$'(fitted)'
AQMdata$class<-as.factor(AQMdata$class)
AQMdata$newclass<-recode(AQMdata$class,"4=1;5=2;6=3;9=4;11=5;12=6;13=7")
data3 <- data_party(AQMfit, 3) 
data6 <- data_party(AQMfit, 6) 
data8 <- data_party(AQMfit, 8) 
data13 <- data_party(AQMfit, 13) 
data2 <- data_party(AQMfit, 2) 
data7 <- data_party(AQMfit, 7) 

class7<-filter(AQMdata,newclass=="7")

#subset of terminal classes 
aqmL<-filter(AQMdata, newclass== '1' | newclass== '2' | newclass== '3')
p+ geom_point(data=aqmL, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 1, 1),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class")

aqmR<-filter(AQMdata, newclass== '4' | newclass== '5' | newclass== '6' | newclass== '7' )
p+ geom_point(data=aqmR, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 16, 1, 1),  name = "Class")+
  scale_colour_manual(values=c('green4', 'lightcoral', "red2", 'sandybrown', 'orange'), name = "Class")


#TMIN in streams 
p+ geom_point(data=data2, aes(x = LON_DD83, y = LAT_DD83, shape=("Stream"))) + 
  geom_point(data=data7, aes(x = LON_DD83, y = LAT_DD83, shape=("Lake/Wetland") ) ) +
  scale_shape_manual(values=c(16,3), name="Ecosystem Type")+ 
  scale_color_manual(values=c('black', "black"))

#Ecoregions
#geom_point(data=data8, aes(x = LON_DD83, y = LAT_DD83, colour=("R1"), shape=Type)) + 
p+ geom_point(data=data13, aes(x = LON_DD83, y = LAT_DD83, colour=("R2")))

#final classes
p+ geom_point(data=AQMdata, aes(x = LON_DD83, y = LAT_DD83, colour=class, shape=Type )) + 
  scale_colour_brewer(palette = 'Paired') + 
  scale_shape_manual(values = c(3, 17, 16))


#ANOVA test
anova(lm(AQMdata$logAQM~AQMdata$class))
out<-LSD.test(AQMdata$logAQM,AQMdata$class, 3623, 1.76, alpha=0.05)
boxplot(logAQM~newclass,
        data=AQMdata,
        xlab="class", 
        ylab="logAQM", col=brewer.pal(7,'Paired'))
text(1, .25, labels='a', col='black')
text(2, 1.35, labels='b', col='black')
text(3, 1.55, labels='b', col='black')
text(4, 1.35, labels='b', col='black')
text(5, 2.25, labels='c', col='black')
text(6, 3.15, labels='d', col='black')
text(7, 3.4, labels='d', col='black')


################################################
#################### MMI ############# 
################################################
hist(allecos$MMI) # normal, don't transform 
MMIall<-rpart(MMI ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + BASIN_AREA + POPDEN + ROADDEN + NDEP + 
                TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83,
              method="anova", data=allecos)
printcp(MMIall)
post(MMIall, title = "Drivers of MMI in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

plotcp(MMIall)
printcp(MMIall)
MMIall1<-prune(MMIall, cp=0.010629)
post(MMIall1, title = "Drivers of MMI in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

MMIfit <- as.party(MMIall1)  #change the format to fit this package
plot(MMIfit)
MMIdata <- data_party(MMIfit) 
MMIdata$class<-MMIdata$'(fitted)'
MMIdata$class<-as.factor(MMIdata$class)
MMIdata$newclass<-car::recode(MMIdata$class,"5=1;7=2;8=3;9=4;11=5;12=6;15=7; 16=8 ; 17=9")

data2 <- data_party(MMIfit, 2) 
data13 <- data_party(MMIfit, 13) 

#subset of terminal classes 
mmiL<-filter(MMIdata, newclass== '1' | newclass== '2' | newclass== '3' | newclass== '4' | newclass== '5' | newclass== '6' )
p+ geom_point(data=mmiL, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 1, 1, 16, 16, 1),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class")

mmiR<-filter(MMIdata, newclass== '7' | newclass== '8' | newclass== '9')
p+ geom_point(data=mmiR, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 1, 16),  name = "Class")+
  scale_colour_manual(values=c('burlywood1', 'darkorange', "plum3"), name = "Class")



#% AG
p+ geom_point(data=data2, aes(x = LON_DD83, y = LAT_DD83, colour=(">=0.076% Ag"), shape=Type)) + 
  geom_point(data=data13, aes(x = LON_DD83, y = LAT_DD83, colour=("<0.076% Ag"), shape=Type ) )

#final classes
p+ geom_point(data=MMIdata, aes(x = LON_DD83, y = LAT_DD83, colour=class, shape=Type )) + 
  scale_colour_brewer(palette = 'Paired') + 
  scale_shape_manual(values = c(3, 17, 16))

#ANOVA test
anova(lm(MMIdata$MMI~MMIdata$class))
out<-LSD.test(MMIdata$MMI,MMIdata$class, 3397, 296, alpha=0.05)
boxplot(MMI~newclass,
        data=MMIdata,
        xlab="class", 
        ylab="MMI", col=brewer.pal(9,'Paired'))
text(1, 19, labels='a', col='black')
text(2, 19, labels='a', col='black')
text(3, 30, labels='b', col='black')
text(4, 35, labels='c', col='black')
text(5, 35, labels='c', col='black')
text(6, 55, labels='d', col='black')
text(7, 35, labels='c', col='black')
text(8, 50, labels='e', col='black')
text(9, 55, labels='d', col='black')


########## Getting Basin information for mapping ################

# Read SHAPEFILE.shp from the current working directory 
library(rgdal)
library(sp)
lakes_shape<- readOGR(dsn = "/Users/katelynking/Desktop/nla2012_lake_basins", layer = "Lake_Basins")
raster::crs(lakes_shape)
lakes_shape@data
plot(lakes_shape)
lulcdata<-read.csv("/Users/katelynking/Desktop/MSU Research/nla2012 raw data/NLA2012_wide_landscape.csv")
joindata<- merge(lakes_shape, lulcdata, by.x='NLA12_ID', by.y='SITE_ID')  
#install.packages('tmap')
library(tmap)
tm_shape(joindata)+
  tm_fill('NLCD2006_FORESTPCT_BSN', style='fixed', title='% Forest in Watersheds',
          breaks=c(0,29.3,34.8,48.4,100)) 

stream_shape<- readOGR(dsn = "/Users/katelynking/Desktop/nrsa0809_watersheds", layer = "NRSA0809_Watersheds")
plot(stream_shape)
stream_shape@data
streamland<-read.csv("/Users/katelynking/Desktop/MSU Research/Streams 2008 Raw Data/land.csv")
streamland1<-streamland[!duplicated(paste(streamland$SITE_ID)),]
streamjoin<- merge(stream_shape, streamland1, by.x='SITE_ID', by.y='SITE_ID')  
tm_shape(streamjoin)+
  tm_fill('PCT_FOR', style='fixed', title='% Forest in Watersheds',
          breaks=c(0,29.3,34.8,48.4,100)) 

################################################################################################
################################ Individual CARTS for each Ecosystem Type ################################
####################################################################################################

library(dplyr)
lake<-filter(allecos, Type == 'Lake' )
stream<-filter(allecos, Type == 'Stream' )
wetland<-filter(allecos, Type == 'Wetland' )

############ LAKES #############
# MMI 
MMI.L<-rpart(MMI ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + BASIN_AREA + POPDEN + ROADDEN + NDEP + 
               TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
               WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
             method="anova", data=lake ) #control = rpart.control(cp = 0.05) <-cp stands for complexity parameter, which tells it the split must improve the R2 by .05 in order to condisder it 
post(MMI.L, title = "Drivers of MMI in Lakes",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

plotcp(MMI.L)
MMI.L1<-prune(MMI.L, cp=.024)
plotcp(MMI.L1)
post(MMI.L1, title = "Drivers of MMI in Lakes",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

#this can be useful to look at R2 of each split 
tmp <- printcp(MMI.L1)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val[nrow(rsq.val),]  #final R2 value; x-error is the cross-validation error

#AQM
lake$logAQM <- log(1+lake$AQM)
AQM.L<-rpart(logAQM ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
               TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
               WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
             method="anova", data=lake)
post(AQM.L, title = "Drivers of Aquatic Vegetation in Lakes",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)
plotcp(AQM.L)
AQM.L1<-prune(AQM.L, cp=.023)
plotcp(AQM.L1)
post(AQM.L1, title = "Drivers of AQM in Lakes",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

tmp <- printcp(AQM.L1)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val[nrow(rsq.val),] #0.24

#CHLA
lake$logCHLA <- log(1+lake$CHLA)
CHLA.L<-rpart(logCHLA ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
              method="anova", data=lake)
post(CHLA.L, title = "Drivers of Chla in Lakes",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

plotcp(CHLA.L)
CHLA.L1<-prune(CHLA.L, cp=.023)
plotcp(CHLA.L1)
post(CHLA.L1, title = "Drivers of CHLA in Lakes",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

#TP
lake$logTP <- log(1+lake$TP)
TP.L<-rpart(logTP ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + BASIN_AREA + POPDEN + ROADDEN + NDEP + 
              TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
              WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
            method="anova", data=lake)
post(TP.L, title = "Drivers of TP in LAkes",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

plotcp(TP.L)
TP.L1<-prune(TP.L, cp=.014)
plotcp(TP.L1)
post(TP.L1, title = "Drivers of TP in Lakes",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

tmp <- printcp(TP.L1)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val[nrow(rsq.val),]
#TN
lake$logTN <- log(1+lake$TN)
TN.L<-rpart(logTN ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + BASIN_AREA + POPDEN + ROADDEN + NDEP + 
              TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
              WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
            method="anova", data=lake, control = rpart.control(cp = 0.05))
post(TN.L, title = "Drivers of TN in LAkes",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)
tmp <- printcp(TN.L)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val[nrow(rsq.val),]

############ STREAMS #############
# MMI 
MMI.S<-rpart(MMI ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + BASIN_AREA + POPDEN + ROADDEN + NDEP + 
               TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
               WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
             method="anova", data=stream, control = rpart.control(cp = 0.05)) #cp stands for complexity parameter, which tells it the split must improve the R2 by .05 in order to condisder it 
post(MMI.S, title = "Drivers of MMI in Streams",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

#AQM
stream$logAQM <- log(1+stream$AQM)
AQM.S<-rpart(logAQM ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
               TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
               WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
             method="anova", data=stream, control = rpart.control(cp = 0.05))
post(AQM.S, title = "Drivers of Aquatic Vegetation in Streams",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

#CHLA
stream$logCHLA <- log(1+stream$CHLA)
CHLA.S<-rpart(logCHLA ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
              method="anova", data=stream, control = rpart.control(cp = 0.05))
post(CHLA.S, title = "Drivers of Chla in Stream",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

#TP
stream$logTP <- log(1+stream$TP)
TP.S<-rpart(logTP ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + BASIN_AREA + POPDEN + ROADDEN + NDEP + 
              TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
              WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
            method="anova", data=stream, control = rpart.control(cp = 0.05))
post(TP.S, title = "Drivers of TP in streams",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

#TN
stream$logTN <- log(1+stream$TN)
TN.S<-rpart(logTN ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + BASIN_AREA + POPDEN + ROADDEN + NDEP + 
              TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
              WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
            method="anova", data=stream, control = rpart.control(cp = 0.05))
post(TN.S, title = "Drivers of TN in streams",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)
tmp <- printcp(TN.L)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val[nrow(rsq.val),]

############ WETLANDS #############
# MMI 
MMI.W<-rpart(MMI ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + BASIN_AREA + POPDEN + ROADDEN + NDEP + 
               TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
               WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
             method="anova", data=wetland, control = rpart.control(cp = 0.05)) #cp stands for complexity parameter, which tells it the split must improve the R2 by .05 in order to condisder it 
post(MMI.W, title = "Drivers of MMI in wetlands",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

#AQM
wetland$logAQM <- log(1+wetland$AQM)
AQM.W<-rpart(logAQM ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
               TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
               WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
             method="anova", data=wetland, control = rpart.control(cp = 0.05))
post(AQM.W, title = "Drivers of Aquatic Vegetation in wetlands",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

#CHLA
wetland$logCHLA <- log(1+wetland$CHLA)
CHLA.W<-rpart(logCHLA ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
              method="anova", data=wetland, control = rpart.control(cp = 0.05))
post(CHLA.W, title = "Drivers of Chla in wetland",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

#TP
wetland$logTP <- log(1+wetland$TP)
TP.W<-rpart(logTP ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + BASIN_AREA + POPDEN + ROADDEN + NDEP + 
              TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
              WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
            method="anova", data=wetland, control = rpart.control(cp = 0.05))
post(TP.W, title = "Drivers of TP in wetlands",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

#TN
wetland$logTN <- log(1+wetland$TN)
TN.W<-rpart(logTN ~ RVEG + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + BASIN_AREA + POPDEN + ROADDEN + NDEP + 
              TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
              WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015,
            method="anova", data=wetland, control = rpart.control(cp = 0.05))
post(TN.W, title = "Drivers of TN in wetlands",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)
tmp <- printcp(TN.L)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val[nrow(rsq.val),]

#back transform all of the results: opposite of natural log in the "e"= 2.718281
2.718281^3.442