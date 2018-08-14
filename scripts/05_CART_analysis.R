### Written by Katelyn King 
#### Start Date: March 2018


### load libraries 
library(rpart)
library(dplyr)
library(gtools)
library(partykit)
library(car)
library(ggplot2)
library(devtools)
library(mapdata)
library(ggmap)
library(RColorBrewer)
library(agricolae)
library(ggsn)

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

# Prune based on 1SE rule  (xerror + xstd of the minimum and then pruning at the row with xerror just below that)
plotcp(fitall, main="TP pruning") #chose the CP value that is furthest left under the line (1SE above the min error)
printcp(fitall) #shows a table of CP values and xerror/xstd
SE<-min(fitall$cptable[,4]) + min(fitall$cptable[,5])  #(xerror + xstd of the minimum)
SE2<-min(fitall$cptable[,4]) + min(fitall$cptable[,5])
xer<-fitall$cptable[,4] > SE
cp<-fitall$cptable[7,"CP"]
fitall1<-prune(fitall, cp=cp)  #use the CP value from the row where pruning should occur
saveRDS(fitall1, "Output/TPtree.rds")
post(fitall1, title = "Drivers of TP in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE)  #horizontal = TRUE
summary(fitall1) #investigate competitor splits
#get Rsq value for the model 
tcp<-printcp(fitall1)
rsq.val <- 1-tcp[,c(3,4)]  
rsq.val[nrow(rsq.val),1]  # R2 = 0.30 

## use partykit package to make maps and graphs  
pfit <- as.party(fitall1)  #change the format to fit this package
plot(pfit)
TPdata<-data_party(pfit)
TPdata$class<-TPdata$'(fitted)'
TPdata$class<-as.factor(TPdata$class)
TPdata$newclass<-recode(TPdata$class,"3=1;4=2;7=3;8=4;10=5;12=6;13=7")

#get the US background map 
devtools::install_github("dkahle/ggmap")
usa<-map_data("usa")  #pull out the usa map
p<-ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 

#terminal map a
datasub<-filter(TPdata, newclass== '1' | newclass== '2' | newclass== '3'| newclass== '4')
tp1<-p+ geom_point(data=datasub, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(1, 16, 1, 16),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class") +
  north(data = usa, symbol=3, scale=.1, location = "bottomright") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.9, 0.4), legend.text = element_text(size=10) )
                                                                  

#Terminal map b
datasub2<-filter(TPdata, newclass== '5' | newclass== '6' | newclass== '7' )
tp2<-p+ geom_point(data=datasub2, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(1, 16, 16),  name = "Class")+
  scale_colour_manual(values = c('lightcoral', "red2", 'sandybrown'), name = "Class") +
  north(data = usa, symbol=3, scale=.1, location = "bottomright") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.9, 0.4), legend.text = element_text(size=10) )


anova(lm(TPdata$logTP~TPdata$class))
out<-LSD.test(TPdata$logTP,TPdata$newclass, 3639, 1.198, alpha=0.05)  #specify the DF and MSE of the residuals
boxplot(logTP~newclass, data=TPdata, xlab="class", ylab="logTP", col=brewer.pal(9,'Paired'))
text(1, 2.8, labels='a', col='black') 
text(2, 3.5, labels='b', col='black') 
text(3, 3.1, labels='c', col='black') 
text(4, 4, labels='d', col='black') 
text(5, 4.5, labels='e', col='black') 
text(6, 4, labels='d,e', col='black') 
text(7, 5.5, labels='f', col='black') 

######## TN ######### 
hist(allecos$TN)
allecos$logTN <- log(1+allecos$TN)
hist(allecos$logTN) #normal 
TNall<-rpart(logTN ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + WS_AREA + POPDEN + ROADDEN + NDEP + 
               TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
               WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83,
             method="anova", data=allecos)

plotcp(TNall)
printcp(TNall)
#prune the tree
SE<-min(TNall$cptable[,4]) + TNall$cptable[which.min(TNall$cptable[,4]),"xstd"] #(xerror + xstd of the minimum)
TNall$cptable[,4] > SE
cp<-TNall$cptable[8,"CP"]
TNall1<-prune(TNall, cp= cp)
saveRDS(TNall1, "Output/TNtree.rds")
post(TNall1, title = "Drivers of TN in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

summary(TNall1) #investigate competitor splits

#get Rsq value for the model 
tcp<-printcp(TNall1)
rsq.val <- 1-tcp[,c(3,4)]  
rsq.val[nrow(rsq.val),1]  # R2for TN  = 0.46

#Plotting and mapping
pfitTN <- as.party(TNall1)  #change the format to fit this package
plot(pfitTN)
TNdata<-data_party(pfitTN)
TNdata$class<-TNdata$'(fitted)'
TNdata$class<-as.factor(TNdata$class)
TNdata$newclass<-recode(TNdata$class,"4=1;5=2;7=3;8=4;11=5;12=6;14=7;15=8")

#terminal map a
TNsub<-filter(TNdata, newclass== '1' | newclass== '2' | newclass== '3'| newclass== '4' )
p+ geom_point(data=TNsub, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 1, 16, 1),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class") +
  north(data = usa, symbol=3, scale=.1, location = "bottomright") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.9, 0.4), legend.text = element_text(size=10) )

#map b
TNsub2<-filter(TNdata, newclass== '5' | newclass== '6' | newclass== '7'| newclass== '8' )
p+ geom_point(data=TNsub2, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(1, 16, 1, 16),  name = "Class")+
  scale_colour_manual(values = c('lightcoral', "red2", 'sandybrown', "darkorange2"), name = "Class") +
  north(data = usa, symbol=3, scale=.1, location = "bottomright") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.9, 0.4), legend.text = element_text(size=10) )

#ANOVA
anova(lm(TNdata$logTN~TNdata$class))
out<-LSD.test(TNdata$logTN,TNdata$newclass, 3628, 0.78, alpha=0.05)  #specify the DF and MSE of the residuals
boxplot(logTN~newclass, data=TNdata, xlab="class", ylab="logTN", col=brewer.pal(8,'Paired'))
text(1, 3.9, labels='a', col='black')
text(2, 4.95, labels='b', col='black')
text(3, 5.3, labels='c', col='black')
text(4, 5.85, labels='d', col='black')
text(5, 4.5, labels='e', col='black')
text(6, 5.85, labels='d', col='black')
text(7, 6.5, labels='f', col='black')
text(8, 7, labels='g', col='black')

############ CHLA #####################
hist(allecos$CHLA) 
allecos$logCHLA <- log(1+allecos$CHLA)
hist(allecos$logCHLA) # sort of normal 
CHLAall<-rpart(logCHLA ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + WS_AREA + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                 TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                 WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83,
               method="anova", data=allecos)

# prune tree
plotcp(CHLAall)
printcp(CHLAall)
SE<-min(CHLAall$cptable[,4]) + CHLAall$cptable[which.min(CHLAall$cptable[,4]),"xstd"] #(xerror + xstd of the minimum)
CHLAall$cptable[,4] > SE
CHLAall$cptable[7,"CP"]
CHLA1<-prune(CHLAall, cp=0.014334)
post(CHLA1, title = "Drivers of Chla in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)
saveRDS(CHLA1, "Output/CHLAtree.rds")

summary(CHLA1) #investigate splits

#get Rsq value for the model 
tcp<-printcp(CHLA1)
rsq.val <- 1-tcp[,c(3,4)]  
rsq.val[nrow(rsq.val),1]  # R2 for CHLA  = 0.30

#plotting and mapping 
chlafit <- as.party(CHLA1)  #change the format to fit this package
plot(chlafit)
chladata<-data_party(chlafit)
chladata$class<-chladata$'(fitted)'
chladata$class<-as.factor(chladata$class)
chladata$newclass<-recode(chladata$class,"4=1;5=2;7=3;8=4;10=5;12=6;14=7;15=8")

#terminal map a
chlsub<-filter(chladata, newclass== '1' | newclass== '2' | newclass== '3'| newclass== '4' )
p+ geom_point(data=chlsub, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 1, 16, 16),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class") +
  north(data = usa, symbol=3, scale=.1, location = "bottomright") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.9, 0.4), legend.text = element_text(size=10) )

chlsub2<-filter(chladata, newclass== '5' | newclass== '6' | newclass== '7' | newclass== '8' )
p+ geom_point(data=chlsub2, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 16, 1, 16),  name = "Class")+
  scale_colour_manual(values=c('lightcoral', "red2", 'sandybrown', 'orange'), name = "Class") +
  north(data = usa, symbol=3, scale=.1, location = "bottomright") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.9, 0.4), legend.text = element_text(size=10) )

#ANOVA test
anova(lm(chladata$logCHLA~chladata$class))
out<-LSD.test(chladata$logCHLA,chladata$newclass, 3429, 1.167, alpha=0.05)  #specify the DF and MSE of the residuals
boxplot(logCHLA~newclass,
        data=chladata,
        xlab="class", 
        ylab="logChla", col=brewer.pal(8,'Paired'))
text(1, .5, labels='a', col='black')
text(2, 1, labels='b', col='black')
text(3, 1.25, labels='c', col='black')
text(4, 2, labels='d,e', col='black')
text(5, 1.25, labels='c', col='black')
text(6, 1.9, labels='d', col='black')
text(7, 2.15, labels='e', col='black')
text(8, 3.1, labels='f', col='black')

################# AQM  ##############
hist(allecos$aqveg)
allecos$logaqveg <- log(1+allecos$aqveg)
hist(allecos$logaqveg) #sort of normal 
AQMall<-rpart(logaqveg ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + WS_AREA + POPDEN + ROADDEN + NDEP + 
                TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83,
              method="anova", data=allecos)

#no pruning needed 

printcp(AQMall)
plotcp(AQMall)

SE<-min(AQMall$cptable[,4]) + AQMall$cptable[which.min(AQMall$cptable[,4]),"xstd"] #(xerror + xstd of the minimum)
AQMall$cptable[,4] > SE
AQMall$cptable[5,"CP"]
AQM1<-prune(AQMall, cp=0.01273124)
post(AQM1, title = "Drivers of Aquatic Vegetation in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)
saveRDS(AQM1, "Output/AQMtree.rds")

summary(AQM1) #investigate splits 

#get Rsq value for the model 
tcp<-printcp(AQM1)
rsq.val <- 1-tcp[,c(3,4)]  
rsq.val[nrow(rsq.val),1]  # R2 for AQM  = 0.25

#plotting and mapping
AQMfit <- as.party(AQM1)  #change the format to fit this package
plot(AQMfit)
AQMdata <- data_party(AQMfit) 
AQMdata$class<-AQMdata$'(fitted)'
AQMdata$class<-as.factor(AQMdata$class)
AQMdata$newclass<-recode(AQMdata$class,"4=1;5=2;6=3;8=4;9=5")

class7<-filter(AQMdata,newclass=="7")

#subset of terminal classes 
aqm1<-filter(AQMdata, newclass== '1' | newclass== '2' | newclass== '3')
p+ geom_point(data=aqm1, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 16, 1),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class") +
  north(data = usa, symbol=3, scale=.1, location = "bottomright") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.9, 0.4), legend.text = element_text(size=10) )

aqm2<-filter(AQMdata, newclass== '4' | newclass== '5' )
p+ geom_point(data=aqm2, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 16),  name = "Class")+
  scale_colour_manual(values=c('green4', 'lightcoral'), name = "Class")+
  north(data = usa, symbol=3, scale=.1, location = "bottomright") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.9, 0.4), legend.text = element_text(size=10) )

#ANOVA test
anova(lm(AQMdata$logaqveg~AQMdata$class))
out<-LSD.test(AQMdata$logaqveg,AQMdata$newclass, 3623, 1.82, alpha=0.05)
boxplot(logaqveg~newclass,
        data=AQMdata,
        xlab="class", 
        ylab="logaqveg", col=brewer.pal(7,'Paired'))
text(1, .25, labels='a', col='black')
text(2, 1.35, labels='b', col='black')
text(3, 1.55, labels='b', col='black')
text(4, 2.35, labels='c', col='black')
text(5, 3.25, labels='d', col='black')


#################### MMI ############# 
hist(allecos$MMI) # normal, don't transform 
MMIall<-rpart(MMI ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + Type + WS_AREA + DEPTH + POPDEN + ROADDEN + NDEP + 
                TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83,
              method="anova", data=allecos)
plotcp(MMIall)
printcp(MMIall)
SE<-min(MMIall$cptable[,4]) + MMIall$cptable[which.min(MMIall$cptable[,4]),"xstd"] #(xerror + xstd of the minimum)
MMIall$cptable[,4] > SE
MMIall$cptable[5,"CP"]
MMIall1<-prune(MMIall, cp=0.014358)
post(MMIall1, title = "Drivers of MMI in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)
saveRDS(MMIall1, "Output/MMItree.rds")

summary(MMIall1) #investigate splits 

#get Rsq value for the model 
tcp<-printcp(MMIall1)
rsq.val <- 1-tcp[,c(3,4)]  
rsq.val[nrow(rsq.val),1]  # R2 for MMI  = 0.13

#plotting and mapping
MMIfit <- as.party(MMIall1)  #change the format to fit this package
plot(MMIfit)
MMIdata <- data_party(MMIfit) 
MMIdata$class<-MMIdata$'(fitted)'
MMIdata$class<-as.factor(MMIdata$class)
MMIdata$newclass<-car::recode(MMIdata$class,"3=1;5=2;6=3;8=4;9=5")

#subset of terminal classes 
mmi1<-filter(MMIdata, newclass== '1' | newclass== '2' | newclass== '3')
p+ geom_point(data=mmi1, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 1, 1),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class") +
  north(data = usa, symbol=3, scale=.1, location = "bottomright") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.9, 0.4), legend.text = element_text(size=10) )

mmi2<-filter(MMIdata, newclass== '4' | newclass== '5' )
p+ geom_point(data=mmi2, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 1),  name = "Class")+
  scale_colour_manual(values=c('green4', 'lightcoral'), name = "Class")+
  north(data = usa, symbol=3, scale=.1, location = "bottomright") + 
  theme_bw() + xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position = c(0.9, 0.4), legend.text = element_text(size=10) )

#ANOVA test
anova(lm(MMIdata$MMI~MMIdata$class))
out<-LSD.test(MMIdata$MMI,MMIdata$newclass, 3399, 323, alpha=0.05)
boxplot(MMI~newclass,
        data=MMIdata,
        xlab="class", 
        ylab="MMI", col=brewer.pal(9,'Paired'))
text(1, 17, labels='a', col='black')
text(2, 23, labels='b', col='black')
text(3, 35, labels='c', col='black')
text(4, 39, labels='d', col='black')
text(5, 59, labels='e', col='black')

summary(allecos)

#back transform all of the results: opposite of natural log in the "e"= 2.718281
back_transform<-function (x) {(2.718281^(x))-1}

#### MAP Figures #### 

library(ggsn)
library(ggplot2)
library(ggmap)
?map_data
usa<-map_data("usa")  #pull out the usa map
p<-ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 

#Figure 1 A 
p+geom_point(data=stream, size = 1, colour="chartreuse4", aes(x = LON_DD83, y = LAT_DD83)) + 
  geom_point(data=lake, size = 1,  colour="#3399CC", aes(x = LON_DD83, y = LAT_DD83)) + 
  geom_point(data=wetland, size = 1, colour="#9966CC", aes(x = LON_DD83, y = LAT_DD83)) +
  north(data = usa, symbol=3, scale=.15, location = "bottomright") + 
  theme_bw()

#Fig 1 B
library(rgdal)
library(tmap)
eco9<- readOGR(dsn = "/Users/katelynking/Desktop/Aggregate_ecoregion9", layer = "Export_Output")
jpeg('eco9map.jpeg',width = 7, height = 6, units = 'in', res = 600)
tm_shape(eco9)+
  tm_fill('WSA9_NAME') + 
  tm_compass(north = 0, type = 'arrow', position =c("left", "top"))
dev.off()
## another way to add a line 
###theme(panel.grid.major = element_blank(), 
      #panel.grid.minor = element_blank(),
      #panel.background = element_rect(colour = "black", size=4, fill=NA))