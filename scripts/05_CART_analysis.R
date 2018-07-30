### Written by Katelyn King 
#### Start Date: March 


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
fitall1<-prune(fitall, cp=0.012552)  #use the CP value from the row where pruning should occur
post(fitall1, title = "Drivers of TP in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE)  #horizontal = TRUE
summary(fitall1) #investigate competitor splits
## use partykit package to make maps and graphs  
pfit <- as.party(fitall1)  #change the format to fit this package
plot(pfit)
TPdata<-data_party(pfit)
TPdata$class<-TPdata$'(fitted)'
TPdata$class<-as.factor(TPdata$class)
TPdata$newclass<-recode(TPdata$class,"3=1;5=2;6=3;9=4;10=5;13=6;14=7;16=8;17=9")

#get the US background map 
devtools::install_github("dkahle/ggmap")
usa<-map_data("usa")  #pull out the usa map
p<-ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 

#terminal map a
datasub<-filter(TPdata, newclass== '1' | newclass== '2' | newclass== '3'| newclass== '4' | newclass== '5' )
p+ geom_point(data=datasub, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(1, 16, 1, 16, 16),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class")

#Terminal map b
datasub2<-filter(TPdata, newclass== '6' | newclass== '7' | newclass== '8'| newclass== '9' )
p+ geom_point(data=datasub2, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 1, 16, 16),  name = "Class")+
  scale_colour_manual(values = c("red2", 'sandybrown', "darkorange2", 'plum3'), name = "Class")

p+ geom_point(data=data16, aes(x = LON_DD83, y = LAT_DD83, colour=("Lake/Stream") )) + 
  geom_point(data=data17, aes(x = LON_DD83, y = LAT_DD83, colour=("Wetland") )) +
  scale_colour_manual(values = c("darkorange2", 'plum3'), name = "Ecosystem Type")

anova(lm(TPdata$logTP~TPdata$class))
out<-LSD.test(TPdata$logTP,TPdata$newclass, 3637, 1.153, alpha=0.05)  #specify the DF and MSE of the residuals
boxplot(logTP~newclass, data=TPdata, xlab="class", ylab="logTP", col=brewer.pal(9,'Paired'))
text(1, 2.8, labels='a', col='black') 
text(2, 2.6, labels='b', col='black') 
text(3, 3.7, labels='c', col='black') 
text(4, 3.15, labels='d', col='black') 
text(5, 4.1, labels='e', col='black') 
text(6, 4, labels='e', col='black') 
text(7, 4.75, labels='f', col='black') 
text(8, 4, labels='e', col='black') 
text(9, 5.65, labels='g', col='black') 

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
TNall1<-prune(TNall, cp= 0.015711)
post(TNall1, title = "Drivers of TN in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)
summary(TNall1)
pfitTN <- as.party(TNall1)  #change the format to fit this package
plot(pfitTN)
TNdata<-data_party(pfit)
TNdata$class<-TNdata$'(fitted)'
TNdata$class<-as.factor(TNdata$class)
TNdata$newclass<-recode(TNdata$class,"4=1;5=2;7=3;8=4;11=5;12=6;14=7;15=8")

#terminal map a
TNsub<-filter(TNdata, newclass== '1' | newclass== '2' | newclass== '3'| newclass== '4' )
p+ geom_point(data=TNsub, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(1, 16, 1, 16),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class")

#map b
TNsub2<-filter(TNdata, newclass== '5' | newclass== '6' | newclass== '7'| newclass== '8' )
p+ geom_point(data=TNsub2, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(1, 16, 1, 16),  name = "Class")+
  scale_colour_manual(values = c('lightcoral', "red2", 'sandybrown', "darkorange2"), name = "Class")

#ANOVA
anova(lm(TNdata$logTN~TNdata$class))
out<-LSD.test(TNdata$logTN,TNdata$newclass, 3628, 0.8, alpha=0.05)  #specify the DF and MSE of the residuals
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

# prune Chla
plotcp(CHLAall)
printcp(CHLAall)
CHLA1<-prune(CHLAall, cp=0.014334)
post(CHLA1, title = "Drivers of Chla in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)
summary(CHLA1)
chlafit <- as.party(CHLA1)  #change the format to fit this package
plot(chlafit)
chladata<-data_party(chlafit)
chladata$class<-chladata$'(fitted)'
chladata$class<-as.factor(chladata$class)
chladata$newclass<-recode(chladata$class,"4=1;5=2;7=3;8=4;10=5;12=6;14=7;15=8")

#terminal map a
chlsub<-filter(chladata, newclass== '1' | newclass== '2' | newclass== '3'| newclass== '4' )
p+ geom_point(data=chlsub, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(1, 16, 1, 16),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class")

chlsub2<-filter(chladata, newclass== '5' | newclass== '6' | newclass== '7' | newclass== '8' )
p+ geom_point(data=chlsub2, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 16, 1, 16),  name = "Class")+
  scale_colour_manual(values=c('lightcoral', "red2", 'sandybrown', 'orange'), name = "Class")

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
summary(AQMall)
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

#ANOVA test
anova(lm(AQMdata$logaqveg~AQMdata$class))
out<-LSD.test(AQMdata$logaqveg,AQMdata$newclass, 3621, 1.76, alpha=0.05)
boxplot(logaqveg~newclass,
        data=AQMdata,
        xlab="class", 
        ylab="logaqveg", col=brewer.pal(7,'Paired'))
text(1, .25, labels='a', col='black')
text(2, 1.35, labels='b', col='black')
text(3, 1.55, labels='b', col='black')
text(4, 1.35, labels='b', col='black')
text(5, 2.25, labels='c', col='black')
text(6, 3.15, labels='d', col='black')
text(7, 3.4, labels='d', col='black')

#################### MMI ############# 
hist(allecos$MMI) # normal, don't transform 
MMIall<-rpart(MMI ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + Type + WS_AREA + DEPTH + POPDEN + ROADDEN + NDEP + 
                TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83,
              method="anova", data=allecos)
plotcp(MMIall)
printcp(MMIall)
MMIall1<-prune(MMIall, cp=0.014358)
post(MMIall1, title = "Drivers of MMI in Freshwater Ecosystems",
     filename = '', 
     digits = 4,
     use.n = TRUE, horizontal = TRUE)

summary(MMIall1)
MMIfit <- as.party(MMIall1)  #change the format to fit this package
plot(MMIfit)
MMIdata <- data_party(MMIfit) 
MMIdata$class<-MMIdata$'(fitted)'
MMIdata$class<-as.factor(MMIdata$class)
MMIdata$newclass<-car::recode(MMIdata$class,"3=1;5=2;6=3;8=4;9=5")

#subset of terminal classes 
mmiL<-filter(MMIdata, newclass== '1' | newclass== '2' | newclass== '3')
p+ geom_point(data=mmiL, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 1, 1),  name = "Class")+
  scale_colour_brewer(palette = 'Paired', name = "Class")

mmiR<-filter(MMIdata, newclass== '4' | newclass== '5' )
p+ geom_point(data=mmiR, aes(x = LON_DD83, y = LAT_DD83, colour=newclass, shape=newclass)) + 
  scale_shape_manual(values=c(16, 1),  name = "Class")+
  scale_colour_manual(values=c('green4', 'lightcoral'), name = "Class")

#ANOVA test
anova(lm(MMIdata$MMI~MMIdata$class))
out<-LSD.test(MMIdata$MMI,MMIdata$newclass, 33999, 323, alpha=0.05)
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
#this can be useful to look at R2 of each split 
tmp <- printcp(MMIall1)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val[nrow(rsq.val),]  #final R2 value; x-error is the cross-validation error

#back transform all of the results: opposite of natural log in the "e"= 2.718281
2.718281^3.442

log(1+5)   #1.791759
(2.718281^(1.791759))-1

back_transform<-function (x) {(2.718281^(x))-1}
back_transform(0.8627)
back_transform(1.63)
back_transform(1.655)
back_transform(1.799)
back_transform(2.148)
back_transform(2.971)
back_transform(3.106)

