#Variogram analysis 
##Start Date: Oct 24, 2017 

#load libraries 
library(sp)
library(rgdal)
library(gstat)

### Load data from data wrangling code 
lake <- read.csv("Data/NLA2012_data.csv", header=T)
stream<-read.csv("Data/NRSA0809_data.csv", header=T)
wetland<-read.csv("Data/NWCA2011_data.csv", header=T)

### ---- checking normality and log transformation ------ 
hist(lake$TP)
hist(lake$TN)
hist(lake$CHLA)
hist(lake$aqveg) 
hist(lake$MMI) #normal 

#log transform variables and add 1 if there 0s  
lake$logTP<-log(lake$TP)
lake$logTN<-log(lake$TN)
lake$logCHLA<-log(1+lake$CHLA)
lake$logaqveg<-log(1+lake$aqveg)

#### STREAMS 
#checking normality using histogram 
hist(stream$TP) 
hist(stream$TN)  
hist(stream$CHLA)
hist(stream$aqveg) 
hist(stream$MMI) #normal 

#log transform variables new columns 
stream$logTP<-log(1+stream$TP)
stream$logTN<-log(1+stream$TN)
stream$logCHLA<-log(stream$CHLA)
stream$logaqveg<-log(1+stream$aqveg)

### wetlands 

#checking normality using histogram 
hist(wetland$TP)
hist(wetland$TN)
hist(wetland$CHLA)
hist(wetland$aqveg) #not great 
hist(wetland$MMI) #normal 

#log transform variables new columns 
wetland$logTP<-log(wetland$TP)
wetland$logTN<-log(wetland$TN)
wetland$logCHLA<-log(1+wetland$CHLA)
wetland$logaqveg<-log(1+wetland$aqveg)

###------------------- semivariogram analysis ---------------- ###

#convert to spatial object
lake.ll <- SpatialPointsDataFrame(coords=lake[,c("LON_DD83","LAT_DD83")], data=lake,
                                  proj4string=CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")) 

# transform the lat&lon coordinates to USA_Contiguous_Albers_Equal_Area_Conic (aea)
prj.new<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=km +ellps=GRS80")
lake.aea <- spTransform(lake.ll, prj.new)

stream.ll<-SpatialPointsDataFrame(coords=stream[,c("LON_DD83","LAT_DD83")], data=stream,
                       proj4string=CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")) 
stream.aea <- spTransform(stream.ll, prj.new)

wetland.ll<-SpatialPointsDataFrame(coords=wetland[,c("LON_DD83","LAT_DD83")], data=wetland,
                                  proj4string=CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")) 
wetland.aea <- spTransform(wetland.ll, prj.new)

##################  Total Phosphorus ###################
#lake
lake.TP.v <- variogram(logTP~1, lake.aea, cutoff=3000, width=20)  
min(lake.TP.v$np) #ensure there are more than 50 pairs in each bin distance 
lake.TP.fit<-fit.variogram(lake.TP.v, vgm("Sph", "Exp")) #fit a model variogram
lake.TP.fit #shows parameters

#wetland
wlTPnoNAs<-wetland.aea[!(is.na(wetland.aea$logTP)),] 
wl.TP.v <- variogram(logTP~1, wlTPnoNAs, cutoff=3000, width=20)  
min(wl.TP.v$np)
wl.TP.fit<-vgm(model="Exp", nugget=.8, psill=1.9, range=605)  
wl.TP.fit

#stream
strTPnoNAs<-stream.aea[!(is.na(stream.aea$logTP)),] 
stream.TP.v <- variogram(logTP~1, strTPnoNAs, cutoff=3000, width=20)  
min(stream.TP.v$np)
stream.TP.fit <- fit.variogram(stream.TP.v, vgm("Sph", "Exp"))
stream.TP.fit

########################### Total Nitrogen #############################
#lake
lake.TN.v <- variogram(logTN~1, lake.aea, cutoff=3000, width=20)
min(lake.TN.v$np)
lake.TN.fit <- fit.variogram(lake.TN.v, vgm("Sph", "Exp"))
lake.TN.fit #shows parameters

#wetland
wlTNnoNAs<-wetland.aea[!(is.na(wetland.aea$logTN)),] 
wl.TN.v <- variogram(logTN~1, wlTNnoNAs, cutoff=3000, width=20)  
min(wl.TN.v$np)
wl.TN.fit <- fit.variogram(wl.TN.v, vgm("Sph", "Exp"))
wl.TN.fit

#stream
strTNnoNAs<-stream.aea[!(is.na(stream.aea$logTN)),] 
stream.TN.v <- variogram(logTN~1, strTNnoNAs, cutoff=3000, width=20)  
min(stream.TN.v$np)
stream.TN.fit <- fit.variogram(stream.TN.v, vgm("Sph", "Exp"))
stream.TN.fit 

########################### Chla #############################
#lake
LCHnoNAs<-lake.aea[!(is.na(lake.aea$logCHLA)),] 
lake.CHL.v <- variogram(logCHLA~1, LCHnoNAs, cutoff=3000, width=20)  
min(lake.CHL.v$np)
lake.CHL.fit<-fit.variogram(lake.CHL.v, vgm(model="Sph", "Exp") )
lake.CHL.fit #shows best fit! and gives parameters

#wetland 
wlCHnoNAs<-wetland.aea[!(is.na(wetland.aea$logCHLA)),] 
wl.CHL.v <- variogram(logCHLA~1, wlCHnoNAs, cutoff=3000, width=20)  
min(wl.CHL.v$np)      
wl.CHL.fit <- vgm(model="Sph", nugget=1.1, psill=1.2, range=101)
wl.CHL.fit

#stream
strCHnoNAs<-stream.aea[!(is.na(stream.aea$logCHLA)),] 
stream.CHL.v <- variogram(logCHLA~1, strCHnoNAs, cutoff=3000, width=20)  
min(stream.CHL.v$np)
stream.CHL.fit <- fit.variogram(stream.CHL.v, vgm("Sph", "Exp"))
stream.CHL.fit #shows best fit! and gives parameters


#################################### MMI ##################################
## LAKES
LMMnoNAs<-lake.aea[!(is.na(lake.aea$MMI)),] 
lake.MMI.v<- variogram(MMI~1, LMMnoNAs, cutoff=3000, width=20)   
min(lake.MMI.v$np)
lake.MMI.fit <- fit.variogram(lake.MMI.v, vgm("Sph", "Exp"))
lake.MMI.fit

###wetland
wlMMnoNAs<-wetland.aea[!(is.na(wetland.aea$MMI)),] 
wl.MMI.v <- variogram(MMI~1, wlMMnoNAs, cutoff=3000, width=20)  
min(wl.MMI.v$np)
wl.MMI.fit <- vgm(model="Exp", nugget = 100, psill=200, range=146)
plot(wl.MMI.v, model=wl.MMI.fit, col='black', main="MMI")

##streams##
strMMnoNAs<-stream.aea[!(is.na(stream.aea$MMI)),] 
stream.MMI.v<- variogram(MMI~1, strMMnoNAs, cutoff=3000, width=20)  
min(stream.MMI.v$np)
stream.MMI.fit<-vgm(model="Sph", nugget = 250, psill=135, range=207)
stream.MMI.fit

#################################### AQM ##################################
#lake 
LaqnoNAs<-lake.aea[!(is.na(lake.aea$logaqveg)),] 
lake.aq.v<- variogram(logaqveg~1, LaqnoNAs, cutoff=3000, width=20)
min(lake.aq.v$np)
lake.aq.fit <- fit.variogram(lake.aq.v, vgm( "Sph", "Exp"))
lake.aq.fit
plot(lake.aq.v, model=lake.aq.fit, col='black', main="logAQMLake")

###wetland 
wlaqnoNAs<-wetland.aea[!(is.na(wetland.aea$logaqveg)),] 
wl.aq.v <- variogram(logaqveg~1, wlaqnoNAs, cutoff=3000, width=20)  
min(wl.aq.v$np)
wl.aq.fit<- vgm("Sph", nugget=1.4, psill=2.1, range=1335)
wl.aq.fit

#streams##
straqnoNAs<-stream.aea[!(is.na(stream.aea$logaqveg)),] 
stream.aq.v <- variogram(logaqveg~1, straqnoNAs, cutoff=3000, width=20)  
min(stream.aq.v$np)
stream.aq.fit <- fit.variogram(stream.aq.v, vgm( "Sph", "Exp"))
stream.aq.fit
plot(stream.aq.v, model=stream.aq.fit, col='black', main="logAQM")

############################### PLOT #######################
jpeg('15panel.jpeg',width = 7, height = 9, units = 'in', res = 600)
par(mfrow=c(5,3))
par(mar=c(3,3,0,0), oma=c(3,3,1,1))

#top left panel 
plot(variogramLine(lake.TP.fit, 3000), type='l', ylim=c(0,2)) 
points(lake.TP.v[,2:3], pch=21, bg="deepskyblue", col='black')  
abline(v=lake.TP.fit[2,3], col="deepskyblue") 
abline(v=2050, col="deepskyblue") 
mtext('a', side = 3, line = -2.5, adj = .02, cex = 1.5)
#top middle 
plot(variogramLine(wl.TP.fit, 3000), type='l', ylim=c(0,3.5)) #this plots the line from the model 
points(wl.TP.v[,2:3], pch=21, bg='mediumpurple',  col='black')  #this plots the points 
abline(v=wl.TP.fit[2,3], col="mediumpurple") #this plots the R range 
abline(v=1930, col="mediumpurple") #this plots the R range 
mtext('b', side = 3, line = -2.5, adj = .02, cex = 1.5)
#top right 
plot(variogramLine(stream.TP.fit, 3000), type='l', ylim=c(0,2)) #this plots the line from the model 
points(stream.TP.v[,2:3], pch=21, bg='green3', col="black")  #this plots the points 
abline(v=stream.TP.fit[2,3], col="green3") 
abline(v=1950, col="green3") #this plots the R range 
mtext('c', side = 3, line = -2.5, adj = .02, cex = 1.5)

#left TN 
plot(variogramLine(lake.TN.fit, 3000), type='l', ylim=c(0,2)) #this plots the line from the model 
points(lake.TN.v[,2:3], pch=21, bg='deepskyblue', col="black")  
abline(v=lake.TN.fit[2,3], col="deepskyblue") 
abline(v=1750, col="deepskyblue") 
mtext('d', side = 3, line = -2.5, adj = .02, cex = 1.5)
# middle 
plot(variogramLine(wl.TN.fit, 3000), type='l', ylim=c(0,2)) 
points(wl.TN.v[,2:3], pch=21, bg='mediumpurple', col="black")  #this plots the points 
abline(v=wl.TN.fit[2,3], col="mediumpurple") #this plots the R range 
mtext('e', side = 3, line = -2.5, adj = .02, cex = 1.5)
# right 
plot(variogramLine(stream.TN.fit, 3000), type='l', ylim=c(0,3)) 
points(stream.TN.v[,2:3], pch=21, bg='green3', col="black")  #this plots the points 
abline(v=stream.TN.fit[2,3], col="green3") 
abline(v=3000, col="green3") 
mtext('f', side = 3, line = -2.5, adj = .02, cex = 1.5)

#Chla 
plot(variogramLine(lake.CHL.fit, 3000), type='l', ylim=c(0,2)) #this plots the line from the model 
points(lake.CHL.v[,2:3], pch=21, bg='deepskyblue', col="black")  
abline(v=lake.CHL.fit[2,3], col="deepskyblue") 
abline(v=1800, col="deepskyblue") 
mtext('g', side = 3, line = -2.5, adj = .02, cex = 1.5)
# middle 
plot(variogramLine(wl.CHL.fit, 3000), type='l', ylim=c(0,3.5)) 
points(wl.CHL.v[,2:3], pch=21, bg='mediumpurple', col="black")  #this plots the points 
abline(v=wl.CHL.fit[2,3], col="mediumpurple") #this plots the R range 
abline(v=1500, col="mediumpurple")
mtext('h', side = 3, line = -2.5, adj = .02, cex = 1.5)
# right 
plot(variogramLine(stream.CHL.fit, 3000), type='l', ylim=c(0,3)) 
points(stream.CHL.v[,2:3], pch=21, bg='green3', col="black")  #this plots the points 
abline(v=stream.CHL.fit[2,3], col="green3") 
abline(v=1700, col="green3") 
mtext('i', side = 3, line = -2.5, adj = .02, cex = 1.5)

#AQM 
plot(variogramLine(lake.aq.fit, 3000), type='l', ylim=c(0,2.5)) #this plots the line from the model 
points(lake.aq.v[,2:3], pch=21, bg='deepskyblue', col="black")  
abline(v=lake.aq.fit[2,3], col="deepskyblue") 
mtext('j', side = 3, line = -2.5, adj = .02, cex = 1.5)
# middle 
plot(variogramLine(wl.aq.fit, 3000), type='l', ylim=c(0,4)) 
points(wl.aq.v[,2:3], pch=21, bg='mediumpurple', col="black")  #this plots the points 
abline(v=wl.aq.fit[2,3], col="mediumpurple") #this plots the R range 
mtext('k', side = 3, line = -2.5, adj = .02, cex = 1.5)
# right 
plot(variogramLine(stream.aq.fit, 3000), type='l', ylim=c(0,2)) 
points(stream.aq.v[,2:3], pch=21, bg='green3', col="black")  #this plots the points 
abline(v=stream.aq.fit[2,3], col="green3") 
abline(v=1630, col="green3") 
mtext('l', side = 3, line = -2.5, adj = .02, cex = 1.5)

#MMI bottom left 
plot(variogramLine(lake.MMI.fit, 3000), type='l', ylim=c(0,300)) #this plots the line from the model 
points(lake.MMI.v[,2:3], pch=21, bg='deepskyblue', col="black")  
abline(v=lake.MMI.fit[2,3], col="deepskyblue") 
mtext('m', side = 3, line = -2.5, adj = .02, cex = 1.5)
#bottom middle 
plot(variogramLine(wl.MMI.fit, 3000), type='l', ylim=c(0,400)) 
points(wl.MMI.v[,2:3], pch=21, bg='mediumpurple', col="black")  #this plots the points 
abline(v=wl.MMI.fit[2,3], col="mediumpurple") #this plots the R range 
mtext('n', side = 3, line = -2.5, adj = .02, cex = 1.5)
#bottom right 
plot(variogramLine(stream.MMI.fit, 3000), type='l', ylim=c(0,500)) 
points(stream.MMI.v[,2:3], pch=21, bg='green3', col="black")  #this plots the points 
abline(v=stream.MMI.fit[2,3], col="green3") 
abline(v=2780, col="green3") 
mtext('o', side = 3, line = -2.5, adj = .02, cex = 1.5)

#add axis labels
mtext("Distance (km)", side = 1, outer = TRUE, cex = 1, line = 1)
mtext("Semivariance", side = 2, outer = TRUE, cex = 1, line = 1)

dev.off()