# Author: Katelyn King, adapted code by Ian McCullough
# Date: Jan 4, 2019
#####################################################################################################
#install.packages('randomForest')
#### R libraries ####
library(dplyr)
library(randomForest)
library(ggplot2)

#### load data ####
lake <- read.csv("Data/NLA2012_data.csv", header=T)
stream<-read.csv("Data/NRSA0809_data.csv", header=T)
wetland<-read.csv("Data/NWCA2011_data.csv", header=T)

### Combine all 3 ecosystems 
allecos<-gtools::smartbind(lake, stream, wetland)

### Compare biotic and abiotic variables to predictor variables###
## Random forest procedure
#set up parameters 
ntreez <- 500
par(mfrow=c(2,2))
par(mar=c(2,2,1,1), oma=c(2,2,0,0))
set_string <-c(8, 188, 18, 999) #TRY DIFFERENT SEED SETS 188, 18, 999 top predictors don't change 

### TP #### 
noNAs<-allecos[!(is.na(allecos$TP)),] 
for (i in 1:length(set_string)) {
  set.seed(set_string[i])
  RF_TP <- randomForest(log(TP+1) ~ Rveg + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                          TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                          WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015, 
                        data=noNAs, ntree=ntreez, importance=T, na.action=na.omit)
  RF_TP
  TP_imp <-randomForest::importance(RF_TP, type=1, scale=FALSE) #mean decrease in accuracy (also called permutation accuracy importance).
  imp<-as.data.frame(TP_imp) 
  imp$'Pred'   <-rownames(imp)
  imp <- structure(imp$`%IncMSE`, names = as.character(imp$Pred))
  
  dotchart(imp[order(imp)], xlab = "mean decrease in accuracy",
                main = set_string[i])
  
} 

# diagnostic for if I ran enough trees to reduce MSE  
plot(RF_TP, xlim=c(0,500))

#### TN ##### 
noNAs<-allecos[!(is.na(allecos$TN)),] 
for (i in 1:length(set_string)) { #TRY DIFFERENT SEED SETS 188, 18, 999, 8
  set.seed(set_string[i])
RF_TN <- randomForest(log(TN+1) ~ Rveg + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                        TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                        WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015, 
                      data=noNAs, ntree=ntreez, importance=T, na.action=na.omit)
RF_TN
TN_imp <-randomForest::importance(RF_TN, type=1, scale=FALSE) #mean decrease in accuracy (also called permutation accuracy importance).
imp<-as.data.frame(TN_imp) 
imp$'Pred'   <-rownames(imp)
imp <- structure(imp$`%IncMSE`, names = as.character(imp$Pred))

dotchart(imp[order(imp)], xlab = "mean decrease in accuracy",
         main = set_string[i])

} 

# diagnostic for if I ran enough trees to reduce MSE  
plot(RF_TN, xlim=c(0,500))

#### ChlA ##### 
noNAs<-allecos[!(is.na(allecos$CHLA)),] 
for (i in 1:length(set_string)) { #TRY DIFFERENT SEED SETS 188, 18, 999, 8
  set.seed(set_string[i])
RF_CHLA <- randomForest(log(CHLA+1) ~ Rveg + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                        TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                        WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015, 
                      data=noNAs, ntree=ntreez, importance=T, na.action=na.omit)
RF_CHLA
CHLA_imp <-randomForest::importance(RF_CHLA, type=1, scale=FALSE) #mean decrease in accuracy (also called permutation accuracy importance).
imp<-as.data.frame(CHLA_imp) 
imp$'Pred'   <-rownames(imp)
imp <- structure(imp$`%IncMSE`, names = as.character(imp$Pred))

dotchart(imp[order(imp)], xlab = "mean decrease in accuracy",
         main = set_string[i])

} 
# diagnostic for if I ran enough trees to reduce MSE  
plot(RF_CHLA, xlim=c(0,500))

#### Aquatic Veg ##### 
noNAs<-allecos[!(is.na(allecos$aqveg)),] 
for (i in 1:length(set_string)) { #TRY DIFFERENT SEED SETS 188, 18, 999, 8
  set.seed(set_string[i])
RF_aqveg <- randomForest(log(aqveg+1) ~ Rveg + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                          TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                          WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015, 
                        data=noNAs, ntree=ntreez, importance=T, na.action=na.omit)
RF_aqveg
veg_imp <-randomForest::importance(RF_aqveg, type=1, scale=FALSE) #mean decrease in accuracy (also called permutation accuracy importance).
imp<-as.data.frame(veg_imp) 
imp$'Pred'   <-rownames(imp)
imp <- structure(imp$`%IncMSE`, names = as.character(imp$Pred))

dotchart(imp[order(imp)], xlab = "mean decrease in accuracy",
         main = set_string[i])

} 
# diagnostic for if I ran enough trees to reduce MSE  
plot(RF_aqveg, xlim=c(0,500))

### 4-PANEL PLOT ####

#ggplot 
#TP
  imp<-as.data.frame(TP_imp) 
  imp$'Pred'   <-rownames(imp)
  io<-imp[order(imp$`%IncMSE`),]
  io$`Pred` <- factor(io$`Pred`, levels=io$`Pred`)  # convert to factor to retain sorted order in plot.
  
  theme_set(theme_bw())  

TP<- ggplot(io, aes(x=`Pred`, y=`%IncMSE`)) + 
  geom_point(stat="identity", size=2) + 
  coord_flip(ylim = c(0, 0.6)) + 
  theme(axis.text.y=element_text(face = c('plain', 'plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'plain', 
                                          'bold', 'bold', 'bold'))) +
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank())

#TN
imp<-as.data.frame(TN_imp) 
imp$'Pred'   <-rownames(imp)
io<-imp[order(imp$`%IncMSE`),]
io$`Pred` <- factor(io$`Pred`, levels=io$`Pred`)  # convert to factor to retain sorted order in plot.

TN<- ggplot(io, aes(x=`Pred`, y=`%IncMSE`)) + 
  geom_point(stat="identity", size=2) + 
  coord_flip(ylim = c(0, 0.6)) + 
  theme(axis.text.y=element_text(face = c( 'plain', 'plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'bold', 
                                          'bold', 'bold', 'bold'))) +
  theme(axis.title.x=element_blank())  + 
  theme(axis.title.y=element_blank())

#CHL
imp<-as.data.frame(CHLA_imp) 
imp$'Pred'   <-rownames(imp)
io<-imp[order(imp$`%IncMSE`),]
io$`Pred` <- factor(io$`Pred`, levels=io$`Pred`)  # convert to factor to retain sorted order in plot.

CHL<- ggplot(io, aes(x=`Pred`, y=`%IncMSE`)) + 
  geom_point(stat="identity", size=2) + 
  coord_flip(ylim = c(0, 0.4)) + 
  theme(axis.text.y=element_text(face = c('plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'plain', 
                                          'plain', 'bold', 'bold', 'bold'))) +
  ylab("mean decrease in accuracy") + 
  theme(axis.title.y=element_blank())


#AqVeg
imp<-as.data.frame(veg_imp) 
imp$'Pred'   <-rownames(imp)
io<-imp[order(imp$`%IncMSE`),]
io$`Pred` <- factor(io$`Pred`, levels=io$`Pred`)  # convert to factor to retain sorted order in plot.

veg<- ggplot(io, aes(x=`Pred`, y=`%IncMSE`)) + 
  geom_point(stat="identity", size=2) + 
  coord_flip( ylim = c(0, 0.85)) + 
  theme(axis.text.y=element_text(face = c('plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'plain', 
                                          'plain', 'plain', 'bold', 'bold'))) +
  ylab("mean decrease in accuracy") + 
  theme(axis.title.y=element_blank())


Fig3<-cowplot::plot_grid(TP, TN, CHL, veg, labels = c('a', 'b', "c", "d"))

#save plot
cowplot::save_plot("Figures/Fig3.pdf", Fig3, ncol = 2, nrow = 2, base_width = 7,
                   base_aspect_ratio = 1.1, dpi=600)

############################################
## Appendix S2 ###
############################################
#plot the number of trees as a function of error 
png('Sup2a.png',width = 7, height = 4, units = 'in', res = 600)
par(mfrow=c(2,2))
par(mar=c(2,2,1,1), oma=c(2,2,0,0))

plot(RF_TP, xlim=c(0,500), main ='')
mtext('a', side = 3, line = 0, adj = -0.1, cex = 1, font=2)
plot(RF_TN, xlim=c(0,500), main ='')
mtext('b', side = 3, line = 0, adj = -0.1, cex = 1, font=2)
plot(RF_CHLA, xlim=c(0,500), main ='')
mtext('c', side = 3, line = 0, adj = -0.1, cex = 1, font=2)
plot(RF_aqveg, xlim=c(0,500), main ='')
mtext('d', side = 3, line = 0, adj = -0.1, cex = 1, font=2)
mtext("number of trees", side = 1, outer = TRUE, cex = 1, line = 1)
mtext("error", side = 2, outer = TRUE, adj=0.53, cex = 1, line = 1)

dev.off()

#### subsampling RF for equal sample size #### 

set.seed(999) #try multiple random bootstrap samples, set.seed( 999, 19, 18, and 188 )
lake_samp<-sample(nrow(lake), size=400, replace = TRUE, prob = NULL)
lake_s <- lake[lake_samp, ]

set.seed(999)
stream_samp<-sample(nrow(stream), size=400, replace = TRUE, prob = NULL)
stream_s <- stream[stream_samp, ]

allecos_s<-gtools::smartbind(lake_s, stream_s, wetland)
allecos_s$lnTP<-log(allecos_s$TP+1)
allecos_s$lnTN<-log(allecos_s$TN+1)
allecos_s$lnCHL<-log(allecos_s$CHLA+1)
allecos_s$lnaqveg<-log(allecos_s$aqveg+1)


#plot importance 
  library(ggplot2)
  theme_set(theme_bw())  

#TP
  
  for (i in 1:length(set_string)) { #TRY DIFFERENT SEED SETS 188, 18, 999, 8
    set.seed(set_string[i])
    s_TP <- randomForest(lnTP ~ Rveg + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                           TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                           WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015, 
                         data=allecos_s, ntree=ntreez, importance=T, na.action=na.omit)
    print(s_TP)
    s_imp <-randomForest::importance(s_TP, type=1, scale=FALSE) 
    imp<-as.data.frame(s_imp) 
    imp$'Pred'   <-rownames(imp)
    imp <- structure(imp$`%IncMSE`, names = as.character(imp$Pred))
    
    dotchart(imp[order(imp)], xlab = "mean decrease in accuracy",
             main = set_string[i])
    
  } 

s_imp <-randomForest::importance(s_TP, type=1, scale=FALSE) 
imp<-as.data.frame(s_imp)  
imp$'Pred'   <-rownames(imp)
io<-imp[order(imp$`%IncMSE`),]
io$`Pred` <- factor(io$`Pred`, levels=io$`Pred`)  # convert to factor to retain sorted order in plot.
  
TP<- ggplot(io, aes(x=`Pred`, y=`%IncMSE`)) + 
    geom_point(stat="identity", size=2) + 
    coord_flip(ylim = c(0, 0.6)) + 
    theme(axis.text.y=element_text(face = c('plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'plain', 'plain', 'bold', 
                                            'bold', 'bold', 'bold',  'bold'))) +
  theme(axis.title.x=element_blank()) + 
    theme(axis.title.y=element_blank())

#TN
for (i in 1:length(set_string)) { #TRY DIFFERENT SEED SETS 188, 18, 999, 8
  set.seed(set_string[i])
  s_TN <- randomForest(lnTN ~ Rveg + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                         TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                         WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015, 
                       data=allecos_s, ntree=ntreez, importance=T, na.action=na.omit)
  print(s_TN)
  s_imp <-randomForest::importance(s_TN, type=1, scale=FALSE) 
  imp<-as.data.frame(s_imp) 
  imp$'Pred'   <-rownames(imp)
  imp <- structure(imp$`%IncMSE`, names = as.character(imp$Pred))
  
  dotchart(imp[order(imp)], xlab = "mean decrease in accuracy",
           main = set_string[i])
  
} 

s_imp <-randomForest::importance(s_TN, type=1, scale=FALSE) 
imp<-as.data.frame(s_imp)  
imp$'Pred'   <-rownames(imp)
io<-imp[order(imp$`%IncMSE`),]
io$`Pred` <- factor(io$`Pred`, levels=io$`Pred`)  # convert to factor to retain sorted order in plot.

TN<- ggplot(io, aes(x=`Pred`, y=`%IncMSE`)) + 
  geom_point(stat="identity", size=2) + 
  coord_flip(ylim = c(0, 0.6)) + 
  theme(axis.text.y=element_text(face = c('plain', 'plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'bold', 
                                          'bold', 'bold', 'bold'))) +
  theme(axis.title.x=element_blank())  + 
  theme(axis.title.y=element_blank())

#CHL
for (i in 1:length(set_string)) { #TRY DIFFERENT SEED SETS 188, 18, 999, 8
  set.seed(set_string[i])
  s_CHL <- randomForest(lnCHL ~ Rveg + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                         TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                         WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015, 
                       data=allecos_s, ntree=ntreez, importance=T, na.action=na.omit)
  print(s_CHL)
  s_imp <-randomForest::importance(s_CHL, type=1, scale=FALSE) 
  imp<-as.data.frame(s_imp) 
  imp$'Pred'   <-rownames(imp)
  imp <- structure(imp$`%IncMSE`, names = as.character(imp$Pred))
  
  dotchart(imp[order(imp)], xlab = "mean decrease in accuracy",
           main = set_string[i])
  
} 

s_imp <-randomForest::importance(s_CHL, type=1, scale=FALSE) 
imp<-as.data.frame(s_imp)  
imp$'Pred'   <-rownames(imp)
io<-imp[order(imp$`%IncMSE`),]
io$`Pred` <- factor(io$`Pred`, levels=io$`Pred`)  # convert to factor to retain sorted order in plot.

CHL<- ggplot(io, aes(x=`Pred`, y=`%IncMSE`)) + 
  geom_point(stat="identity", size=2) + 
  coord_flip() + 
  theme(axis.text.y=element_text(face = c( 'plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'plain',
                                          'plain', 'plain', 'plain', 'plain', 'plain', 'plain', 
                                          'plain', 'bold', 'bold', 'bold'))) +
  ylab("mean decrease in accuracy") + 
  theme(axis.title.y=element_blank())


#AqVeg
for (i in 1:length(set_string)) { #TRY DIFFERENT SEED SETS 188, 18, 999, 8
  set.seed(set_string[i])
  s_AQ <- randomForest(lnaqveg ~ Rveg + ELEVMEAN + Type + DEPTH + POPDEN + ROADDEN + NDEP + 
                         TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                         WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015, 
                       data=allecos_s, ntree=ntreez, importance=T, na.action=na.omit)
  print(s_AQ)
  s_imp <-randomForest::importance(s_AQ, type=1, scale=FALSE) 
  imp<-as.data.frame(s_imp) 
  imp$'Pred'   <-rownames(imp)
  imp <- structure(imp$`%IncMSE`, names = as.character(imp$Pred))
  
  dotchart(imp[order(imp)], xlab = "mean decrease in accuracy",
           main = set_string[i])
  
} 

s_imp <-randomForest::importance(s_AQ, type=1, scale=FALSE) 
imp<-as.data.frame(s_imp)  
imp$'Pred'   <-rownames(imp)
io<-imp[order(imp$`%IncMSE`),]
io$`Pred` <- factor(io$`Pred`, levels=io$`Pred`)  # convert to factor to retain sorted order in plot.

veg<- ggplot(io, aes(x=`Pred`, y=`%IncMSE`)) + 
    geom_point(stat="identity", size=2) + 
    coord_flip( ylim = c(0, 0.8)) + 
    theme(axis.text.y=element_text(face = c('plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'plain', 'plain', 'plain',
                                            'plain', 'plain', 'plain', 'plain', 'plain', 'plain', 
                                            'plain', 'plain', 'plain',  'bold'))) +
    ylab("mean decrease in accuracy") + 
    theme(axis.title.y=element_blank())
  



Sup2<-cowplot::plot_grid(TP, TN, CHL, veg, labels = c('a', 'b', "c", "d"))

#save plot
cowplot::save_plot("Figures/Sup2b.png", Sup2, ncol = 2, nrow = 2, base_width = 7,
                   base_aspect_ratio = 1.1, dpi=600)

