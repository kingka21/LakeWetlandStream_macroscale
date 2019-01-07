# Author: Katelyn King, adapted code by Ian McCullough
# Date: Jan 4, 2019
#####################################################################################################
#install.packages('randomForest')
#### R libraries ####
library(dplyr)
library(randomForest)

#### load data ####
lake <- read.csv("Data/NLA2012_data.csv", header=T)
stream<-read.csv("Data/NRSA0809_data.csv", header=T)
wetland<-read.csv("Data/NWCA2011_data.csv", header=T)

### Combine all 3 ecosystems 
allecos<-gtools::smartbind(lake, stream, wetland)

### Compare biotic and abiotic variables to predictor variables###
## Random forest procedure
# run with all predictors
# eliminate predictors with negative %IncMSE
# rerun without those variables, keep deleting predictors with negative %IncMSE until all positive
# analyze correlation matrix of remaining predictors and eliminate redundant variables?

### TN #### 
set.seed(999)
ntreez <- 500
noNAs<-allecos[!(is.na(allecos$TP)),] 
RF_TP <- randomForest(log(TP+0.00001) ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + WS_AREA + POPDEN + ROADDEN + NDEP + 
                        TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                        WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83, 
                      data=noNAs, ntree=ntreez, importance=T, na.action=na.omit)
#RF_RUEpp_evenness <- randomForest(slope ~ AG_PCT+FOREST_PCT+POPDEN, data=RUEpp_evenness_landscape[,2:ncol(RUEpp_evenness_landscape)], ntree=ntreez, importance=T)
RF_TP
randomForest::importance(RF_TP, type=1)
randomForest::importance(RF_TP , type=2) #MSE for regression

varImpPlot(RF_TP, main=' TP Variable importance')
#IncNodePurity relates to the loss function which by best splits are chosen. 
  #The loss function is mse for regression
  #More useful variables achieve higher increases in node purities, that is to find a split which has a high inter node 'variance' and a small intra node 'variance'
#%IncMSE is the increase in MSE of predictions(estimated with out-of-bag-CV) 


# diagnostic for how many trees to run 
plot(RF_TP, xlim=c(0,600))

set.seed(111)
nreplications <- 100
#rsq is last object of rfmodel$rsq (number of objects=number of trees)
rf.replications <- replicate(nreplications, {
  rf_tmp <- randomForest(log(TP+0.00001) ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + WS_AREA + POPDEN + ROADDEN + NDEP + 
                           TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                           WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83, 
                         data=noNAs, ntree=ntreez, importance=T, na.action=na.omit)
  list(rsq = rf_tmp$rsq, varimp = rf_tmp$importance)
}, simplify = F)
# Make a data.frame with r squared values
rf_summary_df <- data.frame(matrix(ncol = 25, nrow = nreplications))
rf_summary_df[,1] <- t(data.frame(rsq = sapply(rf.replications, function(x){x[["rsq"]]}))[ntreez,]*100)

for (i in 1:nreplications){
  rf_summary_df[i,2] <- rf.replications[[i]]$varimp[1,1] #%IncMSE column
  rf_summary_df[i,3] <- rf.replications[[i]]$varimp[2,1]
  rf_summary_df[i,4] <- rf.replications[[i]]$varimp[3,1]
  rf_summary_df[i,5] <- rf.replications[[i]]$varimp[4,1]
  rf_summary_df[i,6] <- rf.replications[[i]]$varimp[5,1]
  rf_summary_df[i,7] <- rf.replications[[i]]$varimp[6,1]
  rf_summary_df[i,8] <- rf.replications[[i]]$varimp[7,1]
  rf_summary_df[i,9] <- rf.replications[[i]]$varimp[8,1]
  rf_summary_df[i,10] <- rf.replications[[i]]$varimp[9,1]
  rf_summary_df[i,11] <- rf.replications[[i]]$varimp[10,1]
  rf_summary_df[i,12] <- rf.replications[[i]]$varimp[11,1]
  rf_summary_df[i,13] <- rf.replications[[i]]$varimp[12,1]
  rf_summary_df[i,14] <- rf.replications[[i]]$varimp[13,1]
  rf_summary_df[i,15] <- rf.replications[[i]]$varimp[14,1]
  rf_summary_df[i,16] <- rf.replications[[i]]$varimp[15,1]
  rf_summary_df[i,17] <- rf.replications[[i]]$varimp[16,1]
  rf_summary_df[i,18] <- rf.replications[[i]]$varimp[17,1]
  rf_summary_df[i,19] <- rf.replications[[i]]$varimp[18,1]
  rf_summary_df[i,20] <- rf.replications[[i]]$varimp[19,1]
  rf_summary_df[i,21] <- rf.replications[[i]]$varimp[20,1]
  rf_summary_df[i,22] <- rf.replications[[i]]$varimp[21,1]
  rf_summary_df[i,23] <- rf.replications[[i]]$varimp[22,1]
  rf_summary_df[i,24] <- rf.replications[[i]]$varimp[23,1]
  rf_summary_df[i,25] <- rf.replications[[i]]$varimp[24,1]
  
  #rf_summary_df[i,4] <- rf.replications[[i]]$varimp[1,2] #%IncNocePurity column
  #rf_summary_df[i,5] <- rf.replications[[i]]$varimp[2,2]
}
colnames(rf_summary_df) <- c('rsquared','RVEG','elevMax','elevmin','elevmean', "Type", "depth", "WSarea", "popden", "roadden", "Ndep", "Tmin", "Tmax", "PrecipNorm", "PrecipSummer", "Tsummer", "PrecipWinter", "Ag", "Forest", "WL", "Urb", "Shrub", "Region", "lat", "lon")
summary(rf_summary_df)
hist(rf_summary_df[,1], main=paste0(ntreez, " trees"), xlab='% variation explained')

#### TN ##### 
set.seed(999)
ntreez <- 500
noNAs<-allecos[!(is.na(allecos$TN)),] 
RF_TN <- randomForest(log(TN+0.00001) ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + WS_AREA + POPDEN + ROADDEN + NDEP + 
                        TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                        WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83, 
                      data=noNAs, ntree=ntreez, importance=T, na.action=na.omit)
RF_TN
randomForest::importance(RF_TN, type=1)
randomForest::importance(RF_TN , type=2) #MSE for regression

varImpPlot(RF_TN, main='TN Variable importance')

#### ChlA ##### 
set.seed(999)
ntreez <- 500
noNAs<-allecos[!(is.na(allecos$CHLA)),] 
RF_CHLA <- randomForest(log(CHLA+0.00001) ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + WS_AREA + POPDEN + ROADDEN + NDEP + 
                        TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                        WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83, 
                      data=noNAs, ntree=ntreez, importance=T, na.action=na.omit)
RF_CHLA
randomForest::importance(RF_CHLA, type=1)
randomForest::importance(RF_CHLA , type=2) #MSE for regression

varImpPlot(RF_CHLA, main='CHLA Variable importance')

#### Aquatic Veg ##### 
set.seed(999)
ntreez <- 500
noNAs<-allecos[!(is.na(allecos$aqveg)),] 
RF_aqveg <- randomForest(log(aqveg+0.00001) ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + WS_AREA + POPDEN + ROADDEN + NDEP + 
                          TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                          WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83, 
                        data=noNAs, ntree=ntreez, importance=T, na.action=na.omit)
RF_aqveg
randomForest::importance(RF_aqveg, type=1)
randomForest::importance(RF_aqveg , type=2) #MSE for regression

varImpPlot(RF_aqveg, main='aqveg Variable importance')

#### MMI ##### 
set.seed(999)
ntreez <- 500
noNAs<-allecos[!(is.na(allecos$MMI)),] 
RF_MMI <- randomForest(MMI ~ Rveg + ELEVMAX + ELEVMIN + ELEVMEAN + Type + DEPTH + WS_AREA + POPDEN + ROADDEN + NDEP + 
                           TMIN + TMAX + PrecipNorm + PrecipSummer + Tsummer + PrecipWinter + AG_PCT + FOREST_PCT +
                           WETLAND_PCT + URBAN_PCT + SHRUB_GRASS_PCT + AGGR_ECO9_2015 + LAT_DD83 + LON_DD83, 
                         data=noNAs, ntree=ntreez, importance=T, na.action=na.omit)
RF_MMI
randomForest::importance(RF_MMI, type=1)
randomForest::importance(RF_MMI , type=2) #MSE for regression

varImpPlot(RF_MMI, main='MMI Variable importance')
