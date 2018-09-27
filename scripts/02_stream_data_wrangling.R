#### Script written by Katelyn King 
##### Start date: June 6, 2017

### load libraries #### 
library(dplyr)

#### National Rivers and Streams Assessment 2008-2009 files available from website: https://www.epa.gov/national-aquatic-resource-surveys/
#import csv files into R to get variables, set your own working directory 
NRSAchem<-read.csv("/Streams 2008 Raw Data/chem.csv")
NRSAinfo<-read.csv("/Streams 2008 Raw Data/siteinfo_0.csv")
NRSAbent<-read.csv("/Streams 2008 Raw Data/bentcond.csv")
NRSAveg<-read.csv("/Streams 2008 Raw Data/phabmed.csv")
NRSAland<-read.csv("/Streams 2008 Raw Data/land.csv")

#data from EPA personnel. CHLA (ug/L) from the water column
NRSAchla<-read.csv("/Streams 2008 Raw Data/wchla.csv")
NRSA_chla<-dplyr::filter(NRSAchla, SAMPLE_CAT == "P") #primary sample
NRSA_chla<-dplyr::filter(NRSA_chla, SAM_CODE == "REGULAR") #regular sample
NRSA_chla<-dplyr::select(NRSA_chla, UID, CHLA)

#selet needed variables 
NRSA_info<-dplyr::select(NRSAinfo, SITE_ID, UID, DATE_COL, LOC_NAME, AGGR_ECO9_2015, LON_DD83, 
              LAT_DD83, SITE_CLASS, INDEX_VISIT)

#selet needed variables NTL (ug/L), PTL (ug/L)
NRSA_chem<-dplyr::select(NRSAchem, UID, NTL, PTL)

#XFC_AQM (aquatic macrophyte mean cover, proportion), XCMG (RIP VEG Sum of Canopy+Mid+Ground layer cover, proportion)
NRSA_veg<-dplyr::select(NRSAveg, UID, XCMG, XDEPTH_CM, XFC_AQM)

#MMI (no units)
NRSA_bent<-dplyr::select(NRSAbent, UID, MMI_BENT)

#select land use/cover variables ELEV (m),  Pop Den (sqkm), RDDEN (km/sqkm), LULC (%), WAT_AREA (sqkm), Total N Dep (kg/ha)
NRSA_land<-dplyr::select(NRSAland, UID, NHDWAT_NEDELEV_MAX, NHDWAT_ELEV, NHDWAT_NEDELEV_MIN,
                         NHDWAT_POPDENKM, RDDEN, PCT_AG, PCT_URB,
                         PCT_FOR, PCT_WET, PCT_SHRUB_GRASS, NHDWAT_AREA_SQKM, NHDWAT_NADP2009_MEAN_TOTALN)

#join all datasets 
NRSA_joined_data<-left_join(NRSA_chem, NRSA_info, by=("UID")) %>%
                left_join(NRSA_bent, by=("UID")) %>%
                left_join(NRSA_veg, by=("UID")) %>%
                left_join(NRSA_chla, by=("UID")) %>%
                left_join(NRSA_land, by=("UID"))

#filter out duplicate site samples: 
ProbS<- filter(NRSA_joined_data, INDEX_VISIT == "YES") #1924 prob sites 
RefS<- filter(NRSA_joined_data, SITE_CLASS == "HAND") #199 ref streams 
NRSA0809_data<-bind_rows(ProbS, RefS) #2123 all sites

NRSA0809_data$DEPTH<-NRSA_data$XDEPTH_CM/100  # from cm to m 
NRSA0809_data$aqveg<-NRSA_data$XFC_AQM*100  # from proportion to %
NRSA0809_data$Rveg<-NRSA_data$XCMG/3 * 100   # from sum of 3 proportions to % 
NRSA0809_data$Rveg[NRSA0809_data$Rveg > 100] <- 100  #can't have over 100% coverage

#add ecosystem type column 
NRSA0809_data$Type<- 'Stream'

#### Extract climate data from PRISM #######
###############################################
library(sp)
library(raster)
library(rgdal)
library(gstat)
library(prism)
library(lubridate)
library(gtools)

# create a raster of the desired data, you have to enter the entire path 
precip<-raster("/PRISM/PRISM_ppt_30yr_normal_800mM2_annual_bil/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")
tmax<-raster("/PRISM/PRISM_tmax_30yr_normal_800mM2_annual_bil/PRISM_tmax_30yr_normal_800mM2_annual_bil.bil")
tmin<-raster("/PRISM/PRISM_tmin_30yr_normal_800mM2_annual_bil/PRISM_tmin_30yr_normal_800mM2_annual_bil.bil")
crs(precip)

#project to NAD 83 
stream.ll <- SpatialPointsDataFrame(coords=NRSA0809_data[,c("LON_DD83","LAT_DD83")], data=NRSA0809_data,
                                  proj4string=CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")) 

#extract(x, y, ...) #where x is the raster object and y are the points represented by a two column matrix or data.frame
stream.ll$meanprecip<-extract(precip, stream.ll, na.rm=T)
stream.ll$tmax<-raster::extract(tmax, stream.ll, na.rm=T)
stream.ll$tmin<-raster::extract(tmin, stream.ll, na.rm=T)

#stream summer temp for 08-09
t.08.may<-raster("/PRISM/PRISM_tmean_stable_4kmM2_200805_bil/PRISM_tmean_stable_4kmM2_200805_bil.bil")
t.08.jun<-raster("/PRISM/PRISM_tmean_stable_4kmM2_200806_bil/PRISM_tmean_stable_4kmM2_200806_bil.bil")
t.08.jul<-raster("/PRISM/PRISM_tmean_stable_4kmM2_200807_bil/PRISM_tmean_stable_4kmM2_200807_bil.bil")
t.08.aug<-raster("/PRISM/PRISM_tmean_stable_4kmM2_200808_bil/PRISM_tmean_stable_4kmM2_200808_bil.bil")
t.08.sep<-raster("/PRISM/PRISM_tmean_stable_4kmM2_200809_bil/PRISM_tmean_stable_4kmM2_200809_bil.bil")
t.09.may<-raster("/PRISM/PRISM_tmean_stable_4kmM2_200905_bil/PRISM_tmean_stable_4kmM2_200905_bil.bil")
t.09.jun<-raster("/PRISM/PRISM_tmean_stable_4kmM2_200906_bil/PRISM_tmean_stable_4kmM2_200906_bil.bil")
t.09.jul<-raster("/PRISM/PRISM_tmean_stable_4kmM2_200907_bil/PRISM_tmean_stable_4kmM2_200907_bil.bil")
t.09.aug<-raster("/PRISM/PRISM_tmean_stable_4kmM2_200908_bil/PRISM_tmean_stable_4kmM2_200908_bil.bil")
t.09.sep<-raster("/PRISM/PRISM_tmean_stable_4kmM2_200909_bil/PRISM_tmean_stable_4kmM2_200909_bil.bil")
#Average the temps for summer months into a single raster
t.summer.2008=stack(c(t.08.may, t.08.jun,t.08.jul,t.08.aug, t.08.sep))
t.mean.summer.08=mean(t.summer.2008)
t.summer.2009=stack(c(t.09.may, t.09.jun,t.09.jul,t.09.aug, t.09.sep))
t.mean.summer.09=mean(t.summer.2009)
stream.ll$Tsummer.08<-raster::extract(t.mean.summer.08, stream.ll, na.rm=T)
stream.ll$Tsummer.09<-raster::extract(t.mean.summer.09, stream.ll, na.rm=T)

#streams summer precipitation for 08-09
p.08.may<-raster(" /PRISM/PRISM_ppt_stable_4kmM3_200805_bil/PRISM_ppt_stable_4kmM3_200805_bil.bil")
p.08.jun<-raster(" /PRISM/PRISM_ppt_stable_4kmM3_200806_bil/PRISM_ppt_stable_4kmM3_200806_bil.bil")
p.08.jul<-raster(" /PRISM/PRISM_ppt_stable_4kmM3_200807_bil/PRISM_ppt_stable_4kmM3_200807_bil.bil")
p.08.aug<-raster(" /PRISM/PRISM_ppt_stable_4kmM3_200808_bil/PRISM_ppt_stable_4kmM3_200808_bil.bil")
p.08.sep<-raster(" /PRISM/PRISM_ppt_stable_4kmM3_200809_bil/PRISM_ppt_stable_4kmM3_200809_bil.bil")
p.09.may<-raster(" /PRISM/PRISM_ppt_stable_4kmM3_200905_bil/PRISM_ppt_stable_4kmM3_200905_bil.bil")
p.09.jun<-raster(" /PRISM/PRISM_ppt_stable_4kmM3_200906_bil/PRISM_ppt_stable_4kmM3_200906_bil.bil")
p.09.jul<-raster(" /PRISM/PRISM_ppt_stable_4kmM3_200907_bil/PRISM_ppt_stable_4kmM3_200907_bil.bil")
p.09.aug<-raster(" /PRISM/PRISM_ppt_stable_4kmM3_200908_bil/PRISM_ppt_stable_4kmM3_200908_bil.bil")
p.09.sep<-raster(" /PRISM/PRISM_ppt_stable_4kmM3_200909_bil/PRISM_ppt_stable_4kmM3_200909_bil.bil")
#Average the precip for summer months into a single raster
p.summer.2008=stack(c(p.08.may, p.08.jun,p.08.jul,p.08.aug, p.08.sep))
p.mean.summer.08=mean(p.summer.2008)
p.summer.2009=stack(c(p.09.may, p.09.jun,p.09.jul,p.09.aug, p.09.sep))
p.mean.summer.09=mean(p.summer.2009)
stream.ll$PrecipSummer.08<-raster::extract(p.mean.summer.08, stream.ll, na.rm=T)
stream.ll$PrecipSummer.09<-raster::extract(p.mean.summer.09, stream.ll, na.rm=T)

#streams winter precip for dec07, jan08, feb08 and dec 08, jan09, feb09 
p.07.dec<-raster("/PRISM/PRISM_ppt_stable_4kmM3_200712_bil/PRISM_ppt_stable_4kmM3_200712_bil.bil")
p.08.jan<-raster("/PRISM/PRISM_ppt_stable_4kmM3_200801_bil/PRISM_ppt_stable_4kmM3_200801_bil.bil")
p.08.feb<-raster("/PRISM/PRISM_ppt_stable_4kmM3_200802_bil/PRISM_ppt_stable_4kmM3_200802_bil.bil")
p.08.dec<-raster("/PRISM/PRISM_ppt_stable_4kmM3_200812_bil/PRISM_ppt_stable_4kmM3_200812_bil.bil")
p.09.jan<-raster("/PRISM/PRISM_ppt_stable_4kmM3_200901_bil/PRISM_ppt_stable_4kmM3_200901_bil.bil")
p.09.feb<-raster("/PRISM/PRISM_ppt_stable_4kmM3_200902_bil/PRISM_ppt_stable_4kmM3_200902_bil.bil")
p.winter.2008=stack(c(p.07.dec, p.08.jan,p.08.feb))
p.mean.winter.08=mean(p.winter.2008)
p.winter.2009=stack(c(p.08.dec, p.09.jan,p.09.feb))
p.mean.winter.09=mean(p.winter.2009)
stream.ll$PrecipWinter.08<-raster::extract(p.mean.winter.08, stream.ll, na.rm=T)
stream.ll$PrecipWinter.09<-raster::extract(p.mean.winter.09, stream.ll, na.rm=T)
head(stream.ll@data)    ## Check data structure

stream<-as.data.frame(stream.ll)  

###### keep temp and precip with the corresponding year 
#new column for correct format
stream$newdate<-as.Date(stream$DATE_COL, format='%d-%b-%y')
stream$sampleyear<-lubridate::year(stream$newdate)
stream08<-filter(stream, sampleyear == 2008) #filter out 2008 
stream09<- filter(stream, sampleyear == 2009) #filter out 2009
stream08<-subset(stream08, select = -c(Tsummer.09, PrecipSummer.09, PrecipWinter.09) ) #get rid of 2009 
stream09<-subset(stream09, select = -c(Tsummer.08, PrecipSummer.08, PrecipWinter.08) ) #get rid of 2008  
stream08<- stream08 %>% rename (Tsummer=Tsummer.08, PrecipSummer=PrecipSummer.08, PrecipWinter=PrecipWinter.08) #change names
stream09<- stream09 %>% rename (Tsummer=Tsummer.09, PrecipSummer=PrecipSummer.09, PrecipWinter=PrecipWinter.09) #change names 

stream2<-smartbind(stream08,stream09)

### ---- clean up data ------- 
#### get rid of unwanted columns
stream2<-subset(stream2, select = -c(LOC_NAME, SITE_CLASS, INDEX_VISIT, 
                                     newdate, sampleyear, LON_DD83.1, LAT_DD83.1, 
                                     XCMG, XDEPTH_CM, XFC_AQM))


#rename columns 
stream2<- stream2 %>% dplyr::rename (TP= PTL, TN=NTL, MMI= MMI_BENT,
                         AG_PCT=PCT_AG, URBAN_PCT=PCT_URB, FOREST_PCT=PCT_FOR, WETLAND_PCT=PCT_WET, SHRUB_GRASS_PCT=PCT_SHRUB_GRASS, 
                         ELEVMAX=NHDWAT_NEDELEV_MAX, ELEVMEAN=NHDWAT_ELEV, ELEVMIN=NHDWAT_NEDELEV_MIN,
                         POPDEN=NHDWAT_POPDENKM, ROADDEN=RDDEN, TMAX=tmax, TMIN=tmin, WS_AREA=NHDWAT_AREA_SQKM, 
                         NDEP=NHDWAT_NADP2009_MEAN_TOTALN, PrecipNorm=meanprecip)

summary(stream2)

write.csv(stream2, "Data/NRSA0809_data.csv", row.names = FALSE)


