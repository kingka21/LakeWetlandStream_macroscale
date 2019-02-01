#### Script written by Katelyn King 
##### Start date: March 19, 2017

### load libraries #### 
library(dplyr)

#### National Wetlands Condition Assessment 2011 files available from website: https://www.epa.gov/national-aquatic-resource-surveys/
#import csv files into R to get variables, set your own working directory 
WLinfo<-read.csv("/Wetland2011_raw data/nwca2011_siteinfo.csv", header = TRUE)
WLchem<-read.csv("/Wetland2011_raw data/nwca2011_waterchem.csv", header = TRUE)
WLvmmi<-read.csv("/Wetland2011_raw data/nwca2011_cond_stress.csv", header = TRUE)
WLchla<-read.csv("/Wetland2011_raw data/nwca2011_chla.csv", header = TRUE)
WLsal<-read.csv("/Wetland2011_raw data/nwca2011_salinity.csv", header = TRUE)
WLveg<-read.csv("/Wetland2011_raw data/nwca2011_vegtype_grndsurf.csv", header = TRUE)
WLtree<-read.csv("/Wetland2011_raw data/nwca2011_tree.csv", header = TRUE)
WLland<-read.csv( "/Wetland2011_raw data/nwca2011_landscapechar.csv")
WLdepth<-read.csv("/Wetland2011_raw data/nwca2011_aawaterchar_edited.csv")

#select needed files 
WL_info<-dplyr::select(WLinfo, UID, SITE_ID, SITE_USE, AA_CENTER_LAT, AA_CENTER_LON, DATE_COL, AGGR_ECO9_2015 )
WL_chem<-dplyr::select(WLchem, UID, TN, TP) #TP (ug/L), TN (mg/L)
WL_chem$TN_ug<-WL_chem$TN * 1000  #convert mg/L to ug/L
WL_vmmi<-dplyr::select(WLvmmi, UID, VMMI) #VMMI (no units)
WL_chla<-dplyr::select(WLchla, UID, CHLA) #Chla (ug/L)
WL_sal<-dplyr::select(WLsal, UID, WATER_SALINITY) 
WL_land<-dplyr::select(WLland, UID, ELEVMAX_200M, ELEVMEAN_200M, ELEVMIN_200M, NADP_TOTALN_1000, #elev (m), NDEP (kg/ha)
                       NLCD2006_AGRICPCT_1000, NLCD2006_DEVELOPEDPCT_1000, NLCD2006_FORESTPCT_1000, NLCD2006_WETLANDPCT_1000, NLCD2006_52PCT_1000,
                       NLCD2006_71PCT_1000, POPDEN_1000, ROADDEN_1000, TIP_PT) #pop (people/mile2), road (km/km2), temp(degC)
WL_land$SHRUB_GRASS_PCT<- WL_land$NLCD2006_52PCT_1000 + WL_land$NLCD2006_71PCT_1000 #add up 52 shrub/scub + 71 grassland to equal one parameter 
WL_land$POPDEN_peoplesqkm<-WL_land$POPDEN_1000 *2.58999  # convert from (people/sqmi) to people/sqkm
WL_depth<-dplyr::select(WLdepth, UID, SW_DEPTH) #depth (cm)
WL_depth$DEPTH<-WL_depth$SW_DEPTH/100  #convert cm to m 

#Filter out freshwater sites 
WL_infosal<-left_join(WL_info, WL_sal, by="UID")
WL_fresh<-filter(WL_infosal, WATER_SALINITY == "FRESH")

#filter out revisted sites
ProbW<-filter(WL_fresh, SITE_USE == "NWCA_PROBABILITY") #370 prob sites
RefW<-filter(WL_fresh, SITE_USE =="NWCA_NOT_PROBABILITY") #88 ref sites 
WL_sites<-bind_rows(ProbW, RefW)

#join variables 
WL_join<-left_join(WL_sites, WL_chem, by="UID") %>%
            left_join(WL_chla, by="UID") %>%
              left_join(WL_vmmi, by="UID") %>%
                left_join(WL_land, by="UID")%>%
                  left_join(WL_depth, by="UID")

#Filter out sites that don't have enough water for chem sample 
WL_data<-WL_join[!(is.na(WL_join$TP)),] 

#add in aqveg and riparian veg data 
WL_veg<-dplyr::select(WLveg, UID, VTALL_VEG, TALL_VEG, HMED_VEG, MED_VEG, SMALL_VEG, VSMALL_VEG, WATER_AQVEG, WATER_EMERGVEG)
#group by UID and average at the site for each vegetation type (all are % covers)
WLveg_mean<-WL_veg%>%group_by(UID)%>%summarise(mean_VTALL_VEG = mean(VTALL_VEG, na.rm=TRUE), mean_TALL_VEG = mean(TALL_VEG, na.rm=TRUE), 
                                            mean_HMED_VEG = mean(HMED_VEG, na.rm=TRUE), mean_MED_VEG = mean(MED_VEG, na.rm=TRUE), 
                                            mean_SMALL_VEG = mean(SMALL_VEG, na.rm=TRUE), mean_VSMALL_VEG = mean(VSMALL_VEG, na.rm=TRUE), mean_AQVEG = mean(WATER_AQVEG, na.rm=TRUE), 
                                            mean_EMERGVEG = mean(WATER_EMERGVEG, na.rm=TRUE))

WLveg_mean$aqveg<-WLveg_mean$mean_EMERGVEG + WLveg_mean$mean_AQVEG  #add up the submerged, floating, and emergent veg
WLveg_mean$aqveg[WLveg_mean$aqveg > 100] <- 100  #can't have over 100% coverage

#add tree data 
trees<-filter(WLtree, PARAMETER == "VTALL_TREE" | PARAMETER == "TALL_TREE" | PARAMETER == "HMED_TREE" | 
                PARAMETER == "LMED_TREE" | PARAMETER == "SMALL_TREE" | PARAMETER == "VSMALL_TREE" )
trees<-dplyr::select(trees, UID, RESULT)
trees$RESULT<-as.numeric(paste(trees$RESULT))
WLtree_mean<-trees%>%group_by(UID)%>%summarise(mean_TREE = mean(RESULT, na.rm=TRUE))
WLdata2<- left_join(WLveg_mean, WLtree_mean, by='UID')
#average together all riparian veg 
WLdata2$Rveg<-rowMeans(subset(WLdata2, select = c(mean_VTALL_VEG, mean_TALL_VEG, mean_HMED_VEG, mean_MED_VEG, 
                                                  mean_SMALL_VEG, mean_VSMALL_VEG, mean_TREE)), na.rm = TRUE)

#join vegetation with other variables 
NWCA2011_data<-left_join(WL_data, WLdata2, by=("UID"))

NWCA2011_data$Type<- 'Wetland'
NWCA2011_data$WS_AREA<- 3.14   # 1,000m buffer area 1,000*1,000*3.14 = 3,140,000sqm/1,000,000= 3.14 sqkm

#### Extract climate data from PRISM #######
###############################################
library(sp)
library(raster)
library(rgdal)

# create a raster of the desired data, you have to enter the entire path 
precip<-raster("/PRISM/PRISM_ppt_30yr_normal_800mM2_annual_bil/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")
tmax<-raster("/PRISM/PRISM_tmax_30yr_normal_800mM2_annual_bil/PRISM_tmax_30yr_normal_800mM2_annual_bil.bil")
tmin<-raster("/PRISM/PRISM_tmin_30yr_normal_800mM2_annual_bil/PRISM_tmin_30yr_normal_800mM2_annual_bil.bil")
crs(precip)

#project to NAD 83 
wetland.ll <- SpatialPointsDataFrame(coords=NWCA2011_data[,c("AA_CENTER_LON","AA_CENTER_LAT")], data=NWCA2011_data,
                                    proj4string=CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")) 

#extract(x, y, ...) #where x is the raster object and y are the points represented by a two column matrix or data.frame
wetland.ll$meanprecip<-extract(precip, wetland.ll, na.rm=T)
wetland.ll$tmax<-raster::extract(tmax, wetland.ll, na.rm=T)
wetland.ll$tmin<-raster::extract(tmin, wetland.ll, na.rm=T)

#wetlands winter precipitation dec2010, jan/feb2011
p.10.dec<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201012_bil/PRISM_ppt_stable_4kmM3_201012_bil.bil")
p.11.jan<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201101_bil/PRISM_ppt_stable_4kmM3_201101_bil.bil")
p.11.feb<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201102_bil/PRISM_ppt_stable_4kmM3_201102_bil.bil")
p.winter.2011=stack(c(p.10.dec, p.11.jan,p.11.feb))
p.mean.winter.11=mean(p.winter.2011)
wetland.ll$PrecipWinter<-raster::extract(p.mean.winter.11, wetland.ll, na.rm=T)

### wetlands summer precip 
p.11.may<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201105_bil/PRISM_ppt_stable_4kmM3_201105_bil.bil")
p.11.jun<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201106_bil/PRISM_ppt_stable_4kmM3_201106_bil.bil")
p.11.jul<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201107_bil/PRISM_ppt_stable_4kmM3_201107_bil.bil")
p.11.aug<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201108_bil/PRISM_ppt_stable_4kmM3_201108_bil.bil")
p.11.sep<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201109_bil/PRISM_ppt_stable_4kmM3_201109_bil.bil")
p.summer.2011=stack(c(p.11.may, p.11.jun,p.11.jul,p.11.aug, p.11.sep))
p.mean.summer.11=mean(p.summer.2011)
wetland.ll$PrecipSummer<-raster::extract(p.mean.summer.11, wetland.ll, na.rm=T)

wetland<-as.data.frame(wetland.ll)

### ---- clean up data ------- 
#### get rid of unwanted columns
wetland<-subset(wetland, select = -c(
                 NLCD2006_52PCT_1000,  NLCD2006_71PCT_1000, SITE_USE, WATER_SALINITY,
                 TN, SW_DEPTH, mean_VTALL_VEG, mean_TALL_VEG, mean_HMED_VEG, mean_MED_VEG, 
                 mean_SMALL_VEG, mean_VSMALL_VEG, mean_TREE, mean_AQVEG, mean_EMERGVEG, 
                 AA_CENTER_LON.1, AA_CENTER_LAT.1, POPDEN_1000 
                 ))

wetland<- wetland %>% dplyr::rename (LAT_DD83=AA_CENTER_LAT, LON_DD83=AA_CENTER_LON, TN=TN_ug, MMI = VMMI, 
                         ELEVMAX=ELEVMAX_200M, ELEVMEAN=ELEVMEAN_200M, ELEVMIN=ELEVMIN_200M, 
                         NDEP=NADP_TOTALN_1000, POPDEN=POPDEN_peoplesqkm, ROADDEN=ROADDEN_1000,
                         AG_PCT=NLCD2006_AGRICPCT_1000, URBAN_PCT= NLCD2006_DEVELOPEDPCT_1000, FOREST_PCT= NLCD2006_FORESTPCT_1000, 
                         WETLAND_PCT=NLCD2006_WETLANDPCT_1000, 
                         PrecipNorm=meanprecip, TMAX=tmax, TMIN=tmin, Tsummer=TIP_PT)

summary(wetland)
Hmisc::describe(wetland)

write.csv(wetland, "Data/NWCA2011_data.csv", row.names = FALSE)

