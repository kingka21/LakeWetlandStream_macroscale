#### Script written by Katelyn King 
##### Start date: Janurary 18, 2017


### load libraries #### 
library(dplyr)

#### National Lake Assessment 2012 files available from website: https://www.epa.gov/national-aquatic-resource-surveys/
#import csv files into R to get variables, set your own working directory 
NLA2012chem<-read.csv("/nla2012_waterchem_wide.csv")  #TP and TN
NLA2012info<-read.csv("/nla2012_wide_siteinfo_08232016.csv") #site information
NLA2012mmi.chla<-read.csv("/nla12_keyvariables_data.csv") #chla, benthic MMI
NLA2012aqveg<-read.csv("/nla2012_wide_phabmet_10202016.csv") #aquatic vegetation % covers 
NLA2012rveg<-read.csv("/nla2012_wide_phabmet_10202016.csv") #riparian vegetation 

# select out lakes that were sampled, both probability lakes and reference lakes
nla2012chem_info_join<-full_join(NLA2012info, NLA2012chem, by=("UID"))
sampled<-filter(nla2012chem_info_join, STATUS == "Target_Sampled") #should have sample data 
Prob<- filter(sampled, INDEX_NLA == "Y") #1038 probability lakes 
Ref<- filter(sampled, SITETYPE == "HAND") #92 reference lakes 
NLAsites_with_chem<-bind_rows(Prob, Ref)

#select only columns needed for analysis TP (ug/L), TN (mg/L)
NLA_chem<-dplyr::select(NLAsites_with_chem, SITE_ID, UID, DATE_COL, AGGR_ECO9_2015, LAT_DD83, LON_DD83, SITETYPE, PTL_RESULT, NTL_RESULT)
NLA_chem$NTL_RESULT_ug<-NLA_chem$NTL_RESULT*1000   #convert mg to ug 

#select needed columns from chla (ug/L), MMI (no units),  site depth (m)
NLA_MMI<-dplyr::select(NLA2012mmi.chla, UID, INDEX_SITE_DEPTH, MMI_BENT_NLA12, CHLX_RESULT)

#select aquatic veg (proportion)
NLA_AQM<-dplyr::select(NLA2012aqveg, UID, AMFCALL)
NLA_AQM$aqveg<-NLA_AQM$AMFCALL*100 #convert from proportion to %

# make % riparian vegetation coverage: select canopy, ground and understory layers then average across columns
NLA_Rveg<- dplyr::select(NLA2012rveg, UID, RVFCCANBIG_RIP, RVFCCANSMALL_RIP, RVFCGNDNONW_RIP, 
                     RVFCGNDWOODY_RIP, RVFCUNDNONW_RIP, RVFCUNDWOODY_RIP)
NLA_Rveg$RVEG<-rowMeans(subset(NLA_Rveg, select = c(RVFCCANBIG_RIP, RVFCCANSMALL_RIP, RVFCGNDNONW_RIP, 
                                                    RVFCGNDWOODY_RIP, RVFCUNDNONW_RIP, RVFCUNDWOODY_RIP)), na.rm = TRUE)
NLA_Rveg$RVEG_pct<-NLA_Rveg$RVEG*100 #convert from proportion to a %

#data from EPA personnel, requested to be put on the NARS website; elevation (cm), NADP (kg/ha), Popden (people/sqmi), roadden (km/sqkm), LULC (%), 
landscape<-read.csv("/NLA2012_wide_landscape.csv")
NLA_landscape<-dplyr::select(landscape, SITE_ID, ELEVMAX_BSN, ELEVMEAN_BSN, ELEVMIN_BSN, NADP_TOTALN_BSN, NLCD2006_AGRICPCT_BSN,
           NLCD2006_DEVELOPEDPCT_BSN, NLCD2006_FORESTPCT_BSN, NLCD2006_WETLANDPCT_BSN, NLCD2006_71PCT_BSN,
           NLCD2006_52PCT_BSN, POPDEN_BSN, ROADDEN_BSN)
NLA_landscape$POPDEN_peoplesqkm<-NLA_landscape$POPDEN_BSN *2.58999  # convert from (people/sqmi) to people/sqkm
NLA_landscape$ELEVMAX_m<-NLA_landscape$ELEVMAX_BSN/100 #convert cm to m 
NLA_landscape$ELEVMEAN_m<-NLA_landscape$ELEVMEAN_BSN/100  #convert cm to m 
NLA_landscape$ELEVMIN_m<-NLA_landscape$ELEVMIN_BSN/100  #convert cm to m 
NLA_landscape$SHRUB_GRASS_PCT<-NLA_landscape$NLCD2006_71PCT_BSN + NLA_landscape$NLCD2006_52PCT_BSN #add up 52 shrub/scub + 71 grassland to equal one parameter

#data from EPA personnel to get watershed size (sqkm)
watershed<-read.csv("/NLA2012_basins.csv")
NLA_watershed<-dplyr::select(watershed, SITE_ID, BASINAreaSqKM)

#join all datasets
NLA2012_data<-left_join(NLA_chem, NLA_MMI, by=("UID")) %>% 
                    left_join(NLA_AQM, by=("UID"))  %>% 
                      left_join(NLA_Rveg, by=("UID"))  %>% 
                      left_join(NLA_landscape, by = "SITE_ID") %>%
                        left_join(NLA_watershed, by = "SITE_ID" )

#add ecosystem type column 
NLA2012_data$Type<- 'Lake'

#### Extract climate data from PRISM #######
###############################################

library(sp)
library(raster)
library(rgdal)
library(gstat)
install.packages('prism')
library(prism)

# create a raster of the desired data available from PRISM 
precip<-raster("/PRISM/PRISM_ppt_30yr_normal_800mM2_annual_bil/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")
tmax<-raster("/PRISM/PRISM_tmax_30yr_normal_800mM2_annual_bil/PRISM_tmax_30yr_normal_800mM2_annual_bil.bil")
tmin<-raster("/PRISM/PRISM_tmin_30yr_normal_800mM2_annual_bil/PRISM_tmin_30yr_normal_800mM2_annual_bil.bil")
crs(precip)

#project to NAD 83 
lake.ll <- SpatialPointsDataFrame(coords=NLA2012_data[,c("LON_DD83","LAT_DD83")], data=NLA2012_data,
                                     proj4string=CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")) 

#extract(x, y, ...) #where x is the raster object and y are the points represented by a two column matrix or data.frame
lake.ll$meanprecip<-raster::extract(precip, lake.ll, na.rm=T)
lake.ll$tmax<-raster::extract(tmax, lake.ll, na.rm=T)
lake.ll$tmin<-raster::extract(tmin, lake.ll, na.rm=T)
head(lake.ll@data)    ## Check data structure

#lake summer temp 2012
t.12.may<-raster("/PRISM/PRISM_tmean_stable_4kmM2_201205_bil/PRISM_tmean_stable_4kmM2_201205_bil.bil")
t.12.jun<-raster("/PRISM/PRISM_tmean_stable_4kmM2_201206_bil/PRISM_tmean_stable_4kmM2_201206_bil.bil")
t.12.jul<-raster("/PRISM/PRISM_tmean_stable_4kmM2_201207_bil/PRISM_tmean_stable_4kmM2_201207_bil.bil")
t.12.aug<-raster("/PRISM/PRISM_tmean_stable_4kmM2_201208_bil/PRISM_tmean_stable_4kmM2_201208_bil.bil")
t.12.sep<-raster("/PRISM/PRISM_tmean_stable_4kmM2_201209_bil/PRISM_tmean_stable_4kmM2_201209_bil.bil")
t.summer.2012=stack(c(t.12.may, t.12.jun,t.12.jul,t.12.aug, t.12.sep))
t.mean.summer.12=mean(t.summer.2012)
lake.ll$Tsummer<-raster::extract(t.mean.summer.12, lake.ll, na.rm=T)

#lake summer precip 2012
p.12.may<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201205_bil/PRISM_ppt_stable_4kmM3_201205_bil.bil")
p.12.jun<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201206_bil/PRISM_ppt_stable_4kmM3_201206_bil.bil")
p.12.jul<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201207_bil/PRISM_ppt_stable_4kmM3_201207_bil.bil")
p.12.aug<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201208_bil/PRISM_ppt_stable_4kmM3_201208_bil.bil")
p.12.sep<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201209_bil/PRISM_ppt_stable_4kmM3_201209_bil.bil")
p.summer.2012=stack(c(p.12.may, p.12.jun,p.12.jul,p.12.aug, p.12.sep))
p.mean.summer.12=mean(p.summer.2012)
lake.ll$PrecipSummer<-raster::extract(p.mean.summer.12, lake.ll, na.rm=T)

#NLA winter precipitation dec 2011, jan/feb2012
p.11.dec<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201112_bil/PRISM_ppt_stable_4kmM3_201112_bil.bil")
p.12.jan<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201201_bil/PRISM_ppt_stable_4kmM3_201201_bil.bil")
p.12.feb<-raster("/PRISM/PRISM_ppt_stable_4kmM3_201202_bil/PRISM_ppt_stable_4kmM3_201202_bil.bil")
p.winter.2012=stack(c(p.11.dec, p.12.jan,p.12.feb))
p.mean.winter.12=mean(p.winter.2012)
lake.ll$PrecipWinter<-raster::extract(p.mean.winter.12, lake.ll, na.rm=T)

lake<-as.data.frame(lake.ll)  

### ---- clean up data ------- 
#### get rid of unwanted columns 
lake<-subset(lake, select = -c(SITETYPE, NTL_RESULT, AMFCALL, RVFCCANBIG_RIP,  RVFCCANSMALL_RIP,  RVFCGNDNONW_RIP,
                                       RVFCGNDWOODY_RIP,  RVFCUNDNONW_RIP, RVFCUNDWOODY_RIP, RVEG, 
                                       ELEVMIN_BSN, ELEVMEAN_BSN, ELEVMAX_BSN, NLCD2006_71PCT_BSN, NLCD2006_52PCT_BSN,
                                       POPDEN_BSN, LON_DD83.1, LAT_DD83.1))

#rename columns 
lake<- lake %>% dplyr::rename (TP=PTL_RESULT, TN=NTL_RESULT_ug, CHLA=CHLX_RESULT, MMI=MMI_BENT_NLA12, DEPTH=INDEX_SITE_DEPTH, 
                          AG_PCT=NLCD2006_AGRICPCT_BSN, URBAN_PCT=NLCD2006_DEVELOPEDPCT_BSN, FOREST_PCT=NLCD2006_FORESTPCT_BSN, 
                          WETLAND_PCT=NLCD2006_WETLANDPCT_BSN, PrecipNorm=meanprecip, 
                          TMAX=tmax, TMIN=tmin, Rveg=RVEG_pct,
                          ELEVMAX=ELEVMAX_m, ELEVMEAN=ELEVMEAN_m, ELEVMIN=ELEVMIN_m,
                          NDEP=NADP_TOTALN_BSN, POPDEN=POPDEN_peoplesqkm, ROADDEN=ROADDEN_BSN, WS_AREA=BASINAreaSqKM
)

summary(lake)

write.csv(lake, "Data/NLA2012_data.csv", row.names = FALSE)
