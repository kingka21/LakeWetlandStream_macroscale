#Variogram analysis 


#checking normality using histogram 
hist(data2012$PTL_RESULT)
hist(data2012$NTL_RESULT)
hist(data2012$CHLX_RESULT)
hist(data2012$AQM) 
hist(data2012$MMI_BENT_NLA12) #normal 

#log transform variables new columns 
data2012$logTP<-log(data2012$PTL_RESULT)
data2012$logTN<-log(data2012$NTL_RESULT)
data2012$logChla<-log(1+data2012$CHLX_RESULT)
data2012$logAQM<-log(1+data2012$AQM)

hist(data2012$logTP) #normal 
hist(data2012$logTN) #normal 
hist(data2012$logChla) #normal 
hist(data2012$logAQM) #not great 