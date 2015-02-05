rm(list=ls())
options(scipen=999) 

setwd("C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ")
DataTotal=read.table("./DataFinal_totalonly.txt", header=T, fill=T)
PopTotal <- data.frame()

DataProportion=read.table("./DataFinal_proportiononly.txt", header=T, fill=T)

rm(distance)
distance=read.table("./DistanceforR.txt",  header=T)

DataProportion <- cbind( DataProportion, distance[ match(DataProportion[,1], distance[,1]), 6])
colnames(DataProportion)[length(DataProportion[1,])] <- "distance"

DataProportionForDistance <- subset( DataProportion,  DataProportion[,25]>0 )
DataProportionForDistance[,28][DataProportionForDistance[,28]==0] <- 0.000000001

lmfitCon = lm( DataProportionForDistance$distance ~ DataProportionForDistance$ConImprov  )
lmfitInc = lm(  DataProportionForDistance$distance ~ DataProportionForDistance$Case  )
lmfitVal = lm( DataProportionForDistance$distance ~  DataProportionForDistance$Money )
factorCon <- abs(lmfitCon$coefficients[2])
factorInc <- abs(lmfitInc$coefficients[2])
factorVal <- abs(lmfitVal$coefficients[2])
# summary(lmfit)
plot(lmfitCon)
plot( DataProportionForDistance$distance, DataProportionForDistance$ConImprov )
abline(lmfitCon)

plot(DataProportionForDistance$distance, DataProportionForDistance$Con2005  )
plot(DataProportionForDistance$distance, DataProportionForDistance$Con2012  )

DataProportion <- cbind(DataProportion, DataProportion$distance* factorCon + DataProportion$ConImprov)
DataProportion <- cbind(DataProportion, DataProportion$distance* factorInc + DataProportion$Case)
DataProportion <- cbind(DataProportion, DataProportion$distance* factorVal + DataProportion$Money)
colnames(DataProportion)[29:31] <- c("d_Improv", "d_Case", "d_Money")


# proportion & improvement by distance
DataProportion_region1 <- subset(DataProportion, DataProportion$DistanceIndex ==1 ) #within 1mile
DataProportion_region2 <- rbind( subset(DataProportion, DataProportion$DistanceIndex ==2 ), 
                                 DataProportion_region1)  #within 2mile
DataProportion_region3 <- rbind( subset(DataProportion, DataProportion$DistanceIndex ==3 ),
                                 DataProportion_region2)  #within 3mile
DataProportion_region4 <- rbind( subset(DataProportion, DataProportion$DistanceIndex ==4 ),
                                 DataProportion_region3)  #within 4mile

DataProportion_Notregion1 <- subset(DataProportion, DataProportion$DistanceIndex !=1 ) #farther than 1mile
rm(DataProportion_Notregion2)
DataProportion_Notregion2 <- rbind( subset(DataProportion, DataProportion$DistanceIndex ==3 ), 
                                    subset(DataProportion, DataProportion$DistanceIndex ==4 ), 
                                    subset(DataProportion, DataProportion$DistanceIndex ==5 ))  #farther than 2mile
rm(DataProportion_Notregion3)
DataProportion_Notregion3 <-  rbind( subset(DataProportion, DataProportion$DistanceIndex ==4 ), 
                                     subset(DataProportion, DataProportion$DistanceIndex ==5 ))  #farther than 2mile
DataProportion_Notregion4 <-  subset(DataProportion, DataProportion$DistanceIndex ==5 ) #farther than 3mile 

# pop by distance
DataTotal_region1 <- subset(DataTotal, DataTotal$DistanceIndex ==1 ) #within 1mile
DataTotal_region2 <- rbind( subset(DataTotal, DataTotal$DistanceIndex ==2 ), 
                            DataTotal_region1)  #within 2mile
DataTotal_region3 <- rbind( subset(DataTotal, DataTotal$DistanceIndex ==3 ),
                            DataTotal_region2)  #within 3mile
DataTotal_region4 <- rbind( subset(DataTotal, DataTotal$DistanceIndex ==4 ),
                            DataTotal_region3)  #within 4mile

DataTotal_Notregion1 <- subset(DataTotal, DataTotal$DistanceIndex !=1 ) #farther than 1mile
rm(DataTotal_Notregion2)
DataTotal_Notregion2 <- rbind( subset(DataTotal, DataTotal$DistanceIndex ==3 ), 
                               subset(DataTotal, DataTotal$DistanceIndex ==4 ),
                               subset(DataTotal, DataTotal$DistanceIndex ==5 ))  #farther than 2mile
rm(DataTotal_Notregion3)
DataTotal_Notregion3 <-  rbind( subset(DataTotal, DataTotal$DistanceIndex ==4 ), 
                                subset(DataTotal, DataTotal$DistanceIndex ==5 ))  #farther than 2mile
DataTotal_Notregion4 <-  subset(DataTotal, DataTotal$DistanceIndex ==5 ) #farther than 3mile 

##### BY REGION
# DataProportion <- DataProportion_region1 
# DataProportion <- DataProportion_region2 
# DataProportion <- DataProportion_region3 
# DataProportion <- DataProportion_region4 

# DataProportion <- DataProportion_Notregion1 
# DataProportion <- DataProportion_Notregion2 
# DataProportion <- DataProportion_Notregion3 
# DataProportion <- DataProportion_Notregion4 

# 
# DataTotal <- DataTotal_region1
# DataTotal <- DataTotal_region2
# DataTotal <- DataTotal_region3
# DataTotal <- DataTotal_region4

# DataTotal <- DataTotal_Notregion1
# DataTotal <- DataTotal_Notregion2
# DataTotal <- DataTotal_Notregion3
# DataTotal <- DataTotal_Notregion4

## start here
# concentration for ILA
rm(ConProportion)
Imp <- 29
ConProportion <- data.frame (matrix(nrow=length(DataProportion[,1]) , ncol = 23))
ConProportion[,1] <- DataProportion[,1]
ConProportion[,2] <- DataProportion[,Imp] # total
ConProportion[,11] <- DataProportion[,Imp] # total

for (i in 1: length(DataProportion[,1]) ){
  for (j in 1: 8){
    ConProportion[i,2+j] <-  DataProportion[i,1+j] * DataProportion[i,Imp]  # Imp 25 = concentration
  }
  for (j in 1: 12){
    ConProportion[i,11+j] <-  DataProportion[i,9+j] * DataProportion[i,Imp]  # Imp 25 = concentration
  }
}

colnames(ConProportion)[1:23] <- c("GeoID", "total", "C_white", "C_afri", "C_asian" , "C_his" , 
                                   "C_nonwhite", "C_nonafri", "C_nonasian" , "C_nonhis" ,"total",
                                   "C_poor", "C_notpoor",
                                   "C_incg1", "C_incg2", "C_incg3", "C_incg4",
                                   "C_nonincg1", "C_nonincg2", "C_nonincg3", "C_nonincg4",
                                   "C_owner", "C_rent")


# INC for ILA
inc <- 30
IncProportion <- data.frame (matrix(nrow=length(DataProportion[,1]) , ncol = 23))
IncProportion[,1] <- DataProportion[,1] # geoid
IncProportion[,2] <- DataProportion[,inc] # total (for pop)
IncProportion[,11] <- DataProportion[,inc] # total (for hhd)

for (i in 1: length(DataProportion[,1]) ){
  for (j in 1: 8){
    IncProportion[i,2+j] <-  DataProportion[i,1+j] * DataProportion[i,inc]  # inc 26
  }
  
  for (j in 1: 12){
    IncProportion[i,11+j] <-  DataProportion[i,9+j] * DataProportion[i,inc]  # inc 26
  }
}

colnames(IncProportion)[1:23] <- c("GeoID","total", "I_white", "I_afri", "I_asian" , "I_his" , 
                                   "I_nonwhite", "I_nonafri", "I_nonasian" , "I_nonhis" , "total",
                                   "I_poor", "I_notpoor",
                                   "I_incg1", "I_incg2", "I_incg3", "I_incg4",
                                   "I_nonincg1", "I_nonincg2", "I_nonincg3", "I_nonincg4",
                                   "I_owner", "I_rent")

# VAL for ILA
val <- 31
ValProportion <- data.frame (matrix(nrow=length(DataProportion[,1]) , ncol = 23))
ValProportion[,1] <- DataProportion[,1]
ValProportion[,2] <- DataProportion[,val] # total
ValProportion[,11] <- DataProportion[,val] # total (for hhd)

for (i in 1: length(DataProportion[,1]) ){
  for (j in 1: 8){
    ValProportion[i,2+j] <-  DataProportion[i,1+j] * DataProportion[i,val]  # val 27
  }
  for (j in 1: 12){
    ValProportion[i,11+j] <-  DataProportion[i,9+j] * DataProportion[i,val]  # val 27
  }
}
colnames(ValProportion)[1:23] <- c("GeoID", "total","V_white", "V_afri", "V_asian" , "V_his" , 
                                   "V_nonwhite", "V_nonafri", "V_nonasian" , "V_nonhis" , "total",
                                   "V_poor", "V_notpoor",
                                   "V_incg1", "V_incg2", "V_incg3", "V_incg4",
                                   "V_nonincg1", "V_nonincg2", "V_nonincg3", "V_nonincg4",
                                   "V_owner", "V_rent")



# Individual Level Analysis
# population


PopTotal[1,1] <- sum (DataTotal[,2]) # total pop
PopTotal[1,2] <- sum (DataTotal[,4]) # white
PopTotal[1,3] <- sum (DataTotal[,5]) # afri
PopTotal[1,4] <- sum (DataTotal[,6]) # asian
PopTotal[1,5] <- sum (DataTotal[,7]) # his
PopTotal[1,6] <-  PopTotal[1,1]  - PopTotal[1,2] # nonwhite
PopTotal[1,7] <-  PopTotal[1,1]  - PopTotal[1,3] # nonafri
PopTotal[1,8] <-  PopTotal[1,1]  - PopTotal[1,4] # nonasian
PopTotal[1,9] <-  PopTotal[1,1]  - PopTotal[1,5] # nonhis

PopTotal[1,10] <- sum (DataTotal[,8]) # total hhd
PopTotal[1,11] <- sum (DataTotal[,9]) # poor
PopTotal[1,12] <- sum (DataTotal[,10]) # notpoor

PopTotal[1,13] <- sum (DataTotal[,11]) # inc g1
PopTotal[1,14] <- sum (DataTotal[,12]) # inc g2
PopTotal[1,15] <- sum (DataTotal[,13]) # inc g3
PopTotal[1,16] <- sum (DataTotal[,14]) # inc g4  
PopTotal[1,17] <- PopTotal[1,10]  - PopTotal[1,13] # inc g1
PopTotal[1,18] <- PopTotal[1,10]  - PopTotal[1,14] # inc g2
PopTotal[1,19] <- PopTotal[1,10]  - PopTotal[1,15] # inc g3
PopTotal[1,20] <- PopTotal[1,10]  - PopTotal[1,16] # inc g4

PopTotal[1,21] <- sum (DataTotal[,16]) # owner
PopTotal[1,22] <- sum (DataTotal[,17]) # rent

for (i in 1: length(PopTotal)){
  if ( i < 10) {
    PopTotal[2,i] <- PopTotal[1,i] / PopTotal[1,1] 
  }
  else {
    PopTotal[2,i] <- PopTotal[1,i] / PopTotal[1,10] 
  }
}

colnames(PopTotal)[1:22] <- c("TotalPop","white", "afri", "asian" , "his" , 
                              "nonwhite", "nonafri", "nonasian" , "nonhis" ,
                              "TotalHHD", "poor", "notpoor",
                              "incg1", "incg2", "incg3", "incg4",
                              "nonincg1", "nonincg2", "nonincg3", "nonincg4",
                              "owner", "rent")


ILATotal <- data.frame()
for (j in 1: 22) {
  ILATotal[1,j] <- sum (ConProportion[,j+1])
  ILATotal[2,j] <- sum (IncProportion[,j+1])
  ILATotal[3,j] <- sum (ValProportion[,j+1])
}
# colnames(ILATotal)[1:21] <- names(IncProportion) # copy header
colnames(ILATotal)[1:22] <- c("total", "I_white", "I_afri", "I_asian" , "I_his" , 
                              "I_nonwhite", "I_nonafri", "I_nonasian" , "I_nonhis" ,"total",
                              "I_poor", "I_notpoor",
                              "I_incg1", "I_incg2", "I_incg3", "I_incg4",
                              "I_nonincg1", "I_nonincg2", "I_nonincg3", "I_nonincg4",
                              "I_owner", "I_rent")


for (j in 1: length(ILATotal[1,] ) ) {
  ILATotal[4,j] <- ILATotal[1,j] / PopTotal[1,j]
  ILATotal[5,j] <- ILATotal[2,j] / PopTotal[1,j]
  ILATotal[6,j] <- ILATotal[3,j] / PopTotal[1,j]
}


ILARatio <- data.frame()
for (i in 1: 3 ) {
  
  ILARatio[i,1] <-  ILATotal[i+3,2] /  ILATotal[i+3,6] 
  ILARatio[i,2] <-  ILATotal[i+3,3] /  ILATotal[i+3,7] 
  ILARatio[i,3] <-  ILATotal[i+3,4] /  ILATotal[i+3,8] 
  ILARatio[i,4] <-  ILATotal[i+3,5] /  ILATotal[i+3,9] 
  
  ILARatio[i,5] <-  ILATotal[i+3,11] /  ILATotal[i+3,12] 
  
  ILARatio[i,6] <-  ILATotal[i+3,13] /  ILATotal[i+3,17] 
  ILARatio[i,7] <-  ILATotal[i+3,14] /  ILATotal[i+3,18] 
  ILARatio[i,8] <-  ILATotal[i+3,15] /  ILATotal[i+3,19] 
  ILARatio[i,9] <-  ILATotal[i+3,16] /  ILATotal[i+3,20] 
  
  ILARatio[i,10] <-  ILATotal[i+3,21] /  ILATotal[i+3,22]
  
}


colnames(ILARatio)[1:10] <- c("R_white", "R_afri", "R_asian" , "R_his" , 
                              "R_poor", 
                              "R_incg1", "R_incg2", "R_incg3", "R_incg4",
                              "R_owner")



# t test
ILATtest <- data.frame()
ILATtest[1,1] <- t.test(ConProportion[,3], ConProportion[,7])$p.value
ILATtest[1,2] <- t.test(ConProportion[,4], ConProportion[,8])$p.value
ILATtest[1,3] <- t.test(ConProportion[,5], ConProportion[,9])$p.value
ILATtest[1,4] <- t.test(ConProportion[,6], ConProportion[,10])$p.value

ILATtest[1,5] <- t.test(ConProportion[,11], ConProportion[,12])$p.value

ILATtest[1,6] <- t.test(ConProportion[,13], ConProportion[,17])$p.value
ILATtest[1,7] <- t.test(ConProportion[,14], ConProportion[,18])$p.value
ILATtest[1,8] <- t.test(ConProportion[,15], ConProportion[,19])$p.value
ILATtest[1,9] <- t.test(ConProportion[,16], ConProportion[,20])$p.value

ILATtest[1,10] <- t.test(ConProportion[,21], ConProportion[,22])$p.value




# save
write.table(ILATotal,"C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result/ILATotal_d_region1.txt",
            sep="\t",row.names=FALSE)
write.table(ILARatio,"C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result/ILARatio_d_region2.txt",
            sep="\t",row.names=FALSE)
write.table(PopTotal,"C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result/PopTotal_d_region3.txt",
            sep="\t",row.names=FALSE)


write.table(DataProportion,"C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result/DataProportion.txt",
            sep="\t",row.names=FALSE)


## end



plot(Xgroup2, col="red" )
par(new=TRUE)
plot(Xgroup3,  col="green", add=TRUE)


# # piecewise regression
# install.packages("segmented")
# library("segmented")
# lmfitCon.seg<-segmented(lmfitCon, seg.Z=~DataProportionForDistance$ConImprov, psi=NA )
# 
# install.packages("SiZer")
# library(SiZer) # load package with reference to Toms and Lesperance (2003)
# pw.model <- piecewise.linear(DataProportionForDistance$distance, DataProportionForDistance$ConImprov, middle=1, CI=TRUE,
#                               sig.level = 0.05)
# 
# plot(pw.model)
# print(pw.model)
# 
# 
# # correlation
# library(Hmisc)
# xcorall <- cbind(  DataProportionForDistance$distance , DataProportionForDistance$Con2005)
# corall <- rcorr(xcorall, type="pearson")
# 
# xcorregion1 <- cbind(  DataProportion_region1 $distance ,DataProportion_region1 $Con2005)
# corregion1 <- rcorr(xcorregion1, type="pearson")
# xcorNotregion1 <- cbind(  DataProportion_Notregion1 $distance ,DataProportion_Notregion1 $Con2005)
# corNotregion1 <- rcorr(xcorNotregion1, type="pearson")
# 
# xcorregion2 <- cbind(  DataProportion_region2 $distance ,DataProportion_region2 $Con2005)
# corregion2 <- rcorr(xcorregion2, type="pearson")
# xcorNotregion2 <- cbind(  DataProportion_Notregion2 $distance ,DataProportion_Notregion2 $Con2005)
# corNotregion2 <- rcorr(xcorNotregion2, type="pearson")
# 
# xcorregion3 <- cbind(  DataProportion_region3 $distance ,DataProportion_region3 $Con2005)
# corregion3 <- rcorr(xcorregion3, type="pearson")
# xcorNotregion3 <- cbind(  DataProportion_Notregion3 $distance ,DataProportion_Notregion3 $Con2005)
# corNotregion3 <- rcorr(xcorNotregion3, type="pearson")
# 
# xcorregion4 <- cbind(  DataProportion_region4 $distance ,DataProportion_region4 $Con2005)
# corregion4 <- rcorr(xcorregion4, type="pearson")
# xcorNotregion4 <- cbind(  DataProportion_Notregion4 $distance ,DataProportion_Notregion4 $Con2005)
# corNotregion4 <- rcorr(xcorNotregion4, type="pearson")
# 
# xcorNotregion5_temp <- subset(DataProportion, DataProportion$distance > 15.0)
# xcorNotregion5 <- cbind(xcorNotregion5_temp$distance, xcorNotregion5_temp$Con2005 )
# corNotregion5 <- rcorr(xcorNotregion5, type="pearson")
# lmfit2 = lm( y  ~ poly(x, 2, raw=TRUE))
# summary(lmfit2)
# plot(lmfit2)
#  
# lmfit3 = lm( y  ~ log(x))
# summary(lmfit3)
# plot(lmfit3)
# 
# lmfit4 = lm( y ~ (1/x) )
# summary(lmfit4)
# plot( x , y  )
# abline(lmfit4)
# 
# logEstimate = lm( exp (y) ~ x)
# plot( y , x  )
# abline(logEstimate)
