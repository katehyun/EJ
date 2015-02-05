rm(list=ls())
options(scipen=999) 

setwd("C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ")

DataTotal=read.table("./DataFinal_totalonly.txt", header=T, fill=T)
DataProportion=read.table("./DataFinal_proportiononly.txt", header=T, fill=T)
distance=read.table("./DistanceforR.txt",  header=T)

DataProportion <- cbind( DataProportion, distance[ match(DataProportion[,1], distance[,1]), 6])
DataTotal <- cbind(DataTotal, distance[ match(DataTotal[,1], distance[,1]), 6]) 
colnames(DataProportion)[length(DataProportion[1,])] <- "distance"
colnames(DataTotal)[length(DataTotal[1,])] <- "distance"


# DataTotal

DataTotal_distance1 <- subset(DataTotal, DataTotal$distance < 0.25)
DataTotal_distance2 <- subset(DataTotal, DataTotal$distance < 1)
DataTotal_distance3 <- subset(DataTotal, DataTotal$distance < 2)
DataTotal_distance4 <- subset(DataTotal, DataTotal$distance < 4)
DataTotal_distance5 <- subset(DataTotal, DataTotal$cluster3 >= 4)



# DataTotal <- DataTotal_fit2_impact1
# DataTotal <- DataTotal_fit2_impact2
# DataTotal <- DataTotal_fit3_impact1
# DataTotal <- DataTotal_fit3_impact2
# DataTotal <- DataTotal_fit3_impact3
# DataTotal <- rbind(DataTotal_fit3_impact2, DataTotal_fit3_impact3)


# Con Total

# concentration for ILA
rm(Con2005Total)
Imp2005 <- 26
Con2005Total <- data.frame (matrix(nrow=length(DataTotal[,1]) , ncol = 24))
Con2005Total[,1] <- DataTotal[,1]
Con2005Total[,2] <- DataTotal[,Imp2005 ] # total


for (i in 1: length(DataTotal[,1]) ){
  for (j in 1: 23){
    Con2005Total[i,2+j] <-  DataTotal[i,1+j] * DataTotal[i,Imp2005]  # Imp 25 = concentration
  }
}

colnames(Con2005Total)[1:25] <- c("GeoID", "con", "totalPop", "C_white", "C_afri", "C_asian" , "C_his" , 
                                  "C_nonwhite", "C_nonafri", "C_nonasian" , "C_nonhis" ,"totalHHD",
                                  "C_poor", "C_notpoor",
                                  "C_incg1", "C_incg2", "C_incg3", "C_incg4",
                                  "C_nonincg1", "C_nonincg2", "C_nonincg3", "C_nonincg4","income",
                                  "C_owner", "C_rent")


# concentration for ILA
rm(Con2012Total)
Imp2012 <- 27
Con2012Total <- data.frame (matrix(nrow=length(DataTotal[,1]) , ncol = 24))
Con2012Total[,1] <- DataTotal[,1]
Con2012Total[,2] <- DataTotal[,Imp2012 ] # total


for (i in 1: length(DataTotal[,1]) ){
  for (j in 1: 23){
    Con2012Total[i,2+j] <-  DataTotal[i,1+j] * DataTotal[i,Imp2012]  # Imp 25 = concentration
  }
  
}

colnames(Con2012Total)[1:25] <- c("GeoID", "con", "total", "C_white", "C_afri", "C_asian" , "C_his" , 
                                  "C_nonwhite", "C_nonafri", "C_nonasian" , "C_nonhis" ,"total",
                                  "C_poor", "C_notpoor",
                                  "C_incg1", "C_incg2", "C_incg3", "C_incg4",
                                  "C_nonincg1", "C_nonincg2", "C_nonincg3", "C_nonincg4","income",
                                  "C_owner", "C_rent")

# concentration for ILA
rm(ConTotal)
Imp <- 28
ConTotal <- data.frame (matrix(nrow=length(DataTotal[,1]) , ncol = 24))
ConTotal[,1] <- DataTotal[,1]
ConTotal[,2] <- DataTotal[,Imp ] # total


for (i in 1: length(DataTotal[,1]) ){
  for (j in 1: 23){
    ConTotal[i,2+j] <-  DataTotal[i,1+j] * DataTotal[i,Imp]  # Imp 25 = concentration
  }
}

colnames(ConTotal)[1:25] <- c("GeoID", "con", "total", "C_white", "C_afri", "C_asian" , "C_his" , 
                              "C_nonwhite", "C_nonafri", "C_nonasian" , "C_nonhis" ,"total",
                              "C_poor", "C_notpoor",
                              "C_incg1", "C_incg2", "C_incg3", "C_incg4",
                              "C_nonincg1", "C_nonincg2", "C_nonincg3", "C_nonincg4","income",
                              "C_owner", "C_rent")

# Inc Total for ILA
rm(IncTotal)
Inc <- 29
IncTotal <- data.frame (matrix(nrow=length(DataTotal[,1]) , ncol = 24))
IncTotal[,1] <- DataTotal[,1]
IncTotal[,2] <- DataTotal[,Inc ] # total


for (i in 1: length(DataTotal[,1]) ){
  for (j in 1: 23){
    IncTotal[i,2+j] <-  DataTotal[i,1+j] * DataTotal[i,Inc]  # Inc 28 
  }
}

colnames(IncTotal)[1:25] <- c("GeoID", "con", "total", "I_white", "I_afri", "I_asian" , "I_his" , 
                              "I_nonwhite", "I_nonafri", "I_nonasian" , "I_nonhis" ,"total",
                              "I_poor", "I_notpoor",
                              "I_incg1", "I_incg2", "I_incg3", "I_incg4",
                              "I_nonincg1", "I_nonincg2", "I_nonincg3", "I_nonincg4","income",
                              "I_owner", "I_rent")

# Val Total for ILA
rm(ValTotal)
Val <- 30
ValTotal <- data.frame (matrix(nrow=length(DataTotal[,1]) , ncol = 24))
ValTotal[,1] <- DataTotal[,1]
ValTotal[,2] <- DataTotal[,Val ] # total

for (i in 1: length(DataTotal[,1]) ){
  for (j in 1: 23){
    ValTotal[i,2+j] <-  DataTotal[i,1+j] * DataTotal[i,Val]  # Val 28 
  }
}

colnames(ValTotal)[1:25] <- c("GeoID", "con", "total", "V_white", "V_afri", "V_asian" , "V_his" , 
                              "V_nonwhite", "V_nonafri", "V_nonasian" , "V_nonhis" ,"total",
                              "V_poor", "V_notpoor",
                              "V_incg1", "V_incg2", "V_incg3", "V_incg4",
                              "V_nonincg1", "V_nonincg2", "V_nonincg3", "V_nonincg4","income",
                              "V_owner", "V_rent")



PopTotal <- data.frame()
for (i in 1: 24){
  PopTotal[1,i] <- sum (DataTotal[,i])
}



colnames(PopTotal)[1:24] <- c("NAN", "TotalPop",
                              "white", "afri", "asian" , "his" , 
                              "nonwhite", "nonafri", "nonasian" , "nonhis" ,
                              "TotalHHD", 
                              "poor", "notpoor",
                              "incg1", "incg2", "incg3", "incg4",
                              "nonincg1", "nonincg2", "nonincg3", "nonincg4", "totalinc",
                              "owner", "rent")


ILATotal <- data.frame()
for (j in 1: 24) {
  ILATotal[1,j] <- sum (Con2005Total[,j+1])
  ILATotal[2,j] <- sum (Con2012Total[,j+1])
  ILATotal[3,j] <- sum (ConTotal[,j+1])
  ILATotal[4,j] <- sum (IncTotal[,j+1])
  ILATotal[5,j] <- sum (ValTotal[,j+1])
}
# colnames(ILATotal)[1:21] <- names(IncProportion) # copy header
colnames(ILATotal)[1:24] <- c( "con",  "totalPop", "I_white", "I_afri", "I_asian" , "I_his" , 
                               "I_nonwhite", "I_nonafri", "I_nonasian" , "I_nonhis" ,"totalHHD",
                               "I_poor", "I_notpoor",
                               "I_incg1", "I_incg2", "I_incg3", "I_incg4",
                               "I_nonincg1", "I_nonincg2", "I_nonincg3", "I_nonincg4", "totalinc",
                               "I_owner", "I_rent")


for (j in 3: length(ILATotal[1,] ) ) {
  ILATotal[6,j] <- ILATotal[1,j] / sum(PopTotal[1,j])
  ILATotal[7,j] <- ILATotal[2,j] / sum(PopTotal[1,j])
  ILATotal[8,j] <- ILATotal[3,j] / sum(PopTotal[1,j])
  ILATotal[9,j] <- ILATotal[4,j] / sum(PopTotal[1,j])
  ILATotal[10,j] <- ILATotal[5,j] / sum(PopTotal[1,j])
}


ILARatio <- data.frame()
for (i in 1: 10 ) {
  
  if (i < 6){
    ILARatio[i,1] <-  ILATotal[i,3] /  ILATotal[i,2] 
    ILARatio[i,2] <-  ILATotal[i,4] /  ILATotal[i,2] 
    ILARatio[i,3] <-  ILATotal[i,5] /  ILATotal[i,2] 
    ILARatio[i,4] <-  ILATotal[i,6] /  ILATotal[i,2] 
    
    ILARatio[i,5] <-  ILATotal[i,12] /  ILATotal[i,11] 
    
    ILARatio[i,6] <-  ILATotal[i,13] /  ILATotal[i,11] 
    ILARatio[i,7] <-  ILATotal[i,14] /  ILATotal[i,11] 
    ILARatio[i,8] <-  ILATotal[i,15] /  ILATotal[i,11] 
    ILARatio[i,9] <-  ILATotal[i,16] /  ILATotal[i,11] 
    ILARatio[i,10] <-  ILATotal[i,23] /  ILATotal[i,11]
    
  }
  else{ 
    ILARatio[i,1] <-  ILATotal[i,3] /  ILATotal[i,7] 
    ILARatio[i,2] <-  ILATotal[i,4] /  ILATotal[i,8] 
    ILARatio[i,3] <-  ILATotal[i,5] /  ILATotal[i,9] 
    ILARatio[i,4] <-  ILATotal[i,6] /  ILATotal[i,10] 
    
    ILARatio[i,5] <-  ILATotal[i,12] /  ILATotal[i,13] 
    
    ILARatio[i,6] <-  ILATotal[i,14] /  ILATotal[i,18] 
    ILARatio[i,7] <-  ILATotal[i,15] /  ILATotal[i,19] 
    ILARatio[i,8] <-  ILATotal[i,16] /  ILATotal[i,20] 
    ILARatio[i,9] <-  ILATotal[i,17] /  ILATotal[i,21] 
    ILARatio[i,10] <-  ILATotal[i,23] /  ILATotal[i,24]
  }
  
  
}


colnames(ILARatio)[1:10] <- c( "R_white", "R_afri", "R_asian" , "R_his" , 
                               "R_poor", 
                               "R_incg1", "R_incg2", "R_incg3", "R_incg4",
                               "R_owner")



# pop_proportion
ILARatio[11,1] <- PopTotal$white / PopTotal$TotalPop
ILARatio[11,2] <- PopTotal$afri / PopTotal$TotalPop
ILARatio[11,3] <- PopTotal$asian / PopTotal$TotalPop
ILARatio[11,4] <- PopTotal$his / PopTotal$TotalPop

ILARatio[11,5] <- PopTotal$poor / PopTotal$TotalHHD
ILARatio[11,6] <- PopTotal$incg1 / PopTotal$TotalHHD
ILARatio[11,7] <- PopTotal$incg2 / PopTotal$TotalHHD
ILARatio[11,8] <- PopTotal$incg3 / PopTotal$TotalHHD
ILARatio[11,9] <- PopTotal$incg4 / PopTotal$TotalHHD

ILARatio[11,10] <- PopTotal$owner / PopTotal$TotalHHD

# pop ratio
ILARatio[12,1] <- PopTotal$white / PopTotal$nonwhite
ILARatio[12,2] <- PopTotal$afri / PopTotal$nonafri
ILARatio[12,3] <- PopTotal$asian / PopTotal$nonasian
ILARatio[12,4] <- PopTotal$his / PopTotal$nonhis

ILARatio[12,5] <- PopTotal$poor / PopTotal$notpoor

ILARatio[12,6] <- PopTotal$incg1 / PopTotal$nonincg1
ILARatio[12,7] <- PopTotal$incg2 / PopTotal$nonincg2
ILARatio[12,8] <- PopTotal$incg3 / PopTotal$nonincg3
ILARatio[12,9] <- PopTotal$incg4 / PopTotal$nonincg4

ILARatio[12,10] <- PopTotal$owner / PopTotal$rent


rownames(ILARatio)[1:12] <- c( "popburden_05", "popburden_12", "popburden_imp" , "popburden_Inc" ,  "popburden_Val" , 
                               "ratio_05", "ratio_12", "ratio_imp" , "ratio_Inc" ,  "ratio_Val" , 
                               "popproportion" , "popratio")


# t test
ILATtest <- data.frame()

ILATtest[1,1] <- t.test(ConTotal[,4], ConTotal[,8])$p.value
ILATtest[1,2] <- t.test(ConTotal[,5], ConTotal[,9])$p.value
ILATtest[1,3] <- t.test(ConTotal[,6], ConTotal[,10])$p.value
ILATtest[1,4] <- t.test(ConTotal[,7], ConTotal[,11])$p.value

ILATtest[1,5] <- t.test(ConTotal[,13], ConTotal[,14])$p.value

ILATtest[1,6] <- t.test(ConTotal[,15], ConTotal[,19])$p.value
ILATtest[1,7] <- t.test(ConTotal[,16], ConTotal[,20])$p.value
ILATtest[1,8] <- t.test(ConTotal[,17], ConTotal[,21])$p.value
ILATtest[1,9] <- t.test(ConTotal[,18], ConTotal[,22])$p.value

ILATtest[1,10] <- t.test(ConTotal[,23], ConTotal[,24])$p.value


write.table(ILATotal,"C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result2_02042015/ILATotal_fit2_region2.txt",
            sep="\t",row.names=FALSE)
write.table(ILARatio,"C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result2_02042015/ILARatio_fit2_region2.txt",
            sep="\t",row.names=FALSE)
write.table(PopTotal,"C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result2_02042015/PopTotal_fit2_region2.txt",
            sep="\t",row.names=FALSE)

write.table(DataTotal,"C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result2_02042015/DataTotal.txt",
            sep="\t",row.names=FALSE)
