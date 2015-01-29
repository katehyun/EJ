rm(list=ls())
options(scipen=999) 

setwd("C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ")

DataProportion=read.table("./DataFinal_proportiononly.txt", header=T, fill=T)
DataTotal=read.table("./DataFinal_totalonly.txt", header=T, fill=T)


DataProportion_region1 <- subset(DataProportion, DataProportion$DistanceIndex ==1 )
DataProportion_region2 <- rbind( subset(DataProportion, DataProportion$DistanceIndex ==1 ), 
                                 DataProportion_region1) 
DataProportion_region3 <- rbind( subset(DataProportion, DataProportion$DistanceIndex ==1 ),
                                 DataProportion_region2)
DataProportion_region4 <- rbind( subset(DataProportion, DataProportion$DistanceIndex ==1 ),
                                 DataProportion_region3)


#DataProportion <- DataProportion_region1 
#DataProportion <- DataProportion_region2 
#DataProportion <- DataProportion_region3 
#DataProportion <- DataProportion_region4 

# NLI index
for (i in 1: length(DataProportion[,1]) ){
  
    DataProportion[i,28] <- which.max ( DataProportion[i,2:5]) # demo (white, afri, asian, his)
    DataProportion[i,29] <- which.max ( DataProportion[i,10:11]) # poor (poor, notpoor)
    DataProportion[i,30] <- which.max ( DataProportion[i,12:15]) # income
    DataProportion[i,31] <- which.max ( DataProportion[i,20:21]) # rent (owner, rent)
    
}

colnames(DataProportion)[28:31] <- c("demo", "poverty", "income" , "rent")

# concentration for ILA
rm(ConProportion)
ConProportion <- data.frame (matrix(nrow=length(DataProportion[,1]) , ncol = 21))
ConProportion[,1] <- DataProportion[,1]

for (i in 1: length(DataProportion[,1]) ){
  for (j in 1: 20){
      ConProportion[i,1+j] <-  DataProportion[i,1+j] * DataProportion[i,25]  
  }
}

colnames(ConProportion)[1:21] <- c("GeoID", "C_white", "C_afri", "C_asian" , "C_his" , 
                                     "C_nonwhite", "C_nonafri", "C_nonasian" , "C_nonhis" ,
                                     "C_poor", "C_notpoor",
                                     "C_incg1", "C_incg2", "C_incg3", "C_incg4",
                                     "C_nonincg1", "C_nonincg2", "C_nonincg3", "C_nonincg4",
                                     "C_owner", "C_rent")


# INC for ILA
IncProportion <- data.frame (matrix(nrow=length(DataProportion[,1]) , ncol = 21))
IncProportion[,1] <- DataProportion[,1]

for (i in 1: length(DataProportion[,1]) ){
  for (j in 1: 20){
    IncProportion[i,1+j] <-  DataProportion[i,1+j] * DataProportion[i,26]  
  }
}
colnames(IncProportion)[1:21] <- c("GeoID","I_white", "I_afri", "I_asian" , "I_his" , 
                                   "I_nonwhite", "I_nonafri", "I_nonasian" , "I_nonhis" ,
                                   "I_poor", "I_notpoor",
                                   "I_incg1", "I_incg2", "I_incg3", "I_incg4",
                                   "I_nonincg1", "I_nonincg2", "I_nonincg3", "I_nonincg4",
                                   "I_owner", "I_rent")

# VAL for ILA
ValProportion <- data.frame (matrix(nrow=length(DataProportion[,1]) , ncol = 21))
ValProportion[,1] <- DataProportion[,1]
for (i in 1: length(DataProportion[,1]) ){
  for (j in 1: 20){
    ValProportion[i,1+j] <-  DataProportion[i,1+j] * DataProportion[i,27]  
  }
}
colnames(ValProportion)[1:21] <- c("GeoID","V_white", "V_afri", "V_asian" , "V_his" , 
                                     "V_nonwhite", "V_nonafri", "V_nonasian" , "V_nonhis" , 
                                     "V_poor", "V_notpoor",
                                     "V_incg1", "V_incg2", "V_incg3", "V_incg4",
                                     "V_nonincg1", "V_nonincg2", "V_nonincg3", "V_nonincg4",
                                     "V_owner", "V_rent")



# Individual Level Analysis
# population
PopTotal <- data.frame()
for (j in 1: length(DataTotal[1,] ) ) {
  PopTotal[1,j] <- sum (DataTotal[,j])
}

colnames(PopTotal)[1:21] <- names(DataTotal) # copy header

ILATotal <- data.frame()
for (j in 1: length(IncProportion[1,] ) ) {
  ILATotal[1,j] <- sum (ConProportion[,j])
  ILATotal[2,j] <- sum (IncProportion[,j])
  ILATotal[3,j] <- sum (ValProportion[,j])
}
colnames(ILATotal)[1:21] <- names(IncProportion) # copy header

#ILI - by regions 1~4





