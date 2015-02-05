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

#NLI index
for (i in 1: length(DataProportion[,1]) ){
  
  DataProportion[i,29] <- which.max ( DataProportion[i,2:5]) # demo (white, afri, asian, his)
  DataProportion[i,30] <- which.max ( DataProportion[i,10:11]) # poor (poor, notpoor)
  DataProportion[i,31] <- which.max ( DataProportion[i,12:15]) # income
  DataProportion[i,32] <- which.max ( DataProportion[i,20:21]) # rent (owner, rent)
  
}

colnames(DataProportion)[29:32] <- c("demo", "poverty", "income" , "rent")

DataTotal <- cbind(DataTotal, DataProportion[,29][ match( DataProportion[,1], DataTotal[,1])] )
DataTotal <- cbind(DataTotal, DataProportion[,30][ match( DataProportion[,1], DataTotal[,1])] )
DataTotal <- cbind(DataTotal, DataProportion[,31][ match( DataProportion[,1], DataTotal[,1])] )
DataTotal <- cbind(DataTotal, DataProportion[,32][ match( DataProportion[,1], DataTotal[,1])] )
colnames(DataTotal)[32:35] <- c("demo", "poverty", "income" , "rent")


DataTotal_fit2_impact1  <- read.table( "./Result2_02042015/Impactbased_ILA/DataTotal_fit2_impact1.txt", header=T, fill=T)
DataTotal_fit2_impact2  <- read.table( "./Result2_02042015/Impactbased_ILA/DataTotal_fit2_impact2.txt", header=T, fill=T)
DataTotal_fit3_impact1  <- read.table( "./Result2_02042015/Impactbased_ILA/DataTotal_fit3_impact1.txt", header=T, fill=T)
DataTotal_fit3_impact2  <- read.table( "./Result2_02042015/Impactbased_ILA/DataTotal_fit3_impact2.txt", header=T, fill=T)
DataTotal_fit3_impact3  <- read.table( "./Result2_02042015/Impactbased_ILA/DataTotal_fit3_impact3.txt", header=T, fill=T)

DataTotal_distance1 <- read.table( "./Result2_02042015/Distancebased_ILA/DataTotal_distance1.txt", header=T, fill=T)
DataTotal_distanceNot1 <- read.table( "./Result2_02042015/Distancebased_ILA/DataTotal_distanceNot1.txt", header=T, fill=T)
DataTotal_distance2 <- read.table( "./Result2_02042015/Distancebased_ILA/DataTotal_distance2.txt", header=T, fill=T)
DataTotal_distanceNot2 <- read.table( "./Result2_02042015/Distancebased_ILA/DataTotal_distanceNot2.txt", header=T, fill=T)
DataTotal_distance3 <- read.table( "./Result2_02042015/Distancebased_ILA/DataTotal_distance3.txt", header=T, fill=T)
DataTotal_distanceNot3 <- read.table( "./Result2_02042015/Distancebased_ILA/DataTotal_distanceNot3.txt", header=T, fill=T)
DataTotal_distance4 <- read.table( "./Result2_02042015/Distancebased_ILA/DataTotal_distance4.txt", header=T, fill=T)
DataTotal_distanceNot4 <- read.table( "./Result2_02042015/Distancebased_ILA/DataTotal_distanceNot4.txt", header=T, fill=T)

# dominant neighborhood index
# impactbased
DataTotal_fit2_impact1 <- cbind( DataTotal_fit2_impact1, DataTotal[,32][ match( DataTotal_fit2_impact1[,1], DataTotal[,1] )])
DataTotal_fit2_impact1 <- cbind( DataTotal_fit2_impact1, DataTotal[,33][ match( DataTotal_fit2_impact1[,1], DataTotal[,1] )])
DataTotal_fit2_impact1 <- cbind( DataTotal_fit2_impact1, DataTotal[,34][ match( DataTotal_fit2_impact1[,1], DataTotal[,1] )])
DataTotal_fit2_impact1 <- cbind( DataTotal_fit2_impact1, DataTotal[,35][ match( DataTotal_fit2_impact1[,1], DataTotal[,1] )])
colnames(DataTotal_fit2_impact1)[34:37] <- c("demo", "poverty", "income" , "rent")

DataTotal_fit2_impact2 <- cbind( DataTotal_fit2_impact2, DataTotal[,32][ match( DataTotal_fit2_impact2[,1], DataTotal[,1] )])
DataTotal_fit2_impact2 <- cbind( DataTotal_fit2_impact2, DataTotal[,33][ match( DataTotal_fit2_impact2[,1], DataTotal[,1] )])
DataTotal_fit2_impact2 <- cbind( DataTotal_fit2_impact2, DataTotal[,34][ match( DataTotal_fit2_impact2[,1], DataTotal[,1] )])
DataTotal_fit2_impact2 <- cbind( DataTotal_fit2_impact2, DataTotal[,35][ match( DataTotal_fit2_impact2[,1], DataTotal[,1] )])
colnames(DataTotal_fit2_impact2)[34:37] <- c("demo", "poverty", "income" , "rent")

DataTotal_fit3_impact1 <- cbind( DataTotal_fit3_impact1, DataTotal[,32][ match( DataTotal_fit3_impact1[,1], DataTotal[,1] )])
DataTotal_fit3_impact1 <- cbind( DataTotal_fit3_impact1, DataTotal[,33][ match( DataTotal_fit3_impact1[,1], DataTotal[,1] )])
DataTotal_fit3_impact1 <- cbind( DataTotal_fit3_impact1, DataTotal[,34][ match( DataTotal_fit3_impact1[,1], DataTotal[,1] )])
DataTotal_fit3_impact1 <- cbind( DataTotal_fit3_impact1, DataTotal[,35][ match( DataTotal_fit3_impact1[,1], DataTotal[,1] )])
colnames(DataTotal_fit3_impact1)[34:37] <- c("demo", "poverty", "income" , "rent")

DataTotal_fit3_impact2 <- cbind( DataTotal_fit3_impact2, DataTotal[,32][ match( DataTotal_fit3_impact2[,1], DataTotal[,1] )])
DataTotal_fit3_impact2 <- cbind( DataTotal_fit3_impact2, DataTotal[,33][ match( DataTotal_fit3_impact2[,1], DataTotal[,1] )])
DataTotal_fit3_impact2 <- cbind( DataTotal_fit3_impact2, DataTotal[,34][ match( DataTotal_fit3_impact2[,1], DataTotal[,1] )])
DataTotal_fit3_impact2 <- cbind( DataTotal_fit3_impact2, DataTotal[,35][ match( DataTotal_fit3_impact2[,1], DataTotal[,1] )])
colnames(DataTotal_fit3_impact2)[34:37] <- c("demo", "poverty", "income" , "rent")

DataTotal_fit3_impact3 <- cbind( DataTotal_fit3_impact3, DataTotal[,32][ match( DataTotal_fit3_impact3[,1], DataTotal[,1] )])
DataTotal_fit3_impact3 <- cbind( DataTotal_fit3_impact3, DataTotal[,33][ match( DataTotal_fit3_impact3[,1], DataTotal[,1] )])
DataTotal_fit3_impact3 <- cbind( DataTotal_fit3_impact3, DataTotal[,34][ match( DataTotal_fit3_impact3[,1], DataTotal[,1] )])
DataTotal_fit3_impact3 <- cbind( DataTotal_fit3_impact3, DataTotal[,35][ match( DataTotal_fit3_impact3[,1], DataTotal[,1] )])
colnames(DataTotal_fit3_impact3)[34:37] <- c("demo", "poverty", "income" , "rent")

#  distancebased
DataTotal_distance1 <- cbind( DataTotal_distance1, DataTotal[,32][ match( DataTotal_distance1[,1], DataTotal[,1] )])
DataTotal_distance1 <- cbind( DataTotal_distance1, DataTotal[,33][ match( DataTotal_distance1[,1], DataTotal[,1] )])
DataTotal_distance1 <- cbind( DataTotal_distance1, DataTotal[,34][ match( DataTotal_distance1[,1], DataTotal[,1] )])
DataTotal_distance1 <- cbind( DataTotal_distance1, DataTotal[,35][ match( DataTotal_distance1[,1], DataTotal[,1] )])
colnames(DataTotal_distance1)[32:35] <- c("demo", "poverty", "income" , "rent")

DataTotal_distanceNot1 <- cbind( DataTotal_distanceNot1, DataTotal[,32][ match( DataTotal_distanceNot1[,1], DataTotal[,1] )])
DataTotal_distanceNot1 <- cbind( DataTotal_distanceNot1, DataTotal[,33][ match( DataTotal_distanceNot1[,1], DataTotal[,1] )])
DataTotal_distanceNot1 <- cbind( DataTotal_distanceNot1, DataTotal[,34][ match( DataTotal_distanceNot1[,1], DataTotal[,1] )])
DataTotal_distanceNot1 <- cbind( DataTotal_distanceNot1, DataTotal[,35][ match( DataTotal_distanceNot1[,1], DataTotal[,1] )])
colnames(DataTotal_distanceNot1)[32:35] <- c("demo", "poverty", "income" , "rent")

DataTotal_distance2 <- cbind( DataTotal_distance2, DataTotal[,32][ match( DataTotal_distance2[,1], DataTotal[,1] )])
DataTotal_distance2 <- cbind( DataTotal_distance2, DataTotal[,33][ match( DataTotal_distance2[,1], DataTotal[,1] )])
DataTotal_distance2 <- cbind( DataTotal_distance2, DataTotal[,34][ match( DataTotal_distance2[,1], DataTotal[,1] )])
DataTotal_distance2 <- cbind( DataTotal_distance2, DataTotal[,35][ match( DataTotal_distance2[,1], DataTotal[,1] )])
colnames(DataTotal_distance2)[32:35] <- c("demo", "poverty", "income" , "rent")

DataTotal_distanceNot2 <- cbind( DataTotal_distanceNot2, DataTotal[,32][ match( DataTotal_distanceNot2[,1], DataTotal[,1] )])
DataTotal_distanceNot2 <- cbind( DataTotal_distanceNot2, DataTotal[,33][ match( DataTotal_distanceNot2[,1], DataTotal[,1] )])
DataTotal_distanceNot2 <- cbind( DataTotal_distanceNot2, DataTotal[,34][ match( DataTotal_distanceNot2[,1], DataTotal[,1] )])
DataTotal_distanceNot2 <- cbind( DataTotal_distanceNot2, DataTotal[,35][ match( DataTotal_distanceNot2[,1], DataTotal[,1] )])
colnames(DataTotal_distanceNot2)[32:35] <- c("demo", "poverty", "income" , "rent")

DataTotal_distance3 <- cbind( DataTotal_distance3, DataTotal[,32][ match( DataTotal_distance3[,1], DataTotal[,1] )])
DataTotal_distance3 <- cbind( DataTotal_distance3, DataTotal[,33][ match( DataTotal_distance3[,1], DataTotal[,1] )])
DataTotal_distance3 <- cbind( DataTotal_distance3, DataTotal[,34][ match( DataTotal_distance3[,1], DataTotal[,1] )])
DataTotal_distance3 <- cbind( DataTotal_distance3, DataTotal[,35][ match( DataTotal_distance3[,1], DataTotal[,1] )])
colnames(DataTotal_distance3)[32:35] <- c("demo", "poverty", "income" , "rent")

DataTotal_distanceNot3 <- cbind( DataTotal_distanceNot3, DataTotal[,32][ match( DataTotal_distanceNot3[,1], DataTotal[,1] )])
DataTotal_distanceNot3 <- cbind( DataTotal_distanceNot3, DataTotal[,33][ match( DataTotal_distanceNot3[,1], DataTotal[,1] )])
DataTotal_distanceNot3 <- cbind( DataTotal_distanceNot3, DataTotal[,34][ match( DataTotal_distanceNot3[,1], DataTotal[,1] )])
DataTotal_distanceNot3 <- cbind( DataTotal_distanceNot3, DataTotal[,35][ match( DataTotal_distanceNot3[,1], DataTotal[,1] )])
colnames(DataTotal_distanceNot3)[32:35] <- c("demo", "poverty", "income" , "rent")

DataTotal_distance4 <- cbind( DataTotal_distance4, DataTotal[,32][ match( DataTotal_distance4[,1], DataTotal[,1] )])
DataTotal_distance4 <- cbind( DataTotal_distance4, DataTotal[,33][ match( DataTotal_distance4[,1], DataTotal[,1] )])
DataTotal_distance4 <- cbind( DataTotal_distance4, DataTotal[,34][ match( DataTotal_distance4[,1], DataTotal[,1] )])
DataTotal_distance4 <- cbind( DataTotal_distance4, DataTotal[,35][ match( DataTotal_distance4[,1], DataTotal[,1] )])
colnames(DataTotal_distance4)[32:35] <- c("demo", "poverty", "income" , "rent")

DataTotal_distanceNot4 <- cbind( DataTotal_distanceNot4, DataTotal[,32][ match( DataTotal_distanceNot4[,1], DataTotal[,1] )])
DataTotal_distanceNot4 <- cbind( DataTotal_distanceNot4, DataTotal[,33][ match( DataTotal_distanceNot4[,1], DataTotal[,1] )])
DataTotal_distanceNot4 <- cbind( DataTotal_distanceNot4, DataTotal[,34][ match( DataTotal_distanceNot4[,1], DataTotal[,1] )])
DataTotal_distanceNot4 <- cbind( DataTotal_distanceNot4, DataTotal[,35][ match( DataTotal_distanceNot4[,1], DataTotal[,1] )])
colnames(DataTotal_distanceNot4)[32:35] <- c("demo", "poverty", "income" , "rent")


f.impactratio <- function (data, data_pair){
  
  ratio[1,1] <- (sum(data$demo =='1') / length(data[,1]) ) 
  ratio[1,2] <- (sum(data_pair$demo =='1') / length(data_pair[,1]) ) 
  ratio[1,3] <-  ratio[1,1]  /  ratio[1,2]                        
  ratio[1,4] <- (sum(data$demo =='2')/ length(data[,1]) ) 
  ratio[1,5] <- (sum(data_pair$demo =='2') / length(data_pair[,1]) )
  ratio[1,6] <- ratio[1,4] / ratio[1,5]
  ratio[1,7] <- (sum(data$demo =='3')/ length(data[,1]) ) 
  ratio[1,8] <- (sum(data_pair$demo =='3') / length(data_pair[,1]) ) 
  ratio[1,9] <- ratio[1,7] / ratio[1,8]
  ratio[1,10] <- (sum(data$demo =='4') / length(data[,1]) )  
  ratio[1,11] <- (sum(data_pair$demo =='4') / length(data_pair[,1]) )
  ratio[1,12] <- ratio[1,10] / ratio[1,11]
  
  ratio[1,13] <- (sum(data$poverty =='1') / length(data[,1]) ) 
  ratio[1,14] <- (sum(data_pair$poverty =='1') / length(data_pair[,1]) )
  ratio[1,15] <- ratio[1,13] / ratio[1,14]
  
  ratio[1,16] <- (sum(data$income =='1') / length(data[,1]) ) 
  ratio[1,17] <- (sum(data_pair$income =='1') / length(data_pair[,1]) )
  ratio[1,18] <- ratio[1,16] / ratio[1,17]
  ratio[1,19] <- (sum(data$income  =='2')/ length(data[,1]) ) 
  ratio[1,20] <- (sum(data_pair$income  =='2') / length(data_pair[,1]) )
  ratio[1,21] <- ratio[1,19] / ratio[1,20]
  ratio[1,22] <- (sum(data$income  =='3')/ length(data[,1]) ) 
  ratio[1,23] <- (sum(data_pair$income  =='3') / length(data_pair[,1]) ) 
  ratio[1,24] <- ratio[1,22] / ratio[1,23]
  ratio[1,25] <- (sum(data$income  =='4') / length(data[,1]) )  
  ratio[1,26] <- (sum(data_pair$income  =='4') / length(data_pair[,1]) )
  ratio[1,27] <- ratio[1,25] / ratio[1,26]
  
  ratio[1,28] <- (sum(data$rent =='1') / length(data[,1]) ) 
  ratio[1,29] <- (sum(data_pair$rent =='1') / length(data_pair[,1]) )
  ratio[1,30] <- ratio[1,28] / ratio[1,29]
  
  return (ratio)
}


NLI_impact_all <- data.frame()
NLI_impact <- data.frame()
ratio <- data.frame()

data <- DataTotal_fit2_impact1
data_pair <- DataTotal_fit2_impact2
ratio <- f.impactratio (data, data_pair)
NLI_impact_all <- rbind( NLI_impact_all, ratio)
NLI_impact <- rbind( NLI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )

data <- DataTotal_fit3_impact3
data_pair <- DataTotal_fit3_impact1
ratio <- f.impactratio (data, data_pair)
NLI_impact_all <- rbind( NLI_impact_all, ratio)
NLI_impact <- rbind( NLI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )

data <- DataTotal_fit3_impact2
data_pair <- DataTotal_fit3_impact1
ratio <- f.impactratio (data, data_pair)
NLI_impact_all <- rbind( NLI_impact_all, ratio)
NLI_impact <- rbind( NLI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )


data <- DataTotal_distance1
data_pair <- DataTotal_distanceNot1
ratio <- f.impactratio (data, data_pair)
NLI_impact_all <- rbind( NLI_impact_all, ratio)
NLI_impact <- rbind( NLI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                    ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )

data <- DataTotal_distance2
data_pair <- DataTotal_distanceNot2
ratio <- f.impactratio (data, data_pair)
NLI_impact_all <- rbind( NLI_impact_all, ratio)
NLI_impact <- rbind( NLI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                    ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )


data <- DataTotal_distance3
data_pair <- DataTotal_distanceNot3
ratio <- f.impactratio (data, data_pair)
NLI_impact_all <- rbind( NLI_impact_all, ratio)
NLI_impact <- rbind( NLI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )


data <- DataTotal_distance4
data_pair <- DataTotal_distanceNot4
ratio <- f.impactratio (data, data_pair)
NLI_impact_all <- rbind( NLI_impact_all, ratio)
NLI_impact <- rbind( NLI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )


write.table(NLI_impact,
            "C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result2_02042015/NLIImpact.txt",
            sep="\t",row.names=FALSE)
write.table(NLI_impact_all,
            "C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result2_02042015/NLIImpact_all.txt",
            sep="\t",row.names=FALSE)
