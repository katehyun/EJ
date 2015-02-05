rm(list=ls())
options(scipen=999) 
setwd("C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/")

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

f.impactratio <- function (data, data_pair){
  
  ratio[1,1] <- sum(data$White) / sum(data$Total)
  ratio[1,2] <- sum(data_pair$White) / sum(data_pair$Total)
  ratio[1,3] <-  ratio[1,1]  /  ratio[1,2]                        
  ratio[1,4] <- sum(data$AfricanAmerican)/ sum(data$Total ) 
  ratio[1,5] <- sum(data_pair$AfricanAmerican) / sum(data_pair$Total )
  ratio[1,6] <- ratio[1,4] / ratio[1,5]
  ratio[1,7] <- sum(data$Asian)/ sum(data$Total)
  ratio[1,8] <- sum(data_pair$Asian) / sum(data_pair$Total )
  ratio[1,9] <- ratio[1,7] / ratio[1,8]
  ratio[1,10] <- sum(data$Hispanic) /  sum(data$Total) 
  ratio[1,11] <- sum(data_pair$Hispanic) / sum(data_pair$Total  )
  ratio[1,12] <- ratio[1,10] / ratio[1,11]
  
  ratio[1,13] <- sum(data$PoorHHD) /  sum(data$HHD)
  ratio[1,14] <- sum(data_pair$PoorHHD) /  sum(data_pair$HHD)
  ratio[1,15] <- ratio[1,13] / ratio[1,14]
  
  ratio[1,16] <- sum(data$Income_g1) / sum(data$HHD)
  ratio[1,17] <- sum(data_pair$Income_g1) / sum(data_pair$HHD)
  ratio[1,18] <- ratio[1,16] / ratio[1,17]
  ratio[1,19] <- sum(data$Income_g2)/ sum(data$HHD)
  ratio[1,20] <- sum(data_pair$Income_g2) / sum(data_pair$HHD)
  ratio[1,21] <- ratio[1,19] / ratio[1,20]
  ratio[1,22] <- sum(data$Income_g3)/ sum(data$HHD)
  ratio[1,23] <- sum(data_pair$Income_g3) / sum(data_pair$HHD)
  ratio[1,24] <- ratio[1,22] / ratio[1,23]
  ratio[1,25] <- sum(data$Income_g4) / sum(data$HHD) 
  ratio[1,26] <- sum(data_pair$Income_g4) / sum(data_pair$HHD)
  ratio[1,27] <- ratio[1,25] / ratio[1,26]
  
  ratio[1,28] <- sum(data$Owner) /sum(data$HHD)
  ratio[1,29] <- sum(data_pair$Owner) / sum(data_pair$HHD)
  ratio[1,30] <- ratio[1,28] / ratio[1,29]
  
  return (ratio)
}



ILI_impact_all <- data.frame()
ILI_impact <- data.frame()
ratio <- data.frame()

data <- DataTotal_fit2_impact1
data_pair <- DataTotal_fit2_impact2
ratio <- f.impactratio (data, data_pair)
ILI_impact_all <- rbind(ILI_impact_all, ratio)
ILI_impact <- rbind( ILI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )

data <- DataTotal_fit3_impact3
data_pair <- DataTotal_fit3_impact1
ratio <- f.impactratio (data, data_pair)
ILI_impact_all <- rbind(ILI_impact_all, ratio)
ILI_impact <- rbind( ILI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )

data <- DataTotal_fit3_impact2
data_pair <- DataTotal_fit3_impact1
ratio <- f.impactratio (data, data_pair)
ILI_impact_all <- rbind(ILI_impact_all, ratio)
ILI_impact <- rbind( ILI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )

data <- DataTotal_distance1
data_pair <- DataTotal_distanceNot1
ratio <- f.impactratio (data, data_pair)
ILI_impact_all <- rbind(ILI_impact_all, ratio)
ILI_impact <- rbind( ILI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )

data <- DataTotal_distance2
data_pair <- DataTotal_distanceNot2
ratio <- f.impactratio (data, data_pair)
ILI_impact_all <- rbind(ILI_impact_all, ratio)
ILI_impact <- rbind( ILI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )

data <- DataTotal_distance3
data_pair <- DataTotal_distanceNot3
ratio <- f.impactratio (data, data_pair)
ILI_impact_all <- rbind(ILI_impact_all, ratio)
ILI_impact <- rbind( ILI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )

data <- DataTotal_distance4
data_pair <- DataTotal_distanceNot4
ratio <- f.impactratio (data, data_pair)
ILI_impact_all <- rbind(ILI_impact_all, ratio)
ILI_impact <- rbind( ILI_impact, cbind(ratio[1,3] , ratio[1,6], ratio[1,9], ratio[1,12], ratio[1,15], ratio[1,18],
                                       ratio[1,21], ratio[1,24], ratio[1,27], ratio[1,30]) )





write.table(ILI_impact,
            "C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result2_02042015/ILIImpact.txt",
            sep="\t",row.names=FALSE)
write.table(ILI_impact_all,
            "C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result2_02042015/ILIImpact_all.txt",
            sep="\t",row.names=FALSE)
