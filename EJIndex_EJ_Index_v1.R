setwd("C:/Users/Kate Hyun/Dropbox/Kate/EJ/paper research/DataAnalysis_2014_AfterTRB/0128R/EJ/Result2_02042015")

ILARatio_fit2_region1 <- read.table("./Impactbased_ILA/ILARatio_fit2_region1.txt", header=T, fill=T) # major
ILARatio_fit2_region2 <- read.table("./Impactbased_ILA/ILARatio_fit2_region2.txt", header=T, fill=T) # no impact
ILARatio_fit3_region1 <- read.table("./Impactbased_ILA/ILARatio_fit3_region1.txt", header=T, fill=T) # no impact
ILARatio_fit3_region2 <- read.table("./Impactbased_ILA/ILARatio_fit3_region2.txt", header=T, fill=T) # minor
ILARatio_fit3_region3 <- read.table("./Impactbased_ILA/ILARatio_fit3_region3.txt", header=T, fill=T) # major

ILI_impact <- data.frame()
for ( i in 1 : length(ILARatio_fit2_region1[11,]) ){
  ILI_impact[1,i] <- ILARatio_fit2_region1[11,i] / ILARatio_fit2_region2[11,i] #fit2
  ILI_impact[2,i] <- ILARatio_fit3_region3[11,i] / ILARatio_fit3_region1[11,i] #fit3 major
  ILI_impact[3,i] <- ILARatio_fit3_region2[11,i] / ILARatio_fit3_region1[11,i] #fit3 minor
}

