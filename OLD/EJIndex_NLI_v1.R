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

# NLI

Data_demo1 <- subset(DataProportion, DataProportion$demo ==1 )
Data_demo2 <- subset(DataProportion, DataProportion$demo ==2 )
Data_demo3 <- subset(DataProportion, DataProportion$demo ==3 )
Data_demo4 <- subset(DataProportion, DataProportion$demo ==4 )

Data_nondemo1 <- subset(DataProportion, DataProportion$demo !=1 )
Data_nondemo2 <- subset(DataProportion, DataProportion$demo !=2 )
Data_nondemo3 <- subset(DataProportion, DataProportion$demo !=3 )
Data_nondemo4 <- subset(DataProportion, DataProportion$demo !=4 )

Data_pov1 <- subset(DataProportion, DataProportion$poverty ==1 )
Data_pov2 <- subset(DataProportion, DataProportion$poverty ==2 )

Data_inc1 <- subset(DataProportion, DataProportion$income ==1 )
Data_inc2 <- subset(DataProportion, DataProportion$income ==2 )
Data_inc3 <- subset(DataProportion, DataProportion$income ==3 )
Data_inc4 <- subset(DataProportion, DataProportion$income ==4 )

Data_noninc1 <- subset(DataProportion, DataProportion$income !=1 )
Data_noninc2 <- subset(DataProportion, DataProportion$income !=2 )
Data_noninc3 <- subset(DataProportion, DataProportion$income !=3 )
Data_noninc4 <- subset(DataProportion, DataProportion$income !=4 )

Data_rent1 <- subset(DataProportion, DataProportion$rent ==1 )
Data_rent2 <- subset(DataProportion, DataProportion$rent ==2 )


# total
total_imp <- data.frame()


total_imp <- cbind( sum(Data_demo1[,23]), sum(Data_demo1[,24]) , sum(Data_demo1[,25]),
                    sum(Data_demo1[,26]) , sum(Data_demo1[,27]) ) 

total_imp <- rbind( total_imp, 
                    cbind( sum(Data_demo2[,23]), sum(Data_demo2[,24]) , sum(Data_demo2[,25]),
                           sum(Data_demo2[,26]), sum(Data_demo2[,27]) ) )
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_demo3[,23]), sum(Data_demo3[,24]) , sum(Data_demo3[,25]),
                           sum(Data_demo3[,26]), sum(Data_demo3[,27]) )  )
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_demo4[,23]), sum(Data_demo4[,24]) , sum(Data_demo4[,25]),
                           sum(Data_demo4[,26]), sum(Data_demo4[,27]) )  )


total_imp <- rbind( total_imp, 
                    cbind( sum(Data_nondemo1[,23]), sum(Data_nondemo1[,24]) , sum(Data_nondemo1[,25]),
                           sum(Data_nondemo1[,26]) , sum(Data_nondemo1[,27]) ) )

total_imp <- rbind( total_imp, 
                    cbind( sum(Data_nondemo2[,23]), sum(Data_nondemo2[,24]) , sum(Data_nondemo2[,25]),
                           sum(Data_nondemo2[,26]), sum(Data_nondemo2[,27]) ) ) 
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_nondemo3[,23]), sum(Data_nondemo3[,24]) , sum(Data_nondemo3[,25]),
                           sum(Data_nondemo3[,26]), sum(Data_nondemo3[,27]) )  ) 
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_nondemo4[,23]), sum(Data_nondemo4[,24]) , sum(Data_nondemo4[,25]),
                           sum(Data_nondemo4[,26]), sum(Data_nondemo4[,27]) )  ) 



total_imp <- rbind( total_imp, 
                    cbind( sum(Data_pov1[,23]), sum(Data_pov1[,24]) , sum(Data_pov1[,25]),
                           sum(Data_pov1[,26]), sum(Data_pov1[,27]) )  )
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_pov2[,23]), sum(Data_pov2[,24]) , sum(Data_pov2[,25]),
                           sum(Data_pov2[,26]), sum(Data_pov2[,27]) )  )

total_imp <- rbind( total_imp, 
                    cbind( sum(Data_inc1[,23]), sum(Data_inc1[,24]) , sum(Data_inc1[,25]),
                           sum(Data_inc1[,26]), sum(Data_inc1[,27]) )  )
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_inc2[,23]), sum(Data_inc2[,24]) , sum(Data_inc2[,25]),
                           sum(Data_inc2[,26]), sum(Data_inc2[,27]) )  )
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_inc3[,23]), sum(Data_inc3[,24]) , sum(Data_inc3[,25]),
                           sum(Data_inc3[,26]), sum(Data_inc3[,27]) )  )
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_inc4[,23]), sum(Data_inc4[,24]) , sum(Data_inc4[,25]),
                           sum(Data_inc4[,26]), sum(Data_inc4[,27]) )  )

total_imp <- rbind( total_imp, 
                    cbind( sum(Data_noninc1[,23]), sum(Data_noninc1[,24]) , sum(Data_noninc1[,25]),
                           sum(Data_noninc1[,26]), sum(Data_noninc1[,27]) )  )
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_noninc2[,23]), sum(Data_noninc2[,24]) , sum(Data_noninc2[,25]),
                           sum(Data_noninc2[,26]), sum(Data_noninc2[,27]) )  )
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_noninc3[,23]), sum(Data_noninc3[,24]) , sum(Data_noninc3[,25]),
                           sum(Data_noninc3[,26]), sum(Data_noninc3[,27]) )  )
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_noninc4[,23]), sum(Data_noninc4[,24]) , sum(Data_noninc4[,25]),
                           sum(Data_noninc4[,26]), sum(Data_noninc4[,27]) )  )

total_imp <- rbind( total_imp, 
                    cbind( sum(Data_rent1[,23]), sum(Data_rent1[,24]) , sum(Data_rent1[,25]),
                           sum(Data_rent1[,26]), sum(Data_rent1[,27]) )  )
total_imp <- rbind( total_imp, 
                    cbind( sum(Data_rent2[,23]), sum(Data_rent2[,24]) , sum(Data_rent2[,25]),
                           sum(Data_rent2[,26]), sum(Data_rent2[,27]) )  )

rownames(total_imp)[1:20] <- c("demo1" , "demo2" , "demo3" , "demo4" ,
                               "nondemo1" , "nondemo2" , "nondemo3" , "nondemo4" ,
                               "poor", "notpoor",
                               "inc1" , "inc2" , "inc3" , "inc4",
                               "noninc1" , "noninc2" , "noninc3" , "noninc4",
                               "rent1", "rent2")

len_data <- rbind( length(Data_demo1[,1]), length(Data_demo2[,1]), length(Data_demo3[,1]), length(Data_demo4[,1]),
                   length(Data_nondemo1[,1]), length(Data_nondemo2[,1]), length(Data_nondemo3[,1]), length(Data_nondemo4[,1]),
                   length(Data_pov1[,1]),  length(Data_pov2[,1]),
                   length(Data_inc1[,1]), length(Data_inc2[,1]), length(Data_inc3[,1]), length(Data_inc4[,1]),
                   length(Data_noninc1[,1]), length(Data_noninc2[,1]), length(Data_noninc3[,1]), length(Data_noninc4[,1]),
                   length(Data_rent1[,1]), length(Data_rent2[,1]) )

total_imp <- cbind( len_data, total_imp )

colnames(total_imp)[1:6] <- c("BGs", " 2005con", "2012con" , "conimp", "INC", "VAL")

total_imp <- cbind(total_imp, matrix(nrow=20, ncol=5))

for (i in 1: length(total_imp[,1])){
    total_imp[i,7] <- total_imp[i,2] / total_imp[i,1]  
    total_imp[i,8] <- total_imp[i,3] / total_imp[i,1]  
    total_imp[i,9] <- total_imp[i,4] / total_imp[i,1]  
    total_imp[i,10] <- total_imp[i,5] / total_imp[i,1]  
    total_imp[i,11] <- total_imp[i,6] / total_imp[i,1]  
  
}

colnames(total_imp)[7:11] <- c( "mean_2005con", "mean_2012con" , "mean_conimp", "mean_INC", "mean_VAL")
