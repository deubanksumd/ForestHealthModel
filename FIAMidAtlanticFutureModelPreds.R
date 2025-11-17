#Future Predictions for FIA mid-atlantic. Run after Model Testing Code.
setwd("~/Desktop/FIAModelCode/")
library(dplyr)
#CMIP6 downscaled
Clim26<-read.csv("midAtlClim26.csv")
Clim45<-read.csv("midAtlClim45.csv")
Clim85<-read.csv("midAtlClim85.csv")
Clim26 <- Clim26[-c(1)]
Clim45 <- Clim45[-c(1)]
Clim85 <- Clim85[-c(1)]

#Land Use Projections
rcp26 <- read.csv("RCP26FAD.csv")
rcp45 <- read.csv("RCP45FAD.csv")
rcp85 <- read.csv("RCP85FAD.csv")
rcp26 <- rcp26[-c(1)]
rcp45 <- rcp45[-c(1)]
rcp85 <- rcp85[-c(1)]

#read in FIA data
allFIA <- read.csv("allFIAfinal.csv")
allFIA <- allFIA[-c(1)]
SQIdata <- read.csv("allSQIfinal.csv")
SQIdata <- SQIdata[-c(1)]

#Combine climate/landuse data, seperate by year
all2625 <- merge(Clim26[,c(1,2,14,26,38)],rcp26[,c(1,16)])
all4525 <- merge(Clim45[,c(1,2,14,26,38)],rcp45[,c(1,16)])
all8525 <- merge(Clim85[,c(1,2,14,26,38)],rcp85[,c(1,16)])
all2630 <- merge(Clim26[,c(1,3,15,27,39)],rcp26[,c(1,17)])
all4530 <- merge(Clim45[,c(1,3,15,27,39)],rcp45[,c(1,17)])
all8530 <- merge(Clim85[,c(1,3,15,27,39)],rcp85[,c(1,17)])
all2635 <- merge(Clim26[,c(1,4,16,28,40)],rcp26[,c(1,18)])
all4535 <- merge(Clim45[,c(1,4,16,28,40)],rcp45[,c(1,18)])
all8535 <- merge(Clim85[,c(1,4,16,28,40)],rcp85[,c(1,18)])
all2640 <- merge(Clim26[,c(1,5,17,29,41)],rcp26[,c(1,19)])
all4540 <- merge(Clim45[,c(1,5,17,29,41)],rcp45[,c(1,19)])
all8540 <- merge(Clim85[,c(1,5,17,29,41)],rcp85[,c(1,19)])
all2645 <- merge(Clim26[,c(1,6,18,30,42)],rcp26[,c(1,20)])
all4545 <- merge(Clim45[,c(1,6,18,30,42)],rcp45[,c(1,20)])
all8545 <- merge(Clim85[,c(1,6,18,30,42)],rcp85[,c(1,20)])
all2650 <- merge(Clim26[,c(1,7,19,31,43)],rcp26[,c(1,21)])
all4550 <- merge(Clim45[,c(1,7,19,31,43)],rcp45[,c(1,21)])
all8550 <- merge(Clim85[,c(1,7,19,31,43)],rcp85[,c(1,21)])
all2655 <- merge(Clim26[,c(1,8,20,32,44)],rcp26[,c(1,22)])
all4555 <- merge(Clim45[,c(1,8,20,32,44)],rcp45[,c(1,22)])
all8555 <- merge(Clim85[,c(1,8,20,32,44)],rcp85[,c(1,22)])
all2660 <- merge(Clim26[,c(1,9,21,33,45)],rcp26[,c(1,23)])
all4560 <- merge(Clim45[,c(1,9,21,33,45)],rcp45[,c(1,23)])
all8560 <- merge(Clim85[,c(1,9,21,33,45)],rcp85[,c(1,23)])
all2665 <- merge(Clim26[,c(1,10,22,34,46)],rcp26[,c(1,24)])
all4565 <- merge(Clim45[,c(1,10,22,34,46)],rcp45[,c(1,24)])
all8565 <- merge(Clim85[,c(1,10,22,34,46)],rcp85[,c(1,24)])
all2670 <- merge(Clim26[,c(1,11,23,35,47)],rcp26[,c(1,25)])
all4570 <- merge(Clim45[,c(1,11,23,35,47)],rcp45[,c(1,25)])
all8570 <- merge(Clim85[,c(1,11,23,35,47)],rcp85[,c(1,25)])
all2675 <- merge(Clim26[,c(1,12,24,36,48)],rcp26[,c(1,26)])
all4575 <- merge(Clim45[,c(1,12,24,36,48)],rcp45[,c(1,26)])
all8575 <- merge(Clim85[,c(1,12,24,36,48)],rcp85[,c(1,26)])
all2680 <- merge(Clim26[,c(1,13,25,37,49)],rcp26[,c(1,27)])
all4580 <- merge(Clim45[,c(1,13,25,37,49)],rcp45[,c(1,27)])
all8580 <- merge(Clim85[,c(1,13,25,37,49)],rcp85[,c(1,27)])

#Name columns
colnames(all2625) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4525) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8525) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all2630) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4530) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8530) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all2635) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4535) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8535) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all2640) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4540) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8540) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all2645) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4545) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8545) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all2650) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4550) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8550) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all2655) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4555) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8555) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all2660) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4560) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8560) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all2665) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4565) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8565) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all2670) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4570) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8570) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all2675) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4575) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8575) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all2680) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all4580) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(all8580) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")

#combine projected data with plot data
all2625 <- merge(all2625,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4525 <- merge(all4525,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8525 <- merge(all8525,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all2630 <- merge(all2630,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4530 <- merge(all4530,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8530 <- merge(all8530,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all2635 <- merge(all2635,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4535 <- merge(all4535,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8535 <- merge(all8535,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all2640 <- merge(all2640,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4540 <- merge(all4540,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8540 <- merge(all8540,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all2645 <- merge(all2645,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4545 <- merge(all4545,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8545 <- merge(all8545,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all2650 <- merge(all2650,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4550 <- merge(all4550,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8550 <- merge(all8550,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all2655 <- merge(all2655,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4555 <- merge(all4555,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8555 <- merge(all8555,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all2660 <- merge(all2660,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4560 <- merge(all4560,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8560 <- merge(all8560,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all2665 <- merge(all2665,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4565 <- merge(all4565,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8565 <- merge(all8565,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all2670 <- merge(all2670,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4570 <- merge(all4570,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8570 <- merge(all8570,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all2675 <- merge(all2675,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4575 <- merge(all4575,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8575 <- merge(all8575,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all2680 <- merge(all2680,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all4580 <- merge(all4580,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)
all8580 <- merge(all8580,allFIA[c("PlotCN","soil_order","ForestType","Elevation","BAdead","AvgBADeadRate","BAAll")],by="PlotCN",all.x=TRUE)

names(all2625)[10] <- c("BA")
names(all4525)[10] <- c("BA")
names(all8525)[10] <- c("BA")
names(all2630)[10] <- c("BA")
names(all4530)[10] <- c("BA")
names(all8530)[10] <- c("BA")
names(all2635)[10] <- c("BA")
names(all4535)[10] <- c("BA")
names(all8535)[10] <- c("BA")
names(all2640)[10] <- c("BA")
names(all4540)[10] <- c("BA")
names(all8540)[10] <- c("BA")
names(all2645)[10] <- c("BA")
names(all4545)[10] <- c("BA")
names(all8545)[10] <- c("BA")
names(all2650)[10] <- c("BA")
names(all4550)[10] <- c("BA")
names(all8550)[10] <- c("BA")
names(all2655)[10] <- c("BA")
names(all4555)[10] <- c("BA")
names(all8555)[10] <- c("BA")
names(all2660)[10] <- c("BA")
names(all4560)[10] <- c("BA")
names(all8560)[10] <- c("BA")
names(all2665)[10] <- c("BA")
names(all4565)[10] <- c("BA")
names(all8565)[10] <- c("BA")
names(all2670)[10] <- c("BA")
names(all4570)[10] <- c("BA")
names(all8570)[10] <- c("BA")
names(all2675)[10] <- c("BA")
names(all4575)[10] <- c("BA")
names(all8575)[10] <- c("BA")
names(all2680)[10] <- c("BA")
names(all4580)[10] <- c("BA")
names(all8580)[10] <- c("BA")

#remove NA's 
all2625cc <- all2625 [complete.cases(all2625 []),]
all4525cc <- all4525 [complete.cases(all4525 []),]
all8525cc <- all8525 [complete.cases(all8525 []),]
all2630cc <- all2630 [complete.cases(all2630 []),]
all4530cc <- all4530 [complete.cases(all4530 []),]
all8530cc <- all8530 [complete.cases(all8530 []),]
all2635cc <- all2635 [complete.cases(all2635 []),]
all4535cc <- all4535 [complete.cases(all4535 []),]
all8535cc <- all8535 [complete.cases(all8535 []),]
all2640cc <- all2640 [complete.cases(all2640 []),]
all4540cc <- all4540 [complete.cases(all4540 []),]
all8540cc <- all8540 [complete.cases(all8540 []),]
all2645cc <- all2645 [complete.cases(all2645 []),]
all4545cc <- all4545 [complete.cases(all4545 []),]
all8545cc <- all8545 [complete.cases(all8545 []),]
all2650cc <- all2650 [complete.cases(all2650 []),]
all4550cc <- all4550 [complete.cases(all4550 []),]
all8550cc <- all8550 [complete.cases(all8550 []),]
all2655cc <- all2655 [complete.cases(all2655 []),]
all4555cc <- all4555 [complete.cases(all4555 []),]
all8555cc <- all8555 [complete.cases(all8555 []),]
all2660cc <- all2660 [complete.cases(all2660 []),]
all4560cc <- all4560 [complete.cases(all4560 []),]
all8560cc <- all8560 [complete.cases(all8560 []),]
all2665cc <- all2665 [complete.cases(all2665 []),]
all4565cc <- all4565 [complete.cases(all4565 []),]
all8565cc <- all8565 [complete.cases(all8565 []),]
all2670cc <- all2670 [complete.cases(all2670 []),]
all4570cc <- all4570 [complete.cases(all4570 []),]
all8570cc <- all8570 [complete.cases(all8570 []),]
all2675cc <- all2675 [complete.cases(all2675 []),]
all4575cc <- all4575 [complete.cases(all4575 []),]
all8575cc <- all8575 [complete.cases(all8575 []),]
all2680cc <- all2680 [complete.cases(all2680 []),]
all4580cc <- all4580 [complete.cases(all4580 []),]
all8580cc <- all8580 [complete.cases(all8580 []),]

#remove other non-cc dataframes, save memory/increase speed
rm(all2625,all2630,all2635,all2640,all2645,all2650,all2655,all2660,all2665,all2670,all2675,all2680)
rm(all4525,all4530,all4535,all4540,all4545,all4550,all4555,all4560,all4565,all4570,all4575,all4580)
rm(all8525,all8530,all8535,all8540,all8545,all8550,all8555,all8560,all8565,all8570,all8575,all8580)
#add forest groups
Fgroups <- c(100, 120, 140, 150, 160, 170, 180, 200, 220, 240, 260, 280, 300, 320, 
             340, 360, 370, 380, 390, 400, 500, 600, 700, 800, 900, 910, 920, 940, 
             960, 970, 980, 990, 999, Inf)
Flabels <- seq_along(Fgroups[-1])

all2625cc <- all2625cc %>% mutate(ForestGroup = Flabels[findInterval(all2625cc$ForestType,Fgroups)])
all4525cc <- all4525cc %>% mutate(ForestGroup = Flabels[findInterval(all4525cc$ForestType,Fgroups)])
all8525cc <- all8525cc %>% mutate(ForestGroup = Flabels[findInterval(all8525cc$ForestType,Fgroups)])
all2630cc <- all2630cc %>% mutate(ForestGroup = Flabels[findInterval(all2630cc$ForestType,Fgroups)])
all4530cc <- all4530cc %>% mutate(ForestGroup = Flabels[findInterval(all4530cc$ForestType,Fgroups)])
all8530cc <- all8530cc %>% mutate(ForestGroup = Flabels[findInterval(all8530cc$ForestType,Fgroups)])
all2635cc <- all2635cc %>% mutate(ForestGroup = Flabels[findInterval(all2635cc$ForestType,Fgroups)])
all4535cc <- all4535cc %>% mutate(ForestGroup = Flabels[findInterval(all4535cc$ForestType,Fgroups)])
all8535cc <- all8535cc %>% mutate(ForestGroup = Flabels[findInterval(all8535cc$ForestType,Fgroups)])
all2640cc <- all2640cc %>% mutate(ForestGroup = Flabels[findInterval(all2640cc$ForestType,Fgroups)])
all4540cc <- all4540cc %>% mutate(ForestGroup = Flabels[findInterval(all4540cc$ForestType,Fgroups)])
all8540cc <- all8540cc %>% mutate(ForestGroup = Flabels[findInterval(all8540cc$ForestType,Fgroups)])
all2645cc <- all2645cc %>% mutate(ForestGroup = Flabels[findInterval(all2645cc$ForestType,Fgroups)])
all4545cc <- all4545cc %>% mutate(ForestGroup = Flabels[findInterval(all4545cc$ForestType,Fgroups)])
all8545cc <- all8545cc %>% mutate(ForestGroup = Flabels[findInterval(all8545cc$ForestType,Fgroups)])
all2650cc <- all2650cc %>% mutate(ForestGroup = Flabels[findInterval(all2650cc$ForestType,Fgroups)])
all4550cc <- all4550cc %>% mutate(ForestGroup = Flabels[findInterval(all4550cc$ForestType,Fgroups)])
all8550cc <- all8550cc %>% mutate(ForestGroup = Flabels[findInterval(all8550cc$ForestType,Fgroups)])
all2655cc <- all2655cc %>% mutate(ForestGroup = Flabels[findInterval(all2655cc$ForestType,Fgroups)])
all4555cc <- all4555cc %>% mutate(ForestGroup = Flabels[findInterval(all4555cc$ForestType,Fgroups)])
all8555cc <- all8555cc %>% mutate(ForestGroup = Flabels[findInterval(all8555cc$ForestType,Fgroups)])
all2660cc <- all2660cc %>% mutate(ForestGroup = Flabels[findInterval(all2660cc$ForestType,Fgroups)])
all4560cc <- all4560cc %>% mutate(ForestGroup = Flabels[findInterval(all4560cc$ForestType,Fgroups)])
all8560cc <- all8560cc %>% mutate(ForestGroup = Flabels[findInterval(all8560cc$ForestType,Fgroups)])
all2665cc <- all2665cc %>% mutate(ForestGroup = Flabels[findInterval(all2665cc$ForestType,Fgroups)])
all4565cc <- all4565cc %>% mutate(ForestGroup = Flabels[findInterval(all4565cc$ForestType,Fgroups)])
all8565cc <- all8565cc %>% mutate(ForestGroup = Flabels[findInterval(all8565cc$ForestType,Fgroups)])
all2670cc <- all2670cc %>% mutate(ForestGroup = Flabels[findInterval(all2670cc$ForestType,Fgroups)])
all4570cc <- all4570cc %>% mutate(ForestGroup = Flabels[findInterval(all4570cc$ForestType,Fgroups)])
all8570cc <- all8570cc %>% mutate(ForestGroup = Flabels[findInterval(all8570cc$ForestType,Fgroups)])
all2675cc <- all2675cc %>% mutate(ForestGroup = Flabels[findInterval(all2675cc$ForestType,Fgroups)])
all4575cc <- all4575cc %>% mutate(ForestGroup = Flabels[findInterval(all4575cc$ForestType,Fgroups)])
all8575cc <- all8575cc %>% mutate(ForestGroup = Flabels[findInterval(all8575cc$ForestType,Fgroups)])
all2680cc <- all2680cc %>% mutate(ForestGroup = Flabels[findInterval(all2680cc$ForestType,Fgroups)])
all4580cc <- all4580cc %>% mutate(ForestGroup = Flabels[findInterval(all4580cc$ForestType,Fgroups)])
all8580cc <- all8580cc %>% mutate(ForestGroup = Flabels[findInterval(all8580cc$ForestType,Fgroups)])

#fgroup 1
#subset data
all2625cc1 <- all2625cc %>% filter(ForestGroup==1)
all4525cc1 <- all4525cc %>% filter(ForestGroup==1)
all8525cc1 <- all8525cc %>% filter(ForestGroup==1)
all2630cc1 <- all2630cc %>% filter(ForestGroup==1)
all4530cc1 <- all4530cc %>% filter(ForestGroup==1)
all8530cc1 <- all8530cc %>% filter(ForestGroup==1)
all2635cc1 <- all2635cc %>% filter(ForestGroup==1)
all4535cc1 <- all4535cc %>% filter(ForestGroup==1)
all8535cc1 <- all8535cc %>% filter(ForestGroup==1)
all2640cc1 <- all2640cc %>% filter(ForestGroup==1)
all4540cc1 <- all4540cc %>% filter(ForestGroup==1)
all8540cc1 <- all8540cc %>% filter(ForestGroup==1)
all2645cc1 <- all2645cc %>% filter(ForestGroup==1)
all4545cc1 <- all4545cc %>% filter(ForestGroup==1)
all8545cc1 <- all8545cc %>% filter(ForestGroup==1)
all2650cc1 <- all2650cc %>% filter(ForestGroup==1)
all4550cc1 <- all4550cc %>% filter(ForestGroup==1)
all8550cc1 <- all8550cc %>% filter(ForestGroup==1)
all2655cc1 <- all2655cc %>% filter(ForestGroup==1)
all4555cc1 <- all4555cc %>% filter(ForestGroup==1)
all8555cc1 <- all8555cc %>% filter(ForestGroup==1)
all2660cc1 <- all2660cc %>% filter(ForestGroup==1)
all4560cc1 <- all4560cc %>% filter(ForestGroup==1)
all8560cc1 <- all8560cc %>% filter(ForestGroup==1)
all2665cc1 <- all2665cc %>% filter(ForestGroup==1)
all4565cc1 <- all4565cc %>% filter(ForestGroup==1)
all8565cc1 <- all8565cc %>% filter(ForestGroup==1)
all2670cc1 <- all2670cc %>% filter(ForestGroup==1)
all4570cc1 <- all4570cc %>% filter(ForestGroup==1)
all8570cc1 <- all8570cc %>% filter(ForestGroup==1)
all2675cc1 <- all2675cc %>% filter(ForestGroup==1)
all4575cc1 <- all4575cc %>% filter(ForestGroup==1)
all8575cc1 <- all8575cc %>% filter(ForestGroup==1)
all2680cc1 <- all2680cc %>% filter(ForestGroup==1)
all4580cc1 <- all4580cc %>% filter(ForestGroup==1)
all8580cc1 <- all8580cc %>% filter(ForestGroup==1)

all2625cc1$Soil <- as.factor(all2625cc1$soil_order)
all2625cc1$Soil <- as.integer(all2625cc1$Soil)
all2625cc1$FType <- as.factor(all2625cc1$ForestType)
all2625cc1$FType <- as.integer(all2625cc1$FType)
all4525cc1$Soil <- as.factor(all4525cc1$soil_order)
all4525cc1$Soil <- as.integer(all4525cc1$Soil)
all4525cc1$FType <- as.factor(all4525cc1$ForestType)
all4525cc1$FType <- as.integer(all4525cc1$FType)
all8525cc1$Soil <- as.factor(all8525cc1$soil_order)
all8525cc1$Soil <- as.integer(all8525cc1$Soil)
all8525cc1$FType <- as.factor(all8525cc1$ForestType)
all8525cc1$FType <- as.integer(all8525cc1$FType)

all2630cc1$Soil <- as.factor(all2630cc1$soil_order)
all2630cc1$Soil <- as.integer(all2630cc1$Soil)
all2630cc1$FType <- as.factor(all2630cc1$ForestType)
all2630cc1$FType <- as.integer(all2630cc1$FType)
all4530cc1$Soil <- as.factor(all4530cc1$soil_order)
all4530cc1$Soil <- as.integer(all4530cc1$Soil)
all4530cc1$FType <- as.factor(all4530cc1$ForestType)
all4530cc1$FType <- as.integer(all4530cc1$FType)
all8530cc1$Soil <- as.factor(all8530cc1$soil_order)
all8530cc1$Soil <- as.integer(all8530cc1$Soil)
all8530cc1$FType <- as.factor(all8530cc1$ForestType)
all8530cc1$FType <- as.integer(all8530cc1$FType)

all2635cc1$Soil <- as.factor(all2635cc1$soil_order)
all2635cc1$Soil <- as.integer(all2635cc1$Soil)
all2635cc1$FType <- as.factor(all2635cc1$ForestType)
all2635cc1$FType <- as.integer(all2635cc1$FType)
all4535cc1$Soil <- as.factor(all4535cc1$soil_order)
all4535cc1$Soil <- as.integer(all4535cc1$Soil)
all4535cc1$FType <- as.factor(all4535cc1$ForestType)
all4535cc1$FType <- as.integer(all4535cc1$FType)
all8535cc1$Soil <- as.factor(all8535cc1$soil_order)
all8535cc1$Soil <- as.integer(all8535cc1$Soil)
all8535cc1$FType <- as.factor(all8535cc1$ForestType)
all8535cc1$FType <- as.integer(all8535cc1$FType)


all2640cc1$Soil <- as.factor(all2640cc1$soil_order)
all2640cc1$Soil <- as.integer(all2640cc1$Soil)
all2640cc1$FType <- as.factor(all2640cc1$ForestType)
all2640cc1$FType <- as.integer(all2640cc1$FType)
all4540cc1$Soil <- as.factor(all4540cc1$soil_order)
all4540cc1$Soil <- as.integer(all4540cc1$Soil)
all4540cc1$FType <- as.factor(all4540cc1$ForestType)
all4540cc1$FType <- as.integer(all4540cc1$FType)
all8540cc1$Soil <- as.factor(all8540cc1$soil_order)
all8540cc1$Soil <- as.integer(all8540cc1$Soil)
all8540cc1$FType <- as.factor(all8540cc1$ForestType)
all8540cc1$FType <- as.integer(all8540cc1$FType)

all2645cc1$Soil <- as.factor(all2645cc1$soil_order)
all2645cc1$Soil <- as.integer(all2645cc1$Soil)
all2645cc1$FType <- as.factor(all2645cc1$ForestType)
all2645cc1$FType <- as.integer(all2645cc1$FType)
all4545cc1$Soil <- as.factor(all4545cc1$soil_order)
all4545cc1$Soil <- as.integer(all4545cc1$Soil)
all4545cc1$FType <- as.factor(all4545cc1$ForestType)
all4545cc1$FType <- as.integer(all4545cc1$FType)
all8545cc1$Soil <- as.factor(all8545cc1$soil_order)
all8545cc1$Soil <- as.integer(all8545cc1$Soil)
all8545cc1$FType <- as.factor(all8545cc1$ForestType)
all8545cc1$FType <- as.integer(all8545cc1$FType)

all2650cc1$Soil <- as.factor(all2650cc1$soil_order)
all2650cc1$Soil <- as.integer(all2650cc1$Soil)
all2650cc1$FType <- as.factor(all2650cc1$ForestType)
all2650cc1$FType <- as.integer(all2650cc1$FType)
all4550cc1$Soil <- as.factor(all4550cc1$soil_order)
all4550cc1$Soil <- as.integer(all4550cc1$Soil)
all4550cc1$FType <- as.factor(all4550cc1$ForestType)
all4550cc1$FType <- as.integer(all4550cc1$FType)
all8550cc1$Soil <- as.factor(all8550cc1$soil_order)
all8550cc1$Soil <- as.integer(all8550cc1$Soil)
all8550cc1$FType <- as.factor(all8550cc1$ForestType)
all8550cc1$FType <- as.integer(all8550cc1$FType)

all2655cc1$Soil <- as.factor(all2655cc1$soil_order)
all2655cc1$Soil <- as.integer(all2655cc1$Soil)
all2655cc1$FType <- as.factor(all2655cc1$ForestType)
all2655cc1$FType <- as.integer(all2655cc1$FType)
all4555cc1$Soil <- as.factor(all4555cc1$soil_order)
all4555cc1$Soil <- as.integer(all4555cc1$Soil)
all4555cc1$FType <- as.factor(all4555cc1$ForestType)
all4555cc1$FType <- as.integer(all4555cc1$FType)
all8555cc1$Soil <- as.factor(all8555cc1$soil_order)
all8555cc1$Soil <- as.integer(all8555cc1$Soil)
all8555cc1$FType <- as.factor(all8555cc1$ForestType)
all8555cc1$FType <- as.integer(all8555cc1$FType)


all2660cc1$Soil <- as.factor(all2660cc1$soil_order)
all2660cc1$Soil <- as.integer(all2660cc1$Soil)
all2660cc1$FType <- as.factor(all2660cc1$ForestType)
all2660cc1$FType <- as.integer(all2660cc1$FType)
all4560cc1$Soil <- as.factor(all4560cc1$soil_order)
all4560cc1$Soil <- as.integer(all4560cc1$Soil)
all4560cc1$FType <- as.factor(all4560cc1$ForestType)
all4560cc1$FType <- as.integer(all4560cc1$FType)
all8560cc1$Soil <- as.factor(all8560cc1$soil_order)
all8560cc1$Soil <- as.integer(all8560cc1$Soil)
all8560cc1$FType <- as.factor(all8560cc1$ForestType)
all8560cc1$FType <- as.integer(all8560cc1$FType)

all2665cc1$Soil <- as.factor(all2665cc1$soil_order)
all2665cc1$Soil <- as.integer(all2665cc1$Soil)
all2665cc1$FType <- as.factor(all2665cc1$ForestType)
all2665cc1$FType <- as.integer(all2665cc1$FType)
all4565cc1$Soil <- as.factor(all4565cc1$soil_order)
all4565cc1$Soil <- as.integer(all4565cc1$Soil)
all4565cc1$FType <- as.factor(all4565cc1$ForestType)
all4565cc1$FType <- as.integer(all4565cc1$FType)
all8565cc1$Soil <- as.factor(all8565cc1$soil_order)
all8565cc1$Soil <- as.integer(all8565cc1$Soil)
all8565cc1$FType <- as.factor(all8565cc1$ForestType)
all8565cc1$FType <- as.integer(all8565cc1$FType)

all2670cc1$Soil <- as.factor(all2670cc1$soil_order)
all2670cc1$Soil <- as.integer(all2670cc1$Soil)
all2670cc1$FType <- as.factor(all2670cc1$ForestType)
all2670cc1$FType <- as.integer(all2670cc1$FType)
all4570cc1$Soil <- as.factor(all4570cc1$soil_order)
all4570cc1$Soil <- as.integer(all4570cc1$Soil)
all4570cc1$FType <- as.factor(all4570cc1$ForestType)
all4570cc1$FType <- as.integer(all4570cc1$FType)
all8570cc1$Soil <- as.factor(all8570cc1$soil_order)
all8570cc1$Soil <- as.integer(all8570cc1$Soil)
all8570cc1$FType <- as.factor(all8570cc1$ForestType)
all8570cc1$FType <- as.integer(all8570cc1$FType)


all2675cc1$Soil <- as.factor(all2675cc1$soil_order)
all2675cc1$Soil <- as.integer(all2675cc1$Soil)
all2675cc1$FType <- as.factor(all2675cc1$ForestType)
all2675cc1$FType <- as.integer(all2675cc1$FType)
all4575cc1$Soil <- as.factor(all4575cc1$soil_order)
all4575cc1$Soil <- as.integer(all4575cc1$Soil)
all4575cc1$FType <- as.factor(all4575cc1$ForestType)
all4575cc1$FType <- as.integer(all4575cc1$FType)
all8575cc1$Soil <- as.factor(all8575cc1$soil_order)
all8575cc1$Soil <- as.integer(all8575cc1$Soil)
all8575cc1$FType <- as.factor(all8575cc1$ForestType)
all8575cc1$FType <- as.integer(all8575cc1$FType)

all2680cc1$Soil <- as.factor(all2680cc1$soil_order)
all2680cc1$Soil <- as.integer(all2680cc1$Soil)
all2680cc1$FType <- as.factor(all2680cc1$ForestType)
all2680cc1$FType <- as.integer(all2680cc1$FType)
all4580cc1$Soil <- as.factor(all4580cc1$soil_order)
all4580cc1$Soil <- as.integer(all4580cc1$Soil)
all4580cc1$FType <- as.factor(all4580cc1$ForestType)
all4580cc1$FType <- as.integer(all4580cc1$FType)
all8580cc1$Soil <- as.factor(all8580cc1$soil_order)
all8580cc1$Soil <- as.integer(all8580cc1$Soil)
all8580cc1$FType <- as.factor(all8580cc1$ForestType)
all8580cc1$FType <- as.integer(all8580cc1$FType)

#center and scale from model data
all2625cc1 $MAT<- as.data.frame(scale(all2625cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4525cc1 $MAT<- as.data.frame(scale(all4525cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8525cc1 $MAT<- as.data.frame(scale(all8525cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all2630cc1 $MAT <- as.data.frame(scale(all2630cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4530cc1 $MAT <- as.data.frame(scale(all4530cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8530cc1 $MAT <- as.data.frame(scale(all8530cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all2635cc1 $MAT<- as.data.frame(scale(all2635cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4535cc1 $MAT<- as.data.frame(scale(all4535cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8535cc1 $MAT<- as.data.frame(scale(all8535cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all2640cc1 $MAT <- as.data.frame(scale(all2640cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4540cc1 $MAT <- as.data.frame(scale(all4540cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8540cc1 $MAT <- as.data.frame(scale(all8540cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all2645cc1 $MAT<- as.data.frame(scale(all2645cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4545cc1 $MAT<- as.data.frame(scale(all4545cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8545cc1 $MAT<- as.data.frame(scale(all8545cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all2650cc1 $MAT <- as.data.frame(scale(all2650cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4550cc1 $MAT <- as.data.frame(scale(all4550cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8550cc1 $MAT <- as.data.frame(scale(all8550cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all2655cc1 $MAT<- as.data.frame(scale(all2655cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4555cc1 $MAT<- as.data.frame(scale(all4555cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8555cc1 $MAT<- as.data.frame(scale(all8555cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all2660cc1 $MAT <- as.data.frame(scale(all2660cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4560cc1 $MAT <- as.data.frame(scale(all4560cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8560cc1 $MAT <- as.data.frame(scale(all8560cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all2665cc1 $MAT<- as.data.frame(scale(all2665cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4565cc1 $MAT<- as.data.frame(scale(all4565cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8565cc1 $MAT<- as.data.frame(scale(all8565cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all2670cc1 $MAT <- as.data.frame(scale(all2670cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4570cc1 $MAT <- as.data.frame(scale(all4570cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8570cc1 $MAT <- as.data.frame(scale(all8570cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all2675cc1 $MAT<- as.data.frame(scale(all2675cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4575cc1 $MAT<- as.data.frame(scale(all4575cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8575cc1 $MAT<- as.data.frame(scale(all8575cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all2680cc1 $MAT <- as.data.frame(scale(all2680cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all4580cc1 $MAT <- as.data.frame(scale(all4580cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
all8580cc1 $MAT <- as.data.frame(scale(all8580cc1 $MAT, center=centerMAT1,scale=scaleMAT1),col.names="MAT")[, 1]
#PPT
all2625cc1 $PPT<- as.data.frame(scale(all2625cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4525cc1 $PPT<- as.data.frame(scale(all4525cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8525cc1 $PPT<- as.data.frame(scale(all8525cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all2630cc1 $PPT <- as.data.frame(scale(all2630cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4530cc1 $PPT <- as.data.frame(scale(all4530cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8530cc1 $PPT <- as.data.frame(scale(all8530cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all2635cc1 $PPT<- as.data.frame(scale(all2635cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4535cc1 $PPT<- as.data.frame(scale(all4535cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8535cc1 $PPT<- as.data.frame(scale(all8535cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all2640cc1 $PPT <- as.data.frame(scale(all2640cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4540cc1 $PPT <- as.data.frame(scale(all4540cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8540cc1 $PPT <- as.data.frame(scale(all8540cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all2645cc1 $PPT<- as.data.frame(scale(all2645cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4545cc1 $PPT<- as.data.frame(scale(all4545cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8545cc1 $PPT<- as.data.frame(scale(all8545cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all2650cc1 $PPT <- as.data.frame(scale(all2650cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4550cc1 $PPT <- as.data.frame(scale(all4550cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8550cc1 $PPT <- as.data.frame(scale(all8550cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all2655cc1 $PPT<- as.data.frame(scale(all2655cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4555cc1 $PPT<- as.data.frame(scale(all4555cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8555cc1 $PPT<- as.data.frame(scale(all8555cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all2660cc1 $PPT <- as.data.frame(scale(all2660cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4560cc1 $PPT <- as.data.frame(scale(all4560cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8560cc1 $PPT <- as.data.frame(scale(all8560cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all2665cc1 $PPT<- as.data.frame(scale(all2665cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4565cc1 $PPT<- as.data.frame(scale(all4565cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8565cc1 $PPT<- as.data.frame(scale(all8565cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all2670cc1 $PPT <- as.data.frame(scale(all2670cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4570cc1 $PPT <- as.data.frame(scale(all4570cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8570cc1 $PPT <- as.data.frame(scale(all8570cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all2675cc1 $PPT<- as.data.frame(scale(all2675cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4575cc1 $PPT<- as.data.frame(scale(all4575cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8575cc1 $PPT<- as.data.frame(scale(all8575cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all2680cc1 $PPT <- as.data.frame(scale(all2680cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all4580cc1 $PPT <- as.data.frame(scale(all4580cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
all8580cc1 $PPT <- as.data.frame(scale(all8580cc1 $PPT, center=centerPPT1,scale=scalePPT1),col.names="PPT")[, 1]
#RHUM
all2625cc1 $RHUM<- as.data.frame(scale(all2625cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4525cc1 $RHUM<- as.data.frame(scale(all4525cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8525cc1 $RHUM<- as.data.frame(scale(all8525cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all2630cc1 $RHUM <- as.data.frame(scale(all2630cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4530cc1 $RHUM <- as.data.frame(scale(all4530cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8530cc1 $RHUM <- as.data.frame(scale(all8530cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all2635cc1 $RHUM<- as.data.frame(scale(all2635cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4535cc1 $RHUM<- as.data.frame(scale(all4535cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8535cc1 $RHUM<- as.data.frame(scale(all8535cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all2640cc1 $RHUM <- as.data.frame(scale(all2640cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4540cc1 $RHUM <- as.data.frame(scale(all4540cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8540cc1 $RHUM <- as.data.frame(scale(all8540cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all2645cc1 $RHUM<- as.data.frame(scale(all2645cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4545cc1 $RHUM<- as.data.frame(scale(all4545cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8545cc1 $RHUM<- as.data.frame(scale(all8545cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all2650cc1 $RHUM <- as.data.frame(scale(all2650cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4550cc1 $RHUM <- as.data.frame(scale(all4550cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8550cc1 $RHUM <- as.data.frame(scale(all8550cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all2655cc1 $RHUM<- as.data.frame(scale(all2655cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4555cc1 $RHUM<- as.data.frame(scale(all4555cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8555cc1 $RHUM<- as.data.frame(scale(all8555cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all2660cc1 $RHUM <- as.data.frame(scale(all2660cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4560cc1 $RHUM <- as.data.frame(scale(all4560cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8560cc1 $RHUM <- as.data.frame(scale(all8560cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all2665cc1 $RHUM<- as.data.frame(scale(all2665cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4565cc1 $RHUM<- as.data.frame(scale(all4565cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8565cc1 $RHUM<- as.data.frame(scale(all8565cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all2670cc1 $RHUM <- as.data.frame(scale(all2670cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4570cc1 $RHUM <- as.data.frame(scale(all4570cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8570cc1 $RHUM <- as.data.frame(scale(all8570cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all2675cc1 $RHUM<- as.data.frame(scale(all2675cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4575cc1 $RHUM<- as.data.frame(scale(all4575cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8575cc1 $RHUM<- as.data.frame(scale(all8575cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all2680cc1 $RHUM <- as.data.frame(scale(all2680cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all4580cc1 $RHUM <- as.data.frame(scale(all4580cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
all8580cc1 $RHUM <- as.data.frame(scale(all8580cc1 $RHUM, center=centerRHUM1,scale=scaleRHUM1),col.names="RHUM")[, 1]
#RAD
all2625cc1 $RAD<- as.data.frame(scale(all2625cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4525cc1 $RAD<- as.data.frame(scale(all4525cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8525cc1 $RAD<- as.data.frame(scale(all8525cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all2630cc1 $RAD <- as.data.frame(scale(all2630cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4530cc1 $RAD <- as.data.frame(scale(all4530cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8530cc1 $RAD <- as.data.frame(scale(all8530cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all2635cc1 $RAD<- as.data.frame(scale(all2635cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4535cc1 $RAD<- as.data.frame(scale(all4535cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8535cc1 $RAD<- as.data.frame(scale(all8535cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all2640cc1 $RAD <- as.data.frame(scale(all2640cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4540cc1 $RAD <- as.data.frame(scale(all4540cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8540cc1 $RAD <- as.data.frame(scale(all8540cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all2645cc1 $RAD<- as.data.frame(scale(all2645cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4545cc1 $RAD<- as.data.frame(scale(all4545cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8545cc1 $RAD<- as.data.frame(scale(all8545cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all2650cc1 $RAD <- as.data.frame(scale(all2650cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4550cc1 $RAD <- as.data.frame(scale(all4550cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8550cc1 $RAD <- as.data.frame(scale(all8550cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all2655cc1 $RAD<- as.data.frame(scale(all2655cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4555cc1 $RAD<- as.data.frame(scale(all4555cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8555cc1 $RAD<- as.data.frame(scale(all8555cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all2660cc1 $RAD <- as.data.frame(scale(all2660cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4560cc1 $RAD <- as.data.frame(scale(all4560cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8560cc1 $RAD <- as.data.frame(scale(all8560cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all2665cc1 $RAD<- as.data.frame(scale(all2665cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4565cc1 $RAD<- as.data.frame(scale(all4565cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8565cc1 $RAD<- as.data.frame(scale(all8565cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all2670cc1 $RAD <- as.data.frame(scale(all2670cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4570cc1 $RAD <- as.data.frame(scale(all4570cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8570cc1 $RAD <- as.data.frame(scale(all8570cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all2675cc1 $RAD<- as.data.frame(scale(all2675cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4575cc1 $RAD<- as.data.frame(scale(all4575cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8575cc1 $RAD<- as.data.frame(scale(all8575cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all2680cc1 $RAD <- as.data.frame(scale(all2680cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all4580cc1 $RAD <- as.data.frame(scale(all4580cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
all8580cc1 $RAD <- as.data.frame(scale(all8580cc1 $RAD, center=centerRAD1,scale=scaleRAD1),col.names="RAD")[, 1]
#Elevation
all2625cc1 $Elevation<- as.data.frame(scale(all2625cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4525cc1 $Elevation<- as.data.frame(scale(all4525cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8525cc1 $Elevation<- as.data.frame(scale(all8525cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all2630cc1 $Elevation <- as.data.frame(scale(all2630cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4530cc1 $Elevation <- as.data.frame(scale(all4530cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8530cc1 $Elevation <- as.data.frame(scale(all8530cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all2635cc1 $Elevation<- as.data.frame(scale(all2635cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4535cc1 $Elevation<- as.data.frame(scale(all4535cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8535cc1 $Elevation<- as.data.frame(scale(all8535cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all2640cc1 $Elevation <- as.data.frame(scale(all2640cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4540cc1 $Elevation <- as.data.frame(scale(all4540cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8540cc1 $Elevation <- as.data.frame(scale(all8540cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all2645cc1 $Elevation<- as.data.frame(scale(all2645cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4545cc1 $Elevation<- as.data.frame(scale(all4545cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8545cc1 $Elevation<- as.data.frame(scale(all8545cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all2650cc1 $Elevation <- as.data.frame(scale(all2650cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4550cc1 $Elevation <- as.data.frame(scale(all4550cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8550cc1 $Elevation <- as.data.frame(scale(all8550cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all2655cc1 $Elevation<- as.data.frame(scale(all2655cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4555cc1 $Elevation<- as.data.frame(scale(all4555cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8555cc1 $Elevation<- as.data.frame(scale(all8555cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all2660cc1 $Elevation <- as.data.frame(scale(all2660cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4560cc1 $Elevation <- as.data.frame(scale(all4560cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8560cc1 $Elevation <- as.data.frame(scale(all8560cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all2665cc1 $Elevation<- as.data.frame(scale(all2665cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4565cc1 $Elevation<- as.data.frame(scale(all4565cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8565cc1 $Elevation<- as.data.frame(scale(all8565cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all2670cc1 $Elevation <- as.data.frame(scale(all2670cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4570cc1 $Elevation <- as.data.frame(scale(all4570cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8570cc1 $Elevation <- as.data.frame(scale(all8570cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all2675cc1 $Elevation<- as.data.frame(scale(all2675cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4575cc1 $Elevation<- as.data.frame(scale(all4575cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8575cc1 $Elevation<- as.data.frame(scale(all8575cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all2680cc1 $Elevation <- as.data.frame(scale(all2680cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all4580cc1 $Elevation <- as.data.frame(scale(all4580cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]
all8580cc1 $Elevation <- as.data.frame(scale(all8580cc1 $Elevation, center=centerElevation1,scale=scaleElevation1),col.names="Elevation")[, 1]

#add BasalArea + annual rate*years, some negatives produced, turn to 0
all2625cc1$BA <- (all2625cc1$BA)+(all2625cc1$AvgBADeadRate)*6
all4525cc1$BA <- (all4525cc1$BA)+(all4525cc1$AvgBADeadRate)*6
all8525cc1$BA <- (all8525cc1$BA)+(all8525cc1$AvgBADeadRate)*6
all2630cc1$BA <- (all2630cc1$BA)+(all2630cc1$AvgBADeadRate)*11
all4530cc1$BA <- (all4530cc1$BA)+(all4530cc1$AvgBADeadRate)*11
all8530cc1$BA <- (all8530cc1$BA)+(all8530cc1$AvgBADeadRate)*11
all2635cc1$BA <- (all2635cc1$BA)+(all2635cc1$AvgBADeadRate)*16
all4535cc1$BA <-  (all4535cc1$BA)+(all4535cc1$AvgBADeadRate)*16
all8535cc1$BA <- (all8535cc1$BA)+(all8535cc1$AvgBADeadRate)*16
all2640cc1$BA <- (all2640cc1$BA)+(all2640cc1$AvgBADeadRate)*21
all4540cc1$BA <- (all4540cc1$BA)+(all4540cc1$AvgBADeadRate)*21
all8540cc1$BA <-  (all8540cc1$BA)+(all8540cc1$AvgBADeadRate)*21
all2645cc1$BA <- (all2645cc1$BA)+(all2645cc1$AvgBADeadRate)*26
all4545cc1$BA <- (all4545cc1$BA)+(all4545cc1$AvgBADeadRate)*26
all8545cc1$BA <- (all8545cc1$BA)+(all8545cc1$AvgBADeadRate)*26
all2650cc1$BA <- (all2650cc1$BA)+(all2650cc1$AvgBADeadRate)*31
all4550cc1$BA <- (all4550cc1$BA)+(all4550cc1$AvgBADeadRate)*31
all8550cc1$BA <- (all8550cc1$BA)+(all8550cc1$AvgBADeadRate)*31
all2655cc1$BA <-(all2655cc1$BA)+(all2655cc1$AvgBADeadRate)*36
all4555cc1$BA <- (all4555cc1$BA)+(all4555cc1$AvgBADeadRate)*36
all8555cc1$BA <- (all8555cc1$BA)+(all8555cc1$AvgBADeadRate)*36
all2660cc1$BA <-(all2660cc1$BA)+(all2660cc1$AvgBADeadRate)*41
all4560cc1$BA <- (all4560cc1$BA)+(all4560cc1$AvgBADeadRate)*41
all8560cc1$BA <- (all8560cc1$BA)+(all8560cc1$AvgBADeadRate)*41
all2665cc1$BA <- (all2665cc1$BA)+(all2665cc1$AvgBADeadRate)*46
all4565cc1$BA <- (all4565cc1$BA)+(all4565cc1$AvgBADeadRate)*46
all8565cc1$BA <- (all8565cc1$BA)+(all8565cc1$AvgBADeadRate)*46
all2670cc1$BA <- (all2670cc1$BA)+(all2670cc1$AvgBADeadRate)*51
all4570cc1$BA <- (all4570cc1$BA)+(all4570cc1$AvgBADeadRate)*51
all8570cc1$BA <- (all8570cc1$BA)+(all8570cc1$AvgBADeadRate)*51
all2675cc1$BA <- (all2675cc1$BA)+(all2675cc1$AvgBADeadRate)*56
all4575cc1$BA <- (all4575cc1$BA)+(all4575cc1$AvgBADeadRate)*56
all8575cc1$BA <- (all8575cc1$BA)+(all8575cc1$AvgBADeadRate)*56
all2680cc1$BA <- (all2680cc1$BA)+(all2680cc1$AvgBADeadRate)*61
all4580cc1$BA <- (all4580cc1$BA)+(all4580cc1$AvgBADeadRate)*61
all8580cc1$BA <- (all8580cc1$BA)+(all8580cc1$AvgBADeadRate)*61

#remove negatives
all2625cc1$BA[all2625cc1$BA<=0] <- 0.01
all4525cc1$BA[all4525cc1$BA<=0] <- 0.01
all8525cc1$BA[all8525cc1$BA<=0] <- 0.01
all2630cc1$BA[all2630cc1$BA<=0] <- 0.01
all4530cc1$BA[all4530cc1$BA<=0] <- 0.01
all8530cc1$BA[all8530cc1$BA<=0] <- 0.01
all2635cc1$BA[all2635cc1$BA<=0] <- 0.01
all4535cc1$BA[all4535cc1$BA<=0] <- 0.01
all8535cc1$BA[all8535cc1$BA<=0] <- 0.01
all2640cc1$BA[all2640cc1$BA<=0] <- 0.01
all4540cc1$BA[all4540cc1$BA<=0] <- 0.01
all8540cc1$BA[all8540cc1$BA<=0] <- 0.01
all2645cc1$BA[all2645cc1$BA<=0] <- 0.01
all4545cc1$BA[all4545cc1$BA<=0] <- 0.01
all8545cc1$BA[all8545cc1$BA<=0] <- 0.01
all2650cc1$BA[all2650cc1$BA<=0] <- 0.01
all4550cc1$BA[all4550cc1$BA<=0] <- 0.01
all8550cc1$BA[all8550cc1$BA<=0] <- 0.01
all2655cc1$BA[all2655cc1$BA<=0] <- 0.01
all4555cc1$BA[all4555cc1$BA<=0] <- 0.01
all8555cc1$BA[all8555cc1$BA<=0] <- 0.01
all2660cc1$BA[all2660cc1$BA<=0] <- 0.01
all4560cc1$BA[all4560cc1$BA<=0] <- 0.01
all8560cc1$BA[all8560cc1$BA<=0] <- 0.01
all2665cc1$BA[all2665cc1$BA<=0] <- 0.01
all4565cc1$BA[all4565cc1$BA<=0] <- 0.01
all8565cc1$BA[all8565cc1$BA<=0] <- 0.01
all2670cc1$BA[all2670cc1$BA<=0] <- 0.01
all4570cc1$BA[all4570cc1$BA<=0] <- 0.01
all8570cc1$BA[all8570cc1$BA<=0] <- 0.01
all2675cc1$BA[all2675cc1$BA<=0] <- 0.01
all4575cc1$BA[all4575cc1$BA<=0] <- 0.01
all8575cc1$BA[all8575cc1$BA<=0] <- 0.01
all2680cc1$BA[all2680cc1$BA<=0] <- 0.01
all4580cc1$BA[all4580cc1$BA<=0] <- 0.01
all8580cc1$BA[all8580cc1$BA<=0] <- 0.01

#max basal area of dead trees = basal area of all trees. prevents mortality over 100%, not possible.
all2625cc1$BA <- ifelse(all2625cc1$BA > all2625cc1$BAAll, all2625cc1$BAAll, all2625cc1$BA)
all4525cc1$BA <- ifelse(all4525cc1$BA > all4525cc1$BAAll, all4525cc1$BAAll, all4525cc1$BA)
all8525cc1$BA <- ifelse(all8525cc1$BA > all8525cc1$BAAll, all8525cc1$BAAll, all8525cc1$BA)
all2630cc1$BA <- ifelse(all2630cc1$BA > all2630cc1$BAAll, all2630cc1$BAAll, all2630cc1$BA)
all4530cc1$BA <- ifelse(all4530cc1$BA > all4530cc1$BAAll, all4530cc1$BAAll, all4530cc1$BA)
all8530cc1$BA <- ifelse(all8530cc1$BA > all8530cc1$BAAll, all8530cc1$BAAll, all8530cc1$BA)
all2635cc1$BA <- ifelse(all2635cc1$BA > all2635cc1$BAAll, all2635cc1$BAAll, all2635cc1$BA)
all4535cc1$BA <- ifelse(all4535cc1$BA > all4535cc1$BAAll, all4535cc1$BAAll, all4535cc1$BA)
all8535cc1$BA <- ifelse(all8535cc1$BA > all8535cc1$BAAll, all8535cc1$BAAll, all8535cc1$BA)
all2640cc1$BA <- ifelse(all2640cc1$BA > all2640cc1$BAAll, all2640cc1$BAAll, all2640cc1$BA)
all4540cc1$BA <- ifelse(all4540cc1$BA > all4540cc1$BAAll, all4540cc1$BAAll, all4540cc1$BA)
all8540cc1$BA <- ifelse(all8540cc1$BA > all8540cc1$BAAll, all8540cc1$BAAll, all8540cc1$BA)
all2645cc1$BA <- ifelse(all2645cc1$BA > all2645cc1$BAAll, all2645cc1$BAAll, all2645cc1$BA)
all4545cc1$BA <- ifelse(all4545cc1$BA > all4545cc1$BAAll, all4545cc1$BAAll, all4545cc1$BA)
all8545cc1$BA <- ifelse(all8545cc1$BA > all8545cc1$BAAll, all8545cc1$BAAll, all8545cc1$BA)
all2650cc1$BA <- ifelse(all2650cc1$BA > all2650cc1$BAAll, all2650cc1$BAAll, all2650cc1$BA)
all4550cc1$BA <- ifelse(all4550cc1$BA > all4550cc1$BAAll, all4550cc1$BAAll, all4550cc1$BA)
all8550cc1$BA <- ifelse(all8550cc1$BA > all8550cc1$BAAll, all8550cc1$BAAll, all8550cc1$BA)
all2655cc1$BA <- ifelse(all2655cc1$BA > all2655cc1$BAAll, all2655cc1$BAAll, all2655cc1$BA)
all4555cc1$BA <- ifelse(all4555cc1$BA > all4555cc1$BAAll, all4555cc1$BAAll, all4555cc1$BA)
all8555cc1$BA <- ifelse(all8555cc1$BA > all8555cc1$BAAll, all8555cc1$BAAll, all8555cc1$BA)
all2660cc1$BA <- ifelse(all2660cc1$BA > all2660cc1$BAAll, all2660cc1$BAAll, all2660cc1$BA)
all4560cc1$BA <- ifelse(all4560cc1$BA > all4560cc1$BAAll, all4560cc1$BAAll, all4560cc1$BA)
all8560cc1$BA <- ifelse(all8560cc1$BA > all8560cc1$BAAll, all8560cc1$BAAll, all8560cc1$BA)
all2665cc1$BA <- ifelse(all2665cc1$BA > all2665cc1$BAAll, all2665cc1$BAAll, all2665cc1$BA)
all4565cc1$BA <- ifelse(all4565cc1$BA > all4565cc1$BAAll, all4565cc1$BAAll, all4565cc1$BA)
all8565cc1$BA <- ifelse(all8565cc1$BA > all8565cc1$BAAll, all8565cc1$BAAll, all8565cc1$BA)
all2670cc1$BA <- ifelse(all2670cc1$BA > all2670cc1$BAAll, all2670cc1$BAAll, all2670cc1$BA)
all4570cc1$BA <- ifelse(all4570cc1$BA > all4570cc1$BAAll, all4570cc1$BAAll, all4570cc1$BA)
all8570cc1$BA <- ifelse(all8570cc1$BA > all8570cc1$BAAll, all8570cc1$BAAll, all8570cc1$BA)
all2675cc1$BA <- ifelse(all2675cc1$BA > all2675cc1$BAAll, all2675cc1$BAAll, all2675cc1$BA)
all4575cc1$BA <- ifelse(all4575cc1$BA > all4575cc1$BAAll, all4575cc1$BAAll, all4575cc1$BA)
all8575cc1$BA <- ifelse(all8575cc1$BA > all8575cc1$BAAll, all8575cc1$BAAll, all8575cc1$BA)
all2680cc1$BA <- ifelse(all2680cc1$BA > all2680cc1$BAAll, all2680cc1$BAAll, all2680cc1$BA)
all4580cc1$BA <- ifelse(all4580cc1$BA > all4580cc1$BAAll, all4580cc1$BAAll, all4580cc1$BA)
all8580cc1$BA <- ifelse(all8580cc1$BA > all8580cc1$BAAll, all8580cc1$BAAll, all8580cc1$BA)
#scale basal area
all2625cc1 $BA<- as.data.frame(scale(all2625cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4525cc1 $BA<- as.data.frame(scale(all4525cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8525cc1 $BA<- as.data.frame(scale(all8525cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all2630cc1 $BA <- as.data.frame(scale(all2630cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4530cc1 $BA <- as.data.frame(scale(all4530cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8530cc1 $BA <- as.data.frame(scale(all8530cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all2635cc1 $BA<- as.data.frame(scale(all2635cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4535cc1 $BA<- as.data.frame(scale(all4535cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8535cc1 $BA<- as.data.frame(scale(all8535cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all2640cc1 $BA <- as.data.frame(scale(all2640cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4540cc1 $BA <- as.data.frame(scale(all4540cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8540cc1 $BA <- as.data.frame(scale(all8540cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all2645cc1 $BA<- as.data.frame(scale(all2645cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4545cc1 $BA<- as.data.frame(scale(all4545cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8545cc1 $BA<- as.data.frame(scale(all8545cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all2650cc1 $BA <- as.data.frame(scale(all2650cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4550cc1 $BA <- as.data.frame(scale(all4550cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8550cc1 $BA <- as.data.frame(scale(all8550cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all2655cc1 $BA<- as.data.frame(scale(all2655cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4555cc1 $BA<- as.data.frame(scale(all4555cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8555cc1 $BA<- as.data.frame(scale(all8555cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all2660cc1 $BA <- as.data.frame(scale(all2660cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4560cc1 $BA <- as.data.frame(scale(all4560cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8560cc1 $BA <- as.data.frame(scale(all8560cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all2665cc1 $BA<- as.data.frame(scale(all2665cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4565cc1 $BA<- as.data.frame(scale(all4565cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8565cc1 $BA<- as.data.frame(scale(all8565cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all2670cc1 $BA <- as.data.frame(scale(all2670cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4570cc1 $BA <- as.data.frame(scale(all4570cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8570cc1 $BA <- as.data.frame(scale(all8570cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all2675cc1 $BA<- as.data.frame(scale(all2675cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4575cc1 $BA<- as.data.frame(scale(all4575cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8575cc1 $BA<- as.data.frame(scale(all8575cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all2680cc1 $BA <- as.data.frame(scale(all2680cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all4580cc1 $BA <- as.data.frame(scale(all4580cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]
all8580cc1 $BA <- as.data.frame(scale(all8580cc1 $BA, center=centerBA1,scale=scaleBA1),col.names="BA")[, 1]

#1.9850653 most recent year weight
all2625cc1$time_weight <- 1.9850653
all4525cc1$time_weight <- 1.9850653
all8525cc1$time_weight <- 1.9850653
all2630cc1$time_weight <- 1.9850653
all4530cc1$time_weight <- 1.9850653
all8530cc1$time_weight <- 1.9850653
all2635cc1$time_weight <- 1.9850653
all4535cc1$time_weight <- 1.9850653
all8535cc1$time_weight <- 1.9850653
all2640cc1$time_weight <- 1.9850653
all4540cc1$time_weight <- 1.9850653
all8540cc1$time_weight <- 1.9850653
all2645cc1$time_weight <- 1.9850653
all4545cc1$time_weight <- 1.9850653
all8545cc1$time_weight <- 1.9850653
all2650cc1$time_weight <- 1.9850653
all4550cc1$time_weight <- 1.9850653
all8550cc1$time_weight <- 1.9850653
all2655cc1$time_weight <- 1.9850653
all4555cc1$time_weight <- 1.9850653
all8555cc1$time_weight <- 1.9850653
all2660cc1$time_weight <- 1.9850653
all4560cc1$time_weight <- 1.9850653
all8560cc1$time_weight <- 1.9850653
all2665cc1$time_weight <- 1.9850653
all4565cc1$time_weight <- 1.9850653
all8565cc1$time_weight <- 1.9850653
all2670cc1$time_weight <- 1.9850653
all4570cc1$time_weight <- 1.9850653
all8570cc1$time_weight <- 1.9850653
all2675cc1$time_weight <- 1.9850653
all4575cc1$time_weight <- 1.9850653
all8575cc1$time_weight <- 1.9850653
all2680cc1$time_weight <- 1.9850653
all4580cc1$time_weight <- 1.9850653
all8580cc1$time_weight <- 1.9850653



#run simulations for each year 2025-2080
simCarb2625 <- link(CarbSplit1, data=all2625cc1 )
simmeanCarb2625 <- data.frame(apply(simCarb2625,2,mean))
PICarb2625 <- t(data.frame(apply(simCarb2625,2,PI,prob=0.89)))
F1CarbSim2625 <- data.frame(cbind(simmeanCarb2625,PICarb2625))
F1CarbSim2625$PlotCN <- all2625cc1  $PlotCN
F1CarbSim2625 <- F1CarbSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2625) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4525 <- link(CarbSplit1, data=all4525cc1 )
simmeanCarb4525 <- data.frame(apply(simCarb4525,2,mean))
PICarb4525 <- t(data.frame(apply(simCarb4525,2,PI,prob=0.89)))
F1CarbSim4525 <- data.frame(cbind(simmeanCarb4525,PICarb4525))
F1CarbSim4525$PlotCN <- all4525cc1  $PlotCN
F1CarbSim4525 <- F1CarbSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4525) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8525 <- link(CarbSplit1, data=all8525cc1 )
simmeanCarb8525 <- data.frame(apply(simCarb8525,2,mean))
PICarb8525 <- t(data.frame(apply(simCarb8525,2,PI,prob=0.89)))
F1CarbSim8525 <- data.frame(cbind(simmeanCarb8525,PICarb8525))
F1CarbSim8525$PlotCN <- all8525cc1  $PlotCN
F1CarbSim8525 <- F1CarbSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8525) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2630 <- link(CarbSplit1, data=all2630cc1 )
simmeanCarb2630 <- data.frame(apply(simCarb2630,2,mean))
PICarb2630 <- t(data.frame(apply(simCarb2630,2,PI,prob=0.89)))
F1CarbSim2630 <- data.frame(cbind(simmeanCarb2630,PICarb2630))
F1CarbSim2630$PlotCN <- all2630cc1  $PlotCN
F1CarbSim2630 <- F1CarbSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2630) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4530 <- link(CarbSplit1, data=all4530cc1 )
simmeanCarb4530 <- data.frame(apply(simCarb4530,2,mean))
PICarb4530 <- t(data.frame(apply(simCarb4530,2,PI,prob=0.89)))
F1CarbSim4530 <- data.frame(cbind(simmeanCarb4530,PICarb4530))
F1CarbSim4530$PlotCN <- all4530cc1  $PlotCN
F1CarbSim4530 <- F1CarbSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4530) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8530 <- link(CarbSplit1, data=all8530cc1 )
simmeanCarb8530 <- data.frame(apply(simCarb8530,2,mean))
PICarb8530 <- t(data.frame(apply(simCarb8530,2,PI,prob=0.89)))
F1CarbSim8530 <- data.frame(cbind(simmeanCarb8530,PICarb8530))
F1CarbSim8530$PlotCN <- all8530cc1  $PlotCN
F1CarbSim8530 <- F1CarbSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8530) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2635 <- link(CarbSplit1, data=all2635cc1 )
simmeanCarb2635 <- data.frame(apply(simCarb2635,2,mean))
PICarb2635 <- t(data.frame(apply(simCarb2635,2,PI,prob=0.89)))
F1CarbSim2635 <- data.frame(cbind(simmeanCarb2635,PICarb2635))
F1CarbSim2635$PlotCN <- all2635cc1  $PlotCN
F1CarbSim2635 <- F1CarbSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2635) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4535 <- link(CarbSplit1, data=all4535cc1 )
simmeanCarb4535 <- data.frame(apply(simCarb4535,2,mean))
PICarb4535 <- t(data.frame(apply(simCarb4535,2,PI,prob=0.89)))
F1CarbSim4535 <- data.frame(cbind(simmeanCarb4535,PICarb4535))
F1CarbSim4535$PlotCN <- all4535cc1  $PlotCN
F1CarbSim4535 <- F1CarbSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4535) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8535 <- link(CarbSplit1, data=all8535cc1 )
simmeanCarb8535 <- data.frame(apply(simCarb8535,2,mean))
PICarb8535 <- t(data.frame(apply(simCarb8535,2,PI,prob=0.89)))
F1CarbSim8535 <- data.frame(cbind(simmeanCarb8535,PICarb8535))
F1CarbSim8535$PlotCN <- all8535cc1  $PlotCN
F1CarbSim8535 <- F1CarbSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8535) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2640 <- link(CarbSplit1, data=all2640cc1 )
simmeanCarb2640 <- data.frame(apply(simCarb2640,2,mean))
PICarb2640 <- t(data.frame(apply(simCarb2640,2,PI,prob=0.89)))
F1CarbSim2640 <- data.frame(cbind(simmeanCarb2640,PICarb2640))
F1CarbSim2640$PlotCN <- all2640cc1  $PlotCN
F1CarbSim2640 <- F1CarbSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2640) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4540 <- link(CarbSplit1, data=all4540cc1 )
simmeanCarb4540 <- data.frame(apply(simCarb4540,2,mean))
PICarb4540 <- t(data.frame(apply(simCarb4540,2,PI,prob=0.89)))
F1CarbSim4540 <- data.frame(cbind(simmeanCarb4540,PICarb4540))
F1CarbSim4540$PlotCN <- all4540cc1  $PlotCN
F1CarbSim4540 <- F1CarbSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4540) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8540 <- link(CarbSplit1, data=all8540cc1 )
simmeanCarb8540 <- data.frame(apply(simCarb8540,2,mean))
PICarb8540 <- t(data.frame(apply(simCarb8540,2,PI,prob=0.89)))
F1CarbSim8540 <- data.frame(cbind(simmeanCarb8540,PICarb8540))
F1CarbSim8540$PlotCN <- all8540cc1  $PlotCN
F1CarbSim8540 <- F1CarbSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8540) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2645 <- link(CarbSplit1, data=all2645cc1 )
simmeanCarb2645 <- data.frame(apply(simCarb2645,2,mean))
PICarb2645 <- t(data.frame(apply(simCarb2645,2,PI,prob=0.89)))
F1CarbSim2645 <- data.frame(cbind(simmeanCarb2645,PICarb2645))
F1CarbSim2645$PlotCN <- all2645cc1  $PlotCN
F1CarbSim2645 <- F1CarbSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2645) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4545 <- link(CarbSplit1, data=all4545cc1 )
simmeanCarb4545 <- data.frame(apply(simCarb4545,2,mean))
PICarb4545 <- t(data.frame(apply(simCarb4545,2,PI,prob=0.89)))
F1CarbSim4545 <- data.frame(cbind(simmeanCarb4545,PICarb4545))
F1CarbSim4545$PlotCN <- all4545cc1  $PlotCN
F1CarbSim4545 <- F1CarbSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4545) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8545 <- link(CarbSplit1, data=all8545cc1 )
simmeanCarb8545 <- data.frame(apply(simCarb8545,2,mean))
PICarb8545 <- t(data.frame(apply(simCarb8545,2,PI,prob=0.89)))
F1CarbSim8545 <- data.frame(cbind(simmeanCarb8545,PICarb8545))
F1CarbSim8545$PlotCN <- all8545cc1  $PlotCN
F1CarbSim8545 <- F1CarbSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8545) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2650 <- link(CarbSplit1, data=all2650cc1 )
simmeanCarb2650 <- data.frame(apply(simCarb2650,2,mean))
PICarb2650 <- t(data.frame(apply(simCarb2650,2,PI,prob=0.89)))
F1CarbSim2650 <- data.frame(cbind(simmeanCarb2650,PICarb2650))
F1CarbSim2650$PlotCN <- all2650cc1  $PlotCN
F1CarbSim2650 <- F1CarbSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2650) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4550 <- link(CarbSplit1, data=all4550cc1 )
simmeanCarb4550 <- data.frame(apply(simCarb4550,2,mean))
PICarb4550 <- t(data.frame(apply(simCarb4550,2,PI,prob=0.89)))
F1CarbSim4550 <- data.frame(cbind(simmeanCarb4550,PICarb4550))
F1CarbSim4550$PlotCN <- all4550cc1  $PlotCN
F1CarbSim4550 <- F1CarbSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4550) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8550 <- link(CarbSplit1, data=all8550cc1 )
simmeanCarb8550 <- data.frame(apply(simCarb8550,2,mean))
PICarb8550 <- t(data.frame(apply(simCarb8550,2,PI,prob=0.89)))
F1CarbSim8550 <- data.frame(cbind(simmeanCarb8550,PICarb8550))
F1CarbSim8550$PlotCN <- all8550cc1  $PlotCN
F1CarbSim8550 <- F1CarbSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8550) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2655 <- link(CarbSplit1, data=all2655cc1 )
simmeanCarb2655 <- data.frame(apply(simCarb2655,2,mean))
PICarb2655 <- t(data.frame(apply(simCarb2655,2,PI,prob=0.89)))
F1CarbSim2655 <- data.frame(cbind(simmeanCarb2655,PICarb2655))
F1CarbSim2655$PlotCN <- all2655cc1  $PlotCN
F1CarbSim2655 <- F1CarbSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2655) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4555 <- link(CarbSplit1, data=all4555cc1 )
simmeanCarb4555 <- data.frame(apply(simCarb4555,2,mean))
PICarb4555 <- t(data.frame(apply(simCarb4555,2,PI,prob=0.89)))
F1CarbSim4555 <- data.frame(cbind(simmeanCarb4555,PICarb4555))
F1CarbSim4555$PlotCN <- all4555cc1  $PlotCN
F1CarbSim4555 <- F1CarbSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4555) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8555 <- link(CarbSplit1, data=all8555cc1 )
simmeanCarb8555 <- data.frame(apply(simCarb8555,2,mean))
PICarb8555 <- t(data.frame(apply(simCarb8555,2,PI,prob=0.89)))
F1CarbSim8555 <- data.frame(cbind(simmeanCarb8555,PICarb8555))
F1CarbSim8555$PlotCN <- all8555cc1  $PlotCN
F1CarbSim8555 <- F1CarbSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8555) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2660 <- link(CarbSplit1, data=all2660cc1 )
simmeanCarb2660 <- data.frame(apply(simCarb2660,2,mean))
PICarb2660 <- t(data.frame(apply(simCarb2660,2,PI,prob=0.89)))
F1CarbSim2660 <- data.frame(cbind(simmeanCarb2660,PICarb2660))
F1CarbSim2660$PlotCN <- all2660cc1  $PlotCN
F1CarbSim2660 <- F1CarbSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2660) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4560 <- link(CarbSplit1, data=all4560cc1 )
simmeanCarb4560 <- data.frame(apply(simCarb4560,2,mean))
PICarb4560 <- t(data.frame(apply(simCarb4560,2,PI,prob=0.89)))
F1CarbSim4560 <- data.frame(cbind(simmeanCarb4560,PICarb4560))
F1CarbSim4560$PlotCN <- all4560cc1  $PlotCN
F1CarbSim4560 <- F1CarbSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4560) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8560 <- link(CarbSplit1, data=all8560cc1 )
simmeanCarb8560 <- data.frame(apply(simCarb8560,2,mean))
PICarb8560 <- t(data.frame(apply(simCarb8560,2,PI,prob=0.89)))
F1CarbSim8560 <- data.frame(cbind(simmeanCarb8560,PICarb8560))
F1CarbSim8560$PlotCN <- all8560cc1  $PlotCN
F1CarbSim8560 <- F1CarbSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8560) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2665 <- link(CarbSplit1, data=all2665cc1 )
simmeanCarb2665 <- data.frame(apply(simCarb2665,2,mean))
PICarb2665 <- t(data.frame(apply(simCarb2665,2,PI,prob=0.89)))
F1CarbSim2665 <- data.frame(cbind(simmeanCarb2665,PICarb2665))
F1CarbSim2665$PlotCN <- all2665cc1  $PlotCN
F1CarbSim2665 <- F1CarbSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2665) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4565 <- link(CarbSplit1, data=all4565cc1 )
simmeanCarb4565 <- data.frame(apply(simCarb4565,2,mean))
PICarb4565 <- t(data.frame(apply(simCarb4565,2,PI,prob=0.89)))
F1CarbSim4565 <- data.frame(cbind(simmeanCarb4565,PICarb4565))
F1CarbSim4565$PlotCN <- all4565cc1  $PlotCN
F1CarbSim4565 <- F1CarbSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4565) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8565 <- link(CarbSplit1, data=all8565cc1 )
simmeanCarb8565 <- data.frame(apply(simCarb8565,2,mean))
PICarb8565 <- t(data.frame(apply(simCarb8565,2,PI,prob=0.89)))
F1CarbSim8565 <- data.frame(cbind(simmeanCarb8565,PICarb8565))
F1CarbSim8565$PlotCN <- all8565cc1  $PlotCN
F1CarbSim8565 <- F1CarbSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8565) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2670 <- link(CarbSplit1, data=all2670cc1 )
simmeanCarb2670 <- data.frame(apply(simCarb2670,2,mean))
PICarb2670 <- t(data.frame(apply(simCarb2670,2,PI,prob=0.89)))
F1CarbSim2670 <- data.frame(cbind(simmeanCarb2670,PICarb2670))
F1CarbSim2670$PlotCN <- all2670cc1  $PlotCN
F1CarbSim2670 <- F1CarbSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2670) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4570 <- link(CarbSplit1, data=all4570cc1 )
simmeanCarb4570 <- data.frame(apply(simCarb4570,2,mean))
PICarb4570 <- t(data.frame(apply(simCarb4570,2,PI,prob=0.89)))
F1CarbSim4570 <- data.frame(cbind(simmeanCarb4570,PICarb4570))
F1CarbSim4570$PlotCN <- all4570cc1  $PlotCN
F1CarbSim4570 <- F1CarbSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4570) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8570 <- link(CarbSplit1, data=all8570cc1 )
simmeanCarb8570 <- data.frame(apply(simCarb8570,2,mean))
PICarb8570 <- t(data.frame(apply(simCarb8570,2,PI,prob=0.89)))
F1CarbSim8570 <- data.frame(cbind(simmeanCarb8570,PICarb8570))
F1CarbSim8570$PlotCN <- all8570cc1  $PlotCN
F1CarbSim8570 <- F1CarbSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8570) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2675 <- link(CarbSplit1, data=all2675cc1 )
simmeanCarb2675 <- data.frame(apply(simCarb2675,2,mean))
PICarb2675 <- t(data.frame(apply(simCarb2675,2,PI,prob=0.89)))
F1CarbSim2675 <- data.frame(cbind(simmeanCarb2675,PICarb2675))
F1CarbSim2675$PlotCN <- all2675cc1  $PlotCN
F1CarbSim2675 <- F1CarbSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2675) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4575 <- link(CarbSplit1, data=all4575cc1 )
simmeanCarb4575 <- data.frame(apply(simCarb4575,2,mean))
PICarb4575 <- t(data.frame(apply(simCarb4575,2,PI,prob=0.89)))
F1CarbSim4575 <- data.frame(cbind(simmeanCarb4575,PICarb4575))
F1CarbSim4575$PlotCN <- all4575cc1  $PlotCN
F1CarbSim4575 <- F1CarbSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4575) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8575 <- link(CarbSplit1, data=all8575cc1 )
simmeanCarb8575 <- data.frame(apply(simCarb8575,2,mean))
PICarb8575 <- t(data.frame(apply(simCarb8575,2,PI,prob=0.89)))
F1CarbSim8575 <- data.frame(cbind(simmeanCarb8575,PICarb8575))
F1CarbSim8575$PlotCN <- all8575cc1  $PlotCN
F1CarbSim8575 <- F1CarbSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8575) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2680 <- link(CarbSplit1, data=all2680cc1 )
simmeanCarb2680 <- data.frame(apply(simCarb2680,2,mean))
PICarb2680 <- t(data.frame(apply(simCarb2680,2,PI,prob=0.89)))
F1CarbSim2680 <- data.frame(cbind(simmeanCarb2680,PICarb2680))
F1CarbSim2680$PlotCN <- all2680cc1  $PlotCN
F1CarbSim2680 <- F1CarbSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim2680) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4580 <- link(CarbSplit1, data=all4580cc1 )
simmeanCarb4580 <- data.frame(apply(simCarb4580,2,mean))
PICarb4580 <- t(data.frame(apply(simCarb4580,2,PI,prob=0.89)))
F1CarbSim4580 <- data.frame(cbind(simmeanCarb4580,PICarb4580))
F1CarbSim4580$PlotCN <- all4580cc1  $PlotCN
F1CarbSim4580 <- F1CarbSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim4580) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8580 <- link(CarbSplit1, data=all8580cc1 )
simmeanCarb8580 <- data.frame(apply(simCarb8580,2,mean))
PICarb8580 <- t(data.frame(apply(simCarb8580,2,PI,prob=0.89)))
F1CarbSim8580 <- data.frame(cbind(simmeanCarb8580,PICarb8580))
F1CarbSim8580$PlotCN <- all8580cc1  $PlotCN
F1CarbSim8580 <- F1CarbSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F1CarbSim8580) <- c("PlotCN","CarbPerAcre","5CI","95CI")

#create vector of new colnames
CarbSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Carb", "5CI", "95CI"), x)))
CarbSimnames<- append(CarbSimnames,"PlotCN",after=0)

#group data by RCP pathway
F1CarbSim26 <- F1CarbSim2625 %>% cbind(F1CarbSim2630[,2:4]) %>%
  cbind(F1CarbSim2635[,2:4]) %>% cbind(F1CarbSim2640[,2:4]) %>% cbind(F1CarbSim2645[,2:4]) %>%
  cbind(F1CarbSim2650[,2:4]) %>% cbind(F1CarbSim2655[,2:4]) %>% cbind(F1CarbSim2660[,2:4]) %>%
  cbind(F1CarbSim2665[,2:4]) %>% cbind(F1CarbSim2670[,2:4]) %>% cbind(F1CarbSim2675[,2:4]) %>%
  cbind(F1CarbSim2680[,2:4])
colnames(F1CarbSim26) <- CarbSimnames
write.csv(F1CarbSim26,file="F1CarbPred26.csv")

F1CarbSim45 <- F1CarbSim4525 %>% cbind(F1CarbSim4530[,2:4]) %>%
  cbind(F1CarbSim4535[,2:4]) %>% cbind(F1CarbSim4540[,2:4]) %>% cbind(F1CarbSim4545[,2:4]) %>%
  cbind(F1CarbSim4550[,2:4]) %>% cbind(F1CarbSim4555[,2:4]) %>% cbind(F1CarbSim4560[,2:4]) %>%
  cbind(F1CarbSim4565[,2:4]) %>% cbind(F1CarbSim4570[,2:4]) %>% cbind(F1CarbSim4575[,2:4]) %>%
  cbind(F1CarbSim4580[,2:4])
colnames(F1CarbSim45) <- CarbSimnames
write.csv(F1CarbSim45,file="F1CarbPred45.csv")

F1CarbSim85 <- F1CarbSim8525 %>% cbind(F1CarbSim8530[,2:4]) %>%
  cbind(F1CarbSim8535[,2:4]) %>% cbind(F1CarbSim8540[,2:4]) %>% cbind(F1CarbSim8545[,2:4]) %>%
  cbind(F1CarbSim8550[,2:4]) %>% cbind(F1CarbSim8555[,2:4]) %>% cbind(F1CarbSim8560[,2:4]) %>%
  cbind(F1CarbSim8565[,2:4]) %>% cbind(F1CarbSim8570[,2:4]) %>% cbind(F1CarbSim8575[,2:4]) %>%
  cbind(F1CarbSim8580[,2:4])
colnames(F1CarbSim85) <- CarbSimnames
write.csv(F1CarbSim85,file="F1CarbPred85.csv")


#fgroup 5
all2625cc5 <- all2625cc %>% filter(ForestGroup==5)
all4525cc5 <- all4525cc %>% filter(ForestGroup==5)
all8525cc5 <- all8525cc %>% filter(ForestGroup==5)
all2630cc5 <- all2630cc %>% filter(ForestGroup==5)
all4530cc5 <- all4530cc %>% filter(ForestGroup==5)
all8530cc5 <- all8530cc %>% filter(ForestGroup==5)
all2635cc5 <- all2635cc %>% filter(ForestGroup==5)
all4535cc5 <- all4535cc %>% filter(ForestGroup==5)
all8535cc5 <- all8535cc %>% filter(ForestGroup==5)
all2640cc5 <- all2640cc %>% filter(ForestGroup==5)
all4540cc5 <- all4540cc %>% filter(ForestGroup==5)
all8540cc5 <- all8540cc %>% filter(ForestGroup==5)
all2645cc5 <- all2645cc %>% filter(ForestGroup==5)
all4545cc5 <- all4545cc %>% filter(ForestGroup==5)
all8545cc5 <- all8545cc %>% filter(ForestGroup==5)
all2650cc5 <- all2650cc %>% filter(ForestGroup==5)
all4550cc5 <- all4550cc %>% filter(ForestGroup==5)
all8550cc5 <- all8550cc %>% filter(ForestGroup==5)
all2655cc5 <- all2655cc %>% filter(ForestGroup==5)
all4555cc5 <- all4555cc %>% filter(ForestGroup==5)
all8555cc5 <- all8555cc %>% filter(ForestGroup==5)
all2660cc5 <- all2660cc %>% filter(ForestGroup==5)
all4560cc5 <- all4560cc %>% filter(ForestGroup==5)
all8560cc5 <- all8560cc %>% filter(ForestGroup==5)
all2665cc5 <- all2665cc %>% filter(ForestGroup==5)
all4565cc5 <- all4565cc %>% filter(ForestGroup==5)
all8565cc5 <- all8565cc %>% filter(ForestGroup==5)
all2670cc5 <- all2670cc %>% filter(ForestGroup==5)
all4570cc5 <- all4570cc %>% filter(ForestGroup==5)
all8570cc5 <- all8570cc %>% filter(ForestGroup==5)
all2675cc5 <- all2675cc %>% filter(ForestGroup==5)
all4575cc5 <- all4575cc %>% filter(ForestGroup==5)
all8575cc5 <- all8575cc %>% filter(ForestGroup==5)
all2680cc5 <- all2680cc %>% filter(ForestGroup==5)
all4580cc5 <- all4580cc %>% filter(ForestGroup==5)
all8580cc5 <- all8580cc %>% filter(ForestGroup==5)

#turn soil order and forest type into factors
all2625cc5$Soil <- as.factor(all2625cc5$soil_order)
all2625cc5$Soil <- as.integer(all2625cc5$Soil)
all2625cc5$FType <- as.factor(all2625cc5$ForestType)
all2625cc5$FType <- as.integer(all2625cc5$FType)
all4525cc5$Soil <- as.factor(all4525cc5$soil_order)
all4525cc5$Soil <- as.integer(all4525cc5$Soil)
all4525cc5$FType <- as.factor(all4525cc5$ForestType)
all4525cc5$FType <- as.integer(all4525cc5$FType)
all8525cc5$Soil <- as.factor(all8525cc5$soil_order)
all8525cc5$Soil <- as.integer(all8525cc5$Soil)
all8525cc5$FType <- as.factor(all8525cc5$ForestType)
all8525cc5$FType <- as.integer(all8525cc5$FType)

all2630cc5$Soil <- as.factor(all2630cc5$soil_order)
all2630cc5$Soil <- as.integer(all2630cc5$Soil)
all2630cc5$FType <- as.factor(all2630cc5$ForestType)
all2630cc5$FType <- as.integer(all2630cc5$FType)
all4530cc5$Soil <- as.factor(all4530cc5$soil_order)
all4530cc5$Soil <- as.integer(all4530cc5$Soil)
all4530cc5$FType <- as.factor(all4530cc5$ForestType)
all4530cc5$FType <- as.integer(all4530cc5$FType)
all8530cc5$Soil <- as.factor(all8530cc5$soil_order)
all8530cc5$Soil <- as.integer(all8530cc5$Soil)
all8530cc5$FType <- as.factor(all8530cc5$ForestType)
all8530cc5$FType <- as.integer(all8530cc5$FType)

all2635cc5$Soil <- as.factor(all2635cc5$soil_order)
all2635cc5$Soil <- as.integer(all2635cc5$Soil)
all2635cc5$FType <- as.factor(all2635cc5$ForestType)
all2635cc5$FType <- as.integer(all2635cc5$FType)
all4535cc5$Soil <- as.factor(all4535cc5$soil_order)
all4535cc5$Soil <- as.integer(all4535cc5$Soil)
all4535cc5$FType <- as.factor(all4535cc5$ForestType)
all4535cc5$FType <- as.integer(all4535cc5$FType)
all8535cc5$Soil <- as.factor(all8535cc5$soil_order)
all8535cc5$Soil <- as.integer(all8535cc5$Soil)
all8535cc5$FType <- as.factor(all8535cc5$ForestType)
all8535cc5$FType <- as.integer(all8535cc5$FType)


all2640cc5$Soil <- as.factor(all2640cc5$soil_order)
all2640cc5$Soil <- as.integer(all2640cc5$Soil)
all2640cc5$FType <- as.factor(all2640cc5$ForestType)
all2640cc5$FType <- as.integer(all2640cc5$FType)
all4540cc5$Soil <- as.factor(all4540cc5$soil_order)
all4540cc5$Soil <- as.integer(all4540cc5$Soil)
all4540cc5$FType <- as.factor(all4540cc5$ForestType)
all4540cc5$FType <- as.integer(all4540cc5$FType)
all8540cc5$Soil <- as.factor(all8540cc5$soil_order)
all8540cc5$Soil <- as.integer(all8540cc5$Soil)
all8540cc5$FType <- as.factor(all8540cc5$ForestType)
all8540cc5$FType <- as.integer(all8540cc5$FType)

all2645cc5$Soil <- as.factor(all2645cc5$soil_order)
all2645cc5$Soil <- as.integer(all2645cc5$Soil)
all2645cc5$FType <- as.factor(all2645cc5$ForestType)
all2645cc5$FType <- as.integer(all2645cc5$FType)
all4545cc5$Soil <- as.factor(all4545cc5$soil_order)
all4545cc5$Soil <- as.integer(all4545cc5$Soil)
all4545cc5$FType <- as.factor(all4545cc5$ForestType)
all4545cc5$FType <- as.integer(all4545cc5$FType)
all8545cc5$Soil <- as.factor(all8545cc5$soil_order)
all8545cc5$Soil <- as.integer(all8545cc5$Soil)
all8545cc5$FType <- as.factor(all8545cc5$ForestType)
all8545cc5$FType <- as.integer(all8545cc5$FType)

all2650cc5$Soil <- as.factor(all2650cc5$soil_order)
all2650cc5$Soil <- as.integer(all2650cc5$Soil)
all2650cc5$FType <- as.factor(all2650cc5$ForestType)
all2650cc5$FType <- as.integer(all2650cc5$FType)
all4550cc5$Soil <- as.factor(all4550cc5$soil_order)
all4550cc5$Soil <- as.integer(all4550cc5$Soil)
all4550cc5$FType <- as.factor(all4550cc5$ForestType)
all4550cc5$FType <- as.integer(all4550cc5$FType)
all8550cc5$Soil <- as.factor(all8550cc5$soil_order)
all8550cc5$Soil <- as.integer(all8550cc5$Soil)
all8550cc5$FType <- as.factor(all8550cc5$ForestType)
all8550cc5$FType <- as.integer(all8550cc5$FType)

all2655cc5$Soil <- as.factor(all2655cc5$soil_order)
all2655cc5$Soil <- as.integer(all2655cc5$Soil)
all2655cc5$FType <- as.factor(all2655cc5$ForestType)
all2655cc5$FType <- as.integer(all2655cc5$FType)
all4555cc5$Soil <- as.factor(all4555cc5$soil_order)
all4555cc5$Soil <- as.integer(all4555cc5$Soil)
all4555cc5$FType <- as.factor(all4555cc5$ForestType)
all4555cc5$FType <- as.integer(all4555cc5$FType)
all8555cc5$Soil <- as.factor(all8555cc5$soil_order)
all8555cc5$Soil <- as.integer(all8555cc5$Soil)
all8555cc5$FType <- as.factor(all8555cc5$ForestType)
all8555cc5$FType <- as.integer(all8555cc5$FType)


all2660cc5$Soil <- as.factor(all2660cc5$soil_order)
all2660cc5$Soil <- as.integer(all2660cc5$Soil)
all2660cc5$FType <- as.factor(all2660cc5$ForestType)
all2660cc5$FType <- as.integer(all2660cc5$FType)
all4560cc5$Soil <- as.factor(all4560cc5$soil_order)
all4560cc5$Soil <- as.integer(all4560cc5$Soil)
all4560cc5$FType <- as.factor(all4560cc5$ForestType)
all4560cc5$FType <- as.integer(all4560cc5$FType)
all8560cc5$Soil <- as.factor(all8560cc5$soil_order)
all8560cc5$Soil <- as.integer(all8560cc5$Soil)
all8560cc5$FType <- as.factor(all8560cc5$ForestType)
all8560cc5$FType <- as.integer(all8560cc5$FType)

all2665cc5$Soil <- as.factor(all2665cc5$soil_order)
all2665cc5$Soil <- as.integer(all2665cc5$Soil)
all2665cc5$FType <- as.factor(all2665cc5$ForestType)
all2665cc5$FType <- as.integer(all2665cc5$FType)
all4565cc5$Soil <- as.factor(all4565cc5$soil_order)
all4565cc5$Soil <- as.integer(all4565cc5$Soil)
all4565cc5$FType <- as.factor(all4565cc5$ForestType)
all4565cc5$FType <- as.integer(all4565cc5$FType)
all8565cc5$Soil <- as.factor(all8565cc5$soil_order)
all8565cc5$Soil <- as.integer(all8565cc5$Soil)
all8565cc5$FType <- as.factor(all8565cc5$ForestType)
all8565cc5$FType <- as.integer(all8565cc5$FType)

all2670cc5$Soil <- as.factor(all2670cc5$soil_order)
all2670cc5$Soil <- as.integer(all2670cc5$Soil)
all2670cc5$FType <- as.factor(all2670cc5$ForestType)
all2670cc5$FType <- as.integer(all2670cc5$FType)
all4570cc5$Soil <- as.factor(all4570cc5$soil_order)
all4570cc5$Soil <- as.integer(all4570cc5$Soil)
all4570cc5$FType <- as.factor(all4570cc5$ForestType)
all4570cc5$FType <- as.integer(all4570cc5$FType)
all8570cc5$Soil <- as.factor(all8570cc5$soil_order)
all8570cc5$Soil <- as.integer(all8570cc5$Soil)
all8570cc5$FType <- as.factor(all8570cc5$ForestType)
all8570cc5$FType <- as.integer(all8570cc5$FType)


all2675cc5$Soil <- as.factor(all2675cc5$soil_order)
all2675cc5$Soil <- as.integer(all2675cc5$Soil)
all2675cc5$FType <- as.factor(all2675cc5$ForestType)
all2675cc5$FType <- as.integer(all2675cc5$FType)
all4575cc5$Soil <- as.factor(all4575cc5$soil_order)
all4575cc5$Soil <- as.integer(all4575cc5$Soil)
all4575cc5$FType <- as.factor(all4575cc5$ForestType)
all4575cc5$FType <- as.integer(all4575cc5$FType)
all8575cc5$Soil <- as.factor(all8575cc5$soil_order)
all8575cc5$Soil <- as.integer(all8575cc5$Soil)
all8575cc5$FType <- as.factor(all8575cc5$ForestType)
all8575cc5$FType <- as.integer(all8575cc5$FType)

all2680cc5$Soil <- as.factor(all2680cc5$soil_order)
all2680cc5$Soil <- as.integer(all2680cc5$Soil)
all2680cc5$FType <- as.factor(all2680cc5$ForestType)
all2680cc5$FType <- as.integer(all2680cc5$FType)
all4580cc5$Soil <- as.factor(all4580cc5$soil_order)
all4580cc5$Soil <- as.integer(all4580cc5$Soil)
all4580cc5$FType <- as.factor(all4580cc5$ForestType)
all4580cc5$FType <- as.integer(all4580cc5$FType)
all8580cc5$Soil <- as.factor(all8580cc5$soil_order)
all8580cc5$Soil <- as.integer(all8580cc5$Soil)
all8580cc5$FType <- as.factor(all8580cc5$ForestType)
all8580cc5$FType <- as.integer(all8580cc5$FType)

#center and scale from model data
all2625cc5 $MAT<- as.data.frame(scale(all2625cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4525cc5 $MAT<- as.data.frame(scale(all4525cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8525cc5 $MAT<- as.data.frame(scale(all8525cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all2630cc5 $MAT <- as.data.frame(scale(all2630cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4530cc5 $MAT <- as.data.frame(scale(all4530cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8530cc5 $MAT <- as.data.frame(scale(all8530cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all2635cc5 $MAT<- as.data.frame(scale(all2635cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4535cc5 $MAT<- as.data.frame(scale(all4535cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8535cc5 $MAT<- as.data.frame(scale(all8535cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all2640cc5 $MAT <- as.data.frame(scale(all2640cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4540cc5 $MAT <- as.data.frame(scale(all4540cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8540cc5 $MAT <- as.data.frame(scale(all8540cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all2645cc5 $MAT<- as.data.frame(scale(all2645cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4545cc5 $MAT<- as.data.frame(scale(all4545cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8545cc5 $MAT<- as.data.frame(scale(all8545cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all2650cc5 $MAT <- as.data.frame(scale(all2650cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4550cc5 $MAT <- as.data.frame(scale(all4550cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8550cc5 $MAT <- as.data.frame(scale(all8550cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all2655cc5 $MAT<- as.data.frame(scale(all2655cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4555cc5 $MAT<- as.data.frame(scale(all4555cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8555cc5 $MAT<- as.data.frame(scale(all8555cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all2660cc5 $MAT <- as.data.frame(scale(all2660cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4560cc5 $MAT <- as.data.frame(scale(all4560cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8560cc5 $MAT <- as.data.frame(scale(all8560cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all2665cc5 $MAT<- as.data.frame(scale(all2665cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4565cc5 $MAT<- as.data.frame(scale(all4565cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8565cc5 $MAT<- as.data.frame(scale(all8565cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all2670cc5 $MAT <- as.data.frame(scale(all2670cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4570cc5 $MAT <- as.data.frame(scale(all4570cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8570cc5 $MAT <- as.data.frame(scale(all8570cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all2675cc5 $MAT<- as.data.frame(scale(all2675cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4575cc5 $MAT<- as.data.frame(scale(all4575cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8575cc5 $MAT<- as.data.frame(scale(all8575cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all2680cc5 $MAT <- as.data.frame(scale(all2680cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all4580cc5 $MAT <- as.data.frame(scale(all4580cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
all8580cc5 $MAT <- as.data.frame(scale(all8580cc5 $MAT, center=centerMAT5,scale=scaleMAT5),col.names="MAT")[, 1]
#PPT
all2625cc5 $PPT<- as.data.frame(scale(all2625cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4525cc5 $PPT<- as.data.frame(scale(all4525cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8525cc5 $PPT<- as.data.frame(scale(all8525cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all2630cc5 $PPT <- as.data.frame(scale(all2630cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4530cc5 $PPT <- as.data.frame(scale(all4530cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8530cc5 $PPT <- as.data.frame(scale(all8530cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all2635cc5 $PPT<- as.data.frame(scale(all2635cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4535cc5 $PPT<- as.data.frame(scale(all4535cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8535cc5 $PPT<- as.data.frame(scale(all8535cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all2640cc5 $PPT <- as.data.frame(scale(all2640cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4540cc5 $PPT <- as.data.frame(scale(all4540cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8540cc5 $PPT <- as.data.frame(scale(all8540cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all2645cc5 $PPT<- as.data.frame(scale(all2645cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4545cc5 $PPT<- as.data.frame(scale(all4545cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8545cc5 $PPT<- as.data.frame(scale(all8545cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all2650cc5 $PPT <- as.data.frame(scale(all2650cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4550cc5 $PPT <- as.data.frame(scale(all4550cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8550cc5 $PPT <- as.data.frame(scale(all8550cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all2655cc5 $PPT<- as.data.frame(scale(all2655cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4555cc5 $PPT<- as.data.frame(scale(all4555cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8555cc5 $PPT<- as.data.frame(scale(all8555cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all2660cc5 $PPT <- as.data.frame(scale(all2660cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4560cc5 $PPT <- as.data.frame(scale(all4560cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8560cc5 $PPT <- as.data.frame(scale(all8560cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all2665cc5 $PPT<- as.data.frame(scale(all2665cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4565cc5 $PPT<- as.data.frame(scale(all4565cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8565cc5 $PPT<- as.data.frame(scale(all8565cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all2670cc5 $PPT <- as.data.frame(scale(all2670cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4570cc5 $PPT <- as.data.frame(scale(all4570cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8570cc5 $PPT <- as.data.frame(scale(all8570cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all2675cc5 $PPT<- as.data.frame(scale(all2675cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4575cc5 $PPT<- as.data.frame(scale(all4575cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8575cc5 $PPT<- as.data.frame(scale(all8575cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all2680cc5 $PPT <- as.data.frame(scale(all2680cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all4580cc5 $PPT <- as.data.frame(scale(all4580cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
all8580cc5 $PPT <- as.data.frame(scale(all8580cc5 $PPT, center=centerPPT5,scale=scalePPT5),col.names="PPT")[, 1]
#RHUM
all2625cc5 $RHUM<- as.data.frame(scale(all2625cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4525cc5 $RHUM<- as.data.frame(scale(all4525cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8525cc5 $RHUM<- as.data.frame(scale(all8525cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all2630cc5 $RHUM <- as.data.frame(scale(all2630cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4530cc5 $RHUM <- as.data.frame(scale(all4530cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8530cc5 $RHUM <- as.data.frame(scale(all8530cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all2635cc5 $RHUM<- as.data.frame(scale(all2635cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4535cc5 $RHUM<- as.data.frame(scale(all4535cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8535cc5 $RHUM<- as.data.frame(scale(all8535cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all2640cc5 $RHUM <- as.data.frame(scale(all2640cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4540cc5 $RHUM <- as.data.frame(scale(all4540cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8540cc5 $RHUM <- as.data.frame(scale(all8540cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all2645cc5 $RHUM<- as.data.frame(scale(all2645cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4545cc5 $RHUM<- as.data.frame(scale(all4545cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8545cc5 $RHUM<- as.data.frame(scale(all8545cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all2650cc5 $RHUM <- as.data.frame(scale(all2650cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4550cc5 $RHUM <- as.data.frame(scale(all4550cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8550cc5 $RHUM <- as.data.frame(scale(all8550cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all2655cc5 $RHUM<- as.data.frame(scale(all2655cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4555cc5 $RHUM<- as.data.frame(scale(all4555cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8555cc5 $RHUM<- as.data.frame(scale(all8555cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all2660cc5 $RHUM <- as.data.frame(scale(all2660cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4560cc5 $RHUM <- as.data.frame(scale(all4560cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8560cc5 $RHUM <- as.data.frame(scale(all8560cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all2665cc5 $RHUM<- as.data.frame(scale(all2665cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4565cc5 $RHUM<- as.data.frame(scale(all4565cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8565cc5 $RHUM<- as.data.frame(scale(all8565cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all2670cc5 $RHUM <- as.data.frame(scale(all2670cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4570cc5 $RHUM <- as.data.frame(scale(all4570cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8570cc5 $RHUM <- as.data.frame(scale(all8570cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all2675cc5 $RHUM<- as.data.frame(scale(all2675cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4575cc5 $RHUM<- as.data.frame(scale(all4575cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8575cc5 $RHUM<- as.data.frame(scale(all8575cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all2680cc5 $RHUM <- as.data.frame(scale(all2680cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all4580cc5 $RHUM <- as.data.frame(scale(all4580cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
all8580cc5 $RHUM <- as.data.frame(scale(all8580cc5 $RHUM, center=centerRHUM5,scale=scaleRHUM5),col.names="RHUM")[, 1]
#RAD
all2625cc5 $RAD<- as.data.frame(scale(all2625cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4525cc5 $RAD<- as.data.frame(scale(all4525cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8525cc5 $RAD<- as.data.frame(scale(all8525cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all2630cc5 $RAD <- as.data.frame(scale(all2630cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4530cc5 $RAD <- as.data.frame(scale(all4530cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8530cc5 $RAD <- as.data.frame(scale(all8530cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all2635cc5 $RAD<- as.data.frame(scale(all2635cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4535cc5 $RAD<- as.data.frame(scale(all4535cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8535cc5 $RAD<- as.data.frame(scale(all8535cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all2640cc5 $RAD <- as.data.frame(scale(all2640cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4540cc5 $RAD <- as.data.frame(scale(all4540cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8540cc5 $RAD <- as.data.frame(scale(all8540cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all2645cc5 $RAD<- as.data.frame(scale(all2645cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4545cc5 $RAD<- as.data.frame(scale(all4545cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8545cc5 $RAD<- as.data.frame(scale(all8545cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all2650cc5 $RAD <- as.data.frame(scale(all2650cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4550cc5 $RAD <- as.data.frame(scale(all4550cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8550cc5 $RAD <- as.data.frame(scale(all8550cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all2655cc5 $RAD<- as.data.frame(scale(all2655cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4555cc5 $RAD<- as.data.frame(scale(all4555cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8555cc5 $RAD<- as.data.frame(scale(all8555cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all2660cc5 $RAD <- as.data.frame(scale(all2660cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4560cc5 $RAD <- as.data.frame(scale(all4560cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8560cc5 $RAD <- as.data.frame(scale(all8560cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all2665cc5 $RAD<- as.data.frame(scale(all2665cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4565cc5 $RAD<- as.data.frame(scale(all4565cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8565cc5 $RAD<- as.data.frame(scale(all8565cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all2670cc5 $RAD <- as.data.frame(scale(all2670cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4570cc5 $RAD <- as.data.frame(scale(all4570cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8570cc5 $RAD <- as.data.frame(scale(all8570cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all2675cc5 $RAD<- as.data.frame(scale(all2675cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4575cc5 $RAD<- as.data.frame(scale(all4575cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8575cc5 $RAD<- as.data.frame(scale(all8575cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all2680cc5 $RAD <- as.data.frame(scale(all2680cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all4580cc5 $RAD <- as.data.frame(scale(all4580cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
all8580cc5 $RAD <- as.data.frame(scale(all8580cc5 $RAD, center=centerRAD5,scale=scaleRAD5),col.names="RAD")[, 1]
#Elevation
all2625cc5 $Elevation<- as.data.frame(scale(all2625cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4525cc5 $Elevation<- as.data.frame(scale(all4525cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8525cc5 $Elevation<- as.data.frame(scale(all8525cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all2630cc5 $Elevation <- as.data.frame(scale(all2630cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4530cc5 $Elevation <- as.data.frame(scale(all4530cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8530cc5 $Elevation <- as.data.frame(scale(all8530cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all2635cc5 $Elevation<- as.data.frame(scale(all2635cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4535cc5 $Elevation<- as.data.frame(scale(all4535cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8535cc5 $Elevation<- as.data.frame(scale(all8535cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all2640cc5 $Elevation <- as.data.frame(scale(all2640cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4540cc5 $Elevation <- as.data.frame(scale(all4540cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8540cc5 $Elevation <- as.data.frame(scale(all8540cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all2645cc5 $Elevation<- as.data.frame(scale(all2645cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4545cc5 $Elevation<- as.data.frame(scale(all4545cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8545cc5 $Elevation<- as.data.frame(scale(all8545cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all2650cc5 $Elevation <- as.data.frame(scale(all2650cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4550cc5 $Elevation <- as.data.frame(scale(all4550cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8550cc5 $Elevation <- as.data.frame(scale(all8550cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all2655cc5 $Elevation<- as.data.frame(scale(all2655cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4555cc5 $Elevation<- as.data.frame(scale(all4555cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8555cc5 $Elevation<- as.data.frame(scale(all8555cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all2660cc5 $Elevation <- as.data.frame(scale(all2660cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4560cc5 $Elevation <- as.data.frame(scale(all4560cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8560cc5 $Elevation <- as.data.frame(scale(all8560cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all2665cc5 $Elevation<- as.data.frame(scale(all2665cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4565cc5 $Elevation<- as.data.frame(scale(all4565cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8565cc5 $Elevation<- as.data.frame(scale(all8565cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all2670cc5 $Elevation <- as.data.frame(scale(all2670cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4570cc5 $Elevation <- as.data.frame(scale(all4570cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8570cc5 $Elevation <- as.data.frame(scale(all8570cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all2675cc5 $Elevation<- as.data.frame(scale(all2675cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4575cc5 $Elevation<- as.data.frame(scale(all4575cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8575cc5 $Elevation<- as.data.frame(scale(all8575cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all2680cc5 $Elevation <- as.data.frame(scale(all2680cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all4580cc5 $Elevation <- as.data.frame(scale(all4580cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]
all8580cc5 $Elevation <- as.data.frame(scale(all8580cc5 $Elevation, center=centerElevation5,scale=scaleElevation5),col.names="Elevation")[, 1]

#add BasalArea + annual rate*years, some negatives produced, turn to 0
all2625cc5$BA <- (all2625cc5$BA)+(all2625cc5$AvgBADeadRate)*6
all4525cc5$BA <- (all4525cc5$BA)+(all4525cc5$AvgBADeadRate)*6
all8525cc5$BA <- (all8525cc5$BA)+(all8525cc5$AvgBADeadRate)*6
all2630cc5$BA <- (all2630cc5$BA)+(all2630cc5$AvgBADeadRate)*11
all4530cc5$BA <- (all4530cc5$BA)+(all4530cc5$AvgBADeadRate)*11
all8530cc5$BA <- (all8530cc5$BA)+(all8530cc5$AvgBADeadRate)*11
all2635cc5$BA <- (all2635cc5$BA)+(all2635cc5$AvgBADeadRate)*16
all4535cc5$BA <-  (all4535cc5$BA)+(all4535cc5$AvgBADeadRate)*16
all8535cc5$BA <- (all8535cc5$BA)+(all8535cc5$AvgBADeadRate)*16
all2640cc5$BA <- (all2640cc5$BA)+(all2640cc5$AvgBADeadRate)*21
all4540cc5$BA <- (all4540cc5$BA)+(all4540cc5$AvgBADeadRate)*21
all8540cc5$BA <-  (all8540cc5$BA)+(all8540cc5$AvgBADeadRate)*21
all2645cc5$BA <- (all2645cc5$BA)+(all2645cc5$AvgBADeadRate)*26
all4545cc5$BA <- (all4545cc5$BA)+(all4545cc5$AvgBADeadRate)*26
all8545cc5$BA <- (all8545cc5$BA)+(all8545cc5$AvgBADeadRate)*26
all2650cc5$BA <- (all2650cc5$BA)+(all2650cc5$AvgBADeadRate)*31
all4550cc5$BA <- (all4550cc5$BA)+(all4550cc5$AvgBADeadRate)*31
all8550cc5$BA <- (all8550cc5$BA)+(all8550cc5$AvgBADeadRate)*31
all2655cc5$BA <-(all2655cc5$BA)+(all2655cc5$AvgBADeadRate)*36
all4555cc5$BA <- (all4555cc5$BA)+(all4555cc5$AvgBADeadRate)*36
all8555cc5$BA <- (all8555cc5$BA)+(all8555cc5$AvgBADeadRate)*36
all2660cc5$BA <-(all2660cc5$BA)+(all2660cc5$AvgBADeadRate)*41
all4560cc5$BA <- (all4560cc5$BA)+(all4560cc5$AvgBADeadRate)*41
all8560cc5$BA <- (all8560cc5$BA)+(all8560cc5$AvgBADeadRate)*41
all2665cc5$BA <- (all2665cc5$BA)+(all2665cc5$AvgBADeadRate)*46
all4565cc5$BA <- (all4565cc5$BA)+(all4565cc5$AvgBADeadRate)*46
all8565cc5$BA <- (all8565cc5$BA)+(all8565cc5$AvgBADeadRate)*46
all2670cc5$BA <- (all2670cc5$BA)+(all2670cc5$AvgBADeadRate)*51
all4570cc5$BA <- (all4570cc5$BA)+(all4570cc5$AvgBADeadRate)*51
all8570cc5$BA <- (all8570cc5$BA)+(all8570cc5$AvgBADeadRate)*51
all2675cc5$BA <- (all2675cc5$BA)+(all2675cc5$AvgBADeadRate)*56
all4575cc5$BA <- (all4575cc5$BA)+(all4575cc5$AvgBADeadRate)*56
all8575cc5$BA <- (all8575cc5$BA)+(all8575cc5$AvgBADeadRate)*56
all2680cc5$BA <- (all2680cc5$BA)+(all2680cc5$AvgBADeadRate)*61
all4580cc5$BA <- (all4580cc5$BA)+(all4580cc5$AvgBADeadRate)*61
all8580cc5$BA <- (all8580cc5$BA)+(all8580cc5$AvgBADeadRate)*61

#remove negatives
all2625cc5$BA[all2625cc5$BA<=0] <- 0.01
all4525cc5$BA[all4525cc5$BA<=0] <- 0.01
all8525cc5$BA[all8525cc5$BA<=0] <- 0.01
all2630cc5$BA[all2630cc5$BA<=0] <- 0.01
all4530cc5$BA[all4530cc5$BA<=0] <- 0.01
all8530cc5$BA[all8530cc5$BA<=0] <- 0.01
all2635cc5$BA[all2635cc5$BA<=0] <- 0.01
all4535cc5$BA[all4535cc5$BA<=0] <- 0.01
all8535cc5$BA[all8535cc5$BA<=0] <- 0.01
all2640cc5$BA[all2640cc5$BA<=0] <- 0.01
all4540cc5$BA[all4540cc5$BA<=0] <- 0.01
all8540cc5$BA[all8540cc5$BA<=0] <- 0.01
all2645cc5$BA[all2645cc5$BA<=0] <- 0.01
all4545cc5$BA[all4545cc5$BA<=0] <- 0.01
all8545cc5$BA[all8545cc5$BA<=0] <- 0.01
all2650cc5$BA[all2650cc5$BA<=0] <- 0.01
all4550cc5$BA[all4550cc5$BA<=0] <- 0.01
all8550cc5$BA[all8550cc5$BA<=0] <- 0.01
all2655cc5$BA[all2655cc5$BA<=0] <- 0.01
all4555cc5$BA[all4555cc5$BA<=0] <- 0.01
all8555cc5$BA[all8555cc5$BA<=0] <- 0.01
all2660cc5$BA[all2660cc5$BA<=0] <- 0.01
all4560cc5$BA[all4560cc5$BA<=0] <- 0.01
all8560cc5$BA[all8560cc5$BA<=0] <- 0.01
all2665cc5$BA[all2665cc5$BA<=0] <- 0.01
all4565cc5$BA[all4565cc5$BA<=0] <- 0.01
all8565cc5$BA[all8565cc5$BA<=0] <- 0.01
all2670cc5$BA[all2670cc5$BA<=0] <- 0.01
all4570cc5$BA[all4570cc5$BA<=0] <- 0.01
all8570cc5$BA[all8570cc5$BA<=0] <- 0.01
all2675cc5$BA[all2675cc5$BA<=0] <- 0.01
all4575cc5$BA[all4575cc5$BA<=0] <- 0.01
all8575cc5$BA[all8575cc5$BA<=0] <- 0.01
all2680cc5$BA[all2680cc5$BA<=0] <- 0.01
all4580cc5$BA[all4580cc5$BA<=0] <- 0.01
all8580cc5$BA[all8580cc5$BA<=0] <- 0.01

#max basal area of dead trees = basal area of all trees. prevents mortality over 100%, not possible.
all2625cc5$BA <- ifelse(all2625cc5$BA > all2625cc5$BAAll, all2625cc5$BAAll, all2625cc5$BA)
all4525cc5$BA <- ifelse(all4525cc5$BA > all4525cc5$BAAll, all4525cc5$BAAll, all4525cc5$BA)
all8525cc5$BA <- ifelse(all8525cc5$BA > all8525cc5$BAAll, all8525cc5$BAAll, all8525cc5$BA)
all2630cc5$BA <- ifelse(all2630cc5$BA > all2630cc5$BAAll, all2630cc5$BAAll, all2630cc5$BA)
all4530cc5$BA <- ifelse(all4530cc5$BA > all4530cc5$BAAll, all4530cc5$BAAll, all4530cc5$BA)
all8530cc5$BA <- ifelse(all8530cc5$BA > all8530cc5$BAAll, all8530cc5$BAAll, all8530cc5$BA)
all2635cc5$BA <- ifelse(all2635cc5$BA > all2635cc5$BAAll, all2635cc5$BAAll, all2635cc5$BA)
all4535cc5$BA <- ifelse(all4535cc5$BA > all4535cc5$BAAll, all4535cc5$BAAll, all4535cc5$BA)
all8535cc5$BA <- ifelse(all8535cc5$BA > all8535cc5$BAAll, all8535cc5$BAAll, all8535cc5$BA)
all2640cc5$BA <- ifelse(all2640cc5$BA > all2640cc5$BAAll, all2640cc5$BAAll, all2640cc5$BA)
all4540cc5$BA <- ifelse(all4540cc5$BA > all4540cc5$BAAll, all4540cc5$BAAll, all4540cc5$BA)
all8540cc5$BA <- ifelse(all8540cc5$BA > all8540cc5$BAAll, all8540cc5$BAAll, all8540cc5$BA)
all2645cc5$BA <- ifelse(all2645cc5$BA > all2645cc5$BAAll, all2645cc5$BAAll, all2645cc5$BA)
all4545cc5$BA <- ifelse(all4545cc5$BA > all4545cc5$BAAll, all4545cc5$BAAll, all4545cc5$BA)
all8545cc5$BA <- ifelse(all8545cc5$BA > all8545cc5$BAAll, all8545cc5$BAAll, all8545cc5$BA)
all2650cc5$BA <- ifelse(all2650cc5$BA > all2650cc5$BAAll, all2650cc5$BAAll, all2650cc5$BA)
all4550cc5$BA <- ifelse(all4550cc5$BA > all4550cc5$BAAll, all4550cc5$BAAll, all4550cc5$BA)
all8550cc5$BA <- ifelse(all8550cc5$BA > all8550cc5$BAAll, all8550cc5$BAAll, all8550cc5$BA)
all2655cc5$BA <- ifelse(all2655cc5$BA > all2655cc5$BAAll, all2655cc5$BAAll, all2655cc5$BA)
all4555cc5$BA <- ifelse(all4555cc5$BA > all4555cc5$BAAll, all4555cc5$BAAll, all4555cc5$BA)
all8555cc5$BA <- ifelse(all8555cc5$BA > all8555cc5$BAAll, all8555cc5$BAAll, all8555cc5$BA)
all2660cc5$BA <- ifelse(all2660cc5$BA > all2660cc5$BAAll, all2660cc5$BAAll, all2660cc5$BA)
all4560cc5$BA <- ifelse(all4560cc5$BA > all4560cc5$BAAll, all4560cc5$BAAll, all4560cc5$BA)
all8560cc5$BA <- ifelse(all8560cc5$BA > all8560cc5$BAAll, all8560cc5$BAAll, all8560cc5$BA)
all2665cc5$BA <- ifelse(all2665cc5$BA > all2665cc5$BAAll, all2665cc5$BAAll, all2665cc5$BA)
all4565cc5$BA <- ifelse(all4565cc5$BA > all4565cc5$BAAll, all4565cc5$BAAll, all4565cc5$BA)
all8565cc5$BA <- ifelse(all8565cc5$BA > all8565cc5$BAAll, all8565cc5$BAAll, all8565cc5$BA)
all2670cc5$BA <- ifelse(all2670cc5$BA > all2670cc5$BAAll, all2670cc5$BAAll, all2670cc5$BA)
all4570cc5$BA <- ifelse(all4570cc5$BA > all4570cc5$BAAll, all4570cc5$BAAll, all4570cc5$BA)
all8570cc5$BA <- ifelse(all8570cc5$BA > all8570cc5$BAAll, all8570cc5$BAAll, all8570cc5$BA)
all2675cc5$BA <- ifelse(all2675cc5$BA > all2675cc5$BAAll, all2675cc5$BAAll, all2675cc5$BA)
all4575cc5$BA <- ifelse(all4575cc5$BA > all4575cc5$BAAll, all4575cc5$BAAll, all4575cc5$BA)
all8575cc5$BA <- ifelse(all8575cc5$BA > all8575cc5$BAAll, all8575cc5$BAAll, all8575cc5$BA)
all2680cc5$BA <- ifelse(all2680cc5$BA > all2680cc5$BAAll, all2680cc5$BAAll, all2680cc5$BA)
all4580cc5$BA <- ifelse(all4580cc5$BA > all4580cc5$BAAll, all4580cc5$BAAll, all4580cc5$BA)
all8580cc5$BA <- ifelse(all8580cc5$BA > all8580cc5$BAAll, all8580cc5$BAAll, all8580cc5$BA)
#Basal Area
all2625cc5 $BA<- as.data.frame(scale(all2625cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4525cc5 $BA<- as.data.frame(scale(all4525cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8525cc5 $BA<- as.data.frame(scale(all8525cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all2630cc5 $BA <- as.data.frame(scale(all2630cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4530cc5 $BA <- as.data.frame(scale(all4530cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8530cc5 $BA <- as.data.frame(scale(all8530cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all2635cc5 $BA<- as.data.frame(scale(all2635cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4535cc5 $BA<- as.data.frame(scale(all4535cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8535cc5 $BA<- as.data.frame(scale(all8535cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all2640cc5 $BA <- as.data.frame(scale(all2640cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4540cc5 $BA <- as.data.frame(scale(all4540cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8540cc5 $BA <- as.data.frame(scale(all8540cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all2645cc5 $BA<- as.data.frame(scale(all2645cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4545cc5 $BA<- as.data.frame(scale(all4545cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8545cc5 $BA<- as.data.frame(scale(all8545cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all2650cc5 $BA <- as.data.frame(scale(all2650cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4550cc5 $BA <- as.data.frame(scale(all4550cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8550cc5 $BA <- as.data.frame(scale(all8550cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all2655cc5 $BA<- as.data.frame(scale(all2655cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4555cc5 $BA<- as.data.frame(scale(all4555cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8555cc5 $BA<- as.data.frame(scale(all8555cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all2660cc5 $BA <- as.data.frame(scale(all2660cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4560cc5 $BA <- as.data.frame(scale(all4560cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8560cc5 $BA <- as.data.frame(scale(all8560cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all2665cc5 $BA<- as.data.frame(scale(all2665cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4565cc5 $BA<- as.data.frame(scale(all4565cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8565cc5 $BA<- as.data.frame(scale(all8565cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all2670cc5 $BA <- as.data.frame(scale(all2670cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4570cc5 $BA <- as.data.frame(scale(all4570cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8570cc5 $BA <- as.data.frame(scale(all8570cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all2675cc5 $BA<- as.data.frame(scale(all2675cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4575cc5 $BA<- as.data.frame(scale(all4575cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8575cc5 $BA<- as.data.frame(scale(all8575cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all2680cc5 $BA <- as.data.frame(scale(all2680cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all4580cc5 $BA <- as.data.frame(scale(all4580cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
all8580cc5 $BA <- as.data.frame(scale(all8580cc5 $BA, center=centerBA5,scale=scaleBA5),col.names="BA")[, 1]
#time weight f5 group
all2625cc5$time_weight <- 1.800237
all4525cc5$time_weight <- 1.800237
all8525cc5$time_weight <- 1.800237
all2630cc5$time_weight <- 1.800237
all4530cc5$time_weight <- 1.800237
all8530cc5$time_weight <- 1.800237
all2635cc5$time_weight <- 1.800237
all4535cc5$time_weight <- 1.800237
all8535cc5$time_weight <- 1.800237
all2640cc5$time_weight <- 1.800237
all4540cc5$time_weight <- 1.800237
all8540cc5$time_weight <- 1.800237
all2645cc5$time_weight <- 1.800237
all4545cc5$time_weight <- 1.800237
all8545cc5$time_weight <- 1.800237
all2650cc5$time_weight <- 1.800237
all4550cc5$time_weight <- 1.800237
all8550cc5$time_weight <- 1.800237
all2655cc5$time_weight <- 1.800237
all4555cc5$time_weight <- 1.800237
all8555cc5$time_weight <- 1.800237
all2660cc5$time_weight <- 1.800237
all4560cc5$time_weight <- 1.800237
all8560cc5$time_weight <- 1.800237
all2665cc5$time_weight <- 1.800237
all4565cc5$time_weight <- 1.800237
all8565cc5$time_weight <- 1.800237
all2670cc5$time_weight <- 1.800237
all4570cc5$time_weight <- 1.800237
all8570cc5$time_weight <- 1.800237
all2675cc5$time_weight <- 1.800237
all4575cc5$time_weight <- 1.800237
all8575cc5$time_weight <- 1.800237
all2680cc5$time_weight <- 1.800237
all4580cc5$time_weight <- 1.800237
all8580cc5$time_weight <- 1.800237

#run simulations
simCarb2625 <- link(CarbSplit5, data=all2625cc5 )
simmeanCarb2625 <- data.frame(apply(simCarb2625,2,mean))
PICarb2625 <- t(data.frame(apply(simCarb2625,2,PI,prob=0.89)))
F5CarbSim2625 <- data.frame(cbind(simmeanCarb2625,PICarb2625))
F5CarbSim2625$PlotCN <- all2625cc5  $PlotCN
F5CarbSim2625 <- F5CarbSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2625) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4525 <- link(CarbSplit5, data=all4525cc5 )
simmeanCarb4525 <- data.frame(apply(simCarb4525,2,mean))
PICarb4525 <- t(data.frame(apply(simCarb4525,2,PI,prob=0.89)))
F5CarbSim4525 <- data.frame(cbind(simmeanCarb4525,PICarb4525))
F5CarbSim4525$PlotCN <- all4525cc5  $PlotCN
F5CarbSim4525 <- F5CarbSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4525) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8525 <- link(CarbSplit5, data=all8525cc5 )
simmeanCarb8525 <- data.frame(apply(simCarb8525,2,mean))
PICarb8525 <- t(data.frame(apply(simCarb8525,2,PI,prob=0.89)))
F5CarbSim8525 <- data.frame(cbind(simmeanCarb8525,PICarb8525))
F5CarbSim8525$PlotCN <- all8525cc5  $PlotCN
F5CarbSim8525 <- F5CarbSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8525) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2630 <- link(CarbSplit5, data=all2630cc5 )
simmeanCarb2630 <- data.frame(apply(simCarb2630,2,mean))
PICarb2630 <- t(data.frame(apply(simCarb2630,2,PI,prob=0.89)))
F5CarbSim2630 <- data.frame(cbind(simmeanCarb2630,PICarb2630))
F5CarbSim2630$PlotCN <- all2630cc5  $PlotCN
F5CarbSim2630 <- F5CarbSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2630) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4530 <- link(CarbSplit5, data=all4530cc5 )
simmeanCarb4530 <- data.frame(apply(simCarb4530,2,mean))
PICarb4530 <- t(data.frame(apply(simCarb4530,2,PI,prob=0.89)))
F5CarbSim4530 <- data.frame(cbind(simmeanCarb4530,PICarb4530))
F5CarbSim4530$PlotCN <- all4530cc5  $PlotCN
F5CarbSim4530 <- F5CarbSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4530) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8530 <- link(CarbSplit5, data=all8530cc5 )
simmeanCarb8530 <- data.frame(apply(simCarb8530,2,mean))
PICarb8530 <- t(data.frame(apply(simCarb8530,2,PI,prob=0.89)))
F5CarbSim8530 <- data.frame(cbind(simmeanCarb8530,PICarb8530))
F5CarbSim8530$PlotCN <- all8530cc5  $PlotCN
F5CarbSim8530 <- F5CarbSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8530) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2635 <- link(CarbSplit5, data=all2635cc5 )
simmeanCarb2635 <- data.frame(apply(simCarb2635,2,mean))
PICarb2635 <- t(data.frame(apply(simCarb2635,2,PI,prob=0.89)))
F5CarbSim2635 <- data.frame(cbind(simmeanCarb2635,PICarb2635))
F5CarbSim2635$PlotCN <- all2635cc5  $PlotCN
F5CarbSim2635 <- F5CarbSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2635) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4535 <- link(CarbSplit5, data=all4535cc5 )
simmeanCarb4535 <- data.frame(apply(simCarb4535,2,mean))
PICarb4535 <- t(data.frame(apply(simCarb4535,2,PI,prob=0.89)))
F5CarbSim4535 <- data.frame(cbind(simmeanCarb4535,PICarb4535))
F5CarbSim4535$PlotCN <- all4535cc5  $PlotCN
F5CarbSim4535 <- F5CarbSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4535) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8535 <- link(CarbSplit5, data=all8535cc5 )
simmeanCarb8535 <- data.frame(apply(simCarb8535,2,mean))
PICarb8535 <- t(data.frame(apply(simCarb8535,2,PI,prob=0.89)))
F5CarbSim8535 <- data.frame(cbind(simmeanCarb8535,PICarb8535))
F5CarbSim8535$PlotCN <- all8535cc5  $PlotCN
F5CarbSim8535 <- F5CarbSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8535) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2640 <- link(CarbSplit5, data=all2640cc5 )
simmeanCarb2640 <- data.frame(apply(simCarb2640,2,mean))
PICarb2640 <- t(data.frame(apply(simCarb2640,2,PI,prob=0.89)))
F5CarbSim2640 <- data.frame(cbind(simmeanCarb2640,PICarb2640))
F5CarbSim2640$PlotCN <- all2640cc5  $PlotCN
F5CarbSim2640 <- F5CarbSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2640) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4540 <- link(CarbSplit5, data=all4540cc5 )
simmeanCarb4540 <- data.frame(apply(simCarb4540,2,mean))
PICarb4540 <- t(data.frame(apply(simCarb4540,2,PI,prob=0.89)))
F5CarbSim4540 <- data.frame(cbind(simmeanCarb4540,PICarb4540))
F5CarbSim4540$PlotCN <- all4540cc5  $PlotCN
F5CarbSim4540 <- F5CarbSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4540) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8540 <- link(CarbSplit5, data=all8540cc5 )
simmeanCarb8540 <- data.frame(apply(simCarb8540,2,mean))
PICarb8540 <- t(data.frame(apply(simCarb8540,2,PI,prob=0.89)))
F5CarbSim8540 <- data.frame(cbind(simmeanCarb8540,PICarb8540))
F5CarbSim8540$PlotCN <- all8540cc5  $PlotCN
F5CarbSim8540 <- F5CarbSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8540) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2645 <- link(CarbSplit5, data=all2645cc5 )
simmeanCarb2645 <- data.frame(apply(simCarb2645,2,mean))
PICarb2645 <- t(data.frame(apply(simCarb2645,2,PI,prob=0.89)))
F5CarbSim2645 <- data.frame(cbind(simmeanCarb2645,PICarb2645))
F5CarbSim2645$PlotCN <- all2645cc5  $PlotCN
F5CarbSim2645 <- F5CarbSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2645) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4545 <- link(CarbSplit5, data=all4545cc5 )
simmeanCarb4545 <- data.frame(apply(simCarb4545,2,mean))
PICarb4545 <- t(data.frame(apply(simCarb4545,2,PI,prob=0.89)))
F5CarbSim4545 <- data.frame(cbind(simmeanCarb4545,PICarb4545))
F5CarbSim4545$PlotCN <- all4545cc5  $PlotCN
F5CarbSim4545 <- F5CarbSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4545) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8545 <- link(CarbSplit5, data=all8545cc5 )
simmeanCarb8545 <- data.frame(apply(simCarb8545,2,mean))
PICarb8545 <- t(data.frame(apply(simCarb8545,2,PI,prob=0.89)))
F5CarbSim8545 <- data.frame(cbind(simmeanCarb8545,PICarb8545))
F5CarbSim8545$PlotCN <- all8545cc5  $PlotCN
F5CarbSim8545 <- F5CarbSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8545) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2650 <- link(CarbSplit5, data=all2650cc5 )
simmeanCarb2650 <- data.frame(apply(simCarb2650,2,mean))
PICarb2650 <- t(data.frame(apply(simCarb2650,2,PI,prob=0.89)))
F5CarbSim2650 <- data.frame(cbind(simmeanCarb2650,PICarb2650))
F5CarbSim2650$PlotCN <- all2650cc5  $PlotCN
F5CarbSim2650 <- F5CarbSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2650) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4550 <- link(CarbSplit5, data=all4550cc5 )
simmeanCarb4550 <- data.frame(apply(simCarb4550,2,mean))
PICarb4550 <- t(data.frame(apply(simCarb4550,2,PI,prob=0.89)))
F5CarbSim4550 <- data.frame(cbind(simmeanCarb4550,PICarb4550))
F5CarbSim4550$PlotCN <- all4550cc5  $PlotCN
F5CarbSim4550 <- F5CarbSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4550) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8550 <- link(CarbSplit5, data=all8550cc5 )
simmeanCarb8550 <- data.frame(apply(simCarb8550,2,mean))
PICarb8550 <- t(data.frame(apply(simCarb8550,2,PI,prob=0.89)))
F5CarbSim8550 <- data.frame(cbind(simmeanCarb8550,PICarb8550))
F5CarbSim8550$PlotCN <- all8550cc5  $PlotCN
F5CarbSim8550 <- F5CarbSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8550) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2655 <- link(CarbSplit5, data=all2655cc5 )
simmeanCarb2655 <- data.frame(apply(simCarb2655,2,mean))
PICarb2655 <- t(data.frame(apply(simCarb2655,2,PI,prob=0.89)))
F5CarbSim2655 <- data.frame(cbind(simmeanCarb2655,PICarb2655))
F5CarbSim2655$PlotCN <- all2655cc5  $PlotCN
F5CarbSim2655 <- F5CarbSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2655) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4555 <- link(CarbSplit5, data=all4555cc5 )
simmeanCarb4555 <- data.frame(apply(simCarb4555,2,mean))
PICarb4555 <- t(data.frame(apply(simCarb4555,2,PI,prob=0.89)))
F5CarbSim4555 <- data.frame(cbind(simmeanCarb4555,PICarb4555))
F5CarbSim4555$PlotCN <- all4555cc5  $PlotCN
F5CarbSim4555 <- F5CarbSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4555) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8555 <- link(CarbSplit5, data=all8555cc5 )
simmeanCarb8555 <- data.frame(apply(simCarb8555,2,mean))
PICarb8555 <- t(data.frame(apply(simCarb8555,2,PI,prob=0.89)))
F5CarbSim8555 <- data.frame(cbind(simmeanCarb8555,PICarb8555))
F5CarbSim8555$PlotCN <- all8555cc5  $PlotCN
F5CarbSim8555 <- F5CarbSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8555) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2660 <- link(CarbSplit5, data=all2660cc5 )
simmeanCarb2660 <- data.frame(apply(simCarb2660,2,mean))
PICarb2660 <- t(data.frame(apply(simCarb2660,2,PI,prob=0.89)))
F5CarbSim2660 <- data.frame(cbind(simmeanCarb2660,PICarb2660))
F5CarbSim2660$PlotCN <- all2660cc5  $PlotCN
F5CarbSim2660 <- F5CarbSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2660) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4560 <- link(CarbSplit5, data=all4560cc5 )
simmeanCarb4560 <- data.frame(apply(simCarb4560,2,mean))
PICarb4560 <- t(data.frame(apply(simCarb4560,2,PI,prob=0.89)))
F5CarbSim4560 <- data.frame(cbind(simmeanCarb4560,PICarb4560))
F5CarbSim4560$PlotCN <- all4560cc5  $PlotCN
F5CarbSim4560 <- F5CarbSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4560) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8560 <- link(CarbSplit5, data=all8560cc5 )
simmeanCarb8560 <- data.frame(apply(simCarb8560,2,mean))
PICarb8560 <- t(data.frame(apply(simCarb8560,2,PI,prob=0.89)))
F5CarbSim8560 <- data.frame(cbind(simmeanCarb8560,PICarb8560))
F5CarbSim8560$PlotCN <- all8560cc5  $PlotCN
F5CarbSim8560 <- F5CarbSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8560) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2665 <- link(CarbSplit5, data=all2665cc5 )
simmeanCarb2665 <- data.frame(apply(simCarb2665,2,mean))
PICarb2665 <- t(data.frame(apply(simCarb2665,2,PI,prob=0.89)))
F5CarbSim2665 <- data.frame(cbind(simmeanCarb2665,PICarb2665))
F5CarbSim2665$PlotCN <- all2665cc5  $PlotCN
F5CarbSim2665 <- F5CarbSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2665) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4565 <- link(CarbSplit5, data=all4565cc5 )
simmeanCarb4565 <- data.frame(apply(simCarb4565,2,mean))
PICarb4565 <- t(data.frame(apply(simCarb4565,2,PI,prob=0.89)))
F5CarbSim4565 <- data.frame(cbind(simmeanCarb4565,PICarb4565))
F5CarbSim4565$PlotCN <- all4565cc5  $PlotCN
F5CarbSim4565 <- F5CarbSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4565) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8565 <- link(CarbSplit5, data=all8565cc5 )
simmeanCarb8565 <- data.frame(apply(simCarb8565,2,mean))
PICarb8565 <- t(data.frame(apply(simCarb8565,2,PI,prob=0.89)))
F5CarbSim8565 <- data.frame(cbind(simmeanCarb8565,PICarb8565))
F5CarbSim8565$PlotCN <- all8565cc5  $PlotCN
F5CarbSim8565 <- F5CarbSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8565) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2670 <- link(CarbSplit5, data=all2670cc5 )
simmeanCarb2670 <- data.frame(apply(simCarb2670,2,mean))
PICarb2670 <- t(data.frame(apply(simCarb2670,2,PI,prob=0.89)))
F5CarbSim2670 <- data.frame(cbind(simmeanCarb2670,PICarb2670))
F5CarbSim2670$PlotCN <- all2670cc5  $PlotCN
F5CarbSim2670 <- F5CarbSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2670) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4570 <- link(CarbSplit5, data=all4570cc5 )
simmeanCarb4570 <- data.frame(apply(simCarb4570,2,mean))
PICarb4570 <- t(data.frame(apply(simCarb4570,2,PI,prob=0.89)))
F5CarbSim4570 <- data.frame(cbind(simmeanCarb4570,PICarb4570))
F5CarbSim4570$PlotCN <- all4570cc5  $PlotCN
F5CarbSim4570 <- F5CarbSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4570) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8570 <- link(CarbSplit5, data=all8570cc5 )
simmeanCarb8570 <- data.frame(apply(simCarb8570,2,mean))
PICarb8570 <- t(data.frame(apply(simCarb8570,2,PI,prob=0.89)))
F5CarbSim8570 <- data.frame(cbind(simmeanCarb8570,PICarb8570))
F5CarbSim8570$PlotCN <- all8570cc5  $PlotCN
F5CarbSim8570 <- F5CarbSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8570) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2675 <- link(CarbSplit5, data=all2675cc5 )
simmeanCarb2675 <- data.frame(apply(simCarb2675,2,mean))
PICarb2675 <- t(data.frame(apply(simCarb2675,2,PI,prob=0.89)))
F5CarbSim2675 <- data.frame(cbind(simmeanCarb2675,PICarb2675))
F5CarbSim2675$PlotCN <- all2675cc5  $PlotCN
F5CarbSim2675 <- F5CarbSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2675) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4575 <- link(CarbSplit5, data=all4575cc5 )
simmeanCarb4575 <- data.frame(apply(simCarb4575,2,mean))
PICarb4575 <- t(data.frame(apply(simCarb4575,2,PI,prob=0.89)))
F5CarbSim4575 <- data.frame(cbind(simmeanCarb4575,PICarb4575))
F5CarbSim4575$PlotCN <- all4575cc5  $PlotCN
F5CarbSim4575 <- F5CarbSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4575) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8575 <- link(CarbSplit5, data=all8575cc5 )
simmeanCarb8575 <- data.frame(apply(simCarb8575,2,mean))
PICarb8575 <- t(data.frame(apply(simCarb8575,2,PI,prob=0.89)))
F5CarbSim8575 <- data.frame(cbind(simmeanCarb8575,PICarb8575))
F5CarbSim8575$PlotCN <- all8575cc5  $PlotCN
F5CarbSim8575 <- F5CarbSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8575) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2680 <- link(CarbSplit5, data=all2680cc5 )
simmeanCarb2680 <- data.frame(apply(simCarb2680,2,mean))
PICarb2680 <- t(data.frame(apply(simCarb2680,2,PI,prob=0.89)))
F5CarbSim2680 <- data.frame(cbind(simmeanCarb2680,PICarb2680))
F5CarbSim2680$PlotCN <- all2680cc5  $PlotCN
F5CarbSim2680 <- F5CarbSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim2680) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4580 <- link(CarbSplit5, data=all4580cc5 )
simmeanCarb4580 <- data.frame(apply(simCarb4580,2,mean))
PICarb4580 <- t(data.frame(apply(simCarb4580,2,PI,prob=0.89)))
F5CarbSim4580 <- data.frame(cbind(simmeanCarb4580,PICarb4580))
F5CarbSim4580$PlotCN <- all4580cc5  $PlotCN
F5CarbSim4580 <- F5CarbSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim4580) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8580 <- link(CarbSplit5, data=all8580cc5 )
simmeanCarb8580 <- data.frame(apply(simCarb8580,2,mean))
PICarb8580 <- t(data.frame(apply(simCarb8580,2,PI,prob=0.89)))
F5CarbSim8580 <- data.frame(cbind(simmeanCarb8580,PICarb8580))
F5CarbSim8580$PlotCN <- all8580cc5  $PlotCN
F5CarbSim8580 <- F5CarbSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F5CarbSim8580) <- c("PlotCN","CarbPerAcre","5CI","95CI")

#group data by RCP pathway
F5CarbSim26 <- F5CarbSim2625 %>% cbind(F5CarbSim2630[,2:4]) %>%
  cbind(F5CarbSim2635[,2:4]) %>% cbind(F5CarbSim2640[,2:4]) %>% cbind(F5CarbSim2645[,2:4]) %>%
  cbind(F5CarbSim2650[,2:4]) %>% cbind(F5CarbSim2655[,2:4]) %>% cbind(F5CarbSim2660[,2:4]) %>%
  cbind(F5CarbSim2665[,2:4]) %>% cbind(F5CarbSim2670[,2:4]) %>% cbind(F5CarbSim2675[,2:4]) %>%
  cbind(F5CarbSim2680[,2:4])
colnames(F5CarbSim26) <- CarbSimnames
write.csv(F5CarbSim26,file="F5CarbPred26.csv")

F5CarbSim45 <- F5CarbSim4525 %>% cbind(F5CarbSim4530[,2:4]) %>%
  cbind(F5CarbSim4535[,2:4]) %>% cbind(F5CarbSim4540[,2:4]) %>% cbind(F5CarbSim4545[,2:4]) %>%
  cbind(F5CarbSim4550[,2:4]) %>% cbind(F5CarbSim4555[,2:4]) %>% cbind(F5CarbSim4560[,2:4]) %>%
  cbind(F5CarbSim4565[,2:4]) %>% cbind(F5CarbSim4570[,2:4]) %>% cbind(F5CarbSim4575[,2:4]) %>%
  cbind(F5CarbSim4580[,2:4])
colnames(F5CarbSim45) <- CarbSimnames
write.csv(F5CarbSim45,file="F5CarbPred45.csv")

F5CarbSim85 <- F5CarbSim8525 %>% cbind(F5CarbSim8530[,2:4]) %>%
  cbind(F5CarbSim8535[,2:4]) %>% cbind(F5CarbSim8540[,2:4]) %>% cbind(F5CarbSim8545[,2:4]) %>%
  cbind(F5CarbSim8550[,2:4]) %>% cbind(F5CarbSim8555[,2:4]) %>% cbind(F5CarbSim8560[,2:4]) %>%
  cbind(F5CarbSim8565[,2:4]) %>% cbind(F5CarbSim8570[,2:4]) %>% cbind(F5CarbSim8575[,2:4]) %>%
  cbind(F5CarbSim8580[,2:4])
colnames(F5CarbSim85) <- CarbSimnames
write.csv(F5CarbSim85,file="F5CarbPred85.csv")


#fgroup 20
all2625cc20 <- all2625cc %>% filter(ForestGroup==20)
all4525cc20 <- all4525cc %>% filter(ForestGroup==20)
all8525cc20 <- all8525cc %>% filter(ForestGroup==20)
all2630cc20 <- all2630cc %>% filter(ForestGroup==20)
all4530cc20 <- all4530cc %>% filter(ForestGroup==20)
all8530cc20 <- all8530cc %>% filter(ForestGroup==20)
all2635cc20 <- all2635cc %>% filter(ForestGroup==20)
all4535cc20 <- all4535cc %>% filter(ForestGroup==20)
all8535cc20 <- all8535cc %>% filter(ForestGroup==20)
all2640cc20 <- all2640cc %>% filter(ForestGroup==20)
all4540cc20 <- all4540cc %>% filter(ForestGroup==20)
all8540cc20 <- all8540cc %>% filter(ForestGroup==20)
all2645cc20 <- all2645cc %>% filter(ForestGroup==20)
all4545cc20 <- all4545cc %>% filter(ForestGroup==20)
all8545cc20 <- all8545cc %>% filter(ForestGroup==20)
all2650cc20 <- all2650cc %>% filter(ForestGroup==20)
all4550cc20 <- all4550cc %>% filter(ForestGroup==20)
all8550cc20 <- all8550cc %>% filter(ForestGroup==20)
all2655cc20 <- all2655cc %>% filter(ForestGroup==20)
all4555cc20 <- all4555cc %>% filter(ForestGroup==20)
all8555cc20 <- all8555cc %>% filter(ForestGroup==20)
all2660cc20 <- all2660cc %>% filter(ForestGroup==20)
all4560cc20 <- all4560cc %>% filter(ForestGroup==20)
all8560cc20 <- all8560cc %>% filter(ForestGroup==20)
all2665cc20 <- all2665cc %>% filter(ForestGroup==20)
all4565cc20 <- all4565cc %>% filter(ForestGroup==20)
all8565cc20 <- all8565cc %>% filter(ForestGroup==20)
all2670cc20 <- all2670cc %>% filter(ForestGroup==20)
all4570cc20 <- all4570cc %>% filter(ForestGroup==20)
all8570cc20 <- all8570cc %>% filter(ForestGroup==20)
all2675cc20 <- all2675cc %>% filter(ForestGroup==20)
all4575cc20 <- all4575cc %>% filter(ForestGroup==20)
all8575cc20 <- all8575cc %>% filter(ForestGroup==20)
all2680cc20 <- all2680cc %>% filter(ForestGroup==20)
all4580cc20 <- all4580cc %>% filter(ForestGroup==20)
all8580cc20 <- all8580cc %>% filter(ForestGroup==20)

#turn soil order and forest type into factors
all2625cc20$Soil <- as.factor(all2625cc20$soil_order)
all2625cc20$Soil <- as.integer(all2625cc20$Soil)
all2625cc20$FType <- as.factor(all2625cc20$ForestType)
all2625cc20$FType <- as.integer(all2625cc20$FType)
all4525cc20$Soil <- as.factor(all4525cc20$soil_order)
all4525cc20$Soil <- as.integer(all4525cc20$Soil)
all4525cc20$FType <- as.factor(all4525cc20$ForestType)
all4525cc20$FType <- as.integer(all4525cc20$FType)
all8525cc20$Soil <- as.factor(all8525cc20$soil_order)
all8525cc20$Soil <- as.integer(all8525cc20$Soil)
all8525cc20$FType <- as.factor(all8525cc20$ForestType)
all8525cc20$FType <- as.integer(all8525cc20$FType)

all2630cc20$Soil <- as.factor(all2630cc20$soil_order)
all2630cc20$Soil <- as.integer(all2630cc20$Soil)
all2630cc20$FType <- as.factor(all2630cc20$ForestType)
all2630cc20$FType <- as.integer(all2630cc20$FType)
all4530cc20$Soil <- as.factor(all4530cc20$soil_order)
all4530cc20$Soil <- as.integer(all4530cc20$Soil)
all4530cc20$FType <- as.factor(all4530cc20$ForestType)
all4530cc20$FType <- as.integer(all4530cc20$FType)
all8530cc20$Soil <- as.factor(all8530cc20$soil_order)
all8530cc20$Soil <- as.integer(all8530cc20$Soil)
all8530cc20$FType <- as.factor(all8530cc20$ForestType)
all8530cc20$FType <- as.integer(all8530cc20$FType)

all2635cc20$Soil <- as.factor(all2635cc20$soil_order)
all2635cc20$Soil <- as.integer(all2635cc20$Soil)
all2635cc20$FType <- as.factor(all2635cc20$ForestType)
all2635cc20$FType <- as.integer(all2635cc20$FType)
all4535cc20$Soil <- as.factor(all4535cc20$soil_order)
all4535cc20$Soil <- as.integer(all4535cc20$Soil)
all4535cc20$FType <- as.factor(all4535cc20$ForestType)
all4535cc20$FType <- as.integer(all4535cc20$FType)
all8535cc20$Soil <- as.factor(all8535cc20$soil_order)
all8535cc20$Soil <- as.integer(all8535cc20$Soil)
all8535cc20$FType <- as.factor(all8535cc20$ForestType)
all8535cc20$FType <- as.integer(all8535cc20$FType)


all2640cc20$Soil <- as.factor(all2640cc20$soil_order)
all2640cc20$Soil <- as.integer(all2640cc20$Soil)
all2640cc20$FType <- as.factor(all2640cc20$ForestType)
all2640cc20$FType <- as.integer(all2640cc20$FType)
all4540cc20$Soil <- as.factor(all4540cc20$soil_order)
all4540cc20$Soil <- as.integer(all4540cc20$Soil)
all4540cc20$FType <- as.factor(all4540cc20$ForestType)
all4540cc20$FType <- as.integer(all4540cc20$FType)
all8540cc20$Soil <- as.factor(all8540cc20$soil_order)
all8540cc20$Soil <- as.integer(all8540cc20$Soil)
all8540cc20$FType <- as.factor(all8540cc20$ForestType)
all8540cc20$FType <- as.integer(all8540cc20$FType)

all2645cc20$Soil <- as.factor(all2645cc20$soil_order)
all2645cc20$Soil <- as.integer(all2645cc20$Soil)
all2645cc20$FType <- as.factor(all2645cc20$ForestType)
all2645cc20$FType <- as.integer(all2645cc20$FType)
all4545cc20$Soil <- as.factor(all4545cc20$soil_order)
all4545cc20$Soil <- as.integer(all4545cc20$Soil)
all4545cc20$FType <- as.factor(all4545cc20$ForestType)
all4545cc20$FType <- as.integer(all4545cc20$FType)
all8545cc20$Soil <- as.factor(all8545cc20$soil_order)
all8545cc20$Soil <- as.integer(all8545cc20$Soil)
all8545cc20$FType <- as.factor(all8545cc20$ForestType)
all8545cc20$FType <- as.integer(all8545cc20$FType)

all2650cc20$Soil <- as.factor(all2650cc20$soil_order)
all2650cc20$Soil <- as.integer(all2650cc20$Soil)
all2650cc20$FType <- as.factor(all2650cc20$ForestType)
all2650cc20$FType <- as.integer(all2650cc20$FType)
all4550cc20$Soil <- as.factor(all4550cc20$soil_order)
all4550cc20$Soil <- as.integer(all4550cc20$Soil)
all4550cc20$FType <- as.factor(all4550cc20$ForestType)
all4550cc20$FType <- as.integer(all4550cc20$FType)
all8550cc20$Soil <- as.factor(all8550cc20$soil_order)
all8550cc20$Soil <- as.integer(all8550cc20$Soil)
all8550cc20$FType <- as.factor(all8550cc20$ForestType)
all8550cc20$FType <- as.integer(all8550cc20$FType)

all2655cc20$Soil <- as.factor(all2655cc20$soil_order)
all2655cc20$Soil <- as.integer(all2655cc20$Soil)
all2655cc20$FType <- as.factor(all2655cc20$ForestType)
all2655cc20$FType <- as.integer(all2655cc20$FType)
all4555cc20$Soil <- as.factor(all4555cc20$soil_order)
all4555cc20$Soil <- as.integer(all4555cc20$Soil)
all4555cc20$FType <- as.factor(all4555cc20$ForestType)
all4555cc20$FType <- as.integer(all4555cc20$FType)
all8555cc20$Soil <- as.factor(all8555cc20$soil_order)
all8555cc20$Soil <- as.integer(all8555cc20$Soil)
all8555cc20$FType <- as.factor(all8555cc20$ForestType)
all8555cc20$FType <- as.integer(all8555cc20$FType)


all2660cc20$Soil <- as.factor(all2660cc20$soil_order)
all2660cc20$Soil <- as.integer(all2660cc20$Soil)
all2660cc20$FType <- as.factor(all2660cc20$ForestType)
all2660cc20$FType <- as.integer(all2660cc20$FType)
all4560cc20$Soil <- as.factor(all4560cc20$soil_order)
all4560cc20$Soil <- as.integer(all4560cc20$Soil)
all4560cc20$FType <- as.factor(all4560cc20$ForestType)
all4560cc20$FType <- as.integer(all4560cc20$FType)
all8560cc20$Soil <- as.factor(all8560cc20$soil_order)
all8560cc20$Soil <- as.integer(all8560cc20$Soil)
all8560cc20$FType <- as.factor(all8560cc20$ForestType)
all8560cc20$FType <- as.integer(all8560cc20$FType)

all2665cc20$Soil <- as.factor(all2665cc20$soil_order)
all2665cc20$Soil <- as.integer(all2665cc20$Soil)
all2665cc20$FType <- as.factor(all2665cc20$ForestType)
all2665cc20$FType <- as.integer(all2665cc20$FType)
all4565cc20$Soil <- as.factor(all4565cc20$soil_order)
all4565cc20$Soil <- as.integer(all4565cc20$Soil)
all4565cc20$FType <- as.factor(all4565cc20$ForestType)
all4565cc20$FType <- as.integer(all4565cc20$FType)
all8565cc20$Soil <- as.factor(all8565cc20$soil_order)
all8565cc20$Soil <- as.integer(all8565cc20$Soil)
all8565cc20$FType <- as.factor(all8565cc20$ForestType)
all8565cc20$FType <- as.integer(all8565cc20$FType)

all2670cc20$Soil <- as.factor(all2670cc20$soil_order)
all2670cc20$Soil <- as.integer(all2670cc20$Soil)
all2670cc20$FType <- as.factor(all2670cc20$ForestType)
all2670cc20$FType <- as.integer(all2670cc20$FType)
all4570cc20$Soil <- as.factor(all4570cc20$soil_order)
all4570cc20$Soil <- as.integer(all4570cc20$Soil)
all4570cc20$FType <- as.factor(all4570cc20$ForestType)
all4570cc20$FType <- as.integer(all4570cc20$FType)
all8570cc20$Soil <- as.factor(all8570cc20$soil_order)
all8570cc20$Soil <- as.integer(all8570cc20$Soil)
all8570cc20$FType <- as.factor(all8570cc20$ForestType)
all8570cc20$FType <- as.integer(all8570cc20$FType)


all2675cc20$Soil <- as.factor(all2675cc20$soil_order)
all2675cc20$Soil <- as.integer(all2675cc20$Soil)
all2675cc20$FType <- as.factor(all2675cc20$ForestType)
all2675cc20$FType <- as.integer(all2675cc20$FType)
all4575cc20$Soil <- as.factor(all4575cc20$soil_order)
all4575cc20$Soil <- as.integer(all4575cc20$Soil)
all4575cc20$FType <- as.factor(all4575cc20$ForestType)
all4575cc20$FType <- as.integer(all4575cc20$FType)
all8575cc20$Soil <- as.factor(all8575cc20$soil_order)
all8575cc20$Soil <- as.integer(all8575cc20$Soil)
all8575cc20$FType <- as.factor(all8575cc20$ForestType)
all8575cc20$FType <- as.integer(all8575cc20$FType)

all2680cc20$Soil <- as.factor(all2680cc20$soil_order)
all2680cc20$Soil <- as.integer(all2680cc20$Soil)
all2680cc20$FType <- as.factor(all2680cc20$ForestType)
all2680cc20$FType <- as.integer(all2680cc20$FType)
all4580cc20$Soil <- as.factor(all4580cc20$soil_order)
all4580cc20$Soil <- as.integer(all4580cc20$Soil)
all4580cc20$FType <- as.factor(all4580cc20$ForestType)
all4580cc20$FType <- as.integer(all4580cc20$FType)
all8580cc20$Soil <- as.factor(all8580cc20$soil_order)
all8580cc20$Soil <- as.integer(all8580cc20$Soil)
all8580cc20$FType <- as.factor(all8580cc20$ForestType)
all8580cc20$FType <- as.integer(all8580cc20$FType)

#center and scale from model data
all2625cc20 $MAT<- as.data.frame(scale(all2625cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4525cc20 $MAT<- as.data.frame(scale(all4525cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8525cc20 $MAT<- as.data.frame(scale(all8525cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all2630cc20 $MAT <- as.data.frame(scale(all2630cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4530cc20 $MAT <- as.data.frame(scale(all4530cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8530cc20 $MAT <- as.data.frame(scale(all8530cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all2635cc20 $MAT<- as.data.frame(scale(all2635cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4535cc20 $MAT<- as.data.frame(scale(all4535cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8535cc20 $MAT<- as.data.frame(scale(all8535cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all2640cc20 $MAT <- as.data.frame(scale(all2640cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4540cc20 $MAT <- as.data.frame(scale(all4540cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8540cc20 $MAT <- as.data.frame(scale(all8540cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all2645cc20 $MAT<- as.data.frame(scale(all2645cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4545cc20 $MAT<- as.data.frame(scale(all4545cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8545cc20 $MAT<- as.data.frame(scale(all8545cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all2650cc20 $MAT <- as.data.frame(scale(all2650cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4550cc20 $MAT <- as.data.frame(scale(all4550cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8550cc20 $MAT <- as.data.frame(scale(all8550cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all2655cc20 $MAT<- as.data.frame(scale(all2655cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4555cc20 $MAT<- as.data.frame(scale(all4555cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8555cc20 $MAT<- as.data.frame(scale(all8555cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all2660cc20 $MAT <- as.data.frame(scale(all2660cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4560cc20 $MAT <- as.data.frame(scale(all4560cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8560cc20 $MAT <- as.data.frame(scale(all8560cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all2665cc20 $MAT<- as.data.frame(scale(all2665cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4565cc20 $MAT<- as.data.frame(scale(all4565cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8565cc20 $MAT<- as.data.frame(scale(all8565cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all2670cc20 $MAT <- as.data.frame(scale(all2670cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4570cc20 $MAT <- as.data.frame(scale(all4570cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8570cc20 $MAT <- as.data.frame(scale(all8570cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all2675cc20 $MAT<- as.data.frame(scale(all2675cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4575cc20 $MAT<- as.data.frame(scale(all4575cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8575cc20 $MAT<- as.data.frame(scale(all8575cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all2680cc20 $MAT <- as.data.frame(scale(all2680cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all4580cc20 $MAT <- as.data.frame(scale(all4580cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
all8580cc20 $MAT <- as.data.frame(scale(all8580cc20 $MAT, center=centerMAT20,scale=scaleMAT20),col.names="MAT")[, 1]
#PPT
all2625cc20 $PPT<- as.data.frame(scale(all2625cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4525cc20 $PPT<- as.data.frame(scale(all4525cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8525cc20 $PPT<- as.data.frame(scale(all8525cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all2630cc20 $PPT <- as.data.frame(scale(all2630cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4530cc20 $PPT <- as.data.frame(scale(all4530cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8530cc20 $PPT <- as.data.frame(scale(all8530cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all2635cc20 $PPT<- as.data.frame(scale(all2635cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4535cc20 $PPT<- as.data.frame(scale(all4535cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8535cc20 $PPT<- as.data.frame(scale(all8535cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all2640cc20 $PPT <- as.data.frame(scale(all2640cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4540cc20 $PPT <- as.data.frame(scale(all4540cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8540cc20 $PPT <- as.data.frame(scale(all8540cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all2645cc20 $PPT<- as.data.frame(scale(all2645cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4545cc20 $PPT<- as.data.frame(scale(all4545cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8545cc20 $PPT<- as.data.frame(scale(all8545cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all2650cc20 $PPT <- as.data.frame(scale(all2650cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4550cc20 $PPT <- as.data.frame(scale(all4550cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8550cc20 $PPT <- as.data.frame(scale(all8550cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all2655cc20 $PPT<- as.data.frame(scale(all2655cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4555cc20 $PPT<- as.data.frame(scale(all4555cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8555cc20 $PPT<- as.data.frame(scale(all8555cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all2660cc20 $PPT <- as.data.frame(scale(all2660cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4560cc20 $PPT <- as.data.frame(scale(all4560cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8560cc20 $PPT <- as.data.frame(scale(all8560cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all2665cc20 $PPT<- as.data.frame(scale(all2665cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4565cc20 $PPT<- as.data.frame(scale(all4565cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8565cc20 $PPT<- as.data.frame(scale(all8565cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all2670cc20 $PPT <- as.data.frame(scale(all2670cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4570cc20 $PPT <- as.data.frame(scale(all4570cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8570cc20 $PPT <- as.data.frame(scale(all8570cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all2675cc20 $PPT<- as.data.frame(scale(all2675cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4575cc20 $PPT<- as.data.frame(scale(all4575cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8575cc20 $PPT<- as.data.frame(scale(all8575cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all2680cc20 $PPT <- as.data.frame(scale(all2680cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all4580cc20 $PPT <- as.data.frame(scale(all4580cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
all8580cc20 $PPT <- as.data.frame(scale(all8580cc20 $PPT, center=centerPPT20,scale=scalePPT20),col.names="PPT")[, 1]
#RHUM
all2625cc20 $RHUM<- as.data.frame(scale(all2625cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4525cc20 $RHUM<- as.data.frame(scale(all4525cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8525cc20 $RHUM<- as.data.frame(scale(all8525cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all2630cc20 $RHUM <- as.data.frame(scale(all2630cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4530cc20 $RHUM <- as.data.frame(scale(all4530cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8530cc20 $RHUM <- as.data.frame(scale(all8530cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all2635cc20 $RHUM<- as.data.frame(scale(all2635cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4535cc20 $RHUM<- as.data.frame(scale(all4535cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8535cc20 $RHUM<- as.data.frame(scale(all8535cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all2640cc20 $RHUM <- as.data.frame(scale(all2640cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4540cc20 $RHUM <- as.data.frame(scale(all4540cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8540cc20 $RHUM <- as.data.frame(scale(all8540cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all2645cc20 $RHUM<- as.data.frame(scale(all2645cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4545cc20 $RHUM<- as.data.frame(scale(all4545cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8545cc20 $RHUM<- as.data.frame(scale(all8545cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all2650cc20 $RHUM <- as.data.frame(scale(all2650cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4550cc20 $RHUM <- as.data.frame(scale(all4550cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8550cc20 $RHUM <- as.data.frame(scale(all8550cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all2655cc20 $RHUM<- as.data.frame(scale(all2655cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4555cc20 $RHUM<- as.data.frame(scale(all4555cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8555cc20 $RHUM<- as.data.frame(scale(all8555cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all2660cc20 $RHUM <- as.data.frame(scale(all2660cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4560cc20 $RHUM <- as.data.frame(scale(all4560cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8560cc20 $RHUM <- as.data.frame(scale(all8560cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all2665cc20 $RHUM<- as.data.frame(scale(all2665cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4565cc20 $RHUM<- as.data.frame(scale(all4565cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8565cc20 $RHUM<- as.data.frame(scale(all8565cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all2670cc20 $RHUM <- as.data.frame(scale(all2670cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4570cc20 $RHUM <- as.data.frame(scale(all4570cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8570cc20 $RHUM <- as.data.frame(scale(all8570cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all2675cc20 $RHUM<- as.data.frame(scale(all2675cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4575cc20 $RHUM<- as.data.frame(scale(all4575cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8575cc20 $RHUM<- as.data.frame(scale(all8575cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all2680cc20 $RHUM <- as.data.frame(scale(all2680cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all4580cc20 $RHUM <- as.data.frame(scale(all4580cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
all8580cc20 $RHUM <- as.data.frame(scale(all8580cc20 $RHUM, center=centerRHUM20,scale=scaleRHUM20),col.names="RHUM")[, 1]
#RAD
all2625cc20 $RAD<- as.data.frame(scale(all2625cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4525cc20 $RAD<- as.data.frame(scale(all4525cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8525cc20 $RAD<- as.data.frame(scale(all8525cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all2630cc20 $RAD <- as.data.frame(scale(all2630cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4530cc20 $RAD <- as.data.frame(scale(all4530cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8530cc20 $RAD <- as.data.frame(scale(all8530cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all2635cc20 $RAD<- as.data.frame(scale(all2635cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4535cc20 $RAD<- as.data.frame(scale(all4535cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8535cc20 $RAD<- as.data.frame(scale(all8535cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all2640cc20 $RAD <- as.data.frame(scale(all2640cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4540cc20 $RAD <- as.data.frame(scale(all4540cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8540cc20 $RAD <- as.data.frame(scale(all8540cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all2645cc20 $RAD<- as.data.frame(scale(all2645cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4545cc20 $RAD<- as.data.frame(scale(all4545cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8545cc20 $RAD<- as.data.frame(scale(all8545cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all2650cc20 $RAD <- as.data.frame(scale(all2650cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4550cc20 $RAD <- as.data.frame(scale(all4550cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8550cc20 $RAD <- as.data.frame(scale(all8550cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all2655cc20 $RAD<- as.data.frame(scale(all2655cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4555cc20 $RAD<- as.data.frame(scale(all4555cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8555cc20 $RAD<- as.data.frame(scale(all8555cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all2660cc20 $RAD <- as.data.frame(scale(all2660cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4560cc20 $RAD <- as.data.frame(scale(all4560cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8560cc20 $RAD <- as.data.frame(scale(all8560cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all2665cc20 $RAD<- as.data.frame(scale(all2665cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4565cc20 $RAD<- as.data.frame(scale(all4565cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8565cc20 $RAD<- as.data.frame(scale(all8565cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all2670cc20 $RAD <- as.data.frame(scale(all2670cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4570cc20 $RAD <- as.data.frame(scale(all4570cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8570cc20 $RAD <- as.data.frame(scale(all8570cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all2675cc20 $RAD<- as.data.frame(scale(all2675cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4575cc20 $RAD<- as.data.frame(scale(all4575cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8575cc20 $RAD<- as.data.frame(scale(all8575cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all2680cc20 $RAD <- as.data.frame(scale(all2680cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all4580cc20 $RAD <- as.data.frame(scale(all4580cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
all8580cc20 $RAD <- as.data.frame(scale(all8580cc20 $RAD, center=centerRAD20,scale=scaleRAD20),col.names="RAD")[, 1]
#Elevation
all2625cc20 $Elevation<- as.data.frame(scale(all2625cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4525cc20 $Elevation<- as.data.frame(scale(all4525cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8525cc20 $Elevation<- as.data.frame(scale(all8525cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all2630cc20 $Elevation <- as.data.frame(scale(all2630cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4530cc20 $Elevation <- as.data.frame(scale(all4530cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8530cc20 $Elevation <- as.data.frame(scale(all8530cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all2635cc20 $Elevation<- as.data.frame(scale(all2635cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4535cc20 $Elevation<- as.data.frame(scale(all4535cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8535cc20 $Elevation<- as.data.frame(scale(all8535cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all2640cc20 $Elevation <- as.data.frame(scale(all2640cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4540cc20 $Elevation <- as.data.frame(scale(all4540cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8540cc20 $Elevation <- as.data.frame(scale(all8540cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all2645cc20 $Elevation<- as.data.frame(scale(all2645cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4545cc20 $Elevation<- as.data.frame(scale(all4545cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8545cc20 $Elevation<- as.data.frame(scale(all8545cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all2650cc20 $Elevation <- as.data.frame(scale(all2650cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4550cc20 $Elevation <- as.data.frame(scale(all4550cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8550cc20 $Elevation <- as.data.frame(scale(all8550cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all2655cc20 $Elevation<- as.data.frame(scale(all2655cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4555cc20 $Elevation<- as.data.frame(scale(all4555cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8555cc20 $Elevation<- as.data.frame(scale(all8555cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all2660cc20 $Elevation <- as.data.frame(scale(all2660cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4560cc20 $Elevation <- as.data.frame(scale(all4560cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8560cc20 $Elevation <- as.data.frame(scale(all8560cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all2665cc20 $Elevation<- as.data.frame(scale(all2665cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4565cc20 $Elevation<- as.data.frame(scale(all4565cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8565cc20 $Elevation<- as.data.frame(scale(all8565cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all2670cc20 $Elevation <- as.data.frame(scale(all2670cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4570cc20 $Elevation <- as.data.frame(scale(all4570cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8570cc20 $Elevation <- as.data.frame(scale(all8570cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all2675cc20 $Elevation<- as.data.frame(scale(all2675cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4575cc20 $Elevation<- as.data.frame(scale(all4575cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8575cc20 $Elevation<- as.data.frame(scale(all8575cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all2680cc20 $Elevation <- as.data.frame(scale(all2680cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all4580cc20 $Elevation <- as.data.frame(scale(all4580cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
all8580cc20 $Elevation <- as.data.frame(scale(all8580cc20 $Elevation, center=centerElevation20,scale=scaleElevation20),col.names="Elevation")[, 1]
#basal area
all2625cc20$BA <- (all2625cc20$BA)+(all2625cc20$AvgBADeadRate)*6
all4525cc20$BA <- (all4525cc20$BA)+(all4525cc20$AvgBADeadRate)*6
all8525cc20$BA <- (all8525cc20$BA)+(all8525cc20$AvgBADeadRate)*6
all2630cc20$BA <- (all2630cc20$BA)+(all2630cc20$AvgBADeadRate)*11
all4530cc20$BA <- (all4530cc20$BA)+(all4530cc20$AvgBADeadRate)*11
all8530cc20$BA <- (all8530cc20$BA)+(all8530cc20$AvgBADeadRate)*11
all2635cc20$BA <- (all2635cc20$BA)+(all2635cc20$AvgBADeadRate)*16
all4535cc20$BA <-  (all4535cc20$BA)+(all4535cc20$AvgBADeadRate)*16
all8535cc20$BA <- (all8535cc20$BA)+(all8535cc20$AvgBADeadRate)*16
all2640cc20$BA <- (all2640cc20$BA)+(all2640cc20$AvgBADeadRate)*21
all4540cc20$BA <- (all4540cc20$BA)+(all4540cc20$AvgBADeadRate)*21
all8540cc20$BA <-  (all8540cc20$BA)+(all8540cc20$AvgBADeadRate)*21
all2645cc20$BA <- (all2645cc20$BA)+(all2645cc20$AvgBADeadRate)*26
all4545cc20$BA <- (all4545cc20$BA)+(all4545cc20$AvgBADeadRate)*26
all8545cc20$BA <- (all8545cc20$BA)+(all8545cc20$AvgBADeadRate)*26
all2650cc20$BA <- (all2650cc20$BA)+(all2650cc20$AvgBADeadRate)*31
all4550cc20$BA <- (all4550cc20$BA)+(all4550cc20$AvgBADeadRate)*31
all8550cc20$BA <- (all8550cc20$BA)+(all8550cc20$AvgBADeadRate)*31
all2655cc20$BA <-(all2655cc20$BA)+(all2655cc20$AvgBADeadRate)*36
all4555cc20$BA <- (all4555cc20$BA)+(all4555cc20$AvgBADeadRate)*36
all8555cc20$BA <- (all8555cc20$BA)+(all8555cc20$AvgBADeadRate)*36
all2660cc20$BA <-(all2660cc20$BA)+(all2660cc20$AvgBADeadRate)*41
all4560cc20$BA <- (all4560cc20$BA)+(all4560cc20$AvgBADeadRate)*41
all8560cc20$BA <- (all8560cc20$BA)+(all8560cc20$AvgBADeadRate)*41
all2665cc20$BA <- (all2665cc20$BA)+(all2665cc20$AvgBADeadRate)*46
all4565cc20$BA <- (all4565cc20$BA)+(all4565cc20$AvgBADeadRate)*46
all8565cc20$BA <- (all8565cc20$BA)+(all8565cc20$AvgBADeadRate)*46
all2670cc20$BA <- (all2670cc20$BA)+(all2670cc20$AvgBADeadRate)*51
all4570cc20$BA <- (all4570cc20$BA)+(all4570cc20$AvgBADeadRate)*51
all8570cc20$BA <- (all8570cc20$BA)+(all8570cc20$AvgBADeadRate)*51
all2675cc20$BA <- (all2675cc20$BA)+(all2675cc20$AvgBADeadRate)*56
all4575cc20$BA <- (all4575cc20$BA)+(all4575cc20$AvgBADeadRate)*56
all8575cc20$BA <- (all8575cc20$BA)+(all8575cc20$AvgBADeadRate)*56
all2680cc20$BA <- (all2680cc20$BA)+(all2680cc20$AvgBADeadRate)*61
all4580cc20$BA <- (all4580cc20$BA)+(all4580cc20$AvgBADeadRate)*61
all8580cc20$BA <- (all8580cc20$BA)+(all8580cc20$AvgBADeadRate)*61

#remove negatives
all2625cc20$BA[all2625cc20$BA<=0] <- 0.01
all4525cc20$BA[all4525cc20$BA<=0] <- 0.01
all8525cc20$BA[all8525cc20$BA<=0] <- 0.01
all2630cc20$BA[all2630cc20$BA<=0] <- 0.01
all4530cc20$BA[all4530cc20$BA<=0] <- 0.01
all8530cc20$BA[all8530cc20$BA<=0] <- 0.01
all2635cc20$BA[all2635cc20$BA<=0] <- 0.01
all4535cc20$BA[all4535cc20$BA<=0] <- 0.01
all8535cc20$BA[all8535cc20$BA<=0] <- 0.01
all2640cc20$BA[all2640cc20$BA<=0] <- 0.01
all4540cc20$BA[all4540cc20$BA<=0] <- 0.01
all8540cc20$BA[all8540cc20$BA<=0] <- 0.01
all2645cc20$BA[all2645cc20$BA<=0] <- 0.01
all4545cc20$BA[all4545cc20$BA<=0] <- 0.01
all8545cc20$BA[all8545cc20$BA<=0] <- 0.01
all2650cc20$BA[all2650cc20$BA<=0] <- 0.01
all4550cc20$BA[all4550cc20$BA<=0] <- 0.01
all8550cc20$BA[all8550cc20$BA<=0] <- 0.01
all2655cc20$BA[all2655cc20$BA<=0] <- 0.01
all4555cc20$BA[all4555cc20$BA<=0] <- 0.01
all8555cc20$BA[all8555cc20$BA<=0] <- 0.01
all2660cc20$BA[all2660cc20$BA<=0] <- 0.01
all4560cc20$BA[all4560cc20$BA<=0] <- 0.01
all8560cc20$BA[all8560cc20$BA<=0] <- 0.01
all2665cc20$BA[all2665cc20$BA<=0] <- 0.01
all4565cc20$BA[all4565cc20$BA<=0] <- 0.01
all8565cc20$BA[all8565cc20$BA<=0] <- 0.01
all2670cc20$BA[all2670cc20$BA<=0] <- 0.01
all4570cc20$BA[all4570cc20$BA<=0] <- 0.01
all8570cc20$BA[all8570cc20$BA<=0] <- 0.01
all2675cc20$BA[all2675cc20$BA<=0] <- 0.01
all4575cc20$BA[all4575cc20$BA<=0] <- 0.01
all8575cc20$BA[all8575cc20$BA<=0] <- 0.01
all2680cc20$BA[all2680cc20$BA<=0] <- 0.01
all4580cc20$BA[all4580cc20$BA<=0] <- 0.01
all8580cc20$BA[all8580cc20$BA<=0] <- 0.01

#max basal area of dead trees = basal area of all trees. prevents mortality over 100%, not possible.
all2625cc20$BA <- ifelse(all2625cc20$BA > all2625cc20$BAAll, all2625cc20$BAAll, all2625cc20$BA)
all4525cc20$BA <- ifelse(all4525cc20$BA > all4525cc20$BAAll, all4525cc20$BAAll, all4525cc20$BA)
all8525cc20$BA <- ifelse(all8525cc20$BA > all8525cc20$BAAll, all8525cc20$BAAll, all8525cc20$BA)
all2630cc20$BA <- ifelse(all2630cc20$BA > all2630cc20$BAAll, all2630cc20$BAAll, all2630cc20$BA)
all4530cc20$BA <- ifelse(all4530cc20$BA > all4530cc20$BAAll, all4530cc20$BAAll, all4530cc20$BA)
all8530cc20$BA <- ifelse(all8530cc20$BA > all8530cc20$BAAll, all8530cc20$BAAll, all8530cc20$BA)
all2635cc20$BA <- ifelse(all2635cc20$BA > all2635cc20$BAAll, all2635cc20$BAAll, all2635cc20$BA)
all4535cc20$BA <- ifelse(all4535cc20$BA > all4535cc20$BAAll, all4535cc20$BAAll, all4535cc20$BA)
all8535cc20$BA <- ifelse(all8535cc20$BA > all8535cc20$BAAll, all8535cc20$BAAll, all8535cc20$BA)
all2640cc20$BA <- ifelse(all2640cc20$BA > all2640cc20$BAAll, all2640cc20$BAAll, all2640cc20$BA)
all4540cc20$BA <- ifelse(all4540cc20$BA > all4540cc20$BAAll, all4540cc20$BAAll, all4540cc20$BA)
all8540cc20$BA <- ifelse(all8540cc20$BA > all8540cc20$BAAll, all8540cc20$BAAll, all8540cc20$BA)
all2645cc20$BA <- ifelse(all2645cc20$BA > all2645cc20$BAAll, all2645cc20$BAAll, all2645cc20$BA)
all4545cc20$BA <- ifelse(all4545cc20$BA > all4545cc20$BAAll, all4545cc20$BAAll, all4545cc20$BA)
all8545cc20$BA <- ifelse(all8545cc20$BA > all8545cc20$BAAll, all8545cc20$BAAll, all8545cc20$BA)
all2650cc20$BA <- ifelse(all2650cc20$BA > all2650cc20$BAAll, all2650cc20$BAAll, all2650cc20$BA)
all4550cc20$BA <- ifelse(all4550cc20$BA > all4550cc20$BAAll, all4550cc20$BAAll, all4550cc20$BA)
all8550cc20$BA <- ifelse(all8550cc20$BA > all8550cc20$BAAll, all8550cc20$BAAll, all8550cc20$BA)
all2655cc20$BA <- ifelse(all2655cc20$BA > all2655cc20$BAAll, all2655cc20$BAAll, all2655cc20$BA)
all4555cc20$BA <- ifelse(all4555cc20$BA > all4555cc20$BAAll, all4555cc20$BAAll, all4555cc20$BA)
all8555cc20$BA <- ifelse(all8555cc20$BA > all8555cc20$BAAll, all8555cc20$BAAll, all8555cc20$BA)
all2660cc20$BA <- ifelse(all2660cc20$BA > all2660cc20$BAAll, all2660cc20$BAAll, all2660cc20$BA)
all4560cc20$BA <- ifelse(all4560cc20$BA > all4560cc20$BAAll, all4560cc20$BAAll, all4560cc20$BA)
all8560cc20$BA <- ifelse(all8560cc20$BA > all8560cc20$BAAll, all8560cc20$BAAll, all8560cc20$BA)
all2665cc20$BA <- ifelse(all2665cc20$BA > all2665cc20$BAAll, all2665cc20$BAAll, all2665cc20$BA)
all4565cc20$BA <- ifelse(all4565cc20$BA > all4565cc20$BAAll, all4565cc20$BAAll, all4565cc20$BA)
all8565cc20$BA <- ifelse(all8565cc20$BA > all8565cc20$BAAll, all8565cc20$BAAll, all8565cc20$BA)
all2670cc20$BA <- ifelse(all2670cc20$BA > all2670cc20$BAAll, all2670cc20$BAAll, all2670cc20$BA)
all4570cc20$BA <- ifelse(all4570cc20$BA > all4570cc20$BAAll, all4570cc20$BAAll, all4570cc20$BA)
all8570cc20$BA <- ifelse(all8570cc20$BA > all8570cc20$BAAll, all8570cc20$BAAll, all8570cc20$BA)
all2675cc20$BA <- ifelse(all2675cc20$BA > all2675cc20$BAAll, all2675cc20$BAAll, all2675cc20$BA)
all4575cc20$BA <- ifelse(all4575cc20$BA > all4575cc20$BAAll, all4575cc20$BAAll, all4575cc20$BA)
all8575cc20$BA <- ifelse(all8575cc20$BA > all8575cc20$BAAll, all8575cc20$BAAll, all8575cc20$BA)
all2680cc20$BA <- ifelse(all2680cc20$BA > all2680cc20$BAAll, all2680cc20$BAAll, all2680cc20$BA)
all4580cc20$BA <- ifelse(all4580cc20$BA > all4580cc20$BAAll, all4580cc20$BAAll, all4580cc20$BA)
all8580cc20$BA <- ifelse(all8580cc20$BA > all8580cc20$BAAll, all8580cc20$BAAll, all8580cc20$BA)

all2625cc20 $BA<- as.data.frame(scale(all2625cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4525cc20 $BA<- as.data.frame(scale(all4525cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8525cc20 $BA<- as.data.frame(scale(all8525cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all2630cc20 $BA <- as.data.frame(scale(all2630cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4530cc20 $BA <- as.data.frame(scale(all4530cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8530cc20 $BA <- as.data.frame(scale(all8530cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all2635cc20 $BA<- as.data.frame(scale(all2635cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4535cc20 $BA<- as.data.frame(scale(all4535cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8535cc20 $BA<- as.data.frame(scale(all8535cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all2640cc20 $BA <- as.data.frame(scale(all2640cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4540cc20 $BA <- as.data.frame(scale(all4540cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8540cc20 $BA <- as.data.frame(scale(all8540cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all2645cc20 $BA<- as.data.frame(scale(all2645cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4545cc20 $BA<- as.data.frame(scale(all4545cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8545cc20 $BA<- as.data.frame(scale(all8545cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all2650cc20 $BA <- as.data.frame(scale(all2650cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4550cc20 $BA <- as.data.frame(scale(all4550cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8550cc20 $BA <- as.data.frame(scale(all8550cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all2655cc20 $BA<- as.data.frame(scale(all2655cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4555cc20 $BA<- as.data.frame(scale(all4555cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8555cc20 $BA<- as.data.frame(scale(all8555cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all2660cc20 $BA <- as.data.frame(scale(all2660cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4560cc20 $BA <- as.data.frame(scale(all4560cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8560cc20 $BA <- as.data.frame(scale(all8560cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all2665cc20 $BA<- as.data.frame(scale(all2665cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4565cc20 $BA<- as.data.frame(scale(all4565cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8565cc20 $BA<- as.data.frame(scale(all8565cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all2670cc20 $BA <- as.data.frame(scale(all2670cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4570cc20 $BA <- as.data.frame(scale(all4570cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8570cc20 $BA <- as.data.frame(scale(all8570cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all2675cc20 $BA<- as.data.frame(scale(all2675cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4575cc20 $BA<- as.data.frame(scale(all4575cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8575cc20 $BA<- as.data.frame(scale(all8575cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all2680cc20 $BA <- as.data.frame(scale(all2680cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all4580cc20 $BA <- as.data.frame(scale(all4580cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]
all8580cc20 $BA <- as.data.frame(scale(all8580cc20 $BA, center=centerBA20,scale=scaleBA20),col.names="BA")[, 1]

#time weight f20
all2625cc20$time_weight <- 2.026288
all4525cc20$time_weight <- 2.026288
all8525cc20$time_weight <- 2.026288
all2630cc20$time_weight <- 2.026288
all4530cc20$time_weight <- 2.026288
all8530cc20$time_weight <- 2.026288
all2635cc20$time_weight <- 2.026288
all4535cc20$time_weight <- 2.026288
all8535cc20$time_weight <- 2.026288
all2640cc20$time_weight <- 2.026288
all4540cc20$time_weight <- 2.026288
all8540cc20$time_weight <- 2.026288
all2645cc20$time_weight <- 2.026288
all4545cc20$time_weight <- 2.026288
all8545cc20$time_weight <- 2.026288
all2650cc20$time_weight <- 2.026288
all4550cc20$time_weight <- 2.026288
all8550cc20$time_weight <- 2.026288
all2655cc20$time_weight <- 2.026288
all4555cc20$time_weight <- 2.026288
all8555cc20$time_weight <- 2.026288
all2660cc20$time_weight <- 2.026288
all4560cc20$time_weight <- 2.026288
all8560cc20$time_weight <- 2.026288
all2665cc20$time_weight <- 2.026288
all4565cc20$time_weight <- 2.026288
all8565cc20$time_weight <- 2.026288
all2670cc20$time_weight <- 2.026288
all4570cc20$time_weight <- 2.026288
all8570cc20$time_weight <- 2.026288
all2675cc20$time_weight <- 2.026288
all4575cc20$time_weight <- 2.026288
all8575cc20$time_weight <- 2.026288
all2680cc20$time_weight <- 2.026288
all4580cc20$time_weight <- 2.026288
all8580cc20$time_weight <- 2.026288

#run simulations
simCarb2625 <- link(CarbSplit20, data=all2625cc20 )
simmeanCarb2625 <- data.frame(apply(simCarb2625,2,mean))
PICarb2625 <- t(data.frame(apply(simCarb2625,2,PI,prob=0.89)))
F20CarbSim2625 <- data.frame(cbind(simmeanCarb2625,PICarb2625))
F20CarbSim2625$PlotCN <- all2625cc20  $PlotCN
F20CarbSim2625 <- F20CarbSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2625) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4525 <- link(CarbSplit20, data=all4525cc20 )
simmeanCarb4525 <- data.frame(apply(simCarb4525,2,mean))
PICarb4525 <- t(data.frame(apply(simCarb4525,2,PI,prob=0.89)))
F20CarbSim4525 <- data.frame(cbind(simmeanCarb4525,PICarb4525))
F20CarbSim4525$PlotCN <- all4525cc20  $PlotCN
F20CarbSim4525 <- F20CarbSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4525) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8525 <- link(CarbSplit20, data=all8525cc20 )
simmeanCarb8525 <- data.frame(apply(simCarb8525,2,mean))
PICarb8525 <- t(data.frame(apply(simCarb8525,2,PI,prob=0.89)))
F20CarbSim8525 <- data.frame(cbind(simmeanCarb8525,PICarb8525))
F20CarbSim8525$PlotCN <- all8525cc20  $PlotCN
F20CarbSim8525 <- F20CarbSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8525) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2630 <- link(CarbSplit20, data=all2630cc20 )
simmeanCarb2630 <- data.frame(apply(simCarb2630,2,mean))
PICarb2630 <- t(data.frame(apply(simCarb2630,2,PI,prob=0.89)))
F20CarbSim2630 <- data.frame(cbind(simmeanCarb2630,PICarb2630))
F20CarbSim2630$PlotCN <- all2630cc20  $PlotCN
F20CarbSim2630 <- F20CarbSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2630) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4530 <- link(CarbSplit20, data=all4530cc20 )
simmeanCarb4530 <- data.frame(apply(simCarb4530,2,mean))
PICarb4530 <- t(data.frame(apply(simCarb4530,2,PI,prob=0.89)))
F20CarbSim4530 <- data.frame(cbind(simmeanCarb4530,PICarb4530))
F20CarbSim4530$PlotCN <- all4530cc20  $PlotCN
F20CarbSim4530 <- F20CarbSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4530) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8530 <- link(CarbSplit20, data=all8530cc20 )
simmeanCarb8530 <- data.frame(apply(simCarb8530,2,mean))
PICarb8530 <- t(data.frame(apply(simCarb8530,2,PI,prob=0.89)))
F20CarbSim8530 <- data.frame(cbind(simmeanCarb8530,PICarb8530))
F20CarbSim8530$PlotCN <- all8530cc20  $PlotCN
F20CarbSim8530 <- F20CarbSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8530) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2635 <- link(CarbSplit20, data=all2635cc20 )
simmeanCarb2635 <- data.frame(apply(simCarb2635,2,mean))
PICarb2635 <- t(data.frame(apply(simCarb2635,2,PI,prob=0.89)))
F20CarbSim2635 <- data.frame(cbind(simmeanCarb2635,PICarb2635))
F20CarbSim2635$PlotCN <- all2635cc20  $PlotCN
F20CarbSim2635 <- F20CarbSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2635) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4535 <- link(CarbSplit20, data=all4535cc20 )
simmeanCarb4535 <- data.frame(apply(simCarb4535,2,mean))
PICarb4535 <- t(data.frame(apply(simCarb4535,2,PI,prob=0.89)))
F20CarbSim4535 <- data.frame(cbind(simmeanCarb4535,PICarb4535))
F20CarbSim4535$PlotCN <- all4535cc20  $PlotCN
F20CarbSim4535 <- F20CarbSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4535) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8535 <- link(CarbSplit20, data=all8535cc20 )
simmeanCarb8535 <- data.frame(apply(simCarb8535,2,mean))
PICarb8535 <- t(data.frame(apply(simCarb8535,2,PI,prob=0.89)))
F20CarbSim8535 <- data.frame(cbind(simmeanCarb8535,PICarb8535))
F20CarbSim8535$PlotCN <- all8535cc20  $PlotCN
F20CarbSim8535 <- F20CarbSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8535) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2640 <- link(CarbSplit20, data=all2640cc20 )
simmeanCarb2640 <- data.frame(apply(simCarb2640,2,mean))
PICarb2640 <- t(data.frame(apply(simCarb2640,2,PI,prob=0.89)))
F20CarbSim2640 <- data.frame(cbind(simmeanCarb2640,PICarb2640))
F20CarbSim2640$PlotCN <- all2640cc20  $PlotCN
F20CarbSim2640 <- F20CarbSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2640) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4540 <- link(CarbSplit20, data=all4540cc20 )
simmeanCarb4540 <- data.frame(apply(simCarb4540,2,mean))
PICarb4540 <- t(data.frame(apply(simCarb4540,2,PI,prob=0.89)))
F20CarbSim4540 <- data.frame(cbind(simmeanCarb4540,PICarb4540))
F20CarbSim4540$PlotCN <- all4540cc20  $PlotCN
F20CarbSim4540 <- F20CarbSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4540) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8540 <- link(CarbSplit20, data=all8540cc20 )
simmeanCarb8540 <- data.frame(apply(simCarb8540,2,mean))
PICarb8540 <- t(data.frame(apply(simCarb8540,2,PI,prob=0.89)))
F20CarbSim8540 <- data.frame(cbind(simmeanCarb8540,PICarb8540))
F20CarbSim8540$PlotCN <- all8540cc20  $PlotCN
F20CarbSim8540 <- F20CarbSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8540) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2645 <- link(CarbSplit20, data=all2645cc20 )
simmeanCarb2645 <- data.frame(apply(simCarb2645,2,mean))
PICarb2645 <- t(data.frame(apply(simCarb2645,2,PI,prob=0.89)))
F20CarbSim2645 <- data.frame(cbind(simmeanCarb2645,PICarb2645))
F20CarbSim2645$PlotCN <- all2645cc20  $PlotCN
F20CarbSim2645 <- F20CarbSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2645) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4545 <- link(CarbSplit20, data=all4545cc20 )
simmeanCarb4545 <- data.frame(apply(simCarb4545,2,mean))
PICarb4545 <- t(data.frame(apply(simCarb4545,2,PI,prob=0.89)))
F20CarbSim4545 <- data.frame(cbind(simmeanCarb4545,PICarb4545))
F20CarbSim4545$PlotCN <- all4545cc20  $PlotCN
F20CarbSim4545 <- F20CarbSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4545) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8545 <- link(CarbSplit20, data=all8545cc20 )
simmeanCarb8545 <- data.frame(apply(simCarb8545,2,mean))
PICarb8545 <- t(data.frame(apply(simCarb8545,2,PI,prob=0.89)))
F20CarbSim8545 <- data.frame(cbind(simmeanCarb8545,PICarb8545))
F20CarbSim8545$PlotCN <- all8545cc20  $PlotCN
F20CarbSim8545 <- F20CarbSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8545) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2650 <- link(CarbSplit20, data=all2650cc20 )
simmeanCarb2650 <- data.frame(apply(simCarb2650,2,mean))
PICarb2650 <- t(data.frame(apply(simCarb2650,2,PI,prob=0.89)))
F20CarbSim2650 <- data.frame(cbind(simmeanCarb2650,PICarb2650))
F20CarbSim2650$PlotCN <- all2650cc20  $PlotCN
F20CarbSim2650 <- F20CarbSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2650) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4550 <- link(CarbSplit20, data=all4550cc20 )
simmeanCarb4550 <- data.frame(apply(simCarb4550,2,mean))
PICarb4550 <- t(data.frame(apply(simCarb4550,2,PI,prob=0.89)))
F20CarbSim4550 <- data.frame(cbind(simmeanCarb4550,PICarb4550))
F20CarbSim4550$PlotCN <- all4550cc20  $PlotCN
F20CarbSim4550 <- F20CarbSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4550) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8550 <- link(CarbSplit20, data=all8550cc20 )
simmeanCarb8550 <- data.frame(apply(simCarb8550,2,mean))
PICarb8550 <- t(data.frame(apply(simCarb8550,2,PI,prob=0.89)))
F20CarbSim8550 <- data.frame(cbind(simmeanCarb8550,PICarb8550))
F20CarbSim8550$PlotCN <- all8550cc20  $PlotCN
F20CarbSim8550 <- F20CarbSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8550) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2655 <- link(CarbSplit20, data=all2655cc20 )
simmeanCarb2655 <- data.frame(apply(simCarb2655,2,mean))
PICarb2655 <- t(data.frame(apply(simCarb2655,2,PI,prob=0.89)))
F20CarbSim2655 <- data.frame(cbind(simmeanCarb2655,PICarb2655))
F20CarbSim2655$PlotCN <- all2655cc20  $PlotCN
F20CarbSim2655 <- F20CarbSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2655) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4555 <- link(CarbSplit20, data=all4555cc20 )
simmeanCarb4555 <- data.frame(apply(simCarb4555,2,mean))
PICarb4555 <- t(data.frame(apply(simCarb4555,2,PI,prob=0.89)))
F20CarbSim4555 <- data.frame(cbind(simmeanCarb4555,PICarb4555))
F20CarbSim4555$PlotCN <- all4555cc20  $PlotCN
F20CarbSim4555 <- F20CarbSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4555) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8555 <- link(CarbSplit20, data=all8555cc20 )
simmeanCarb8555 <- data.frame(apply(simCarb8555,2,mean))
PICarb8555 <- t(data.frame(apply(simCarb8555,2,PI,prob=0.89)))
F20CarbSim8555 <- data.frame(cbind(simmeanCarb8555,PICarb8555))
F20CarbSim8555$PlotCN <- all8555cc20  $PlotCN
F20CarbSim8555 <- F20CarbSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8555) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2660 <- link(CarbSplit20, data=all2660cc20 )
simmeanCarb2660 <- data.frame(apply(simCarb2660,2,mean))
PICarb2660 <- t(data.frame(apply(simCarb2660,2,PI,prob=0.89)))
F20CarbSim2660 <- data.frame(cbind(simmeanCarb2660,PICarb2660))
F20CarbSim2660$PlotCN <- all2660cc20  $PlotCN
F20CarbSim2660 <- F20CarbSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2660) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4560 <- link(CarbSplit20, data=all4560cc20 )
simmeanCarb4560 <- data.frame(apply(simCarb4560,2,mean))
PICarb4560 <- t(data.frame(apply(simCarb4560,2,PI,prob=0.89)))
F20CarbSim4560 <- data.frame(cbind(simmeanCarb4560,PICarb4560))
F20CarbSim4560$PlotCN <- all4560cc20  $PlotCN
F20CarbSim4560 <- F20CarbSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4560) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8560 <- link(CarbSplit20, data=all8560cc20 )
simmeanCarb8560 <- data.frame(apply(simCarb8560,2,mean))
PICarb8560 <- t(data.frame(apply(simCarb8560,2,PI,prob=0.89)))
F20CarbSim8560 <- data.frame(cbind(simmeanCarb8560,PICarb8560))
F20CarbSim8560$PlotCN <- all8560cc20  $PlotCN
F20CarbSim8560 <- F20CarbSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8560) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2665 <- link(CarbSplit20, data=all2665cc20 )
simmeanCarb2665 <- data.frame(apply(simCarb2665,2,mean))
PICarb2665 <- t(data.frame(apply(simCarb2665,2,PI,prob=0.89)))
F20CarbSim2665 <- data.frame(cbind(simmeanCarb2665,PICarb2665))
F20CarbSim2665$PlotCN <- all2665cc20  $PlotCN
F20CarbSim2665 <- F20CarbSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2665) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4565 <- link(CarbSplit20, data=all4565cc20 )
simmeanCarb4565 <- data.frame(apply(simCarb4565,2,mean))
PICarb4565 <- t(data.frame(apply(simCarb4565,2,PI,prob=0.89)))
F20CarbSim4565 <- data.frame(cbind(simmeanCarb4565,PICarb4565))
F20CarbSim4565$PlotCN <- all4565cc20  $PlotCN
F20CarbSim4565 <- F20CarbSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4565) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8565 <- link(CarbSplit20, data=all8565cc20 )
simmeanCarb8565 <- data.frame(apply(simCarb8565,2,mean))
PICarb8565 <- t(data.frame(apply(simCarb8565,2,PI,prob=0.89)))
F20CarbSim8565 <- data.frame(cbind(simmeanCarb8565,PICarb8565))
F20CarbSim8565$PlotCN <- all8565cc20  $PlotCN
F20CarbSim8565 <- F20CarbSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8565) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2670 <- link(CarbSplit20, data=all2670cc20 )
simmeanCarb2670 <- data.frame(apply(simCarb2670,2,mean))
PICarb2670 <- t(data.frame(apply(simCarb2670,2,PI,prob=0.89)))
F20CarbSim2670 <- data.frame(cbind(simmeanCarb2670,PICarb2670))
F20CarbSim2670$PlotCN <- all2670cc20  $PlotCN
F20CarbSim2670 <- F20CarbSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2670) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4570 <- link(CarbSplit20, data=all4570cc20 )
simmeanCarb4570 <- data.frame(apply(simCarb4570,2,mean))
PICarb4570 <- t(data.frame(apply(simCarb4570,2,PI,prob=0.89)))
F20CarbSim4570 <- data.frame(cbind(simmeanCarb4570,PICarb4570))
F20CarbSim4570$PlotCN <- all4570cc20  $PlotCN
F20CarbSim4570 <- F20CarbSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4570) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8570 <- link(CarbSplit20, data=all8570cc20 )
simmeanCarb8570 <- data.frame(apply(simCarb8570,2,mean))
PICarb8570 <- t(data.frame(apply(simCarb8570,2,PI,prob=0.89)))
F20CarbSim8570 <- data.frame(cbind(simmeanCarb8570,PICarb8570))
F20CarbSim8570$PlotCN <- all8570cc20  $PlotCN
F20CarbSim8570 <- F20CarbSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8570) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2675 <- link(CarbSplit20, data=all2675cc20 )
simmeanCarb2675 <- data.frame(apply(simCarb2675,2,mean))
PICarb2675 <- t(data.frame(apply(simCarb2675,2,PI,prob=0.89)))
F20CarbSim2675 <- data.frame(cbind(simmeanCarb2675,PICarb2675))
F20CarbSim2675$PlotCN <- all2675cc20  $PlotCN
F20CarbSim2675 <- F20CarbSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2675) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4575 <- link(CarbSplit20, data=all4575cc20 )
simmeanCarb4575 <- data.frame(apply(simCarb4575,2,mean))
PICarb4575 <- t(data.frame(apply(simCarb4575,2,PI,prob=0.89)))
F20CarbSim4575 <- data.frame(cbind(simmeanCarb4575,PICarb4575))
F20CarbSim4575$PlotCN <- all4575cc20  $PlotCN
F20CarbSim4575 <- F20CarbSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4575) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8575 <- link(CarbSplit20, data=all8575cc20 )
simmeanCarb8575 <- data.frame(apply(simCarb8575,2,mean))
PICarb8575 <- t(data.frame(apply(simCarb8575,2,PI,prob=0.89)))
F20CarbSim8575 <- data.frame(cbind(simmeanCarb8575,PICarb8575))
F20CarbSim8575$PlotCN <- all8575cc20  $PlotCN
F20CarbSim8575 <- F20CarbSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8575) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2680 <- link(CarbSplit20, data=all2680cc20 )
simmeanCarb2680 <- data.frame(apply(simCarb2680,2,mean))
PICarb2680 <- t(data.frame(apply(simCarb2680,2,PI,prob=0.89)))
F20CarbSim2680 <- data.frame(cbind(simmeanCarb2680,PICarb2680))
F20CarbSim2680$PlotCN <- all2680cc20  $PlotCN
F20CarbSim2680 <- F20CarbSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim2680) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4580 <- link(CarbSplit20, data=all4580cc20 )
simmeanCarb4580 <- data.frame(apply(simCarb4580,2,mean))
PICarb4580 <- t(data.frame(apply(simCarb4580,2,PI,prob=0.89)))
F20CarbSim4580 <- data.frame(cbind(simmeanCarb4580,PICarb4580))
F20CarbSim4580$PlotCN <- all4580cc20  $PlotCN
F20CarbSim4580 <- F20CarbSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim4580) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8580 <- link(CarbSplit20, data=all8580cc20 )
simmeanCarb8580 <- data.frame(apply(simCarb8580,2,mean))
PICarb8580 <- t(data.frame(apply(simCarb8580,2,PI,prob=0.89)))
F20CarbSim8580 <- data.frame(cbind(simmeanCarb8580,PICarb8580))
F20CarbSim8580$PlotCN <- all8580cc20  $PlotCN
F20CarbSim8580 <- F20CarbSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F20CarbSim8580) <- c("PlotCN","CarbPerAcre","5CI","95CI")


#group data by RCP pathway
F20CarbSim26 <- F20CarbSim2625 %>% cbind(F20CarbSim2630[,2:4]) %>%
  cbind(F20CarbSim2635[,2:4]) %>% cbind(F20CarbSim2640[,2:4]) %>% cbind(F20CarbSim2645[,2:4]) %>%
  cbind(F20CarbSim2650[,2:4]) %>% cbind(F20CarbSim2655[,2:4]) %>% cbind(F20CarbSim2660[,2:4]) %>%
  cbind(F20CarbSim2665[,2:4]) %>% cbind(F20CarbSim2670[,2:4]) %>% cbind(F20CarbSim2675[,2:4]) %>%
  cbind(F20CarbSim2680[,2:4])
colnames(F20CarbSim26) <- CarbSimnames
write.csv(F20CarbSim26,file="F20CarbPred26.csv")

F20CarbSim45 <- F20CarbSim4525 %>% cbind(F20CarbSim4530[,2:4]) %>%
  cbind(F20CarbSim4535[,2:4]) %>% cbind(F20CarbSim4540[,2:4]) %>% cbind(F20CarbSim4545[,2:4]) %>%
  cbind(F20CarbSim4550[,2:4]) %>% cbind(F20CarbSim4555[,2:4]) %>% cbind(F20CarbSim4560[,2:4]) %>%
  cbind(F20CarbSim4565[,2:4]) %>% cbind(F20CarbSim4570[,2:4]) %>% cbind(F20CarbSim4575[,2:4]) %>%
  cbind(F20CarbSim4580[,2:4])
colnames(F20CarbSim45) <- CarbSimnames
write.csv(F20CarbSim45,file="F20CarbPred45.csv")

F20CarbSim85 <- F20CarbSim8525 %>% cbind(F20CarbSim8530[,2:4]) %>%
  cbind(F20CarbSim8535[,2:4]) %>% cbind(F20CarbSim8540[,2:4]) %>% cbind(F20CarbSim8545[,2:4]) %>%
  cbind(F20CarbSim8550[,2:4]) %>% cbind(F20CarbSim8555[,2:4]) %>% cbind(F20CarbSim8560[,2:4]) %>%
  cbind(F20CarbSim8565[,2:4]) %>% cbind(F20CarbSim8570[,2:4]) %>% cbind(F20CarbSim8575[,2:4]) %>%
  cbind(F20CarbSim8580[,2:4])
colnames(F20CarbSim85) <- CarbSimnames
write.csv(F20CarbSim85,file="F20CarbPred85.csv")


#fgroup 21
all2625cc21 <- all2625cc %>% filter(ForestGroup==21)
all4525cc21 <- all4525cc %>% filter(ForestGroup==21)
all8525cc21 <- all8525cc %>% filter(ForestGroup==21)
all2630cc21 <- all2630cc %>% filter(ForestGroup==21)
all4530cc21 <- all4530cc %>% filter(ForestGroup==21)
all8530cc21 <- all8530cc %>% filter(ForestGroup==21)
all2635cc21 <- all2635cc %>% filter(ForestGroup==21)
all4535cc21 <- all4535cc %>% filter(ForestGroup==21)
all8535cc21 <- all8535cc %>% filter(ForestGroup==21)
all2640cc21 <- all2640cc %>% filter(ForestGroup==21)
all4540cc21 <- all4540cc %>% filter(ForestGroup==21)
all8540cc21 <- all8540cc %>% filter(ForestGroup==21)
all2645cc21 <- all2645cc %>% filter(ForestGroup==21)
all4545cc21 <- all4545cc %>% filter(ForestGroup==21)
all8545cc21 <- all8545cc %>% filter(ForestGroup==21)
all2650cc21 <- all2650cc %>% filter(ForestGroup==21)
all4550cc21 <- all4550cc %>% filter(ForestGroup==21)
all8550cc21 <- all8550cc %>% filter(ForestGroup==21)
all2655cc21 <- all2655cc %>% filter(ForestGroup==21)
all4555cc21 <- all4555cc %>% filter(ForestGroup==21)
all8555cc21 <- all8555cc %>% filter(ForestGroup==21)
all2660cc21 <- all2660cc %>% filter(ForestGroup==21)
all4560cc21 <- all4560cc %>% filter(ForestGroup==21)
all8560cc21 <- all8560cc %>% filter(ForestGroup==21)
all2665cc21 <- all2665cc %>% filter(ForestGroup==21)
all4565cc21 <- all4565cc %>% filter(ForestGroup==21)
all8565cc21 <- all8565cc %>% filter(ForestGroup==21)
all2670cc21 <- all2670cc %>% filter(ForestGroup==21)
all4570cc21 <- all4570cc %>% filter(ForestGroup==21)
all8570cc21 <- all8570cc %>% filter(ForestGroup==21)
all2675cc21 <- all2675cc %>% filter(ForestGroup==21)
all4575cc21 <- all4575cc %>% filter(ForestGroup==21)
all8575cc21 <- all8575cc %>% filter(ForestGroup==21)
all2680cc21 <- all2680cc %>% filter(ForestGroup==21)
all4580cc21 <- all4580cc %>% filter(ForestGroup==21)
all8580cc21 <- all8580cc %>% filter(ForestGroup==21)

#turn soil order and forest type into factors
all2625cc21$Soil <- as.factor(all2625cc21$soil_order)
all2625cc21$Soil <- as.integer(all2625cc21$Soil)
all2625cc21$FType <- as.factor(all2625cc21$ForestType)
all2625cc21$FType <- as.integer(all2625cc21$FType)
all4525cc21$Soil <- as.factor(all4525cc21$soil_order)
all4525cc21$Soil <- as.integer(all4525cc21$Soil)
all4525cc21$FType <- as.factor(all4525cc21$ForestType)
all4525cc21$FType <- as.integer(all4525cc21$FType)
all8525cc21$Soil <- as.factor(all8525cc21$soil_order)
all8525cc21$Soil <- as.integer(all8525cc21$Soil)
all8525cc21$FType <- as.factor(all8525cc21$ForestType)
all8525cc21$FType <- as.integer(all8525cc21$FType)

all2630cc21$Soil <- as.factor(all2630cc21$soil_order)
all2630cc21$Soil <- as.integer(all2630cc21$Soil)
all2630cc21$FType <- as.factor(all2630cc21$ForestType)
all2630cc21$FType <- as.integer(all2630cc21$FType)
all4530cc21$Soil <- as.factor(all4530cc21$soil_order)
all4530cc21$Soil <- as.integer(all4530cc21$Soil)
all4530cc21$FType <- as.factor(all4530cc21$ForestType)
all4530cc21$FType <- as.integer(all4530cc21$FType)
all8530cc21$Soil <- as.factor(all8530cc21$soil_order)
all8530cc21$Soil <- as.integer(all8530cc21$Soil)
all8530cc21$FType <- as.factor(all8530cc21$ForestType)
all8530cc21$FType <- as.integer(all8530cc21$FType)

all2635cc21$Soil <- as.factor(all2635cc21$soil_order)
all2635cc21$Soil <- as.integer(all2635cc21$Soil)
all2635cc21$FType <- as.factor(all2635cc21$ForestType)
all2635cc21$FType <- as.integer(all2635cc21$FType)
all4535cc21$Soil <- as.factor(all4535cc21$soil_order)
all4535cc21$Soil <- as.integer(all4535cc21$Soil)
all4535cc21$FType <- as.factor(all4535cc21$ForestType)
all4535cc21$FType <- as.integer(all4535cc21$FType)
all8535cc21$Soil <- as.factor(all8535cc21$soil_order)
all8535cc21$Soil <- as.integer(all8535cc21$Soil)
all8535cc21$FType <- as.factor(all8535cc21$ForestType)
all8535cc21$FType <- as.integer(all8535cc21$FType)


all2640cc21$Soil <- as.factor(all2640cc21$soil_order)
all2640cc21$Soil <- as.integer(all2640cc21$Soil)
all2640cc21$FType <- as.factor(all2640cc21$ForestType)
all2640cc21$FType <- as.integer(all2640cc21$FType)
all4540cc21$Soil <- as.factor(all4540cc21$soil_order)
all4540cc21$Soil <- as.integer(all4540cc21$Soil)
all4540cc21$FType <- as.factor(all4540cc21$ForestType)
all4540cc21$FType <- as.integer(all4540cc21$FType)
all8540cc21$Soil <- as.factor(all8540cc21$soil_order)
all8540cc21$Soil <- as.integer(all8540cc21$Soil)
all8540cc21$FType <- as.factor(all8540cc21$ForestType)
all8540cc21$FType <- as.integer(all8540cc21$FType)

all2645cc21$Soil <- as.factor(all2645cc21$soil_order)
all2645cc21$Soil <- as.integer(all2645cc21$Soil)
all2645cc21$FType <- as.factor(all2645cc21$ForestType)
all2645cc21$FType <- as.integer(all2645cc21$FType)
all4545cc21$Soil <- as.factor(all4545cc21$soil_order)
all4545cc21$Soil <- as.integer(all4545cc21$Soil)
all4545cc21$FType <- as.factor(all4545cc21$ForestType)
all4545cc21$FType <- as.integer(all4545cc21$FType)
all8545cc21$Soil <- as.factor(all8545cc21$soil_order)
all8545cc21$Soil <- as.integer(all8545cc21$Soil)
all8545cc21$FType <- as.factor(all8545cc21$ForestType)
all8545cc21$FType <- as.integer(all8545cc21$FType)

all2650cc21$Soil <- as.factor(all2650cc21$soil_order)
all2650cc21$Soil <- as.integer(all2650cc21$Soil)
all2650cc21$FType <- as.factor(all2650cc21$ForestType)
all2650cc21$FType <- as.integer(all2650cc21$FType)
all4550cc21$Soil <- as.factor(all4550cc21$soil_order)
all4550cc21$Soil <- as.integer(all4550cc21$Soil)
all4550cc21$FType <- as.factor(all4550cc21$ForestType)
all4550cc21$FType <- as.integer(all4550cc21$FType)
all8550cc21$Soil <- as.factor(all8550cc21$soil_order)
all8550cc21$Soil <- as.integer(all8550cc21$Soil)
all8550cc21$FType <- as.factor(all8550cc21$ForestType)
all8550cc21$FType <- as.integer(all8550cc21$FType)

all2655cc21$Soil <- as.factor(all2655cc21$soil_order)
all2655cc21$Soil <- as.integer(all2655cc21$Soil)
all2655cc21$FType <- as.factor(all2655cc21$ForestType)
all2655cc21$FType <- as.integer(all2655cc21$FType)
all4555cc21$Soil <- as.factor(all4555cc21$soil_order)
all4555cc21$Soil <- as.integer(all4555cc21$Soil)
all4555cc21$FType <- as.factor(all4555cc21$ForestType)
all4555cc21$FType <- as.integer(all4555cc21$FType)
all8555cc21$Soil <- as.factor(all8555cc21$soil_order)
all8555cc21$Soil <- as.integer(all8555cc21$Soil)
all8555cc21$FType <- as.factor(all8555cc21$ForestType)
all8555cc21$FType <- as.integer(all8555cc21$FType)


all2660cc21$Soil <- as.factor(all2660cc21$soil_order)
all2660cc21$Soil <- as.integer(all2660cc21$Soil)
all2660cc21$FType <- as.factor(all2660cc21$ForestType)
all2660cc21$FType <- as.integer(all2660cc21$FType)
all4560cc21$Soil <- as.factor(all4560cc21$soil_order)
all4560cc21$Soil <- as.integer(all4560cc21$Soil)
all4560cc21$FType <- as.factor(all4560cc21$ForestType)
all4560cc21$FType <- as.integer(all4560cc21$FType)
all8560cc21$Soil <- as.factor(all8560cc21$soil_order)
all8560cc21$Soil <- as.integer(all8560cc21$Soil)
all8560cc21$FType <- as.factor(all8560cc21$ForestType)
all8560cc21$FType <- as.integer(all8560cc21$FType)

all2665cc21$Soil <- as.factor(all2665cc21$soil_order)
all2665cc21$Soil <- as.integer(all2665cc21$Soil)
all2665cc21$FType <- as.factor(all2665cc21$ForestType)
all2665cc21$FType <- as.integer(all2665cc21$FType)
all4565cc21$Soil <- as.factor(all4565cc21$soil_order)
all4565cc21$Soil <- as.integer(all4565cc21$Soil)
all4565cc21$FType <- as.factor(all4565cc21$ForestType)
all4565cc21$FType <- as.integer(all4565cc21$FType)
all8565cc21$Soil <- as.factor(all8565cc21$soil_order)
all8565cc21$Soil <- as.integer(all8565cc21$Soil)
all8565cc21$FType <- as.factor(all8565cc21$ForestType)
all8565cc21$FType <- as.integer(all8565cc21$FType)

all2670cc21$Soil <- as.factor(all2670cc21$soil_order)
all2670cc21$Soil <- as.integer(all2670cc21$Soil)
all2670cc21$FType <- as.factor(all2670cc21$ForestType)
all2670cc21$FType <- as.integer(all2670cc21$FType)
all4570cc21$Soil <- as.factor(all4570cc21$soil_order)
all4570cc21$Soil <- as.integer(all4570cc21$Soil)
all4570cc21$FType <- as.factor(all4570cc21$ForestType)
all4570cc21$FType <- as.integer(all4570cc21$FType)
all8570cc21$Soil <- as.factor(all8570cc21$soil_order)
all8570cc21$Soil <- as.integer(all8570cc21$Soil)
all8570cc21$FType <- as.factor(all8570cc21$ForestType)
all8570cc21$FType <- as.integer(all8570cc21$FType)


all2675cc21$Soil <- as.factor(all2675cc21$soil_order)
all2675cc21$Soil <- as.integer(all2675cc21$Soil)
all2675cc21$FType <- as.factor(all2675cc21$ForestType)
all2675cc21$FType <- as.integer(all2675cc21$FType)
all4575cc21$Soil <- as.factor(all4575cc21$soil_order)
all4575cc21$Soil <- as.integer(all4575cc21$Soil)
all4575cc21$FType <- as.factor(all4575cc21$ForestType)
all4575cc21$FType <- as.integer(all4575cc21$FType)
all8575cc21$Soil <- as.factor(all8575cc21$soil_order)
all8575cc21$Soil <- as.integer(all8575cc21$Soil)
all8575cc21$FType <- as.factor(all8575cc21$ForestType)
all8575cc21$FType <- as.integer(all8575cc21$FType)

all2680cc21$Soil <- as.factor(all2680cc21$soil_order)
all2680cc21$Soil <- as.integer(all2680cc21$Soil)
all2680cc21$FType <- as.factor(all2680cc21$ForestType)
all2680cc21$FType <- as.integer(all2680cc21$FType)
all4580cc21$Soil <- as.factor(all4580cc21$soil_order)
all4580cc21$Soil <- as.integer(all4580cc21$Soil)
all4580cc21$FType <- as.factor(all4580cc21$ForestType)
all4580cc21$FType <- as.integer(all4580cc21$FType)
all8580cc21$Soil <- as.factor(all8580cc21$soil_order)
all8580cc21$Soil <- as.integer(all8580cc21$Soil)
all8580cc21$FType <- as.factor(all8580cc21$ForestType)
all8580cc21$FType <- as.integer(all8580cc21$FType)


#center and scale from model data
all2625cc21 $MAT<- as.data.frame(scale(all2625cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4525cc21 $MAT<- as.data.frame(scale(all4525cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8525cc21 $MAT<- as.data.frame(scale(all8525cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all2630cc21 $MAT <- as.data.frame(scale(all2630cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4530cc21 $MAT <- as.data.frame(scale(all4530cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8530cc21 $MAT <- as.data.frame(scale(all8530cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all2635cc21 $MAT<- as.data.frame(scale(all2635cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4535cc21 $MAT<- as.data.frame(scale(all4535cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8535cc21 $MAT<- as.data.frame(scale(all8535cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all2640cc21 $MAT <- as.data.frame(scale(all2640cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4540cc21 $MAT <- as.data.frame(scale(all4540cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8540cc21 $MAT <- as.data.frame(scale(all8540cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all2645cc21 $MAT<- as.data.frame(scale(all2645cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4545cc21 $MAT<- as.data.frame(scale(all4545cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8545cc21 $MAT<- as.data.frame(scale(all8545cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all2650cc21 $MAT <- as.data.frame(scale(all2650cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4550cc21 $MAT <- as.data.frame(scale(all4550cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8550cc21 $MAT <- as.data.frame(scale(all8550cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all2655cc21 $MAT<- as.data.frame(scale(all2655cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4555cc21 $MAT<- as.data.frame(scale(all4555cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8555cc21 $MAT<- as.data.frame(scale(all8555cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all2660cc21 $MAT <- as.data.frame(scale(all2660cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4560cc21 $MAT <- as.data.frame(scale(all4560cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8560cc21 $MAT <- as.data.frame(scale(all8560cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all2665cc21 $MAT<- as.data.frame(scale(all2665cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4565cc21 $MAT<- as.data.frame(scale(all4565cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8565cc21 $MAT<- as.data.frame(scale(all8565cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all2670cc21 $MAT <- as.data.frame(scale(all2670cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4570cc21 $MAT <- as.data.frame(scale(all4570cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8570cc21 $MAT <- as.data.frame(scale(all8570cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all2675cc21 $MAT<- as.data.frame(scale(all2675cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4575cc21 $MAT<- as.data.frame(scale(all4575cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8575cc21 $MAT<- as.data.frame(scale(all8575cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all2680cc21 $MAT <- as.data.frame(scale(all2680cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all4580cc21 $MAT <- as.data.frame(scale(all4580cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
all8580cc21 $MAT <- as.data.frame(scale(all8580cc21 $MAT, center=centerMAT21,scale=scaleMAT21),col.names="MAT")[, 1]
#PPT
all2625cc21 $PPT<- as.data.frame(scale(all2625cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4525cc21 $PPT<- as.data.frame(scale(all4525cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8525cc21 $PPT<- as.data.frame(scale(all8525cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all2630cc21 $PPT <- as.data.frame(scale(all2630cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4530cc21 $PPT <- as.data.frame(scale(all4530cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8530cc21 $PPT <- as.data.frame(scale(all8530cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all2635cc21 $PPT<- as.data.frame(scale(all2635cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4535cc21 $PPT<- as.data.frame(scale(all4535cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8535cc21 $PPT<- as.data.frame(scale(all8535cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all2640cc21 $PPT <- as.data.frame(scale(all2640cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4540cc21 $PPT <- as.data.frame(scale(all4540cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8540cc21 $PPT <- as.data.frame(scale(all8540cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all2645cc21 $PPT<- as.data.frame(scale(all2645cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4545cc21 $PPT<- as.data.frame(scale(all4545cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8545cc21 $PPT<- as.data.frame(scale(all8545cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all2650cc21 $PPT <- as.data.frame(scale(all2650cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4550cc21 $PPT <- as.data.frame(scale(all4550cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8550cc21 $PPT <- as.data.frame(scale(all8550cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all2655cc21 $PPT<- as.data.frame(scale(all2655cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4555cc21 $PPT<- as.data.frame(scale(all4555cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8555cc21 $PPT<- as.data.frame(scale(all8555cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all2660cc21 $PPT <- as.data.frame(scale(all2660cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4560cc21 $PPT <- as.data.frame(scale(all4560cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8560cc21 $PPT <- as.data.frame(scale(all8560cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all2665cc21 $PPT<- as.data.frame(scale(all2665cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4565cc21 $PPT<- as.data.frame(scale(all4565cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8565cc21 $PPT<- as.data.frame(scale(all8565cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all2670cc21 $PPT <- as.data.frame(scale(all2670cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4570cc21 $PPT <- as.data.frame(scale(all4570cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8570cc21 $PPT <- as.data.frame(scale(all8570cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all2675cc21 $PPT<- as.data.frame(scale(all2675cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4575cc21 $PPT<- as.data.frame(scale(all4575cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8575cc21 $PPT<- as.data.frame(scale(all8575cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all2680cc21 $PPT <- as.data.frame(scale(all2680cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all4580cc21 $PPT <- as.data.frame(scale(all4580cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
all8580cc21 $PPT <- as.data.frame(scale(all8580cc21 $PPT, center=centerPPT21,scale=scalePPT21),col.names="PPT")[, 1]
#RHUM
all2625cc21 $RHUM<- as.data.frame(scale(all2625cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4525cc21 $RHUM<- as.data.frame(scale(all4525cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8525cc21 $RHUM<- as.data.frame(scale(all8525cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all2630cc21 $RHUM <- as.data.frame(scale(all2630cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4530cc21 $RHUM <- as.data.frame(scale(all4530cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8530cc21 $RHUM <- as.data.frame(scale(all8530cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all2635cc21 $RHUM<- as.data.frame(scale(all2635cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4535cc21 $RHUM<- as.data.frame(scale(all4535cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8535cc21 $RHUM<- as.data.frame(scale(all8535cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all2640cc21 $RHUM <- as.data.frame(scale(all2640cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4540cc21 $RHUM <- as.data.frame(scale(all4540cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8540cc21 $RHUM <- as.data.frame(scale(all8540cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all2645cc21 $RHUM<- as.data.frame(scale(all2645cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4545cc21 $RHUM<- as.data.frame(scale(all4545cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8545cc21 $RHUM<- as.data.frame(scale(all8545cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all2650cc21 $RHUM <- as.data.frame(scale(all2650cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4550cc21 $RHUM <- as.data.frame(scale(all4550cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8550cc21 $RHUM <- as.data.frame(scale(all8550cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all2655cc21 $RHUM<- as.data.frame(scale(all2655cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4555cc21 $RHUM<- as.data.frame(scale(all4555cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8555cc21 $RHUM<- as.data.frame(scale(all8555cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all2660cc21 $RHUM <- as.data.frame(scale(all2660cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4560cc21 $RHUM <- as.data.frame(scale(all4560cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8560cc21 $RHUM <- as.data.frame(scale(all8560cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all2665cc21 $RHUM<- as.data.frame(scale(all2665cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4565cc21 $RHUM<- as.data.frame(scale(all4565cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8565cc21 $RHUM<- as.data.frame(scale(all8565cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all2670cc21 $RHUM <- as.data.frame(scale(all2670cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4570cc21 $RHUM <- as.data.frame(scale(all4570cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8570cc21 $RHUM <- as.data.frame(scale(all8570cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all2675cc21 $RHUM<- as.data.frame(scale(all2675cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4575cc21 $RHUM<- as.data.frame(scale(all4575cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8575cc21 $RHUM<- as.data.frame(scale(all8575cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all2680cc21 $RHUM <- as.data.frame(scale(all2680cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all4580cc21 $RHUM <- as.data.frame(scale(all4580cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
all8580cc21 $RHUM <- as.data.frame(scale(all8580cc21 $RHUM, center=centerRHUM21,scale=scaleRHUM21),col.names="RHUM")[, 1]
#RAD
all2625cc21 $RAD<- as.data.frame(scale(all2625cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4525cc21 $RAD<- as.data.frame(scale(all4525cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8525cc21 $RAD<- as.data.frame(scale(all8525cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all2630cc21 $RAD <- as.data.frame(scale(all2630cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4530cc21 $RAD <- as.data.frame(scale(all4530cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8530cc21 $RAD <- as.data.frame(scale(all8530cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all2635cc21 $RAD<- as.data.frame(scale(all2635cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4535cc21 $RAD<- as.data.frame(scale(all4535cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8535cc21 $RAD<- as.data.frame(scale(all8535cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all2640cc21 $RAD <- as.data.frame(scale(all2640cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4540cc21 $RAD <- as.data.frame(scale(all4540cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8540cc21 $RAD <- as.data.frame(scale(all8540cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all2645cc21 $RAD<- as.data.frame(scale(all2645cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4545cc21 $RAD<- as.data.frame(scale(all4545cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8545cc21 $RAD<- as.data.frame(scale(all8545cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all2650cc21 $RAD <- as.data.frame(scale(all2650cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4550cc21 $RAD <- as.data.frame(scale(all4550cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8550cc21 $RAD <- as.data.frame(scale(all8550cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all2655cc21 $RAD<- as.data.frame(scale(all2655cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4555cc21 $RAD<- as.data.frame(scale(all4555cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8555cc21 $RAD<- as.data.frame(scale(all8555cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all2660cc21 $RAD <- as.data.frame(scale(all2660cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4560cc21 $RAD <- as.data.frame(scale(all4560cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8560cc21 $RAD <- as.data.frame(scale(all8560cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all2665cc21 $RAD<- as.data.frame(scale(all2665cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4565cc21 $RAD<- as.data.frame(scale(all4565cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8565cc21 $RAD<- as.data.frame(scale(all8565cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all2670cc21 $RAD <- as.data.frame(scale(all2670cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4570cc21 $RAD <- as.data.frame(scale(all4570cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8570cc21 $RAD <- as.data.frame(scale(all8570cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all2675cc21 $RAD<- as.data.frame(scale(all2675cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4575cc21 $RAD<- as.data.frame(scale(all4575cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8575cc21 $RAD<- as.data.frame(scale(all8575cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all2680cc21 $RAD <- as.data.frame(scale(all2680cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all4580cc21 $RAD <- as.data.frame(scale(all4580cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
all8580cc21 $RAD <- as.data.frame(scale(all8580cc21 $RAD, center=centerRAD21,scale=scaleRAD21),col.names="RAD")[, 1]
#Elevation
all2625cc21 $Elevation<- as.data.frame(scale(all2625cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4525cc21 $Elevation<- as.data.frame(scale(all4525cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8525cc21 $Elevation<- as.data.frame(scale(all8525cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all2630cc21 $Elevation <- as.data.frame(scale(all2630cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4530cc21 $Elevation <- as.data.frame(scale(all4530cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8530cc21 $Elevation <- as.data.frame(scale(all8530cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all2635cc21 $Elevation<- as.data.frame(scale(all2635cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4535cc21 $Elevation<- as.data.frame(scale(all4535cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8535cc21 $Elevation<- as.data.frame(scale(all8535cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all2640cc21 $Elevation <- as.data.frame(scale(all2640cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4540cc21 $Elevation <- as.data.frame(scale(all4540cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8540cc21 $Elevation <- as.data.frame(scale(all8540cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all2645cc21 $Elevation<- as.data.frame(scale(all2645cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4545cc21 $Elevation<- as.data.frame(scale(all4545cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8545cc21 $Elevation<- as.data.frame(scale(all8545cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all2650cc21 $Elevation <- as.data.frame(scale(all2650cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4550cc21 $Elevation <- as.data.frame(scale(all4550cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8550cc21 $Elevation <- as.data.frame(scale(all8550cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all2655cc21 $Elevation<- as.data.frame(scale(all2655cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4555cc21 $Elevation<- as.data.frame(scale(all4555cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8555cc21 $Elevation<- as.data.frame(scale(all8555cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all2660cc21 $Elevation <- as.data.frame(scale(all2660cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4560cc21 $Elevation <- as.data.frame(scale(all4560cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8560cc21 $Elevation <- as.data.frame(scale(all8560cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all2665cc21 $Elevation<- as.data.frame(scale(all2665cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4565cc21 $Elevation<- as.data.frame(scale(all4565cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8565cc21 $Elevation<- as.data.frame(scale(all8565cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all2670cc21 $Elevation <- as.data.frame(scale(all2670cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4570cc21 $Elevation <- as.data.frame(scale(all4570cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8570cc21 $Elevation <- as.data.frame(scale(all8570cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all2675cc21 $Elevation<- as.data.frame(scale(all2675cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4575cc21 $Elevation<- as.data.frame(scale(all4575cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8575cc21 $Elevation<- as.data.frame(scale(all8575cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all2680cc21 $Elevation <- as.data.frame(scale(all2680cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all4580cc21 $Elevation <- as.data.frame(scale(all4580cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
all8580cc21 $Elevation <- as.data.frame(scale(all8580cc21 $Elevation, center=centerElevation21,scale=scaleElevation21),col.names="Elevation")[, 1]
#Basal Area
all2625cc21$BA <- (all2625cc21$BA)+(all2625cc21$AvgBADeadRate)*6
all4525cc21$BA <- (all4525cc21$BA)+(all4525cc21$AvgBADeadRate)*6
all8525cc21$BA <- (all8525cc21$BA)+(all8525cc21$AvgBADeadRate)*6
all2630cc21$BA <- (all2630cc21$BA)+(all2630cc21$AvgBADeadRate)*11
all4530cc21$BA <- (all4530cc21$BA)+(all4530cc21$AvgBADeadRate)*11
all8530cc21$BA <- (all8530cc21$BA)+(all8530cc21$AvgBADeadRate)*11
all2635cc21$BA <- (all2635cc21$BA)+(all2635cc21$AvgBADeadRate)*16
all4535cc21$BA <-  (all4535cc21$BA)+(all4535cc21$AvgBADeadRate)*16
all8535cc21$BA <- (all8535cc21$BA)+(all8535cc21$AvgBADeadRate)*16
all2640cc21$BA <- (all2640cc21$BA)+(all2640cc21$AvgBADeadRate)*21
all4540cc21$BA <- (all4540cc21$BA)+(all4540cc21$AvgBADeadRate)*21
all8540cc21$BA <-  (all8540cc21$BA)+(all8540cc21$AvgBADeadRate)*21
all2645cc21$BA <- (all2645cc21$BA)+(all2645cc21$AvgBADeadRate)*26
all4545cc21$BA <- (all4545cc21$BA)+(all4545cc21$AvgBADeadRate)*26
all8545cc21$BA <- (all8545cc21$BA)+(all8545cc21$AvgBADeadRate)*26
all2650cc21$BA <- (all2650cc21$BA)+(all2650cc21$AvgBADeadRate)*31
all4550cc21$BA <- (all4550cc21$BA)+(all4550cc21$AvgBADeadRate)*31
all8550cc21$BA <- (all8550cc21$BA)+(all8550cc21$AvgBADeadRate)*31
all2655cc21$BA <-(all2655cc21$BA)+(all2655cc21$AvgBADeadRate)*36
all4555cc21$BA <- (all4555cc21$BA)+(all4555cc21$AvgBADeadRate)*36
all8555cc21$BA <- (all8555cc21$BA)+(all8555cc21$AvgBADeadRate)*36
all2660cc21$BA <-(all2660cc21$BA)+(all2660cc21$AvgBADeadRate)*41
all4560cc21$BA <- (all4560cc21$BA)+(all4560cc21$AvgBADeadRate)*41
all8560cc21$BA <- (all8560cc21$BA)+(all8560cc21$AvgBADeadRate)*41
all2665cc21$BA <- (all2665cc21$BA)+(all2665cc21$AvgBADeadRate)*46
all4565cc21$BA <- (all4565cc21$BA)+(all4565cc21$AvgBADeadRate)*46
all8565cc21$BA <- (all8565cc21$BA)+(all8565cc21$AvgBADeadRate)*46
all2670cc21$BA <- (all2670cc21$BA)+(all2670cc21$AvgBADeadRate)*51
all4570cc21$BA <- (all4570cc21$BA)+(all4570cc21$AvgBADeadRate)*51
all8570cc21$BA <- (all8570cc21$BA)+(all8570cc21$AvgBADeadRate)*51
all2675cc21$BA <- (all2675cc21$BA)+(all2675cc21$AvgBADeadRate)*56
all4575cc21$BA <- (all4575cc21$BA)+(all4575cc21$AvgBADeadRate)*56
all8575cc21$BA <- (all8575cc21$BA)+(all8575cc21$AvgBADeadRate)*56
all2680cc21$BA <- (all2680cc21$BA)+(all2680cc21$AvgBADeadRate)*61
all4580cc21$BA <- (all4580cc21$BA)+(all4580cc21$AvgBADeadRate)*61
all8580cc21$BA <- (all8580cc21$BA)+(all8580cc21$AvgBADeadRate)*61

#remove negatives
all2625cc21$BA[all2625cc21$BA<=0] <- 0.01
all4525cc21$BA[all4525cc21$BA<=0] <- 0.01
all8525cc21$BA[all8525cc21$BA<=0] <- 0.01
all2630cc21$BA[all2630cc21$BA<=0] <- 0.01
all4530cc21$BA[all4530cc21$BA<=0] <- 0.01
all8530cc21$BA[all8530cc21$BA<=0] <- 0.01
all2635cc21$BA[all2635cc21$BA<=0] <- 0.01
all4535cc21$BA[all4535cc21$BA<=0] <- 0.01
all8535cc21$BA[all8535cc21$BA<=0] <- 0.01
all2640cc21$BA[all2640cc21$BA<=0] <- 0.01
all4540cc21$BA[all4540cc21$BA<=0] <- 0.01
all8540cc21$BA[all8540cc21$BA<=0] <- 0.01
all2645cc21$BA[all2645cc21$BA<=0] <- 0.01
all4545cc21$BA[all4545cc21$BA<=0] <- 0.01
all8545cc21$BA[all8545cc21$BA<=0] <- 0.01
all2650cc21$BA[all2650cc21$BA<=0] <- 0.01
all4550cc21$BA[all4550cc21$BA<=0] <- 0.01
all8550cc21$BA[all8550cc21$BA<=0] <- 0.01
all2655cc21$BA[all2655cc21$BA<=0] <- 0.01
all4555cc21$BA[all4555cc21$BA<=0] <- 0.01
all8555cc21$BA[all8555cc21$BA<=0] <- 0.01
all2660cc21$BA[all2660cc21$BA<=0] <- 0.01
all4560cc21$BA[all4560cc21$BA<=0] <- 0.01
all8560cc21$BA[all8560cc21$BA<=0] <- 0.01
all2665cc21$BA[all2665cc21$BA<=0] <- 0.01
all4565cc21$BA[all4565cc21$BA<=0] <- 0.01
all8565cc21$BA[all8565cc21$BA<=0] <- 0.01
all2670cc21$BA[all2670cc21$BA<=0] <- 0.01
all4570cc21$BA[all4570cc21$BA<=0] <- 0.01
all8570cc21$BA[all8570cc21$BA<=0] <- 0.01
all2675cc21$BA[all2675cc21$BA<=0] <- 0.01
all4575cc21$BA[all4575cc21$BA<=0] <- 0.01
all8575cc21$BA[all8575cc21$BA<=0] <- 0.01
all2680cc21$BA[all2680cc21$BA<=0] <- 0.01
all4580cc21$BA[all4580cc21$BA<=0] <- 0.01
all8580cc21$BA[all8580cc21$BA<=0] <- 0.01

#max basal area of dead trees = basal area of all trees. prevents mortality over 100%, not possible.
all2625cc21$BA <- ifelse(all2625cc21$BA > all2625cc21$BAAll, all2625cc21$BAAll, all2625cc21$BA)
all4525cc21$BA <- ifelse(all4525cc21$BA > all4525cc21$BAAll, all4525cc21$BAAll, all4525cc21$BA)
all8525cc21$BA <- ifelse(all8525cc21$BA > all8525cc21$BAAll, all8525cc21$BAAll, all8525cc21$BA)
all2630cc21$BA <- ifelse(all2630cc21$BA > all2630cc21$BAAll, all2630cc21$BAAll, all2630cc21$BA)
all4530cc21$BA <- ifelse(all4530cc21$BA > all4530cc21$BAAll, all4530cc21$BAAll, all4530cc21$BA)
all8530cc21$BA <- ifelse(all8530cc21$BA > all8530cc21$BAAll, all8530cc21$BAAll, all8530cc21$BA)
all2635cc21$BA <- ifelse(all2635cc21$BA > all2635cc21$BAAll, all2635cc21$BAAll, all2635cc21$BA)
all4535cc21$BA <- ifelse(all4535cc21$BA > all4535cc21$BAAll, all4535cc21$BAAll, all4535cc21$BA)
all8535cc21$BA <- ifelse(all8535cc21$BA > all8535cc21$BAAll, all8535cc21$BAAll, all8535cc21$BA)
all2640cc21$BA <- ifelse(all2640cc21$BA > all2640cc21$BAAll, all2640cc21$BAAll, all2640cc21$BA)
all4540cc21$BA <- ifelse(all4540cc21$BA > all4540cc21$BAAll, all4540cc21$BAAll, all4540cc21$BA)
all8540cc21$BA <- ifelse(all8540cc21$BA > all8540cc21$BAAll, all8540cc21$BAAll, all8540cc21$BA)
all2645cc21$BA <- ifelse(all2645cc21$BA > all2645cc21$BAAll, all2645cc21$BAAll, all2645cc21$BA)
all4545cc21$BA <- ifelse(all4545cc21$BA > all4545cc21$BAAll, all4545cc21$BAAll, all4545cc21$BA)
all8545cc21$BA <- ifelse(all8545cc21$BA > all8545cc21$BAAll, all8545cc21$BAAll, all8545cc21$BA)
all2650cc21$BA <- ifelse(all2650cc21$BA > all2650cc21$BAAll, all2650cc21$BAAll, all2650cc21$BA)
all4550cc21$BA <- ifelse(all4550cc21$BA > all4550cc21$BAAll, all4550cc21$BAAll, all4550cc21$BA)
all8550cc21$BA <- ifelse(all8550cc21$BA > all8550cc21$BAAll, all8550cc21$BAAll, all8550cc21$BA)
all2655cc21$BA <- ifelse(all2655cc21$BA > all2655cc21$BAAll, all2655cc21$BAAll, all2655cc21$BA)
all4555cc21$BA <- ifelse(all4555cc21$BA > all4555cc21$BAAll, all4555cc21$BAAll, all4555cc21$BA)
all8555cc21$BA <- ifelse(all8555cc21$BA > all8555cc21$BAAll, all8555cc21$BAAll, all8555cc21$BA)
all2660cc21$BA <- ifelse(all2660cc21$BA > all2660cc21$BAAll, all2660cc21$BAAll, all2660cc21$BA)
all4560cc21$BA <- ifelse(all4560cc21$BA > all4560cc21$BAAll, all4560cc21$BAAll, all4560cc21$BA)
all8560cc21$BA <- ifelse(all8560cc21$BA > all8560cc21$BAAll, all8560cc21$BAAll, all8560cc21$BA)
all2665cc21$BA <- ifelse(all2665cc21$BA > all2665cc21$BAAll, all2665cc21$BAAll, all2665cc21$BA)
all4565cc21$BA <- ifelse(all4565cc21$BA > all4565cc21$BAAll, all4565cc21$BAAll, all4565cc21$BA)
all8565cc21$BA <- ifelse(all8565cc21$BA > all8565cc21$BAAll, all8565cc21$BAAll, all8565cc21$BA)
all2670cc21$BA <- ifelse(all2670cc21$BA > all2670cc21$BAAll, all2670cc21$BAAll, all2670cc21$BA)
all4570cc21$BA <- ifelse(all4570cc21$BA > all4570cc21$BAAll, all4570cc21$BAAll, all4570cc21$BA)
all8570cc21$BA <- ifelse(all8570cc21$BA > all8570cc21$BAAll, all8570cc21$BAAll, all8570cc21$BA)
all2675cc21$BA <- ifelse(all2675cc21$BA > all2675cc21$BAAll, all2675cc21$BAAll, all2675cc21$BA)
all4575cc21$BA <- ifelse(all4575cc21$BA > all4575cc21$BAAll, all4575cc21$BAAll, all4575cc21$BA)
all8575cc21$BA <- ifelse(all8575cc21$BA > all8575cc21$BAAll, all8575cc21$BAAll, all8575cc21$BA)
all2680cc21$BA <- ifelse(all2680cc21$BA > all2680cc21$BAAll, all2680cc21$BAAll, all2680cc21$BA)
all4580cc21$BA <- ifelse(all4580cc21$BA > all4580cc21$BAAll, all4580cc21$BAAll, all4580cc21$BA)
all8580cc21$BA <- ifelse(all8580cc21$BA > all8580cc21$BAAll, all8580cc21$BAAll, all8580cc21$BA)

all2625cc21 $BA<- as.data.frame(scale(all2625cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4525cc21 $BA<- as.data.frame(scale(all4525cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8525cc21 $BA<- as.data.frame(scale(all8525cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all2630cc21 $BA <- as.data.frame(scale(all2630cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4530cc21 $BA <- as.data.frame(scale(all4530cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8530cc21 $BA <- as.data.frame(scale(all8530cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all2635cc21 $BA<- as.data.frame(scale(all2635cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4535cc21 $BA<- as.data.frame(scale(all4535cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8535cc21 $BA<- as.data.frame(scale(all8535cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all2640cc21 $BA <- as.data.frame(scale(all2640cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4540cc21 $BA <- as.data.frame(scale(all4540cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8540cc21 $BA <- as.data.frame(scale(all8540cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all2645cc21 $BA<- as.data.frame(scale(all2645cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4545cc21 $BA<- as.data.frame(scale(all4545cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8545cc21 $BA<- as.data.frame(scale(all8545cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all2650cc21 $BA <- as.data.frame(scale(all2650cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4550cc21 $BA <- as.data.frame(scale(all4550cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8550cc21 $BA <- as.data.frame(scale(all8550cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all2655cc21 $BA<- as.data.frame(scale(all2655cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4555cc21 $BA<- as.data.frame(scale(all4555cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8555cc21 $BA<- as.data.frame(scale(all8555cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all2660cc21 $BA <- as.data.frame(scale(all2660cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4560cc21 $BA <- as.data.frame(scale(all4560cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8560cc21 $BA <- as.data.frame(scale(all8560cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all2665cc21 $BA<- as.data.frame(scale(all2665cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4565cc21 $BA<- as.data.frame(scale(all4565cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8565cc21 $BA<- as.data.frame(scale(all8565cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all2670cc21 $BA <- as.data.frame(scale(all2670cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4570cc21 $BA <- as.data.frame(scale(all4570cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8570cc21 $BA <- as.data.frame(scale(all8570cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all2675cc21 $BA<- as.data.frame(scale(all2675cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4575cc21 $BA<- as.data.frame(scale(all4575cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8575cc21 $BA<- as.data.frame(scale(all8575cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all2680cc21 $BA <- as.data.frame(scale(all2680cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all4580cc21 $BA <- as.data.frame(scale(all4580cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]
all8580cc21 $BA <- as.data.frame(scale(all8580cc21 $BA, center=centerBA21,scale=scaleBA21),col.names="BA")[, 1]

#time weight f21
all2625cc21$time_weight <- 2.149483
all4525cc21$time_weight <- 2.149483
all8525cc21$time_weight <- 2.149483
all2630cc21$time_weight <- 2.149483
all4530cc21$time_weight <- 2.149483
all8530cc21$time_weight <- 2.149483
all2635cc21$time_weight <- 2.149483
all4535cc21$time_weight <- 2.149483
all8535cc21$time_weight <- 2.149483
all2640cc21$time_weight <- 2.149483
all4540cc21$time_weight <- 2.149483
all8540cc21$time_weight <- 2.149483
all2645cc21$time_weight <- 2.149483
all4545cc21$time_weight <- 2.149483
all8545cc21$time_weight <- 2.149483
all2650cc21$time_weight <- 2.149483
all4550cc21$time_weight <- 2.149483
all8550cc21$time_weight <- 2.149483
all2655cc21$time_weight <- 2.149483
all4555cc21$time_weight <- 2.149483
all8555cc21$time_weight <- 2.149483
all2660cc21$time_weight <- 2.149483
all4560cc21$time_weight <- 2.149483
all8560cc21$time_weight <- 2.149483
all2665cc21$time_weight <- 2.149483
all4565cc21$time_weight <- 2.149483
all8565cc21$time_weight <- 2.149483
all2670cc21$time_weight <- 2.149483
all4570cc21$time_weight <- 2.149483
all8570cc21$time_weight <- 2.149483
all2675cc21$time_weight <- 2.149483
all4575cc21$time_weight <- 2.149483
all8575cc21$time_weight <- 2.149483
all2680cc21$time_weight <- 2.149483
all4580cc21$time_weight <- 2.149483
all8580cc21$time_weight <- 2.149483

#run simulations
simCarb2625 <- link(CarbSplit21, data=all2625cc21 )
simmeanCarb2625 <- data.frame(apply(simCarb2625,2,mean))
PICarb2625 <- t(data.frame(apply(simCarb2625,2,PI,prob=0.89)))
F21CarbSim2625 <- data.frame(cbind(simmeanCarb2625,PICarb2625))
F21CarbSim2625$PlotCN <- all2625cc21  $PlotCN
F21CarbSim2625 <- F21CarbSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2625) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4525 <- link(CarbSplit21, data=all4525cc21 )
simmeanCarb4525 <- data.frame(apply(simCarb4525,2,mean))
PICarb4525 <- t(data.frame(apply(simCarb4525,2,PI,prob=0.89)))
F21CarbSim4525 <- data.frame(cbind(simmeanCarb4525,PICarb4525))
F21CarbSim4525$PlotCN <- all4525cc21  $PlotCN
F21CarbSim4525 <- F21CarbSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4525) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8525 <- link(CarbSplit21, data=all8525cc21 )
simmeanCarb8525 <- data.frame(apply(simCarb8525,2,mean))
PICarb8525 <- t(data.frame(apply(simCarb8525,2,PI,prob=0.89)))
F21CarbSim8525 <- data.frame(cbind(simmeanCarb8525,PICarb8525))
F21CarbSim8525$PlotCN <- all8525cc21  $PlotCN
F21CarbSim8525 <- F21CarbSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8525) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2630 <- link(CarbSplit21, data=all2630cc21 )
simmeanCarb2630 <- data.frame(apply(simCarb2630,2,mean))
PICarb2630 <- t(data.frame(apply(simCarb2630,2,PI,prob=0.89)))
F21CarbSim2630 <- data.frame(cbind(simmeanCarb2630,PICarb2630))
F21CarbSim2630$PlotCN <- all2630cc21  $PlotCN
F21CarbSim2630 <- F21CarbSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2630) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4530 <- link(CarbSplit21, data=all4530cc21 )
simmeanCarb4530 <- data.frame(apply(simCarb4530,2,mean))
PICarb4530 <- t(data.frame(apply(simCarb4530,2,PI,prob=0.89)))
F21CarbSim4530 <- data.frame(cbind(simmeanCarb4530,PICarb4530))
F21CarbSim4530$PlotCN <- all4530cc21  $PlotCN
F21CarbSim4530 <- F21CarbSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4530) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8530 <- link(CarbSplit21, data=all8530cc21 )
simmeanCarb8530 <- data.frame(apply(simCarb8530,2,mean))
PICarb8530 <- t(data.frame(apply(simCarb8530,2,PI,prob=0.89)))
F21CarbSim8530 <- data.frame(cbind(simmeanCarb8530,PICarb8530))
F21CarbSim8530$PlotCN <- all8530cc21  $PlotCN
F21CarbSim8530 <- F21CarbSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8530) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2635 <- link(CarbSplit21, data=all2635cc21 )
simmeanCarb2635 <- data.frame(apply(simCarb2635,2,mean))
PICarb2635 <- t(data.frame(apply(simCarb2635,2,PI,prob=0.89)))
F21CarbSim2635 <- data.frame(cbind(simmeanCarb2635,PICarb2635))
F21CarbSim2635$PlotCN <- all2635cc21  $PlotCN
F21CarbSim2635 <- F21CarbSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2635) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4535 <- link(CarbSplit21, data=all4535cc21 )
simmeanCarb4535 <- data.frame(apply(simCarb4535,2,mean))
PICarb4535 <- t(data.frame(apply(simCarb4535,2,PI,prob=0.89)))
F21CarbSim4535 <- data.frame(cbind(simmeanCarb4535,PICarb4535))
F21CarbSim4535$PlotCN <- all4535cc21  $PlotCN
F21CarbSim4535 <- F21CarbSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4535) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8535 <- link(CarbSplit21, data=all8535cc21 )
simmeanCarb8535 <- data.frame(apply(simCarb8535,2,mean))
PICarb8535 <- t(data.frame(apply(simCarb8535,2,PI,prob=0.89)))
F21CarbSim8535 <- data.frame(cbind(simmeanCarb8535,PICarb8535))
F21CarbSim8535$PlotCN <- all8535cc21  $PlotCN
F21CarbSim8535 <- F21CarbSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8535) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2640 <- link(CarbSplit21, data=all2640cc21 )
simmeanCarb2640 <- data.frame(apply(simCarb2640,2,mean))
PICarb2640 <- t(data.frame(apply(simCarb2640,2,PI,prob=0.89)))
F21CarbSim2640 <- data.frame(cbind(simmeanCarb2640,PICarb2640))
F21CarbSim2640$PlotCN <- all2640cc21  $PlotCN
F21CarbSim2640 <- F21CarbSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2640) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4540 <- link(CarbSplit21, data=all4540cc21 )
simmeanCarb4540 <- data.frame(apply(simCarb4540,2,mean))
PICarb4540 <- t(data.frame(apply(simCarb4540,2,PI,prob=0.89)))
F21CarbSim4540 <- data.frame(cbind(simmeanCarb4540,PICarb4540))
F21CarbSim4540$PlotCN <- all4540cc21  $PlotCN
F21CarbSim4540 <- F21CarbSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4540) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8540 <- link(CarbSplit21, data=all8540cc21 )
simmeanCarb8540 <- data.frame(apply(simCarb8540,2,mean))
PICarb8540 <- t(data.frame(apply(simCarb8540,2,PI,prob=0.89)))
F21CarbSim8540 <- data.frame(cbind(simmeanCarb8540,PICarb8540))
F21CarbSim8540$PlotCN <- all8540cc21  $PlotCN
F21CarbSim8540 <- F21CarbSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8540) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2645 <- link(CarbSplit21, data=all2645cc21 )
simmeanCarb2645 <- data.frame(apply(simCarb2645,2,mean))
PICarb2645 <- t(data.frame(apply(simCarb2645,2,PI,prob=0.89)))
F21CarbSim2645 <- data.frame(cbind(simmeanCarb2645,PICarb2645))
F21CarbSim2645$PlotCN <- all2645cc21  $PlotCN
F21CarbSim2645 <- F21CarbSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2645) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4545 <- link(CarbSplit21, data=all4545cc21 )
simmeanCarb4545 <- data.frame(apply(simCarb4545,2,mean))
PICarb4545 <- t(data.frame(apply(simCarb4545,2,PI,prob=0.89)))
F21CarbSim4545 <- data.frame(cbind(simmeanCarb4545,PICarb4545))
F21CarbSim4545$PlotCN <- all4545cc21  $PlotCN
F21CarbSim4545 <- F21CarbSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4545) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8545 <- link(CarbSplit21, data=all8545cc21 )
simmeanCarb8545 <- data.frame(apply(simCarb8545,2,mean))
PICarb8545 <- t(data.frame(apply(simCarb8545,2,PI,prob=0.89)))
F21CarbSim8545 <- data.frame(cbind(simmeanCarb8545,PICarb8545))
F21CarbSim8545$PlotCN <- all8545cc21  $PlotCN
F21CarbSim8545 <- F21CarbSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8545) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2650 <- link(CarbSplit21, data=all2650cc21 )
simmeanCarb2650 <- data.frame(apply(simCarb2650,2,mean))
PICarb2650 <- t(data.frame(apply(simCarb2650,2,PI,prob=0.89)))
F21CarbSim2650 <- data.frame(cbind(simmeanCarb2650,PICarb2650))
F21CarbSim2650$PlotCN <- all2650cc21  $PlotCN
F21CarbSim2650 <- F21CarbSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2650) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4550 <- link(CarbSplit21, data=all4550cc21 )
simmeanCarb4550 <- data.frame(apply(simCarb4550,2,mean))
PICarb4550 <- t(data.frame(apply(simCarb4550,2,PI,prob=0.89)))
F21CarbSim4550 <- data.frame(cbind(simmeanCarb4550,PICarb4550))
F21CarbSim4550$PlotCN <- all4550cc21  $PlotCN
F21CarbSim4550 <- F21CarbSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4550) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8550 <- link(CarbSplit21, data=all8550cc21 )
simmeanCarb8550 <- data.frame(apply(simCarb8550,2,mean))
PICarb8550 <- t(data.frame(apply(simCarb8550,2,PI,prob=0.89)))
F21CarbSim8550 <- data.frame(cbind(simmeanCarb8550,PICarb8550))
F21CarbSim8550$PlotCN <- all8550cc21  $PlotCN
F21CarbSim8550 <- F21CarbSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8550) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2655 <- link(CarbSplit21, data=all2655cc21 )
simmeanCarb2655 <- data.frame(apply(simCarb2655,2,mean))
PICarb2655 <- t(data.frame(apply(simCarb2655,2,PI,prob=0.89)))
F21CarbSim2655 <- data.frame(cbind(simmeanCarb2655,PICarb2655))
F21CarbSim2655$PlotCN <- all2655cc21  $PlotCN
F21CarbSim2655 <- F21CarbSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2655) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4555 <- link(CarbSplit21, data=all4555cc21 )
simmeanCarb4555 <- data.frame(apply(simCarb4555,2,mean))
PICarb4555 <- t(data.frame(apply(simCarb4555,2,PI,prob=0.89)))
F21CarbSim4555 <- data.frame(cbind(simmeanCarb4555,PICarb4555))
F21CarbSim4555$PlotCN <- all4555cc21  $PlotCN
F21CarbSim4555 <- F21CarbSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4555) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8555 <- link(CarbSplit21, data=all8555cc21 )
simmeanCarb8555 <- data.frame(apply(simCarb8555,2,mean))
PICarb8555 <- t(data.frame(apply(simCarb8555,2,PI,prob=0.89)))
F21CarbSim8555 <- data.frame(cbind(simmeanCarb8555,PICarb8555))
F21CarbSim8555$PlotCN <- all8555cc21  $PlotCN
F21CarbSim8555 <- F21CarbSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8555) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2660 <- link(CarbSplit21, data=all2660cc21 )
simmeanCarb2660 <- data.frame(apply(simCarb2660,2,mean))
PICarb2660 <- t(data.frame(apply(simCarb2660,2,PI,prob=0.89)))
F21CarbSim2660 <- data.frame(cbind(simmeanCarb2660,PICarb2660))
F21CarbSim2660$PlotCN <- all2660cc21  $PlotCN
F21CarbSim2660 <- F21CarbSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2660) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4560 <- link(CarbSplit21, data=all4560cc21 )
simmeanCarb4560 <- data.frame(apply(simCarb4560,2,mean))
PICarb4560 <- t(data.frame(apply(simCarb4560,2,PI,prob=0.89)))
F21CarbSim4560 <- data.frame(cbind(simmeanCarb4560,PICarb4560))
F21CarbSim4560$PlotCN <- all4560cc21  $PlotCN
F21CarbSim4560 <- F21CarbSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4560) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8560 <- link(CarbSplit21, data=all8560cc21 )
simmeanCarb8560 <- data.frame(apply(simCarb8560,2,mean))
PICarb8560 <- t(data.frame(apply(simCarb8560,2,PI,prob=0.89)))
F21CarbSim8560 <- data.frame(cbind(simmeanCarb8560,PICarb8560))
F21CarbSim8560$PlotCN <- all8560cc21  $PlotCN
F21CarbSim8560 <- F21CarbSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8560) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2665 <- link(CarbSplit21, data=all2665cc21 )
simmeanCarb2665 <- data.frame(apply(simCarb2665,2,mean))
PICarb2665 <- t(data.frame(apply(simCarb2665,2,PI,prob=0.89)))
F21CarbSim2665 <- data.frame(cbind(simmeanCarb2665,PICarb2665))
F21CarbSim2665$PlotCN <- all2665cc21  $PlotCN
F21CarbSim2665 <- F21CarbSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2665) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4565 <- link(CarbSplit21, data=all4565cc21 )
simmeanCarb4565 <- data.frame(apply(simCarb4565,2,mean))
PICarb4565 <- t(data.frame(apply(simCarb4565,2,PI,prob=0.89)))
F21CarbSim4565 <- data.frame(cbind(simmeanCarb4565,PICarb4565))
F21CarbSim4565$PlotCN <- all4565cc21  $PlotCN
F21CarbSim4565 <- F21CarbSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4565) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8565 <- link(CarbSplit21, data=all8565cc21 )
simmeanCarb8565 <- data.frame(apply(simCarb8565,2,mean))
PICarb8565 <- t(data.frame(apply(simCarb8565,2,PI,prob=0.89)))
F21CarbSim8565 <- data.frame(cbind(simmeanCarb8565,PICarb8565))
F21CarbSim8565$PlotCN <- all8565cc21  $PlotCN
F21CarbSim8565 <- F21CarbSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8565) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2670 <- link(CarbSplit21, data=all2670cc21 )
simmeanCarb2670 <- data.frame(apply(simCarb2670,2,mean))
PICarb2670 <- t(data.frame(apply(simCarb2670,2,PI,prob=0.89)))
F21CarbSim2670 <- data.frame(cbind(simmeanCarb2670,PICarb2670))
F21CarbSim2670$PlotCN <- all2670cc21  $PlotCN
F21CarbSim2670 <- F21CarbSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2670) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4570 <- link(CarbSplit21, data=all4570cc21 )
simmeanCarb4570 <- data.frame(apply(simCarb4570,2,mean))
PICarb4570 <- t(data.frame(apply(simCarb4570,2,PI,prob=0.89)))
F21CarbSim4570 <- data.frame(cbind(simmeanCarb4570,PICarb4570))
F21CarbSim4570$PlotCN <- all4570cc21  $PlotCN
F21CarbSim4570 <- F21CarbSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4570) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8570 <- link(CarbSplit21, data=all8570cc21 )
simmeanCarb8570 <- data.frame(apply(simCarb8570,2,mean))
PICarb8570 <- t(data.frame(apply(simCarb8570,2,PI,prob=0.89)))
F21CarbSim8570 <- data.frame(cbind(simmeanCarb8570,PICarb8570))
F21CarbSim8570$PlotCN <- all8570cc21  $PlotCN
F21CarbSim8570 <- F21CarbSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8570) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2675 <- link(CarbSplit21, data=all2675cc21 )
simmeanCarb2675 <- data.frame(apply(simCarb2675,2,mean))
PICarb2675 <- t(data.frame(apply(simCarb2675,2,PI,prob=0.89)))
F21CarbSim2675 <- data.frame(cbind(simmeanCarb2675,PICarb2675))
F21CarbSim2675$PlotCN <- all2675cc21  $PlotCN
F21CarbSim2675 <- F21CarbSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2675) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4575 <- link(CarbSplit21, data=all4575cc21 )
simmeanCarb4575 <- data.frame(apply(simCarb4575,2,mean))
PICarb4575 <- t(data.frame(apply(simCarb4575,2,PI,prob=0.89)))
F21CarbSim4575 <- data.frame(cbind(simmeanCarb4575,PICarb4575))
F21CarbSim4575$PlotCN <- all4575cc21  $PlotCN
F21CarbSim4575 <- F21CarbSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4575) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8575 <- link(CarbSplit21, data=all8575cc21 )
simmeanCarb8575 <- data.frame(apply(simCarb8575,2,mean))
PICarb8575 <- t(data.frame(apply(simCarb8575,2,PI,prob=0.89)))
F21CarbSim8575 <- data.frame(cbind(simmeanCarb8575,PICarb8575))
F21CarbSim8575$PlotCN <- all8575cc21  $PlotCN
F21CarbSim8575 <- F21CarbSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8575) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2680 <- link(CarbSplit21, data=all2680cc21 )
simmeanCarb2680 <- data.frame(apply(simCarb2680,2,mean))
PICarb2680 <- t(data.frame(apply(simCarb2680,2,PI,prob=0.89)))
F21CarbSim2680 <- data.frame(cbind(simmeanCarb2680,PICarb2680))
F21CarbSim2680$PlotCN <- all2680cc21  $PlotCN
F21CarbSim2680 <- F21CarbSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim2680) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4580 <- link(CarbSplit21, data=all4580cc21 )
simmeanCarb4580 <- data.frame(apply(simCarb4580,2,mean))
PICarb4580 <- t(data.frame(apply(simCarb4580,2,PI,prob=0.89)))
F21CarbSim4580 <- data.frame(cbind(simmeanCarb4580,PICarb4580))
F21CarbSim4580$PlotCN <- all4580cc21  $PlotCN
F21CarbSim4580 <- F21CarbSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim4580) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8580 <- link(CarbSplit21, data=all8580cc21 )
simmeanCarb8580 <- data.frame(apply(simCarb8580,2,mean))
PICarb8580 <- t(data.frame(apply(simCarb8580,2,PI,prob=0.89)))
F21CarbSim8580 <- data.frame(cbind(simmeanCarb8580,PICarb8580))
F21CarbSim8580$PlotCN <- all8580cc21  $PlotCN
F21CarbSim8580 <- F21CarbSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F21CarbSim8580) <- c("PlotCN","CarbPerAcre","5CI","95CI")

#create vector of new colnames
CarbSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Carb", "5CI", "95CI"), x)))
CarbSimnames<- append(CarbSimnames,"PlotCN",after=0)

#group data by RCP pathway
F21CarbSim26 <- F21CarbSim2625 %>% cbind(F21CarbSim2630[,2:4]) %>%
  cbind(F21CarbSim2635[,2:4]) %>% cbind(F21CarbSim2640[,2:4]) %>% cbind(F21CarbSim2645[,2:4]) %>%
  cbind(F21CarbSim2650[,2:4]) %>% cbind(F21CarbSim2655[,2:4]) %>% cbind(F21CarbSim2660[,2:4]) %>%
  cbind(F21CarbSim2665[,2:4]) %>% cbind(F21CarbSim2670[,2:4]) %>% cbind(F21CarbSim2675[,2:4]) %>%
  cbind(F21CarbSim2680[,2:4])
colnames(F21CarbSim26) <- CarbSimnames
write.csv(F21CarbSim26,file="F21CarbPred26.csv")

F21CarbSim45 <- F21CarbSim4525 %>% cbind(F21CarbSim4530[,2:4]) %>%
  cbind(F21CarbSim4535[,2:4]) %>% cbind(F21CarbSim4540[,2:4]) %>% cbind(F21CarbSim4545[,2:4]) %>%
  cbind(F21CarbSim4550[,2:4]) %>% cbind(F21CarbSim4555[,2:4]) %>% cbind(F21CarbSim4560[,2:4]) %>%
  cbind(F21CarbSim4565[,2:4]) %>% cbind(F21CarbSim4570[,2:4]) %>% cbind(F21CarbSim4575[,2:4]) %>%
  cbind(F21CarbSim4580[,2:4])
colnames(F21CarbSim45) <- CarbSimnames
write.csv(F21CarbSim45,file="F21CarbPred45.csv")

F21CarbSim85 <- F21CarbSim8525 %>% cbind(F21CarbSim8530[,2:4]) %>%
  cbind(F21CarbSim8535[,2:4]) %>% cbind(F21CarbSim8540[,2:4]) %>% cbind(F21CarbSim8545[,2:4]) %>%
  cbind(F21CarbSim8550[,2:4]) %>% cbind(F21CarbSim8555[,2:4]) %>% cbind(F21CarbSim8560[,2:4]) %>%
  cbind(F21CarbSim8565[,2:4]) %>% cbind(F21CarbSim8570[,2:4]) %>% cbind(F21CarbSim8575[,2:4]) %>%
  cbind(F21CarbSim8580[,2:4])
colnames(F21CarbSim85) <- CarbSimnames
write.csv(F21CarbSim85,file="F21CarbPred85.csv")


#fgroup 23
all2625cc23 <- all2625cc %>% filter(ForestGroup==23)
all4525cc23 <- all4525cc %>% filter(ForestGroup==23)
all8525cc23 <- all8525cc %>% filter(ForestGroup==23)
all2630cc23 <- all2630cc %>% filter(ForestGroup==23)
all4530cc23 <- all4530cc %>% filter(ForestGroup==23)
all8530cc23 <- all8530cc %>% filter(ForestGroup==23)
all2635cc23 <- all2635cc %>% filter(ForestGroup==23)
all4535cc23 <- all4535cc %>% filter(ForestGroup==23)
all8535cc23 <- all8535cc %>% filter(ForestGroup==23)
all2640cc23 <- all2640cc %>% filter(ForestGroup==23)
all4540cc23 <- all4540cc %>% filter(ForestGroup==23)
all8540cc23 <- all8540cc %>% filter(ForestGroup==23)
all2645cc23 <- all2645cc %>% filter(ForestGroup==23)
all4545cc23 <- all4545cc %>% filter(ForestGroup==23)
all8545cc23 <- all8545cc %>% filter(ForestGroup==23)
all2650cc23 <- all2650cc %>% filter(ForestGroup==23)
all4550cc23 <- all4550cc %>% filter(ForestGroup==23)
all8550cc23 <- all8550cc %>% filter(ForestGroup==23)
all2655cc23 <- all2655cc %>% filter(ForestGroup==23)
all4555cc23 <- all4555cc %>% filter(ForestGroup==23)
all8555cc23 <- all8555cc %>% filter(ForestGroup==23)
all2660cc23 <- all2660cc %>% filter(ForestGroup==23)
all4560cc23 <- all4560cc %>% filter(ForestGroup==23)
all8560cc23 <- all8560cc %>% filter(ForestGroup==23)
all2665cc23 <- all2665cc %>% filter(ForestGroup==23)
all4565cc23 <- all4565cc %>% filter(ForestGroup==23)
all8565cc23 <- all8565cc %>% filter(ForestGroup==23)
all2670cc23 <- all2670cc %>% filter(ForestGroup==23)
all4570cc23 <- all4570cc %>% filter(ForestGroup==23)
all8570cc23 <- all8570cc %>% filter(ForestGroup==23)
all2675cc23 <- all2675cc %>% filter(ForestGroup==23)
all4575cc23 <- all4575cc %>% filter(ForestGroup==23)
all8575cc23 <- all8575cc %>% filter(ForestGroup==23)
all2680cc23 <- all2680cc %>% filter(ForestGroup==23)
all4580cc23 <- all4580cc %>% filter(ForestGroup==23)
all8580cc23 <- all8580cc %>% filter(ForestGroup==23)

#factor/integer Ftype again to match model
#turn soil order and forest type into factors
all2625cc23$Soil <- as.factor(all2625cc23$soil_order)
all2625cc23$Soil <- as.integer(all2625cc23$Soil)
all2625cc23$FType <- as.factor(all2625cc23$ForestType)
all2625cc23$FType <- as.integer(all2625cc23$FType)
all4525cc23$Soil <- as.factor(all4525cc23$soil_order)
all4525cc23$Soil <- as.integer(all4525cc23$Soil)
all4525cc23$FType <- as.factor(all4525cc23$ForestType)
all4525cc23$FType <- as.integer(all4525cc23$FType)
all8525cc23$Soil <- as.factor(all8525cc23$soil_order)
all8525cc23$Soil <- as.integer(all8525cc23$Soil)
all8525cc23$FType <- as.factor(all8525cc23$ForestType)
all8525cc23$FType <- as.integer(all8525cc23$FType)

all2630cc23$Soil <- as.factor(all2630cc23$soil_order)
all2630cc23$Soil <- as.integer(all2630cc23$Soil)
all2630cc23$FType <- as.factor(all2630cc23$ForestType)
all2630cc23$FType <- as.integer(all2630cc23$FType)
all4530cc23$Soil <- as.factor(all4530cc23$soil_order)
all4530cc23$Soil <- as.integer(all4530cc23$Soil)
all4530cc23$FType <- as.factor(all4530cc23$ForestType)
all4530cc23$FType <- as.integer(all4530cc23$FType)
all8530cc23$Soil <- as.factor(all8530cc23$soil_order)
all8530cc23$Soil <- as.integer(all8530cc23$Soil)
all8530cc23$FType <- as.factor(all8530cc23$ForestType)
all8530cc23$FType <- as.integer(all8530cc23$FType)

all2635cc23$Soil <- as.factor(all2635cc23$soil_order)
all2635cc23$Soil <- as.integer(all2635cc23$Soil)
all2635cc23$FType <- as.factor(all2635cc23$ForestType)
all2635cc23$FType <- as.integer(all2635cc23$FType)
all4535cc23$Soil <- as.factor(all4535cc23$soil_order)
all4535cc23$Soil <- as.integer(all4535cc23$Soil)
all4535cc23$FType <- as.factor(all4535cc23$ForestType)
all4535cc23$FType <- as.integer(all4535cc23$FType)
all8535cc23$Soil <- as.factor(all8535cc23$soil_order)
all8535cc23$Soil <- as.integer(all8535cc23$Soil)
all8535cc23$FType <- as.factor(all8535cc23$ForestType)
all8535cc23$FType <- as.integer(all8535cc23$FType)


all2640cc23$Soil <- as.factor(all2640cc23$soil_order)
all2640cc23$Soil <- as.integer(all2640cc23$Soil)
all2640cc23$FType <- as.factor(all2640cc23$ForestType)
all2640cc23$FType <- as.integer(all2640cc23$FType)
all4540cc23$Soil <- as.factor(all4540cc23$soil_order)
all4540cc23$Soil <- as.integer(all4540cc23$Soil)
all4540cc23$FType <- as.factor(all4540cc23$ForestType)
all4540cc23$FType <- as.integer(all4540cc23$FType)
all8540cc23$Soil <- as.factor(all8540cc23$soil_order)
all8540cc23$Soil <- as.integer(all8540cc23$Soil)
all8540cc23$FType <- as.factor(all8540cc23$ForestType)
all8540cc23$FType <- as.integer(all8540cc23$FType)

all2645cc23$Soil <- as.factor(all2645cc23$soil_order)
all2645cc23$Soil <- as.integer(all2645cc23$Soil)
all2645cc23$FType <- as.factor(all2645cc23$ForestType)
all2645cc23$FType <- as.integer(all2645cc23$FType)
all4545cc23$Soil <- as.factor(all4545cc23$soil_order)
all4545cc23$Soil <- as.integer(all4545cc23$Soil)
all4545cc23$FType <- as.factor(all4545cc23$ForestType)
all4545cc23$FType <- as.integer(all4545cc23$FType)
all8545cc23$Soil <- as.factor(all8545cc23$soil_order)
all8545cc23$Soil <- as.integer(all8545cc23$Soil)
all8545cc23$FType <- as.factor(all8545cc23$ForestType)
all8545cc23$FType <- as.integer(all8545cc23$FType)

all2650cc23$Soil <- as.factor(all2650cc23$soil_order)
all2650cc23$Soil <- as.integer(all2650cc23$Soil)
all2650cc23$FType <- as.factor(all2650cc23$ForestType)
all2650cc23$FType <- as.integer(all2650cc23$FType)
all4550cc23$Soil <- as.factor(all4550cc23$soil_order)
all4550cc23$Soil <- as.integer(all4550cc23$Soil)
all4550cc23$FType <- as.factor(all4550cc23$ForestType)
all4550cc23$FType <- as.integer(all4550cc23$FType)
all8550cc23$Soil <- as.factor(all8550cc23$soil_order)
all8550cc23$Soil <- as.integer(all8550cc23$Soil)
all8550cc23$FType <- as.factor(all8550cc23$ForestType)
all8550cc23$FType <- as.integer(all8550cc23$FType)

all2655cc23$Soil <- as.factor(all2655cc23$soil_order)
all2655cc23$Soil <- as.integer(all2655cc23$Soil)
all2655cc23$FType <- as.factor(all2655cc23$ForestType)
all2655cc23$FType <- as.integer(all2655cc23$FType)
all4555cc23$Soil <- as.factor(all4555cc23$soil_order)
all4555cc23$Soil <- as.integer(all4555cc23$Soil)
all4555cc23$FType <- as.factor(all4555cc23$ForestType)
all4555cc23$FType <- as.integer(all4555cc23$FType)
all8555cc23$Soil <- as.factor(all8555cc23$soil_order)
all8555cc23$Soil <- as.integer(all8555cc23$Soil)
all8555cc23$FType <- as.factor(all8555cc23$ForestType)
all8555cc23$FType <- as.integer(all8555cc23$FType)


all2660cc23$Soil <- as.factor(all2660cc23$soil_order)
all2660cc23$Soil <- as.integer(all2660cc23$Soil)
all2660cc23$FType <- as.factor(all2660cc23$ForestType)
all2660cc23$FType <- as.integer(all2660cc23$FType)
all4560cc23$Soil <- as.factor(all4560cc23$soil_order)
all4560cc23$Soil <- as.integer(all4560cc23$Soil)
all4560cc23$FType <- as.factor(all4560cc23$ForestType)
all4560cc23$FType <- as.integer(all4560cc23$FType)
all8560cc23$Soil <- as.factor(all8560cc23$soil_order)
all8560cc23$Soil <- as.integer(all8560cc23$Soil)
all8560cc23$FType <- as.factor(all8560cc23$ForestType)
all8560cc23$FType <- as.integer(all8560cc23$FType)

all2665cc23$Soil <- as.factor(all2665cc23$soil_order)
all2665cc23$Soil <- as.integer(all2665cc23$Soil)
all2665cc23$FType <- as.factor(all2665cc23$ForestType)
all2665cc23$FType <- as.integer(all2665cc23$FType)
all4565cc23$Soil <- as.factor(all4565cc23$soil_order)
all4565cc23$Soil <- as.integer(all4565cc23$Soil)
all4565cc23$FType <- as.factor(all4565cc23$ForestType)
all4565cc23$FType <- as.integer(all4565cc23$FType)
all8565cc23$Soil <- as.factor(all8565cc23$soil_order)
all8565cc23$Soil <- as.integer(all8565cc23$Soil)
all8565cc23$FType <- as.factor(all8565cc23$ForestType)
all8565cc23$FType <- as.integer(all8565cc23$FType)

all2670cc23$Soil <- as.factor(all2670cc23$soil_order)
all2670cc23$Soil <- as.integer(all2670cc23$Soil)
all2670cc23$FType <- as.factor(all2670cc23$ForestType)
all2670cc23$FType <- as.integer(all2670cc23$FType)
all4570cc23$Soil <- as.factor(all4570cc23$soil_order)
all4570cc23$Soil <- as.integer(all4570cc23$Soil)
all4570cc23$FType <- as.factor(all4570cc23$ForestType)
all4570cc23$FType <- as.integer(all4570cc23$FType)
all8570cc23$Soil <- as.factor(all8570cc23$soil_order)
all8570cc23$Soil <- as.integer(all8570cc23$Soil)
all8570cc23$FType <- as.factor(all8570cc23$ForestType)
all8570cc23$FType <- as.integer(all8570cc23$FType)


all2675cc23$Soil <- as.factor(all2675cc23$soil_order)
all2675cc23$Soil <- as.integer(all2675cc23$Soil)
all2675cc23$FType <- as.factor(all2675cc23$ForestType)
all2675cc23$FType <- as.integer(all2675cc23$FType)
all4575cc23$Soil <- as.factor(all4575cc23$soil_order)
all4575cc23$Soil <- as.integer(all4575cc23$Soil)
all4575cc23$FType <- as.factor(all4575cc23$ForestType)
all4575cc23$FType <- as.integer(all4575cc23$FType)
all8575cc23$Soil <- as.factor(all8575cc23$soil_order)
all8575cc23$Soil <- as.integer(all8575cc23$Soil)
all8575cc23$FType <- as.factor(all8575cc23$ForestType)
all8575cc23$FType <- as.integer(all8575cc23$FType)

all2680cc23$Soil <- as.factor(all2680cc23$soil_order)
all2680cc23$Soil <- as.integer(all2680cc23$Soil)
all2680cc23$FType <- as.factor(all2680cc23$ForestType)
all2680cc23$FType <- as.integer(all2680cc23$FType)
all4580cc23$Soil <- as.factor(all4580cc23$soil_order)
all4580cc23$Soil <- as.integer(all4580cc23$Soil)
all4580cc23$FType <- as.factor(all4580cc23$ForestType)
all4580cc23$FType <- as.integer(all4580cc23$FType)
all8580cc23$Soil <- as.factor(all8580cc23$soil_order)
all8580cc23$Soil <- as.integer(all8580cc23$Soil)
all8580cc23$FType <- as.factor(all8580cc23$ForestType)
all8580cc23$FType <- as.integer(all8580cc23$FType)


#center and scale from model data
all2625cc23 $MAT<- as.data.frame(scale(all2625cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4525cc23 $MAT<- as.data.frame(scale(all4525cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8525cc23 $MAT<- as.data.frame(scale(all8525cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all2630cc23 $MAT <- as.data.frame(scale(all2630cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4530cc23 $MAT <- as.data.frame(scale(all4530cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8530cc23 $MAT <- as.data.frame(scale(all8530cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all2635cc23 $MAT<- as.data.frame(scale(all2635cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4535cc23 $MAT<- as.data.frame(scale(all4535cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8535cc23 $MAT<- as.data.frame(scale(all8535cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all2640cc23 $MAT <- as.data.frame(scale(all2640cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4540cc23 $MAT <- as.data.frame(scale(all4540cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8540cc23 $MAT <- as.data.frame(scale(all8540cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all2645cc23 $MAT<- as.data.frame(scale(all2645cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4545cc23 $MAT<- as.data.frame(scale(all4545cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8545cc23 $MAT<- as.data.frame(scale(all8545cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all2650cc23 $MAT <- as.data.frame(scale(all2650cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4550cc23 $MAT <- as.data.frame(scale(all4550cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8550cc23 $MAT <- as.data.frame(scale(all8550cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all2655cc23 $MAT<- as.data.frame(scale(all2655cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4555cc23 $MAT<- as.data.frame(scale(all4555cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8555cc23 $MAT<- as.data.frame(scale(all8555cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all2660cc23 $MAT <- as.data.frame(scale(all2660cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4560cc23 $MAT <- as.data.frame(scale(all4560cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8560cc23 $MAT <- as.data.frame(scale(all8560cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all2665cc23 $MAT<- as.data.frame(scale(all2665cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4565cc23 $MAT<- as.data.frame(scale(all4565cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8565cc23 $MAT<- as.data.frame(scale(all8565cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all2670cc23 $MAT <- as.data.frame(scale(all2670cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4570cc23 $MAT <- as.data.frame(scale(all4570cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8570cc23 $MAT <- as.data.frame(scale(all8570cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all2675cc23 $MAT<- as.data.frame(scale(all2675cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4575cc23 $MAT<- as.data.frame(scale(all4575cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8575cc23 $MAT<- as.data.frame(scale(all8575cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all2680cc23 $MAT <- as.data.frame(scale(all2680cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all4580cc23 $MAT <- as.data.frame(scale(all4580cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
all8580cc23 $MAT <- as.data.frame(scale(all8580cc23 $MAT, center=centerMAT23,scale=scaleMAT23),col.names="MAT")[, 1]
#PPT
all2625cc23 $PPT<- as.data.frame(scale(all2625cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4525cc23 $PPT<- as.data.frame(scale(all4525cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8525cc23 $PPT<- as.data.frame(scale(all8525cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all2630cc23 $PPT <- as.data.frame(scale(all2630cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4530cc23 $PPT <- as.data.frame(scale(all4530cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8530cc23 $PPT <- as.data.frame(scale(all8530cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all2635cc23 $PPT<- as.data.frame(scale(all2635cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4535cc23 $PPT<- as.data.frame(scale(all4535cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8535cc23 $PPT<- as.data.frame(scale(all8535cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all2640cc23 $PPT <- as.data.frame(scale(all2640cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4540cc23 $PPT <- as.data.frame(scale(all4540cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8540cc23 $PPT <- as.data.frame(scale(all8540cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all2645cc23 $PPT<- as.data.frame(scale(all2645cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4545cc23 $PPT<- as.data.frame(scale(all4545cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8545cc23 $PPT<- as.data.frame(scale(all8545cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all2650cc23 $PPT <- as.data.frame(scale(all2650cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4550cc23 $PPT <- as.data.frame(scale(all4550cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8550cc23 $PPT <- as.data.frame(scale(all8550cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all2655cc23 $PPT<- as.data.frame(scale(all2655cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4555cc23 $PPT<- as.data.frame(scale(all4555cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8555cc23 $PPT<- as.data.frame(scale(all8555cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all2660cc23 $PPT <- as.data.frame(scale(all2660cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4560cc23 $PPT <- as.data.frame(scale(all4560cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8560cc23 $PPT <- as.data.frame(scale(all8560cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all2665cc23 $PPT<- as.data.frame(scale(all2665cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4565cc23 $PPT<- as.data.frame(scale(all4565cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8565cc23 $PPT<- as.data.frame(scale(all8565cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all2670cc23 $PPT <- as.data.frame(scale(all2670cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4570cc23 $PPT <- as.data.frame(scale(all4570cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8570cc23 $PPT <- as.data.frame(scale(all8570cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all2675cc23 $PPT<- as.data.frame(scale(all2675cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4575cc23 $PPT<- as.data.frame(scale(all4575cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8575cc23 $PPT<- as.data.frame(scale(all8575cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all2680cc23 $PPT <- as.data.frame(scale(all2680cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all4580cc23 $PPT <- as.data.frame(scale(all4580cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
all8580cc23 $PPT <- as.data.frame(scale(all8580cc23 $PPT, center=centerPPT23,scale=scalePPT23),col.names="PPT")[, 1]
#RHUM
all2625cc23 $RHUM<- as.data.frame(scale(all2625cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4525cc23 $RHUM<- as.data.frame(scale(all4525cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8525cc23 $RHUM<- as.data.frame(scale(all8525cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all2630cc23 $RHUM <- as.data.frame(scale(all2630cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4530cc23 $RHUM <- as.data.frame(scale(all4530cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8530cc23 $RHUM <- as.data.frame(scale(all8530cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all2635cc23 $RHUM<- as.data.frame(scale(all2635cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4535cc23 $RHUM<- as.data.frame(scale(all4535cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8535cc23 $RHUM<- as.data.frame(scale(all8535cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all2640cc23 $RHUM <- as.data.frame(scale(all2640cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4540cc23 $RHUM <- as.data.frame(scale(all4540cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8540cc23 $RHUM <- as.data.frame(scale(all8540cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all2645cc23 $RHUM<- as.data.frame(scale(all2645cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4545cc23 $RHUM<- as.data.frame(scale(all4545cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8545cc23 $RHUM<- as.data.frame(scale(all8545cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all2650cc23 $RHUM <- as.data.frame(scale(all2650cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4550cc23 $RHUM <- as.data.frame(scale(all4550cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8550cc23 $RHUM <- as.data.frame(scale(all8550cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all2655cc23 $RHUM<- as.data.frame(scale(all2655cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4555cc23 $RHUM<- as.data.frame(scale(all4555cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8555cc23 $RHUM<- as.data.frame(scale(all8555cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all2660cc23 $RHUM <- as.data.frame(scale(all2660cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4560cc23 $RHUM <- as.data.frame(scale(all4560cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8560cc23 $RHUM <- as.data.frame(scale(all8560cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all2665cc23 $RHUM<- as.data.frame(scale(all2665cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4565cc23 $RHUM<- as.data.frame(scale(all4565cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8565cc23 $RHUM<- as.data.frame(scale(all8565cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all2670cc23 $RHUM <- as.data.frame(scale(all2670cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4570cc23 $RHUM <- as.data.frame(scale(all4570cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8570cc23 $RHUM <- as.data.frame(scale(all8570cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all2675cc23 $RHUM<- as.data.frame(scale(all2675cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4575cc23 $RHUM<- as.data.frame(scale(all4575cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8575cc23 $RHUM<- as.data.frame(scale(all8575cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all2680cc23 $RHUM <- as.data.frame(scale(all2680cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all4580cc23 $RHUM <- as.data.frame(scale(all4580cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
all8580cc23 $RHUM <- as.data.frame(scale(all8580cc23 $RHUM, center=centerRHUM23,scale=scaleRHUM23),col.names="RHUM")[, 1]
#RAD
all2625cc23 $RAD<- as.data.frame(scale(all2625cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4525cc23 $RAD<- as.data.frame(scale(all4525cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8525cc23 $RAD<- as.data.frame(scale(all8525cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all2630cc23 $RAD <- as.data.frame(scale(all2630cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4530cc23 $RAD <- as.data.frame(scale(all4530cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8530cc23 $RAD <- as.data.frame(scale(all8530cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all2635cc23 $RAD<- as.data.frame(scale(all2635cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4535cc23 $RAD<- as.data.frame(scale(all4535cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8535cc23 $RAD<- as.data.frame(scale(all8535cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all2640cc23 $RAD <- as.data.frame(scale(all2640cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4540cc23 $RAD <- as.data.frame(scale(all4540cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8540cc23 $RAD <- as.data.frame(scale(all8540cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all2645cc23 $RAD<- as.data.frame(scale(all2645cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4545cc23 $RAD<- as.data.frame(scale(all4545cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8545cc23 $RAD<- as.data.frame(scale(all8545cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all2650cc23 $RAD <- as.data.frame(scale(all2650cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4550cc23 $RAD <- as.data.frame(scale(all4550cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8550cc23 $RAD <- as.data.frame(scale(all8550cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all2655cc23 $RAD<- as.data.frame(scale(all2655cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4555cc23 $RAD<- as.data.frame(scale(all4555cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8555cc23 $RAD<- as.data.frame(scale(all8555cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all2660cc23 $RAD <- as.data.frame(scale(all2660cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4560cc23 $RAD <- as.data.frame(scale(all4560cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8560cc23 $RAD <- as.data.frame(scale(all8560cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all2665cc23 $RAD<- as.data.frame(scale(all2665cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4565cc23 $RAD<- as.data.frame(scale(all4565cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8565cc23 $RAD<- as.data.frame(scale(all8565cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all2670cc23 $RAD <- as.data.frame(scale(all2670cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4570cc23 $RAD <- as.data.frame(scale(all4570cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8570cc23 $RAD <- as.data.frame(scale(all8570cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all2675cc23 $RAD<- as.data.frame(scale(all2675cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4575cc23 $RAD<- as.data.frame(scale(all4575cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8575cc23 $RAD<- as.data.frame(scale(all8575cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all2680cc23 $RAD <- as.data.frame(scale(all2680cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all4580cc23 $RAD <- as.data.frame(scale(all4580cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
all8580cc23 $RAD <- as.data.frame(scale(all8580cc23 $RAD, center=centerRAD23,scale=scaleRAD23),col.names="RAD")[, 1]
#Elevation
all2625cc23 $Elevation<- as.data.frame(scale(all2625cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4525cc23 $Elevation<- as.data.frame(scale(all4525cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8525cc23 $Elevation<- as.data.frame(scale(all8525cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all2630cc23 $Elevation <- as.data.frame(scale(all2630cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4530cc23 $Elevation <- as.data.frame(scale(all4530cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8530cc23 $Elevation <- as.data.frame(scale(all8530cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all2635cc23 $Elevation<- as.data.frame(scale(all2635cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4535cc23 $Elevation<- as.data.frame(scale(all4535cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8535cc23 $Elevation<- as.data.frame(scale(all8535cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all2640cc23 $Elevation <- as.data.frame(scale(all2640cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4540cc23 $Elevation <- as.data.frame(scale(all4540cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8540cc23 $Elevation <- as.data.frame(scale(all8540cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all2645cc23 $Elevation<- as.data.frame(scale(all2645cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4545cc23 $Elevation<- as.data.frame(scale(all4545cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8545cc23 $Elevation<- as.data.frame(scale(all8545cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all2650cc23 $Elevation <- as.data.frame(scale(all2650cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4550cc23 $Elevation <- as.data.frame(scale(all4550cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8550cc23 $Elevation <- as.data.frame(scale(all8550cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all2655cc23 $Elevation<- as.data.frame(scale(all2655cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4555cc23 $Elevation<- as.data.frame(scale(all4555cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8555cc23 $Elevation<- as.data.frame(scale(all8555cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all2660cc23 $Elevation <- as.data.frame(scale(all2660cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4560cc23 $Elevation <- as.data.frame(scale(all4560cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8560cc23 $Elevation <- as.data.frame(scale(all8560cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all2665cc23 $Elevation<- as.data.frame(scale(all2665cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4565cc23 $Elevation<- as.data.frame(scale(all4565cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8565cc23 $Elevation<- as.data.frame(scale(all8565cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all2670cc23 $Elevation <- as.data.frame(scale(all2670cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4570cc23 $Elevation <- as.data.frame(scale(all4570cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8570cc23 $Elevation <- as.data.frame(scale(all8570cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all2675cc23 $Elevation<- as.data.frame(scale(all2675cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4575cc23 $Elevation<- as.data.frame(scale(all4575cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8575cc23 $Elevation<- as.data.frame(scale(all8575cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all2680cc23 $Elevation <- as.data.frame(scale(all2680cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all4580cc23 $Elevation <- as.data.frame(scale(all4580cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
all8580cc23 $Elevation <- as.data.frame(scale(all8580cc23 $Elevation, center=centerElevation23,scale=scaleElevation23),col.names="Elevation")[, 1]
#BasalArea

all2625cc23$BA <- (all2625cc23$BA)+(all2625cc23$AvgBADeadRate)*6
all4525cc23$BA <- (all4525cc23$BA)+(all4525cc23$AvgBADeadRate)*6
all8525cc23$BA <- (all8525cc23$BA)+(all8525cc23$AvgBADeadRate)*6
all2630cc23$BA <- (all2630cc23$BA)+(all2630cc23$AvgBADeadRate)*11
all4530cc23$BA <- (all4530cc23$BA)+(all4530cc23$AvgBADeadRate)*11
all8530cc23$BA <- (all8530cc23$BA)+(all8530cc23$AvgBADeadRate)*11
all2635cc23$BA <- (all2635cc23$BA)+(all2635cc23$AvgBADeadRate)*16
all4535cc23$BA <-  (all4535cc23$BA)+(all4535cc23$AvgBADeadRate)*16
all8535cc23$BA <- (all8535cc23$BA)+(all8535cc23$AvgBADeadRate)*16
all2640cc23$BA <- (all2640cc23$BA)+(all2640cc23$AvgBADeadRate)*21
all4540cc23$BA <- (all4540cc23$BA)+(all4540cc23$AvgBADeadRate)*21
all8540cc23$BA <-  (all8540cc23$BA)+(all8540cc23$AvgBADeadRate)*21
all2645cc23$BA <- (all2645cc23$BA)+(all2645cc23$AvgBADeadRate)*26
all4545cc23$BA <- (all4545cc23$BA)+(all4545cc23$AvgBADeadRate)*26
all8545cc23$BA <- (all8545cc23$BA)+(all8545cc23$AvgBADeadRate)*26
all2650cc23$BA <- (all2650cc23$BA)+(all2650cc23$AvgBADeadRate)*31
all4550cc23$BA <- (all4550cc23$BA)+(all4550cc23$AvgBADeadRate)*31
all8550cc23$BA <- (all8550cc23$BA)+(all8550cc23$AvgBADeadRate)*31
all2655cc23$BA <-(all2655cc23$BA)+(all2655cc23$AvgBADeadRate)*36
all4555cc23$BA <- (all4555cc23$BA)+(all4555cc23$AvgBADeadRate)*36
all8555cc23$BA <- (all8555cc23$BA)+(all8555cc23$AvgBADeadRate)*36
all2660cc23$BA <-(all2660cc23$BA)+(all2660cc23$AvgBADeadRate)*41
all4560cc23$BA <- (all4560cc23$BA)+(all4560cc23$AvgBADeadRate)*41
all8560cc23$BA <- (all8560cc23$BA)+(all8560cc23$AvgBADeadRate)*41
all2665cc23$BA <- (all2665cc23$BA)+(all2665cc23$AvgBADeadRate)*46
all4565cc23$BA <- (all4565cc23$BA)+(all4565cc23$AvgBADeadRate)*46
all8565cc23$BA <- (all8565cc23$BA)+(all8565cc23$AvgBADeadRate)*46
all2670cc23$BA <- (all2670cc23$BA)+(all2670cc23$AvgBADeadRate)*51
all4570cc23$BA <- (all4570cc23$BA)+(all4570cc23$AvgBADeadRate)*51
all8570cc23$BA <- (all8570cc23$BA)+(all8570cc23$AvgBADeadRate)*51
all2675cc23$BA <- (all2675cc23$BA)+(all2675cc23$AvgBADeadRate)*56
all4575cc23$BA <- (all4575cc23$BA)+(all4575cc23$AvgBADeadRate)*56
all8575cc23$BA <- (all8575cc23$BA)+(all8575cc23$AvgBADeadRate)*56
all2680cc23$BA <- (all2680cc23$BA)+(all2680cc23$AvgBADeadRate)*61
all4580cc23$BA <- (all4580cc23$BA)+(all4580cc23$AvgBADeadRate)*61
all8580cc23$BA <- (all8580cc23$BA)+(all8580cc23$AvgBADeadRate)*61

#remove negatives
all2625cc23$BA[all2625cc23$BA<=0] <- 0.01
all4525cc23$BA[all4525cc23$BA<=0] <- 0.01
all8525cc23$BA[all8525cc23$BA<=0] <- 0.01
all2630cc23$BA[all2630cc23$BA<=0] <- 0.01
all4530cc23$BA[all4530cc23$BA<=0] <- 0.01
all8530cc23$BA[all8530cc23$BA<=0] <- 0.01
all2635cc23$BA[all2635cc23$BA<=0] <- 0.01
all4535cc23$BA[all4535cc23$BA<=0] <- 0.01
all8535cc23$BA[all8535cc23$BA<=0] <- 0.01
all2640cc23$BA[all2640cc23$BA<=0] <- 0.01
all4540cc23$BA[all4540cc23$BA<=0] <- 0.01
all8540cc23$BA[all8540cc23$BA<=0] <- 0.01
all2645cc23$BA[all2645cc23$BA<=0] <- 0.01
all4545cc23$BA[all4545cc23$BA<=0] <- 0.01
all8545cc23$BA[all8545cc23$BA<=0] <- 0.01
all2650cc23$BA[all2650cc23$BA<=0] <- 0.01
all4550cc23$BA[all4550cc23$BA<=0] <- 0.01
all8550cc23$BA[all8550cc23$BA<=0] <- 0.01
all2655cc23$BA[all2655cc23$BA<=0] <- 0.01
all4555cc23$BA[all4555cc23$BA<=0] <- 0.01
all8555cc23$BA[all8555cc23$BA<=0] <- 0.01
all2660cc23$BA[all2660cc23$BA<=0] <- 0.01
all4560cc23$BA[all4560cc23$BA<=0] <- 0.01
all8560cc23$BA[all8560cc23$BA<=0] <- 0.01
all2665cc23$BA[all2665cc23$BA<=0] <- 0.01
all4565cc23$BA[all4565cc23$BA<=0] <- 0.01
all8565cc23$BA[all8565cc23$BA<=0] <- 0.01
all2670cc23$BA[all2670cc23$BA<=0] <- 0.01
all4570cc23$BA[all4570cc23$BA<=0] <- 0.01
all8570cc23$BA[all8570cc23$BA<=0] <- 0.01
all2675cc23$BA[all2675cc23$BA<=0] <- 0.01
all4575cc23$BA[all4575cc23$BA<=0] <- 0.01
all8575cc23$BA[all8575cc23$BA<=0] <- 0.01
all2680cc23$BA[all2680cc23$BA<=0] <- 0.01
all4580cc23$BA[all4580cc23$BA<=0] <- 0.01
all8580cc23$BA[all8580cc23$BA<=0] <- 0.01

#max basal area of dead trees = basal area of all trees. prevents mortality over 100%, not possible.
all2625cc23$BA <- ifelse(all2625cc23$BA > all2625cc23$BAAll, all2625cc23$BAAll, all2625cc23$BA)
all4525cc23$BA <- ifelse(all4525cc23$BA > all4525cc23$BAAll, all4525cc23$BAAll, all4525cc23$BA)
all8525cc23$BA <- ifelse(all8525cc23$BA > all8525cc23$BAAll, all8525cc23$BAAll, all8525cc23$BA)
all2630cc23$BA <- ifelse(all2630cc23$BA > all2630cc23$BAAll, all2630cc23$BAAll, all2630cc23$BA)
all4530cc23$BA <- ifelse(all4530cc23$BA > all4530cc23$BAAll, all4530cc23$BAAll, all4530cc23$BA)
all8530cc23$BA <- ifelse(all8530cc23$BA > all8530cc23$BAAll, all8530cc23$BAAll, all8530cc23$BA)
all2635cc23$BA <- ifelse(all2635cc23$BA > all2635cc23$BAAll, all2635cc23$BAAll, all2635cc23$BA)
all4535cc23$BA <- ifelse(all4535cc23$BA > all4535cc23$BAAll, all4535cc23$BAAll, all4535cc23$BA)
all8535cc23$BA <- ifelse(all8535cc23$BA > all8535cc23$BAAll, all8535cc23$BAAll, all8535cc23$BA)
all2640cc23$BA <- ifelse(all2640cc23$BA > all2640cc23$BAAll, all2640cc23$BAAll, all2640cc23$BA)
all4540cc23$BA <- ifelse(all4540cc23$BA > all4540cc23$BAAll, all4540cc23$BAAll, all4540cc23$BA)
all8540cc23$BA <- ifelse(all8540cc23$BA > all8540cc23$BAAll, all8540cc23$BAAll, all8540cc23$BA)
all2645cc23$BA <- ifelse(all2645cc23$BA > all2645cc23$BAAll, all2645cc23$BAAll, all2645cc23$BA)
all4545cc23$BA <- ifelse(all4545cc23$BA > all4545cc23$BAAll, all4545cc23$BAAll, all4545cc23$BA)
all8545cc23$BA <- ifelse(all8545cc23$BA > all8545cc23$BAAll, all8545cc23$BAAll, all8545cc23$BA)
all2650cc23$BA <- ifelse(all2650cc23$BA > all2650cc23$BAAll, all2650cc23$BAAll, all2650cc23$BA)
all4550cc23$BA <- ifelse(all4550cc23$BA > all4550cc23$BAAll, all4550cc23$BAAll, all4550cc23$BA)
all8550cc23$BA <- ifelse(all8550cc23$BA > all8550cc23$BAAll, all8550cc23$BAAll, all8550cc23$BA)
all2655cc23$BA <- ifelse(all2655cc23$BA > all2655cc23$BAAll, all2655cc23$BAAll, all2655cc23$BA)
all4555cc23$BA <- ifelse(all4555cc23$BA > all4555cc23$BAAll, all4555cc23$BAAll, all4555cc23$BA)
all8555cc23$BA <- ifelse(all8555cc23$BA > all8555cc23$BAAll, all8555cc23$BAAll, all8555cc23$BA)
all2660cc23$BA <- ifelse(all2660cc23$BA > all2660cc23$BAAll, all2660cc23$BAAll, all2660cc23$BA)
all4560cc23$BA <- ifelse(all4560cc23$BA > all4560cc23$BAAll, all4560cc23$BAAll, all4560cc23$BA)
all8560cc23$BA <- ifelse(all8560cc23$BA > all8560cc23$BAAll, all8560cc23$BAAll, all8560cc23$BA)
all2665cc23$BA <- ifelse(all2665cc23$BA > all2665cc23$BAAll, all2665cc23$BAAll, all2665cc23$BA)
all4565cc23$BA <- ifelse(all4565cc23$BA > all4565cc23$BAAll, all4565cc23$BAAll, all4565cc23$BA)
all8565cc23$BA <- ifelse(all8565cc23$BA > all8565cc23$BAAll, all8565cc23$BAAll, all8565cc23$BA)
all2670cc23$BA <- ifelse(all2670cc23$BA > all2670cc23$BAAll, all2670cc23$BAAll, all2670cc23$BA)
all4570cc23$BA <- ifelse(all4570cc23$BA > all4570cc23$BAAll, all4570cc23$BAAll, all4570cc23$BA)
all8570cc23$BA <- ifelse(all8570cc23$BA > all8570cc23$BAAll, all8570cc23$BAAll, all8570cc23$BA)
all2675cc23$BA <- ifelse(all2675cc23$BA > all2675cc23$BAAll, all2675cc23$BAAll, all2675cc23$BA)
all4575cc23$BA <- ifelse(all4575cc23$BA > all4575cc23$BAAll, all4575cc23$BAAll, all4575cc23$BA)
all8575cc23$BA <- ifelse(all8575cc23$BA > all8575cc23$BAAll, all8575cc23$BAAll, all8575cc23$BA)
all2680cc23$BA <- ifelse(all2680cc23$BA > all2680cc23$BAAll, all2680cc23$BAAll, all2680cc23$BA)
all4580cc23$BA <- ifelse(all4580cc23$BA > all4580cc23$BAAll, all4580cc23$BAAll, all4580cc23$BA)
all8580cc23$BA <- ifelse(all8580cc23$BA > all8580cc23$BAAll, all8580cc23$BAAll, all8580cc23$BA)

all2625cc23 $BA<- as.data.frame(scale(all2625cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4525cc23 $BA<- as.data.frame(scale(all4525cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8525cc23 $BA<- as.data.frame(scale(all8525cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all2630cc23 $BA <- as.data.frame(scale(all2630cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4530cc23 $BA <- as.data.frame(scale(all4530cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8530cc23 $BA <- as.data.frame(scale(all8530cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all2635cc23 $BA<- as.data.frame(scale(all2635cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4535cc23 $BA<- as.data.frame(scale(all4535cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8535cc23 $BA<- as.data.frame(scale(all8535cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all2640cc23 $BA <- as.data.frame(scale(all2640cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4540cc23 $BA <- as.data.frame(scale(all4540cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8540cc23 $BA <- as.data.frame(scale(all8540cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all2645cc23 $BA<- as.data.frame(scale(all2645cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4545cc23 $BA<- as.data.frame(scale(all4545cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8545cc23 $BA<- as.data.frame(scale(all8545cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all2650cc23 $BA <- as.data.frame(scale(all2650cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4550cc23 $BA <- as.data.frame(scale(all4550cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8550cc23 $BA <- as.data.frame(scale(all8550cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all2655cc23 $BA<- as.data.frame(scale(all2655cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4555cc23 $BA<- as.data.frame(scale(all4555cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8555cc23 $BA<- as.data.frame(scale(all8555cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all2660cc23 $BA <- as.data.frame(scale(all2660cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4560cc23 $BA <- as.data.frame(scale(all4560cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8560cc23 $BA <- as.data.frame(scale(all8560cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all2665cc23 $BA<- as.data.frame(scale(all2665cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4565cc23 $BA<- as.data.frame(scale(all4565cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8565cc23 $BA<- as.data.frame(scale(all8565cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all2670cc23 $BA <- as.data.frame(scale(all2670cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4570cc23 $BA <- as.data.frame(scale(all4570cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8570cc23 $BA <- as.data.frame(scale(all8570cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all2675cc23 $BA<- as.data.frame(scale(all2675cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4575cc23 $BA<- as.data.frame(scale(all4575cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8575cc23 $BA<- as.data.frame(scale(all8575cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all2680cc23 $BA <- as.data.frame(scale(all2680cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all4580cc23 $BA <- as.data.frame(scale(all4580cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]
all8580cc23 $BA <- as.data.frame(scale(all8580cc23 $BA, center=centerBA23,scale=scaleBA23),col.names="BA")[, 1]

#time weight f23
all2625cc23$time_weight <- 1.931244
all4525cc23$time_weight <- 1.931244
all8525cc23$time_weight <- 1.931244
all2630cc23$time_weight <- 1.931244
all4530cc23$time_weight <- 1.931244
all8530cc23$time_weight <- 1.931244
all2635cc23$time_weight <- 1.931244
all4535cc23$time_weight <- 1.931244
all8535cc23$time_weight <- 1.931244
all2640cc23$time_weight <- 1.931244
all4540cc23$time_weight <- 1.931244
all8540cc23$time_weight <- 1.931244
all2645cc23$time_weight <- 1.931244
all4545cc23$time_weight <- 1.931244
all8545cc23$time_weight <- 1.931244
all2650cc23$time_weight <- 1.931244
all4550cc23$time_weight <- 1.931244
all8550cc23$time_weight <- 1.931244
all2655cc23$time_weight <- 1.931244
all4555cc23$time_weight <- 1.931244
all8555cc23$time_weight <- 1.931244
all2660cc23$time_weight <- 1.931244
all4560cc23$time_weight <- 1.931244
all8560cc23$time_weight <- 1.931244
all2665cc23$time_weight <- 1.931244
all4565cc23$time_weight <- 1.931244
all8565cc23$time_weight <- 1.931244
all2670cc23$time_weight <- 1.931244
all4570cc23$time_weight <- 1.931244
all8570cc23$time_weight <- 1.931244
all2675cc23$time_weight <- 1.931244
all4575cc23$time_weight <- 1.931244
all8575cc23$time_weight <- 1.931244
all2680cc23$time_weight <- 1.931244
all4580cc23$time_weight <- 1.931244
all8580cc23$time_weight <- 1.931244
#run simulations
simCarb2625 <- link(CarbSplit23, data=all2625cc23 )
simmeanCarb2625 <- data.frame(apply(simCarb2625,2,mean))
PICarb2625 <- t(data.frame(apply(simCarb2625,2,PI,prob=0.89)))
F23CarbSim2625 <- data.frame(cbind(simmeanCarb2625,PICarb2625))
F23CarbSim2625$PlotCN <- all2625cc23  $PlotCN
F23CarbSim2625 <- F23CarbSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2625) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4525 <- link(CarbSplit23, data=all4525cc23 )
simmeanCarb4525 <- data.frame(apply(simCarb4525,2,mean))
PICarb4525 <- t(data.frame(apply(simCarb4525,2,PI,prob=0.89)))
F23CarbSim4525 <- data.frame(cbind(simmeanCarb4525,PICarb4525))
F23CarbSim4525$PlotCN <- all4525cc23  $PlotCN
F23CarbSim4525 <- F23CarbSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4525) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8525 <- link(CarbSplit23, data=all8525cc23 )
simmeanCarb8525 <- data.frame(apply(simCarb8525,2,mean))
PICarb8525 <- t(data.frame(apply(simCarb8525,2,PI,prob=0.89)))
F23CarbSim8525 <- data.frame(cbind(simmeanCarb8525,PICarb8525))
F23CarbSim8525$PlotCN <- all8525cc23  $PlotCN
F23CarbSim8525 <- F23CarbSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8525) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2630 <- link(CarbSplit23, data=all2630cc23 )
simmeanCarb2630 <- data.frame(apply(simCarb2630,2,mean))
PICarb2630 <- t(data.frame(apply(simCarb2630,2,PI,prob=0.89)))
F23CarbSim2630 <- data.frame(cbind(simmeanCarb2630,PICarb2630))
F23CarbSim2630$PlotCN <- all2630cc23  $PlotCN
F23CarbSim2630 <- F23CarbSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2630) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4530 <- link(CarbSplit23, data=all4530cc23 )
simmeanCarb4530 <- data.frame(apply(simCarb4530,2,mean))
PICarb4530 <- t(data.frame(apply(simCarb4530,2,PI,prob=0.89)))
F23CarbSim4530 <- data.frame(cbind(simmeanCarb4530,PICarb4530))
F23CarbSim4530$PlotCN <- all4530cc23  $PlotCN
F23CarbSim4530 <- F23CarbSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4530) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8530 <- link(CarbSplit23, data=all8530cc23 )
simmeanCarb8530 <- data.frame(apply(simCarb8530,2,mean))
PICarb8530 <- t(data.frame(apply(simCarb8530,2,PI,prob=0.89)))
F23CarbSim8530 <- data.frame(cbind(simmeanCarb8530,PICarb8530))
F23CarbSim8530$PlotCN <- all8530cc23  $PlotCN
F23CarbSim8530 <- F23CarbSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8530) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2635 <- link(CarbSplit23, data=all2635cc23 )
simmeanCarb2635 <- data.frame(apply(simCarb2635,2,mean))
PICarb2635 <- t(data.frame(apply(simCarb2635,2,PI,prob=0.89)))
F23CarbSim2635 <- data.frame(cbind(simmeanCarb2635,PICarb2635))
F23CarbSim2635$PlotCN <- all2635cc23  $PlotCN
F23CarbSim2635 <- F23CarbSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2635) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4535 <- link(CarbSplit23, data=all4535cc23 )
simmeanCarb4535 <- data.frame(apply(simCarb4535,2,mean))
PICarb4535 <- t(data.frame(apply(simCarb4535,2,PI,prob=0.89)))
F23CarbSim4535 <- data.frame(cbind(simmeanCarb4535,PICarb4535))
F23CarbSim4535$PlotCN <- all4535cc23  $PlotCN
F23CarbSim4535 <- F23CarbSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4535) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8535 <- link(CarbSplit23, data=all8535cc23 )
simmeanCarb8535 <- data.frame(apply(simCarb8535,2,mean))
PICarb8535 <- t(data.frame(apply(simCarb8535,2,PI,prob=0.89)))
F23CarbSim8535 <- data.frame(cbind(simmeanCarb8535,PICarb8535))
F23CarbSim8535$PlotCN <- all8535cc23  $PlotCN
F23CarbSim8535 <- F23CarbSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8535) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2640 <- link(CarbSplit23, data=all2640cc23 )
simmeanCarb2640 <- data.frame(apply(simCarb2640,2,mean))
PICarb2640 <- t(data.frame(apply(simCarb2640,2,PI,prob=0.89)))
F23CarbSim2640 <- data.frame(cbind(simmeanCarb2640,PICarb2640))
F23CarbSim2640$PlotCN <- all2640cc23  $PlotCN
F23CarbSim2640 <- F23CarbSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2640) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4540 <- link(CarbSplit23, data=all4540cc23 )
simmeanCarb4540 <- data.frame(apply(simCarb4540,2,mean))
PICarb4540 <- t(data.frame(apply(simCarb4540,2,PI,prob=0.89)))
F23CarbSim4540 <- data.frame(cbind(simmeanCarb4540,PICarb4540))
F23CarbSim4540$PlotCN <- all4540cc23  $PlotCN
F23CarbSim4540 <- F23CarbSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4540) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8540 <- link(CarbSplit23, data=all8540cc23 )
simmeanCarb8540 <- data.frame(apply(simCarb8540,2,mean))
PICarb8540 <- t(data.frame(apply(simCarb8540,2,PI,prob=0.89)))
F23CarbSim8540 <- data.frame(cbind(simmeanCarb8540,PICarb8540))
F23CarbSim8540$PlotCN <- all8540cc23  $PlotCN
F23CarbSim8540 <- F23CarbSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8540) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2645 <- link(CarbSplit23, data=all2645cc23 )
simmeanCarb2645 <- data.frame(apply(simCarb2645,2,mean))
PICarb2645 <- t(data.frame(apply(simCarb2645,2,PI,prob=0.89)))
F23CarbSim2645 <- data.frame(cbind(simmeanCarb2645,PICarb2645))
F23CarbSim2645$PlotCN <- all2645cc23  $PlotCN
F23CarbSim2645 <- F23CarbSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2645) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4545 <- link(CarbSplit23, data=all4545cc23 )
simmeanCarb4545 <- data.frame(apply(simCarb4545,2,mean))
PICarb4545 <- t(data.frame(apply(simCarb4545,2,PI,prob=0.89)))
F23CarbSim4545 <- data.frame(cbind(simmeanCarb4545,PICarb4545))
F23CarbSim4545$PlotCN <- all4545cc23  $PlotCN
F23CarbSim4545 <- F23CarbSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4545) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8545 <- link(CarbSplit23, data=all8545cc23 )
simmeanCarb8545 <- data.frame(apply(simCarb8545,2,mean))
PICarb8545 <- t(data.frame(apply(simCarb8545,2,PI,prob=0.89)))
F23CarbSim8545 <- data.frame(cbind(simmeanCarb8545,PICarb8545))
F23CarbSim8545$PlotCN <- all8545cc23  $PlotCN
F23CarbSim8545 <- F23CarbSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8545) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2650 <- link(CarbSplit23, data=all2650cc23 )
simmeanCarb2650 <- data.frame(apply(simCarb2650,2,mean))
PICarb2650 <- t(data.frame(apply(simCarb2650,2,PI,prob=0.89)))
F23CarbSim2650 <- data.frame(cbind(simmeanCarb2650,PICarb2650))
F23CarbSim2650$PlotCN <- all2650cc23  $PlotCN
F23CarbSim2650 <- F23CarbSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2650) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4550 <- link(CarbSplit23, data=all4550cc23 )
simmeanCarb4550 <- data.frame(apply(simCarb4550,2,mean))
PICarb4550 <- t(data.frame(apply(simCarb4550,2,PI,prob=0.89)))
F23CarbSim4550 <- data.frame(cbind(simmeanCarb4550,PICarb4550))
F23CarbSim4550$PlotCN <- all4550cc23  $PlotCN
F23CarbSim4550 <- F23CarbSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4550) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8550 <- link(CarbSplit23, data=all8550cc23 )
simmeanCarb8550 <- data.frame(apply(simCarb8550,2,mean))
PICarb8550 <- t(data.frame(apply(simCarb8550,2,PI,prob=0.89)))
F23CarbSim8550 <- data.frame(cbind(simmeanCarb8550,PICarb8550))
F23CarbSim8550$PlotCN <- all8550cc23  $PlotCN
F23CarbSim8550 <- F23CarbSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8550) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2655 <- link(CarbSplit23, data=all2655cc23 )
simmeanCarb2655 <- data.frame(apply(simCarb2655,2,mean))
PICarb2655 <- t(data.frame(apply(simCarb2655,2,PI,prob=0.89)))
F23CarbSim2655 <- data.frame(cbind(simmeanCarb2655,PICarb2655))
F23CarbSim2655$PlotCN <- all2655cc23  $PlotCN
F23CarbSim2655 <- F23CarbSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2655) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4555 <- link(CarbSplit23, data=all4555cc23 )
simmeanCarb4555 <- data.frame(apply(simCarb4555,2,mean))
PICarb4555 <- t(data.frame(apply(simCarb4555,2,PI,prob=0.89)))
F23CarbSim4555 <- data.frame(cbind(simmeanCarb4555,PICarb4555))
F23CarbSim4555$PlotCN <- all4555cc23  $PlotCN
F23CarbSim4555 <- F23CarbSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4555) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8555 <- link(CarbSplit23, data=all8555cc23 )
simmeanCarb8555 <- data.frame(apply(simCarb8555,2,mean))
PICarb8555 <- t(data.frame(apply(simCarb8555,2,PI,prob=0.89)))
F23CarbSim8555 <- data.frame(cbind(simmeanCarb8555,PICarb8555))
F23CarbSim8555$PlotCN <- all8555cc23  $PlotCN
F23CarbSim8555 <- F23CarbSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8555) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2660 <- link(CarbSplit23, data=all2660cc23 )
simmeanCarb2660 <- data.frame(apply(simCarb2660,2,mean))
PICarb2660 <- t(data.frame(apply(simCarb2660,2,PI,prob=0.89)))
F23CarbSim2660 <- data.frame(cbind(simmeanCarb2660,PICarb2660))
F23CarbSim2660$PlotCN <- all2660cc23  $PlotCN
F23CarbSim2660 <- F23CarbSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2660) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4560 <- link(CarbSplit23, data=all4560cc23 )
simmeanCarb4560 <- data.frame(apply(simCarb4560,2,mean))
PICarb4560 <- t(data.frame(apply(simCarb4560,2,PI,prob=0.89)))
F23CarbSim4560 <- data.frame(cbind(simmeanCarb4560,PICarb4560))
F23CarbSim4560$PlotCN <- all4560cc23  $PlotCN
F23CarbSim4560 <- F23CarbSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4560) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8560 <- link(CarbSplit23, data=all8560cc23 )
simmeanCarb8560 <- data.frame(apply(simCarb8560,2,mean))
PICarb8560 <- t(data.frame(apply(simCarb8560,2,PI,prob=0.89)))
F23CarbSim8560 <- data.frame(cbind(simmeanCarb8560,PICarb8560))
F23CarbSim8560$PlotCN <- all8560cc23  $PlotCN
F23CarbSim8560 <- F23CarbSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8560) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2665 <- link(CarbSplit23, data=all2665cc23 )
simmeanCarb2665 <- data.frame(apply(simCarb2665,2,mean))
PICarb2665 <- t(data.frame(apply(simCarb2665,2,PI,prob=0.89)))
F23CarbSim2665 <- data.frame(cbind(simmeanCarb2665,PICarb2665))
F23CarbSim2665$PlotCN <- all2665cc23  $PlotCN
F23CarbSim2665 <- F23CarbSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2665) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4565 <- link(CarbSplit23, data=all4565cc23 )
simmeanCarb4565 <- data.frame(apply(simCarb4565,2,mean))
PICarb4565 <- t(data.frame(apply(simCarb4565,2,PI,prob=0.89)))
F23CarbSim4565 <- data.frame(cbind(simmeanCarb4565,PICarb4565))
F23CarbSim4565$PlotCN <- all4565cc23  $PlotCN
F23CarbSim4565 <- F23CarbSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4565) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8565 <- link(CarbSplit23, data=all8565cc23 )
simmeanCarb8565 <- data.frame(apply(simCarb8565,2,mean))
PICarb8565 <- t(data.frame(apply(simCarb8565,2,PI,prob=0.89)))
F23CarbSim8565 <- data.frame(cbind(simmeanCarb8565,PICarb8565))
F23CarbSim8565$PlotCN <- all8565cc23  $PlotCN
F23CarbSim8565 <- F23CarbSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8565) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2670 <- link(CarbSplit23, data=all2670cc23 )
simmeanCarb2670 <- data.frame(apply(simCarb2670,2,mean))
PICarb2670 <- t(data.frame(apply(simCarb2670,2,PI,prob=0.89)))
F23CarbSim2670 <- data.frame(cbind(simmeanCarb2670,PICarb2670))
F23CarbSim2670$PlotCN <- all2670cc23  $PlotCN
F23CarbSim2670 <- F23CarbSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2670) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4570 <- link(CarbSplit23, data=all4570cc23 )
simmeanCarb4570 <- data.frame(apply(simCarb4570,2,mean))
PICarb4570 <- t(data.frame(apply(simCarb4570,2,PI,prob=0.89)))
F23CarbSim4570 <- data.frame(cbind(simmeanCarb4570,PICarb4570))
F23CarbSim4570$PlotCN <- all4570cc23  $PlotCN
F23CarbSim4570 <- F23CarbSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4570) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8570 <- link(CarbSplit23, data=all8570cc23 )
simmeanCarb8570 <- data.frame(apply(simCarb8570,2,mean))
PICarb8570 <- t(data.frame(apply(simCarb8570,2,PI,prob=0.89)))
F23CarbSim8570 <- data.frame(cbind(simmeanCarb8570,PICarb8570))
F23CarbSim8570$PlotCN <- all8570cc23  $PlotCN
F23CarbSim8570 <- F23CarbSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8570) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2675 <- link(CarbSplit23, data=all2675cc23 )
simmeanCarb2675 <- data.frame(apply(simCarb2675,2,mean))
PICarb2675 <- t(data.frame(apply(simCarb2675,2,PI,prob=0.89)))
F23CarbSim2675 <- data.frame(cbind(simmeanCarb2675,PICarb2675))
F23CarbSim2675$PlotCN <- all2675cc23  $PlotCN
F23CarbSim2675 <- F23CarbSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2675) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4575 <- link(CarbSplit23, data=all4575cc23 )
simmeanCarb4575 <- data.frame(apply(simCarb4575,2,mean))
PICarb4575 <- t(data.frame(apply(simCarb4575,2,PI,prob=0.89)))
F23CarbSim4575 <- data.frame(cbind(simmeanCarb4575,PICarb4575))
F23CarbSim4575$PlotCN <- all4575cc23  $PlotCN
F23CarbSim4575 <- F23CarbSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4575) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8575 <- link(CarbSplit23, data=all8575cc23 )
simmeanCarb8575 <- data.frame(apply(simCarb8575,2,mean))
PICarb8575 <- t(data.frame(apply(simCarb8575,2,PI,prob=0.89)))
F23CarbSim8575 <- data.frame(cbind(simmeanCarb8575,PICarb8575))
F23CarbSim8575$PlotCN <- all8575cc23  $PlotCN
F23CarbSim8575 <- F23CarbSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8575) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2680 <- link(CarbSplit23, data=all2680cc23 )
simmeanCarb2680 <- data.frame(apply(simCarb2680,2,mean))
PICarb2680 <- t(data.frame(apply(simCarb2680,2,PI,prob=0.89)))
F23CarbSim2680 <- data.frame(cbind(simmeanCarb2680,PICarb2680))
F23CarbSim2680$PlotCN <- all2680cc23  $PlotCN
F23CarbSim2680 <- F23CarbSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim2680) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4580 <- link(CarbSplit23, data=all4580cc23 )
simmeanCarb4580 <- data.frame(apply(simCarb4580,2,mean))
PICarb4580 <- t(data.frame(apply(simCarb4580,2,PI,prob=0.89)))
F23CarbSim4580 <- data.frame(cbind(simmeanCarb4580,PICarb4580))
F23CarbSim4580$PlotCN <- all4580cc23  $PlotCN
F23CarbSim4580 <- F23CarbSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim4580) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8580 <- link(CarbSplit23, data=all8580cc23 )
simmeanCarb8580 <- data.frame(apply(simCarb8580,2,mean))
PICarb8580 <- t(data.frame(apply(simCarb8580,2,PI,prob=0.89)))
F23CarbSim8580 <- data.frame(cbind(simmeanCarb8580,PICarb8580))
F23CarbSim8580$PlotCN <- all8580cc23  $PlotCN
F23CarbSim8580 <- F23CarbSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F23CarbSim8580) <- c("PlotCN","CarbPerAcre","5CI","95CI")

#create vector of new colnames
CarbSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Carb", "5CI", "95CI"), x)))
CarbSimnames<- append(CarbSimnames,"PlotCN",after=0)

#group data by RCP pathway
F23CarbSim26 <- F23CarbSim2625 %>% cbind(F23CarbSim2630[,2:4]) %>%
  cbind(F23CarbSim2635[,2:4]) %>% cbind(F23CarbSim2640[,2:4]) %>% cbind(F23CarbSim2645[,2:4]) %>%
  cbind(F23CarbSim2650[,2:4]) %>% cbind(F23CarbSim2655[,2:4]) %>% cbind(F23CarbSim2660[,2:4]) %>%
  cbind(F23CarbSim2665[,2:4]) %>% cbind(F23CarbSim2670[,2:4]) %>% cbind(F23CarbSim2675[,2:4]) %>%
  cbind(F23CarbSim2680[,2:4])
colnames(F23CarbSim26) <- CarbSimnames
write.csv(F23CarbSim26,file="F23CarbPred26.csv")

F23CarbSim45 <- F23CarbSim4525 %>% cbind(F23CarbSim4530[,2:4]) %>%
  cbind(F23CarbSim4535[,2:4]) %>% cbind(F23CarbSim4540[,2:4]) %>% cbind(F23CarbSim4545[,2:4]) %>%
  cbind(F23CarbSim4550[,2:4]) %>% cbind(F23CarbSim4555[,2:4]) %>% cbind(F23CarbSim4560[,2:4]) %>%
  cbind(F23CarbSim4565[,2:4]) %>% cbind(F23CarbSim4570[,2:4]) %>% cbind(F23CarbSim4575[,2:4]) %>%
  cbind(F23CarbSim4580[,2:4])
colnames(F23CarbSim45) <- CarbSimnames
write.csv(F23CarbSim45,file="F23CarbPred45.csv")

F23CarbSim85 <- F23CarbSim8525 %>% cbind(F23CarbSim8530[,2:4]) %>%
  cbind(F23CarbSim8535[,2:4]) %>% cbind(F23CarbSim8540[,2:4]) %>% cbind(F23CarbSim8545[,2:4]) %>%
  cbind(F23CarbSim8550[,2:4]) %>% cbind(F23CarbSim8555[,2:4]) %>% cbind(F23CarbSim8560[,2:4]) %>%
  cbind(F23CarbSim8565[,2:4]) %>% cbind(F23CarbSim8570[,2:4]) %>% cbind(F23CarbSim8575[,2:4]) %>%
  cbind(F23CarbSim8580[,2:4])
colnames(F23CarbSim85) <- CarbSimnames
write.csv(F23CarbSim85,file="F23CarbPred85.csv")


#fgroup 24
all2625cc24 <- all2625cc %>% filter(ForestGroup==24)
all4525cc24 <- all4525cc %>% filter(ForestGroup==24)
all8525cc24 <- all8525cc %>% filter(ForestGroup==24)
all2630cc24 <- all2630cc %>% filter(ForestGroup==24)
all4530cc24 <- all4530cc %>% filter(ForestGroup==24)
all8530cc24 <- all8530cc %>% filter(ForestGroup==24)
all2635cc24 <- all2635cc %>% filter(ForestGroup==24)
all4535cc24 <- all4535cc %>% filter(ForestGroup==24)
all8535cc24 <- all8535cc %>% filter(ForestGroup==24)
all2640cc24 <- all2640cc %>% filter(ForestGroup==24)
all4540cc24 <- all4540cc %>% filter(ForestGroup==24)
all8540cc24 <- all8540cc %>% filter(ForestGroup==24)
all2645cc24 <- all2645cc %>% filter(ForestGroup==24)
all4545cc24 <- all4545cc %>% filter(ForestGroup==24)
all8545cc24 <- all8545cc %>% filter(ForestGroup==24)
all2650cc24 <- all2650cc %>% filter(ForestGroup==24)
all4550cc24 <- all4550cc %>% filter(ForestGroup==24)
all8550cc24 <- all8550cc %>% filter(ForestGroup==24)
all2655cc24 <- all2655cc %>% filter(ForestGroup==24)
all4555cc24 <- all4555cc %>% filter(ForestGroup==24)
all8555cc24 <- all8555cc %>% filter(ForestGroup==24)
all2660cc24 <- all2660cc %>% filter(ForestGroup==24)
all4560cc24 <- all4560cc %>% filter(ForestGroup==24)
all8560cc24 <- all8560cc %>% filter(ForestGroup==24)
all2665cc24 <- all2665cc %>% filter(ForestGroup==24)
all4565cc24 <- all4565cc %>% filter(ForestGroup==24)
all8565cc24 <- all8565cc %>% filter(ForestGroup==24)
all2670cc24 <- all2670cc %>% filter(ForestGroup==24)
all4570cc24 <- all4570cc %>% filter(ForestGroup==24)
all8570cc24 <- all8570cc %>% filter(ForestGroup==24)
all2675cc24 <- all2675cc %>% filter(ForestGroup==24)
all4575cc24 <- all4575cc %>% filter(ForestGroup==24)
all8575cc24 <- all8575cc %>% filter(ForestGroup==24)
all2680cc24 <- all2680cc %>% filter(ForestGroup==24)
all4580cc24 <- all4580cc %>% filter(ForestGroup==24)
all8580cc24 <- all8580cc %>% filter(ForestGroup==24)

#turn soil order and forest type into factors
all2625cc24$Soil <- as.factor(all2625cc24$soil_order)
all2625cc24$Soil <- as.integer(all2625cc24$Soil)
all2625cc24$FType <- as.factor(all2625cc24$ForestType)
all2625cc24$FType <- as.integer(all2625cc24$FType)
all4525cc24$Soil <- as.factor(all4525cc24$soil_order)
all4525cc24$Soil <- as.integer(all4525cc24$Soil)
all4525cc24$FType <- as.factor(all4525cc24$ForestType)
all4525cc24$FType <- as.integer(all4525cc24$FType)
all8525cc24$Soil <- as.factor(all8525cc24$soil_order)
all8525cc24$Soil <- as.integer(all8525cc24$Soil)
all8525cc24$FType <- as.factor(all8525cc24$ForestType)
all8525cc24$FType <- as.integer(all8525cc24$FType)

all2630cc24$Soil <- as.factor(all2630cc24$soil_order)
all2630cc24$Soil <- as.integer(all2630cc24$Soil)
all2630cc24$FType <- as.factor(all2630cc24$ForestType)
all2630cc24$FType <- as.integer(all2630cc24$FType)
all4530cc24$Soil <- as.factor(all4530cc24$soil_order)
all4530cc24$Soil <- as.integer(all4530cc24$Soil)
all4530cc24$FType <- as.factor(all4530cc24$ForestType)
all4530cc24$FType <- as.integer(all4530cc24$FType)
all8530cc24$Soil <- as.factor(all8530cc24$soil_order)
all8530cc24$Soil <- as.integer(all8530cc24$Soil)
all8530cc24$FType <- as.factor(all8530cc24$ForestType)
all8530cc24$FType <- as.integer(all8530cc24$FType)

all2635cc24$Soil <- as.factor(all2635cc24$soil_order)
all2635cc24$Soil <- as.integer(all2635cc24$Soil)
all2635cc24$FType <- as.factor(all2635cc24$ForestType)
all2635cc24$FType <- as.integer(all2635cc24$FType)
all4535cc24$Soil <- as.factor(all4535cc24$soil_order)
all4535cc24$Soil <- as.integer(all4535cc24$Soil)
all4535cc24$FType <- as.factor(all4535cc24$ForestType)
all4535cc24$FType <- as.integer(all4535cc24$FType)
all8535cc24$Soil <- as.factor(all8535cc24$soil_order)
all8535cc24$Soil <- as.integer(all8535cc24$Soil)
all8535cc24$FType <- as.factor(all8535cc24$ForestType)
all8535cc24$FType <- as.integer(all8535cc24$FType)


all2640cc24$Soil <- as.factor(all2640cc24$soil_order)
all2640cc24$Soil <- as.integer(all2640cc24$Soil)
all2640cc24$FType <- as.factor(all2640cc24$ForestType)
all2640cc24$FType <- as.integer(all2640cc24$FType)
all4540cc24$Soil <- as.factor(all4540cc24$soil_order)
all4540cc24$Soil <- as.integer(all4540cc24$Soil)
all4540cc24$FType <- as.factor(all4540cc24$ForestType)
all4540cc24$FType <- as.integer(all4540cc24$FType)
all8540cc24$Soil <- as.factor(all8540cc24$soil_order)
all8540cc24$Soil <- as.integer(all8540cc24$Soil)
all8540cc24$FType <- as.factor(all8540cc24$ForestType)
all8540cc24$FType <- as.integer(all8540cc24$FType)

all2645cc24$Soil <- as.factor(all2645cc24$soil_order)
all2645cc24$Soil <- as.integer(all2645cc24$Soil)
all2645cc24$FType <- as.factor(all2645cc24$ForestType)
all2645cc24$FType <- as.integer(all2645cc24$FType)
all4545cc24$Soil <- as.factor(all4545cc24$soil_order)
all4545cc24$Soil <- as.integer(all4545cc24$Soil)
all4545cc24$FType <- as.factor(all4545cc24$ForestType)
all4545cc24$FType <- as.integer(all4545cc24$FType)
all8545cc24$Soil <- as.factor(all8545cc24$soil_order)
all8545cc24$Soil <- as.integer(all8545cc24$Soil)
all8545cc24$FType <- as.factor(all8545cc24$ForestType)
all8545cc24$FType <- as.integer(all8545cc24$FType)

all2650cc24$Soil <- as.factor(all2650cc24$soil_order)
all2650cc24$Soil <- as.integer(all2650cc24$Soil)
all2650cc24$FType <- as.factor(all2650cc24$ForestType)
all2650cc24$FType <- as.integer(all2650cc24$FType)
all4550cc24$Soil <- as.factor(all4550cc24$soil_order)
all4550cc24$Soil <- as.integer(all4550cc24$Soil)
all4550cc24$FType <- as.factor(all4550cc24$ForestType)
all4550cc24$FType <- as.integer(all4550cc24$FType)
all8550cc24$Soil <- as.factor(all8550cc24$soil_order)
all8550cc24$Soil <- as.integer(all8550cc24$Soil)
all8550cc24$FType <- as.factor(all8550cc24$ForestType)
all8550cc24$FType <- as.integer(all8550cc24$FType)

all2655cc24$Soil <- as.factor(all2655cc24$soil_order)
all2655cc24$Soil <- as.integer(all2655cc24$Soil)
all2655cc24$FType <- as.factor(all2655cc24$ForestType)
all2655cc24$FType <- as.integer(all2655cc24$FType)
all4555cc24$Soil <- as.factor(all4555cc24$soil_order)
all4555cc24$Soil <- as.integer(all4555cc24$Soil)
all4555cc24$FType <- as.factor(all4555cc24$ForestType)
all4555cc24$FType <- as.integer(all4555cc24$FType)
all8555cc24$Soil <- as.factor(all8555cc24$soil_order)
all8555cc24$Soil <- as.integer(all8555cc24$Soil)
all8555cc24$FType <- as.factor(all8555cc24$ForestType)
all8555cc24$FType <- as.integer(all8555cc24$FType)


all2660cc24$Soil <- as.factor(all2660cc24$soil_order)
all2660cc24$Soil <- as.integer(all2660cc24$Soil)
all2660cc24$FType <- as.factor(all2660cc24$ForestType)
all2660cc24$FType <- as.integer(all2660cc24$FType)
all4560cc24$Soil <- as.factor(all4560cc24$soil_order)
all4560cc24$Soil <- as.integer(all4560cc24$Soil)
all4560cc24$FType <- as.factor(all4560cc24$ForestType)
all4560cc24$FType <- as.integer(all4560cc24$FType)
all8560cc24$Soil <- as.factor(all8560cc24$soil_order)
all8560cc24$Soil <- as.integer(all8560cc24$Soil)
all8560cc24$FType <- as.factor(all8560cc24$ForestType)
all8560cc24$FType <- as.integer(all8560cc24$FType)

all2665cc24$Soil <- as.factor(all2665cc24$soil_order)
all2665cc24$Soil <- as.integer(all2665cc24$Soil)
all2665cc24$FType <- as.factor(all2665cc24$ForestType)
all2665cc24$FType <- as.integer(all2665cc24$FType)
all4565cc24$Soil <- as.factor(all4565cc24$soil_order)
all4565cc24$Soil <- as.integer(all4565cc24$Soil)
all4565cc24$FType <- as.factor(all4565cc24$ForestType)
all4565cc24$FType <- as.integer(all4565cc24$FType)
all8565cc24$Soil <- as.factor(all8565cc24$soil_order)
all8565cc24$Soil <- as.integer(all8565cc24$Soil)
all8565cc24$FType <- as.factor(all8565cc24$ForestType)
all8565cc24$FType <- as.integer(all8565cc24$FType)

all2670cc24$Soil <- as.factor(all2670cc24$soil_order)
all2670cc24$Soil <- as.integer(all2670cc24$Soil)
all2670cc24$FType <- as.factor(all2670cc24$ForestType)
all2670cc24$FType <- as.integer(all2670cc24$FType)
all4570cc24$Soil <- as.factor(all4570cc24$soil_order)
all4570cc24$Soil <- as.integer(all4570cc24$Soil)
all4570cc24$FType <- as.factor(all4570cc24$ForestType)
all4570cc24$FType <- as.integer(all4570cc24$FType)
all8570cc24$Soil <- as.factor(all8570cc24$soil_order)
all8570cc24$Soil <- as.integer(all8570cc24$Soil)
all8570cc24$FType <- as.factor(all8570cc24$ForestType)
all8570cc24$FType <- as.integer(all8570cc24$FType)


all2675cc24$Soil <- as.factor(all2675cc24$soil_order)
all2675cc24$Soil <- as.integer(all2675cc24$Soil)
all2675cc24$FType <- as.factor(all2675cc24$ForestType)
all2675cc24$FType <- as.integer(all2675cc24$FType)
all4575cc24$Soil <- as.factor(all4575cc24$soil_order)
all4575cc24$Soil <- as.integer(all4575cc24$Soil)
all4575cc24$FType <- as.factor(all4575cc24$ForestType)
all4575cc24$FType <- as.integer(all4575cc24$FType)
all8575cc24$Soil <- as.factor(all8575cc24$soil_order)
all8575cc24$Soil <- as.integer(all8575cc24$Soil)
all8575cc24$FType <- as.factor(all8575cc24$ForestType)
all8575cc24$FType <- as.integer(all8575cc24$FType)

all2680cc24$Soil <- as.factor(all2680cc24$soil_order)
all2680cc24$Soil <- as.integer(all2680cc24$Soil)
all2680cc24$FType <- as.factor(all2680cc24$ForestType)
all2680cc24$FType <- as.integer(all2680cc24$FType)
all4580cc24$Soil <- as.factor(all4580cc24$soil_order)
all4580cc24$Soil <- as.integer(all4580cc24$Soil)
all4580cc24$FType <- as.factor(all4580cc24$ForestType)
all4580cc24$FType <- as.integer(all4580cc24$FType)
all8580cc24$Soil <- as.factor(all8580cc24$soil_order)
all8580cc24$Soil <- as.integer(all8580cc24$Soil)
all8580cc24$FType <- as.factor(all8580cc24$ForestType)
all8580cc24$FType <- as.integer(all8580cc24$FType)

#center and scale from model data
all2625cc24 $MAT<- as.data.frame(scale(all2625cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4525cc24 $MAT<- as.data.frame(scale(all4525cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8525cc24 $MAT<- as.data.frame(scale(all8525cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all2630cc24 $MAT <- as.data.frame(scale(all2630cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4530cc24 $MAT <- as.data.frame(scale(all4530cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8530cc24 $MAT <- as.data.frame(scale(all8530cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all2635cc24 $MAT<- as.data.frame(scale(all2635cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4535cc24 $MAT<- as.data.frame(scale(all4535cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8535cc24 $MAT<- as.data.frame(scale(all8535cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all2640cc24 $MAT <- as.data.frame(scale(all2640cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4540cc24 $MAT <- as.data.frame(scale(all4540cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8540cc24 $MAT <- as.data.frame(scale(all8540cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all2645cc24 $MAT<- as.data.frame(scale(all2645cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4545cc24 $MAT<- as.data.frame(scale(all4545cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8545cc24 $MAT<- as.data.frame(scale(all8545cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all2650cc24 $MAT <- as.data.frame(scale(all2650cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4550cc24 $MAT <- as.data.frame(scale(all4550cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8550cc24 $MAT <- as.data.frame(scale(all8550cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all2655cc24 $MAT<- as.data.frame(scale(all2655cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4555cc24 $MAT<- as.data.frame(scale(all4555cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8555cc24 $MAT<- as.data.frame(scale(all8555cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all2660cc24 $MAT <- as.data.frame(scale(all2660cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4560cc24 $MAT <- as.data.frame(scale(all4560cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8560cc24 $MAT <- as.data.frame(scale(all8560cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all2665cc24 $MAT<- as.data.frame(scale(all2665cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4565cc24 $MAT<- as.data.frame(scale(all4565cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8565cc24 $MAT<- as.data.frame(scale(all8565cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all2670cc24 $MAT <- as.data.frame(scale(all2670cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4570cc24 $MAT <- as.data.frame(scale(all4570cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8570cc24 $MAT <- as.data.frame(scale(all8570cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all2675cc24 $MAT<- as.data.frame(scale(all2675cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4575cc24 $MAT<- as.data.frame(scale(all4575cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8575cc24 $MAT<- as.data.frame(scale(all8575cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all2680cc24 $MAT <- as.data.frame(scale(all2680cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all4580cc24 $MAT <- as.data.frame(scale(all4580cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
all8580cc24 $MAT <- as.data.frame(scale(all8580cc24 $MAT, center=centerMAT24,scale=scaleMAT24),col.names="MAT")[, 1]
#PPT
all2625cc24 $PPT<- as.data.frame(scale(all2625cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4525cc24 $PPT<- as.data.frame(scale(all4525cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8525cc24 $PPT<- as.data.frame(scale(all8525cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all2630cc24 $PPT <- as.data.frame(scale(all2630cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4530cc24 $PPT <- as.data.frame(scale(all4530cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8530cc24 $PPT <- as.data.frame(scale(all8530cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all2635cc24 $PPT<- as.data.frame(scale(all2635cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4535cc24 $PPT<- as.data.frame(scale(all4535cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8535cc24 $PPT<- as.data.frame(scale(all8535cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all2640cc24 $PPT <- as.data.frame(scale(all2640cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4540cc24 $PPT <- as.data.frame(scale(all4540cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8540cc24 $PPT <- as.data.frame(scale(all8540cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all2645cc24 $PPT<- as.data.frame(scale(all2645cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4545cc24 $PPT<- as.data.frame(scale(all4545cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8545cc24 $PPT<- as.data.frame(scale(all8545cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all2650cc24 $PPT <- as.data.frame(scale(all2650cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4550cc24 $PPT <- as.data.frame(scale(all4550cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8550cc24 $PPT <- as.data.frame(scale(all8550cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all2655cc24 $PPT<- as.data.frame(scale(all2655cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4555cc24 $PPT<- as.data.frame(scale(all4555cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8555cc24 $PPT<- as.data.frame(scale(all8555cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all2660cc24 $PPT <- as.data.frame(scale(all2660cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4560cc24 $PPT <- as.data.frame(scale(all4560cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8560cc24 $PPT <- as.data.frame(scale(all8560cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all2665cc24 $PPT<- as.data.frame(scale(all2665cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4565cc24 $PPT<- as.data.frame(scale(all4565cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8565cc24 $PPT<- as.data.frame(scale(all8565cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all2670cc24 $PPT <- as.data.frame(scale(all2670cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4570cc24 $PPT <- as.data.frame(scale(all4570cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8570cc24 $PPT <- as.data.frame(scale(all8570cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all2675cc24 $PPT<- as.data.frame(scale(all2675cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4575cc24 $PPT<- as.data.frame(scale(all4575cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8575cc24 $PPT<- as.data.frame(scale(all8575cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all2680cc24 $PPT <- as.data.frame(scale(all2680cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all4580cc24 $PPT <- as.data.frame(scale(all4580cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
all8580cc24 $PPT <- as.data.frame(scale(all8580cc24 $PPT, center=centerPPT24,scale=scalePPT24),col.names="PPT")[, 1]
#RHUM
all2625cc24 $RHUM<- as.data.frame(scale(all2625cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4525cc24 $RHUM<- as.data.frame(scale(all4525cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8525cc24 $RHUM<- as.data.frame(scale(all8525cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all2630cc24 $RHUM <- as.data.frame(scale(all2630cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4530cc24 $RHUM <- as.data.frame(scale(all4530cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8530cc24 $RHUM <- as.data.frame(scale(all8530cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all2635cc24 $RHUM<- as.data.frame(scale(all2635cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4535cc24 $RHUM<- as.data.frame(scale(all4535cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8535cc24 $RHUM<- as.data.frame(scale(all8535cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all2640cc24 $RHUM <- as.data.frame(scale(all2640cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4540cc24 $RHUM <- as.data.frame(scale(all4540cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8540cc24 $RHUM <- as.data.frame(scale(all8540cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all2645cc24 $RHUM<- as.data.frame(scale(all2645cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4545cc24 $RHUM<- as.data.frame(scale(all4545cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8545cc24 $RHUM<- as.data.frame(scale(all8545cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all2650cc24 $RHUM <- as.data.frame(scale(all2650cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4550cc24 $RHUM <- as.data.frame(scale(all4550cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8550cc24 $RHUM <- as.data.frame(scale(all8550cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all2655cc24 $RHUM<- as.data.frame(scale(all2655cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4555cc24 $RHUM<- as.data.frame(scale(all4555cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8555cc24 $RHUM<- as.data.frame(scale(all8555cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all2660cc24 $RHUM <- as.data.frame(scale(all2660cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4560cc24 $RHUM <- as.data.frame(scale(all4560cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8560cc24 $RHUM <- as.data.frame(scale(all8560cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all2665cc24 $RHUM<- as.data.frame(scale(all2665cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4565cc24 $RHUM<- as.data.frame(scale(all4565cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8565cc24 $RHUM<- as.data.frame(scale(all8565cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all2670cc24 $RHUM <- as.data.frame(scale(all2670cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4570cc24 $RHUM <- as.data.frame(scale(all4570cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8570cc24 $RHUM <- as.data.frame(scale(all8570cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all2675cc24 $RHUM<- as.data.frame(scale(all2675cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4575cc24 $RHUM<- as.data.frame(scale(all4575cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8575cc24 $RHUM<- as.data.frame(scale(all8575cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all2680cc24 $RHUM <- as.data.frame(scale(all2680cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all4580cc24 $RHUM <- as.data.frame(scale(all4580cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
all8580cc24 $RHUM <- as.data.frame(scale(all8580cc24 $RHUM, center=centerRHUM24,scale=scaleRHUM24),col.names="RHUM")[, 1]
#RAD
all2625cc24 $RAD<- as.data.frame(scale(all2625cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4525cc24 $RAD<- as.data.frame(scale(all4525cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8525cc24 $RAD<- as.data.frame(scale(all8525cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all2630cc24 $RAD <- as.data.frame(scale(all2630cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4530cc24 $RAD <- as.data.frame(scale(all4530cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8530cc24 $RAD <- as.data.frame(scale(all8530cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all2635cc24 $RAD<- as.data.frame(scale(all2635cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4535cc24 $RAD<- as.data.frame(scale(all4535cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8535cc24 $RAD<- as.data.frame(scale(all8535cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all2640cc24 $RAD <- as.data.frame(scale(all2640cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4540cc24 $RAD <- as.data.frame(scale(all4540cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8540cc24 $RAD <- as.data.frame(scale(all8540cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all2645cc24 $RAD<- as.data.frame(scale(all2645cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4545cc24 $RAD<- as.data.frame(scale(all4545cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8545cc24 $RAD<- as.data.frame(scale(all8545cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all2650cc24 $RAD <- as.data.frame(scale(all2650cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4550cc24 $RAD <- as.data.frame(scale(all4550cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8550cc24 $RAD <- as.data.frame(scale(all8550cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all2655cc24 $RAD<- as.data.frame(scale(all2655cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4555cc24 $RAD<- as.data.frame(scale(all4555cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8555cc24 $RAD<- as.data.frame(scale(all8555cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all2660cc24 $RAD <- as.data.frame(scale(all2660cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4560cc24 $RAD <- as.data.frame(scale(all4560cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8560cc24 $RAD <- as.data.frame(scale(all8560cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all2665cc24 $RAD<- as.data.frame(scale(all2665cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4565cc24 $RAD<- as.data.frame(scale(all4565cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8565cc24 $RAD<- as.data.frame(scale(all8565cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all2670cc24 $RAD <- as.data.frame(scale(all2670cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4570cc24 $RAD <- as.data.frame(scale(all4570cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8570cc24 $RAD <- as.data.frame(scale(all8570cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all2675cc24 $RAD<- as.data.frame(scale(all2675cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4575cc24 $RAD<- as.data.frame(scale(all4575cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8575cc24 $RAD<- as.data.frame(scale(all8575cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all2680cc24 $RAD <- as.data.frame(scale(all2680cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all4580cc24 $RAD <- as.data.frame(scale(all4580cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
all8580cc24 $RAD <- as.data.frame(scale(all8580cc24 $RAD, center=centerRAD24,scale=scaleRAD24),col.names="RAD")[, 1]
#Elevation
all2625cc24 $Elevation<- as.data.frame(scale(all2625cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4525cc24 $Elevation<- as.data.frame(scale(all4525cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8525cc24 $Elevation<- as.data.frame(scale(all8525cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all2630cc24 $Elevation <- as.data.frame(scale(all2630cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4530cc24 $Elevation <- as.data.frame(scale(all4530cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8530cc24 $Elevation <- as.data.frame(scale(all8530cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all2635cc24 $Elevation<- as.data.frame(scale(all2635cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4535cc24 $Elevation<- as.data.frame(scale(all4535cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8535cc24 $Elevation<- as.data.frame(scale(all8535cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all2640cc24 $Elevation <- as.data.frame(scale(all2640cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4540cc24 $Elevation <- as.data.frame(scale(all4540cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8540cc24 $Elevation <- as.data.frame(scale(all8540cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all2645cc24 $Elevation<- as.data.frame(scale(all2645cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4545cc24 $Elevation<- as.data.frame(scale(all4545cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8545cc24 $Elevation<- as.data.frame(scale(all8545cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all2650cc24 $Elevation <- as.data.frame(scale(all2650cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4550cc24 $Elevation <- as.data.frame(scale(all4550cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8550cc24 $Elevation <- as.data.frame(scale(all8550cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all2655cc24 $Elevation<- as.data.frame(scale(all2655cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4555cc24 $Elevation<- as.data.frame(scale(all4555cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8555cc24 $Elevation<- as.data.frame(scale(all8555cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all2660cc24 $Elevation <- as.data.frame(scale(all2660cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4560cc24 $Elevation <- as.data.frame(scale(all4560cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8560cc24 $Elevation <- as.data.frame(scale(all8560cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all2665cc24 $Elevation<- as.data.frame(scale(all2665cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4565cc24 $Elevation<- as.data.frame(scale(all4565cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8565cc24 $Elevation<- as.data.frame(scale(all8565cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all2670cc24 $Elevation <- as.data.frame(scale(all2670cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4570cc24 $Elevation <- as.data.frame(scale(all4570cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8570cc24 $Elevation <- as.data.frame(scale(all8570cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all2675cc24 $Elevation<- as.data.frame(scale(all2675cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4575cc24 $Elevation<- as.data.frame(scale(all4575cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8575cc24 $Elevation<- as.data.frame(scale(all8575cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all2680cc24 $Elevation <- as.data.frame(scale(all2680cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all4580cc24 $Elevation <- as.data.frame(scale(all4580cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
all8580cc24 $Elevation <- as.data.frame(scale(all8580cc24 $Elevation, center=centerElevation24,scale=scaleElevation24),col.names="Elevation")[, 1]
#basal area
all2625cc24$BA <- (all2625cc24$BA)+(all2625cc24$AvgBADeadRate)*6
all4525cc24$BA <- (all4525cc24$BA)+(all4525cc24$AvgBADeadRate)*6
all8525cc24$BA <- (all8525cc24$BA)+(all8525cc24$AvgBADeadRate)*6
all2630cc24$BA <- (all2630cc24$BA)+(all2630cc24$AvgBADeadRate)*11
all4530cc24$BA <- (all4530cc24$BA)+(all4530cc24$AvgBADeadRate)*11
all8530cc24$BA <- (all8530cc24$BA)+(all8530cc24$AvgBADeadRate)*11
all2635cc24$BA <- (all2635cc24$BA)+(all2635cc24$AvgBADeadRate)*16
all4535cc24$BA <-  (all4535cc24$BA)+(all4535cc24$AvgBADeadRate)*16
all8535cc24$BA <- (all8535cc24$BA)+(all8535cc24$AvgBADeadRate)*16
all2640cc24$BA <- (all2640cc24$BA)+(all2640cc24$AvgBADeadRate)*21
all4540cc24$BA <- (all4540cc24$BA)+(all4540cc24$AvgBADeadRate)*21
all8540cc24$BA <-  (all8540cc24$BA)+(all8540cc24$AvgBADeadRate)*21
all2645cc24$BA <- (all2645cc24$BA)+(all2645cc24$AvgBADeadRate)*26
all4545cc24$BA <- (all4545cc24$BA)+(all4545cc24$AvgBADeadRate)*26
all8545cc24$BA <- (all8545cc24$BA)+(all8545cc24$AvgBADeadRate)*26
all2650cc24$BA <- (all2650cc24$BA)+(all2650cc24$AvgBADeadRate)*31
all4550cc24$BA <- (all4550cc24$BA)+(all4550cc24$AvgBADeadRate)*31
all8550cc24$BA <- (all8550cc24$BA)+(all8550cc24$AvgBADeadRate)*31
all2655cc24$BA <-(all2655cc24$BA)+(all2655cc24$AvgBADeadRate)*36
all4555cc24$BA <- (all4555cc24$BA)+(all4555cc24$AvgBADeadRate)*36
all8555cc24$BA <- (all8555cc24$BA)+(all8555cc24$AvgBADeadRate)*36
all2660cc24$BA <-(all2660cc24$BA)+(all2660cc24$AvgBADeadRate)*41
all4560cc24$BA <- (all4560cc24$BA)+(all4560cc24$AvgBADeadRate)*41
all8560cc24$BA <- (all8560cc24$BA)+(all8560cc24$AvgBADeadRate)*41
all2665cc24$BA <- (all2665cc24$BA)+(all2665cc24$AvgBADeadRate)*46
all4565cc24$BA <- (all4565cc24$BA)+(all4565cc24$AvgBADeadRate)*46
all8565cc24$BA <- (all8565cc24$BA)+(all8565cc24$AvgBADeadRate)*46
all2670cc24$BA <- (all2670cc24$BA)+(all2670cc24$AvgBADeadRate)*51
all4570cc24$BA <- (all4570cc24$BA)+(all4570cc24$AvgBADeadRate)*51
all8570cc24$BA <- (all8570cc24$BA)+(all8570cc24$AvgBADeadRate)*51
all2675cc24$BA <- (all2675cc24$BA)+(all2675cc24$AvgBADeadRate)*56
all4575cc24$BA <- (all4575cc24$BA)+(all4575cc24$AvgBADeadRate)*56
all8575cc24$BA <- (all8575cc24$BA)+(all8575cc24$AvgBADeadRate)*56
all2680cc24$BA <- (all2680cc24$BA)+(all2680cc24$AvgBADeadRate)*61
all4580cc24$BA <- (all4580cc24$BA)+(all4580cc24$AvgBADeadRate)*61
all8580cc24$BA <- (all8580cc24$BA)+(all8580cc24$AvgBADeadRate)*61

#remove negatives
all2625cc24$BA[all2625cc24$BA<=0] <- 0.01
all4525cc24$BA[all4525cc24$BA<=0] <- 0.01
all8525cc24$BA[all8525cc24$BA<=0] <- 0.01
all2630cc24$BA[all2630cc24$BA<=0] <- 0.01
all4530cc24$BA[all4530cc24$BA<=0] <- 0.01
all8530cc24$BA[all8530cc24$BA<=0] <- 0.01
all2635cc24$BA[all2635cc24$BA<=0] <- 0.01
all4535cc24$BA[all4535cc24$BA<=0] <- 0.01
all8535cc24$BA[all8535cc24$BA<=0] <- 0.01
all2640cc24$BA[all2640cc24$BA<=0] <- 0.01
all4540cc24$BA[all4540cc24$BA<=0] <- 0.01
all8540cc24$BA[all8540cc24$BA<=0] <- 0.01
all2645cc24$BA[all2645cc24$BA<=0] <- 0.01
all4545cc24$BA[all4545cc24$BA<=0] <- 0.01
all8545cc24$BA[all8545cc24$BA<=0] <- 0.01
all2650cc24$BA[all2650cc24$BA<=0] <- 0.01
all4550cc24$BA[all4550cc24$BA<=0] <- 0.01
all8550cc24$BA[all8550cc24$BA<=0] <- 0.01
all2655cc24$BA[all2655cc24$BA<=0] <- 0.01
all4555cc24$BA[all4555cc24$BA<=0] <- 0.01
all8555cc24$BA[all8555cc24$BA<=0] <- 0.01
all2660cc24$BA[all2660cc24$BA<=0] <- 0.01
all4560cc24$BA[all4560cc24$BA<=0] <- 0.01
all8560cc24$BA[all8560cc24$BA<=0] <- 0.01
all2665cc24$BA[all2665cc24$BA<=0] <- 0.01
all4565cc24$BA[all4565cc24$BA<=0] <- 0.01
all8565cc24$BA[all8565cc24$BA<=0] <- 0.01
all2670cc24$BA[all2670cc24$BA<=0] <- 0.01
all4570cc24$BA[all4570cc24$BA<=0] <- 0.01
all8570cc24$BA[all8570cc24$BA<=0] <- 0.01
all2675cc24$BA[all2675cc24$BA<=0] <- 0.01
all4575cc24$BA[all4575cc24$BA<=0] <- 0.01
all8575cc24$BA[all8575cc24$BA<=0] <- 0.01
all2680cc24$BA[all2680cc24$BA<=0] <- 0.01
all4580cc24$BA[all4580cc24$BA<=0] <- 0.01
all8580cc24$BA[all8580cc24$BA<=0] <- 0.01

#max basal area of dead trees = basal area of all trees. prevents mortality over 100%, not possible.
all2625cc24$BA <- ifelse(all2625cc24$BA > all2625cc24$BAAll, all2625cc24$BAAll, all2625cc24$BA)
all4525cc24$BA <- ifelse(all4525cc24$BA > all4525cc24$BAAll, all4525cc24$BAAll, all4525cc24$BA)
all8525cc24$BA <- ifelse(all8525cc24$BA > all8525cc24$BAAll, all8525cc24$BAAll, all8525cc24$BA)
all2630cc24$BA <- ifelse(all2630cc24$BA > all2630cc24$BAAll, all2630cc24$BAAll, all2630cc24$BA)
all4530cc24$BA <- ifelse(all4530cc24$BA > all4530cc24$BAAll, all4530cc24$BAAll, all4530cc24$BA)
all8530cc24$BA <- ifelse(all8530cc24$BA > all8530cc24$BAAll, all8530cc24$BAAll, all8530cc24$BA)
all2635cc24$BA <- ifelse(all2635cc24$BA > all2635cc24$BAAll, all2635cc24$BAAll, all2635cc24$BA)
all4535cc24$BA <- ifelse(all4535cc24$BA > all4535cc24$BAAll, all4535cc24$BAAll, all4535cc24$BA)
all8535cc24$BA <- ifelse(all8535cc24$BA > all8535cc24$BAAll, all8535cc24$BAAll, all8535cc24$BA)
all2640cc24$BA <- ifelse(all2640cc24$BA > all2640cc24$BAAll, all2640cc24$BAAll, all2640cc24$BA)
all4540cc24$BA <- ifelse(all4540cc24$BA > all4540cc24$BAAll, all4540cc24$BAAll, all4540cc24$BA)
all8540cc24$BA <- ifelse(all8540cc24$BA > all8540cc24$BAAll, all8540cc24$BAAll, all8540cc24$BA)
all2645cc24$BA <- ifelse(all2645cc24$BA > all2645cc24$BAAll, all2645cc24$BAAll, all2645cc24$BA)
all4545cc24$BA <- ifelse(all4545cc24$BA > all4545cc24$BAAll, all4545cc24$BAAll, all4545cc24$BA)
all8545cc24$BA <- ifelse(all8545cc24$BA > all8545cc24$BAAll, all8545cc24$BAAll, all8545cc24$BA)
all2650cc24$BA <- ifelse(all2650cc24$BA > all2650cc24$BAAll, all2650cc24$BAAll, all2650cc24$BA)
all4550cc24$BA <- ifelse(all4550cc24$BA > all4550cc24$BAAll, all4550cc24$BAAll, all4550cc24$BA)
all8550cc24$BA <- ifelse(all8550cc24$BA > all8550cc24$BAAll, all8550cc24$BAAll, all8550cc24$BA)
all2655cc24$BA <- ifelse(all2655cc24$BA > all2655cc24$BAAll, all2655cc24$BAAll, all2655cc24$BA)
all4555cc24$BA <- ifelse(all4555cc24$BA > all4555cc24$BAAll, all4555cc24$BAAll, all4555cc24$BA)
all8555cc24$BA <- ifelse(all8555cc24$BA > all8555cc24$BAAll, all8555cc24$BAAll, all8555cc24$BA)
all2660cc24$BA <- ifelse(all2660cc24$BA > all2660cc24$BAAll, all2660cc24$BAAll, all2660cc24$BA)
all4560cc24$BA <- ifelse(all4560cc24$BA > all4560cc24$BAAll, all4560cc24$BAAll, all4560cc24$BA)
all8560cc24$BA <- ifelse(all8560cc24$BA > all8560cc24$BAAll, all8560cc24$BAAll, all8560cc24$BA)
all2665cc24$BA <- ifelse(all2665cc24$BA > all2665cc24$BAAll, all2665cc24$BAAll, all2665cc24$BA)
all4565cc24$BA <- ifelse(all4565cc24$BA > all4565cc24$BAAll, all4565cc24$BAAll, all4565cc24$BA)
all8565cc24$BA <- ifelse(all8565cc24$BA > all8565cc24$BAAll, all8565cc24$BAAll, all8565cc24$BA)
all2670cc24$BA <- ifelse(all2670cc24$BA > all2670cc24$BAAll, all2670cc24$BAAll, all2670cc24$BA)
all4570cc24$BA <- ifelse(all4570cc24$BA > all4570cc24$BAAll, all4570cc24$BAAll, all4570cc24$BA)
all8570cc24$BA <- ifelse(all8570cc24$BA > all8570cc24$BAAll, all8570cc24$BAAll, all8570cc24$BA)
all2675cc24$BA <- ifelse(all2675cc24$BA > all2675cc24$BAAll, all2675cc24$BAAll, all2675cc24$BA)
all4575cc24$BA <- ifelse(all4575cc24$BA > all4575cc24$BAAll, all4575cc24$BAAll, all4575cc24$BA)
all8575cc24$BA <- ifelse(all8575cc24$BA > all8575cc24$BAAll, all8575cc24$BAAll, all8575cc24$BA)
all2680cc24$BA <- ifelse(all2680cc24$BA > all2680cc24$BAAll, all2680cc24$BAAll, all2680cc24$BA)
all4580cc24$BA <- ifelse(all4580cc24$BA > all4580cc24$BAAll, all4580cc24$BAAll, all4580cc24$BA)
all8580cc24$BA <- ifelse(all8580cc24$BA > all8580cc24$BAAll, all8580cc24$BAAll, all8580cc24$BA)

all2625cc24 $BA<- as.data.frame(scale(all2625cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4525cc24 $BA<- as.data.frame(scale(all4525cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8525cc24 $BA<- as.data.frame(scale(all8525cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all2630cc24 $BA <- as.data.frame(scale(all2630cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4530cc24 $BA <- as.data.frame(scale(all4530cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8530cc24 $BA <- as.data.frame(scale(all8530cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all2635cc24 $BA<- as.data.frame(scale(all2635cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4535cc24 $BA<- as.data.frame(scale(all4535cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8535cc24 $BA<- as.data.frame(scale(all8535cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all2640cc24 $BA <- as.data.frame(scale(all2640cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4540cc24 $BA <- as.data.frame(scale(all4540cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8540cc24 $BA <- as.data.frame(scale(all8540cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all2645cc24 $BA<- as.data.frame(scale(all2645cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4545cc24 $BA<- as.data.frame(scale(all4545cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8545cc24 $BA<- as.data.frame(scale(all8545cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all2650cc24 $BA <- as.data.frame(scale(all2650cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4550cc24 $BA <- as.data.frame(scale(all4550cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8550cc24 $BA <- as.data.frame(scale(all8550cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all2655cc24 $BA<- as.data.frame(scale(all2655cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4555cc24 $BA<- as.data.frame(scale(all4555cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8555cc24 $BA<- as.data.frame(scale(all8555cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all2660cc24 $BA <- as.data.frame(scale(all2660cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4560cc24 $BA <- as.data.frame(scale(all4560cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8560cc24 $BA <- as.data.frame(scale(all8560cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all2665cc24 $BA<- as.data.frame(scale(all2665cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4565cc24 $BA<- as.data.frame(scale(all4565cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8565cc24 $BA<- as.data.frame(scale(all8565cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all2670cc24 $BA <- as.data.frame(scale(all2670cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4570cc24 $BA <- as.data.frame(scale(all4570cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8570cc24 $BA <- as.data.frame(scale(all8570cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all2675cc24 $BA<- as.data.frame(scale(all2675cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4575cc24 $BA<- as.data.frame(scale(all4575cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8575cc24 $BA<- as.data.frame(scale(all8575cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all2680cc24 $BA <- as.data.frame(scale(all2680cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all4580cc24 $BA <- as.data.frame(scale(all4580cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]
all8580cc24 $BA <- as.data.frame(scale(all8580cc24 $BA, center=centerBA24,scale=scaleBA24),col.names="BA")[, 1]

all2625cc24$time_weight <- 2.152608
all4525cc24$time_weight <- 2.152608
all8525cc24$time_weight <- 2.152608
all2630cc24$time_weight <- 2.152608
all4530cc24$time_weight <- 2.152608
all8530cc24$time_weight <- 2.152608
all2635cc24$time_weight <- 2.152608
all4535cc24$time_weight <- 2.152608
all8535cc24$time_weight <- 2.152608
all2640cc24$time_weight <- 2.152608
all4540cc24$time_weight <- 2.152608
all8540cc24$time_weight <- 2.152608
all2645cc24$time_weight <- 2.152608
all4545cc24$time_weight <- 2.152608
all8545cc24$time_weight <- 2.152608
all2650cc24$time_weight <- 2.152608
all4550cc24$time_weight <- 2.152608
all8550cc24$time_weight <- 2.152608
all2655cc24$time_weight <- 2.152608
all4555cc24$time_weight <- 2.152608
all8555cc24$time_weight <- 2.152608
all2660cc24$time_weight <- 2.152608
all4560cc24$time_weight <- 2.152608
all8560cc24$time_weight <- 2.152608
all2665cc24$time_weight <- 2.152608
all4565cc24$time_weight <- 2.152608
all8565cc24$time_weight <- 2.152608
all2670cc24$time_weight <- 2.152608
all4570cc24$time_weight <- 2.152608
all8570cc24$time_weight <- 2.152608
all2675cc24$time_weight <- 2.152608
all4575cc24$time_weight <- 2.152608
all8575cc24$time_weight <- 2.152608
all2680cc24$time_weight <- 2.152608
all4580cc24$time_weight <- 2.152608
all8580cc24$time_weight <- 2.152608

#run simulations
simCarb2625 <- link(CarbSplit24, data=all2625cc24 )
simmeanCarb2625 <- data.frame(apply(simCarb2625,2,mean))
PICarb2625 <- t(data.frame(apply(simCarb2625,2,PI,prob=0.89)))
F24CarbSim2625 <- data.frame(cbind(simmeanCarb2625,PICarb2625))
F24CarbSim2625$PlotCN <- all2625cc24  $PlotCN
F24CarbSim2625 <- F24CarbSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2625) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4525 <- link(CarbSplit24, data=all4525cc24 )
simmeanCarb4525 <- data.frame(apply(simCarb4525,2,mean))
PICarb4525 <- t(data.frame(apply(simCarb4525,2,PI,prob=0.89)))
F24CarbSim4525 <- data.frame(cbind(simmeanCarb4525,PICarb4525))
F24CarbSim4525$PlotCN <- all4525cc24  $PlotCN
F24CarbSim4525 <- F24CarbSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4525) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8525 <- link(CarbSplit24, data=all8525cc24 )
simmeanCarb8525 <- data.frame(apply(simCarb8525,2,mean))
PICarb8525 <- t(data.frame(apply(simCarb8525,2,PI,prob=0.89)))
F24CarbSim8525 <- data.frame(cbind(simmeanCarb8525,PICarb8525))
F24CarbSim8525$PlotCN <- all8525cc24  $PlotCN
F24CarbSim8525 <- F24CarbSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8525) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2630 <- link(CarbSplit24, data=all2630cc24 )
simmeanCarb2630 <- data.frame(apply(simCarb2630,2,mean))
PICarb2630 <- t(data.frame(apply(simCarb2630,2,PI,prob=0.89)))
F24CarbSim2630 <- data.frame(cbind(simmeanCarb2630,PICarb2630))
F24CarbSim2630$PlotCN <- all2630cc24  $PlotCN
F24CarbSim2630 <- F24CarbSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2630) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4530 <- link(CarbSplit24, data=all4530cc24 )
simmeanCarb4530 <- data.frame(apply(simCarb4530,2,mean))
PICarb4530 <- t(data.frame(apply(simCarb4530,2,PI,prob=0.89)))
F24CarbSim4530 <- data.frame(cbind(simmeanCarb4530,PICarb4530))
F24CarbSim4530$PlotCN <- all4530cc24  $PlotCN
F24CarbSim4530 <- F24CarbSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4530) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8530 <- link(CarbSplit24, data=all8530cc24 )
simmeanCarb8530 <- data.frame(apply(simCarb8530,2,mean))
PICarb8530 <- t(data.frame(apply(simCarb8530,2,PI,prob=0.89)))
F24CarbSim8530 <- data.frame(cbind(simmeanCarb8530,PICarb8530))
F24CarbSim8530$PlotCN <- all8530cc24  $PlotCN
F24CarbSim8530 <- F24CarbSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8530) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2635 <- link(CarbSplit24, data=all2635cc24 )
simmeanCarb2635 <- data.frame(apply(simCarb2635,2,mean))
PICarb2635 <- t(data.frame(apply(simCarb2635,2,PI,prob=0.89)))
F24CarbSim2635 <- data.frame(cbind(simmeanCarb2635,PICarb2635))
F24CarbSim2635$PlotCN <- all2635cc24  $PlotCN
F24CarbSim2635 <- F24CarbSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2635) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4535 <- link(CarbSplit24, data=all4535cc24 )
simmeanCarb4535 <- data.frame(apply(simCarb4535,2,mean))
PICarb4535 <- t(data.frame(apply(simCarb4535,2,PI,prob=0.89)))
F24CarbSim4535 <- data.frame(cbind(simmeanCarb4535,PICarb4535))
F24CarbSim4535$PlotCN <- all4535cc24  $PlotCN
F24CarbSim4535 <- F24CarbSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4535) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8535 <- link(CarbSplit24, data=all8535cc24 )
simmeanCarb8535 <- data.frame(apply(simCarb8535,2,mean))
PICarb8535 <- t(data.frame(apply(simCarb8535,2,PI,prob=0.89)))
F24CarbSim8535 <- data.frame(cbind(simmeanCarb8535,PICarb8535))
F24CarbSim8535$PlotCN <- all8535cc24  $PlotCN
F24CarbSim8535 <- F24CarbSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8535) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2640 <- link(CarbSplit24, data=all2640cc24 )
simmeanCarb2640 <- data.frame(apply(simCarb2640,2,mean))
PICarb2640 <- t(data.frame(apply(simCarb2640,2,PI,prob=0.89)))
F24CarbSim2640 <- data.frame(cbind(simmeanCarb2640,PICarb2640))
F24CarbSim2640$PlotCN <- all2640cc24  $PlotCN
F24CarbSim2640 <- F24CarbSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2640) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4540 <- link(CarbSplit24, data=all4540cc24 )
simmeanCarb4540 <- data.frame(apply(simCarb4540,2,mean))
PICarb4540 <- t(data.frame(apply(simCarb4540,2,PI,prob=0.89)))
F24CarbSim4540 <- data.frame(cbind(simmeanCarb4540,PICarb4540))
F24CarbSim4540$PlotCN <- all4540cc24  $PlotCN
F24CarbSim4540 <- F24CarbSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4540) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8540 <- link(CarbSplit24, data=all8540cc24 )
simmeanCarb8540 <- data.frame(apply(simCarb8540,2,mean))
PICarb8540 <- t(data.frame(apply(simCarb8540,2,PI,prob=0.89)))
F24CarbSim8540 <- data.frame(cbind(simmeanCarb8540,PICarb8540))
F24CarbSim8540$PlotCN <- all8540cc24  $PlotCN
F24CarbSim8540 <- F24CarbSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8540) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2645 <- link(CarbSplit24, data=all2645cc24 )
simmeanCarb2645 <- data.frame(apply(simCarb2645,2,mean))
PICarb2645 <- t(data.frame(apply(simCarb2645,2,PI,prob=0.89)))
F24CarbSim2645 <- data.frame(cbind(simmeanCarb2645,PICarb2645))
F24CarbSim2645$PlotCN <- all2645cc24  $PlotCN
F24CarbSim2645 <- F24CarbSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2645) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4545 <- link(CarbSplit24, data=all4545cc24 )
simmeanCarb4545 <- data.frame(apply(simCarb4545,2,mean))
PICarb4545 <- t(data.frame(apply(simCarb4545,2,PI,prob=0.89)))
F24CarbSim4545 <- data.frame(cbind(simmeanCarb4545,PICarb4545))
F24CarbSim4545$PlotCN <- all4545cc24  $PlotCN
F24CarbSim4545 <- F24CarbSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4545) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8545 <- link(CarbSplit24, data=all8545cc24 )
simmeanCarb8545 <- data.frame(apply(simCarb8545,2,mean))
PICarb8545 <- t(data.frame(apply(simCarb8545,2,PI,prob=0.89)))
F24CarbSim8545 <- data.frame(cbind(simmeanCarb8545,PICarb8545))
F24CarbSim8545$PlotCN <- all8545cc24  $PlotCN
F24CarbSim8545 <- F24CarbSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8545) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2650 <- link(CarbSplit24, data=all2650cc24 )
simmeanCarb2650 <- data.frame(apply(simCarb2650,2,mean))
PICarb2650 <- t(data.frame(apply(simCarb2650,2,PI,prob=0.89)))
F24CarbSim2650 <- data.frame(cbind(simmeanCarb2650,PICarb2650))
F24CarbSim2650$PlotCN <- all2650cc24  $PlotCN
F24CarbSim2650 <- F24CarbSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2650) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4550 <- link(CarbSplit24, data=all4550cc24 )
simmeanCarb4550 <- data.frame(apply(simCarb4550,2,mean))
PICarb4550 <- t(data.frame(apply(simCarb4550,2,PI,prob=0.89)))
F24CarbSim4550 <- data.frame(cbind(simmeanCarb4550,PICarb4550))
F24CarbSim4550$PlotCN <- all4550cc24  $PlotCN
F24CarbSim4550 <- F24CarbSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4550) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8550 <- link(CarbSplit24, data=all8550cc24 )
simmeanCarb8550 <- data.frame(apply(simCarb8550,2,mean))
PICarb8550 <- t(data.frame(apply(simCarb8550,2,PI,prob=0.89)))
F24CarbSim8550 <- data.frame(cbind(simmeanCarb8550,PICarb8550))
F24CarbSim8550$PlotCN <- all8550cc24  $PlotCN
F24CarbSim8550 <- F24CarbSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8550) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2655 <- link(CarbSplit24, data=all2655cc24 )
simmeanCarb2655 <- data.frame(apply(simCarb2655,2,mean))
PICarb2655 <- t(data.frame(apply(simCarb2655,2,PI,prob=0.89)))
F24CarbSim2655 <- data.frame(cbind(simmeanCarb2655,PICarb2655))
F24CarbSim2655$PlotCN <- all2655cc24  $PlotCN
F24CarbSim2655 <- F24CarbSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2655) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4555 <- link(CarbSplit24, data=all4555cc24 )
simmeanCarb4555 <- data.frame(apply(simCarb4555,2,mean))
PICarb4555 <- t(data.frame(apply(simCarb4555,2,PI,prob=0.89)))
F24CarbSim4555 <- data.frame(cbind(simmeanCarb4555,PICarb4555))
F24CarbSim4555$PlotCN <- all4555cc24  $PlotCN
F24CarbSim4555 <- F24CarbSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4555) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8555 <- link(CarbSplit24, data=all8555cc24 )
simmeanCarb8555 <- data.frame(apply(simCarb8555,2,mean))
PICarb8555 <- t(data.frame(apply(simCarb8555,2,PI,prob=0.89)))
F24CarbSim8555 <- data.frame(cbind(simmeanCarb8555,PICarb8555))
F24CarbSim8555$PlotCN <- all8555cc24  $PlotCN
F24CarbSim8555 <- F24CarbSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8555) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2660 <- link(CarbSplit24, data=all2660cc24 )
simmeanCarb2660 <- data.frame(apply(simCarb2660,2,mean))
PICarb2660 <- t(data.frame(apply(simCarb2660,2,PI,prob=0.89)))
F24CarbSim2660 <- data.frame(cbind(simmeanCarb2660,PICarb2660))
F24CarbSim2660$PlotCN <- all2660cc24  $PlotCN
F24CarbSim2660 <- F24CarbSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2660) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4560 <- link(CarbSplit24, data=all4560cc24 )
simmeanCarb4560 <- data.frame(apply(simCarb4560,2,mean))
PICarb4560 <- t(data.frame(apply(simCarb4560,2,PI,prob=0.89)))
F24CarbSim4560 <- data.frame(cbind(simmeanCarb4560,PICarb4560))
F24CarbSim4560$PlotCN <- all4560cc24  $PlotCN
F24CarbSim4560 <- F24CarbSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4560) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8560 <- link(CarbSplit24, data=all8560cc24 )
simmeanCarb8560 <- data.frame(apply(simCarb8560,2,mean))
PICarb8560 <- t(data.frame(apply(simCarb8560,2,PI,prob=0.89)))
F24CarbSim8560 <- data.frame(cbind(simmeanCarb8560,PICarb8560))
F24CarbSim8560$PlotCN <- all8560cc24  $PlotCN
F24CarbSim8560 <- F24CarbSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8560) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2665 <- link(CarbSplit24, data=all2665cc24 )
simmeanCarb2665 <- data.frame(apply(simCarb2665,2,mean))
PICarb2665 <- t(data.frame(apply(simCarb2665,2,PI,prob=0.89)))
F24CarbSim2665 <- data.frame(cbind(simmeanCarb2665,PICarb2665))
F24CarbSim2665$PlotCN <- all2665cc24  $PlotCN
F24CarbSim2665 <- F24CarbSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2665) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4565 <- link(CarbSplit24, data=all4565cc24 )
simmeanCarb4565 <- data.frame(apply(simCarb4565,2,mean))
PICarb4565 <- t(data.frame(apply(simCarb4565,2,PI,prob=0.89)))
F24CarbSim4565 <- data.frame(cbind(simmeanCarb4565,PICarb4565))
F24CarbSim4565$PlotCN <- all4565cc24  $PlotCN
F24CarbSim4565 <- F24CarbSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4565) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8565 <- link(CarbSplit24, data=all8565cc24 )
simmeanCarb8565 <- data.frame(apply(simCarb8565,2,mean))
PICarb8565 <- t(data.frame(apply(simCarb8565,2,PI,prob=0.89)))
F24CarbSim8565 <- data.frame(cbind(simmeanCarb8565,PICarb8565))
F24CarbSim8565$PlotCN <- all8565cc24  $PlotCN
F24CarbSim8565 <- F24CarbSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8565) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2670 <- link(CarbSplit24, data=all2670cc24 )
simmeanCarb2670 <- data.frame(apply(simCarb2670,2,mean))
PICarb2670 <- t(data.frame(apply(simCarb2670,2,PI,prob=0.89)))
F24CarbSim2670 <- data.frame(cbind(simmeanCarb2670,PICarb2670))
F24CarbSim2670$PlotCN <- all2670cc24  $PlotCN
F24CarbSim2670 <- F24CarbSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2670) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4570 <- link(CarbSplit24, data=all4570cc24 )
simmeanCarb4570 <- data.frame(apply(simCarb4570,2,mean))
PICarb4570 <- t(data.frame(apply(simCarb4570,2,PI,prob=0.89)))
F24CarbSim4570 <- data.frame(cbind(simmeanCarb4570,PICarb4570))
F24CarbSim4570$PlotCN <- all4570cc24  $PlotCN
F24CarbSim4570 <- F24CarbSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4570) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8570 <- link(CarbSplit24, data=all8570cc24 )
simmeanCarb8570 <- data.frame(apply(simCarb8570,2,mean))
PICarb8570 <- t(data.frame(apply(simCarb8570,2,PI,prob=0.89)))
F24CarbSim8570 <- data.frame(cbind(simmeanCarb8570,PICarb8570))
F24CarbSim8570$PlotCN <- all8570cc24  $PlotCN
F24CarbSim8570 <- F24CarbSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8570) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2675 <- link(CarbSplit24, data=all2675cc24 )
simmeanCarb2675 <- data.frame(apply(simCarb2675,2,mean))
PICarb2675 <- t(data.frame(apply(simCarb2675,2,PI,prob=0.89)))
F24CarbSim2675 <- data.frame(cbind(simmeanCarb2675,PICarb2675))
F24CarbSim2675$PlotCN <- all2675cc24  $PlotCN
F24CarbSim2675 <- F24CarbSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2675) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4575 <- link(CarbSplit24, data=all4575cc24 )
simmeanCarb4575 <- data.frame(apply(simCarb4575,2,mean))
PICarb4575 <- t(data.frame(apply(simCarb4575,2,PI,prob=0.89)))
F24CarbSim4575 <- data.frame(cbind(simmeanCarb4575,PICarb4575))
F24CarbSim4575$PlotCN <- all4575cc24  $PlotCN
F24CarbSim4575 <- F24CarbSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4575) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8575 <- link(CarbSplit24, data=all8575cc24 )
simmeanCarb8575 <- data.frame(apply(simCarb8575,2,mean))
PICarb8575 <- t(data.frame(apply(simCarb8575,2,PI,prob=0.89)))
F24CarbSim8575 <- data.frame(cbind(simmeanCarb8575,PICarb8575))
F24CarbSim8575$PlotCN <- all8575cc24  $PlotCN
F24CarbSim8575 <- F24CarbSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8575) <- c("PlotCN","CarbPerAcre","5CI","95CI")

simCarb2680 <- link(CarbSplit24, data=all2680cc24 )
simmeanCarb2680 <- data.frame(apply(simCarb2680,2,mean))
PICarb2680 <- t(data.frame(apply(simCarb2680,2,PI,prob=0.89)))
F24CarbSim2680 <- data.frame(cbind(simmeanCarb2680,PICarb2680))
F24CarbSim2680$PlotCN <- all2680cc24  $PlotCN
F24CarbSim2680 <- F24CarbSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim2680) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb4580 <- link(CarbSplit24, data=all4580cc24 )
simmeanCarb4580 <- data.frame(apply(simCarb4580,2,mean))
PICarb4580 <- t(data.frame(apply(simCarb4580,2,PI,prob=0.89)))
F24CarbSim4580 <- data.frame(cbind(simmeanCarb4580,PICarb4580))
F24CarbSim4580$PlotCN <- all4580cc24  $PlotCN
F24CarbSim4580 <- F24CarbSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim4580) <- c("PlotCN","CarbPerAcre","5CI","95CI")
simCarb8580 <- link(CarbSplit24, data=all8580cc24 )
simmeanCarb8580 <- data.frame(apply(simCarb8580,2,mean))
PICarb8580 <- t(data.frame(apply(simCarb8580,2,PI,prob=0.89)))
F24CarbSim8580 <- data.frame(cbind(simmeanCarb8580,PICarb8580))
F24CarbSim8580$PlotCN <- all8580cc24  $PlotCN
F24CarbSim8580 <- F24CarbSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F24CarbSim8580) <- c("PlotCN","CarbPerAcre","5CI","95CI")

#create vector of new colnames
CarbSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Carb", "5CI", "95CI"), x)))
CarbSimnames<- append(CarbSimnames,"PlotCN",after=0)

#group data by RCP pathway
F24CarbSim26T <- F24CarbSim2625 %>% cbind(F24CarbSim2630[,2:4]) %>%
  cbind(F24CarbSim2635[,2:4]) %>% cbind(F24CarbSim2640[,2:4]) %>% cbind(F24CarbSim2645[,2:4]) %>%
  cbind(F24CarbSim2650[,2:4]) %>% cbind(F24CarbSim2655[,2:4]) %>% cbind(F24CarbSim2660[,2:4]) %>%
  cbind(F24CarbSim2665[,2:4]) %>% cbind(F24CarbSim2670[,2:4]) %>% cbind(F24CarbSim2675[,2:4]) %>%
  cbind(F24CarbSim2680[,2:4])
colnames(F24CarbSim26T) <- CarbSimnames
write.csv(F24CarbSim26T,file="F24CarbPred26T.csv")

F24CarbSim45T <- F24CarbSim4525 %>% cbind(F24CarbSim4530[,2:4]) %>%
  cbind(F24CarbSim4535[,2:4]) %>% cbind(F24CarbSim4540[,2:4]) %>% cbind(F24CarbSim4545[,2:4]) %>%
  cbind(F24CarbSim4550[,2:4]) %>% cbind(F24CarbSim4555[,2:4]) %>% cbind(F24CarbSim4560[,2:4]) %>%
  cbind(F24CarbSim4565[,2:4]) %>% cbind(F24CarbSim4570[,2:4]) %>% cbind(F24CarbSim4575[,2:4]) %>%
  cbind(F24CarbSim4580[,2:4])
colnames(F24CarbSim45T) <- CarbSimnames
write.csv(F24CarbSim45T,file="F24CarbPred45T.csv")

F24CarbSim85T <- F24CarbSim8525 %>% cbind(F24CarbSim8530[,2:4]) %>%
  cbind(F24CarbSim8535[,2:4]) %>% cbind(F24CarbSim8540[,2:4]) %>% cbind(F24CarbSim8545[,2:4]) %>%
  cbind(F24CarbSim8550[,2:4]) %>% cbind(F24CarbSim8555[,2:4]) %>% cbind(F24CarbSim8560[,2:4]) %>%
  cbind(F24CarbSim8565[,2:4]) %>% cbind(F24CarbSim8570[,2:4]) %>% cbind(F24CarbSim8575[,2:4]) %>%
  cbind(F24CarbSim8580[,2:4])
colnames(F24CarbSim85T) <- CarbSimnames
write.csv(F24CarbSim85T,file="F24CarbPred85T.csv")

#remove extra dfs, save memory
rm(all2625cc,all2630cc,all2635cc,all2640cc,all2645cc,all2650cc,all2655cc,all2660cc,all2665cc,all2670cc,all2675cc,all2680cc)
rm(all4525cc,all4530cc,all4535cc,all4540cc,all4545cc,all4550cc,all4555cc,all4560cc,all4565cc,all4570cc,all4575cc,all4580cc)
rm(all8525cc,all8530cc,all8535cc,all8540cc,all8545cc,all8550cc,all8555cc,all8560cc,all8565cc,all8570cc,all8575cc,all8580cc)


#Run for Hill Models
#fgroup 1
simHill2625 <- link(HillSplit1_G, data=all2625cc1)
simmeanHill2625 <- data.frame(apply(simHill2625,2,mean))
PIHill2625 <- t(data.frame(apply(simHill2625,2,PI,prob=0.89)))
F1HillSim2625 <- data.frame(cbind(simmeanHill2625,PIHill2625))
F1HillSim2625$PlotCN <- all2625cc1 $PlotCN
F1HillSim2625 <- F1HillSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2625) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4525 <- link(HillSplit1_G, data=all4525cc1)
simmeanHill4525 <- data.frame(apply(simHill4525,2,mean))
PIHill4525 <- t(data.frame(apply(simHill4525,2,PI,prob=0.89)))
F1HillSim4525 <- data.frame(cbind(simmeanHill4525,PIHill4525))
F1HillSim4525$PlotCN <- all4525cc1 $PlotCN
F1HillSim4525 <- F1HillSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4525) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8525 <- link(HillSplit1_G, data=all8525cc1)
simmeanHill8525 <- data.frame(apply(simHill8525,2,mean))
PIHill8525 <- t(data.frame(apply(simHill8525,2,PI,prob=0.89)))
F1HillSim8525 <- data.frame(cbind(simmeanHill8525,PIHill8525))
F1HillSim8525$PlotCN <- all8525cc1 $PlotCN
F1HillSim8525 <- F1HillSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8525) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2630 <- link(HillSplit1_G, data=all2630cc1)
simmeanHill2630 <- data.frame(apply(simHill2630,2,mean))
PIHill2630 <- t(data.frame(apply(simHill2630,2,PI,prob=0.89)))
F1HillSim2630 <- data.frame(cbind(simmeanHill2630,PIHill2630))
F1HillSim2630$PlotCN <- all2630cc1 $PlotCN
F1HillSim2630 <- F1HillSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2630) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4530 <- link(HillSplit1_G, data=all4530cc1)
simmeanHill4530 <- data.frame(apply(simHill4530,2,mean))
PIHill4530 <- t(data.frame(apply(simHill4530,2,PI,prob=0.89)))
F1HillSim4530 <- data.frame(cbind(simmeanHill4530,PIHill4530))
F1HillSim4530$PlotCN <- all4530cc1 $PlotCN
F1HillSim4530 <- F1HillSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4530) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8530 <- link(HillSplit1_G, data=all8530cc1)
simmeanHill8530 <- data.frame(apply(simHill8530,2,mean))
PIHill8530 <- t(data.frame(apply(simHill8530,2,PI,prob=0.89)))
F1HillSim8530 <- data.frame(cbind(simmeanHill8530,PIHill8530))
F1HillSim8530$PlotCN <- all8530cc1 $PlotCN
F1HillSim8530 <- F1HillSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8530) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2635 <- link(HillSplit1_G, data=all2635cc1)
simmeanHill2635 <- data.frame(apply(simHill2635,2,mean))
PIHill2635 <- t(data.frame(apply(simHill2635,2,PI,prob=0.89)))
F1HillSim2635 <- data.frame(cbind(simmeanHill2635,PIHill2635))
F1HillSim2635$PlotCN <- all2635cc1 $PlotCN
F1HillSim2635 <- F1HillSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2635) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4535 <- link(HillSplit1_G, data=all4535cc1)
simmeanHill4535 <- data.frame(apply(simHill4535,2,mean))
PIHill4535 <- t(data.frame(apply(simHill4535,2,PI,prob=0.89)))
F1HillSim4535 <- data.frame(cbind(simmeanHill4535,PIHill4535))
F1HillSim4535$PlotCN <- all4535cc1 $PlotCN
F1HillSim4535 <- F1HillSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4535) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8535 <- link(HillSplit1_G, data=all8535cc1)
simmeanHill8535 <- data.frame(apply(simHill8535,2,mean))
PIHill8535 <- t(data.frame(apply(simHill8535,2,PI,prob=0.89)))
F1HillSim8535 <- data.frame(cbind(simmeanHill8535,PIHill8535))
F1HillSim8535$PlotCN <- all8535cc1 $PlotCN
F1HillSim8535 <- F1HillSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8535) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2640 <- link(HillSplit1_G, data=all2640cc1)
simmeanHill2640 <- data.frame(apply(simHill2640,2,mean))
PIHill2640 <- t(data.frame(apply(simHill2640,2,PI,prob=0.89)))
F1HillSim2640 <- data.frame(cbind(simmeanHill2640,PIHill2640))
F1HillSim2640$PlotCN <- all2640cc1 $PlotCN
F1HillSim2640 <- F1HillSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2640) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4540 <- link(HillSplit1_G, data=all4540cc1)
simmeanHill4540 <- data.frame(apply(simHill4540,2,mean))
PIHill4540 <- t(data.frame(apply(simHill4540,2,PI,prob=0.89)))
F1HillSim4540 <- data.frame(cbind(simmeanHill4540,PIHill4540))
F1HillSim4540$PlotCN <- all4540cc1 $PlotCN
F1HillSim4540 <- F1HillSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4540) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8540 <- link(HillSplit1_G, data=all8540cc1)
simmeanHill8540 <- data.frame(apply(simHill8540,2,mean))
PIHill8540 <- t(data.frame(apply(simHill8540,2,PI,prob=0.89)))
F1HillSim8540 <- data.frame(cbind(simmeanHill8540,PIHill8540))
F1HillSim8540$PlotCN <- all8540cc1 $PlotCN
F1HillSim8540 <- F1HillSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8540) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2645 <- link(HillSplit1_G, data=all2645cc1)
simmeanHill2645 <- data.frame(apply(simHill2645,2,mean))
PIHill2645 <- t(data.frame(apply(simHill2645,2,PI,prob=0.89)))
F1HillSim2645 <- data.frame(cbind(simmeanHill2645,PIHill2645))
F1HillSim2645$PlotCN <- all2645cc1 $PlotCN
F1HillSim2645 <- F1HillSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2645) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4545 <- link(HillSplit1_G, data=all4545cc1)
simmeanHill4545 <- data.frame(apply(simHill4545,2,mean))
PIHill4545 <- t(data.frame(apply(simHill4545,2,PI,prob=0.89)))
F1HillSim4545 <- data.frame(cbind(simmeanHill4545,PIHill4545))
F1HillSim4545$PlotCN <- all4545cc1 $PlotCN
F1HillSim4545 <- F1HillSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4545) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8545 <- link(HillSplit1_G, data=all8545cc1)
simmeanHill8545 <- data.frame(apply(simHill8545,2,mean))
PIHill8545 <- t(data.frame(apply(simHill8545,2,PI,prob=0.89)))
F1HillSim8545 <- data.frame(cbind(simmeanHill8545,PIHill8545))
F1HillSim8545$PlotCN <- all8545cc1 $PlotCN
F1HillSim8545 <- F1HillSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8545) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2650 <- link(HillSplit1_G, data=all2650cc1)
simmeanHill2650 <- data.frame(apply(simHill2650,2,mean))
PIHill2650 <- t(data.frame(apply(simHill2650,2,PI,prob=0.89)))
F1HillSim2650 <- data.frame(cbind(simmeanHill2650,PIHill2650))
F1HillSim2650$PlotCN <- all2650cc1 $PlotCN
F1HillSim2650 <- F1HillSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2650) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4550 <- link(HillSplit1_G, data=all4550cc1)
simmeanHill4550 <- data.frame(apply(simHill4550,2,mean))
PIHill4550 <- t(data.frame(apply(simHill4550,2,PI,prob=0.89)))
F1HillSim4550 <- data.frame(cbind(simmeanHill4550,PIHill4550))
F1HillSim4550$PlotCN <- all4550cc1 $PlotCN
F1HillSim4550 <- F1HillSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4550) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8550 <- link(HillSplit1_G, data=all8550cc1)
simmeanHill8550 <- data.frame(apply(simHill8550,2,mean))
PIHill8550 <- t(data.frame(apply(simHill8550,2,PI,prob=0.89)))
F1HillSim8550 <- data.frame(cbind(simmeanHill8550,PIHill8550))
F1HillSim8550$PlotCN <- all8550cc1 $PlotCN
F1HillSim8550 <- F1HillSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8550) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2655 <- link(HillSplit1_G, data=all2655cc1)
simmeanHill2655 <- data.frame(apply(simHill2655,2,mean))
PIHill2655 <- t(data.frame(apply(simHill2655,2,PI,prob=0.89)))
F1HillSim2655 <- data.frame(cbind(simmeanHill2655,PIHill2655))
F1HillSim2655$PlotCN <- all2655cc1 $PlotCN
F1HillSim2655 <- F1HillSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2655) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4555 <- link(HillSplit1_G, data=all4555cc1)
simmeanHill4555 <- data.frame(apply(simHill4555,2,mean))
PIHill4555 <- t(data.frame(apply(simHill4555,2,PI,prob=0.89)))
F1HillSim4555 <- data.frame(cbind(simmeanHill4555,PIHill4555))
F1HillSim4555$PlotCN <- all4555cc1 $PlotCN
F1HillSim4555 <- F1HillSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4555) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8555 <- link(HillSplit1_G, data=all8555cc1)
simmeanHill8555 <- data.frame(apply(simHill8555,2,mean))
PIHill8555 <- t(data.frame(apply(simHill8555,2,PI,prob=0.89)))
F1HillSim8555 <- data.frame(cbind(simmeanHill8555,PIHill8555))
F1HillSim8555$PlotCN <- all8555cc1 $PlotCN
F1HillSim8555 <- F1HillSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8555) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2660 <- link(HillSplit1_G, data=all2660cc1)
simmeanHill2660 <- data.frame(apply(simHill2660,2,mean))
PIHill2660 <- t(data.frame(apply(simHill2660,2,PI,prob=0.89)))
F1HillSim2660 <- data.frame(cbind(simmeanHill2660,PIHill2660))
F1HillSim2660$PlotCN <- all2660cc1 $PlotCN
F1HillSim2660 <- F1HillSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2660) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4560 <- link(HillSplit1_G, data=all4560cc1)
simmeanHill4560 <- data.frame(apply(simHill4560,2,mean))
PIHill4560 <- t(data.frame(apply(simHill4560,2,PI,prob=0.89)))
F1HillSim4560 <- data.frame(cbind(simmeanHill4560,PIHill4560))
F1HillSim4560$PlotCN <- all4560cc1 $PlotCN
F1HillSim4560 <- F1HillSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4560) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8560 <- link(HillSplit1_G, data=all8560cc1)
simmeanHill8560 <- data.frame(apply(simHill8560,2,mean))
PIHill8560 <- t(data.frame(apply(simHill8560,2,PI,prob=0.89)))
F1HillSim8560 <- data.frame(cbind(simmeanHill8560,PIHill8560))
F1HillSim8560$PlotCN <- all8560cc1 $PlotCN
F1HillSim8560 <- F1HillSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8560) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2665 <- link(HillSplit1_G, data=all2665cc1)
simmeanHill2665 <- data.frame(apply(simHill2665,2,mean))
PIHill2665 <- t(data.frame(apply(simHill2665,2,PI,prob=0.89)))
F1HillSim2665 <- data.frame(cbind(simmeanHill2665,PIHill2665))
F1HillSim2665$PlotCN <- all2665cc1 $PlotCN
F1HillSim2665 <- F1HillSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2665) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4565 <- link(HillSplit1_G, data=all4565cc1)
simmeanHill4565 <- data.frame(apply(simHill4565,2,mean))
PIHill4565 <- t(data.frame(apply(simHill4565,2,PI,prob=0.89)))
F1HillSim4565 <- data.frame(cbind(simmeanHill4565,PIHill4565))
F1HillSim4565$PlotCN <- all4565cc1 $PlotCN
F1HillSim4565 <- F1HillSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4565) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8565 <- link(HillSplit1_G, data=all8565cc1)
simmeanHill8565 <- data.frame(apply(simHill8565,2,mean))
PIHill8565 <- t(data.frame(apply(simHill8565,2,PI,prob=0.89)))
F1HillSim8565 <- data.frame(cbind(simmeanHill8565,PIHill8565))
F1HillSim8565$PlotCN <- all8565cc1 $PlotCN
F1HillSim8565 <- F1HillSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8565) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2670 <- link(HillSplit1_G, data=all2670cc1)
simmeanHill2670 <- data.frame(apply(simHill2670,2,mean))
PIHill2670 <- t(data.frame(apply(simHill2670,2,PI,prob=0.89)))
F1HillSim2670 <- data.frame(cbind(simmeanHill2670,PIHill2670))
F1HillSim2670$PlotCN <- all2670cc1 $PlotCN
F1HillSim2670 <- F1HillSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2670) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4570 <- link(HillSplit1_G, data=all4570cc1)
simmeanHill4570 <- data.frame(apply(simHill4570,2,mean))
PIHill4570 <- t(data.frame(apply(simHill4570,2,PI,prob=0.89)))
F1HillSim4570 <- data.frame(cbind(simmeanHill4570,PIHill4570))
F1HillSim4570$PlotCN <- all4570cc1 $PlotCN
F1HillSim4570 <- F1HillSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4570) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8570 <- link(HillSplit1_G, data=all8570cc1)
simmeanHill8570 <- data.frame(apply(simHill8570,2,mean))
PIHill8570 <- t(data.frame(apply(simHill8570,2,PI,prob=0.89)))
F1HillSim8570 <- data.frame(cbind(simmeanHill8570,PIHill8570))
F1HillSim8570$PlotCN <- all8570cc1 $PlotCN
F1HillSim8570 <- F1HillSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8570) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2675 <- link(HillSplit1_G, data=all2675cc1)
simmeanHill2675 <- data.frame(apply(simHill2675,2,mean))
PIHill2675 <- t(data.frame(apply(simHill2675,2,PI,prob=0.89)))
F1HillSim2675 <- data.frame(cbind(simmeanHill2675,PIHill2675))
F1HillSim2675$PlotCN <- all2675cc1 $PlotCN
F1HillSim2675 <- F1HillSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2675) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4575 <- link(HillSplit1_G, data=all4575cc1)
simmeanHill4575 <- data.frame(apply(simHill4575,2,mean))
PIHill4575 <- t(data.frame(apply(simHill4575,2,PI,prob=0.89)))
F1HillSim4575 <- data.frame(cbind(simmeanHill4575,PIHill4575))
F1HillSim4575$PlotCN <- all4575cc1 $PlotCN
F1HillSim4575 <- F1HillSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4575) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8575 <- link(HillSplit1_G, data=all8575cc1)
simmeanHill8575 <- data.frame(apply(simHill8575,2,mean))
PIHill8575 <- t(data.frame(apply(simHill8575,2,PI,prob=0.89)))
F1HillSim8575 <- data.frame(cbind(simmeanHill8575,PIHill8575))
F1HillSim8575$PlotCN <- all8575cc1 $PlotCN
F1HillSim8575 <- F1HillSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8575) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2680 <- link(HillSplit1_G, data=all2680cc1)
simmeanHill2680 <- data.frame(apply(simHill2680,2,mean))
PIHill2680 <- t(data.frame(apply(simHill2680,2,PI,prob=0.89)))
F1HillSim2680 <- data.frame(cbind(simmeanHill2680,PIHill2680))
F1HillSim2680$PlotCN <- all2680cc1 $PlotCN
F1HillSim2680 <- F1HillSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim2680) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4580 <- link(HillSplit1_G, data=all4580cc1)
simmeanHill4580 <- data.frame(apply(simHill4580,2,mean))
PIHill4580 <- t(data.frame(apply(simHill4580,2,PI,prob=0.89)))
F1HillSim4580 <- data.frame(cbind(simmeanHill4580,PIHill4580))
F1HillSim4580$PlotCN <- all4580cc1 $PlotCN
F1HillSim4580 <- F1HillSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim4580) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8580 <- link(HillSplit1_G, data=all8580cc1)
simmeanHill8580 <- data.frame(apply(simHill8580,2,mean))
PIHill8580 <- t(data.frame(apply(simHill8580,2,PI,prob=0.89)))
F1HillSim8580 <- data.frame(cbind(simmeanHill8580,PIHill8580))
F1HillSim8580$PlotCN <- all8580cc1 $PlotCN
F1HillSim8580 <- F1HillSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F1HillSim8580) <- c("PlotCN","HillPerAcre","5CI","95CI")


#create vector of new colnames
HillSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Hill", "5CI", "95CI"), x)))
HillSimnames<- append(HillSimnames,"PlotCN",after=0)

#group data by RCP pathway
F1HillSim26 <- F1HillSim2625 %>% cbind(F1HillSim2630[,2:4]) %>%
  cbind(F1HillSim2635[,2:4]) %>% cbind(F1HillSim2640[,2:4]) %>% cbind(F1HillSim2645[,2:4]) %>%
  cbind(F1HillSim2650[,2:4]) %>% cbind(F1HillSim2655[,2:4]) %>% cbind(F1HillSim2660[,2:4]) %>%
  cbind(F1HillSim2665[,2:4]) %>% cbind(F1HillSim2670[,2:4]) %>% cbind(F1HillSim2675[,2:4]) %>%
  cbind(F1HillSim2680[,2:4])
colnames(F1HillSim26) <- HillSimnames
write.csv(F1HillSim26,file="F1HillPred26.csv")

F1HillSim45 <- F1HillSim4525 %>% cbind(F1HillSim4530[,2:4]) %>%
  cbind(F1HillSim4535[,2:4]) %>% cbind(F1HillSim4540[,2:4]) %>% cbind(F1HillSim4545[,2:4]) %>%
  cbind(F1HillSim4550[,2:4]) %>% cbind(F1HillSim4555[,2:4]) %>% cbind(F1HillSim4560[,2:4]) %>%
  cbind(F1HillSim4565[,2:4]) %>% cbind(F1HillSim4570[,2:4]) %>% cbind(F1HillSim4575[,2:4]) %>%
  cbind(F1HillSim4580[,2:4])
colnames(F1HillSim45) <- HillSimnames
write.csv(F1HillSim45,file="F1HillPred45.csv")

F1HillSim85 <- F1HillSim8525 %>% cbind(F1HillSim8530[,2:4]) %>%
  cbind(F1HillSim8535[,2:4]) %>% cbind(F1HillSim8540[,2:4]) %>% cbind(F1HillSim8545[,2:4]) %>%
  cbind(F1HillSim8550[,2:4]) %>% cbind(F1HillSim8555[,2:4]) %>% cbind(F1HillSim8560[,2:4]) %>%
  cbind(F1HillSim8565[,2:4]) %>% cbind(F1HillSim8570[,2:4]) %>% cbind(F1HillSim8575[,2:4]) %>%
  cbind(F1HillSim8580[,2:4])
colnames(F1HillSim85) <- HillSimnames
write.csv(F1HillSim85,file="F1HillPred85.csv")


#fgroup 5
simHill2625 <- link(HillSplit5_G, data=all2625cc5 )
simmeanHill2625 <- data.frame(apply(simHill2625,2,mean))
PIHill2625 <- t(data.frame(apply(simHill2625,2,PI,prob=0.89)))
F5HillSim2625 <- data.frame(cbind(simmeanHill2625,PIHill2625))
F5HillSim2625$PlotCN <- all2625cc5  $PlotCN
F5HillSim2625 <- F5HillSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2625) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4525 <- link(HillSplit5_G, data=all4525cc5 )
simmeanHill4525 <- data.frame(apply(simHill4525,2,mean))
PIHill4525 <- t(data.frame(apply(simHill4525,2,PI,prob=0.89)))
F5HillSim4525 <- data.frame(cbind(simmeanHill4525,PIHill4525))
F5HillSim4525$PlotCN <- all4525cc5  $PlotCN
F5HillSim4525 <- F5HillSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4525) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8525 <- link(HillSplit5_G, data=all8525cc5 )
simmeanHill8525 <- data.frame(apply(simHill8525,2,mean))
PIHill8525 <- t(data.frame(apply(simHill8525,2,PI,prob=0.89)))
F5HillSim8525 <- data.frame(cbind(simmeanHill8525,PIHill8525))
F5HillSim8525$PlotCN <- all8525cc5  $PlotCN
F5HillSim8525 <- F5HillSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8525) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2630 <- link(HillSplit5_G, data=all2630cc5 )
simmeanHill2630 <- data.frame(apply(simHill2630,2,mean))
PIHill2630 <- t(data.frame(apply(simHill2630,2,PI,prob=0.89)))
F5HillSim2630 <- data.frame(cbind(simmeanHill2630,PIHill2630))
F5HillSim2630$PlotCN <- all2630cc5  $PlotCN
F5HillSim2630 <- F5HillSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2630) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4530 <- link(HillSplit5_G, data=all4530cc5 )
simmeanHill4530 <- data.frame(apply(simHill4530,2,mean))
PIHill4530 <- t(data.frame(apply(simHill4530,2,PI,prob=0.89)))
F5HillSim4530 <- data.frame(cbind(simmeanHill4530,PIHill4530))
F5HillSim4530$PlotCN <- all4530cc5  $PlotCN
F5HillSim4530 <- F5HillSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4530) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8530 <- link(HillSplit5_G, data=all8530cc5 )
simmeanHill8530 <- data.frame(apply(simHill8530,2,mean))
PIHill8530 <- t(data.frame(apply(simHill8530,2,PI,prob=0.89)))
F5HillSim8530 <- data.frame(cbind(simmeanHill8530,PIHill8530))
F5HillSim8530$PlotCN <- all8530cc5  $PlotCN
F5HillSim8530 <- F5HillSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8530) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2635 <- link(HillSplit5_G, data=all2635cc5 )
simmeanHill2635 <- data.frame(apply(simHill2635,2,mean))
PIHill2635 <- t(data.frame(apply(simHill2635,2,PI,prob=0.89)))
F5HillSim2635 <- data.frame(cbind(simmeanHill2635,PIHill2635))
F5HillSim2635$PlotCN <- all2635cc5  $PlotCN
F5HillSim2635 <- F5HillSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2635) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4535 <- link(HillSplit5_G, data=all4535cc5 )
simmeanHill4535 <- data.frame(apply(simHill4535,2,mean))
PIHill4535 <- t(data.frame(apply(simHill4535,2,PI,prob=0.89)))
F5HillSim4535 <- data.frame(cbind(simmeanHill4535,PIHill4535))
F5HillSim4535$PlotCN <- all4535cc5  $PlotCN
F5HillSim4535 <- F5HillSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4535) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8535 <- link(HillSplit5_G, data=all8535cc5 )
simmeanHill8535 <- data.frame(apply(simHill8535,2,mean))
PIHill8535 <- t(data.frame(apply(simHill8535,2,PI,prob=0.89)))
F5HillSim8535 <- data.frame(cbind(simmeanHill8535,PIHill8535))
F5HillSim8535$PlotCN <- all8535cc5  $PlotCN
F5HillSim8535 <- F5HillSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8535) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2640 <- link(HillSplit5_G, data=all2640cc5 )
simmeanHill2640 <- data.frame(apply(simHill2640,2,mean))
PIHill2640 <- t(data.frame(apply(simHill2640,2,PI,prob=0.89)))
F5HillSim2640 <- data.frame(cbind(simmeanHill2640,PIHill2640))
F5HillSim2640$PlotCN <- all2640cc5  $PlotCN
F5HillSim2640 <- F5HillSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2640) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4540 <- link(HillSplit5_G, data=all4540cc5 )
simmeanHill4540 <- data.frame(apply(simHill4540,2,mean))
PIHill4540 <- t(data.frame(apply(simHill4540,2,PI,prob=0.89)))
F5HillSim4540 <- data.frame(cbind(simmeanHill4540,PIHill4540))
F5HillSim4540$PlotCN <- all4540cc5  $PlotCN
F5HillSim4540 <- F5HillSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4540) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8540 <- link(HillSplit5_G, data=all8540cc5 )
simmeanHill8540 <- data.frame(apply(simHill8540,2,mean))
PIHill8540 <- t(data.frame(apply(simHill8540,2,PI,prob=0.89)))
F5HillSim8540 <- data.frame(cbind(simmeanHill8540,PIHill8540))
F5HillSim8540$PlotCN <- all8540cc5  $PlotCN
F5HillSim8540 <- F5HillSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8540) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2645 <- link(HillSplit5_G, data=all2645cc5 )
simmeanHill2645 <- data.frame(apply(simHill2645,2,mean))
PIHill2645 <- t(data.frame(apply(simHill2645,2,PI,prob=0.89)))
F5HillSim2645 <- data.frame(cbind(simmeanHill2645,PIHill2645))
F5HillSim2645$PlotCN <- all2645cc5  $PlotCN
F5HillSim2645 <- F5HillSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2645) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4545 <- link(HillSplit5_G, data=all4545cc5 )
simmeanHill4545 <- data.frame(apply(simHill4545,2,mean))
PIHill4545 <- t(data.frame(apply(simHill4545,2,PI,prob=0.89)))
F5HillSim4545 <- data.frame(cbind(simmeanHill4545,PIHill4545))
F5HillSim4545$PlotCN <- all4545cc5  $PlotCN
F5HillSim4545 <- F5HillSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4545) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8545 <- link(HillSplit5_G, data=all8545cc5 )
simmeanHill8545 <- data.frame(apply(simHill8545,2,mean))
PIHill8545 <- t(data.frame(apply(simHill8545,2,PI,prob=0.89)))
F5HillSim8545 <- data.frame(cbind(simmeanHill8545,PIHill8545))
F5HillSim8545$PlotCN <- all8545cc5  $PlotCN
F5HillSim8545 <- F5HillSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8545) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2650 <- link(HillSplit5_G, data=all2650cc5 )
simmeanHill2650 <- data.frame(apply(simHill2650,2,mean))
PIHill2650 <- t(data.frame(apply(simHill2650,2,PI,prob=0.89)))
F5HillSim2650 <- data.frame(cbind(simmeanHill2650,PIHill2650))
F5HillSim2650$PlotCN <- all2650cc5  $PlotCN
F5HillSim2650 <- F5HillSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2650) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4550 <- link(HillSplit5_G, data=all4550cc5 )
simmeanHill4550 <- data.frame(apply(simHill4550,2,mean))
PIHill4550 <- t(data.frame(apply(simHill4550,2,PI,prob=0.89)))
F5HillSim4550 <- data.frame(cbind(simmeanHill4550,PIHill4550))
F5HillSim4550$PlotCN <- all4550cc5  $PlotCN
F5HillSim4550 <- F5HillSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4550) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8550 <- link(HillSplit5_G, data=all8550cc5 )
simmeanHill8550 <- data.frame(apply(simHill8550,2,mean))
PIHill8550 <- t(data.frame(apply(simHill8550,2,PI,prob=0.89)))
F5HillSim8550 <- data.frame(cbind(simmeanHill8550,PIHill8550))
F5HillSim8550$PlotCN <- all8550cc5  $PlotCN
F5HillSim8550 <- F5HillSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8550) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2655 <- link(HillSplit5_G, data=all2655cc5 )
simmeanHill2655 <- data.frame(apply(simHill2655,2,mean))
PIHill2655 <- t(data.frame(apply(simHill2655,2,PI,prob=0.89)))
F5HillSim2655 <- data.frame(cbind(simmeanHill2655,PIHill2655))
F5HillSim2655$PlotCN <- all2655cc5  $PlotCN
F5HillSim2655 <- F5HillSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2655) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4555 <- link(HillSplit5_G, data=all4555cc5 )
simmeanHill4555 <- data.frame(apply(simHill4555,2,mean))
PIHill4555 <- t(data.frame(apply(simHill4555,2,PI,prob=0.89)))
F5HillSim4555 <- data.frame(cbind(simmeanHill4555,PIHill4555))
F5HillSim4555$PlotCN <- all4555cc5  $PlotCN
F5HillSim4555 <- F5HillSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4555) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8555 <- link(HillSplit5_G, data=all8555cc5 )
simmeanHill8555 <- data.frame(apply(simHill8555,2,mean))
PIHill8555 <- t(data.frame(apply(simHill8555,2,PI,prob=0.89)))
F5HillSim8555 <- data.frame(cbind(simmeanHill8555,PIHill8555))
F5HillSim8555$PlotCN <- all8555cc5  $PlotCN
F5HillSim8555 <- F5HillSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8555) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2660 <- link(HillSplit5_G, data=all2660cc5 )
simmeanHill2660 <- data.frame(apply(simHill2660,2,mean))
PIHill2660 <- t(data.frame(apply(simHill2660,2,PI,prob=0.89)))
F5HillSim2660 <- data.frame(cbind(simmeanHill2660,PIHill2660))
F5HillSim2660$PlotCN <- all2660cc5  $PlotCN
F5HillSim2660 <- F5HillSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2660) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4560 <- link(HillSplit5_G, data=all4560cc5 )
simmeanHill4560 <- data.frame(apply(simHill4560,2,mean))
PIHill4560 <- t(data.frame(apply(simHill4560,2,PI,prob=0.89)))
F5HillSim4560 <- data.frame(cbind(simmeanHill4560,PIHill4560))
F5HillSim4560$PlotCN <- all4560cc5  $PlotCN
F5HillSim4560 <- F5HillSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4560) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8560 <- link(HillSplit5_G, data=all8560cc5 )
simmeanHill8560 <- data.frame(apply(simHill8560,2,mean))
PIHill8560 <- t(data.frame(apply(simHill8560,2,PI,prob=0.89)))
F5HillSim8560 <- data.frame(cbind(simmeanHill8560,PIHill8560))
F5HillSim8560$PlotCN <- all8560cc5  $PlotCN
F5HillSim8560 <- F5HillSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8560) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2665 <- link(HillSplit5_G, data=all2665cc5 )
simmeanHill2665 <- data.frame(apply(simHill2665,2,mean))
PIHill2665 <- t(data.frame(apply(simHill2665,2,PI,prob=0.89)))
F5HillSim2665 <- data.frame(cbind(simmeanHill2665,PIHill2665))
F5HillSim2665$PlotCN <- all2665cc5  $PlotCN
F5HillSim2665 <- F5HillSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2665) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4565 <- link(HillSplit5_G, data=all4565cc5 )
simmeanHill4565 <- data.frame(apply(simHill4565,2,mean))
PIHill4565 <- t(data.frame(apply(simHill4565,2,PI,prob=0.89)))
F5HillSim4565 <- data.frame(cbind(simmeanHill4565,PIHill4565))
F5HillSim4565$PlotCN <- all4565cc5  $PlotCN
F5HillSim4565 <- F5HillSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4565) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8565 <- link(HillSplit5_G, data=all8565cc5 )
simmeanHill8565 <- data.frame(apply(simHill8565,2,mean))
PIHill8565 <- t(data.frame(apply(simHill8565,2,PI,prob=0.89)))
F5HillSim8565 <- data.frame(cbind(simmeanHill8565,PIHill8565))
F5HillSim8565$PlotCN <- all8565cc5  $PlotCN
F5HillSim8565 <- F5HillSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8565) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2670 <- link(HillSplit5_G, data=all2670cc5 )
simmeanHill2670 <- data.frame(apply(simHill2670,2,mean))
PIHill2670 <- t(data.frame(apply(simHill2670,2,PI,prob=0.89)))
F5HillSim2670 <- data.frame(cbind(simmeanHill2670,PIHill2670))
F5HillSim2670$PlotCN <- all2670cc5  $PlotCN
F5HillSim2670 <- F5HillSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2670) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4570 <- link(HillSplit5_G, data=all4570cc5 )
simmeanHill4570 <- data.frame(apply(simHill4570,2,mean))
PIHill4570 <- t(data.frame(apply(simHill4570,2,PI,prob=0.89)))
F5HillSim4570 <- data.frame(cbind(simmeanHill4570,PIHill4570))
F5HillSim4570$PlotCN <- all4570cc5  $PlotCN
F5HillSim4570 <- F5HillSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4570) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8570 <- link(HillSplit5_G, data=all8570cc5 )
simmeanHill8570 <- data.frame(apply(simHill8570,2,mean))
PIHill8570 <- t(data.frame(apply(simHill8570,2,PI,prob=0.89)))
F5HillSim8570 <- data.frame(cbind(simmeanHill8570,PIHill8570))
F5HillSim8570$PlotCN <- all8570cc5  $PlotCN
F5HillSim8570 <- F5HillSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8570) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2675 <- link(HillSplit5_G, data=all2675cc5 )
simmeanHill2675 <- data.frame(apply(simHill2675,2,mean))
PIHill2675 <- t(data.frame(apply(simHill2675,2,PI,prob=0.89)))
F5HillSim2675 <- data.frame(cbind(simmeanHill2675,PIHill2675))
F5HillSim2675$PlotCN <- all2675cc5  $PlotCN
F5HillSim2675 <- F5HillSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2675) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4575 <- link(HillSplit5_G, data=all4575cc5 )
simmeanHill4575 <- data.frame(apply(simHill4575,2,mean))
PIHill4575 <- t(data.frame(apply(simHill4575,2,PI,prob=0.89)))
F5HillSim4575 <- data.frame(cbind(simmeanHill4575,PIHill4575))
F5HillSim4575$PlotCN <- all4575cc5  $PlotCN
F5HillSim4575 <- F5HillSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4575) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8575 <- link(HillSplit5_G, data=all8575cc5 )
simmeanHill8575 <- data.frame(apply(simHill8575,2,mean))
PIHill8575 <- t(data.frame(apply(simHill8575,2,PI,prob=0.89)))
F5HillSim8575 <- data.frame(cbind(simmeanHill8575,PIHill8575))
F5HillSim8575$PlotCN <- all8575cc5  $PlotCN
F5HillSim8575 <- F5HillSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8575) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2680 <- link(HillSplit5_G, data=all2680cc5 )
simmeanHill2680 <- data.frame(apply(simHill2680,2,mean))
PIHill2680 <- t(data.frame(apply(simHill2680,2,PI,prob=0.89)))
F5HillSim2680 <- data.frame(cbind(simmeanHill2680,PIHill2680))
F5HillSim2680$PlotCN <- all2680cc5  $PlotCN
F5HillSim2680 <- F5HillSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim2680) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4580 <- link(HillSplit5_G, data=all4580cc5 )
simmeanHill4580 <- data.frame(apply(simHill4580,2,mean))
PIHill4580 <- t(data.frame(apply(simHill4580,2,PI,prob=0.89)))
F5HillSim4580 <- data.frame(cbind(simmeanHill4580,PIHill4580))
F5HillSim4580$PlotCN <- all4580cc5  $PlotCN
F5HillSim4580 <- F5HillSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim4580) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8580 <- link(HillSplit5_G, data=all8580cc5 )
simmeanHill8580 <- data.frame(apply(simHill8580,2,mean))
PIHill8580 <- t(data.frame(apply(simHill8580,2,PI,prob=0.89)))
F5HillSim8580 <- data.frame(cbind(simmeanHill8580,PIHill8580))
F5HillSim8580$PlotCN <- all8580cc5  $PlotCN
F5HillSim8580 <- F5HillSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F5HillSim8580) <- c("PlotCN","HillPerAcre","5CI","95CI")

#group data by RCP pathway
F5HillSim26 <- F5HillSim2625 %>% cbind(F5HillSim2630[,2:4]) %>%
  cbind(F5HillSim2635[,2:4]) %>% cbind(F5HillSim2640[,2:4]) %>% cbind(F5HillSim2645[,2:4]) %>%
  cbind(F5HillSim2650[,2:4]) %>% cbind(F5HillSim2655[,2:4]) %>% cbind(F5HillSim2660[,2:4]) %>%
  cbind(F5HillSim2665[,2:4]) %>% cbind(F5HillSim2670[,2:4]) %>% cbind(F5HillSim2675[,2:4]) %>%
  cbind(F5HillSim2680[,2:4])
colnames(F5HillSim26) <- HillSimnames
write.csv(F5HillSim26,file="F5HillPred26.csv")

F5HillSim45 <- F5HillSim4525 %>% cbind(F5HillSim4530[,2:4]) %>%
  cbind(F5HillSim4535[,2:4]) %>% cbind(F5HillSim4540[,2:4]) %>% cbind(F5HillSim4545[,2:4]) %>%
  cbind(F5HillSim4550[,2:4]) %>% cbind(F5HillSim4555[,2:4]) %>% cbind(F5HillSim4560[,2:4]) %>%
  cbind(F5HillSim4565[,2:4]) %>% cbind(F5HillSim4570[,2:4]) %>% cbind(F5HillSim4575[,2:4]) %>%
  cbind(F5HillSim4580[,2:4])
colnames(F5HillSim45) <- HillSimnames
write.csv(F5HillSim45,file="F5HillPred45.csv")

F5HillSim85 <- F5HillSim8525 %>% cbind(F5HillSim8530[,2:4]) %>%
  cbind(F5HillSim8535[,2:4]) %>% cbind(F5HillSim8540[,2:4]) %>% cbind(F5HillSim8545[,2:4]) %>%
  cbind(F5HillSim8550[,2:4]) %>% cbind(F5HillSim8555[,2:4]) %>% cbind(F5HillSim8560[,2:4]) %>%
  cbind(F5HillSim8565[,2:4]) %>% cbind(F5HillSim8570[,2:4]) %>% cbind(F5HillSim8575[,2:4]) %>%
  cbind(F5HillSim8580[,2:4])
colnames(F5HillSim85) <- HillSimnames
write.csv(F5HillSim85,file="F5HillPred85.csv")


#fgroup 20
simHill2625 <- link(HillSplit20_G, data=all2625cc20 )
simmeanHill2625 <- data.frame(apply(simHill2625,2,mean))
PIHill2625 <- t(data.frame(apply(simHill2625,2,PI,prob=0.89)))
F20HillSim2625 <- data.frame(cbind(simmeanHill2625,PIHill2625))
F20HillSim2625$PlotCN <- all2625cc20  $PlotCN
F20HillSim2625 <- F20HillSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2625) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4525 <- link(HillSplit20_G, data=all4525cc20 )
simmeanHill4525 <- data.frame(apply(simHill4525,2,mean))
PIHill4525 <- t(data.frame(apply(simHill4525,2,PI,prob=0.89)))
F20HillSim4525 <- data.frame(cbind(simmeanHill4525,PIHill4525))
F20HillSim4525$PlotCN <- all4525cc20  $PlotCN
F20HillSim4525 <- F20HillSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4525) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8525 <- link(HillSplit20_G, data=all8525cc20 )
simmeanHill8525 <- data.frame(apply(simHill8525,2,mean))
PIHill8525 <- t(data.frame(apply(simHill8525,2,PI,prob=0.89)))
F20HillSim8525 <- data.frame(cbind(simmeanHill8525,PIHill8525))
F20HillSim8525$PlotCN <- all8525cc20  $PlotCN
F20HillSim8525 <- F20HillSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8525) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2630 <- link(HillSplit20_G, data=all2630cc20 )
simmeanHill2630 <- data.frame(apply(simHill2630,2,mean))
PIHill2630 <- t(data.frame(apply(simHill2630,2,PI,prob=0.89)))
F20HillSim2630 <- data.frame(cbind(simmeanHill2630,PIHill2630))
F20HillSim2630$PlotCN <- all2630cc20  $PlotCN
F20HillSim2630 <- F20HillSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2630) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4530 <- link(HillSplit20_G, data=all4530cc20 )
simmeanHill4530 <- data.frame(apply(simHill4530,2,mean))
PIHill4530 <- t(data.frame(apply(simHill4530,2,PI,prob=0.89)))
F20HillSim4530 <- data.frame(cbind(simmeanHill4530,PIHill4530))
F20HillSim4530$PlotCN <- all4530cc20  $PlotCN
F20HillSim4530 <- F20HillSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4530) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8530 <- link(HillSplit20_G, data=all8530cc20 )
simmeanHill8530 <- data.frame(apply(simHill8530,2,mean))
PIHill8530 <- t(data.frame(apply(simHill8530,2,PI,prob=0.89)))
F20HillSim8530 <- data.frame(cbind(simmeanHill8530,PIHill8530))
F20HillSim8530$PlotCN <- all8530cc20  $PlotCN
F20HillSim8530 <- F20HillSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8530) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2635 <- link(HillSplit20_G, data=all2635cc20 )
simmeanHill2635 <- data.frame(apply(simHill2635,2,mean))
PIHill2635 <- t(data.frame(apply(simHill2635,2,PI,prob=0.89)))
F20HillSim2635 <- data.frame(cbind(simmeanHill2635,PIHill2635))
F20HillSim2635$PlotCN <- all2635cc20  $PlotCN
F20HillSim2635 <- F20HillSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2635) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4535 <- link(HillSplit20_G, data=all4535cc20 )
simmeanHill4535 <- data.frame(apply(simHill4535,2,mean))
PIHill4535 <- t(data.frame(apply(simHill4535,2,PI,prob=0.89)))
F20HillSim4535 <- data.frame(cbind(simmeanHill4535,PIHill4535))
F20HillSim4535$PlotCN <- all4535cc20  $PlotCN
F20HillSim4535 <- F20HillSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4535) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8535 <- link(HillSplit20_G, data=all8535cc20 )
simmeanHill8535 <- data.frame(apply(simHill8535,2,mean))
PIHill8535 <- t(data.frame(apply(simHill8535,2,PI,prob=0.89)))
F20HillSim8535 <- data.frame(cbind(simmeanHill8535,PIHill8535))
F20HillSim8535$PlotCN <- all8535cc20  $PlotCN
F20HillSim8535 <- F20HillSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8535) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2640 <- link(HillSplit20_G, data=all2640cc20 )
simmeanHill2640 <- data.frame(apply(simHill2640,2,mean))
PIHill2640 <- t(data.frame(apply(simHill2640,2,PI,prob=0.89)))
F20HillSim2640 <- data.frame(cbind(simmeanHill2640,PIHill2640))
F20HillSim2640$PlotCN <- all2640cc20  $PlotCN
F20HillSim2640 <- F20HillSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2640) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4540 <- link(HillSplit20_G, data=all4540cc20 )
simmeanHill4540 <- data.frame(apply(simHill4540,2,mean))
PIHill4540 <- t(data.frame(apply(simHill4540,2,PI,prob=0.89)))
F20HillSim4540 <- data.frame(cbind(simmeanHill4540,PIHill4540))
F20HillSim4540$PlotCN <- all4540cc20  $PlotCN
F20HillSim4540 <- F20HillSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4540) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8540 <- link(HillSplit20_G, data=all8540cc20 )
simmeanHill8540 <- data.frame(apply(simHill8540,2,mean))
PIHill8540 <- t(data.frame(apply(simHill8540,2,PI,prob=0.89)))
F20HillSim8540 <- data.frame(cbind(simmeanHill8540,PIHill8540))
F20HillSim8540$PlotCN <- all8540cc20  $PlotCN
F20HillSim8540 <- F20HillSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8540) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2645 <- link(HillSplit20_G, data=all2645cc20 )
simmeanHill2645 <- data.frame(apply(simHill2645,2,mean))
PIHill2645 <- t(data.frame(apply(simHill2645,2,PI,prob=0.89)))
F20HillSim2645 <- data.frame(cbind(simmeanHill2645,PIHill2645))
F20HillSim2645$PlotCN <- all2645cc20  $PlotCN
F20HillSim2645 <- F20HillSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2645) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4545 <- link(HillSplit20_G, data=all4545cc20 )
simmeanHill4545 <- data.frame(apply(simHill4545,2,mean))
PIHill4545 <- t(data.frame(apply(simHill4545,2,PI,prob=0.89)))
F20HillSim4545 <- data.frame(cbind(simmeanHill4545,PIHill4545))
F20HillSim4545$PlotCN <- all4545cc20  $PlotCN
F20HillSim4545 <- F20HillSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4545) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8545 <- link(HillSplit20_G, data=all8545cc20 )
simmeanHill8545 <- data.frame(apply(simHill8545,2,mean))
PIHill8545 <- t(data.frame(apply(simHill8545,2,PI,prob=0.89)))
F20HillSim8545 <- data.frame(cbind(simmeanHill8545,PIHill8545))
F20HillSim8545$PlotCN <- all8545cc20  $PlotCN
F20HillSim8545 <- F20HillSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8545) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2650 <- link(HillSplit20_G, data=all2650cc20 )
simmeanHill2650 <- data.frame(apply(simHill2650,2,mean))
PIHill2650 <- t(data.frame(apply(simHill2650,2,PI,prob=0.89)))
F20HillSim2650 <- data.frame(cbind(simmeanHill2650,PIHill2650))
F20HillSim2650$PlotCN <- all2650cc20  $PlotCN
F20HillSim2650 <- F20HillSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2650) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4550 <- link(HillSplit20_G, data=all4550cc20 )
simmeanHill4550 <- data.frame(apply(simHill4550,2,mean))
PIHill4550 <- t(data.frame(apply(simHill4550,2,PI,prob=0.89)))
F20HillSim4550 <- data.frame(cbind(simmeanHill4550,PIHill4550))
F20HillSim4550$PlotCN <- all4550cc20  $PlotCN
F20HillSim4550 <- F20HillSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4550) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8550 <- link(HillSplit20_G, data=all8550cc20 )
simmeanHill8550 <- data.frame(apply(simHill8550,2,mean))
PIHill8550 <- t(data.frame(apply(simHill8550,2,PI,prob=0.89)))
F20HillSim8550 <- data.frame(cbind(simmeanHill8550,PIHill8550))
F20HillSim8550$PlotCN <- all8550cc20  $PlotCN
F20HillSim8550 <- F20HillSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8550) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2655 <- link(HillSplit20_G, data=all2655cc20 )
simmeanHill2655 <- data.frame(apply(simHill2655,2,mean))
PIHill2655 <- t(data.frame(apply(simHill2655,2,PI,prob=0.89)))
F20HillSim2655 <- data.frame(cbind(simmeanHill2655,PIHill2655))
F20HillSim2655$PlotCN <- all2655cc20  $PlotCN
F20HillSim2655 <- F20HillSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2655) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4555 <- link(HillSplit20_G, data=all4555cc20 )
simmeanHill4555 <- data.frame(apply(simHill4555,2,mean))
PIHill4555 <- t(data.frame(apply(simHill4555,2,PI,prob=0.89)))
F20HillSim4555 <- data.frame(cbind(simmeanHill4555,PIHill4555))
F20HillSim4555$PlotCN <- all4555cc20  $PlotCN
F20HillSim4555 <- F20HillSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4555) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8555 <- link(HillSplit20_G, data=all8555cc20 )
simmeanHill8555 <- data.frame(apply(simHill8555,2,mean))
PIHill8555 <- t(data.frame(apply(simHill8555,2,PI,prob=0.89)))
F20HillSim8555 <- data.frame(cbind(simmeanHill8555,PIHill8555))
F20HillSim8555$PlotCN <- all8555cc20  $PlotCN
F20HillSim8555 <- F20HillSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8555) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2660 <- link(HillSplit20_G, data=all2660cc20 )
simmeanHill2660 <- data.frame(apply(simHill2660,2,mean))
PIHill2660 <- t(data.frame(apply(simHill2660,2,PI,prob=0.89)))
F20HillSim2660 <- data.frame(cbind(simmeanHill2660,PIHill2660))
F20HillSim2660$PlotCN <- all2660cc20  $PlotCN
F20HillSim2660 <- F20HillSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2660) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4560 <- link(HillSplit20_G, data=all4560cc20 )
simmeanHill4560 <- data.frame(apply(simHill4560,2,mean))
PIHill4560 <- t(data.frame(apply(simHill4560,2,PI,prob=0.89)))
F20HillSim4560 <- data.frame(cbind(simmeanHill4560,PIHill4560))
F20HillSim4560$PlotCN <- all4560cc20  $PlotCN
F20HillSim4560 <- F20HillSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4560) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8560 <- link(HillSplit20_G, data=all8560cc20 )
simmeanHill8560 <- data.frame(apply(simHill8560,2,mean))
PIHill8560 <- t(data.frame(apply(simHill8560,2,PI,prob=0.89)))
F20HillSim8560 <- data.frame(cbind(simmeanHill8560,PIHill8560))
F20HillSim8560$PlotCN <- all8560cc20  $PlotCN
F20HillSim8560 <- F20HillSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8560) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2665 <- link(HillSplit20_G, data=all2665cc20 )
simmeanHill2665 <- data.frame(apply(simHill2665,2,mean))
PIHill2665 <- t(data.frame(apply(simHill2665,2,PI,prob=0.89)))
F20HillSim2665 <- data.frame(cbind(simmeanHill2665,PIHill2665))
F20HillSim2665$PlotCN <- all2665cc20  $PlotCN
F20HillSim2665 <- F20HillSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2665) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4565 <- link(HillSplit20_G, data=all4565cc20 )
simmeanHill4565 <- data.frame(apply(simHill4565,2,mean))
PIHill4565 <- t(data.frame(apply(simHill4565,2,PI,prob=0.89)))
F20HillSim4565 <- data.frame(cbind(simmeanHill4565,PIHill4565))
F20HillSim4565$PlotCN <- all4565cc20  $PlotCN
F20HillSim4565 <- F20HillSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4565) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8565 <- link(HillSplit20_G, data=all8565cc20 )
simmeanHill8565 <- data.frame(apply(simHill8565,2,mean))
PIHill8565 <- t(data.frame(apply(simHill8565,2,PI,prob=0.89)))
F20HillSim8565 <- data.frame(cbind(simmeanHill8565,PIHill8565))
F20HillSim8565$PlotCN <- all8565cc20  $PlotCN
F20HillSim8565 <- F20HillSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8565) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2670 <- link(HillSplit20_G, data=all2670cc20 )
simmeanHill2670 <- data.frame(apply(simHill2670,2,mean))
PIHill2670 <- t(data.frame(apply(simHill2670,2,PI,prob=0.89)))
F20HillSim2670 <- data.frame(cbind(simmeanHill2670,PIHill2670))
F20HillSim2670$PlotCN <- all2670cc20  $PlotCN
F20HillSim2670 <- F20HillSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2670) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4570 <- link(HillSplit20_G, data=all4570cc20 )
simmeanHill4570 <- data.frame(apply(simHill4570,2,mean))
PIHill4570 <- t(data.frame(apply(simHill4570,2,PI,prob=0.89)))
F20HillSim4570 <- data.frame(cbind(simmeanHill4570,PIHill4570))
F20HillSim4570$PlotCN <- all4570cc20  $PlotCN
F20HillSim4570 <- F20HillSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4570) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8570 <- link(HillSplit20_G, data=all8570cc20 )
simmeanHill8570 <- data.frame(apply(simHill8570,2,mean))
PIHill8570 <- t(data.frame(apply(simHill8570,2,PI,prob=0.89)))
F20HillSim8570 <- data.frame(cbind(simmeanHill8570,PIHill8570))
F20HillSim8570$PlotCN <- all8570cc20  $PlotCN
F20HillSim8570 <- F20HillSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8570) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2675 <- link(HillSplit20_G, data=all2675cc20 )
simmeanHill2675 <- data.frame(apply(simHill2675,2,mean))
PIHill2675 <- t(data.frame(apply(simHill2675,2,PI,prob=0.89)))
F20HillSim2675 <- data.frame(cbind(simmeanHill2675,PIHill2675))
F20HillSim2675$PlotCN <- all2675cc20  $PlotCN
F20HillSim2675 <- F20HillSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2675) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4575 <- link(HillSplit20_G, data=all4575cc20 )
simmeanHill4575 <- data.frame(apply(simHill4575,2,mean))
PIHill4575 <- t(data.frame(apply(simHill4575,2,PI,prob=0.89)))
F20HillSim4575 <- data.frame(cbind(simmeanHill4575,PIHill4575))
F20HillSim4575$PlotCN <- all4575cc20  $PlotCN
F20HillSim4575 <- F20HillSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4575) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8575 <- link(HillSplit20_G, data=all8575cc20 )
simmeanHill8575 <- data.frame(apply(simHill8575,2,mean))
PIHill8575 <- t(data.frame(apply(simHill8575,2,PI,prob=0.89)))
F20HillSim8575 <- data.frame(cbind(simmeanHill8575,PIHill8575))
F20HillSim8575$PlotCN <- all8575cc20  $PlotCN
F20HillSim8575 <- F20HillSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8575) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2680 <- link(HillSplit20_G, data=all2680cc20 )
simmeanHill2680 <- data.frame(apply(simHill2680,2,mean))
PIHill2680 <- t(data.frame(apply(simHill2680,2,PI,prob=0.89)))
F20HillSim2680 <- data.frame(cbind(simmeanHill2680,PIHill2680))
F20HillSim2680$PlotCN <- all2680cc20  $PlotCN
F20HillSim2680 <- F20HillSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim2680) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4580 <- link(HillSplit20_G, data=all4580cc20 )
simmeanHill4580 <- data.frame(apply(simHill4580,2,mean))
PIHill4580 <- t(data.frame(apply(simHill4580,2,PI,prob=0.89)))
F20HillSim4580 <- data.frame(cbind(simmeanHill4580,PIHill4580))
F20HillSim4580$PlotCN <- all4580cc20  $PlotCN
F20HillSim4580 <- F20HillSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim4580) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8580 <- link(HillSplit20_G, data=all8580cc20 )
simmeanHill8580 <- data.frame(apply(simHill8580,2,mean))
PIHill8580 <- t(data.frame(apply(simHill8580,2,PI,prob=0.89)))
F20HillSim8580 <- data.frame(cbind(simmeanHill8580,PIHill8580))
F20HillSim8580$PlotCN <- all8580cc20  $PlotCN
F20HillSim8580 <- F20HillSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F20HillSim8580) <- c("PlotCN","HillPerAcre","5CI","95CI")


#group data by RCP pathway
F20HillSim26 <- F20HillSim2625 %>% cbind(F20HillSim2630[,2:4]) %>%
  cbind(F20HillSim2635[,2:4]) %>% cbind(F20HillSim2640[,2:4]) %>% cbind(F20HillSim2645[,2:4]) %>%
  cbind(F20HillSim2650[,2:4]) %>% cbind(F20HillSim2655[,2:4]) %>% cbind(F20HillSim2660[,2:4]) %>%
  cbind(F20HillSim2665[,2:4]) %>% cbind(F20HillSim2670[,2:4]) %>% cbind(F20HillSim2675[,2:4]) %>%
  cbind(F20HillSim2680[,2:4])
colnames(F20HillSim26) <- HillSimnames
write.csv(F20HillSim26,file="F20HillPred26.csv")

F20HillSim45 <- F20HillSim4525 %>% cbind(F20HillSim4530[,2:4]) %>%
  cbind(F20HillSim4535[,2:4]) %>% cbind(F20HillSim4540[,2:4]) %>% cbind(F20HillSim4545[,2:4]) %>%
  cbind(F20HillSim4550[,2:4]) %>% cbind(F20HillSim4555[,2:4]) %>% cbind(F20HillSim4560[,2:4]) %>%
  cbind(F20HillSim4565[,2:4]) %>% cbind(F20HillSim4570[,2:4]) %>% cbind(F20HillSim4575[,2:4]) %>%
  cbind(F20HillSim4580[,2:4])
colnames(F20HillSim45) <- HillSimnames
write.csv(F20HillSim45,file="F20HillPred45.csv")

F20HillSim85 <- F20HillSim8525 %>% cbind(F20HillSim8530[,2:4]) %>%
  cbind(F20HillSim8535[,2:4]) %>% cbind(F20HillSim8540[,2:4]) %>% cbind(F20HillSim8545[,2:4]) %>%
  cbind(F20HillSim8550[,2:4]) %>% cbind(F20HillSim8555[,2:4]) %>% cbind(F20HillSim8560[,2:4]) %>%
  cbind(F20HillSim8565[,2:4]) %>% cbind(F20HillSim8570[,2:4]) %>% cbind(F20HillSim8575[,2:4]) %>%
  cbind(F20HillSim8580[,2:4])
colnames(F20HillSim85) <- HillSimnames
write.csv(F20HillSim85,file="F20HillPred85.csv")


#fgroup 21
simHill2625 <- link(HillSplit21_G, data=all2625cc21 )
simmeanHill2625 <- data.frame(apply(simHill2625,2,mean))
PIHill2625 <- t(data.frame(apply(simHill2625,2,PI,prob=0.89)))
F21HillSim2625 <- data.frame(cbind(simmeanHill2625,PIHill2625))
F21HillSim2625$PlotCN <- all2625cc21  $PlotCN
F21HillSim2625 <- F21HillSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2625) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4525 <- link(HillSplit21_G, data=all4525cc21 )
simmeanHill4525 <- data.frame(apply(simHill4525,2,mean))
PIHill4525 <- t(data.frame(apply(simHill4525,2,PI,prob=0.89)))
F21HillSim4525 <- data.frame(cbind(simmeanHill4525,PIHill4525))
F21HillSim4525$PlotCN <- all4525cc21  $PlotCN
F21HillSim4525 <- F21HillSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4525) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8525 <- link(HillSplit21_G, data=all8525cc21 )
simmeanHill8525 <- data.frame(apply(simHill8525,2,mean))
PIHill8525 <- t(data.frame(apply(simHill8525,2,PI,prob=0.89)))
F21HillSim8525 <- data.frame(cbind(simmeanHill8525,PIHill8525))
F21HillSim8525$PlotCN <- all8525cc21  $PlotCN
F21HillSim8525 <- F21HillSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8525) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2630 <- link(HillSplit21_G, data=all2630cc21 )
simmeanHill2630 <- data.frame(apply(simHill2630,2,mean))
PIHill2630 <- t(data.frame(apply(simHill2630,2,PI,prob=0.89)))
F21HillSim2630 <- data.frame(cbind(simmeanHill2630,PIHill2630))
F21HillSim2630$PlotCN <- all2630cc21  $PlotCN
F21HillSim2630 <- F21HillSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2630) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4530 <- link(HillSplit21_G, data=all4530cc21 )
simmeanHill4530 <- data.frame(apply(simHill4530,2,mean))
PIHill4530 <- t(data.frame(apply(simHill4530,2,PI,prob=0.89)))
F21HillSim4530 <- data.frame(cbind(simmeanHill4530,PIHill4530))
F21HillSim4530$PlotCN <- all4530cc21  $PlotCN
F21HillSim4530 <- F21HillSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4530) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8530 <- link(HillSplit21_G, data=all8530cc21 )
simmeanHill8530 <- data.frame(apply(simHill8530,2,mean))
PIHill8530 <- t(data.frame(apply(simHill8530,2,PI,prob=0.89)))
F21HillSim8530 <- data.frame(cbind(simmeanHill8530,PIHill8530))
F21HillSim8530$PlotCN <- all8530cc21  $PlotCN
F21HillSim8530 <- F21HillSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8530) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2635 <- link(HillSplit21_G, data=all2635cc21 )
simmeanHill2635 <- data.frame(apply(simHill2635,2,mean))
PIHill2635 <- t(data.frame(apply(simHill2635,2,PI,prob=0.89)))
F21HillSim2635 <- data.frame(cbind(simmeanHill2635,PIHill2635))
F21HillSim2635$PlotCN <- all2635cc21  $PlotCN
F21HillSim2635 <- F21HillSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2635) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4535 <- link(HillSplit21_G, data=all4535cc21 )
simmeanHill4535 <- data.frame(apply(simHill4535,2,mean))
PIHill4535 <- t(data.frame(apply(simHill4535,2,PI,prob=0.89)))
F21HillSim4535 <- data.frame(cbind(simmeanHill4535,PIHill4535))
F21HillSim4535$PlotCN <- all4535cc21  $PlotCN
F21HillSim4535 <- F21HillSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4535) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8535 <- link(HillSplit21_G, data=all8535cc21 )
simmeanHill8535 <- data.frame(apply(simHill8535,2,mean))
PIHill8535 <- t(data.frame(apply(simHill8535,2,PI,prob=0.89)))
F21HillSim8535 <- data.frame(cbind(simmeanHill8535,PIHill8535))
F21HillSim8535$PlotCN <- all8535cc21  $PlotCN
F21HillSim8535 <- F21HillSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8535) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2640 <- link(HillSplit21_G, data=all2640cc21 )
simmeanHill2640 <- data.frame(apply(simHill2640,2,mean))
PIHill2640 <- t(data.frame(apply(simHill2640,2,PI,prob=0.89)))
F21HillSim2640 <- data.frame(cbind(simmeanHill2640,PIHill2640))
F21HillSim2640$PlotCN <- all2640cc21  $PlotCN
F21HillSim2640 <- F21HillSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2640) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4540 <- link(HillSplit21_G, data=all4540cc21 )
simmeanHill4540 <- data.frame(apply(simHill4540,2,mean))
PIHill4540 <- t(data.frame(apply(simHill4540,2,PI,prob=0.89)))
F21HillSim4540 <- data.frame(cbind(simmeanHill4540,PIHill4540))
F21HillSim4540$PlotCN <- all4540cc21  $PlotCN
F21HillSim4540 <- F21HillSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4540) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8540 <- link(HillSplit21_G, data=all8540cc21 )
simmeanHill8540 <- data.frame(apply(simHill8540,2,mean))
PIHill8540 <- t(data.frame(apply(simHill8540,2,PI,prob=0.89)))
F21HillSim8540 <- data.frame(cbind(simmeanHill8540,PIHill8540))
F21HillSim8540$PlotCN <- all8540cc21  $PlotCN
F21HillSim8540 <- F21HillSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8540) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2645 <- link(HillSplit21_G, data=all2645cc21 )
simmeanHill2645 <- data.frame(apply(simHill2645,2,mean))
PIHill2645 <- t(data.frame(apply(simHill2645,2,PI,prob=0.89)))
F21HillSim2645 <- data.frame(cbind(simmeanHill2645,PIHill2645))
F21HillSim2645$PlotCN <- all2645cc21  $PlotCN
F21HillSim2645 <- F21HillSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2645) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4545 <- link(HillSplit21_G, data=all4545cc21 )
simmeanHill4545 <- data.frame(apply(simHill4545,2,mean))
PIHill4545 <- t(data.frame(apply(simHill4545,2,PI,prob=0.89)))
F21HillSim4545 <- data.frame(cbind(simmeanHill4545,PIHill4545))
F21HillSim4545$PlotCN <- all4545cc21  $PlotCN
F21HillSim4545 <- F21HillSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4545) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8545 <- link(HillSplit21_G, data=all8545cc21 )
simmeanHill8545 <- data.frame(apply(simHill8545,2,mean))
PIHill8545 <- t(data.frame(apply(simHill8545,2,PI,prob=0.89)))
F21HillSim8545 <- data.frame(cbind(simmeanHill8545,PIHill8545))
F21HillSim8545$PlotCN <- all8545cc21  $PlotCN
F21HillSim8545 <- F21HillSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8545) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2650 <- link(HillSplit21_G, data=all2650cc21 )
simmeanHill2650 <- data.frame(apply(simHill2650,2,mean))
PIHill2650 <- t(data.frame(apply(simHill2650,2,PI,prob=0.89)))
F21HillSim2650 <- data.frame(cbind(simmeanHill2650,PIHill2650))
F21HillSim2650$PlotCN <- all2650cc21  $PlotCN
F21HillSim2650 <- F21HillSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2650) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4550 <- link(HillSplit21_G, data=all4550cc21 )
simmeanHill4550 <- data.frame(apply(simHill4550,2,mean))
PIHill4550 <- t(data.frame(apply(simHill4550,2,PI,prob=0.89)))
F21HillSim4550 <- data.frame(cbind(simmeanHill4550,PIHill4550))
F21HillSim4550$PlotCN <- all4550cc21  $PlotCN
F21HillSim4550 <- F21HillSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4550) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8550 <- link(HillSplit21_G, data=all8550cc21 )
simmeanHill8550 <- data.frame(apply(simHill8550,2,mean))
PIHill8550 <- t(data.frame(apply(simHill8550,2,PI,prob=0.89)))
F21HillSim8550 <- data.frame(cbind(simmeanHill8550,PIHill8550))
F21HillSim8550$PlotCN <- all8550cc21  $PlotCN
F21HillSim8550 <- F21HillSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8550) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2655 <- link(HillSplit21_G, data=all2655cc21 )
simmeanHill2655 <- data.frame(apply(simHill2655,2,mean))
PIHill2655 <- t(data.frame(apply(simHill2655,2,PI,prob=0.89)))
F21HillSim2655 <- data.frame(cbind(simmeanHill2655,PIHill2655))
F21HillSim2655$PlotCN <- all2655cc21  $PlotCN
F21HillSim2655 <- F21HillSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2655) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4555 <- link(HillSplit21_G, data=all4555cc21 )
simmeanHill4555 <- data.frame(apply(simHill4555,2,mean))
PIHill4555 <- t(data.frame(apply(simHill4555,2,PI,prob=0.89)))
F21HillSim4555 <- data.frame(cbind(simmeanHill4555,PIHill4555))
F21HillSim4555$PlotCN <- all4555cc21  $PlotCN
F21HillSim4555 <- F21HillSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4555) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8555 <- link(HillSplit21_G, data=all8555cc21 )
simmeanHill8555 <- data.frame(apply(simHill8555,2,mean))
PIHill8555 <- t(data.frame(apply(simHill8555,2,PI,prob=0.89)))
F21HillSim8555 <- data.frame(cbind(simmeanHill8555,PIHill8555))
F21HillSim8555$PlotCN <- all8555cc21  $PlotCN
F21HillSim8555 <- F21HillSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8555) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2660 <- link(HillSplit21_G, data=all2660cc21 )
simmeanHill2660 <- data.frame(apply(simHill2660,2,mean))
PIHill2660 <- t(data.frame(apply(simHill2660,2,PI,prob=0.89)))
F21HillSim2660 <- data.frame(cbind(simmeanHill2660,PIHill2660))
F21HillSim2660$PlotCN <- all2660cc21  $PlotCN
F21HillSim2660 <- F21HillSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2660) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4560 <- link(HillSplit21_G, data=all4560cc21 )
simmeanHill4560 <- data.frame(apply(simHill4560,2,mean))
PIHill4560 <- t(data.frame(apply(simHill4560,2,PI,prob=0.89)))
F21HillSim4560 <- data.frame(cbind(simmeanHill4560,PIHill4560))
F21HillSim4560$PlotCN <- all4560cc21  $PlotCN
F21HillSim4560 <- F21HillSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4560) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8560 <- link(HillSplit21_G, data=all8560cc21 )
simmeanHill8560 <- data.frame(apply(simHill8560,2,mean))
PIHill8560 <- t(data.frame(apply(simHill8560,2,PI,prob=0.89)))
F21HillSim8560 <- data.frame(cbind(simmeanHill8560,PIHill8560))
F21HillSim8560$PlotCN <- all8560cc21  $PlotCN
F21HillSim8560 <- F21HillSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8560) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2665 <- link(HillSplit21_G, data=all2665cc21 )
simmeanHill2665 <- data.frame(apply(simHill2665,2,mean))
PIHill2665 <- t(data.frame(apply(simHill2665,2,PI,prob=0.89)))
F21HillSim2665 <- data.frame(cbind(simmeanHill2665,PIHill2665))
F21HillSim2665$PlotCN <- all2665cc21  $PlotCN
F21HillSim2665 <- F21HillSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2665) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4565 <- link(HillSplit21_G, data=all4565cc21 )
simmeanHill4565 <- data.frame(apply(simHill4565,2,mean))
PIHill4565 <- t(data.frame(apply(simHill4565,2,PI,prob=0.89)))
F21HillSim4565 <- data.frame(cbind(simmeanHill4565,PIHill4565))
F21HillSim4565$PlotCN <- all4565cc21  $PlotCN
F21HillSim4565 <- F21HillSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4565) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8565 <- link(HillSplit21_G, data=all8565cc21 )
simmeanHill8565 <- data.frame(apply(simHill8565,2,mean))
PIHill8565 <- t(data.frame(apply(simHill8565,2,PI,prob=0.89)))
F21HillSim8565 <- data.frame(cbind(simmeanHill8565,PIHill8565))
F21HillSim8565$PlotCN <- all8565cc21  $PlotCN
F21HillSim8565 <- F21HillSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8565) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2670 <- link(HillSplit21_G, data=all2670cc21 )
simmeanHill2670 <- data.frame(apply(simHill2670,2,mean))
PIHill2670 <- t(data.frame(apply(simHill2670,2,PI,prob=0.89)))
F21HillSim2670 <- data.frame(cbind(simmeanHill2670,PIHill2670))
F21HillSim2670$PlotCN <- all2670cc21  $PlotCN
F21HillSim2670 <- F21HillSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2670) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4570 <- link(HillSplit21_G, data=all4570cc21 )
simmeanHill4570 <- data.frame(apply(simHill4570,2,mean))
PIHill4570 <- t(data.frame(apply(simHill4570,2,PI,prob=0.89)))
F21HillSim4570 <- data.frame(cbind(simmeanHill4570,PIHill4570))
F21HillSim4570$PlotCN <- all4570cc21  $PlotCN
F21HillSim4570 <- F21HillSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4570) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8570 <- link(HillSplit21_G, data=all8570cc21 )
simmeanHill8570 <- data.frame(apply(simHill8570,2,mean))
PIHill8570 <- t(data.frame(apply(simHill8570,2,PI,prob=0.89)))
F21HillSim8570 <- data.frame(cbind(simmeanHill8570,PIHill8570))
F21HillSim8570$PlotCN <- all8570cc21  $PlotCN
F21HillSim8570 <- F21HillSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8570) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2675 <- link(HillSplit21_G, data=all2675cc21 )
simmeanHill2675 <- data.frame(apply(simHill2675,2,mean))
PIHill2675 <- t(data.frame(apply(simHill2675,2,PI,prob=0.89)))
F21HillSim2675 <- data.frame(cbind(simmeanHill2675,PIHill2675))
F21HillSim2675$PlotCN <- all2675cc21  $PlotCN
F21HillSim2675 <- F21HillSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2675) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4575 <- link(HillSplit21_G, data=all4575cc21 )
simmeanHill4575 <- data.frame(apply(simHill4575,2,mean))
PIHill4575 <- t(data.frame(apply(simHill4575,2,PI,prob=0.89)))
F21HillSim4575 <- data.frame(cbind(simmeanHill4575,PIHill4575))
F21HillSim4575$PlotCN <- all4575cc21  $PlotCN
F21HillSim4575 <- F21HillSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4575) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8575 <- link(HillSplit21_G, data=all8575cc21 )
simmeanHill8575 <- data.frame(apply(simHill8575,2,mean))
PIHill8575 <- t(data.frame(apply(simHill8575,2,PI,prob=0.89)))
F21HillSim8575 <- data.frame(cbind(simmeanHill8575,PIHill8575))
F21HillSim8575$PlotCN <- all8575cc21  $PlotCN
F21HillSim8575 <- F21HillSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8575) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2680 <- link(HillSplit21_G, data=all2680cc21 )
simmeanHill2680 <- data.frame(apply(simHill2680,2,mean))
PIHill2680 <- t(data.frame(apply(simHill2680,2,PI,prob=0.89)))
F21HillSim2680 <- data.frame(cbind(simmeanHill2680,PIHill2680))
F21HillSim2680$PlotCN <- all2680cc21  $PlotCN
F21HillSim2680 <- F21HillSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim2680) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4580 <- link(HillSplit21_G, data=all4580cc21 )
simmeanHill4580 <- data.frame(apply(simHill4580,2,mean))
PIHill4580 <- t(data.frame(apply(simHill4580,2,PI,prob=0.89)))
F21HillSim4580 <- data.frame(cbind(simmeanHill4580,PIHill4580))
F21HillSim4580$PlotCN <- all4580cc21  $PlotCN
F21HillSim4580 <- F21HillSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim4580) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8580 <- link(HillSplit21_G, data=all8580cc21 )
simmeanHill8580 <- data.frame(apply(simHill8580,2,mean))
PIHill8580 <- t(data.frame(apply(simHill8580,2,PI,prob=0.89)))
F21HillSim8580 <- data.frame(cbind(simmeanHill8580,PIHill8580))
F21HillSim8580$PlotCN <- all8580cc21  $PlotCN
F21HillSim8580 <- F21HillSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F21HillSim8580) <- c("PlotCN","HillPerAcre","5CI","95CI")


#create vector of new colnames
HillSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Hill", "5CI", "95CI"), x)))
HillSimnames<- append(HillSimnames,"PlotCN",after=0)

#group data by RCP pathway
F21HillSim26 <- F21HillSim2625 %>% cbind(F21HillSim2630[,2:4]) %>%
  cbind(F21HillSim2635[,2:4]) %>% cbind(F21HillSim2640[,2:4]) %>% cbind(F21HillSim2645[,2:4]) %>%
  cbind(F21HillSim2650[,2:4]) %>% cbind(F21HillSim2655[,2:4]) %>% cbind(F21HillSim2660[,2:4]) %>%
  cbind(F21HillSim2665[,2:4]) %>% cbind(F21HillSim2670[,2:4]) %>% cbind(F21HillSim2675[,2:4]) %>%
  cbind(F21HillSim2680[,2:4])
colnames(F21HillSim26) <- HillSimnames
write.csv(F21HillSim26,file="F21HillPred26.csv")

F21HillSim45 <- F21HillSim4525 %>% cbind(F21HillSim4530[,2:4]) %>%
  cbind(F21HillSim4535[,2:4]) %>% cbind(F21HillSim4540[,2:4]) %>% cbind(F21HillSim4545[,2:4]) %>%
  cbind(F21HillSim4550[,2:4]) %>% cbind(F21HillSim4555[,2:4]) %>% cbind(F21HillSim4560[,2:4]) %>%
  cbind(F21HillSim4565[,2:4]) %>% cbind(F21HillSim4570[,2:4]) %>% cbind(F21HillSim4575[,2:4]) %>%
  cbind(F21HillSim4580[,2:4])
colnames(F21HillSim45) <- HillSimnames
write.csv(F21HillSim45,file="F21HillPred45.csv")

F21HillSim85 <- F21HillSim8525 %>% cbind(F21HillSim8530[,2:4]) %>%
  cbind(F21HillSim8535[,2:4]) %>% cbind(F21HillSim8540[,2:4]) %>% cbind(F21HillSim8545[,2:4]) %>%
  cbind(F21HillSim8550[,2:4]) %>% cbind(F21HillSim8555[,2:4]) %>% cbind(F21HillSim8560[,2:4]) %>%
  cbind(F21HillSim8565[,2:4]) %>% cbind(F21HillSim8570[,2:4]) %>% cbind(F21HillSim8575[,2:4]) %>%
  cbind(F21HillSim8580[,2:4])
colnames(F21HillSim85) <- HillSimnames
write.csv(F21HillSim85,file="F21HillPred85.csv")


#fgroup 23
simHill2625 <- link(HillSplit23_G, data=all2625cc23 )
simmeanHill2625 <- data.frame(apply(simHill2625,2,mean))
PIHill2625 <- t(data.frame(apply(simHill2625,2,PI,prob=0.89)))
F23HillSim2625 <- data.frame(cbind(simmeanHill2625,PIHill2625))
F23HillSim2625$PlotCN <- all2625cc23  $PlotCN
F23HillSim2625 <- F23HillSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2625) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4525 <- link(HillSplit23_G, data=all4525cc23 )
simmeanHill4525 <- data.frame(apply(simHill4525,2,mean))
PIHill4525 <- t(data.frame(apply(simHill4525,2,PI,prob=0.89)))
F23HillSim4525 <- data.frame(cbind(simmeanHill4525,PIHill4525))
F23HillSim4525$PlotCN <- all4525cc23  $PlotCN
F23HillSim4525 <- F23HillSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4525) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8525 <- link(HillSplit23_G, data=all8525cc23 )
simmeanHill8525 <- data.frame(apply(simHill8525,2,mean))
PIHill8525 <- t(data.frame(apply(simHill8525,2,PI,prob=0.89)))
F23HillSim8525 <- data.frame(cbind(simmeanHill8525,PIHill8525))
F23HillSim8525$PlotCN <- all8525cc23  $PlotCN
F23HillSim8525 <- F23HillSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8525) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2630 <- link(HillSplit23_G, data=all2630cc23 )
simmeanHill2630 <- data.frame(apply(simHill2630,2,mean))
PIHill2630 <- t(data.frame(apply(simHill2630,2,PI,prob=0.89)))
F23HillSim2630 <- data.frame(cbind(simmeanHill2630,PIHill2630))
F23HillSim2630$PlotCN <- all2630cc23  $PlotCN
F23HillSim2630 <- F23HillSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2630) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4530 <- link(HillSplit23_G, data=all4530cc23 )
simmeanHill4530 <- data.frame(apply(simHill4530,2,mean))
PIHill4530 <- t(data.frame(apply(simHill4530,2,PI,prob=0.89)))
F23HillSim4530 <- data.frame(cbind(simmeanHill4530,PIHill4530))
F23HillSim4530$PlotCN <- all4530cc23  $PlotCN
F23HillSim4530 <- F23HillSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4530) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8530 <- link(HillSplit23_G, data=all8530cc23 )
simmeanHill8530 <- data.frame(apply(simHill8530,2,mean))
PIHill8530 <- t(data.frame(apply(simHill8530,2,PI,prob=0.89)))
F23HillSim8530 <- data.frame(cbind(simmeanHill8530,PIHill8530))
F23HillSim8530$PlotCN <- all8530cc23  $PlotCN
F23HillSim8530 <- F23HillSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8530) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2635 <- link(HillSplit23_G, data=all2635cc23 )
simmeanHill2635 <- data.frame(apply(simHill2635,2,mean))
PIHill2635 <- t(data.frame(apply(simHill2635,2,PI,prob=0.89)))
F23HillSim2635 <- data.frame(cbind(simmeanHill2635,PIHill2635))
F23HillSim2635$PlotCN <- all2635cc23  $PlotCN
F23HillSim2635 <- F23HillSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2635) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4535 <- link(HillSplit23_G, data=all4535cc23 )
simmeanHill4535 <- data.frame(apply(simHill4535,2,mean))
PIHill4535 <- t(data.frame(apply(simHill4535,2,PI,prob=0.89)))
F23HillSim4535 <- data.frame(cbind(simmeanHill4535,PIHill4535))
F23HillSim4535$PlotCN <- all4535cc23  $PlotCN
F23HillSim4535 <- F23HillSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4535) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8535 <- link(HillSplit23_G, data=all8535cc23 )
simmeanHill8535 <- data.frame(apply(simHill8535,2,mean))
PIHill8535 <- t(data.frame(apply(simHill8535,2,PI,prob=0.89)))
F23HillSim8535 <- data.frame(cbind(simmeanHill8535,PIHill8535))
F23HillSim8535$PlotCN <- all8535cc23  $PlotCN
F23HillSim8535 <- F23HillSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8535) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2640 <- link(HillSplit23_G, data=all2640cc23 )
simmeanHill2640 <- data.frame(apply(simHill2640,2,mean))
PIHill2640 <- t(data.frame(apply(simHill2640,2,PI,prob=0.89)))
F23HillSim2640 <- data.frame(cbind(simmeanHill2640,PIHill2640))
F23HillSim2640$PlotCN <- all2640cc23  $PlotCN
F23HillSim2640 <- F23HillSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2640) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4540 <- link(HillSplit23_G, data=all4540cc23 )
simmeanHill4540 <- data.frame(apply(simHill4540,2,mean))
PIHill4540 <- t(data.frame(apply(simHill4540,2,PI,prob=0.89)))
F23HillSim4540 <- data.frame(cbind(simmeanHill4540,PIHill4540))
F23HillSim4540$PlotCN <- all4540cc23  $PlotCN
F23HillSim4540 <- F23HillSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4540) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8540 <- link(HillSplit23_G, data=all8540cc23 )
simmeanHill8540 <- data.frame(apply(simHill8540,2,mean))
PIHill8540 <- t(data.frame(apply(simHill8540,2,PI,prob=0.89)))
F23HillSim8540 <- data.frame(cbind(simmeanHill8540,PIHill8540))
F23HillSim8540$PlotCN <- all8540cc23  $PlotCN
F23HillSim8540 <- F23HillSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8540) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2645 <- link(HillSplit23_G, data=all2645cc23 )
simmeanHill2645 <- data.frame(apply(simHill2645,2,mean))
PIHill2645 <- t(data.frame(apply(simHill2645,2,PI,prob=0.89)))
F23HillSim2645 <- data.frame(cbind(simmeanHill2645,PIHill2645))
F23HillSim2645$PlotCN <- all2645cc23  $PlotCN
F23HillSim2645 <- F23HillSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2645) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4545 <- link(HillSplit23_G, data=all4545cc23 )
simmeanHill4545 <- data.frame(apply(simHill4545,2,mean))
PIHill4545 <- t(data.frame(apply(simHill4545,2,PI,prob=0.89)))
F23HillSim4545 <- data.frame(cbind(simmeanHill4545,PIHill4545))
F23HillSim4545$PlotCN <- all4545cc23  $PlotCN
F23HillSim4545 <- F23HillSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4545) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8545 <- link(HillSplit23_G, data=all8545cc23 )
simmeanHill8545 <- data.frame(apply(simHill8545,2,mean))
PIHill8545 <- t(data.frame(apply(simHill8545,2,PI,prob=0.89)))
F23HillSim8545 <- data.frame(cbind(simmeanHill8545,PIHill8545))
F23HillSim8545$PlotCN <- all8545cc23  $PlotCN
F23HillSim8545 <- F23HillSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8545) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2650 <- link(HillSplit23_G, data=all2650cc23 )
simmeanHill2650 <- data.frame(apply(simHill2650,2,mean))
PIHill2650 <- t(data.frame(apply(simHill2650,2,PI,prob=0.89)))
F23HillSim2650 <- data.frame(cbind(simmeanHill2650,PIHill2650))
F23HillSim2650$PlotCN <- all2650cc23  $PlotCN
F23HillSim2650 <- F23HillSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2650) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4550 <- link(HillSplit23_G, data=all4550cc23 )
simmeanHill4550 <- data.frame(apply(simHill4550,2,mean))
PIHill4550 <- t(data.frame(apply(simHill4550,2,PI,prob=0.89)))
F23HillSim4550 <- data.frame(cbind(simmeanHill4550,PIHill4550))
F23HillSim4550$PlotCN <- all4550cc23  $PlotCN
F23HillSim4550 <- F23HillSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4550) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8550 <- link(HillSplit23_G, data=all8550cc23 )
simmeanHill8550 <- data.frame(apply(simHill8550,2,mean))
PIHill8550 <- t(data.frame(apply(simHill8550,2,PI,prob=0.89)))
F23HillSim8550 <- data.frame(cbind(simmeanHill8550,PIHill8550))
F23HillSim8550$PlotCN <- all8550cc23  $PlotCN
F23HillSim8550 <- F23HillSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8550) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2655 <- link(HillSplit23_G, data=all2655cc23 )
simmeanHill2655 <- data.frame(apply(simHill2655,2,mean))
PIHill2655 <- t(data.frame(apply(simHill2655,2,PI,prob=0.89)))
F23HillSim2655 <- data.frame(cbind(simmeanHill2655,PIHill2655))
F23HillSim2655$PlotCN <- all2655cc23  $PlotCN
F23HillSim2655 <- F23HillSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2655) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4555 <- link(HillSplit23_G, data=all4555cc23 )
simmeanHill4555 <- data.frame(apply(simHill4555,2,mean))
PIHill4555 <- t(data.frame(apply(simHill4555,2,PI,prob=0.89)))
F23HillSim4555 <- data.frame(cbind(simmeanHill4555,PIHill4555))
F23HillSim4555$PlotCN <- all4555cc23  $PlotCN
F23HillSim4555 <- F23HillSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4555) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8555 <- link(HillSplit23_G, data=all8555cc23 )
simmeanHill8555 <- data.frame(apply(simHill8555,2,mean))
PIHill8555 <- t(data.frame(apply(simHill8555,2,PI,prob=0.89)))
F23HillSim8555 <- data.frame(cbind(simmeanHill8555,PIHill8555))
F23HillSim8555$PlotCN <- all8555cc23  $PlotCN
F23HillSim8555 <- F23HillSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8555) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2660 <- link(HillSplit23_G, data=all2660cc23 )
simmeanHill2660 <- data.frame(apply(simHill2660,2,mean))
PIHill2660 <- t(data.frame(apply(simHill2660,2,PI,prob=0.89)))
F23HillSim2660 <- data.frame(cbind(simmeanHill2660,PIHill2660))
F23HillSim2660$PlotCN <- all2660cc23  $PlotCN
F23HillSim2660 <- F23HillSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2660) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4560 <- link(HillSplit23_G, data=all4560cc23 )
simmeanHill4560 <- data.frame(apply(simHill4560,2,mean))
PIHill4560 <- t(data.frame(apply(simHill4560,2,PI,prob=0.89)))
F23HillSim4560 <- data.frame(cbind(simmeanHill4560,PIHill4560))
F23HillSim4560$PlotCN <- all4560cc23  $PlotCN
F23HillSim4560 <- F23HillSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4560) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8560 <- link(HillSplit23_G, data=all8560cc23 )
simmeanHill8560 <- data.frame(apply(simHill8560,2,mean))
PIHill8560 <- t(data.frame(apply(simHill8560,2,PI,prob=0.89)))
F23HillSim8560 <- data.frame(cbind(simmeanHill8560,PIHill8560))
F23HillSim8560$PlotCN <- all8560cc23  $PlotCN
F23HillSim8560 <- F23HillSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8560) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2665 <- link(HillSplit23_G, data=all2665cc23 )
simmeanHill2665 <- data.frame(apply(simHill2665,2,mean))
PIHill2665 <- t(data.frame(apply(simHill2665,2,PI,prob=0.89)))
F23HillSim2665 <- data.frame(cbind(simmeanHill2665,PIHill2665))
F23HillSim2665$PlotCN <- all2665cc23  $PlotCN
F23HillSim2665 <- F23HillSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2665) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4565 <- link(HillSplit23_G, data=all4565cc23 )
simmeanHill4565 <- data.frame(apply(simHill4565,2,mean))
PIHill4565 <- t(data.frame(apply(simHill4565,2,PI,prob=0.89)))
F23HillSim4565 <- data.frame(cbind(simmeanHill4565,PIHill4565))
F23HillSim4565$PlotCN <- all4565cc23  $PlotCN
F23HillSim4565 <- F23HillSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4565) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8565 <- link(HillSplit23_G, data=all8565cc23 )
simmeanHill8565 <- data.frame(apply(simHill8565,2,mean))
PIHill8565 <- t(data.frame(apply(simHill8565,2,PI,prob=0.89)))
F23HillSim8565 <- data.frame(cbind(simmeanHill8565,PIHill8565))
F23HillSim8565$PlotCN <- all8565cc23  $PlotCN
F23HillSim8565 <- F23HillSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8565) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2670 <- link(HillSplit23_G, data=all2670cc23 )
simmeanHill2670 <- data.frame(apply(simHill2670,2,mean))
PIHill2670 <- t(data.frame(apply(simHill2670,2,PI,prob=0.89)))
F23HillSim2670 <- data.frame(cbind(simmeanHill2670,PIHill2670))
F23HillSim2670$PlotCN <- all2670cc23  $PlotCN
F23HillSim2670 <- F23HillSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2670) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4570 <- link(HillSplit23_G, data=all4570cc23 )
simmeanHill4570 <- data.frame(apply(simHill4570,2,mean))
PIHill4570 <- t(data.frame(apply(simHill4570,2,PI,prob=0.89)))
F23HillSim4570 <- data.frame(cbind(simmeanHill4570,PIHill4570))
F23HillSim4570$PlotCN <- all4570cc23  $PlotCN
F23HillSim4570 <- F23HillSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4570) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8570 <- link(HillSplit23_G, data=all8570cc23 )
simmeanHill8570 <- data.frame(apply(simHill8570,2,mean))
PIHill8570 <- t(data.frame(apply(simHill8570,2,PI,prob=0.89)))
F23HillSim8570 <- data.frame(cbind(simmeanHill8570,PIHill8570))
F23HillSim8570$PlotCN <- all8570cc23  $PlotCN
F23HillSim8570 <- F23HillSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8570) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2675 <- link(HillSplit23_G, data=all2675cc23 )
simmeanHill2675 <- data.frame(apply(simHill2675,2,mean))
PIHill2675 <- t(data.frame(apply(simHill2675,2,PI,prob=0.89)))
F23HillSim2675 <- data.frame(cbind(simmeanHill2675,PIHill2675))
F23HillSim2675$PlotCN <- all2675cc23  $PlotCN
F23HillSim2675 <- F23HillSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2675) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4575 <- link(HillSplit23_G, data=all4575cc23 )
simmeanHill4575 <- data.frame(apply(simHill4575,2,mean))
PIHill4575 <- t(data.frame(apply(simHill4575,2,PI,prob=0.89)))
F23HillSim4575 <- data.frame(cbind(simmeanHill4575,PIHill4575))
F23HillSim4575$PlotCN <- all4575cc23  $PlotCN
F23HillSim4575 <- F23HillSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4575) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8575 <- link(HillSplit23_G, data=all8575cc23 )
simmeanHill8575 <- data.frame(apply(simHill8575,2,mean))
PIHill8575 <- t(data.frame(apply(simHill8575,2,PI,prob=0.89)))
F23HillSim8575 <- data.frame(cbind(simmeanHill8575,PIHill8575))
F23HillSim8575$PlotCN <- all8575cc23  $PlotCN
F23HillSim8575 <- F23HillSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8575) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2680 <- link(HillSplit23_G, data=all2680cc23 )
simmeanHill2680 <- data.frame(apply(simHill2680,2,mean))
PIHill2680 <- t(data.frame(apply(simHill2680,2,PI,prob=0.89)))
F23HillSim2680 <- data.frame(cbind(simmeanHill2680,PIHill2680))
F23HillSim2680$PlotCN <- all2680cc23  $PlotCN
F23HillSim2680 <- F23HillSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim2680) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4580 <- link(HillSplit23_G, data=all4580cc23 )
simmeanHill4580 <- data.frame(apply(simHill4580,2,mean))
PIHill4580 <- t(data.frame(apply(simHill4580,2,PI,prob=0.89)))
F23HillSim4580 <- data.frame(cbind(simmeanHill4580,PIHill4580))
F23HillSim4580$PlotCN <- all4580cc23  $PlotCN
F23HillSim4580 <- F23HillSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim4580) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8580 <- link(HillSplit23_G, data=all8580cc23 )
simmeanHill8580 <- data.frame(apply(simHill8580,2,mean))
PIHill8580 <- t(data.frame(apply(simHill8580,2,PI,prob=0.89)))
F23HillSim8580 <- data.frame(cbind(simmeanHill8580,PIHill8580))
F23HillSim8580$PlotCN <- all8580cc23  $PlotCN
F23HillSim8580 <- F23HillSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F23HillSim8580) <- c("PlotCN","HillPerAcre","5CI","95CI")

#create vector of new colnames
HillSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Hill", "5CI", "95CI"), x)))
HillSimnames<- append(HillSimnames,"PlotCN",after=0)

#group data by RCP pathway
F23HillSim26 <- F23HillSim2625 %>% cbind(F23HillSim2630[,2:4]) %>%
  cbind(F23HillSim2635[,2:4]) %>% cbind(F23HillSim2640[,2:4]) %>% cbind(F23HillSim2645[,2:4]) %>%
  cbind(F23HillSim2650[,2:4]) %>% cbind(F23HillSim2655[,2:4]) %>% cbind(F23HillSim2660[,2:4]) %>%
  cbind(F23HillSim2665[,2:4]) %>% cbind(F23HillSim2670[,2:4]) %>% cbind(F23HillSim2675[,2:4]) %>%
  cbind(F23HillSim2680[,2:4])
colnames(F23HillSim26) <- HillSimnames
write.csv(F23HillSim26,file="F23HillPred26.csv")

F23HillSim45 <- F23HillSim4525 %>% cbind(F23HillSim4530[,2:4]) %>%
  cbind(F23HillSim4535[,2:4]) %>% cbind(F23HillSim4540[,2:4]) %>% cbind(F23HillSim4545[,2:4]) %>%
  cbind(F23HillSim4550[,2:4]) %>% cbind(F23HillSim4555[,2:4]) %>% cbind(F23HillSim4560[,2:4]) %>%
  cbind(F23HillSim4565[,2:4]) %>% cbind(F23HillSim4570[,2:4]) %>% cbind(F23HillSim4575[,2:4]) %>%
  cbind(F23HillSim4580[,2:4])
colnames(F23HillSim45) <- HillSimnames
write.csv(F23HillSim45,file="F23HillPred45.csv")

F23HillSim85 <- F23HillSim8525 %>% cbind(F23HillSim8530[,2:4]) %>%
  cbind(F23HillSim8535[,2:4]) %>% cbind(F23HillSim8540[,2:4]) %>% cbind(F23HillSim8545[,2:4]) %>%
  cbind(F23HillSim8550[,2:4]) %>% cbind(F23HillSim8555[,2:4]) %>% cbind(F23HillSim8560[,2:4]) %>%
  cbind(F23HillSim8565[,2:4]) %>% cbind(F23HillSim8570[,2:4]) %>% cbind(F23HillSim8575[,2:4]) %>%
  cbind(F23HillSim8580[,2:4])
colnames(F23HillSim85) <- HillSimnames
write.csv(F23HillSim85,file="F23HillPred85.csv")


#fgroup 24
simHill2625 <- link(HillSplit24_G, data=all2625cc24 )
simmeanHill2625 <- data.frame(apply(simHill2625,2,mean))
PIHill2625 <- t(data.frame(apply(simHill2625,2,PI,prob=0.89)))
F24HillSim2625 <- data.frame(cbind(simmeanHill2625,PIHill2625))
F24HillSim2625$PlotCN <- all2625cc24  $PlotCN
F24HillSim2625 <- F24HillSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2625) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4525 <- link(HillSplit24_G, data=all4525cc24 )
simmeanHill4525 <- data.frame(apply(simHill4525,2,mean))
PIHill4525 <- t(data.frame(apply(simHill4525,2,PI,prob=0.89)))
F24HillSim4525 <- data.frame(cbind(simmeanHill4525,PIHill4525))
F24HillSim4525$PlotCN <- all4525cc24  $PlotCN
F24HillSim4525 <- F24HillSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4525) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8525 <- link(HillSplit24_G, data=all8525cc24 )
simmeanHill8525 <- data.frame(apply(simHill8525,2,mean))
PIHill8525 <- t(data.frame(apply(simHill8525,2,PI,prob=0.89)))
F24HillSim8525 <- data.frame(cbind(simmeanHill8525,PIHill8525))
F24HillSim8525$PlotCN <- all8525cc24  $PlotCN
F24HillSim8525 <- F24HillSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8525) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2630 <- link(HillSplit24_G, data=all2630cc24 )
simmeanHill2630 <- data.frame(apply(simHill2630,2,mean))
PIHill2630 <- t(data.frame(apply(simHill2630,2,PI,prob=0.89)))
F24HillSim2630 <- data.frame(cbind(simmeanHill2630,PIHill2630))
F24HillSim2630$PlotCN <- all2630cc24  $PlotCN
F24HillSim2630 <- F24HillSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2630) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4530 <- link(HillSplit24_G, data=all4530cc24 )
simmeanHill4530 <- data.frame(apply(simHill4530,2,mean))
PIHill4530 <- t(data.frame(apply(simHill4530,2,PI,prob=0.89)))
F24HillSim4530 <- data.frame(cbind(simmeanHill4530,PIHill4530))
F24HillSim4530$PlotCN <- all4530cc24  $PlotCN
F24HillSim4530 <- F24HillSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4530) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8530 <- link(HillSplit24_G, data=all8530cc24 )
simmeanHill8530 <- data.frame(apply(simHill8530,2,mean))
PIHill8530 <- t(data.frame(apply(simHill8530,2,PI,prob=0.89)))
F24HillSim8530 <- data.frame(cbind(simmeanHill8530,PIHill8530))
F24HillSim8530$PlotCN <- all8530cc24  $PlotCN
F24HillSim8530 <- F24HillSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8530) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2635 <- link(HillSplit24_G, data=all2635cc24 )
simmeanHill2635 <- data.frame(apply(simHill2635,2,mean))
PIHill2635 <- t(data.frame(apply(simHill2635,2,PI,prob=0.89)))
F24HillSim2635 <- data.frame(cbind(simmeanHill2635,PIHill2635))
F24HillSim2635$PlotCN <- all2635cc24  $PlotCN
F24HillSim2635 <- F24HillSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2635) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4535 <- link(HillSplit24_G, data=all4535cc24 )
simmeanHill4535 <- data.frame(apply(simHill4535,2,mean))
PIHill4535 <- t(data.frame(apply(simHill4535,2,PI,prob=0.89)))
F24HillSim4535 <- data.frame(cbind(simmeanHill4535,PIHill4535))
F24HillSim4535$PlotCN <- all4535cc24  $PlotCN
F24HillSim4535 <- F24HillSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4535) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8535 <- link(HillSplit24_G, data=all8535cc24 )
simmeanHill8535 <- data.frame(apply(simHill8535,2,mean))
PIHill8535 <- t(data.frame(apply(simHill8535,2,PI,prob=0.89)))
F24HillSim8535 <- data.frame(cbind(simmeanHill8535,PIHill8535))
F24HillSim8535$PlotCN <- all8535cc24  $PlotCN
F24HillSim8535 <- F24HillSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8535) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2640 <- link(HillSplit24_G, data=all2640cc24 )
simmeanHill2640 <- data.frame(apply(simHill2640,2,mean))
PIHill2640 <- t(data.frame(apply(simHill2640,2,PI,prob=0.89)))
F24HillSim2640 <- data.frame(cbind(simmeanHill2640,PIHill2640))
F24HillSim2640$PlotCN <- all2640cc24  $PlotCN
F24HillSim2640 <- F24HillSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2640) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4540 <- link(HillSplit24_G, data=all4540cc24 )
simmeanHill4540 <- data.frame(apply(simHill4540,2,mean))
PIHill4540 <- t(data.frame(apply(simHill4540,2,PI,prob=0.89)))
F24HillSim4540 <- data.frame(cbind(simmeanHill4540,PIHill4540))
F24HillSim4540$PlotCN <- all4540cc24  $PlotCN
F24HillSim4540 <- F24HillSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4540) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8540 <- link(HillSplit24_G, data=all8540cc24 )
simmeanHill8540 <- data.frame(apply(simHill8540,2,mean))
PIHill8540 <- t(data.frame(apply(simHill8540,2,PI,prob=0.89)))
F24HillSim8540 <- data.frame(cbind(simmeanHill8540,PIHill8540))
F24HillSim8540$PlotCN <- all8540cc24  $PlotCN
F24HillSim8540 <- F24HillSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8540) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2645 <- link(HillSplit24_G, data=all2645cc24 )
simmeanHill2645 <- data.frame(apply(simHill2645,2,mean))
PIHill2645 <- t(data.frame(apply(simHill2645,2,PI,prob=0.89)))
F24HillSim2645 <- data.frame(cbind(simmeanHill2645,PIHill2645))
F24HillSim2645$PlotCN <- all2645cc24  $PlotCN
F24HillSim2645 <- F24HillSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2645) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4545 <- link(HillSplit24_G, data=all4545cc24 )
simmeanHill4545 <- data.frame(apply(simHill4545,2,mean))
PIHill4545 <- t(data.frame(apply(simHill4545,2,PI,prob=0.89)))
F24HillSim4545 <- data.frame(cbind(simmeanHill4545,PIHill4545))
F24HillSim4545$PlotCN <- all4545cc24  $PlotCN
F24HillSim4545 <- F24HillSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4545) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8545 <- link(HillSplit24_G, data=all8545cc24 )
simmeanHill8545 <- data.frame(apply(simHill8545,2,mean))
PIHill8545 <- t(data.frame(apply(simHill8545,2,PI,prob=0.89)))
F24HillSim8545 <- data.frame(cbind(simmeanHill8545,PIHill8545))
F24HillSim8545$PlotCN <- all8545cc24  $PlotCN
F24HillSim8545 <- F24HillSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8545) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2650 <- link(HillSplit24_G, data=all2650cc24 )
simmeanHill2650 <- data.frame(apply(simHill2650,2,mean))
PIHill2650 <- t(data.frame(apply(simHill2650,2,PI,prob=0.89)))
F24HillSim2650 <- data.frame(cbind(simmeanHill2650,PIHill2650))
F24HillSim2650$PlotCN <- all2650cc24  $PlotCN
F24HillSim2650 <- F24HillSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2650) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4550 <- link(HillSplit24_G, data=all4550cc24 )
simmeanHill4550 <- data.frame(apply(simHill4550,2,mean))
PIHill4550 <- t(data.frame(apply(simHill4550,2,PI,prob=0.89)))
F24HillSim4550 <- data.frame(cbind(simmeanHill4550,PIHill4550))
F24HillSim4550$PlotCN <- all4550cc24  $PlotCN
F24HillSim4550 <- F24HillSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4550) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8550 <- link(HillSplit24_G, data=all8550cc24 )
simmeanHill8550 <- data.frame(apply(simHill8550,2,mean))
PIHill8550 <- t(data.frame(apply(simHill8550,2,PI,prob=0.89)))
F24HillSim8550 <- data.frame(cbind(simmeanHill8550,PIHill8550))
F24HillSim8550$PlotCN <- all8550cc24  $PlotCN
F24HillSim8550 <- F24HillSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8550) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2655 <- link(HillSplit24_G, data=all2655cc24 )
simmeanHill2655 <- data.frame(apply(simHill2655,2,mean))
PIHill2655 <- t(data.frame(apply(simHill2655,2,PI,prob=0.89)))
F24HillSim2655 <- data.frame(cbind(simmeanHill2655,PIHill2655))
F24HillSim2655$PlotCN <- all2655cc24  $PlotCN
F24HillSim2655 <- F24HillSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2655) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4555 <- link(HillSplit24_G, data=all4555cc24 )
simmeanHill4555 <- data.frame(apply(simHill4555,2,mean))
PIHill4555 <- t(data.frame(apply(simHill4555,2,PI,prob=0.89)))
F24HillSim4555 <- data.frame(cbind(simmeanHill4555,PIHill4555))
F24HillSim4555$PlotCN <- all4555cc24  $PlotCN
F24HillSim4555 <- F24HillSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4555) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8555 <- link(HillSplit24_G, data=all8555cc24 )
simmeanHill8555 <- data.frame(apply(simHill8555,2,mean))
PIHill8555 <- t(data.frame(apply(simHill8555,2,PI,prob=0.89)))
F24HillSim8555 <- data.frame(cbind(simmeanHill8555,PIHill8555))
F24HillSim8555$PlotCN <- all8555cc24  $PlotCN
F24HillSim8555 <- F24HillSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8555) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2660 <- link(HillSplit24_G, data=all2660cc24 )
simmeanHill2660 <- data.frame(apply(simHill2660,2,mean))
PIHill2660 <- t(data.frame(apply(simHill2660,2,PI,prob=0.89)))
F24HillSim2660 <- data.frame(cbind(simmeanHill2660,PIHill2660))
F24HillSim2660$PlotCN <- all2660cc24  $PlotCN
F24HillSim2660 <- F24HillSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2660) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4560 <- link(HillSplit24_G, data=all4560cc24 )
simmeanHill4560 <- data.frame(apply(simHill4560,2,mean))
PIHill4560 <- t(data.frame(apply(simHill4560,2,PI,prob=0.89)))
F24HillSim4560 <- data.frame(cbind(simmeanHill4560,PIHill4560))
F24HillSim4560$PlotCN <- all4560cc24  $PlotCN
F24HillSim4560 <- F24HillSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4560) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8560 <- link(HillSplit24_G, data=all8560cc24 )
simmeanHill8560 <- data.frame(apply(simHill8560,2,mean))
PIHill8560 <- t(data.frame(apply(simHill8560,2,PI,prob=0.89)))
F24HillSim8560 <- data.frame(cbind(simmeanHill8560,PIHill8560))
F24HillSim8560$PlotCN <- all8560cc24  $PlotCN
F24HillSim8560 <- F24HillSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8560) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2665 <- link(HillSplit24_G, data=all2665cc24 )
simmeanHill2665 <- data.frame(apply(simHill2665,2,mean))
PIHill2665 <- t(data.frame(apply(simHill2665,2,PI,prob=0.89)))
F24HillSim2665 <- data.frame(cbind(simmeanHill2665,PIHill2665))
F24HillSim2665$PlotCN <- all2665cc24  $PlotCN
F24HillSim2665 <- F24HillSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2665) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4565 <- link(HillSplit24_G, data=all4565cc24 )
simmeanHill4565 <- data.frame(apply(simHill4565,2,mean))
PIHill4565 <- t(data.frame(apply(simHill4565,2,PI,prob=0.89)))
F24HillSim4565 <- data.frame(cbind(simmeanHill4565,PIHill4565))
F24HillSim4565$PlotCN <- all4565cc24  $PlotCN
F24HillSim4565 <- F24HillSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4565) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8565 <- link(HillSplit24_G, data=all8565cc24 )
simmeanHill8565 <- data.frame(apply(simHill8565,2,mean))
PIHill8565 <- t(data.frame(apply(simHill8565,2,PI,prob=0.89)))
F24HillSim8565 <- data.frame(cbind(simmeanHill8565,PIHill8565))
F24HillSim8565$PlotCN <- all8565cc24  $PlotCN
F24HillSim8565 <- F24HillSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8565) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2670 <- link(HillSplit24_G, data=all2670cc24 )
simmeanHill2670 <- data.frame(apply(simHill2670,2,mean))
PIHill2670 <- t(data.frame(apply(simHill2670,2,PI,prob=0.89)))
F24HillSim2670 <- data.frame(cbind(simmeanHill2670,PIHill2670))
F24HillSim2670$PlotCN <- all2670cc24  $PlotCN
F24HillSim2670 <- F24HillSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2670) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4570 <- link(HillSplit24_G, data=all4570cc24 )
simmeanHill4570 <- data.frame(apply(simHill4570,2,mean))
PIHill4570 <- t(data.frame(apply(simHill4570,2,PI,prob=0.89)))
F24HillSim4570 <- data.frame(cbind(simmeanHill4570,PIHill4570))
F24HillSim4570$PlotCN <- all4570cc24  $PlotCN
F24HillSim4570 <- F24HillSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4570) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8570 <- link(HillSplit24_G, data=all8570cc24 )
simmeanHill8570 <- data.frame(apply(simHill8570,2,mean))
PIHill8570 <- t(data.frame(apply(simHill8570,2,PI,prob=0.89)))
F24HillSim8570 <- data.frame(cbind(simmeanHill8570,PIHill8570))
F24HillSim8570$PlotCN <- all8570cc24  $PlotCN
F24HillSim8570 <- F24HillSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8570) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2675 <- link(HillSplit24_G, data=all2675cc24 )
simmeanHill2675 <- data.frame(apply(simHill2675,2,mean))
PIHill2675 <- t(data.frame(apply(simHill2675,2,PI,prob=0.89)))
F24HillSim2675 <- data.frame(cbind(simmeanHill2675,PIHill2675))
F24HillSim2675$PlotCN <- all2675cc24  $PlotCN
F24HillSim2675 <- F24HillSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2675) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4575 <- link(HillSplit24_G, data=all4575cc24 )
simmeanHill4575 <- data.frame(apply(simHill4575,2,mean))
PIHill4575 <- t(data.frame(apply(simHill4575,2,PI,prob=0.89)))
F24HillSim4575 <- data.frame(cbind(simmeanHill4575,PIHill4575))
F24HillSim4575$PlotCN <- all4575cc24  $PlotCN
F24HillSim4575 <- F24HillSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4575) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8575 <- link(HillSplit24_G, data=all8575cc24 )
simmeanHill8575 <- data.frame(apply(simHill8575,2,mean))
PIHill8575 <- t(data.frame(apply(simHill8575,2,PI,prob=0.89)))
F24HillSim8575 <- data.frame(cbind(simmeanHill8575,PIHill8575))
F24HillSim8575$PlotCN <- all8575cc24  $PlotCN
F24HillSim8575 <- F24HillSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8575) <- c("PlotCN","HillPerAcre","5CI","95CI")

simHill2680 <- link(HillSplit24_G, data=all2680cc24 )
simmeanHill2680 <- data.frame(apply(simHill2680,2,mean))
PIHill2680 <- t(data.frame(apply(simHill2680,2,PI,prob=0.89)))
F24HillSim2680 <- data.frame(cbind(simmeanHill2680,PIHill2680))
F24HillSim2680$PlotCN <- all2680cc24  $PlotCN
F24HillSim2680 <- F24HillSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim2680) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill4580 <- link(HillSplit24_G, data=all4580cc24 )
simmeanHill4580 <- data.frame(apply(simHill4580,2,mean))
PIHill4580 <- t(data.frame(apply(simHill4580,2,PI,prob=0.89)))
F24HillSim4580 <- data.frame(cbind(simmeanHill4580,PIHill4580))
F24HillSim4580$PlotCN <- all4580cc24  $PlotCN
F24HillSim4580 <- F24HillSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim4580) <- c("PlotCN","HillPerAcre","5CI","95CI")
simHill8580 <- link(HillSplit24_G, data=all8580cc24 )
simmeanHill8580 <- data.frame(apply(simHill8580,2,mean))
PIHill8580 <- t(data.frame(apply(simHill8580,2,PI,prob=0.89)))
F24HillSim8580 <- data.frame(cbind(simmeanHill8580,PIHill8580))
F24HillSim8580$PlotCN <- all8580cc24  $PlotCN
F24HillSim8580 <- F24HillSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F24HillSim8580) <- c("PlotCN","HillPerAcre","5CI","95CI")

#create vector of new colnames
HillSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Hill", "5CI", "95CI"), x)))
HillSimnames<- append(HillSimnames,"PlotCN",after=0)

#group data by RCP pathway
F24HillSim26 <- F24HillSim2625 %>% cbind(F24HillSim2630[,2:4]) %>%
  cbind(F24HillSim2635[,2:4]) %>% cbind(F24HillSim2640[,2:4]) %>% cbind(F24HillSim2645[,2:4]) %>%
  cbind(F24HillSim2650[,2:4]) %>% cbind(F24HillSim2655[,2:4]) %>% cbind(F24HillSim2660[,2:4]) %>%
  cbind(F24HillSim2665[,2:4]) %>% cbind(F24HillSim2670[,2:4]) %>% cbind(F24HillSim2675[,2:4]) %>%
  cbind(F24HillSim2680[,2:4])
colnames(F24HillSim26) <- HillSimnames
write.csv(F24HillSim26,file="F24HillPred26.csv")

F24HillSim45 <- F24HillSim4525 %>% cbind(F24HillSim4530[,2:4]) %>%
  cbind(F24HillSim4535[,2:4]) %>% cbind(F24HillSim4540[,2:4]) %>% cbind(F24HillSim4545[,2:4]) %>%
  cbind(F24HillSim4550[,2:4]) %>% cbind(F24HillSim4555[,2:4]) %>% cbind(F24HillSim4560[,2:4]) %>%
  cbind(F24HillSim4565[,2:4]) %>% cbind(F24HillSim4570[,2:4]) %>% cbind(F24HillSim4575[,2:4]) %>%
  cbind(F24HillSim4580[,2:4])
colnames(F24HillSim45) <- HillSimnames
write.csv(F24HillSim45,file="F24HillPred45.csv")

F24HillSim85 <- F24HillSim8525 %>% cbind(F24HillSim8530[,2:4]) %>%
  cbind(F24HillSim8535[,2:4]) %>% cbind(F24HillSim8540[,2:4]) %>% cbind(F24HillSim8545[,2:4]) %>%
  cbind(F24HillSim8550[,2:4]) %>% cbind(F24HillSim8555[,2:4]) %>% cbind(F24HillSim8560[,2:4]) %>%
  cbind(F24HillSim8565[,2:4]) %>% cbind(F24HillSim8570[,2:4]) %>% cbind(F24HillSim8575[,2:4]) %>%
  cbind(F24HillSim8580[,2:4])
colnames(F24HillSim85) <- HillSimnames
write.csv(F24HillSim85,file="F24HillPred85.csv")

#Jaccards Predictions
#fgroup 1
simJacc2625 <- link(JaccSplit1_beta, data=all2625cc1)
simmeanJacc2625 <- data.frame(apply(simJacc2625,2,mean))
PIJacc2625 <- t(data.frame(apply(simJacc2625,2,PI,prob=0.89)))
F1JaccSim2625 <- data.frame(cbind(simmeanJacc2625,PIJacc2625))
F1JaccSim2625$PlotCN <- all2625cc1 $PlotCN
F1JaccSim2625 <- F1JaccSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2625) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4525 <- link(JaccSplit1_beta, data=all4525cc1)
simmeanJacc4525 <- data.frame(apply(simJacc4525,2,mean))
PIJacc4525 <- t(data.frame(apply(simJacc4525,2,PI,prob=0.89)))
F1JaccSim4525 <- data.frame(cbind(simmeanJacc4525,PIJacc4525))
F1JaccSim4525$PlotCN <- all4525cc1 $PlotCN
F1JaccSim4525 <- F1JaccSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4525) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8525 <- link(JaccSplit1_beta, data=all8525cc1)
simmeanJacc8525 <- data.frame(apply(simJacc8525,2,mean))
PIJacc8525 <- t(data.frame(apply(simJacc8525,2,PI,prob=0.89)))
F1JaccSim8525 <- data.frame(cbind(simmeanJacc8525,PIJacc8525))
F1JaccSim8525$PlotCN <- all8525cc1 $PlotCN
F1JaccSim8525 <- F1JaccSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8525) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2630 <- link(JaccSplit1_beta, data=all2630cc1)
simmeanJacc2630 <- data.frame(apply(simJacc2630,2,mean))
PIJacc2630 <- t(data.frame(apply(simJacc2630,2,PI,prob=0.89)))
F1JaccSim2630 <- data.frame(cbind(simmeanJacc2630,PIJacc2630))
F1JaccSim2630$PlotCN <- all2630cc1 $PlotCN
F1JaccSim2630 <- F1JaccSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2630) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4530 <- link(JaccSplit1_beta, data=all4530cc1)
simmeanJacc4530 <- data.frame(apply(simJacc4530,2,mean))
PIJacc4530 <- t(data.frame(apply(simJacc4530,2,PI,prob=0.89)))
F1JaccSim4530 <- data.frame(cbind(simmeanJacc4530,PIJacc4530))
F1JaccSim4530$PlotCN <- all4530cc1 $PlotCN
F1JaccSim4530 <- F1JaccSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4530) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8530 <- link(JaccSplit1_beta, data=all8530cc1)
simmeanJacc8530 <- data.frame(apply(simJacc8530,2,mean))
PIJacc8530 <- t(data.frame(apply(simJacc8530,2,PI,prob=0.89)))
F1JaccSim8530 <- data.frame(cbind(simmeanJacc8530,PIJacc8530))
F1JaccSim8530$PlotCN <- all8530cc1 $PlotCN
F1JaccSim8530 <- F1JaccSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8530) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2635 <- link(JaccSplit1_beta, data=all2635cc1)
simmeanJacc2635 <- data.frame(apply(simJacc2635,2,mean))
PIJacc2635 <- t(data.frame(apply(simJacc2635,2,PI,prob=0.89)))
F1JaccSim2635 <- data.frame(cbind(simmeanJacc2635,PIJacc2635))
F1JaccSim2635$PlotCN <- all2635cc1 $PlotCN
F1JaccSim2635 <- F1JaccSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2635) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4535 <- link(JaccSplit1_beta, data=all4535cc1)
simmeanJacc4535 <- data.frame(apply(simJacc4535,2,mean))
PIJacc4535 <- t(data.frame(apply(simJacc4535,2,PI,prob=0.89)))
F1JaccSim4535 <- data.frame(cbind(simmeanJacc4535,PIJacc4535))
F1JaccSim4535$PlotCN <- all4535cc1 $PlotCN
F1JaccSim4535 <- F1JaccSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4535) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8535 <- link(JaccSplit1_beta, data=all8535cc1)
simmeanJacc8535 <- data.frame(apply(simJacc8535,2,mean))
PIJacc8535 <- t(data.frame(apply(simJacc8535,2,PI,prob=0.89)))
F1JaccSim8535 <- data.frame(cbind(simmeanJacc8535,PIJacc8535))
F1JaccSim8535$PlotCN <- all8535cc1 $PlotCN
F1JaccSim8535 <- F1JaccSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8535) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2640 <- link(JaccSplit1_beta, data=all2640cc1)
simmeanJacc2640 <- data.frame(apply(simJacc2640,2,mean))
PIJacc2640 <- t(data.frame(apply(simJacc2640,2,PI,prob=0.89)))
F1JaccSim2640 <- data.frame(cbind(simmeanJacc2640,PIJacc2640))
F1JaccSim2640$PlotCN <- all2640cc1 $PlotCN
F1JaccSim2640 <- F1JaccSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2640) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4540 <- link(JaccSplit1_beta, data=all4540cc1)
simmeanJacc4540 <- data.frame(apply(simJacc4540,2,mean))
PIJacc4540 <- t(data.frame(apply(simJacc4540,2,PI,prob=0.89)))
F1JaccSim4540 <- data.frame(cbind(simmeanJacc4540,PIJacc4540))
F1JaccSim4540$PlotCN <- all4540cc1 $PlotCN
F1JaccSim4540 <- F1JaccSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4540) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8540 <- link(JaccSplit1_beta, data=all8540cc1)
simmeanJacc8540 <- data.frame(apply(simJacc8540,2,mean))
PIJacc8540 <- t(data.frame(apply(simJacc8540,2,PI,prob=0.89)))
F1JaccSim8540 <- data.frame(cbind(simmeanJacc8540,PIJacc8540))
F1JaccSim8540$PlotCN <- all8540cc1 $PlotCN
F1JaccSim8540 <- F1JaccSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8540) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2645 <- link(JaccSplit1_beta, data=all2645cc1)
simmeanJacc2645 <- data.frame(apply(simJacc2645,2,mean))
PIJacc2645 <- t(data.frame(apply(simJacc2645,2,PI,prob=0.89)))
F1JaccSim2645 <- data.frame(cbind(simmeanJacc2645,PIJacc2645))
F1JaccSim2645$PlotCN <- all2645cc1 $PlotCN
F1JaccSim2645 <- F1JaccSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2645) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4545 <- link(JaccSplit1_beta, data=all4545cc1)
simmeanJacc4545 <- data.frame(apply(simJacc4545,2,mean))
PIJacc4545 <- t(data.frame(apply(simJacc4545,2,PI,prob=0.89)))
F1JaccSim4545 <- data.frame(cbind(simmeanJacc4545,PIJacc4545))
F1JaccSim4545$PlotCN <- all4545cc1 $PlotCN
F1JaccSim4545 <- F1JaccSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4545) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8545 <- link(JaccSplit1_beta, data=all8545cc1)
simmeanJacc8545 <- data.frame(apply(simJacc8545,2,mean))
PIJacc8545 <- t(data.frame(apply(simJacc8545,2,PI,prob=0.89)))
F1JaccSim8545 <- data.frame(cbind(simmeanJacc8545,PIJacc8545))
F1JaccSim8545$PlotCN <- all8545cc1 $PlotCN
F1JaccSim8545 <- F1JaccSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8545) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2650 <- link(JaccSplit1_beta, data=all2650cc1)
simmeanJacc2650 <- data.frame(apply(simJacc2650,2,mean))
PIJacc2650 <- t(data.frame(apply(simJacc2650,2,PI,prob=0.89)))
F1JaccSim2650 <- data.frame(cbind(simmeanJacc2650,PIJacc2650))
F1JaccSim2650$PlotCN <- all2650cc1 $PlotCN
F1JaccSim2650 <- F1JaccSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2650) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4550 <- link(JaccSplit1_beta, data=all4550cc1)
simmeanJacc4550 <- data.frame(apply(simJacc4550,2,mean))
PIJacc4550 <- t(data.frame(apply(simJacc4550,2,PI,prob=0.89)))
F1JaccSim4550 <- data.frame(cbind(simmeanJacc4550,PIJacc4550))
F1JaccSim4550$PlotCN <- all4550cc1 $PlotCN
F1JaccSim4550 <- F1JaccSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4550) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8550 <- link(JaccSplit1_beta, data=all8550cc1)
simmeanJacc8550 <- data.frame(apply(simJacc8550,2,mean))
PIJacc8550 <- t(data.frame(apply(simJacc8550,2,PI,prob=0.89)))
F1JaccSim8550 <- data.frame(cbind(simmeanJacc8550,PIJacc8550))
F1JaccSim8550$PlotCN <- all8550cc1 $PlotCN
F1JaccSim8550 <- F1JaccSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8550) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2655 <- link(JaccSplit1_beta, data=all2655cc1)
simmeanJacc2655 <- data.frame(apply(simJacc2655,2,mean))
PIJacc2655 <- t(data.frame(apply(simJacc2655,2,PI,prob=0.89)))
F1JaccSim2655 <- data.frame(cbind(simmeanJacc2655,PIJacc2655))
F1JaccSim2655$PlotCN <- all2655cc1 $PlotCN
F1JaccSim2655 <- F1JaccSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2655) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4555 <- link(JaccSplit1_beta, data=all4555cc1)
simmeanJacc4555 <- data.frame(apply(simJacc4555,2,mean))
PIJacc4555 <- t(data.frame(apply(simJacc4555,2,PI,prob=0.89)))
F1JaccSim4555 <- data.frame(cbind(simmeanJacc4555,PIJacc4555))
F1JaccSim4555$PlotCN <- all4555cc1 $PlotCN
F1JaccSim4555 <- F1JaccSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4555) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8555 <- link(JaccSplit1_beta, data=all8555cc1)
simmeanJacc8555 <- data.frame(apply(simJacc8555,2,mean))
PIJacc8555 <- t(data.frame(apply(simJacc8555,2,PI,prob=0.89)))
F1JaccSim8555 <- data.frame(cbind(simmeanJacc8555,PIJacc8555))
F1JaccSim8555$PlotCN <- all8555cc1 $PlotCN
F1JaccSim8555 <- F1JaccSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8555) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2660 <- link(JaccSplit1_beta, data=all2660cc1)
simmeanJacc2660 <- data.frame(apply(simJacc2660,2,mean))
PIJacc2660 <- t(data.frame(apply(simJacc2660,2,PI,prob=0.89)))
F1JaccSim2660 <- data.frame(cbind(simmeanJacc2660,PIJacc2660))
F1JaccSim2660$PlotCN <- all2660cc1 $PlotCN
F1JaccSim2660 <- F1JaccSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2660) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4560 <- link(JaccSplit1_beta, data=all4560cc1)
simmeanJacc4560 <- data.frame(apply(simJacc4560,2,mean))
PIJacc4560 <- t(data.frame(apply(simJacc4560,2,PI,prob=0.89)))
F1JaccSim4560 <- data.frame(cbind(simmeanJacc4560,PIJacc4560))
F1JaccSim4560$PlotCN <- all4560cc1 $PlotCN
F1JaccSim4560 <- F1JaccSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4560) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8560 <- link(JaccSplit1_beta, data=all8560cc1)
simmeanJacc8560 <- data.frame(apply(simJacc8560,2,mean))
PIJacc8560 <- t(data.frame(apply(simJacc8560,2,PI,prob=0.89)))
F1JaccSim8560 <- data.frame(cbind(simmeanJacc8560,PIJacc8560))
F1JaccSim8560$PlotCN <- all8560cc1 $PlotCN
F1JaccSim8560 <- F1JaccSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8560) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2665 <- link(JaccSplit1_beta, data=all2665cc1)
simmeanJacc2665 <- data.frame(apply(simJacc2665,2,mean))
PIJacc2665 <- t(data.frame(apply(simJacc2665,2,PI,prob=0.89)))
F1JaccSim2665 <- data.frame(cbind(simmeanJacc2665,PIJacc2665))
F1JaccSim2665$PlotCN <- all2665cc1 $PlotCN
F1JaccSim2665 <- F1JaccSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2665) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4565 <- link(JaccSplit1_beta, data=all4565cc1)
simmeanJacc4565 <- data.frame(apply(simJacc4565,2,mean))
PIJacc4565 <- t(data.frame(apply(simJacc4565,2,PI,prob=0.89)))
F1JaccSim4565 <- data.frame(cbind(simmeanJacc4565,PIJacc4565))
F1JaccSim4565$PlotCN <- all4565cc1 $PlotCN
F1JaccSim4565 <- F1JaccSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4565) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8565 <- link(JaccSplit1_beta, data=all8565cc1)
simmeanJacc8565 <- data.frame(apply(simJacc8565,2,mean))
PIJacc8565 <- t(data.frame(apply(simJacc8565,2,PI,prob=0.89)))
F1JaccSim8565 <- data.frame(cbind(simmeanJacc8565,PIJacc8565))
F1JaccSim8565$PlotCN <- all8565cc1 $PlotCN
F1JaccSim8565 <- F1JaccSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8565) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2670 <- link(JaccSplit1_beta, data=all2670cc1)
simmeanJacc2670 <- data.frame(apply(simJacc2670,2,mean))
PIJacc2670 <- t(data.frame(apply(simJacc2670,2,PI,prob=0.89)))
F1JaccSim2670 <- data.frame(cbind(simmeanJacc2670,PIJacc2670))
F1JaccSim2670$PlotCN <- all2670cc1 $PlotCN
F1JaccSim2670 <- F1JaccSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2670) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4570 <- link(JaccSplit1_beta, data=all4570cc1)
simmeanJacc4570 <- data.frame(apply(simJacc4570,2,mean))
PIJacc4570 <- t(data.frame(apply(simJacc4570,2,PI,prob=0.89)))
F1JaccSim4570 <- data.frame(cbind(simmeanJacc4570,PIJacc4570))
F1JaccSim4570$PlotCN <- all4570cc1 $PlotCN
F1JaccSim4570 <- F1JaccSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4570) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8570 <- link(JaccSplit1_beta, data=all8570cc1)
simmeanJacc8570 <- data.frame(apply(simJacc8570,2,mean))
PIJacc8570 <- t(data.frame(apply(simJacc8570,2,PI,prob=0.89)))
F1JaccSim8570 <- data.frame(cbind(simmeanJacc8570,PIJacc8570))
F1JaccSim8570$PlotCN <- all8570cc1 $PlotCN
F1JaccSim8570 <- F1JaccSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8570) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2675 <- link(JaccSplit1_beta, data=all2675cc1)
simmeanJacc2675 <- data.frame(apply(simJacc2675,2,mean))
PIJacc2675 <- t(data.frame(apply(simJacc2675,2,PI,prob=0.89)))
F1JaccSim2675 <- data.frame(cbind(simmeanJacc2675,PIJacc2675))
F1JaccSim2675$PlotCN <- all2675cc1 $PlotCN
F1JaccSim2675 <- F1JaccSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2675) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4575 <- link(JaccSplit1_beta, data=all4575cc1)
simmeanJacc4575 <- data.frame(apply(simJacc4575,2,mean))
PIJacc4575 <- t(data.frame(apply(simJacc4575,2,PI,prob=0.89)))
F1JaccSim4575 <- data.frame(cbind(simmeanJacc4575,PIJacc4575))
F1JaccSim4575$PlotCN <- all4575cc1 $PlotCN
F1JaccSim4575 <- F1JaccSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4575) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8575 <- link(JaccSplit1_beta, data=all8575cc1)
simmeanJacc8575 <- data.frame(apply(simJacc8575,2,mean))
PIJacc8575 <- t(data.frame(apply(simJacc8575,2,PI,prob=0.89)))
F1JaccSim8575 <- data.frame(cbind(simmeanJacc8575,PIJacc8575))
F1JaccSim8575$PlotCN <- all8575cc1 $PlotCN
F1JaccSim8575 <- F1JaccSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8575) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2680 <- link(JaccSplit1_beta, data=all2680cc1)
simmeanJacc2680 <- data.frame(apply(simJacc2680,2,mean))
PIJacc2680 <- t(data.frame(apply(simJacc2680,2,PI,prob=0.89)))
F1JaccSim2680 <- data.frame(cbind(simmeanJacc2680,PIJacc2680))
F1JaccSim2680$PlotCN <- all2680cc1 $PlotCN
F1JaccSim2680 <- F1JaccSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim2680) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4580 <- link(JaccSplit1_beta, data=all4580cc1)
simmeanJacc4580 <- data.frame(apply(simJacc4580,2,mean))
PIJacc4580 <- t(data.frame(apply(simJacc4580,2,PI,prob=0.89)))
F1JaccSim4580 <- data.frame(cbind(simmeanJacc4580,PIJacc4580))
F1JaccSim4580$PlotCN <- all4580cc1 $PlotCN
F1JaccSim4580 <- F1JaccSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim4580) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8580 <- link(JaccSplit1_beta, data=all8580cc1)
simmeanJacc8580 <- data.frame(apply(simJacc8580,2,mean))
PIJacc8580 <- t(data.frame(apply(simJacc8580,2,PI,prob=0.89)))
F1JaccSim8580 <- data.frame(cbind(simmeanJacc8580,PIJacc8580))
F1JaccSim8580$PlotCN <- all8580cc1 $PlotCN
F1JaccSim8580 <- F1JaccSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F1JaccSim8580) <- c("PlotCN","Jaccards","5CI","95CI")


#create vector of new colnames
JaccSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Jacc", "5CI", "95CI"), x)))
JaccSimnames<- append(JaccSimnames,"PlotCN",after=0)

#group data by RCP pathway
F1JaccSim26 <- F1JaccSim2625 %>% cbind(F1JaccSim2630[,2:4]) %>%
  cbind(F1JaccSim2635[,2:4]) %>% cbind(F1JaccSim2640[,2:4]) %>% cbind(F1JaccSim2645[,2:4]) %>%
  cbind(F1JaccSim2650[,2:4]) %>% cbind(F1JaccSim2655[,2:4]) %>% cbind(F1JaccSim2660[,2:4]) %>%
  cbind(F1JaccSim2665[,2:4]) %>% cbind(F1JaccSim2670[,2:4]) %>% cbind(F1JaccSim2675[,2:4]) %>%
  cbind(F1JaccSim2680[,2:4])
colnames(F1JaccSim26) <- JaccSimnames
write.csv(F1JaccSim26,file="F1JaccPred26b.csv")

F1JaccSim45 <- F1JaccSim4525 %>% cbind(F1JaccSim4530[,2:4]) %>%
  cbind(F1JaccSim4535[,2:4]) %>% cbind(F1JaccSim4540[,2:4]) %>% cbind(F1JaccSim4545[,2:4]) %>%
  cbind(F1JaccSim4550[,2:4]) %>% cbind(F1JaccSim4555[,2:4]) %>% cbind(F1JaccSim4560[,2:4]) %>%
  cbind(F1JaccSim4565[,2:4]) %>% cbind(F1JaccSim4570[,2:4]) %>% cbind(F1JaccSim4575[,2:4]) %>%
  cbind(F1JaccSim4580[,2:4])
colnames(F1JaccSim45) <- JaccSimnames
write.csv(F1JaccSim45,file="F1JaccPred45b.csv")

F1JaccSim85 <- F1JaccSim8525 %>% cbind(F1JaccSim8530[,2:4]) %>%
  cbind(F1JaccSim8535[,2:4]) %>% cbind(F1JaccSim8540[,2:4]) %>% cbind(F1JaccSim8545[,2:4]) %>%
  cbind(F1JaccSim8550[,2:4]) %>% cbind(F1JaccSim8555[,2:4]) %>% cbind(F1JaccSim8560[,2:4]) %>%
  cbind(F1JaccSim8565[,2:4]) %>% cbind(F1JaccSim8570[,2:4]) %>% cbind(F1JaccSim8575[,2:4]) %>%
  cbind(F1JaccSim8580[,2:4])
colnames(F1JaccSim85) <- JaccSimnames
write.csv(F1JaccSim85,file="F1JaccPred85b.csv")


#fgroup 5
simJacc5625 <- link(JaccSplit5_beta, data=all2625cc5 )
simmeanJacc5625 <- data.frame(apply(simJacc5625,2,mean))
PIJacc5625 <- t(data.frame(apply(simJacc5625,2,PI,prob=0.89)))
F5JaccSim2625 <- data.frame(cbind(simmeanJacc5625,PIJacc5625))
F5JaccSim2625$PlotCN <- all2625cc5  $PlotCN
F5JaccSim2625 <- F5JaccSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2625) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4525 <- link(JaccSplit5_beta, data=all4525cc5 )
simmeanJacc4525 <- data.frame(apply(simJacc4525,2,mean))
PIJacc4525 <- t(data.frame(apply(simJacc4525,2,PI,prob=0.89)))
F5JaccSim4525 <- data.frame(cbind(simmeanJacc4525,PIJacc4525))
F5JaccSim4525$PlotCN <- all4525cc5  $PlotCN
F5JaccSim4525 <- F5JaccSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4525) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8525 <- link(JaccSplit5_beta, data=all8525cc5 )
simmeanJacc8525 <- data.frame(apply(simJacc8525,2,mean))
PIJacc8525 <- t(data.frame(apply(simJacc8525,2,PI,prob=0.89)))
F5JaccSim8525 <- data.frame(cbind(simmeanJacc8525,PIJacc8525))
F5JaccSim8525$PlotCN <- all8525cc5  $PlotCN
F5JaccSim8525 <- F5JaccSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8525) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc5630 <- link(JaccSplit5_beta, data=all2630cc5 )
simmeanJacc5630 <- data.frame(apply(simJacc5630,2,mean))
PIJacc5630 <- t(data.frame(apply(simJacc5630,2,PI,prob=0.89)))
F5JaccSim2630 <- data.frame(cbind(simmeanJacc5630,PIJacc5630))
F5JaccSim2630$PlotCN <- all2630cc5  $PlotCN
F5JaccSim2630 <- F5JaccSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2630) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4530 <- link(JaccSplit5_beta, data=all4530cc5 )
simmeanJacc4530 <- data.frame(apply(simJacc4530,2,mean))
PIJacc4530 <- t(data.frame(apply(simJacc4530,2,PI,prob=0.89)))
F5JaccSim4530 <- data.frame(cbind(simmeanJacc4530,PIJacc4530))
F5JaccSim4530$PlotCN <- all4530cc5  $PlotCN
F5JaccSim4530 <- F5JaccSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4530) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8530 <- link(JaccSplit5_beta, data=all8530cc5 )
simmeanJacc8530 <- data.frame(apply(simJacc8530,2,mean))
PIJacc8530 <- t(data.frame(apply(simJacc8530,2,PI,prob=0.89)))
F5JaccSim8530 <- data.frame(cbind(simmeanJacc8530,PIJacc8530))
F5JaccSim8530$PlotCN <- all8530cc5  $PlotCN
F5JaccSim8530 <- F5JaccSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8530) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc5635 <- link(JaccSplit5_beta, data=all2635cc5 )
simmeanJacc5635 <- data.frame(apply(simJacc5635,2,mean))
PIJacc5635 <- t(data.frame(apply(simJacc5635,2,PI,prob=0.89)))
F5JaccSim2635 <- data.frame(cbind(simmeanJacc5635,PIJacc5635))
F5JaccSim2635$PlotCN <- all2635cc5  $PlotCN
F5JaccSim2635 <- F5JaccSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2635) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4535 <- link(JaccSplit5_beta, data=all4535cc5 )
simmeanJacc4535 <- data.frame(apply(simJacc4535,2,mean))
PIJacc4535 <- t(data.frame(apply(simJacc4535,2,PI,prob=0.89)))
F5JaccSim4535 <- data.frame(cbind(simmeanJacc4535,PIJacc4535))
F5JaccSim4535$PlotCN <- all4535cc5  $PlotCN
F5JaccSim4535 <- F5JaccSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4535) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8535 <- link(JaccSplit5_beta, data=all8535cc5 )
simmeanJacc8535 <- data.frame(apply(simJacc8535,2,mean))
PIJacc8535 <- t(data.frame(apply(simJacc8535,2,PI,prob=0.89)))
F5JaccSim8535 <- data.frame(cbind(simmeanJacc8535,PIJacc8535))
F5JaccSim8535$PlotCN <- all8535cc5  $PlotCN
F5JaccSim8535 <- F5JaccSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8535) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc5640 <- link(JaccSplit5_beta, data=all2640cc5 )
simmeanJacc5640 <- data.frame(apply(simJacc5640,2,mean))
PIJacc5640 <- t(data.frame(apply(simJacc5640,2,PI,prob=0.89)))
F5JaccSim2640 <- data.frame(cbind(simmeanJacc5640,PIJacc5640))
F5JaccSim2640$PlotCN <- all2640cc5  $PlotCN
F5JaccSim2640 <- F5JaccSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2640) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4540 <- link(JaccSplit5_beta, data=all4540cc5 )
simmeanJacc4540 <- data.frame(apply(simJacc4540,2,mean))
PIJacc4540 <- t(data.frame(apply(simJacc4540,2,PI,prob=0.89)))
F5JaccSim4540 <- data.frame(cbind(simmeanJacc4540,PIJacc4540))
F5JaccSim4540$PlotCN <- all4540cc5  $PlotCN
F5JaccSim4540 <- F5JaccSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4540) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8540 <- link(JaccSplit5_beta, data=all8540cc5 )
simmeanJacc8540 <- data.frame(apply(simJacc8540,2,mean))
PIJacc8540 <- t(data.frame(apply(simJacc8540,2,PI,prob=0.89)))
F5JaccSim8540 <- data.frame(cbind(simmeanJacc8540,PIJacc8540))
F5JaccSim8540$PlotCN <- all8540cc5  $PlotCN
F5JaccSim8540 <- F5JaccSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8540) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc5645 <- link(JaccSplit5_beta, data=all2645cc5 )
simmeanJacc5645 <- data.frame(apply(simJacc5645,2,mean))
PIJacc5645 <- t(data.frame(apply(simJacc5645,2,PI,prob=0.89)))
F5JaccSim2645 <- data.frame(cbind(simmeanJacc5645,PIJacc5645))
F5JaccSim2645$PlotCN <- all2645cc5  $PlotCN
F5JaccSim2645 <- F5JaccSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2645) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4545 <- link(JaccSplit5_beta, data=all4545cc5 )
simmeanJacc4545 <- data.frame(apply(simJacc4545,2,mean))
PIJacc4545 <- t(data.frame(apply(simJacc4545,2,PI,prob=0.89)))
F5JaccSim4545 <- data.frame(cbind(simmeanJacc4545,PIJacc4545))
F5JaccSim4545$PlotCN <- all4545cc5  $PlotCN
F5JaccSim4545 <- F5JaccSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4545) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8545 <- link(JaccSplit5_beta, data=all8545cc5 )
simmeanJacc8545 <- data.frame(apply(simJacc8545,2,mean))
PIJacc8545 <- t(data.frame(apply(simJacc8545,2,PI,prob=0.89)))
F5JaccSim8545 <- data.frame(cbind(simmeanJacc8545,PIJacc8545))
F5JaccSim8545$PlotCN <- all8545cc5  $PlotCN
F5JaccSim8545 <- F5JaccSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8545) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc5650 <- link(JaccSplit5_beta, data=all2650cc5 )
simmeanJacc5650 <- data.frame(apply(simJacc5650,2,mean))
PIJacc5650 <- t(data.frame(apply(simJacc5650,2,PI,prob=0.89)))
F5JaccSim2650 <- data.frame(cbind(simmeanJacc5650,PIJacc5650))
F5JaccSim2650$PlotCN <- all2650cc5  $PlotCN
F5JaccSim2650 <- F5JaccSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2650) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4550 <- link(JaccSplit5_beta, data=all4550cc5 )
simmeanJacc4550 <- data.frame(apply(simJacc4550,2,mean))
PIJacc4550 <- t(data.frame(apply(simJacc4550,2,PI,prob=0.89)))
F5JaccSim4550 <- data.frame(cbind(simmeanJacc4550,PIJacc4550))
F5JaccSim4550$PlotCN <- all4550cc5  $PlotCN
F5JaccSim4550 <- F5JaccSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4550) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8550 <- link(JaccSplit5_beta, data=all8550cc5 )
simmeanJacc8550 <- data.frame(apply(simJacc8550,2,mean))
PIJacc8550 <- t(data.frame(apply(simJacc8550,2,PI,prob=0.89)))
F5JaccSim8550 <- data.frame(cbind(simmeanJacc8550,PIJacc8550))
F5JaccSim8550$PlotCN <- all8550cc5  $PlotCN
F5JaccSim8550 <- F5JaccSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8550) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc5655 <- link(JaccSplit5_beta, data=all2655cc5 )
simmeanJacc5655 <- data.frame(apply(simJacc5655,2,mean))
PIJacc5655 <- t(data.frame(apply(simJacc5655,2,PI,prob=0.89)))
F5JaccSim2655 <- data.frame(cbind(simmeanJacc5655,PIJacc5655))
F5JaccSim2655$PlotCN <- all2655cc5  $PlotCN
F5JaccSim2655 <- F5JaccSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2655) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4555 <- link(JaccSplit5_beta, data=all4555cc5 )
simmeanJacc4555 <- data.frame(apply(simJacc4555,2,mean))
PIJacc4555 <- t(data.frame(apply(simJacc4555,2,PI,prob=0.89)))
F5JaccSim4555 <- data.frame(cbind(simmeanJacc4555,PIJacc4555))
F5JaccSim4555$PlotCN <- all4555cc5  $PlotCN
F5JaccSim4555 <- F5JaccSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4555) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8555 <- link(JaccSplit5_beta, data=all8555cc5 )
simmeanJacc8555 <- data.frame(apply(simJacc8555,2,mean))
PIJacc8555 <- t(data.frame(apply(simJacc8555,2,PI,prob=0.89)))
F5JaccSim8555 <- data.frame(cbind(simmeanJacc8555,PIJacc8555))
F5JaccSim8555$PlotCN <- all8555cc5  $PlotCN
F5JaccSim8555 <- F5JaccSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8555) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc5660 <- link(JaccSplit5_beta, data=all2660cc5 )
simmeanJacc5660 <- data.frame(apply(simJacc5660,2,mean))
PIJacc5660 <- t(data.frame(apply(simJacc5660,2,PI,prob=0.89)))
F5JaccSim2660 <- data.frame(cbind(simmeanJacc5660,PIJacc5660))
F5JaccSim2660$PlotCN <- all2660cc5  $PlotCN
F5JaccSim2660 <- F5JaccSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2660) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4560 <- link(JaccSplit5_beta, data=all4560cc5 )
simmeanJacc4560 <- data.frame(apply(simJacc4560,2,mean))
PIJacc4560 <- t(data.frame(apply(simJacc4560,2,PI,prob=0.89)))
F5JaccSim4560 <- data.frame(cbind(simmeanJacc4560,PIJacc4560))
F5JaccSim4560$PlotCN <- all4560cc5  $PlotCN
F5JaccSim4560 <- F5JaccSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4560) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8560 <- link(JaccSplit5_beta, data=all8560cc5 )
simmeanJacc8560 <- data.frame(apply(simJacc8560,2,mean))
PIJacc8560 <- t(data.frame(apply(simJacc8560,2,PI,prob=0.89)))
F5JaccSim8560 <- data.frame(cbind(simmeanJacc8560,PIJacc8560))
F5JaccSim8560$PlotCN <- all8560cc5  $PlotCN
F5JaccSim8560 <- F5JaccSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8560) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc5665 <- link(JaccSplit5_beta, data=all2665cc5 )
simmeanJacc5665 <- data.frame(apply(simJacc5665,2,mean))
PIJacc5665 <- t(data.frame(apply(simJacc5665,2,PI,prob=0.89)))
F5JaccSim2665 <- data.frame(cbind(simmeanJacc5665,PIJacc5665))
F5JaccSim2665$PlotCN <- all2665cc5  $PlotCN
F5JaccSim2665 <- F5JaccSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2665) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4565 <- link(JaccSplit5_beta, data=all4565cc5 )
simmeanJacc4565 <- data.frame(apply(simJacc4565,2,mean))
PIJacc4565 <- t(data.frame(apply(simJacc4565,2,PI,prob=0.89)))
F5JaccSim4565 <- data.frame(cbind(simmeanJacc4565,PIJacc4565))
F5JaccSim4565$PlotCN <- all4565cc5  $PlotCN
F5JaccSim4565 <- F5JaccSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4565) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8565 <- link(JaccSplit5_beta, data=all8565cc5 )
simmeanJacc8565 <- data.frame(apply(simJacc8565,2,mean))
PIJacc8565 <- t(data.frame(apply(simJacc8565,2,PI,prob=0.89)))
F5JaccSim8565 <- data.frame(cbind(simmeanJacc8565,PIJacc8565))
F5JaccSim8565$PlotCN <- all8565cc5  $PlotCN
F5JaccSim8565 <- F5JaccSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8565) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc5670 <- link(JaccSplit5_beta, data=all2670cc5 )
simmeanJacc5670 <- data.frame(apply(simJacc5670,2,mean))
PIJacc5670 <- t(data.frame(apply(simJacc5670,2,PI,prob=0.89)))
F5JaccSim2670 <- data.frame(cbind(simmeanJacc5670,PIJacc5670))
F5JaccSim2670$PlotCN <- all2670cc5  $PlotCN
F5JaccSim2670 <- F5JaccSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2670) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4570 <- link(JaccSplit5_beta, data=all4570cc5 )
simmeanJacc4570 <- data.frame(apply(simJacc4570,2,mean))
PIJacc4570 <- t(data.frame(apply(simJacc4570,2,PI,prob=0.89)))
F5JaccSim4570 <- data.frame(cbind(simmeanJacc4570,PIJacc4570))
F5JaccSim4570$PlotCN <- all4570cc5  $PlotCN
F5JaccSim4570 <- F5JaccSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4570) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8570 <- link(JaccSplit5_beta, data=all8570cc5 )
simmeanJacc8570 <- data.frame(apply(simJacc8570,2,mean))
PIJacc8570 <- t(data.frame(apply(simJacc8570,2,PI,prob=0.89)))
F5JaccSim8570 <- data.frame(cbind(simmeanJacc8570,PIJacc8570))
F5JaccSim8570$PlotCN <- all8570cc5  $PlotCN
F5JaccSim8570 <- F5JaccSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8570) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc5675 <- link(JaccSplit5_beta, data=all2675cc5 )
simmeanJacc5675 <- data.frame(apply(simJacc5675,2,mean))
PIJacc5675 <- t(data.frame(apply(simJacc5675,2,PI,prob=0.89)))
F5JaccSim2675 <- data.frame(cbind(simmeanJacc5675,PIJacc5675))
F5JaccSim2675$PlotCN <- all2675cc5  $PlotCN
F5JaccSim2675 <- F5JaccSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2675) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4575 <- link(JaccSplit5_beta, data=all4575cc5 )
simmeanJacc4575 <- data.frame(apply(simJacc4575,2,mean))
PIJacc4575 <- t(data.frame(apply(simJacc4575,2,PI,prob=0.89)))
F5JaccSim4575 <- data.frame(cbind(simmeanJacc4575,PIJacc4575))
F5JaccSim4575$PlotCN <- all4575cc5  $PlotCN
F5JaccSim4575 <- F5JaccSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4575) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8575 <- link(JaccSplit5_beta, data=all8575cc5 )
simmeanJacc8575 <- data.frame(apply(simJacc8575,2,mean))
PIJacc8575 <- t(data.frame(apply(simJacc8575,2,PI,prob=0.89)))
F5JaccSim8575 <- data.frame(cbind(simmeanJacc8575,PIJacc8575))
F5JaccSim8575$PlotCN <- all8575cc5  $PlotCN
F5JaccSim8575 <- F5JaccSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8575) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc5680 <- link(JaccSplit5_beta, data=all2680cc5 )
simmeanJacc5680 <- data.frame(apply(simJacc5680,2,mean))
PIJacc5680 <- t(data.frame(apply(simJacc5680,2,PI,prob=0.89)))
F5JaccSim2680 <- data.frame(cbind(simmeanJacc5680,PIJacc5680))
F5JaccSim2680$PlotCN <- all2680cc5  $PlotCN
F5JaccSim2680 <- F5JaccSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim2680) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4580 <- link(JaccSplit5_beta, data=all4580cc5 )
simmeanJacc4580 <- data.frame(apply(simJacc4580,2,mean))
PIJacc4580 <- t(data.frame(apply(simJacc4580,2,PI,prob=0.89)))
F5JaccSim4580 <- data.frame(cbind(simmeanJacc4580,PIJacc4580))
F5JaccSim4580$PlotCN <- all4580cc5  $PlotCN
F5JaccSim4580 <- F5JaccSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim4580) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8580 <- link(JaccSplit5_beta, data=all8580cc5 )
simmeanJacc8580 <- data.frame(apply(simJacc8580,2,mean))
PIJacc8580 <- t(data.frame(apply(simJacc8580,2,PI,prob=0.89)))
F5JaccSim8580 <- data.frame(cbind(simmeanJacc8580,PIJacc8580))
F5JaccSim8580$PlotCN <- all8580cc5  $PlotCN
F5JaccSim8580 <- F5JaccSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F5JaccSim8580) <- c("PlotCN","Jaccards","5CI","95CI")

#group data by RCP pathway
F5JaccSim26 <- F5JaccSim2625 %>% cbind(F5JaccSim2630[,2:4]) %>%
  cbind(F5JaccSim2635[,2:4]) %>% cbind(F5JaccSim2640[,2:4]) %>% cbind(F5JaccSim2645[,2:4]) %>%
  cbind(F5JaccSim2650[,2:4]) %>% cbind(F5JaccSim2655[,2:4]) %>% cbind(F5JaccSim2660[,2:4]) %>%
  cbind(F5JaccSim2665[,2:4]) %>% cbind(F5JaccSim2670[,2:4]) %>% cbind(F5JaccSim2675[,2:4]) %>%
  cbind(F5JaccSim2680[,2:4])
colnames(F5JaccSim26) <- JaccSimnames
write.csv(F5JaccSim26,file="F5JaccPred26b.csv")

F5JaccSim45 <- F5JaccSim4525 %>% cbind(F5JaccSim4530[,2:4]) %>%
  cbind(F5JaccSim4535[,2:4]) %>% cbind(F5JaccSim4540[,2:4]) %>% cbind(F5JaccSim4545[,2:4]) %>%
  cbind(F5JaccSim4550[,2:4]) %>% cbind(F5JaccSim4555[,2:4]) %>% cbind(F5JaccSim4560[,2:4]) %>%
  cbind(F5JaccSim4565[,2:4]) %>% cbind(F5JaccSim4570[,2:4]) %>% cbind(F5JaccSim4575[,2:4]) %>%
  cbind(F5JaccSim4580[,2:4])
colnames(F5JaccSim45) <- JaccSimnames
write.csv(F5JaccSim45,file="F5JaccPred45b.csv")

F5JaccSim85 <- F5JaccSim8525 %>% cbind(F5JaccSim8530[,2:4]) %>%
  cbind(F5JaccSim8535[,2:4]) %>% cbind(F5JaccSim8540[,2:4]) %>% cbind(F5JaccSim8545[,2:4]) %>%
  cbind(F5JaccSim8550[,2:4]) %>% cbind(F5JaccSim8555[,2:4]) %>% cbind(F5JaccSim8560[,2:4]) %>%
  cbind(F5JaccSim8565[,2:4]) %>% cbind(F5JaccSim8570[,2:4]) %>% cbind(F5JaccSim8575[,2:4]) %>%
  cbind(F5JaccSim8580[,2:4])
colnames(F5JaccSim85) <- JaccSimnames
write.csv(F5JaccSim85,file="F5JaccPred85b.csv")


#fgroup 20
simJacc2625 <- link(JaccSplit20_beta, data=all2625cc20 )
simmeanJacc2625 <- data.frame(apply(simJacc2625,2,mean))
PIJacc2625 <- t(data.frame(apply(simJacc2625,2,PI,prob=0.89)))
F20JaccSim2625 <- data.frame(cbind(simmeanJacc2625,PIJacc2625))
F20JaccSim2625$PlotCN <- all2625cc20  $PlotCN
F20JaccSim2625 <- F20JaccSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2625) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4525 <- link(JaccSplit20_beta, data=all4525cc20 )
simmeanJacc4525 <- data.frame(apply(simJacc4525,2,mean))
PIJacc4525 <- t(data.frame(apply(simJacc4525,2,PI,prob=0.89)))
F20JaccSim4525 <- data.frame(cbind(simmeanJacc4525,PIJacc4525))
F20JaccSim4525$PlotCN <- all4525cc20  $PlotCN
F20JaccSim4525 <- F20JaccSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4525) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8525 <- link(JaccSplit20_beta, data=all8525cc20 )
simmeanJacc8525 <- data.frame(apply(simJacc8525,2,mean))
PIJacc8525 <- t(data.frame(apply(simJacc8525,2,PI,prob=0.89)))
F20JaccSim8525 <- data.frame(cbind(simmeanJacc8525,PIJacc8525))
F20JaccSim8525$PlotCN <- all8525cc20  $PlotCN
F20JaccSim8525 <- F20JaccSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8525) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2630 <- link(JaccSplit20_beta, data=all2630cc20 )
simmeanJacc2630 <- data.frame(apply(simJacc2630,2,mean))
PIJacc2630 <- t(data.frame(apply(simJacc2630,2,PI,prob=0.89)))
F20JaccSim2630 <- data.frame(cbind(simmeanJacc2630,PIJacc2630))
F20JaccSim2630$PlotCN <- all2630cc20  $PlotCN
F20JaccSim2630 <- F20JaccSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2630) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4530 <- link(JaccSplit20_beta, data=all4530cc20 )
simmeanJacc4530 <- data.frame(apply(simJacc4530,2,mean))
PIJacc4530 <- t(data.frame(apply(simJacc4530,2,PI,prob=0.89)))
F20JaccSim4530 <- data.frame(cbind(simmeanJacc4530,PIJacc4530))
F20JaccSim4530$PlotCN <- all4530cc20  $PlotCN
F20JaccSim4530 <- F20JaccSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4530) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8530 <- link(JaccSplit20_beta, data=all8530cc20 )
simmeanJacc8530 <- data.frame(apply(simJacc8530,2,mean))
PIJacc8530 <- t(data.frame(apply(simJacc8530,2,PI,prob=0.89)))
F20JaccSim8530 <- data.frame(cbind(simmeanJacc8530,PIJacc8530))
F20JaccSim8530$PlotCN <- all8530cc20  $PlotCN
F20JaccSim8530 <- F20JaccSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8530) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2635 <- link(JaccSplit20_beta, data=all2635cc20 )
simmeanJacc2635 <- data.frame(apply(simJacc2635,2,mean))
PIJacc2635 <- t(data.frame(apply(simJacc2635,2,PI,prob=0.89)))
F20JaccSim2635 <- data.frame(cbind(simmeanJacc2635,PIJacc2635))
F20JaccSim2635$PlotCN <- all2635cc20  $PlotCN
F20JaccSim2635 <- F20JaccSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2635) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4535 <- link(JaccSplit20_beta, data=all4535cc20 )
simmeanJacc4535 <- data.frame(apply(simJacc4535,2,mean))
PIJacc4535 <- t(data.frame(apply(simJacc4535,2,PI,prob=0.89)))
F20JaccSim4535 <- data.frame(cbind(simmeanJacc4535,PIJacc4535))
F20JaccSim4535$PlotCN <- all4535cc20  $PlotCN
F20JaccSim4535 <- F20JaccSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4535) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8535 <- link(JaccSplit20_beta, data=all8535cc20 )
simmeanJacc8535 <- data.frame(apply(simJacc8535,2,mean))
PIJacc8535 <- t(data.frame(apply(simJacc8535,2,PI,prob=0.89)))
F20JaccSim8535 <- data.frame(cbind(simmeanJacc8535,PIJacc8535))
F20JaccSim8535$PlotCN <- all8535cc20  $PlotCN
F20JaccSim8535 <- F20JaccSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8535) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2640 <- link(JaccSplit20_beta, data=all2640cc20 )
simmeanJacc2640 <- data.frame(apply(simJacc2640,2,mean))
PIJacc2640 <- t(data.frame(apply(simJacc2640,2,PI,prob=0.89)))
F20JaccSim2640 <- data.frame(cbind(simmeanJacc2640,PIJacc2640))
F20JaccSim2640$PlotCN <- all2640cc20  $PlotCN
F20JaccSim2640 <- F20JaccSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2640) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4540 <- link(JaccSplit20_beta, data=all4540cc20 )
simmeanJacc4540 <- data.frame(apply(simJacc4540,2,mean))
PIJacc4540 <- t(data.frame(apply(simJacc4540,2,PI,prob=0.89)))
F20JaccSim4540 <- data.frame(cbind(simmeanJacc4540,PIJacc4540))
F20JaccSim4540$PlotCN <- all4540cc20  $PlotCN
F20JaccSim4540 <- F20JaccSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4540) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8540 <- link(JaccSplit20_beta, data=all8540cc20 )
simmeanJacc8540 <- data.frame(apply(simJacc8540,2,mean))
PIJacc8540 <- t(data.frame(apply(simJacc8540,2,PI,prob=0.89)))
F20JaccSim8540 <- data.frame(cbind(simmeanJacc8540,PIJacc8540))
F20JaccSim8540$PlotCN <- all8540cc20  $PlotCN
F20JaccSim8540 <- F20JaccSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8540) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2645 <- link(JaccSplit20_beta, data=all2645cc20 )
simmeanJacc2645 <- data.frame(apply(simJacc2645,2,mean))
PIJacc2645 <- t(data.frame(apply(simJacc2645,2,PI,prob=0.89)))
F20JaccSim2645 <- data.frame(cbind(simmeanJacc2645,PIJacc2645))
F20JaccSim2645$PlotCN <- all2645cc20  $PlotCN
F20JaccSim2645 <- F20JaccSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2645) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4545 <- link(JaccSplit20_beta, data=all4545cc20 )
simmeanJacc4545 <- data.frame(apply(simJacc4545,2,mean))
PIJacc4545 <- t(data.frame(apply(simJacc4545,2,PI,prob=0.89)))
F20JaccSim4545 <- data.frame(cbind(simmeanJacc4545,PIJacc4545))
F20JaccSim4545$PlotCN <- all4545cc20  $PlotCN
F20JaccSim4545 <- F20JaccSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4545) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8545 <- link(JaccSplit20_beta, data=all8545cc20 )
simmeanJacc8545 <- data.frame(apply(simJacc8545,2,mean))
PIJacc8545 <- t(data.frame(apply(simJacc8545,2,PI,prob=0.89)))
F20JaccSim8545 <- data.frame(cbind(simmeanJacc8545,PIJacc8545))
F20JaccSim8545$PlotCN <- all8545cc20  $PlotCN
F20JaccSim8545 <- F20JaccSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8545) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2650 <- link(JaccSplit20_beta, data=all2650cc20 )
simmeanJacc2650 <- data.frame(apply(simJacc2650,2,mean))
PIJacc2650 <- t(data.frame(apply(simJacc2650,2,PI,prob=0.89)))
F20JaccSim2650 <- data.frame(cbind(simmeanJacc2650,PIJacc2650))
F20JaccSim2650$PlotCN <- all2650cc20  $PlotCN
F20JaccSim2650 <- F20JaccSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2650) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4550 <- link(JaccSplit20_beta, data=all4550cc20 )
simmeanJacc4550 <- data.frame(apply(simJacc4550,2,mean))
PIJacc4550 <- t(data.frame(apply(simJacc4550,2,PI,prob=0.89)))
F20JaccSim4550 <- data.frame(cbind(simmeanJacc4550,PIJacc4550))
F20JaccSim4550$PlotCN <- all4550cc20  $PlotCN
F20JaccSim4550 <- F20JaccSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4550) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8550 <- link(JaccSplit20_beta, data=all8550cc20 )
simmeanJacc8550 <- data.frame(apply(simJacc8550,2,mean))
PIJacc8550 <- t(data.frame(apply(simJacc8550,2,PI,prob=0.89)))
F20JaccSim8550 <- data.frame(cbind(simmeanJacc8550,PIJacc8550))
F20JaccSim8550$PlotCN <- all8550cc20  $PlotCN
F20JaccSim8550 <- F20JaccSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8550) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2655 <- link(JaccSplit20_beta, data=all2655cc20 )
simmeanJacc2655 <- data.frame(apply(simJacc2655,2,mean))
PIJacc2655 <- t(data.frame(apply(simJacc2655,2,PI,prob=0.89)))
F20JaccSim2655 <- data.frame(cbind(simmeanJacc2655,PIJacc2655))
F20JaccSim2655$PlotCN <- all2655cc20  $PlotCN
F20JaccSim2655 <- F20JaccSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2655) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4555 <- link(JaccSplit20_beta, data=all4555cc20 )
simmeanJacc4555 <- data.frame(apply(simJacc4555,2,mean))
PIJacc4555 <- t(data.frame(apply(simJacc4555,2,PI,prob=0.89)))
F20JaccSim4555 <- data.frame(cbind(simmeanJacc4555,PIJacc4555))
F20JaccSim4555$PlotCN <- all4555cc20  $PlotCN
F20JaccSim4555 <- F20JaccSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4555) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8555 <- link(JaccSplit20_beta, data=all8555cc20 )
simmeanJacc8555 <- data.frame(apply(simJacc8555,2,mean))
PIJacc8555 <- t(data.frame(apply(simJacc8555,2,PI,prob=0.89)))
F20JaccSim8555 <- data.frame(cbind(simmeanJacc8555,PIJacc8555))
F20JaccSim8555$PlotCN <- all8555cc20  $PlotCN
F20JaccSim8555 <- F20JaccSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8555) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2660 <- link(JaccSplit20_beta, data=all2660cc20 )
simmeanJacc2660 <- data.frame(apply(simJacc2660,2,mean))
PIJacc2660 <- t(data.frame(apply(simJacc2660,2,PI,prob=0.89)))
F20JaccSim2660 <- data.frame(cbind(simmeanJacc2660,PIJacc2660))
F20JaccSim2660$PlotCN <- all2660cc20  $PlotCN
F20JaccSim2660 <- F20JaccSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2660) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4560 <- link(JaccSplit20_beta, data=all4560cc20 )
simmeanJacc4560 <- data.frame(apply(simJacc4560,2,mean))
PIJacc4560 <- t(data.frame(apply(simJacc4560,2,PI,prob=0.89)))
F20JaccSim4560 <- data.frame(cbind(simmeanJacc4560,PIJacc4560))
F20JaccSim4560$PlotCN <- all4560cc20  $PlotCN
F20JaccSim4560 <- F20JaccSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4560) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8560 <- link(JaccSplit20_beta, data=all8560cc20 )
simmeanJacc8560 <- data.frame(apply(simJacc8560,2,mean))
PIJacc8560 <- t(data.frame(apply(simJacc8560,2,PI,prob=0.89)))
F20JaccSim8560 <- data.frame(cbind(simmeanJacc8560,PIJacc8560))
F20JaccSim8560$PlotCN <- all8560cc20  $PlotCN
F20JaccSim8560 <- F20JaccSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8560) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2665 <- link(JaccSplit20_beta, data=all2665cc20 )
simmeanJacc2665 <- data.frame(apply(simJacc2665,2,mean))
PIJacc2665 <- t(data.frame(apply(simJacc2665,2,PI,prob=0.89)))
F20JaccSim2665 <- data.frame(cbind(simmeanJacc2665,PIJacc2665))
F20JaccSim2665$PlotCN <- all2665cc20  $PlotCN
F20JaccSim2665 <- F20JaccSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2665) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4565 <- link(JaccSplit20_beta, data=all4565cc20 )
simmeanJacc4565 <- data.frame(apply(simJacc4565,2,mean))
PIJacc4565 <- t(data.frame(apply(simJacc4565,2,PI,prob=0.89)))
F20JaccSim4565 <- data.frame(cbind(simmeanJacc4565,PIJacc4565))
F20JaccSim4565$PlotCN <- all4565cc20  $PlotCN
F20JaccSim4565 <- F20JaccSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4565) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8565 <- link(JaccSplit20_beta, data=all8565cc20 )
simmeanJacc8565 <- data.frame(apply(simJacc8565,2,mean))
PIJacc8565 <- t(data.frame(apply(simJacc8565,2,PI,prob=0.89)))
F20JaccSim8565 <- data.frame(cbind(simmeanJacc8565,PIJacc8565))
F20JaccSim8565$PlotCN <- all8565cc20  $PlotCN
F20JaccSim8565 <- F20JaccSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8565) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2670 <- link(JaccSplit20_beta, data=all2670cc20 )
simmeanJacc2670 <- data.frame(apply(simJacc2670,2,mean))
PIJacc2670 <- t(data.frame(apply(simJacc2670,2,PI,prob=0.89)))
F20JaccSim2670 <- data.frame(cbind(simmeanJacc2670,PIJacc2670))
F20JaccSim2670$PlotCN <- all2670cc20  $PlotCN
F20JaccSim2670 <- F20JaccSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2670) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4570 <- link(JaccSplit20_beta, data=all4570cc20 )
simmeanJacc4570 <- data.frame(apply(simJacc4570,2,mean))
PIJacc4570 <- t(data.frame(apply(simJacc4570,2,PI,prob=0.89)))
F20JaccSim4570 <- data.frame(cbind(simmeanJacc4570,PIJacc4570))
F20JaccSim4570$PlotCN <- all4570cc20  $PlotCN
F20JaccSim4570 <- F20JaccSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4570) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8570 <- link(JaccSplit20_beta, data=all8570cc20 )
simmeanJacc8570 <- data.frame(apply(simJacc8570,2,mean))
PIJacc8570 <- t(data.frame(apply(simJacc8570,2,PI,prob=0.89)))
F20JaccSim8570 <- data.frame(cbind(simmeanJacc8570,PIJacc8570))
F20JaccSim8570$PlotCN <- all8570cc20  $PlotCN
F20JaccSim8570 <- F20JaccSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8570) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2675 <- link(JaccSplit20_beta, data=all2675cc20 )
simmeanJacc2675 <- data.frame(apply(simJacc2675,2,mean))
PIJacc2675 <- t(data.frame(apply(simJacc2675,2,PI,prob=0.89)))
F20JaccSim2675 <- data.frame(cbind(simmeanJacc2675,PIJacc2675))
F20JaccSim2675$PlotCN <- all2675cc20  $PlotCN
F20JaccSim2675 <- F20JaccSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2675) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4575 <- link(JaccSplit20_beta, data=all4575cc20 )
simmeanJacc4575 <- data.frame(apply(simJacc4575,2,mean))
PIJacc4575 <- t(data.frame(apply(simJacc4575,2,PI,prob=0.89)))
F20JaccSim4575 <- data.frame(cbind(simmeanJacc4575,PIJacc4575))
F20JaccSim4575$PlotCN <- all4575cc20  $PlotCN
F20JaccSim4575 <- F20JaccSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4575) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8575 <- link(JaccSplit20_beta, data=all8575cc20 )
simmeanJacc8575 <- data.frame(apply(simJacc8575,2,mean))
PIJacc8575 <- t(data.frame(apply(simJacc8575,2,PI,prob=0.89)))
F20JaccSim8575 <- data.frame(cbind(simmeanJacc8575,PIJacc8575))
F20JaccSim8575$PlotCN <- all8575cc20  $PlotCN
F20JaccSim8575 <- F20JaccSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8575) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2680 <- link(JaccSplit20_beta, data=all2680cc20 )
simmeanJacc2680 <- data.frame(apply(simJacc2680,2,mean))
PIJacc2680 <- t(data.frame(apply(simJacc2680,2,PI,prob=0.89)))
F20JaccSim2680 <- data.frame(cbind(simmeanJacc2680,PIJacc2680))
F20JaccSim2680$PlotCN <- all2680cc20  $PlotCN
F20JaccSim2680 <- F20JaccSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim2680) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4580 <- link(JaccSplit20_beta, data=all4580cc20 )
simmeanJacc4580 <- data.frame(apply(simJacc4580,2,mean))
PIJacc4580 <- t(data.frame(apply(simJacc4580,2,PI,prob=0.89)))
F20JaccSim4580 <- data.frame(cbind(simmeanJacc4580,PIJacc4580))
F20JaccSim4580$PlotCN <- all4580cc20  $PlotCN
F20JaccSim4580 <- F20JaccSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim4580) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8580 <- link(JaccSplit20_beta, data=all8580cc20 )
simmeanJacc8580 <- data.frame(apply(simJacc8580,2,mean))
PIJacc8580 <- t(data.frame(apply(simJacc8580,2,PI,prob=0.89)))
F20JaccSim8580 <- data.frame(cbind(simmeanJacc8580,PIJacc8580))
F20JaccSim8580$PlotCN <- all8580cc20  $PlotCN
F20JaccSim8580 <- F20JaccSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F20JaccSim8580) <- c("PlotCN","Jaccards","5CI","95CI")


#group data by RCP pathway
F20JaccSim26 <- F20JaccSim2625 %>% cbind(F20JaccSim2630[,2:4]) %>%
  cbind(F20JaccSim2635[,2:4]) %>% cbind(F20JaccSim2640[,2:4]) %>% cbind(F20JaccSim2645[,2:4]) %>%
  cbind(F20JaccSim2650[,2:4]) %>% cbind(F20JaccSim2655[,2:4]) %>% cbind(F20JaccSim2660[,2:4]) %>%
  cbind(F20JaccSim2665[,2:4]) %>% cbind(F20JaccSim2670[,2:4]) %>% cbind(F20JaccSim2675[,2:4]) %>%
  cbind(F20JaccSim2680[,2:4])
colnames(F20JaccSim26) <- JaccSimnames
write.csv(F20JaccSim26,file="F20JaccPred26b.csv")

F20JaccSim45 <- F20JaccSim4525 %>% cbind(F20JaccSim4530[,2:4]) %>%
  cbind(F20JaccSim4535[,2:4]) %>% cbind(F20JaccSim4540[,2:4]) %>% cbind(F20JaccSim4545[,2:4]) %>%
  cbind(F20JaccSim4550[,2:4]) %>% cbind(F20JaccSim4555[,2:4]) %>% cbind(F20JaccSim4560[,2:4]) %>%
  cbind(F20JaccSim4565[,2:4]) %>% cbind(F20JaccSim4570[,2:4]) %>% cbind(F20JaccSim4575[,2:4]) %>%
  cbind(F20JaccSim4580[,2:4])
colnames(F20JaccSim45) <- JaccSimnames
write.csv(F20JaccSim45,file="F20JaccPred45b.csv")

F20JaccSim85 <- F20JaccSim8525 %>% cbind(F20JaccSim8530[,2:4]) %>%
  cbind(F20JaccSim8535[,2:4]) %>% cbind(F20JaccSim8540[,2:4]) %>% cbind(F20JaccSim8545[,2:4]) %>%
  cbind(F20JaccSim8550[,2:4]) %>% cbind(F20JaccSim8555[,2:4]) %>% cbind(F20JaccSim8560[,2:4]) %>%
  cbind(F20JaccSim8565[,2:4]) %>% cbind(F20JaccSim8570[,2:4]) %>% cbind(F20JaccSim8575[,2:4]) %>%
  cbind(F20JaccSim8580[,2:4])
colnames(F20JaccSim85) <- JaccSimnames
write.csv(F20JaccSim85,file="F20JaccPred85b.csv")


#fgroup 21
simJacc2625 <- link(JaccSplit21_beta, data=all2625cc21 )
simmeanJacc2625 <- data.frame(apply(simJacc2625,2,mean))
PIJacc2625 <- t(data.frame(apply(simJacc2625,2,PI,prob=0.89)))
F21JaccSim2625 <- data.frame(cbind(simmeanJacc2625,PIJacc2625))
F21JaccSim2625$PlotCN <- all2625cc21  $PlotCN
F21JaccSim2625 <- F21JaccSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2625) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4525 <- link(JaccSplit21_beta, data=all4525cc21 )
simmeanJacc4525 <- data.frame(apply(simJacc4525,2,mean))
PIJacc4525 <- t(data.frame(apply(simJacc4525,2,PI,prob=0.89)))
F21JaccSim4525 <- data.frame(cbind(simmeanJacc4525,PIJacc4525))
F21JaccSim4525$PlotCN <- all4525cc21  $PlotCN
F21JaccSim4525 <- F21JaccSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4525) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8525 <- link(JaccSplit21_beta, data=all8525cc21 )
simmeanJacc8525 <- data.frame(apply(simJacc8525,2,mean))
PIJacc8525 <- t(data.frame(apply(simJacc8525,2,PI,prob=0.89)))
F21JaccSim8525 <- data.frame(cbind(simmeanJacc8525,PIJacc8525))
F21JaccSim8525$PlotCN <- all8525cc21  $PlotCN
F21JaccSim8525 <- F21JaccSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8525) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2630 <- link(JaccSplit21_beta, data=all2630cc21 )
simmeanJacc2630 <- data.frame(apply(simJacc2630,2,mean))
PIJacc2630 <- t(data.frame(apply(simJacc2630,2,PI,prob=0.89)))
F21JaccSim2630 <- data.frame(cbind(simmeanJacc2630,PIJacc2630))
F21JaccSim2630$PlotCN <- all2630cc21  $PlotCN
F21JaccSim2630 <- F21JaccSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2630) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4530 <- link(JaccSplit21_beta, data=all4530cc21 )
simmeanJacc4530 <- data.frame(apply(simJacc4530,2,mean))
PIJacc4530 <- t(data.frame(apply(simJacc4530,2,PI,prob=0.89)))
F21JaccSim4530 <- data.frame(cbind(simmeanJacc4530,PIJacc4530))
F21JaccSim4530$PlotCN <- all4530cc21  $PlotCN
F21JaccSim4530 <- F21JaccSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4530) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8530 <- link(JaccSplit21_beta, data=all8530cc21 )
simmeanJacc8530 <- data.frame(apply(simJacc8530,2,mean))
PIJacc8530 <- t(data.frame(apply(simJacc8530,2,PI,prob=0.89)))
F21JaccSim8530 <- data.frame(cbind(simmeanJacc8530,PIJacc8530))
F21JaccSim8530$PlotCN <- all8530cc21  $PlotCN
F21JaccSim8530 <- F21JaccSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8530) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2635 <- link(JaccSplit21_beta, data=all2635cc21 )
simmeanJacc2635 <- data.frame(apply(simJacc2635,2,mean))
PIJacc2635 <- t(data.frame(apply(simJacc2635,2,PI,prob=0.89)))
F21JaccSim2635 <- data.frame(cbind(simmeanJacc2635,PIJacc2635))
F21JaccSim2635$PlotCN <- all2635cc21  $PlotCN
F21JaccSim2635 <- F21JaccSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2635) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4535 <- link(JaccSplit21_beta, data=all4535cc21 )
simmeanJacc4535 <- data.frame(apply(simJacc4535,2,mean))
PIJacc4535 <- t(data.frame(apply(simJacc4535,2,PI,prob=0.89)))
F21JaccSim4535 <- data.frame(cbind(simmeanJacc4535,PIJacc4535))
F21JaccSim4535$PlotCN <- all4535cc21  $PlotCN
F21JaccSim4535 <- F21JaccSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4535) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8535 <- link(JaccSplit21_beta, data=all8535cc21 )
simmeanJacc8535 <- data.frame(apply(simJacc8535,2,mean))
PIJacc8535 <- t(data.frame(apply(simJacc8535,2,PI,prob=0.89)))
F21JaccSim8535 <- data.frame(cbind(simmeanJacc8535,PIJacc8535))
F21JaccSim8535$PlotCN <- all8535cc21  $PlotCN
F21JaccSim8535 <- F21JaccSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8535) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2640 <- link(JaccSplit21_beta, data=all2640cc21 )
simmeanJacc2640 <- data.frame(apply(simJacc2640,2,mean))
PIJacc2640 <- t(data.frame(apply(simJacc2640,2,PI,prob=0.89)))
F21JaccSim2640 <- data.frame(cbind(simmeanJacc2640,PIJacc2640))
F21JaccSim2640$PlotCN <- all2640cc21  $PlotCN
F21JaccSim2640 <- F21JaccSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2640) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4540 <- link(JaccSplit21_beta, data=all4540cc21 )
simmeanJacc4540 <- data.frame(apply(simJacc4540,2,mean))
PIJacc4540 <- t(data.frame(apply(simJacc4540,2,PI,prob=0.89)))
F21JaccSim4540 <- data.frame(cbind(simmeanJacc4540,PIJacc4540))
F21JaccSim4540$PlotCN <- all4540cc21  $PlotCN
F21JaccSim4540 <- F21JaccSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4540) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8540 <- link(JaccSplit21_beta, data=all8540cc21 )
simmeanJacc8540 <- data.frame(apply(simJacc8540,2,mean))
PIJacc8540 <- t(data.frame(apply(simJacc8540,2,PI,prob=0.89)))
F21JaccSim8540 <- data.frame(cbind(simmeanJacc8540,PIJacc8540))
F21JaccSim8540$PlotCN <- all8540cc21  $PlotCN
F21JaccSim8540 <- F21JaccSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8540) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2645 <- link(JaccSplit21_beta, data=all2645cc21 )
simmeanJacc2645 <- data.frame(apply(simJacc2645,2,mean))
PIJacc2645 <- t(data.frame(apply(simJacc2645,2,PI,prob=0.89)))
F21JaccSim2645 <- data.frame(cbind(simmeanJacc2645,PIJacc2645))
F21JaccSim2645$PlotCN <- all2645cc21  $PlotCN
F21JaccSim2645 <- F21JaccSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2645) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4545 <- link(JaccSplit21_beta, data=all4545cc21 )
simmeanJacc4545 <- data.frame(apply(simJacc4545,2,mean))
PIJacc4545 <- t(data.frame(apply(simJacc4545,2,PI,prob=0.89)))
F21JaccSim4545 <- data.frame(cbind(simmeanJacc4545,PIJacc4545))
F21JaccSim4545$PlotCN <- all4545cc21  $PlotCN
F21JaccSim4545 <- F21JaccSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4545) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8545 <- link(JaccSplit21_beta, data=all8545cc21 )
simmeanJacc8545 <- data.frame(apply(simJacc8545,2,mean))
PIJacc8545 <- t(data.frame(apply(simJacc8545,2,PI,prob=0.89)))
F21JaccSim8545 <- data.frame(cbind(simmeanJacc8545,PIJacc8545))
F21JaccSim8545$PlotCN <- all8545cc21  $PlotCN
F21JaccSim8545 <- F21JaccSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8545) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2650 <- link(JaccSplit21_beta, data=all2650cc21 )
simmeanJacc2650 <- data.frame(apply(simJacc2650,2,mean))
PIJacc2650 <- t(data.frame(apply(simJacc2650,2,PI,prob=0.89)))
F21JaccSim2650 <- data.frame(cbind(simmeanJacc2650,PIJacc2650))
F21JaccSim2650$PlotCN <- all2650cc21  $PlotCN
F21JaccSim2650 <- F21JaccSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2650) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4550 <- link(JaccSplit21_beta, data=all4550cc21 )
simmeanJacc4550 <- data.frame(apply(simJacc4550,2,mean))
PIJacc4550 <- t(data.frame(apply(simJacc4550,2,PI,prob=0.89)))
F21JaccSim4550 <- data.frame(cbind(simmeanJacc4550,PIJacc4550))
F21JaccSim4550$PlotCN <- all4550cc21  $PlotCN
F21JaccSim4550 <- F21JaccSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4550) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8550 <- link(JaccSplit21_beta, data=all8550cc21 )
simmeanJacc8550 <- data.frame(apply(simJacc8550,2,mean))
PIJacc8550 <- t(data.frame(apply(simJacc8550,2,PI,prob=0.89)))
F21JaccSim8550 <- data.frame(cbind(simmeanJacc8550,PIJacc8550))
F21JaccSim8550$PlotCN <- all8550cc21  $PlotCN
F21JaccSim8550 <- F21JaccSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8550) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2655 <- link(JaccSplit21_beta, data=all2655cc21 )
simmeanJacc2655 <- data.frame(apply(simJacc2655,2,mean))
PIJacc2655 <- t(data.frame(apply(simJacc2655,2,PI,prob=0.89)))
F21JaccSim2655 <- data.frame(cbind(simmeanJacc2655,PIJacc2655))
F21JaccSim2655$PlotCN <- all2655cc21  $PlotCN
F21JaccSim2655 <- F21JaccSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2655) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4555 <- link(JaccSplit21_beta, data=all4555cc21 )
simmeanJacc4555 <- data.frame(apply(simJacc4555,2,mean))
PIJacc4555 <- t(data.frame(apply(simJacc4555,2,PI,prob=0.89)))
F21JaccSim4555 <- data.frame(cbind(simmeanJacc4555,PIJacc4555))
F21JaccSim4555$PlotCN <- all4555cc21  $PlotCN
F21JaccSim4555 <- F21JaccSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4555) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8555 <- link(JaccSplit21_beta, data=all8555cc21 )
simmeanJacc8555 <- data.frame(apply(simJacc8555,2,mean))
PIJacc8555 <- t(data.frame(apply(simJacc8555,2,PI,prob=0.89)))
F21JaccSim8555 <- data.frame(cbind(simmeanJacc8555,PIJacc8555))
F21JaccSim8555$PlotCN <- all8555cc21  $PlotCN
F21JaccSim8555 <- F21JaccSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8555) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2660 <- link(JaccSplit21_beta, data=all2660cc21 )
simmeanJacc2660 <- data.frame(apply(simJacc2660,2,mean))
PIJacc2660 <- t(data.frame(apply(simJacc2660,2,PI,prob=0.89)))
F21JaccSim2660 <- data.frame(cbind(simmeanJacc2660,PIJacc2660))
F21JaccSim2660$PlotCN <- all2660cc21  $PlotCN
F21JaccSim2660 <- F21JaccSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2660) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4560 <- link(JaccSplit21_beta, data=all4560cc21 )
simmeanJacc4560 <- data.frame(apply(simJacc4560,2,mean))
PIJacc4560 <- t(data.frame(apply(simJacc4560,2,PI,prob=0.89)))
F21JaccSim4560 <- data.frame(cbind(simmeanJacc4560,PIJacc4560))
F21JaccSim4560$PlotCN <- all4560cc21  $PlotCN
F21JaccSim4560 <- F21JaccSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4560) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8560 <- link(JaccSplit21_beta, data=all8560cc21 )
simmeanJacc8560 <- data.frame(apply(simJacc8560,2,mean))
PIJacc8560 <- t(data.frame(apply(simJacc8560,2,PI,prob=0.89)))
F21JaccSim8560 <- data.frame(cbind(simmeanJacc8560,PIJacc8560))
F21JaccSim8560$PlotCN <- all8560cc21  $PlotCN
F21JaccSim8560 <- F21JaccSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8560) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2665 <- link(JaccSplit21_beta, data=all2665cc21 )
simmeanJacc2665 <- data.frame(apply(simJacc2665,2,mean))
PIJacc2665 <- t(data.frame(apply(simJacc2665,2,PI,prob=0.89)))
F21JaccSim2665 <- data.frame(cbind(simmeanJacc2665,PIJacc2665))
F21JaccSim2665$PlotCN <- all2665cc21  $PlotCN
F21JaccSim2665 <- F21JaccSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2665) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4565 <- link(JaccSplit21_beta, data=all4565cc21 )
simmeanJacc4565 <- data.frame(apply(simJacc4565,2,mean))
PIJacc4565 <- t(data.frame(apply(simJacc4565,2,PI,prob=0.89)))
F21JaccSim4565 <- data.frame(cbind(simmeanJacc4565,PIJacc4565))
F21JaccSim4565$PlotCN <- all4565cc21  $PlotCN
F21JaccSim4565 <- F21JaccSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4565) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8565 <- link(JaccSplit21_beta, data=all8565cc21 )
simmeanJacc8565 <- data.frame(apply(simJacc8565,2,mean))
PIJacc8565 <- t(data.frame(apply(simJacc8565,2,PI,prob=0.89)))
F21JaccSim8565 <- data.frame(cbind(simmeanJacc8565,PIJacc8565))
F21JaccSim8565$PlotCN <- all8565cc21  $PlotCN
F21JaccSim8565 <- F21JaccSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8565) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2670 <- link(JaccSplit21_beta, data=all2670cc21 )
simmeanJacc2670 <- data.frame(apply(simJacc2670,2,mean))
PIJacc2670 <- t(data.frame(apply(simJacc2670,2,PI,prob=0.89)))
F21JaccSim2670 <- data.frame(cbind(simmeanJacc2670,PIJacc2670))
F21JaccSim2670$PlotCN <- all2670cc21  $PlotCN
F21JaccSim2670 <- F21JaccSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2670) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4570 <- link(JaccSplit21_beta, data=all4570cc21 )
simmeanJacc4570 <- data.frame(apply(simJacc4570,2,mean))
PIJacc4570 <- t(data.frame(apply(simJacc4570,2,PI,prob=0.89)))
F21JaccSim4570 <- data.frame(cbind(simmeanJacc4570,PIJacc4570))
F21JaccSim4570$PlotCN <- all4570cc21  $PlotCN
F21JaccSim4570 <- F21JaccSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4570) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8570 <- link(JaccSplit21_beta, data=all8570cc21 )
simmeanJacc8570 <- data.frame(apply(simJacc8570,2,mean))
PIJacc8570 <- t(data.frame(apply(simJacc8570,2,PI,prob=0.89)))
F21JaccSim8570 <- data.frame(cbind(simmeanJacc8570,PIJacc8570))
F21JaccSim8570$PlotCN <- all8570cc21  $PlotCN
F21JaccSim8570 <- F21JaccSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8570) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2675 <- link(JaccSplit21_beta, data=all2675cc21 )
simmeanJacc2675 <- data.frame(apply(simJacc2675,2,mean))
PIJacc2675 <- t(data.frame(apply(simJacc2675,2,PI,prob=0.89)))
F21JaccSim2675 <- data.frame(cbind(simmeanJacc2675,PIJacc2675))
F21JaccSim2675$PlotCN <- all2675cc21  $PlotCN
F21JaccSim2675 <- F21JaccSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2675) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4575 <- link(JaccSplit21_beta, data=all4575cc21 )
simmeanJacc4575 <- data.frame(apply(simJacc4575,2,mean))
PIJacc4575 <- t(data.frame(apply(simJacc4575,2,PI,prob=0.89)))
F21JaccSim4575 <- data.frame(cbind(simmeanJacc4575,PIJacc4575))
F21JaccSim4575$PlotCN <- all4575cc21  $PlotCN
F21JaccSim4575 <- F21JaccSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4575) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8575 <- link(JaccSplit21_beta, data=all8575cc21 )
simmeanJacc8575 <- data.frame(apply(simJacc8575,2,mean))
PIJacc8575 <- t(data.frame(apply(simJacc8575,2,PI,prob=0.89)))
F21JaccSim8575 <- data.frame(cbind(simmeanJacc8575,PIJacc8575))
F21JaccSim8575$PlotCN <- all8575cc21  $PlotCN
F21JaccSim8575 <- F21JaccSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8575) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2680 <- link(JaccSplit21_beta, data=all2680cc21 )
simmeanJacc2680 <- data.frame(apply(simJacc2680,2,mean))
PIJacc2680 <- t(data.frame(apply(simJacc2680,2,PI,prob=0.89)))
F21JaccSim2680 <- data.frame(cbind(simmeanJacc2680,PIJacc2680))
F21JaccSim2680$PlotCN <- all2680cc21  $PlotCN
F21JaccSim2680 <- F21JaccSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim2680) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4580 <- link(JaccSplit21_beta, data=all4580cc21 )
simmeanJacc4580 <- data.frame(apply(simJacc4580,2,mean))
PIJacc4580 <- t(data.frame(apply(simJacc4580,2,PI,prob=0.89)))
F21JaccSim4580 <- data.frame(cbind(simmeanJacc4580,PIJacc4580))
F21JaccSim4580$PlotCN <- all4580cc21  $PlotCN
F21JaccSim4580 <- F21JaccSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim4580) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8580 <- link(JaccSplit21_beta, data=all8580cc21 )
simmeanJacc8580 <- data.frame(apply(simJacc8580,2,mean))
PIJacc8580 <- t(data.frame(apply(simJacc8580,2,PI,prob=0.89)))
F21JaccSim8580 <- data.frame(cbind(simmeanJacc8580,PIJacc8580))
F21JaccSim8580$PlotCN <- all8580cc21  $PlotCN
F21JaccSim8580 <- F21JaccSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F21JaccSim8580) <- c("PlotCN","Jaccards","5CI","95CI")


#create vector of new colnames
JaccSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Jacc", "5CI", "95CI"), x)))
JaccSimnames<- append(JaccSimnames,"PlotCN",after=0)

#group data by RCP pathway
F21JaccSim26 <- F21JaccSim2625 %>% cbind(F21JaccSim2630[,2:4]) %>%
  cbind(F21JaccSim2635[,2:4]) %>% cbind(F21JaccSim2640[,2:4]) %>% cbind(F21JaccSim2645[,2:4]) %>%
  cbind(F21JaccSim2650[,2:4]) %>% cbind(F21JaccSim2655[,2:4]) %>% cbind(F21JaccSim2660[,2:4]) %>%
  cbind(F21JaccSim2665[,2:4]) %>% cbind(F21JaccSim2670[,2:4]) %>% cbind(F21JaccSim2675[,2:4]) %>%
  cbind(F21JaccSim2680[,2:4])
colnames(F21JaccSim26) <- JaccSimnames
write.csv(F21JaccSim26,file="F21JaccPred26b.csv")

F21JaccSim45 <- F21JaccSim4525 %>% cbind(F21JaccSim4530[,2:4]) %>%
  cbind(F21JaccSim4535[,2:4]) %>% cbind(F21JaccSim4540[,2:4]) %>% cbind(F21JaccSim4545[,2:4]) %>%
  cbind(F21JaccSim4550[,2:4]) %>% cbind(F21JaccSim4555[,2:4]) %>% cbind(F21JaccSim4560[,2:4]) %>%
  cbind(F21JaccSim4565[,2:4]) %>% cbind(F21JaccSim4570[,2:4]) %>% cbind(F21JaccSim4575[,2:4]) %>%
  cbind(F21JaccSim4580[,2:4])
colnames(F21JaccSim45) <- JaccSimnames
write.csv(F21JaccSim45,file="F21JaccPred45b.csv")

F21JaccSim85 <- F21JaccSim8525 %>% cbind(F21JaccSim8530[,2:4]) %>%
  cbind(F21JaccSim8535[,2:4]) %>% cbind(F21JaccSim8540[,2:4]) %>% cbind(F21JaccSim8545[,2:4]) %>%
  cbind(F21JaccSim8550[,2:4]) %>% cbind(F21JaccSim8555[,2:4]) %>% cbind(F21JaccSim8560[,2:4]) %>%
  cbind(F21JaccSim8565[,2:4]) %>% cbind(F21JaccSim8570[,2:4]) %>% cbind(F21JaccSim8575[,2:4]) %>%
  cbind(F21JaccSim8580[,2:4])
colnames(F21JaccSim85) <- JaccSimnames
write.csv(F21JaccSim85,file="F21JaccPred85b.csv")


#fgroup 23
simJacc2625 <- link(JaccSplit23_beta, data=all2625cc23 )
simmeanJacc2625 <- data.frame(apply(simJacc2625,2,mean))
PIJacc2625 <- t(data.frame(apply(simJacc2625,2,PI,prob=0.89)))
F23JaccSim2625 <- data.frame(cbind(simmeanJacc2625,PIJacc2625))
F23JaccSim2625$PlotCN <- all2625cc23  $PlotCN
F23JaccSim2625 <- F23JaccSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2625) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4525 <- link(JaccSplit23_beta, data=all4525cc23 )
simmeanJacc4525 <- data.frame(apply(simJacc4525,2,mean))
PIJacc4525 <- t(data.frame(apply(simJacc4525,2,PI,prob=0.89)))
F23JaccSim4525 <- data.frame(cbind(simmeanJacc4525,PIJacc4525))
F23JaccSim4525$PlotCN <- all4525cc23  $PlotCN
F23JaccSim4525 <- F23JaccSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4525) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8525 <- link(JaccSplit23_beta, data=all8525cc23 )
simmeanJacc8525 <- data.frame(apply(simJacc8525,2,mean))
PIJacc8525 <- t(data.frame(apply(simJacc8525,2,PI,prob=0.89)))
F23JaccSim8525 <- data.frame(cbind(simmeanJacc8525,PIJacc8525))
F23JaccSim8525$PlotCN <- all8525cc23  $PlotCN
F23JaccSim8525 <- F23JaccSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8525) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2630 <- link(JaccSplit23_beta, data=all2630cc23 )
simmeanJacc2630 <- data.frame(apply(simJacc2630,2,mean))
PIJacc2630 <- t(data.frame(apply(simJacc2630,2,PI,prob=0.89)))
F23JaccSim2630 <- data.frame(cbind(simmeanJacc2630,PIJacc2630))
F23JaccSim2630$PlotCN <- all2630cc23  $PlotCN
F23JaccSim2630 <- F23JaccSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2630) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4530 <- link(JaccSplit23_beta, data=all4530cc23 )
simmeanJacc4530 <- data.frame(apply(simJacc4530,2,mean))
PIJacc4530 <- t(data.frame(apply(simJacc4530,2,PI,prob=0.89)))
F23JaccSim4530 <- data.frame(cbind(simmeanJacc4530,PIJacc4530))
F23JaccSim4530$PlotCN <- all4530cc23  $PlotCN
F23JaccSim4530 <- F23JaccSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4530) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8530 <- link(JaccSplit23_beta, data=all8530cc23 )
simmeanJacc8530 <- data.frame(apply(simJacc8530,2,mean))
PIJacc8530 <- t(data.frame(apply(simJacc8530,2,PI,prob=0.89)))
F23JaccSim8530 <- data.frame(cbind(simmeanJacc8530,PIJacc8530))
F23JaccSim8530$PlotCN <- all8530cc23  $PlotCN
F23JaccSim8530 <- F23JaccSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8530) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2635 <- link(JaccSplit23_beta, data=all2635cc23 )
simmeanJacc2635 <- data.frame(apply(simJacc2635,2,mean))
PIJacc2635 <- t(data.frame(apply(simJacc2635,2,PI,prob=0.89)))
F23JaccSim2635 <- data.frame(cbind(simmeanJacc2635,PIJacc2635))
F23JaccSim2635$PlotCN <- all2635cc23  $PlotCN
F23JaccSim2635 <- F23JaccSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2635) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4535 <- link(JaccSplit23_beta, data=all4535cc23 )
simmeanJacc4535 <- data.frame(apply(simJacc4535,2,mean))
PIJacc4535 <- t(data.frame(apply(simJacc4535,2,PI,prob=0.89)))
F23JaccSim4535 <- data.frame(cbind(simmeanJacc4535,PIJacc4535))
F23JaccSim4535$PlotCN <- all4535cc23  $PlotCN
F23JaccSim4535 <- F23JaccSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4535) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8535 <- link(JaccSplit23_beta, data=all8535cc23 )
simmeanJacc8535 <- data.frame(apply(simJacc8535,2,mean))
PIJacc8535 <- t(data.frame(apply(simJacc8535,2,PI,prob=0.89)))
F23JaccSim8535 <- data.frame(cbind(simmeanJacc8535,PIJacc8535))
F23JaccSim8535$PlotCN <- all8535cc23  $PlotCN
F23JaccSim8535 <- F23JaccSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8535) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2640 <- link(JaccSplit23_beta, data=all2640cc23 )
simmeanJacc2640 <- data.frame(apply(simJacc2640,2,mean))
PIJacc2640 <- t(data.frame(apply(simJacc2640,2,PI,prob=0.89)))
F23JaccSim2640 <- data.frame(cbind(simmeanJacc2640,PIJacc2640))
F23JaccSim2640$PlotCN <- all2640cc23  $PlotCN
F23JaccSim2640 <- F23JaccSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2640) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4540 <- link(JaccSplit23_beta, data=all4540cc23 )
simmeanJacc4540 <- data.frame(apply(simJacc4540,2,mean))
PIJacc4540 <- t(data.frame(apply(simJacc4540,2,PI,prob=0.89)))
F23JaccSim4540 <- data.frame(cbind(simmeanJacc4540,PIJacc4540))
F23JaccSim4540$PlotCN <- all4540cc23  $PlotCN
F23JaccSim4540 <- F23JaccSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4540) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8540 <- link(JaccSplit23_beta, data=all8540cc23 )
simmeanJacc8540 <- data.frame(apply(simJacc8540,2,mean))
PIJacc8540 <- t(data.frame(apply(simJacc8540,2,PI,prob=0.89)))
F23JaccSim8540 <- data.frame(cbind(simmeanJacc8540,PIJacc8540))
F23JaccSim8540$PlotCN <- all8540cc23  $PlotCN
F23JaccSim8540 <- F23JaccSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8540) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2645 <- link(JaccSplit23_beta, data=all2645cc23 )
simmeanJacc2645 <- data.frame(apply(simJacc2645,2,mean))
PIJacc2645 <- t(data.frame(apply(simJacc2645,2,PI,prob=0.89)))
F23JaccSim2645 <- data.frame(cbind(simmeanJacc2645,PIJacc2645))
F23JaccSim2645$PlotCN <- all2645cc23  $PlotCN
F23JaccSim2645 <- F23JaccSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2645) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4545 <- link(JaccSplit23_beta, data=all4545cc23 )
simmeanJacc4545 <- data.frame(apply(simJacc4545,2,mean))
PIJacc4545 <- t(data.frame(apply(simJacc4545,2,PI,prob=0.89)))
F23JaccSim4545 <- data.frame(cbind(simmeanJacc4545,PIJacc4545))
F23JaccSim4545$PlotCN <- all4545cc23  $PlotCN
F23JaccSim4545 <- F23JaccSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4545) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8545 <- link(JaccSplit23_beta, data=all8545cc23 )
simmeanJacc8545 <- data.frame(apply(simJacc8545,2,mean))
PIJacc8545 <- t(data.frame(apply(simJacc8545,2,PI,prob=0.89)))
F23JaccSim8545 <- data.frame(cbind(simmeanJacc8545,PIJacc8545))
F23JaccSim8545$PlotCN <- all8545cc23  $PlotCN
F23JaccSim8545 <- F23JaccSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8545) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2650 <- link(JaccSplit23_beta, data=all2650cc23 )
simmeanJacc2650 <- data.frame(apply(simJacc2650,2,mean))
PIJacc2650 <- t(data.frame(apply(simJacc2650,2,PI,prob=0.89)))
F23JaccSim2650 <- data.frame(cbind(simmeanJacc2650,PIJacc2650))
F23JaccSim2650$PlotCN <- all2650cc23  $PlotCN
F23JaccSim2650 <- F23JaccSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2650) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4550 <- link(JaccSplit23_beta, data=all4550cc23 )
simmeanJacc4550 <- data.frame(apply(simJacc4550,2,mean))
PIJacc4550 <- t(data.frame(apply(simJacc4550,2,PI,prob=0.89)))
F23JaccSim4550 <- data.frame(cbind(simmeanJacc4550,PIJacc4550))
F23JaccSim4550$PlotCN <- all4550cc23  $PlotCN
F23JaccSim4550 <- F23JaccSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4550) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8550 <- link(JaccSplit23_beta, data=all8550cc23 )
simmeanJacc8550 <- data.frame(apply(simJacc8550,2,mean))
PIJacc8550 <- t(data.frame(apply(simJacc8550,2,PI,prob=0.89)))
F23JaccSim8550 <- data.frame(cbind(simmeanJacc8550,PIJacc8550))
F23JaccSim8550$PlotCN <- all8550cc23  $PlotCN
F23JaccSim8550 <- F23JaccSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8550) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2655 <- link(JaccSplit23_beta, data=all2655cc23 )
simmeanJacc2655 <- data.frame(apply(simJacc2655,2,mean))
PIJacc2655 <- t(data.frame(apply(simJacc2655,2,PI,prob=0.89)))
F23JaccSim2655 <- data.frame(cbind(simmeanJacc2655,PIJacc2655))
F23JaccSim2655$PlotCN <- all2655cc23  $PlotCN
F23JaccSim2655 <- F23JaccSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2655) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4555 <- link(JaccSplit23_beta, data=all4555cc23 )
simmeanJacc4555 <- data.frame(apply(simJacc4555,2,mean))
PIJacc4555 <- t(data.frame(apply(simJacc4555,2,PI,prob=0.89)))
F23JaccSim4555 <- data.frame(cbind(simmeanJacc4555,PIJacc4555))
F23JaccSim4555$PlotCN <- all4555cc23  $PlotCN
F23JaccSim4555 <- F23JaccSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4555) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8555 <- link(JaccSplit23_beta, data=all8555cc23 )
simmeanJacc8555 <- data.frame(apply(simJacc8555,2,mean))
PIJacc8555 <- t(data.frame(apply(simJacc8555,2,PI,prob=0.89)))
F23JaccSim8555 <- data.frame(cbind(simmeanJacc8555,PIJacc8555))
F23JaccSim8555$PlotCN <- all8555cc23  $PlotCN
F23JaccSim8555 <- F23JaccSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8555) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2660 <- link(JaccSplit23_beta, data=all2660cc23 )
simmeanJacc2660 <- data.frame(apply(simJacc2660,2,mean))
PIJacc2660 <- t(data.frame(apply(simJacc2660,2,PI,prob=0.89)))
F23JaccSim2660 <- data.frame(cbind(simmeanJacc2660,PIJacc2660))
F23JaccSim2660$PlotCN <- all2660cc23  $PlotCN
F23JaccSim2660 <- F23JaccSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2660) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4560 <- link(JaccSplit23_beta, data=all4560cc23 )
simmeanJacc4560 <- data.frame(apply(simJacc4560,2,mean))
PIJacc4560 <- t(data.frame(apply(simJacc4560,2,PI,prob=0.89)))
F23JaccSim4560 <- data.frame(cbind(simmeanJacc4560,PIJacc4560))
F23JaccSim4560$PlotCN <- all4560cc23  $PlotCN
F23JaccSim4560 <- F23JaccSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4560) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8560 <- link(JaccSplit23_beta, data=all8560cc23 )
simmeanJacc8560 <- data.frame(apply(simJacc8560,2,mean))
PIJacc8560 <- t(data.frame(apply(simJacc8560,2,PI,prob=0.89)))
F23JaccSim8560 <- data.frame(cbind(simmeanJacc8560,PIJacc8560))
F23JaccSim8560$PlotCN <- all8560cc23  $PlotCN
F23JaccSim8560 <- F23JaccSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8560) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2665 <- link(JaccSplit23_beta, data=all2665cc23 )
simmeanJacc2665 <- data.frame(apply(simJacc2665,2,mean))
PIJacc2665 <- t(data.frame(apply(simJacc2665,2,PI,prob=0.89)))
F23JaccSim2665 <- data.frame(cbind(simmeanJacc2665,PIJacc2665))
F23JaccSim2665$PlotCN <- all2665cc23  $PlotCN
F23JaccSim2665 <- F23JaccSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2665) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4565 <- link(JaccSplit23_beta, data=all4565cc23 )
simmeanJacc4565 <- data.frame(apply(simJacc4565,2,mean))
PIJacc4565 <- t(data.frame(apply(simJacc4565,2,PI,prob=0.89)))
F23JaccSim4565 <- data.frame(cbind(simmeanJacc4565,PIJacc4565))
F23JaccSim4565$PlotCN <- all4565cc23  $PlotCN
F23JaccSim4565 <- F23JaccSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4565) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8565 <- link(JaccSplit23_beta, data=all8565cc23 )
simmeanJacc8565 <- data.frame(apply(simJacc8565,2,mean))
PIJacc8565 <- t(data.frame(apply(simJacc8565,2,PI,prob=0.89)))
F23JaccSim8565 <- data.frame(cbind(simmeanJacc8565,PIJacc8565))
F23JaccSim8565$PlotCN <- all8565cc23  $PlotCN
F23JaccSim8565 <- F23JaccSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8565) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2670 <- link(JaccSplit23_beta, data=all2670cc23 )
simmeanJacc2670 <- data.frame(apply(simJacc2670,2,mean))
PIJacc2670 <- t(data.frame(apply(simJacc2670,2,PI,prob=0.89)))
F23JaccSim2670 <- data.frame(cbind(simmeanJacc2670,PIJacc2670))
F23JaccSim2670$PlotCN <- all2670cc23  $PlotCN
F23JaccSim2670 <- F23JaccSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2670) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4570 <- link(JaccSplit23_beta, data=all4570cc23 )
simmeanJacc4570 <- data.frame(apply(simJacc4570,2,mean))
PIJacc4570 <- t(data.frame(apply(simJacc4570,2,PI,prob=0.89)))
F23JaccSim4570 <- data.frame(cbind(simmeanJacc4570,PIJacc4570))
F23JaccSim4570$PlotCN <- all4570cc23  $PlotCN
F23JaccSim4570 <- F23JaccSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4570) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8570 <- link(JaccSplit23_beta, data=all8570cc23 )
simmeanJacc8570 <- data.frame(apply(simJacc8570,2,mean))
PIJacc8570 <- t(data.frame(apply(simJacc8570,2,PI,prob=0.89)))
F23JaccSim8570 <- data.frame(cbind(simmeanJacc8570,PIJacc8570))
F23JaccSim8570$PlotCN <- all8570cc23  $PlotCN
F23JaccSim8570 <- F23JaccSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8570) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2675 <- link(JaccSplit23_beta, data=all2675cc23 )
simmeanJacc2675 <- data.frame(apply(simJacc2675,2,mean))
PIJacc2675 <- t(data.frame(apply(simJacc2675,2,PI,prob=0.89)))
F23JaccSim2675 <- data.frame(cbind(simmeanJacc2675,PIJacc2675))
F23JaccSim2675$PlotCN <- all2675cc23  $PlotCN
F23JaccSim2675 <- F23JaccSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2675) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4575 <- link(JaccSplit23_beta, data=all4575cc23 )
simmeanJacc4575 <- data.frame(apply(simJacc4575,2,mean))
PIJacc4575 <- t(data.frame(apply(simJacc4575,2,PI,prob=0.89)))
F23JaccSim4575 <- data.frame(cbind(simmeanJacc4575,PIJacc4575))
F23JaccSim4575$PlotCN <- all4575cc23  $PlotCN
F23JaccSim4575 <- F23JaccSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4575) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8575 <- link(JaccSplit23_beta, data=all8575cc23 )
simmeanJacc8575 <- data.frame(apply(simJacc8575,2,mean))
PIJacc8575 <- t(data.frame(apply(simJacc8575,2,PI,prob=0.89)))
F23JaccSim8575 <- data.frame(cbind(simmeanJacc8575,PIJacc8575))
F23JaccSim8575$PlotCN <- all8575cc23  $PlotCN
F23JaccSim8575 <- F23JaccSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8575) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2680 <- link(JaccSplit23_beta, data=all2680cc23 )
simmeanJacc2680 <- data.frame(apply(simJacc2680,2,mean))
PIJacc2680 <- t(data.frame(apply(simJacc2680,2,PI,prob=0.89)))
F23JaccSim2680 <- data.frame(cbind(simmeanJacc2680,PIJacc2680))
F23JaccSim2680$PlotCN <- all2680cc23  $PlotCN
F23JaccSim2680 <- F23JaccSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim2680) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4580 <- link(JaccSplit23_beta, data=all4580cc23 )
simmeanJacc4580 <- data.frame(apply(simJacc4580,2,mean))
PIJacc4580 <- t(data.frame(apply(simJacc4580,2,PI,prob=0.89)))
F23JaccSim4580 <- data.frame(cbind(simmeanJacc4580,PIJacc4580))
F23JaccSim4580$PlotCN <- all4580cc23  $PlotCN
F23JaccSim4580 <- F23JaccSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim4580) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8580 <- link(JaccSplit23_beta, data=all8580cc23 )
simmeanJacc8580 <- data.frame(apply(simJacc8580,2,mean))
PIJacc8580 <- t(data.frame(apply(simJacc8580,2,PI,prob=0.89)))
F23JaccSim8580 <- data.frame(cbind(simmeanJacc8580,PIJacc8580))
F23JaccSim8580$PlotCN <- all8580cc23  $PlotCN
F23JaccSim8580 <- F23JaccSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F23JaccSim8580) <- c("PlotCN","Jaccards","5CI","95CI")

#create vector of new colnames
JaccSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Jacc", "5CI", "95CI"), x)))
JaccSimnames<- append(JaccSimnames,"PlotCN",after=0)

#group data by RCP pathway
F23JaccSim26 <- F23JaccSim2625 %>% cbind(F23JaccSim2630[,2:4]) %>%
  cbind(F23JaccSim2635[,2:4]) %>% cbind(F23JaccSim2640[,2:4]) %>% cbind(F23JaccSim2645[,2:4]) %>%
  cbind(F23JaccSim2650[,2:4]) %>% cbind(F23JaccSim2655[,2:4]) %>% cbind(F23JaccSim2660[,2:4]) %>%
  cbind(F23JaccSim2665[,2:4]) %>% cbind(F23JaccSim2670[,2:4]) %>% cbind(F23JaccSim2675[,2:4]) %>%
  cbind(F23JaccSim2680[,2:4])
colnames(F23JaccSim26) <- JaccSimnames
write.csv(F23JaccSim26,file="F23JaccPred26b.csv")

F23JaccSim45 <- F23JaccSim4525 %>% cbind(F23JaccSim4530[,2:4]) %>%
  cbind(F23JaccSim4535[,2:4]) %>% cbind(F23JaccSim4540[,2:4]) %>% cbind(F23JaccSim4545[,2:4]) %>%
  cbind(F23JaccSim4550[,2:4]) %>% cbind(F23JaccSim4555[,2:4]) %>% cbind(F23JaccSim4560[,2:4]) %>%
  cbind(F23JaccSim4565[,2:4]) %>% cbind(F23JaccSim4570[,2:4]) %>% cbind(F23JaccSim4575[,2:4]) %>%
  cbind(F23JaccSim4580[,2:4])
colnames(F23JaccSim45) <- JaccSimnames
write.csv(F23JaccSim45,file="F23JaccPred45b.csv")

F23JaccSim85 <- F23JaccSim8525 %>% cbind(F23JaccSim8530[,2:4]) %>%
  cbind(F23JaccSim8535[,2:4]) %>% cbind(F23JaccSim8540[,2:4]) %>% cbind(F23JaccSim8545[,2:4]) %>%
  cbind(F23JaccSim8550[,2:4]) %>% cbind(F23JaccSim8555[,2:4]) %>% cbind(F23JaccSim8560[,2:4]) %>%
  cbind(F23JaccSim8565[,2:4]) %>% cbind(F23JaccSim8570[,2:4]) %>% cbind(F23JaccSim8575[,2:4]) %>%
  cbind(F23JaccSim8580[,2:4])
colnames(F23JaccSim85) <- JaccSimnames
write.csv(F23JaccSim85,file="F23JaccPred85b.csv")


#fgroup 24
simJacc2625 <- link(JaccSplit24_beta, data=all2625cc24 )
simmeanJacc2625 <- data.frame(apply(simJacc2625,2,mean))
PIJacc2625 <- t(data.frame(apply(simJacc2625,2,PI,prob=0.89)))
F24JaccSim2625 <- data.frame(cbind(simmeanJacc2625,PIJacc2625))
F24JaccSim2625$PlotCN <- all2625cc24  $PlotCN
F24JaccSim2625 <- F24JaccSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2625) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4525 <- link(JaccSplit24_beta, data=all4525cc24 )
simmeanJacc4525 <- data.frame(apply(simJacc4525,2,mean))
PIJacc4525 <- t(data.frame(apply(simJacc4525,2,PI,prob=0.89)))
F24JaccSim4525 <- data.frame(cbind(simmeanJacc4525,PIJacc4525))
F24JaccSim4525$PlotCN <- all4525cc24  $PlotCN
F24JaccSim4525 <- F24JaccSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4525) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8525 <- link(JaccSplit24_beta, data=all8525cc24 )
simmeanJacc8525 <- data.frame(apply(simJacc8525,2,mean))
PIJacc8525 <- t(data.frame(apply(simJacc8525,2,PI,prob=0.89)))
F24JaccSim8525 <- data.frame(cbind(simmeanJacc8525,PIJacc8525))
F24JaccSim8525$PlotCN <- all8525cc24  $PlotCN
F24JaccSim8525 <- F24JaccSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8525) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2630 <- link(JaccSplit24_beta, data=all2630cc24 )
simmeanJacc2630 <- data.frame(apply(simJacc2630,2,mean))
PIJacc2630 <- t(data.frame(apply(simJacc2630,2,PI,prob=0.89)))
F24JaccSim2630 <- data.frame(cbind(simmeanJacc2630,PIJacc2630))
F24JaccSim2630$PlotCN <- all2630cc24  $PlotCN
F24JaccSim2630 <- F24JaccSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2630) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4530 <- link(JaccSplit24_beta, data=all4530cc24 )
simmeanJacc4530 <- data.frame(apply(simJacc4530,2,mean))
PIJacc4530 <- t(data.frame(apply(simJacc4530,2,PI,prob=0.89)))
F24JaccSim4530 <- data.frame(cbind(simmeanJacc4530,PIJacc4530))
F24JaccSim4530$PlotCN <- all4530cc24  $PlotCN
F24JaccSim4530 <- F24JaccSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4530) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8530 <- link(JaccSplit24_beta, data=all8530cc24 )
simmeanJacc8530 <- data.frame(apply(simJacc8530,2,mean))
PIJacc8530 <- t(data.frame(apply(simJacc8530,2,PI,prob=0.89)))
F24JaccSim8530 <- data.frame(cbind(simmeanJacc8530,PIJacc8530))
F24JaccSim8530$PlotCN <- all8530cc24  $PlotCN
F24JaccSim8530 <- F24JaccSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8530) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2635 <- link(JaccSplit24_beta, data=all2635cc24 )
simmeanJacc2635 <- data.frame(apply(simJacc2635,2,mean))
PIJacc2635 <- t(data.frame(apply(simJacc2635,2,PI,prob=0.89)))
F24JaccSim2635 <- data.frame(cbind(simmeanJacc2635,PIJacc2635))
F24JaccSim2635$PlotCN <- all2635cc24  $PlotCN
F24JaccSim2635 <- F24JaccSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2635) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4535 <- link(JaccSplit24_beta, data=all4535cc24 )
simmeanJacc4535 <- data.frame(apply(simJacc4535,2,mean))
PIJacc4535 <- t(data.frame(apply(simJacc4535,2,PI,prob=0.89)))
F24JaccSim4535 <- data.frame(cbind(simmeanJacc4535,PIJacc4535))
F24JaccSim4535$PlotCN <- all4535cc24  $PlotCN
F24JaccSim4535 <- F24JaccSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4535) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8535 <- link(JaccSplit24_beta, data=all8535cc24 )
simmeanJacc8535 <- data.frame(apply(simJacc8535,2,mean))
PIJacc8535 <- t(data.frame(apply(simJacc8535,2,PI,prob=0.89)))
F24JaccSim8535 <- data.frame(cbind(simmeanJacc8535,PIJacc8535))
F24JaccSim8535$PlotCN <- all8535cc24  $PlotCN
F24JaccSim8535 <- F24JaccSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8535) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2640 <- link(JaccSplit24_beta, data=all2640cc24 )
simmeanJacc2640 <- data.frame(apply(simJacc2640,2,mean))
PIJacc2640 <- t(data.frame(apply(simJacc2640,2,PI,prob=0.89)))
F24JaccSim2640 <- data.frame(cbind(simmeanJacc2640,PIJacc2640))
F24JaccSim2640$PlotCN <- all2640cc24  $PlotCN
F24JaccSim2640 <- F24JaccSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2640) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4540 <- link(JaccSplit24_beta, data=all4540cc24 )
simmeanJacc4540 <- data.frame(apply(simJacc4540,2,mean))
PIJacc4540 <- t(data.frame(apply(simJacc4540,2,PI,prob=0.89)))
F24JaccSim4540 <- data.frame(cbind(simmeanJacc4540,PIJacc4540))
F24JaccSim4540$PlotCN <- all4540cc24  $PlotCN
F24JaccSim4540 <- F24JaccSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4540) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8540 <- link(JaccSplit24_beta, data=all8540cc24 )
simmeanJacc8540 <- data.frame(apply(simJacc8540,2,mean))
PIJacc8540 <- t(data.frame(apply(simJacc8540,2,PI,prob=0.89)))
F24JaccSim8540 <- data.frame(cbind(simmeanJacc8540,PIJacc8540))
F24JaccSim8540$PlotCN <- all8540cc24  $PlotCN
F24JaccSim8540 <- F24JaccSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8540) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2645 <- link(JaccSplit24_beta, data=all2645cc24 )
simmeanJacc2645 <- data.frame(apply(simJacc2645,2,mean))
PIJacc2645 <- t(data.frame(apply(simJacc2645,2,PI,prob=0.89)))
F24JaccSim2645 <- data.frame(cbind(simmeanJacc2645,PIJacc2645))
F24JaccSim2645$PlotCN <- all2645cc24  $PlotCN
F24JaccSim2645 <- F24JaccSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2645) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4545 <- link(JaccSplit24_beta, data=all4545cc24 )
simmeanJacc4545 <- data.frame(apply(simJacc4545,2,mean))
PIJacc4545 <- t(data.frame(apply(simJacc4545,2,PI,prob=0.89)))
F24JaccSim4545 <- data.frame(cbind(simmeanJacc4545,PIJacc4545))
F24JaccSim4545$PlotCN <- all4545cc24  $PlotCN
F24JaccSim4545 <- F24JaccSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4545) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8545 <- link(JaccSplit24_beta, data=all8545cc24 )
simmeanJacc8545 <- data.frame(apply(simJacc8545,2,mean))
PIJacc8545 <- t(data.frame(apply(simJacc8545,2,PI,prob=0.89)))
F24JaccSim8545 <- data.frame(cbind(simmeanJacc8545,PIJacc8545))
F24JaccSim8545$PlotCN <- all8545cc24  $PlotCN
F24JaccSim8545 <- F24JaccSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8545) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2650 <- link(JaccSplit24_beta, data=all2650cc24 )
simmeanJacc2650 <- data.frame(apply(simJacc2650,2,mean))
PIJacc2650 <- t(data.frame(apply(simJacc2650,2,PI,prob=0.89)))
F24JaccSim2650 <- data.frame(cbind(simmeanJacc2650,PIJacc2650))
F24JaccSim2650$PlotCN <- all2650cc24  $PlotCN
F24JaccSim2650 <- F24JaccSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2650) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4550 <- link(JaccSplit24_beta, data=all4550cc24 )
simmeanJacc4550 <- data.frame(apply(simJacc4550,2,mean))
PIJacc4550 <- t(data.frame(apply(simJacc4550,2,PI,prob=0.89)))
F24JaccSim4550 <- data.frame(cbind(simmeanJacc4550,PIJacc4550))
F24JaccSim4550$PlotCN <- all4550cc24  $PlotCN
F24JaccSim4550 <- F24JaccSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4550) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8550 <- link(JaccSplit24_beta, data=all8550cc24 )
simmeanJacc8550 <- data.frame(apply(simJacc8550,2,mean))
PIJacc8550 <- t(data.frame(apply(simJacc8550,2,PI,prob=0.89)))
F24JaccSim8550 <- data.frame(cbind(simmeanJacc8550,PIJacc8550))
F24JaccSim8550$PlotCN <- all8550cc24  $PlotCN
F24JaccSim8550 <- F24JaccSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8550) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2655 <- link(JaccSplit24_beta, data=all2655cc24 )
simmeanJacc2655 <- data.frame(apply(simJacc2655,2,mean))
PIJacc2655 <- t(data.frame(apply(simJacc2655,2,PI,prob=0.89)))
F24JaccSim2655 <- data.frame(cbind(simmeanJacc2655,PIJacc2655))
F24JaccSim2655$PlotCN <- all2655cc24  $PlotCN
F24JaccSim2655 <- F24JaccSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2655) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4555 <- link(JaccSplit24_beta, data=all4555cc24 )
simmeanJacc4555 <- data.frame(apply(simJacc4555,2,mean))
PIJacc4555 <- t(data.frame(apply(simJacc4555,2,PI,prob=0.89)))
F24JaccSim4555 <- data.frame(cbind(simmeanJacc4555,PIJacc4555))
F24JaccSim4555$PlotCN <- all4555cc24  $PlotCN
F24JaccSim4555 <- F24JaccSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4555) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8555 <- link(JaccSplit24_beta, data=all8555cc24 )
simmeanJacc8555 <- data.frame(apply(simJacc8555,2,mean))
PIJacc8555 <- t(data.frame(apply(simJacc8555,2,PI,prob=0.89)))
F24JaccSim8555 <- data.frame(cbind(simmeanJacc8555,PIJacc8555))
F24JaccSim8555$PlotCN <- all8555cc24  $PlotCN
F24JaccSim8555 <- F24JaccSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8555) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2660 <- link(JaccSplit24_beta, data=all2660cc24 )
simmeanJacc2660 <- data.frame(apply(simJacc2660,2,mean))
PIJacc2660 <- t(data.frame(apply(simJacc2660,2,PI,prob=0.89)))
F24JaccSim2660 <- data.frame(cbind(simmeanJacc2660,PIJacc2660))
F24JaccSim2660$PlotCN <- all2660cc24  $PlotCN
F24JaccSim2660 <- F24JaccSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2660) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4560 <- link(JaccSplit24_beta, data=all4560cc24 )
simmeanJacc4560 <- data.frame(apply(simJacc4560,2,mean))
PIJacc4560 <- t(data.frame(apply(simJacc4560,2,PI,prob=0.89)))
F24JaccSim4560 <- data.frame(cbind(simmeanJacc4560,PIJacc4560))
F24JaccSim4560$PlotCN <- all4560cc24  $PlotCN
F24JaccSim4560 <- F24JaccSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4560) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8560 <- link(JaccSplit24_beta, data=all8560cc24 )
simmeanJacc8560 <- data.frame(apply(simJacc8560,2,mean))
PIJacc8560 <- t(data.frame(apply(simJacc8560,2,PI,prob=0.89)))
F24JaccSim8560 <- data.frame(cbind(simmeanJacc8560,PIJacc8560))
F24JaccSim8560$PlotCN <- all8560cc24  $PlotCN
F24JaccSim8560 <- F24JaccSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8560) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2665 <- link(JaccSplit24_beta, data=all2665cc24 )
simmeanJacc2665 <- data.frame(apply(simJacc2665,2,mean))
PIJacc2665 <- t(data.frame(apply(simJacc2665,2,PI,prob=0.89)))
F24JaccSim2665 <- data.frame(cbind(simmeanJacc2665,PIJacc2665))
F24JaccSim2665$PlotCN <- all2665cc24  $PlotCN
F24JaccSim2665 <- F24JaccSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2665) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4565 <- link(JaccSplit24_beta, data=all4565cc24 )
simmeanJacc4565 <- data.frame(apply(simJacc4565,2,mean))
PIJacc4565 <- t(data.frame(apply(simJacc4565,2,PI,prob=0.89)))
F24JaccSim4565 <- data.frame(cbind(simmeanJacc4565,PIJacc4565))
F24JaccSim4565$PlotCN <- all4565cc24  $PlotCN
F24JaccSim4565 <- F24JaccSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4565) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8565 <- link(JaccSplit24_beta, data=all8565cc24 )
simmeanJacc8565 <- data.frame(apply(simJacc8565,2,mean))
PIJacc8565 <- t(data.frame(apply(simJacc8565,2,PI,prob=0.89)))
F24JaccSim8565 <- data.frame(cbind(simmeanJacc8565,PIJacc8565))
F24JaccSim8565$PlotCN <- all8565cc24  $PlotCN
F24JaccSim8565 <- F24JaccSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8565) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2670 <- link(JaccSplit24_beta, data=all2670cc24 )
simmeanJacc2670 <- data.frame(apply(simJacc2670,2,mean))
PIJacc2670 <- t(data.frame(apply(simJacc2670,2,PI,prob=0.89)))
F24JaccSim2670 <- data.frame(cbind(simmeanJacc2670,PIJacc2670))
F24JaccSim2670$PlotCN <- all2670cc24  $PlotCN
F24JaccSim2670 <- F24JaccSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2670) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4570 <- link(JaccSplit24_beta, data=all4570cc24 )
simmeanJacc4570 <- data.frame(apply(simJacc4570,2,mean))
PIJacc4570 <- t(data.frame(apply(simJacc4570,2,PI,prob=0.89)))
F24JaccSim4570 <- data.frame(cbind(simmeanJacc4570,PIJacc4570))
F24JaccSim4570$PlotCN <- all4570cc24  $PlotCN
F24JaccSim4570 <- F24JaccSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4570) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8570 <- link(JaccSplit24_beta, data=all8570cc24 )
simmeanJacc8570 <- data.frame(apply(simJacc8570,2,mean))
PIJacc8570 <- t(data.frame(apply(simJacc8570,2,PI,prob=0.89)))
F24JaccSim8570 <- data.frame(cbind(simmeanJacc8570,PIJacc8570))
F24JaccSim8570$PlotCN <- all8570cc24  $PlotCN
F24JaccSim8570 <- F24JaccSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8570) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2675 <- link(JaccSplit24_beta, data=all2675cc24 )
simmeanJacc2675 <- data.frame(apply(simJacc2675,2,mean))
PIJacc2675 <- t(data.frame(apply(simJacc2675,2,PI,prob=0.89)))
F24JaccSim2675 <- data.frame(cbind(simmeanJacc2675,PIJacc2675))
F24JaccSim2675$PlotCN <- all2675cc24  $PlotCN
F24JaccSim2675 <- F24JaccSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2675) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4575 <- link(JaccSplit24_beta, data=all4575cc24 )
simmeanJacc4575 <- data.frame(apply(simJacc4575,2,mean))
PIJacc4575 <- t(data.frame(apply(simJacc4575,2,PI,prob=0.89)))
F24JaccSim4575 <- data.frame(cbind(simmeanJacc4575,PIJacc4575))
F24JaccSim4575$PlotCN <- all4575cc24  $PlotCN
F24JaccSim4575 <- F24JaccSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4575) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8575 <- link(JaccSplit24_beta, data=all8575cc24 )
simmeanJacc8575 <- data.frame(apply(simJacc8575,2,mean))
PIJacc8575 <- t(data.frame(apply(simJacc8575,2,PI,prob=0.89)))
F24JaccSim8575 <- data.frame(cbind(simmeanJacc8575,PIJacc8575))
F24JaccSim8575$PlotCN <- all8575cc24  $PlotCN
F24JaccSim8575 <- F24JaccSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8575) <- c("PlotCN","Jaccards","5CI","95CI")

simJacc2680 <- link(JaccSplit24_beta, data=all2680cc24 )
simmeanJacc2680 <- data.frame(apply(simJacc2680,2,mean))
PIJacc2680 <- t(data.frame(apply(simJacc2680,2,PI,prob=0.89)))
F24JaccSim2680 <- data.frame(cbind(simmeanJacc2680,PIJacc2680))
F24JaccSim2680$PlotCN <- all2680cc24  $PlotCN
F24JaccSim2680 <- F24JaccSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim2680) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc4580 <- link(JaccSplit24_beta, data=all4580cc24 )
simmeanJacc4580 <- data.frame(apply(simJacc4580,2,mean))
PIJacc4580 <- t(data.frame(apply(simJacc4580,2,PI,prob=0.89)))
F24JaccSim4580 <- data.frame(cbind(simmeanJacc4580,PIJacc4580))
F24JaccSim4580$PlotCN <- all4580cc24  $PlotCN
F24JaccSim4580 <- F24JaccSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim4580) <- c("PlotCN","Jaccards","5CI","95CI")
simJacc8580 <- link(JaccSplit24_beta, data=all8580cc24 )
simmeanJacc8580 <- data.frame(apply(simJacc8580,2,mean))
PIJacc8580 <- t(data.frame(apply(simJacc8580,2,PI,prob=0.89)))
F24JaccSim8580 <- data.frame(cbind(simmeanJacc8580,PIJacc8580))
F24JaccSim8580$PlotCN <- all8580cc24  $PlotCN
F24JaccSim8580 <- F24JaccSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F24JaccSim8580) <- c("PlotCN","Jaccards","5CI","95CI")

#create vector of new colnames
JaccSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Jacc", "5CI", "95CI"), x)))
JaccSimnames<- append(JaccSimnames,"PlotCN",after=0)

#group data by RCP pathway
F24JaccSim26T <- F24JaccSim2625 %>% cbind(F24JaccSim2630[,2:4]) %>%
  cbind(F24JaccSim2635[,2:4]) %>% cbind(F24JaccSim2640[,2:4]) %>% cbind(F24JaccSim2645[,2:4]) %>%
  cbind(F24JaccSim2650[,2:4]) %>% cbind(F24JaccSim2655[,2:4]) %>% cbind(F24JaccSim2660[,2:4]) %>%
  cbind(F24JaccSim2665[,2:4]) %>% cbind(F24JaccSim2670[,2:4]) %>% cbind(F24JaccSim2675[,2:4]) %>%
  cbind(F24JaccSim2680[,2:4])
colnames(F24JaccSim26T) <- JaccSimnames
write.csv(F24JaccSim26T,file="F24JaccPred26T.csv")

F24JaccSim45T <- F24JaccSim4525 %>% cbind(F24JaccSim4530[,2:4]) %>%
  cbind(F24JaccSim4535[,2:4]) %>% cbind(F24JaccSim4540[,2:4]) %>% cbind(F24JaccSim4545[,2:4]) %>%
  cbind(F24JaccSim4550[,2:4]) %>% cbind(F24JaccSim4555[,2:4]) %>% cbind(F24JaccSim4560[,2:4]) %>%
  cbind(F24JaccSim4565[,2:4]) %>% cbind(F24JaccSim4570[,2:4]) %>% cbind(F24JaccSim4575[,2:4]) %>%
  cbind(F24JaccSim4580[,2:4])
colnames(F24JaccSim45T) <- JaccSimnames
write.csv(F24JaccSim45T,file="F24JaccPred45T.csv")

F24JaccSim85T <- F24JaccSim8525 %>% cbind(F24JaccSim8530[,2:4]) %>%
  cbind(F24JaccSim8535[,2:4]) %>% cbind(F24JaccSim8540[,2:4]) %>% cbind(F24JaccSim8545[,2:4]) %>%
  cbind(F24JaccSim8550[,2:4]) %>% cbind(F24JaccSim8555[,2:4]) %>% cbind(F24JaccSim8560[,2:4]) %>%
  cbind(F24JaccSim8565[,2:4]) %>% cbind(F24JaccSim8570[,2:4]) %>% cbind(F24JaccSim8575[,2:4]) %>%
  cbind(F24JaccSim8580[,2:4])
colnames(F24JaccSim85T) <- JaccSimnames
write.csv(F24JaccSim85T,file="F24JaccPred85T.csv")

#MPD Predictions
#fgroup 1
simMPD2625 <- link(MPDSplit1, data=all2625cc1)
simmeanMPD2625 <- data.frame(apply(simMPD2625,2,mean))
PIMPD2625 <- t(data.frame(apply(simMPD2625,2,PI,prob=0.89)))
F1MPDSim2625 <- data.frame(cbind(simmeanMPD2625,PIMPD2625))
F1MPDSim2625$PlotCN <- all2625cc1 $PlotCN
F1MPDSim2625 <- F1MPDSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2625) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4525 <- link(MPDSplit1, data=all4525cc1)
simmeanMPD4525 <- data.frame(apply(simMPD4525,2,mean))
PIMPD4525 <- t(data.frame(apply(simMPD4525,2,PI,prob=0.89)))
F1MPDSim4525 <- data.frame(cbind(simmeanMPD4525,PIMPD4525))
F1MPDSim4525$PlotCN <- all4525cc1 $PlotCN
F1MPDSim4525 <- F1MPDSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4525) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8525 <- link(MPDSplit1, data=all8525cc1)
simmeanMPD8525 <- data.frame(apply(simMPD8525,2,mean))
PIMPD8525 <- t(data.frame(apply(simMPD8525,2,PI,prob=0.89)))
F1MPDSim8525 <- data.frame(cbind(simmeanMPD8525,PIMPD8525))
F1MPDSim8525$PlotCN <- all8525cc1 $PlotCN
F1MPDSim8525 <- F1MPDSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8525) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2630 <- link(MPDSplit1, data=all2630cc1)
simmeanMPD2630 <- data.frame(apply(simMPD2630,2,mean))
PIMPD2630 <- t(data.frame(apply(simMPD2630,2,PI,prob=0.89)))
F1MPDSim2630 <- data.frame(cbind(simmeanMPD2630,PIMPD2630))
F1MPDSim2630$PlotCN <- all2630cc1 $PlotCN
F1MPDSim2630 <- F1MPDSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2630) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4530 <- link(MPDSplit1, data=all4530cc1)
simmeanMPD4530 <- data.frame(apply(simMPD4530,2,mean))
PIMPD4530 <- t(data.frame(apply(simMPD4530,2,PI,prob=0.89)))
F1MPDSim4530 <- data.frame(cbind(simmeanMPD4530,PIMPD4530))
F1MPDSim4530$PlotCN <- all4530cc1 $PlotCN
F1MPDSim4530 <- F1MPDSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4530) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8530 <- link(MPDSplit1, data=all8530cc1)
simmeanMPD8530 <- data.frame(apply(simMPD8530,2,mean))
PIMPD8530 <- t(data.frame(apply(simMPD8530,2,PI,prob=0.89)))
F1MPDSim8530 <- data.frame(cbind(simmeanMPD8530,PIMPD8530))
F1MPDSim8530$PlotCN <- all8530cc1 $PlotCN
F1MPDSim8530 <- F1MPDSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8530) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2635 <- link(MPDSplit1, data=all2635cc1)
simmeanMPD2635 <- data.frame(apply(simMPD2635,2,mean))
PIMPD2635 <- t(data.frame(apply(simMPD2635,2,PI,prob=0.89)))
F1MPDSim2635 <- data.frame(cbind(simmeanMPD2635,PIMPD2635))
F1MPDSim2635$PlotCN <- all2635cc1 $PlotCN
F1MPDSim2635 <- F1MPDSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2635) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4535 <- link(MPDSplit1, data=all4535cc1)
simmeanMPD4535 <- data.frame(apply(simMPD4535,2,mean))
PIMPD4535 <- t(data.frame(apply(simMPD4535,2,PI,prob=0.89)))
F1MPDSim4535 <- data.frame(cbind(simmeanMPD4535,PIMPD4535))
F1MPDSim4535$PlotCN <- all4535cc1 $PlotCN
F1MPDSim4535 <- F1MPDSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4535) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8535 <- link(MPDSplit1, data=all8535cc1)
simmeanMPD8535 <- data.frame(apply(simMPD8535,2,mean))
PIMPD8535 <- t(data.frame(apply(simMPD8535,2,PI,prob=0.89)))
F1MPDSim8535 <- data.frame(cbind(simmeanMPD8535,PIMPD8535))
F1MPDSim8535$PlotCN <- all8535cc1 $PlotCN
F1MPDSim8535 <- F1MPDSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8535) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2640 <- link(MPDSplit1, data=all2640cc1)
simmeanMPD2640 <- data.frame(apply(simMPD2640,2,mean))
PIMPD2640 <- t(data.frame(apply(simMPD2640,2,PI,prob=0.89)))
F1MPDSim2640 <- data.frame(cbind(simmeanMPD2640,PIMPD2640))
F1MPDSim2640$PlotCN <- all2640cc1 $PlotCN
F1MPDSim2640 <- F1MPDSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2640) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4540 <- link(MPDSplit1, data=all4540cc1)
simmeanMPD4540 <- data.frame(apply(simMPD4540,2,mean))
PIMPD4540 <- t(data.frame(apply(simMPD4540,2,PI,prob=0.89)))
F1MPDSim4540 <- data.frame(cbind(simmeanMPD4540,PIMPD4540))
F1MPDSim4540$PlotCN <- all4540cc1 $PlotCN
F1MPDSim4540 <- F1MPDSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4540) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8540 <- link(MPDSplit1, data=all8540cc1)
simmeanMPD8540 <- data.frame(apply(simMPD8540,2,mean))
PIMPD8540 <- t(data.frame(apply(simMPD8540,2,PI,prob=0.89)))
F1MPDSim8540 <- data.frame(cbind(simmeanMPD8540,PIMPD8540))
F1MPDSim8540$PlotCN <- all8540cc1 $PlotCN
F1MPDSim8540 <- F1MPDSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8540) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2645 <- link(MPDSplit1, data=all2645cc1)
simmeanMPD2645 <- data.frame(apply(simMPD2645,2,mean))
PIMPD2645 <- t(data.frame(apply(simMPD2645,2,PI,prob=0.89)))
F1MPDSim2645 <- data.frame(cbind(simmeanMPD2645,PIMPD2645))
F1MPDSim2645$PlotCN <- all2645cc1 $PlotCN
F1MPDSim2645 <- F1MPDSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2645) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4545 <- link(MPDSplit1, data=all4545cc1)
simmeanMPD4545 <- data.frame(apply(simMPD4545,2,mean))
PIMPD4545 <- t(data.frame(apply(simMPD4545,2,PI,prob=0.89)))
F1MPDSim4545 <- data.frame(cbind(simmeanMPD4545,PIMPD4545))
F1MPDSim4545$PlotCN <- all4545cc1 $PlotCN
F1MPDSim4545 <- F1MPDSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4545) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8545 <- link(MPDSplit1, data=all8545cc1)
simmeanMPD8545 <- data.frame(apply(simMPD8545,2,mean))
PIMPD8545 <- t(data.frame(apply(simMPD8545,2,PI,prob=0.89)))
F1MPDSim8545 <- data.frame(cbind(simmeanMPD8545,PIMPD8545))
F1MPDSim8545$PlotCN <- all8545cc1 $PlotCN
F1MPDSim8545 <- F1MPDSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8545) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2650 <- link(MPDSplit1, data=all2650cc1)
simmeanMPD2650 <- data.frame(apply(simMPD2650,2,mean))
PIMPD2650 <- t(data.frame(apply(simMPD2650,2,PI,prob=0.89)))
F1MPDSim2650 <- data.frame(cbind(simmeanMPD2650,PIMPD2650))
F1MPDSim2650$PlotCN <- all2650cc1 $PlotCN
F1MPDSim2650 <- F1MPDSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2650) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4550 <- link(MPDSplit1, data=all4550cc1)
simmeanMPD4550 <- data.frame(apply(simMPD4550,2,mean))
PIMPD4550 <- t(data.frame(apply(simMPD4550,2,PI,prob=0.89)))
F1MPDSim4550 <- data.frame(cbind(simmeanMPD4550,PIMPD4550))
F1MPDSim4550$PlotCN <- all4550cc1 $PlotCN
F1MPDSim4550 <- F1MPDSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4550) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8550 <- link(MPDSplit1, data=all8550cc1)
simmeanMPD8550 <- data.frame(apply(simMPD8550,2,mean))
PIMPD8550 <- t(data.frame(apply(simMPD8550,2,PI,prob=0.89)))
F1MPDSim8550 <- data.frame(cbind(simmeanMPD8550,PIMPD8550))
F1MPDSim8550$PlotCN <- all8550cc1 $PlotCN
F1MPDSim8550 <- F1MPDSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8550) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2655 <- link(MPDSplit1, data=all2655cc1)
simmeanMPD2655 <- data.frame(apply(simMPD2655,2,mean))
PIMPD2655 <- t(data.frame(apply(simMPD2655,2,PI,prob=0.89)))
F1MPDSim2655 <- data.frame(cbind(simmeanMPD2655,PIMPD2655))
F1MPDSim2655$PlotCN <- all2655cc1 $PlotCN
F1MPDSim2655 <- F1MPDSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2655) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4555 <- link(MPDSplit1, data=all4555cc1)
simmeanMPD4555 <- data.frame(apply(simMPD4555,2,mean))
PIMPD4555 <- t(data.frame(apply(simMPD4555,2,PI,prob=0.89)))
F1MPDSim4555 <- data.frame(cbind(simmeanMPD4555,PIMPD4555))
F1MPDSim4555$PlotCN <- all4555cc1 $PlotCN
F1MPDSim4555 <- F1MPDSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4555) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8555 <- link(MPDSplit1, data=all8555cc1)
simmeanMPD8555 <- data.frame(apply(simMPD8555,2,mean))
PIMPD8555 <- t(data.frame(apply(simMPD8555,2,PI,prob=0.89)))
F1MPDSim8555 <- data.frame(cbind(simmeanMPD8555,PIMPD8555))
F1MPDSim8555$PlotCN <- all8555cc1 $PlotCN
F1MPDSim8555 <- F1MPDSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8555) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2660 <- link(MPDSplit1, data=all2660cc1)
simmeanMPD2660 <- data.frame(apply(simMPD2660,2,mean))
PIMPD2660 <- t(data.frame(apply(simMPD2660,2,PI,prob=0.89)))
F1MPDSim2660 <- data.frame(cbind(simmeanMPD2660,PIMPD2660))
F1MPDSim2660$PlotCN <- all2660cc1 $PlotCN
F1MPDSim2660 <- F1MPDSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2660) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4560 <- link(MPDSplit1, data=all4560cc1)
simmeanMPD4560 <- data.frame(apply(simMPD4560,2,mean))
PIMPD4560 <- t(data.frame(apply(simMPD4560,2,PI,prob=0.89)))
F1MPDSim4560 <- data.frame(cbind(simmeanMPD4560,PIMPD4560))
F1MPDSim4560$PlotCN <- all4560cc1 $PlotCN
F1MPDSim4560 <- F1MPDSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4560) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8560 <- link(MPDSplit1, data=all8560cc1)
simmeanMPD8560 <- data.frame(apply(simMPD8560,2,mean))
PIMPD8560 <- t(data.frame(apply(simMPD8560,2,PI,prob=0.89)))
F1MPDSim8560 <- data.frame(cbind(simmeanMPD8560,PIMPD8560))
F1MPDSim8560$PlotCN <- all8560cc1 $PlotCN
F1MPDSim8560 <- F1MPDSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8560) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2665 <- link(MPDSplit1, data=all2665cc1)
simmeanMPD2665 <- data.frame(apply(simMPD2665,2,mean))
PIMPD2665 <- t(data.frame(apply(simMPD2665,2,PI,prob=0.89)))
F1MPDSim2665 <- data.frame(cbind(simmeanMPD2665,PIMPD2665))
F1MPDSim2665$PlotCN <- all2665cc1 $PlotCN
F1MPDSim2665 <- F1MPDSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2665) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4565 <- link(MPDSplit1, data=all4565cc1)
simmeanMPD4565 <- data.frame(apply(simMPD4565,2,mean))
PIMPD4565 <- t(data.frame(apply(simMPD4565,2,PI,prob=0.89)))
F1MPDSim4565 <- data.frame(cbind(simmeanMPD4565,PIMPD4565))
F1MPDSim4565$PlotCN <- all4565cc1 $PlotCN
F1MPDSim4565 <- F1MPDSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4565) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8565 <- link(MPDSplit1, data=all8565cc1)
simmeanMPD8565 <- data.frame(apply(simMPD8565,2,mean))
PIMPD8565 <- t(data.frame(apply(simMPD8565,2,PI,prob=0.89)))
F1MPDSim8565 <- data.frame(cbind(simmeanMPD8565,PIMPD8565))
F1MPDSim8565$PlotCN <- all8565cc1 $PlotCN
F1MPDSim8565 <- F1MPDSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8565) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2670 <- link(MPDSplit1, data=all2670cc1)
simmeanMPD2670 <- data.frame(apply(simMPD2670,2,mean))
PIMPD2670 <- t(data.frame(apply(simMPD2670,2,PI,prob=0.89)))
F1MPDSim2670 <- data.frame(cbind(simmeanMPD2670,PIMPD2670))
F1MPDSim2670$PlotCN <- all2670cc1 $PlotCN
F1MPDSim2670 <- F1MPDSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2670) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4570 <- link(MPDSplit1, data=all4570cc1)
simmeanMPD4570 <- data.frame(apply(simMPD4570,2,mean))
PIMPD4570 <- t(data.frame(apply(simMPD4570,2,PI,prob=0.89)))
F1MPDSim4570 <- data.frame(cbind(simmeanMPD4570,PIMPD4570))
F1MPDSim4570$PlotCN <- all4570cc1 $PlotCN
F1MPDSim4570 <- F1MPDSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4570) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8570 <- link(MPDSplit1, data=all8570cc1)
simmeanMPD8570 <- data.frame(apply(simMPD8570,2,mean))
PIMPD8570 <- t(data.frame(apply(simMPD8570,2,PI,prob=0.89)))
F1MPDSim8570 <- data.frame(cbind(simmeanMPD8570,PIMPD8570))
F1MPDSim8570$PlotCN <- all8570cc1 $PlotCN
F1MPDSim8570 <- F1MPDSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8570) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2675 <- link(MPDSplit1, data=all2675cc1)
simmeanMPD2675 <- data.frame(apply(simMPD2675,2,mean))
PIMPD2675 <- t(data.frame(apply(simMPD2675,2,PI,prob=0.89)))
F1MPDSim2675 <- data.frame(cbind(simmeanMPD2675,PIMPD2675))
F1MPDSim2675$PlotCN <- all2675cc1 $PlotCN
F1MPDSim2675 <- F1MPDSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2675) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4575 <- link(MPDSplit1, data=all4575cc1)
simmeanMPD4575 <- data.frame(apply(simMPD4575,2,mean))
PIMPD4575 <- t(data.frame(apply(simMPD4575,2,PI,prob=0.89)))
F1MPDSim4575 <- data.frame(cbind(simmeanMPD4575,PIMPD4575))
F1MPDSim4575$PlotCN <- all4575cc1 $PlotCN
F1MPDSim4575 <- F1MPDSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4575) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8575 <- link(MPDSplit1, data=all8575cc1)
simmeanMPD8575 <- data.frame(apply(simMPD8575,2,mean))
PIMPD8575 <- t(data.frame(apply(simMPD8575,2,PI,prob=0.89)))
F1MPDSim8575 <- data.frame(cbind(simmeanMPD8575,PIMPD8575))
F1MPDSim8575$PlotCN <- all8575cc1 $PlotCN
F1MPDSim8575 <- F1MPDSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8575) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2680 <- link(MPDSplit1, data=all2680cc1)
simmeanMPD2680 <- data.frame(apply(simMPD2680,2,mean))
PIMPD2680 <- t(data.frame(apply(simMPD2680,2,PI,prob=0.89)))
F1MPDSim2680 <- data.frame(cbind(simmeanMPD2680,PIMPD2680))
F1MPDSim2680$PlotCN <- all2680cc1 $PlotCN
F1MPDSim2680 <- F1MPDSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim2680) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4580 <- link(MPDSplit1, data=all4580cc1)
simmeanMPD4580 <- data.frame(apply(simMPD4580,2,mean))
PIMPD4580 <- t(data.frame(apply(simMPD4580,2,PI,prob=0.89)))
F1MPDSim4580 <- data.frame(cbind(simmeanMPD4580,PIMPD4580))
F1MPDSim4580$PlotCN <- all4580cc1 $PlotCN
F1MPDSim4580 <- F1MPDSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim4580) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8580 <- link(MPDSplit1, data=all8580cc1)
simmeanMPD8580 <- data.frame(apply(simMPD8580,2,mean))
PIMPD8580 <- t(data.frame(apply(simMPD8580,2,PI,prob=0.89)))
F1MPDSim8580 <- data.frame(cbind(simmeanMPD8580,PIMPD8580))
F1MPDSim8580$PlotCN <- all8580cc1 $PlotCN
F1MPDSim8580 <- F1MPDSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F1MPDSim8580) <- c("PlotCN","MPDPerAcre","5CI","95CI")


#create vector of new colnames
MPDSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("MPD", "5CI", "95CI"), x)))
MPDSimnames<- append(MPDSimnames,"PlotCN",after=0)

#group data by RCP pathway
F1MPDSim26 <- F1MPDSim2625 %>% cbind(F1MPDSim2630[,2:4]) %>%
  cbind(F1MPDSim2635[,2:4]) %>% cbind(F1MPDSim2640[,2:4]) %>% cbind(F1MPDSim2645[,2:4]) %>%
  cbind(F1MPDSim2650[,2:4]) %>% cbind(F1MPDSim2655[,2:4]) %>% cbind(F1MPDSim2660[,2:4]) %>%
  cbind(F1MPDSim2665[,2:4]) %>% cbind(F1MPDSim2670[,2:4]) %>% cbind(F1MPDSim2675[,2:4]) %>%
  cbind(F1MPDSim2680[,2:4])
colnames(F1MPDSim26) <- MPDSimnames
write.csv(F1MPDSim26,file="F1MPDPred26.csv")

F1MPDSim45 <- F1MPDSim4525 %>% cbind(F1MPDSim4530[,2:4]) %>%
  cbind(F1MPDSim4535[,2:4]) %>% cbind(F1MPDSim4540[,2:4]) %>% cbind(F1MPDSim4545[,2:4]) %>%
  cbind(F1MPDSim4550[,2:4]) %>% cbind(F1MPDSim4555[,2:4]) %>% cbind(F1MPDSim4560[,2:4]) %>%
  cbind(F1MPDSim4565[,2:4]) %>% cbind(F1MPDSim4570[,2:4]) %>% cbind(F1MPDSim4575[,2:4]) %>%
  cbind(F1MPDSim4580[,2:4])
colnames(F1MPDSim45) <- MPDSimnames
write.csv(F1MPDSim45,file="F1MPDPred45.csv")

F1MPDSim85 <- F1MPDSim8525 %>% cbind(F1MPDSim8530[,2:4]) %>%
  cbind(F1MPDSim8535[,2:4]) %>% cbind(F1MPDSim8540[,2:4]) %>% cbind(F1MPDSim8545[,2:4]) %>%
  cbind(F1MPDSim8550[,2:4]) %>% cbind(F1MPDSim8555[,2:4]) %>% cbind(F1MPDSim8560[,2:4]) %>%
  cbind(F1MPDSim8565[,2:4]) %>% cbind(F1MPDSim8570[,2:4]) %>% cbind(F1MPDSim8575[,2:4]) %>%
  cbind(F1MPDSim8580[,2:4])
colnames(F1MPDSim85) <- MPDSimnames
write.csv(F1MPDSim85,file="F1MPDPred85.csv")


#fgroup 5
simMPD2625 <- link(MPDSplit5, data=all2625cc5 )
simmeanMPD2625 <- data.frame(apply(simMPD2625,2,mean))
PIMPD2625 <- t(data.frame(apply(simMPD2625,2,PI,prob=0.89)))
F5MPDSim2625 <- data.frame(cbind(simmeanMPD2625,PIMPD2625))
F5MPDSim2625$PlotCN <- all2625cc5  $PlotCN
F5MPDSim2625 <- F5MPDSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2625) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4525 <- link(MPDSplit5, data=all4525cc5 )
simmeanMPD4525 <- data.frame(apply(simMPD4525,2,mean))
PIMPD4525 <- t(data.frame(apply(simMPD4525,2,PI,prob=0.89)))
F5MPDSim4525 <- data.frame(cbind(simmeanMPD4525,PIMPD4525))
F5MPDSim4525$PlotCN <- all4525cc5  $PlotCN
F5MPDSim4525 <- F5MPDSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4525) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8525 <- link(MPDSplit5, data=all8525cc5 )
simmeanMPD8525 <- data.frame(apply(simMPD8525,2,mean))
PIMPD8525 <- t(data.frame(apply(simMPD8525,2,PI,prob=0.89)))
F5MPDSim8525 <- data.frame(cbind(simmeanMPD8525,PIMPD8525))
F5MPDSim8525$PlotCN <- all8525cc5  $PlotCN
F5MPDSim8525 <- F5MPDSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8525) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2630 <- link(MPDSplit5, data=all2630cc5 )
simmeanMPD2630 <- data.frame(apply(simMPD2630,2,mean))
PIMPD2630 <- t(data.frame(apply(simMPD2630,2,PI,prob=0.89)))
F5MPDSim2630 <- data.frame(cbind(simmeanMPD2630,PIMPD2630))
F5MPDSim2630$PlotCN <- all2630cc5  $PlotCN
F5MPDSim2630 <- F5MPDSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2630) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4530 <- link(MPDSplit5, data=all4530cc5 )
simmeanMPD4530 <- data.frame(apply(simMPD4530,2,mean))
PIMPD4530 <- t(data.frame(apply(simMPD4530,2,PI,prob=0.89)))
F5MPDSim4530 <- data.frame(cbind(simmeanMPD4530,PIMPD4530))
F5MPDSim4530$PlotCN <- all4530cc5  $PlotCN
F5MPDSim4530 <- F5MPDSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4530) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8530 <- link(MPDSplit5, data=all8530cc5 )
simmeanMPD8530 <- data.frame(apply(simMPD8530,2,mean))
PIMPD8530 <- t(data.frame(apply(simMPD8530,2,PI,prob=0.89)))
F5MPDSim8530 <- data.frame(cbind(simmeanMPD8530,PIMPD8530))
F5MPDSim8530$PlotCN <- all8530cc5  $PlotCN
F5MPDSim8530 <- F5MPDSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8530) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2635 <- link(MPDSplit5, data=all2635cc5 )
simmeanMPD2635 <- data.frame(apply(simMPD2635,2,mean))
PIMPD2635 <- t(data.frame(apply(simMPD2635,2,PI,prob=0.89)))
F5MPDSim2635 <- data.frame(cbind(simmeanMPD2635,PIMPD2635))
F5MPDSim2635$PlotCN <- all2635cc5  $PlotCN
F5MPDSim2635 <- F5MPDSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2635) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4535 <- link(MPDSplit5, data=all4535cc5 )
simmeanMPD4535 <- data.frame(apply(simMPD4535,2,mean))
PIMPD4535 <- t(data.frame(apply(simMPD4535,2,PI,prob=0.89)))
F5MPDSim4535 <- data.frame(cbind(simmeanMPD4535,PIMPD4535))
F5MPDSim4535$PlotCN <- all4535cc5  $PlotCN
F5MPDSim4535 <- F5MPDSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4535) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8535 <- link(MPDSplit5, data=all8535cc5 )
simmeanMPD8535 <- data.frame(apply(simMPD8535,2,mean))
PIMPD8535 <- t(data.frame(apply(simMPD8535,2,PI,prob=0.89)))
F5MPDSim8535 <- data.frame(cbind(simmeanMPD8535,PIMPD8535))
F5MPDSim8535$PlotCN <- all8535cc5  $PlotCN
F5MPDSim8535 <- F5MPDSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8535) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2640 <- link(MPDSplit5, data=all2640cc5 )
simmeanMPD2640 <- data.frame(apply(simMPD2640,2,mean))
PIMPD2640 <- t(data.frame(apply(simMPD2640,2,PI,prob=0.89)))
F5MPDSim2640 <- data.frame(cbind(simmeanMPD2640,PIMPD2640))
F5MPDSim2640$PlotCN <- all2640cc5  $PlotCN
F5MPDSim2640 <- F5MPDSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2640) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4540 <- link(MPDSplit5, data=all4540cc5 )
simmeanMPD4540 <- data.frame(apply(simMPD4540,2,mean))
PIMPD4540 <- t(data.frame(apply(simMPD4540,2,PI,prob=0.89)))
F5MPDSim4540 <- data.frame(cbind(simmeanMPD4540,PIMPD4540))
F5MPDSim4540$PlotCN <- all4540cc5  $PlotCN
F5MPDSim4540 <- F5MPDSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4540) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8540 <- link(MPDSplit5, data=all8540cc5 )
simmeanMPD8540 <- data.frame(apply(simMPD8540,2,mean))
PIMPD8540 <- t(data.frame(apply(simMPD8540,2,PI,prob=0.89)))
F5MPDSim8540 <- data.frame(cbind(simmeanMPD8540,PIMPD8540))
F5MPDSim8540$PlotCN <- all8540cc5  $PlotCN
F5MPDSim8540 <- F5MPDSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8540) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2645 <- link(MPDSplit5, data=all2645cc5 )
simmeanMPD2645 <- data.frame(apply(simMPD2645,2,mean))
PIMPD2645 <- t(data.frame(apply(simMPD2645,2,PI,prob=0.89)))
F5MPDSim2645 <- data.frame(cbind(simmeanMPD2645,PIMPD2645))
F5MPDSim2645$PlotCN <- all2645cc5  $PlotCN
F5MPDSim2645 <- F5MPDSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2645) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4545 <- link(MPDSplit5, data=all4545cc5 )
simmeanMPD4545 <- data.frame(apply(simMPD4545,2,mean))
PIMPD4545 <- t(data.frame(apply(simMPD4545,2,PI,prob=0.89)))
F5MPDSim4545 <- data.frame(cbind(simmeanMPD4545,PIMPD4545))
F5MPDSim4545$PlotCN <- all4545cc5  $PlotCN
F5MPDSim4545 <- F5MPDSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4545) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8545 <- link(MPDSplit5, data=all8545cc5 )
simmeanMPD8545 <- data.frame(apply(simMPD8545,2,mean))
PIMPD8545 <- t(data.frame(apply(simMPD8545,2,PI,prob=0.89)))
F5MPDSim8545 <- data.frame(cbind(simmeanMPD8545,PIMPD8545))
F5MPDSim8545$PlotCN <- all8545cc5  $PlotCN
F5MPDSim8545 <- F5MPDSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8545) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2650 <- link(MPDSplit5, data=all2650cc5 )
simmeanMPD2650 <- data.frame(apply(simMPD2650,2,mean))
PIMPD2650 <- t(data.frame(apply(simMPD2650,2,PI,prob=0.89)))
F5MPDSim2650 <- data.frame(cbind(simmeanMPD2650,PIMPD2650))
F5MPDSim2650$PlotCN <- all2650cc5  $PlotCN
F5MPDSim2650 <- F5MPDSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2650) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4550 <- link(MPDSplit5, data=all4550cc5 )
simmeanMPD4550 <- data.frame(apply(simMPD4550,2,mean))
PIMPD4550 <- t(data.frame(apply(simMPD4550,2,PI,prob=0.89)))
F5MPDSim4550 <- data.frame(cbind(simmeanMPD4550,PIMPD4550))
F5MPDSim4550$PlotCN <- all4550cc5  $PlotCN
F5MPDSim4550 <- F5MPDSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4550) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8550 <- link(MPDSplit5, data=all8550cc5 )
simmeanMPD8550 <- data.frame(apply(simMPD8550,2,mean))
PIMPD8550 <- t(data.frame(apply(simMPD8550,2,PI,prob=0.89)))
F5MPDSim8550 <- data.frame(cbind(simmeanMPD8550,PIMPD8550))
F5MPDSim8550$PlotCN <- all8550cc5  $PlotCN
F5MPDSim8550 <- F5MPDSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8550) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2655 <- link(MPDSplit5, data=all2655cc5 )
simmeanMPD2655 <- data.frame(apply(simMPD2655,2,mean))
PIMPD2655 <- t(data.frame(apply(simMPD2655,2,PI,prob=0.89)))
F5MPDSim2655 <- data.frame(cbind(simmeanMPD2655,PIMPD2655))
F5MPDSim2655$PlotCN <- all2655cc5  $PlotCN
F5MPDSim2655 <- F5MPDSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2655) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4555 <- link(MPDSplit5, data=all4555cc5 )
simmeanMPD4555 <- data.frame(apply(simMPD4555,2,mean))
PIMPD4555 <- t(data.frame(apply(simMPD4555,2,PI,prob=0.89)))
F5MPDSim4555 <- data.frame(cbind(simmeanMPD4555,PIMPD4555))
F5MPDSim4555$PlotCN <- all4555cc5  $PlotCN
F5MPDSim4555 <- F5MPDSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4555) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8555 <- link(MPDSplit5, data=all8555cc5 )
simmeanMPD8555 <- data.frame(apply(simMPD8555,2,mean))
PIMPD8555 <- t(data.frame(apply(simMPD8555,2,PI,prob=0.89)))
F5MPDSim8555 <- data.frame(cbind(simmeanMPD8555,PIMPD8555))
F5MPDSim8555$PlotCN <- all8555cc5  $PlotCN
F5MPDSim8555 <- F5MPDSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8555) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2660 <- link(MPDSplit5, data=all2660cc5 )
simmeanMPD2660 <- data.frame(apply(simMPD2660,2,mean))
PIMPD2660 <- t(data.frame(apply(simMPD2660,2,PI,prob=0.89)))
F5MPDSim2660 <- data.frame(cbind(simmeanMPD2660,PIMPD2660))
F5MPDSim2660$PlotCN <- all2660cc5  $PlotCN
F5MPDSim2660 <- F5MPDSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2660) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4560 <- link(MPDSplit5, data=all4560cc5 )
simmeanMPD4560 <- data.frame(apply(simMPD4560,2,mean))
PIMPD4560 <- t(data.frame(apply(simMPD4560,2,PI,prob=0.89)))
F5MPDSim4560 <- data.frame(cbind(simmeanMPD4560,PIMPD4560))
F5MPDSim4560$PlotCN <- all4560cc5  $PlotCN
F5MPDSim4560 <- F5MPDSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4560) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8560 <- link(MPDSplit5, data=all8560cc5 )
simmeanMPD8560 <- data.frame(apply(simMPD8560,2,mean))
PIMPD8560 <- t(data.frame(apply(simMPD8560,2,PI,prob=0.89)))
F5MPDSim8560 <- data.frame(cbind(simmeanMPD8560,PIMPD8560))
F5MPDSim8560$PlotCN <- all8560cc5  $PlotCN
F5MPDSim8560 <- F5MPDSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8560) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2665 <- link(MPDSplit5, data=all2665cc5 )
simmeanMPD2665 <- data.frame(apply(simMPD2665,2,mean))
PIMPD2665 <- t(data.frame(apply(simMPD2665,2,PI,prob=0.89)))
F5MPDSim2665 <- data.frame(cbind(simmeanMPD2665,PIMPD2665))
F5MPDSim2665$PlotCN <- all2665cc5  $PlotCN
F5MPDSim2665 <- F5MPDSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2665) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4565 <- link(MPDSplit5, data=all4565cc5 )
simmeanMPD4565 <- data.frame(apply(simMPD4565,2,mean))
PIMPD4565 <- t(data.frame(apply(simMPD4565,2,PI,prob=0.89)))
F5MPDSim4565 <- data.frame(cbind(simmeanMPD4565,PIMPD4565))
F5MPDSim4565$PlotCN <- all4565cc5  $PlotCN
F5MPDSim4565 <- F5MPDSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4565) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8565 <- link(MPDSplit5, data=all8565cc5 )
simmeanMPD8565 <- data.frame(apply(simMPD8565,2,mean))
PIMPD8565 <- t(data.frame(apply(simMPD8565,2,PI,prob=0.89)))
F5MPDSim8565 <- data.frame(cbind(simmeanMPD8565,PIMPD8565))
F5MPDSim8565$PlotCN <- all8565cc5  $PlotCN
F5MPDSim8565 <- F5MPDSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8565) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2670 <- link(MPDSplit5, data=all2670cc5 )
simmeanMPD2670 <- data.frame(apply(simMPD2670,2,mean))
PIMPD2670 <- t(data.frame(apply(simMPD2670,2,PI,prob=0.89)))
F5MPDSim2670 <- data.frame(cbind(simmeanMPD2670,PIMPD2670))
F5MPDSim2670$PlotCN <- all2670cc5  $PlotCN
F5MPDSim2670 <- F5MPDSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2670) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4570 <- link(MPDSplit5, data=all4570cc5 )
simmeanMPD4570 <- data.frame(apply(simMPD4570,2,mean))
PIMPD4570 <- t(data.frame(apply(simMPD4570,2,PI,prob=0.89)))
F5MPDSim4570 <- data.frame(cbind(simmeanMPD4570,PIMPD4570))
F5MPDSim4570$PlotCN <- all4570cc5  $PlotCN
F5MPDSim4570 <- F5MPDSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4570) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8570 <- link(MPDSplit5, data=all8570cc5 )
simmeanMPD8570 <- data.frame(apply(simMPD8570,2,mean))
PIMPD8570 <- t(data.frame(apply(simMPD8570,2,PI,prob=0.89)))
F5MPDSim8570 <- data.frame(cbind(simmeanMPD8570,PIMPD8570))
F5MPDSim8570$PlotCN <- all8570cc5  $PlotCN
F5MPDSim8570 <- F5MPDSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8570) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2675 <- link(MPDSplit5, data=all2675cc5 )
simmeanMPD2675 <- data.frame(apply(simMPD2675,2,mean))
PIMPD2675 <- t(data.frame(apply(simMPD2675,2,PI,prob=0.89)))
F5MPDSim2675 <- data.frame(cbind(simmeanMPD2675,PIMPD2675))
F5MPDSim2675$PlotCN <- all2675cc5  $PlotCN
F5MPDSim2675 <- F5MPDSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2675) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4575 <- link(MPDSplit5, data=all4575cc5 )
simmeanMPD4575 <- data.frame(apply(simMPD4575,2,mean))
PIMPD4575 <- t(data.frame(apply(simMPD4575,2,PI,prob=0.89)))
F5MPDSim4575 <- data.frame(cbind(simmeanMPD4575,PIMPD4575))
F5MPDSim4575$PlotCN <- all4575cc5  $PlotCN
F5MPDSim4575 <- F5MPDSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4575) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8575 <- link(MPDSplit5, data=all8575cc5 )
simmeanMPD8575 <- data.frame(apply(simMPD8575,2,mean))
PIMPD8575 <- t(data.frame(apply(simMPD8575,2,PI,prob=0.89)))
F5MPDSim8575 <- data.frame(cbind(simmeanMPD8575,PIMPD8575))
F5MPDSim8575$PlotCN <- all8575cc5  $PlotCN
F5MPDSim8575 <- F5MPDSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8575) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2680 <- link(MPDSplit5, data=all2680cc5 )
simmeanMPD2680 <- data.frame(apply(simMPD2680,2,mean))
PIMPD2680 <- t(data.frame(apply(simMPD2680,2,PI,prob=0.89)))
F5MPDSim2680 <- data.frame(cbind(simmeanMPD2680,PIMPD2680))
F5MPDSim2680$PlotCN <- all2680cc5  $PlotCN
F5MPDSim2680 <- F5MPDSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim2680) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4580 <- link(MPDSplit5, data=all4580cc5 )
simmeanMPD4580 <- data.frame(apply(simMPD4580,2,mean))
PIMPD4580 <- t(data.frame(apply(simMPD4580,2,PI,prob=0.89)))
F5MPDSim4580 <- data.frame(cbind(simmeanMPD4580,PIMPD4580))
F5MPDSim4580$PlotCN <- all4580cc5  $PlotCN
F5MPDSim4580 <- F5MPDSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim4580) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8580 <- link(MPDSplit5, data=all8580cc5 )
simmeanMPD8580 <- data.frame(apply(simMPD8580,2,mean))
PIMPD8580 <- t(data.frame(apply(simMPD8580,2,PI,prob=0.89)))
F5MPDSim8580 <- data.frame(cbind(simmeanMPD8580,PIMPD8580))
F5MPDSim8580$PlotCN <- all8580cc5  $PlotCN
F5MPDSim8580 <- F5MPDSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F5MPDSim8580) <- c("PlotCN","MPDPerAcre","5CI","95CI")

#group data by RCP pathway
F5MPDSim26 <- F5MPDSim2625 %>% cbind(F5MPDSim2630[,2:4]) %>%
  cbind(F5MPDSim2635[,2:4]) %>% cbind(F5MPDSim2640[,2:4]) %>% cbind(F5MPDSim2645[,2:4]) %>%
  cbind(F5MPDSim2650[,2:4]) %>% cbind(F5MPDSim2655[,2:4]) %>% cbind(F5MPDSim2660[,2:4]) %>%
  cbind(F5MPDSim2665[,2:4]) %>% cbind(F5MPDSim2670[,2:4]) %>% cbind(F5MPDSim2675[,2:4]) %>%
  cbind(F5MPDSim2680[,2:4])
colnames(F5MPDSim26) <- MPDSimnames
write.csv(F5MPDSim26,file="F5MPDPred26.csv")

F5MPDSim45 <- F5MPDSim4525 %>% cbind(F5MPDSim4530[,2:4]) %>%
  cbind(F5MPDSim4535[,2:4]) %>% cbind(F5MPDSim4540[,2:4]) %>% cbind(F5MPDSim4545[,2:4]) %>%
  cbind(F5MPDSim4550[,2:4]) %>% cbind(F5MPDSim4555[,2:4]) %>% cbind(F5MPDSim4560[,2:4]) %>%
  cbind(F5MPDSim4565[,2:4]) %>% cbind(F5MPDSim4570[,2:4]) %>% cbind(F5MPDSim4575[,2:4]) %>%
  cbind(F5MPDSim4580[,2:4])
colnames(F5MPDSim45) <- MPDSimnames
write.csv(F5MPDSim45,file="F5MPDPred45.csv")

F5MPDSim85 <- F5MPDSim8525 %>% cbind(F5MPDSim8530[,2:4]) %>%
  cbind(F5MPDSim8535[,2:4]) %>% cbind(F5MPDSim8540[,2:4]) %>% cbind(F5MPDSim8545[,2:4]) %>%
  cbind(F5MPDSim8550[,2:4]) %>% cbind(F5MPDSim8555[,2:4]) %>% cbind(F5MPDSim8560[,2:4]) %>%
  cbind(F5MPDSim8565[,2:4]) %>% cbind(F5MPDSim8570[,2:4]) %>% cbind(F5MPDSim8575[,2:4]) %>%
  cbind(F5MPDSim8580[,2:4])
colnames(F5MPDSim85) <- MPDSimnames
write.csv(F5MPDSim85,file="F5MPDPred85.csv")


#fgroup 20
simMPD2625 <- link(MPDSplit20, data=all2625cc20 )
simmeanMPD2625 <- data.frame(apply(simMPD2625,2,mean))
PIMPD2625 <- t(data.frame(apply(simMPD2625,2,PI,prob=0.89)))
F20MPDSim2625 <- data.frame(cbind(simmeanMPD2625,PIMPD2625))
F20MPDSim2625$PlotCN <- all2625cc20  $PlotCN
F20MPDSim2625 <- F20MPDSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2625) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4525 <- link(MPDSplit20, data=all4525cc20 )
simmeanMPD4525 <- data.frame(apply(simMPD4525,2,mean))
PIMPD4525 <- t(data.frame(apply(simMPD4525,2,PI,prob=0.89)))
F20MPDSim4525 <- data.frame(cbind(simmeanMPD4525,PIMPD4525))
F20MPDSim4525$PlotCN <- all4525cc20  $PlotCN
F20MPDSim4525 <- F20MPDSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4525) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8525 <- link(MPDSplit20, data=all8525cc20 )
simmeanMPD8525 <- data.frame(apply(simMPD8525,2,mean))
PIMPD8525 <- t(data.frame(apply(simMPD8525,2,PI,prob=0.89)))
F20MPDSim8525 <- data.frame(cbind(simmeanMPD8525,PIMPD8525))
F20MPDSim8525$PlotCN <- all8525cc20  $PlotCN
F20MPDSim8525 <- F20MPDSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8525) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2630 <- link(MPDSplit20, data=all2630cc20 )
simmeanMPD2630 <- data.frame(apply(simMPD2630,2,mean))
PIMPD2630 <- t(data.frame(apply(simMPD2630,2,PI,prob=0.89)))
F20MPDSim2630 <- data.frame(cbind(simmeanMPD2630,PIMPD2630))
F20MPDSim2630$PlotCN <- all2630cc20  $PlotCN
F20MPDSim2630 <- F20MPDSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2630) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4530 <- link(MPDSplit20, data=all4530cc20 )
simmeanMPD4530 <- data.frame(apply(simMPD4530,2,mean))
PIMPD4530 <- t(data.frame(apply(simMPD4530,2,PI,prob=0.89)))
F20MPDSim4530 <- data.frame(cbind(simmeanMPD4530,PIMPD4530))
F20MPDSim4530$PlotCN <- all4530cc20  $PlotCN
F20MPDSim4530 <- F20MPDSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4530) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8530 <- link(MPDSplit20, data=all8530cc20 )
simmeanMPD8530 <- data.frame(apply(simMPD8530,2,mean))
PIMPD8530 <- t(data.frame(apply(simMPD8530,2,PI,prob=0.89)))
F20MPDSim8530 <- data.frame(cbind(simmeanMPD8530,PIMPD8530))
F20MPDSim8530$PlotCN <- all8530cc20  $PlotCN
F20MPDSim8530 <- F20MPDSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8530) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2635 <- link(MPDSplit20, data=all2635cc20 )
simmeanMPD2635 <- data.frame(apply(simMPD2635,2,mean))
PIMPD2635 <- t(data.frame(apply(simMPD2635,2,PI,prob=0.89)))
F20MPDSim2635 <- data.frame(cbind(simmeanMPD2635,PIMPD2635))
F20MPDSim2635$PlotCN <- all2635cc20  $PlotCN
F20MPDSim2635 <- F20MPDSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2635) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4535 <- link(MPDSplit20, data=all4535cc20 )
simmeanMPD4535 <- data.frame(apply(simMPD4535,2,mean))
PIMPD4535 <- t(data.frame(apply(simMPD4535,2,PI,prob=0.89)))
F20MPDSim4535 <- data.frame(cbind(simmeanMPD4535,PIMPD4535))
F20MPDSim4535$PlotCN <- all4535cc20  $PlotCN
F20MPDSim4535 <- F20MPDSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4535) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8535 <- link(MPDSplit20, data=all8535cc20 )
simmeanMPD8535 <- data.frame(apply(simMPD8535,2,mean))
PIMPD8535 <- t(data.frame(apply(simMPD8535,2,PI,prob=0.89)))
F20MPDSim8535 <- data.frame(cbind(simmeanMPD8535,PIMPD8535))
F20MPDSim8535$PlotCN <- all8535cc20  $PlotCN
F20MPDSim8535 <- F20MPDSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8535) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2640 <- link(MPDSplit20, data=all2640cc20 )
simmeanMPD2640 <- data.frame(apply(simMPD2640,2,mean))
PIMPD2640 <- t(data.frame(apply(simMPD2640,2,PI,prob=0.89)))
F20MPDSim2640 <- data.frame(cbind(simmeanMPD2640,PIMPD2640))
F20MPDSim2640$PlotCN <- all2640cc20  $PlotCN
F20MPDSim2640 <- F20MPDSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2640) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4540 <- link(MPDSplit20, data=all4540cc20 )
simmeanMPD4540 <- data.frame(apply(simMPD4540,2,mean))
PIMPD4540 <- t(data.frame(apply(simMPD4540,2,PI,prob=0.89)))
F20MPDSim4540 <- data.frame(cbind(simmeanMPD4540,PIMPD4540))
F20MPDSim4540$PlotCN <- all4540cc20  $PlotCN
F20MPDSim4540 <- F20MPDSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4540) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8540 <- link(MPDSplit20, data=all8540cc20 )
simmeanMPD8540 <- data.frame(apply(simMPD8540,2,mean))
PIMPD8540 <- t(data.frame(apply(simMPD8540,2,PI,prob=0.89)))
F20MPDSim8540 <- data.frame(cbind(simmeanMPD8540,PIMPD8540))
F20MPDSim8540$PlotCN <- all8540cc20  $PlotCN
F20MPDSim8540 <- F20MPDSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8540) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2645 <- link(MPDSplit20, data=all2645cc20 )
simmeanMPD2645 <- data.frame(apply(simMPD2645,2,mean))
PIMPD2645 <- t(data.frame(apply(simMPD2645,2,PI,prob=0.89)))
F20MPDSim2645 <- data.frame(cbind(simmeanMPD2645,PIMPD2645))
F20MPDSim2645$PlotCN <- all2645cc20  $PlotCN
F20MPDSim2645 <- F20MPDSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2645) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4545 <- link(MPDSplit20, data=all4545cc20 )
simmeanMPD4545 <- data.frame(apply(simMPD4545,2,mean))
PIMPD4545 <- t(data.frame(apply(simMPD4545,2,PI,prob=0.89)))
F20MPDSim4545 <- data.frame(cbind(simmeanMPD4545,PIMPD4545))
F20MPDSim4545$PlotCN <- all4545cc20  $PlotCN
F20MPDSim4545 <- F20MPDSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4545) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8545 <- link(MPDSplit20, data=all8545cc20 )
simmeanMPD8545 <- data.frame(apply(simMPD8545,2,mean))
PIMPD8545 <- t(data.frame(apply(simMPD8545,2,PI,prob=0.89)))
F20MPDSim8545 <- data.frame(cbind(simmeanMPD8545,PIMPD8545))
F20MPDSim8545$PlotCN <- all8545cc20  $PlotCN
F20MPDSim8545 <- F20MPDSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8545) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2650 <- link(MPDSplit20, data=all2650cc20 )
simmeanMPD2650 <- data.frame(apply(simMPD2650,2,mean))
PIMPD2650 <- t(data.frame(apply(simMPD2650,2,PI,prob=0.89)))
F20MPDSim2650 <- data.frame(cbind(simmeanMPD2650,PIMPD2650))
F20MPDSim2650$PlotCN <- all2650cc20  $PlotCN
F20MPDSim2650 <- F20MPDSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2650) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4550 <- link(MPDSplit20, data=all4550cc20 )
simmeanMPD4550 <- data.frame(apply(simMPD4550,2,mean))
PIMPD4550 <- t(data.frame(apply(simMPD4550,2,PI,prob=0.89)))
F20MPDSim4550 <- data.frame(cbind(simmeanMPD4550,PIMPD4550))
F20MPDSim4550$PlotCN <- all4550cc20  $PlotCN
F20MPDSim4550 <- F20MPDSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4550) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8550 <- link(MPDSplit20, data=all8550cc20 )
simmeanMPD8550 <- data.frame(apply(simMPD8550,2,mean))
PIMPD8550 <- t(data.frame(apply(simMPD8550,2,PI,prob=0.89)))
F20MPDSim8550 <- data.frame(cbind(simmeanMPD8550,PIMPD8550))
F20MPDSim8550$PlotCN <- all8550cc20  $PlotCN
F20MPDSim8550 <- F20MPDSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8550) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2655 <- link(MPDSplit20, data=all2655cc20 )
simmeanMPD2655 <- data.frame(apply(simMPD2655,2,mean))
PIMPD2655 <- t(data.frame(apply(simMPD2655,2,PI,prob=0.89)))
F20MPDSim2655 <- data.frame(cbind(simmeanMPD2655,PIMPD2655))
F20MPDSim2655$PlotCN <- all2655cc20  $PlotCN
F20MPDSim2655 <- F20MPDSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2655) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4555 <- link(MPDSplit20, data=all4555cc20 )
simmeanMPD4555 <- data.frame(apply(simMPD4555,2,mean))
PIMPD4555 <- t(data.frame(apply(simMPD4555,2,PI,prob=0.89)))
F20MPDSim4555 <- data.frame(cbind(simmeanMPD4555,PIMPD4555))
F20MPDSim4555$PlotCN <- all4555cc20  $PlotCN
F20MPDSim4555 <- F20MPDSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4555) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8555 <- link(MPDSplit20, data=all8555cc20 )
simmeanMPD8555 <- data.frame(apply(simMPD8555,2,mean))
PIMPD8555 <- t(data.frame(apply(simMPD8555,2,PI,prob=0.89)))
F20MPDSim8555 <- data.frame(cbind(simmeanMPD8555,PIMPD8555))
F20MPDSim8555$PlotCN <- all8555cc20  $PlotCN
F20MPDSim8555 <- F20MPDSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8555) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2660 <- link(MPDSplit20, data=all2660cc20 )
simmeanMPD2660 <- data.frame(apply(simMPD2660,2,mean))
PIMPD2660 <- t(data.frame(apply(simMPD2660,2,PI,prob=0.89)))
F20MPDSim2660 <- data.frame(cbind(simmeanMPD2660,PIMPD2660))
F20MPDSim2660$PlotCN <- all2660cc20  $PlotCN
F20MPDSim2660 <- F20MPDSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2660) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4560 <- link(MPDSplit20, data=all4560cc20 )
simmeanMPD4560 <- data.frame(apply(simMPD4560,2,mean))
PIMPD4560 <- t(data.frame(apply(simMPD4560,2,PI,prob=0.89)))
F20MPDSim4560 <- data.frame(cbind(simmeanMPD4560,PIMPD4560))
F20MPDSim4560$PlotCN <- all4560cc20  $PlotCN
F20MPDSim4560 <- F20MPDSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4560) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8560 <- link(MPDSplit20, data=all8560cc20 )
simmeanMPD8560 <- data.frame(apply(simMPD8560,2,mean))
PIMPD8560 <- t(data.frame(apply(simMPD8560,2,PI,prob=0.89)))
F20MPDSim8560 <- data.frame(cbind(simmeanMPD8560,PIMPD8560))
F20MPDSim8560$PlotCN <- all8560cc20  $PlotCN
F20MPDSim8560 <- F20MPDSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8560) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2665 <- link(MPDSplit20, data=all2665cc20 )
simmeanMPD2665 <- data.frame(apply(simMPD2665,2,mean))
PIMPD2665 <- t(data.frame(apply(simMPD2665,2,PI,prob=0.89)))
F20MPDSim2665 <- data.frame(cbind(simmeanMPD2665,PIMPD2665))
F20MPDSim2665$PlotCN <- all2665cc20  $PlotCN
F20MPDSim2665 <- F20MPDSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2665) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4565 <- link(MPDSplit20, data=all4565cc20 )
simmeanMPD4565 <- data.frame(apply(simMPD4565,2,mean))
PIMPD4565 <- t(data.frame(apply(simMPD4565,2,PI,prob=0.89)))
F20MPDSim4565 <- data.frame(cbind(simmeanMPD4565,PIMPD4565))
F20MPDSim4565$PlotCN <- all4565cc20  $PlotCN
F20MPDSim4565 <- F20MPDSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4565) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8565 <- link(MPDSplit20, data=all8565cc20 )
simmeanMPD8565 <- data.frame(apply(simMPD8565,2,mean))
PIMPD8565 <- t(data.frame(apply(simMPD8565,2,PI,prob=0.89)))
F20MPDSim8565 <- data.frame(cbind(simmeanMPD8565,PIMPD8565))
F20MPDSim8565$PlotCN <- all8565cc20  $PlotCN
F20MPDSim8565 <- F20MPDSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8565) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2670 <- link(MPDSplit20, data=all2670cc20 )
simmeanMPD2670 <- data.frame(apply(simMPD2670,2,mean))
PIMPD2670 <- t(data.frame(apply(simMPD2670,2,PI,prob=0.89)))
F20MPDSim2670 <- data.frame(cbind(simmeanMPD2670,PIMPD2670))
F20MPDSim2670$PlotCN <- all2670cc20  $PlotCN
F20MPDSim2670 <- F20MPDSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2670) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4570 <- link(MPDSplit20, data=all4570cc20 )
simmeanMPD4570 <- data.frame(apply(simMPD4570,2,mean))
PIMPD4570 <- t(data.frame(apply(simMPD4570,2,PI,prob=0.89)))
F20MPDSim4570 <- data.frame(cbind(simmeanMPD4570,PIMPD4570))
F20MPDSim4570$PlotCN <- all4570cc20  $PlotCN
F20MPDSim4570 <- F20MPDSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4570) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8570 <- link(MPDSplit20, data=all8570cc20 )
simmeanMPD8570 <- data.frame(apply(simMPD8570,2,mean))
PIMPD8570 <- t(data.frame(apply(simMPD8570,2,PI,prob=0.89)))
F20MPDSim8570 <- data.frame(cbind(simmeanMPD8570,PIMPD8570))
F20MPDSim8570$PlotCN <- all8570cc20  $PlotCN
F20MPDSim8570 <- F20MPDSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8570) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2675 <- link(MPDSplit20, data=all2675cc20 )
simmeanMPD2675 <- data.frame(apply(simMPD2675,2,mean))
PIMPD2675 <- t(data.frame(apply(simMPD2675,2,PI,prob=0.89)))
F20MPDSim2675 <- data.frame(cbind(simmeanMPD2675,PIMPD2675))
F20MPDSim2675$PlotCN <- all2675cc20  $PlotCN
F20MPDSim2675 <- F20MPDSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2675) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4575 <- link(MPDSplit20, data=all4575cc20 )
simmeanMPD4575 <- data.frame(apply(simMPD4575,2,mean))
PIMPD4575 <- t(data.frame(apply(simMPD4575,2,PI,prob=0.89)))
F20MPDSim4575 <- data.frame(cbind(simmeanMPD4575,PIMPD4575))
F20MPDSim4575$PlotCN <- all4575cc20  $PlotCN
F20MPDSim4575 <- F20MPDSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4575) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8575 <- link(MPDSplit20, data=all8575cc20 )
simmeanMPD8575 <- data.frame(apply(simMPD8575,2,mean))
PIMPD8575 <- t(data.frame(apply(simMPD8575,2,PI,prob=0.89)))
F20MPDSim8575 <- data.frame(cbind(simmeanMPD8575,PIMPD8575))
F20MPDSim8575$PlotCN <- all8575cc20  $PlotCN
F20MPDSim8575 <- F20MPDSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8575) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2680 <- link(MPDSplit20, data=all2680cc20 )
simmeanMPD2680 <- data.frame(apply(simMPD2680,2,mean))
PIMPD2680 <- t(data.frame(apply(simMPD2680,2,PI,prob=0.89)))
F20MPDSim2680 <- data.frame(cbind(simmeanMPD2680,PIMPD2680))
F20MPDSim2680$PlotCN <- all2680cc20  $PlotCN
F20MPDSim2680 <- F20MPDSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim2680) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4580 <- link(MPDSplit20, data=all4580cc20 )
simmeanMPD4580 <- data.frame(apply(simMPD4580,2,mean))
PIMPD4580 <- t(data.frame(apply(simMPD4580,2,PI,prob=0.89)))
F20MPDSim4580 <- data.frame(cbind(simmeanMPD4580,PIMPD4580))
F20MPDSim4580$PlotCN <- all4580cc20  $PlotCN
F20MPDSim4580 <- F20MPDSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim4580) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8580 <- link(MPDSplit20, data=all8580cc20 )
simmeanMPD8580 <- data.frame(apply(simMPD8580,2,mean))
PIMPD8580 <- t(data.frame(apply(simMPD8580,2,PI,prob=0.89)))
F20MPDSim8580 <- data.frame(cbind(simmeanMPD8580,PIMPD8580))
F20MPDSim8580$PlotCN <- all8580cc20  $PlotCN
F20MPDSim8580 <- F20MPDSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F20MPDSim8580) <- c("PlotCN","MPDPerAcre","5CI","95CI")


#group data by RCP pathway
F20MPDSim26 <- F20MPDSim2625 %>% cbind(F20MPDSim2630[,2:4]) %>%
  cbind(F20MPDSim2635[,2:4]) %>% cbind(F20MPDSim2640[,2:4]) %>% cbind(F20MPDSim2645[,2:4]) %>%
  cbind(F20MPDSim2650[,2:4]) %>% cbind(F20MPDSim2655[,2:4]) %>% cbind(F20MPDSim2660[,2:4]) %>%
  cbind(F20MPDSim2665[,2:4]) %>% cbind(F20MPDSim2670[,2:4]) %>% cbind(F20MPDSim2675[,2:4]) %>%
  cbind(F20MPDSim2680[,2:4])
colnames(F20MPDSim26) <- MPDSimnames
write.csv(F20MPDSim26,file="F20MPDPred26.csv")

F20MPDSim45 <- F20MPDSim4525 %>% cbind(F20MPDSim4530[,2:4]) %>%
  cbind(F20MPDSim4535[,2:4]) %>% cbind(F20MPDSim4540[,2:4]) %>% cbind(F20MPDSim4545[,2:4]) %>%
  cbind(F20MPDSim4550[,2:4]) %>% cbind(F20MPDSim4555[,2:4]) %>% cbind(F20MPDSim4560[,2:4]) %>%
  cbind(F20MPDSim4565[,2:4]) %>% cbind(F20MPDSim4570[,2:4]) %>% cbind(F20MPDSim4575[,2:4]) %>%
  cbind(F20MPDSim4580[,2:4])
colnames(F20MPDSim45) <- MPDSimnames
write.csv(F20MPDSim45,file="F20MPDPred45.csv")

F20MPDSim85 <- F20MPDSim8525 %>% cbind(F20MPDSim8530[,2:4]) %>%
  cbind(F20MPDSim8535[,2:4]) %>% cbind(F20MPDSim8540[,2:4]) %>% cbind(F20MPDSim8545[,2:4]) %>%
  cbind(F20MPDSim8550[,2:4]) %>% cbind(F20MPDSim8555[,2:4]) %>% cbind(F20MPDSim8560[,2:4]) %>%
  cbind(F20MPDSim8565[,2:4]) %>% cbind(F20MPDSim8570[,2:4]) %>% cbind(F20MPDSim8575[,2:4]) %>%
  cbind(F20MPDSim8580[,2:4])
colnames(F20MPDSim85) <- MPDSimnames
write.csv(F20MPDSim85,file="F20MPDPred85.csv")


#fgroup 21
simMPD2625 <- link(MPDSplit21, data=all2625cc21 )
simmeanMPD2625 <- data.frame(apply(simMPD2625,2,mean))
PIMPD2625 <- t(data.frame(apply(simMPD2625,2,PI,prob=0.89)))
F21MPDSim2625 <- data.frame(cbind(simmeanMPD2625,PIMPD2625))
F21MPDSim2625$PlotCN <- all2625cc21  $PlotCN
F21MPDSim2625 <- F21MPDSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2625) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4525 <- link(MPDSplit21, data=all4525cc21 )
simmeanMPD4525 <- data.frame(apply(simMPD4525,2,mean))
PIMPD4525 <- t(data.frame(apply(simMPD4525,2,PI,prob=0.89)))
F21MPDSim4525 <- data.frame(cbind(simmeanMPD4525,PIMPD4525))
F21MPDSim4525$PlotCN <- all4525cc21  $PlotCN
F21MPDSim4525 <- F21MPDSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4525) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8525 <- link(MPDSplit21, data=all8525cc21 )
simmeanMPD8525 <- data.frame(apply(simMPD8525,2,mean))
PIMPD8525 <- t(data.frame(apply(simMPD8525,2,PI,prob=0.89)))
F21MPDSim8525 <- data.frame(cbind(simmeanMPD8525,PIMPD8525))
F21MPDSim8525$PlotCN <- all8525cc21  $PlotCN
F21MPDSim8525 <- F21MPDSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8525) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2630 <- link(MPDSplit21, data=all2630cc21 )
simmeanMPD2630 <- data.frame(apply(simMPD2630,2,mean))
PIMPD2630 <- t(data.frame(apply(simMPD2630,2,PI,prob=0.89)))
F21MPDSim2630 <- data.frame(cbind(simmeanMPD2630,PIMPD2630))
F21MPDSim2630$PlotCN <- all2630cc21  $PlotCN
F21MPDSim2630 <- F21MPDSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2630) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4530 <- link(MPDSplit21, data=all4530cc21 )
simmeanMPD4530 <- data.frame(apply(simMPD4530,2,mean))
PIMPD4530 <- t(data.frame(apply(simMPD4530,2,PI,prob=0.89)))
F21MPDSim4530 <- data.frame(cbind(simmeanMPD4530,PIMPD4530))
F21MPDSim4530$PlotCN <- all4530cc21  $PlotCN
F21MPDSim4530 <- F21MPDSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4530) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8530 <- link(MPDSplit21, data=all8530cc21 )
simmeanMPD8530 <- data.frame(apply(simMPD8530,2,mean))
PIMPD8530 <- t(data.frame(apply(simMPD8530,2,PI,prob=0.89)))
F21MPDSim8530 <- data.frame(cbind(simmeanMPD8530,PIMPD8530))
F21MPDSim8530$PlotCN <- all8530cc21  $PlotCN
F21MPDSim8530 <- F21MPDSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8530) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2635 <- link(MPDSplit21, data=all2635cc21 )
simmeanMPD2635 <- data.frame(apply(simMPD2635,2,mean))
PIMPD2635 <- t(data.frame(apply(simMPD2635,2,PI,prob=0.89)))
F21MPDSim2635 <- data.frame(cbind(simmeanMPD2635,PIMPD2635))
F21MPDSim2635$PlotCN <- all2635cc21  $PlotCN
F21MPDSim2635 <- F21MPDSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2635) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4535 <- link(MPDSplit21, data=all4535cc21 )
simmeanMPD4535 <- data.frame(apply(simMPD4535,2,mean))
PIMPD4535 <- t(data.frame(apply(simMPD4535,2,PI,prob=0.89)))
F21MPDSim4535 <- data.frame(cbind(simmeanMPD4535,PIMPD4535))
F21MPDSim4535$PlotCN <- all4535cc21  $PlotCN
F21MPDSim4535 <- F21MPDSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4535) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8535 <- link(MPDSplit21, data=all8535cc21 )
simmeanMPD8535 <- data.frame(apply(simMPD8535,2,mean))
PIMPD8535 <- t(data.frame(apply(simMPD8535,2,PI,prob=0.89)))
F21MPDSim8535 <- data.frame(cbind(simmeanMPD8535,PIMPD8535))
F21MPDSim8535$PlotCN <- all8535cc21  $PlotCN
F21MPDSim8535 <- F21MPDSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8535) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2640 <- link(MPDSplit21, data=all2640cc21 )
simmeanMPD2640 <- data.frame(apply(simMPD2640,2,mean))
PIMPD2640 <- t(data.frame(apply(simMPD2640,2,PI,prob=0.89)))
F21MPDSim2640 <- data.frame(cbind(simmeanMPD2640,PIMPD2640))
F21MPDSim2640$PlotCN <- all2640cc21  $PlotCN
F21MPDSim2640 <- F21MPDSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2640) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4540 <- link(MPDSplit21, data=all4540cc21 )
simmeanMPD4540 <- data.frame(apply(simMPD4540,2,mean))
PIMPD4540 <- t(data.frame(apply(simMPD4540,2,PI,prob=0.89)))
F21MPDSim4540 <- data.frame(cbind(simmeanMPD4540,PIMPD4540))
F21MPDSim4540$PlotCN <- all4540cc21  $PlotCN
F21MPDSim4540 <- F21MPDSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4540) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8540 <- link(MPDSplit21, data=all8540cc21 )
simmeanMPD8540 <- data.frame(apply(simMPD8540,2,mean))
PIMPD8540 <- t(data.frame(apply(simMPD8540,2,PI,prob=0.89)))
F21MPDSim8540 <- data.frame(cbind(simmeanMPD8540,PIMPD8540))
F21MPDSim8540$PlotCN <- all8540cc21  $PlotCN
F21MPDSim8540 <- F21MPDSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8540) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2645 <- link(MPDSplit21, data=all2645cc21 )
simmeanMPD2645 <- data.frame(apply(simMPD2645,2,mean))
PIMPD2645 <- t(data.frame(apply(simMPD2645,2,PI,prob=0.89)))
F21MPDSim2645 <- data.frame(cbind(simmeanMPD2645,PIMPD2645))
F21MPDSim2645$PlotCN <- all2645cc21  $PlotCN
F21MPDSim2645 <- F21MPDSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2645) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4545 <- link(MPDSplit21, data=all4545cc21 )
simmeanMPD4545 <- data.frame(apply(simMPD4545,2,mean))
PIMPD4545 <- t(data.frame(apply(simMPD4545,2,PI,prob=0.89)))
F21MPDSim4545 <- data.frame(cbind(simmeanMPD4545,PIMPD4545))
F21MPDSim4545$PlotCN <- all4545cc21  $PlotCN
F21MPDSim4545 <- F21MPDSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4545) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8545 <- link(MPDSplit21, data=all8545cc21 )
simmeanMPD8545 <- data.frame(apply(simMPD8545,2,mean))
PIMPD8545 <- t(data.frame(apply(simMPD8545,2,PI,prob=0.89)))
F21MPDSim8545 <- data.frame(cbind(simmeanMPD8545,PIMPD8545))
F21MPDSim8545$PlotCN <- all8545cc21  $PlotCN
F21MPDSim8545 <- F21MPDSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8545) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2650 <- link(MPDSplit21, data=all2650cc21 )
simmeanMPD2650 <- data.frame(apply(simMPD2650,2,mean))
PIMPD2650 <- t(data.frame(apply(simMPD2650,2,PI,prob=0.89)))
F21MPDSim2650 <- data.frame(cbind(simmeanMPD2650,PIMPD2650))
F21MPDSim2650$PlotCN <- all2650cc21  $PlotCN
F21MPDSim2650 <- F21MPDSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2650) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4550 <- link(MPDSplit21, data=all4550cc21 )
simmeanMPD4550 <- data.frame(apply(simMPD4550,2,mean))
PIMPD4550 <- t(data.frame(apply(simMPD4550,2,PI,prob=0.89)))
F21MPDSim4550 <- data.frame(cbind(simmeanMPD4550,PIMPD4550))
F21MPDSim4550$PlotCN <- all4550cc21  $PlotCN
F21MPDSim4550 <- F21MPDSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4550) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8550 <- link(MPDSplit21, data=all8550cc21 )
simmeanMPD8550 <- data.frame(apply(simMPD8550,2,mean))
PIMPD8550 <- t(data.frame(apply(simMPD8550,2,PI,prob=0.89)))
F21MPDSim8550 <- data.frame(cbind(simmeanMPD8550,PIMPD8550))
F21MPDSim8550$PlotCN <- all8550cc21  $PlotCN
F21MPDSim8550 <- F21MPDSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8550) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2655 <- link(MPDSplit21, data=all2655cc21 )
simmeanMPD2655 <- data.frame(apply(simMPD2655,2,mean))
PIMPD2655 <- t(data.frame(apply(simMPD2655,2,PI,prob=0.89)))
F21MPDSim2655 <- data.frame(cbind(simmeanMPD2655,PIMPD2655))
F21MPDSim2655$PlotCN <- all2655cc21  $PlotCN
F21MPDSim2655 <- F21MPDSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2655) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4555 <- link(MPDSplit21, data=all4555cc21 )
simmeanMPD4555 <- data.frame(apply(simMPD4555,2,mean))
PIMPD4555 <- t(data.frame(apply(simMPD4555,2,PI,prob=0.89)))
F21MPDSim4555 <- data.frame(cbind(simmeanMPD4555,PIMPD4555))
F21MPDSim4555$PlotCN <- all4555cc21  $PlotCN
F21MPDSim4555 <- F21MPDSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4555) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8555 <- link(MPDSplit21, data=all8555cc21 )
simmeanMPD8555 <- data.frame(apply(simMPD8555,2,mean))
PIMPD8555 <- t(data.frame(apply(simMPD8555,2,PI,prob=0.89)))
F21MPDSim8555 <- data.frame(cbind(simmeanMPD8555,PIMPD8555))
F21MPDSim8555$PlotCN <- all8555cc21  $PlotCN
F21MPDSim8555 <- F21MPDSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8555) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2660 <- link(MPDSplit21, data=all2660cc21 )
simmeanMPD2660 <- data.frame(apply(simMPD2660,2,mean))
PIMPD2660 <- t(data.frame(apply(simMPD2660,2,PI,prob=0.89)))
F21MPDSim2660 <- data.frame(cbind(simmeanMPD2660,PIMPD2660))
F21MPDSim2660$PlotCN <- all2660cc21  $PlotCN
F21MPDSim2660 <- F21MPDSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2660) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4560 <- link(MPDSplit21, data=all4560cc21 )
simmeanMPD4560 <- data.frame(apply(simMPD4560,2,mean))
PIMPD4560 <- t(data.frame(apply(simMPD4560,2,PI,prob=0.89)))
F21MPDSim4560 <- data.frame(cbind(simmeanMPD4560,PIMPD4560))
F21MPDSim4560$PlotCN <- all4560cc21  $PlotCN
F21MPDSim4560 <- F21MPDSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4560) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8560 <- link(MPDSplit21, data=all8560cc21 )
simmeanMPD8560 <- data.frame(apply(simMPD8560,2,mean))
PIMPD8560 <- t(data.frame(apply(simMPD8560,2,PI,prob=0.89)))
F21MPDSim8560 <- data.frame(cbind(simmeanMPD8560,PIMPD8560))
F21MPDSim8560$PlotCN <- all8560cc21  $PlotCN
F21MPDSim8560 <- F21MPDSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8560) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2665 <- link(MPDSplit21, data=all2665cc21 )
simmeanMPD2665 <- data.frame(apply(simMPD2665,2,mean))
PIMPD2665 <- t(data.frame(apply(simMPD2665,2,PI,prob=0.89)))
F21MPDSim2665 <- data.frame(cbind(simmeanMPD2665,PIMPD2665))
F21MPDSim2665$PlotCN <- all2665cc21  $PlotCN
F21MPDSim2665 <- F21MPDSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2665) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4565 <- link(MPDSplit21, data=all4565cc21 )
simmeanMPD4565 <- data.frame(apply(simMPD4565,2,mean))
PIMPD4565 <- t(data.frame(apply(simMPD4565,2,PI,prob=0.89)))
F21MPDSim4565 <- data.frame(cbind(simmeanMPD4565,PIMPD4565))
F21MPDSim4565$PlotCN <- all4565cc21  $PlotCN
F21MPDSim4565 <- F21MPDSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4565) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8565 <- link(MPDSplit21, data=all8565cc21 )
simmeanMPD8565 <- data.frame(apply(simMPD8565,2,mean))
PIMPD8565 <- t(data.frame(apply(simMPD8565,2,PI,prob=0.89)))
F21MPDSim8565 <- data.frame(cbind(simmeanMPD8565,PIMPD8565))
F21MPDSim8565$PlotCN <- all8565cc21  $PlotCN
F21MPDSim8565 <- F21MPDSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8565) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2670 <- link(MPDSplit21, data=all2670cc21 )
simmeanMPD2670 <- data.frame(apply(simMPD2670,2,mean))
PIMPD2670 <- t(data.frame(apply(simMPD2670,2,PI,prob=0.89)))
F21MPDSim2670 <- data.frame(cbind(simmeanMPD2670,PIMPD2670))
F21MPDSim2670$PlotCN <- all2670cc21  $PlotCN
F21MPDSim2670 <- F21MPDSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2670) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4570 <- link(MPDSplit21, data=all4570cc21 )
simmeanMPD4570 <- data.frame(apply(simMPD4570,2,mean))
PIMPD4570 <- t(data.frame(apply(simMPD4570,2,PI,prob=0.89)))
F21MPDSim4570 <- data.frame(cbind(simmeanMPD4570,PIMPD4570))
F21MPDSim4570$PlotCN <- all4570cc21  $PlotCN
F21MPDSim4570 <- F21MPDSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4570) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8570 <- link(MPDSplit21, data=all8570cc21 )
simmeanMPD8570 <- data.frame(apply(simMPD8570,2,mean))
PIMPD8570 <- t(data.frame(apply(simMPD8570,2,PI,prob=0.89)))
F21MPDSim8570 <- data.frame(cbind(simmeanMPD8570,PIMPD8570))
F21MPDSim8570$PlotCN <- all8570cc21  $PlotCN
F21MPDSim8570 <- F21MPDSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8570) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2675 <- link(MPDSplit21, data=all2675cc21 )
simmeanMPD2675 <- data.frame(apply(simMPD2675,2,mean))
PIMPD2675 <- t(data.frame(apply(simMPD2675,2,PI,prob=0.89)))
F21MPDSim2675 <- data.frame(cbind(simmeanMPD2675,PIMPD2675))
F21MPDSim2675$PlotCN <- all2675cc21  $PlotCN
F21MPDSim2675 <- F21MPDSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2675) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4575 <- link(MPDSplit21, data=all4575cc21 )
simmeanMPD4575 <- data.frame(apply(simMPD4575,2,mean))
PIMPD4575 <- t(data.frame(apply(simMPD4575,2,PI,prob=0.89)))
F21MPDSim4575 <- data.frame(cbind(simmeanMPD4575,PIMPD4575))
F21MPDSim4575$PlotCN <- all4575cc21  $PlotCN
F21MPDSim4575 <- F21MPDSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4575) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8575 <- link(MPDSplit21, data=all8575cc21 )
simmeanMPD8575 <- data.frame(apply(simMPD8575,2,mean))
PIMPD8575 <- t(data.frame(apply(simMPD8575,2,PI,prob=0.89)))
F21MPDSim8575 <- data.frame(cbind(simmeanMPD8575,PIMPD8575))
F21MPDSim8575$PlotCN <- all8575cc21  $PlotCN
F21MPDSim8575 <- F21MPDSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8575) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2680 <- link(MPDSplit21, data=all2680cc21 )
simmeanMPD2680 <- data.frame(apply(simMPD2680,2,mean))
PIMPD2680 <- t(data.frame(apply(simMPD2680,2,PI,prob=0.89)))
F21MPDSim2680 <- data.frame(cbind(simmeanMPD2680,PIMPD2680))
F21MPDSim2680$PlotCN <- all2680cc21  $PlotCN
F21MPDSim2680 <- F21MPDSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim2680) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4580 <- link(MPDSplit21, data=all4580cc21 )
simmeanMPD4580 <- data.frame(apply(simMPD4580,2,mean))
PIMPD4580 <- t(data.frame(apply(simMPD4580,2,PI,prob=0.89)))
F21MPDSim4580 <- data.frame(cbind(simmeanMPD4580,PIMPD4580))
F21MPDSim4580$PlotCN <- all4580cc21  $PlotCN
F21MPDSim4580 <- F21MPDSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim4580) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8580 <- link(MPDSplit21, data=all8580cc21 )
simmeanMPD8580 <- data.frame(apply(simMPD8580,2,mean))
PIMPD8580 <- t(data.frame(apply(simMPD8580,2,PI,prob=0.89)))
F21MPDSim8580 <- data.frame(cbind(simmeanMPD8580,PIMPD8580))
F21MPDSim8580$PlotCN <- all8580cc21  $PlotCN
F21MPDSim8580 <- F21MPDSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F21MPDSim8580) <- c("PlotCN","MPDPerAcre","5CI","95CI")


#create vector of new colnames
MPDSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("MPD", "5CI", "95CI"), x)))
MPDSimnames<- append(MPDSimnames,"PlotCN",after=0)

#group data by RCP pathway
F21MPDSim26 <- F21MPDSim2625 %>% cbind(F21MPDSim2630[,2:4]) %>%
  cbind(F21MPDSim2635[,2:4]) %>% cbind(F21MPDSim2640[,2:4]) %>% cbind(F21MPDSim2645[,2:4]) %>%
  cbind(F21MPDSim2650[,2:4]) %>% cbind(F21MPDSim2655[,2:4]) %>% cbind(F21MPDSim2660[,2:4]) %>%
  cbind(F21MPDSim2665[,2:4]) %>% cbind(F21MPDSim2670[,2:4]) %>% cbind(F21MPDSim2675[,2:4]) %>%
  cbind(F21MPDSim2680[,2:4])
colnames(F21MPDSim26) <- MPDSimnames
write.csv(F21MPDSim26,file="F21MPDPred26.csv")

F21MPDSim45 <- F21MPDSim4525 %>% cbind(F21MPDSim4530[,2:4]) %>%
  cbind(F21MPDSim4535[,2:4]) %>% cbind(F21MPDSim4540[,2:4]) %>% cbind(F21MPDSim4545[,2:4]) %>%
  cbind(F21MPDSim4550[,2:4]) %>% cbind(F21MPDSim4555[,2:4]) %>% cbind(F21MPDSim4560[,2:4]) %>%
  cbind(F21MPDSim4565[,2:4]) %>% cbind(F21MPDSim4570[,2:4]) %>% cbind(F21MPDSim4575[,2:4]) %>%
  cbind(F21MPDSim4580[,2:4])
colnames(F21MPDSim45) <- MPDSimnames
write.csv(F21MPDSim45,file="F21MPDPred45.csv")

F21MPDSim85 <- F21MPDSim8525 %>% cbind(F21MPDSim8530[,2:4]) %>%
  cbind(F21MPDSim8535[,2:4]) %>% cbind(F21MPDSim8540[,2:4]) %>% cbind(F21MPDSim8545[,2:4]) %>%
  cbind(F21MPDSim8550[,2:4]) %>% cbind(F21MPDSim8555[,2:4]) %>% cbind(F21MPDSim8560[,2:4]) %>%
  cbind(F21MPDSim8565[,2:4]) %>% cbind(F21MPDSim8570[,2:4]) %>% cbind(F21MPDSim8575[,2:4]) %>%
  cbind(F21MPDSim8580[,2:4])
colnames(F21MPDSim85) <- MPDSimnames
write.csv(F21MPDSim85,file="F21MPDPred85.csv")


#fgroup 23
simMPD2625 <- link(MPDSplit23, data=all2625cc23 )
simmeanMPD2625 <- data.frame(apply(simMPD2625,2,mean))
PIMPD2625 <- t(data.frame(apply(simMPD2625,2,PI,prob=0.89)))
F23MPDSim2625 <- data.frame(cbind(simmeanMPD2625,PIMPD2625))
F23MPDSim2625$PlotCN <- all2625cc23  $PlotCN
F23MPDSim2625 <- F23MPDSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2625) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4525 <- link(MPDSplit23, data=all4525cc23 )
simmeanMPD4525 <- data.frame(apply(simMPD4525,2,mean))
PIMPD4525 <- t(data.frame(apply(simMPD4525,2,PI,prob=0.89)))
F23MPDSim4525 <- data.frame(cbind(simmeanMPD4525,PIMPD4525))
F23MPDSim4525$PlotCN <- all4525cc23  $PlotCN
F23MPDSim4525 <- F23MPDSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4525) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8525 <- link(MPDSplit23, data=all8525cc23 )
simmeanMPD8525 <- data.frame(apply(simMPD8525,2,mean))
PIMPD8525 <- t(data.frame(apply(simMPD8525,2,PI,prob=0.89)))
F23MPDSim8525 <- data.frame(cbind(simmeanMPD8525,PIMPD8525))
F23MPDSim8525$PlotCN <- all8525cc23  $PlotCN
F23MPDSim8525 <- F23MPDSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8525) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2630 <- link(MPDSplit23, data=all2630cc23 )
simmeanMPD2630 <- data.frame(apply(simMPD2630,2,mean))
PIMPD2630 <- t(data.frame(apply(simMPD2630,2,PI,prob=0.89)))
F23MPDSim2630 <- data.frame(cbind(simmeanMPD2630,PIMPD2630))
F23MPDSim2630$PlotCN <- all2630cc23  $PlotCN
F23MPDSim2630 <- F23MPDSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2630) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4530 <- link(MPDSplit23, data=all4530cc23 )
simmeanMPD4530 <- data.frame(apply(simMPD4530,2,mean))
PIMPD4530 <- t(data.frame(apply(simMPD4530,2,PI,prob=0.89)))
F23MPDSim4530 <- data.frame(cbind(simmeanMPD4530,PIMPD4530))
F23MPDSim4530$PlotCN <- all4530cc23  $PlotCN
F23MPDSim4530 <- F23MPDSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4530) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8530 <- link(MPDSplit23, data=all8530cc23 )
simmeanMPD8530 <- data.frame(apply(simMPD8530,2,mean))
PIMPD8530 <- t(data.frame(apply(simMPD8530,2,PI,prob=0.89)))
F23MPDSim8530 <- data.frame(cbind(simmeanMPD8530,PIMPD8530))
F23MPDSim8530$PlotCN <- all8530cc23  $PlotCN
F23MPDSim8530 <- F23MPDSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8530) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2635 <- link(MPDSplit23, data=all2635cc23 )
simmeanMPD2635 <- data.frame(apply(simMPD2635,2,mean))
PIMPD2635 <- t(data.frame(apply(simMPD2635,2,PI,prob=0.89)))
F23MPDSim2635 <- data.frame(cbind(simmeanMPD2635,PIMPD2635))
F23MPDSim2635$PlotCN <- all2635cc23  $PlotCN
F23MPDSim2635 <- F23MPDSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2635) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4535 <- link(MPDSplit23, data=all4535cc23 )
simmeanMPD4535 <- data.frame(apply(simMPD4535,2,mean))
PIMPD4535 <- t(data.frame(apply(simMPD4535,2,PI,prob=0.89)))
F23MPDSim4535 <- data.frame(cbind(simmeanMPD4535,PIMPD4535))
F23MPDSim4535$PlotCN <- all4535cc23  $PlotCN
F23MPDSim4535 <- F23MPDSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4535) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8535 <- link(MPDSplit23, data=all8535cc23 )
simmeanMPD8535 <- data.frame(apply(simMPD8535,2,mean))
PIMPD8535 <- t(data.frame(apply(simMPD8535,2,PI,prob=0.89)))
F23MPDSim8535 <- data.frame(cbind(simmeanMPD8535,PIMPD8535))
F23MPDSim8535$PlotCN <- all8535cc23  $PlotCN
F23MPDSim8535 <- F23MPDSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8535) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2640 <- link(MPDSplit23, data=all2640cc23 )
simmeanMPD2640 <- data.frame(apply(simMPD2640,2,mean))
PIMPD2640 <- t(data.frame(apply(simMPD2640,2,PI,prob=0.89)))
F23MPDSim2640 <- data.frame(cbind(simmeanMPD2640,PIMPD2640))
F23MPDSim2640$PlotCN <- all2640cc23  $PlotCN
F23MPDSim2640 <- F23MPDSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2640) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4540 <- link(MPDSplit23, data=all4540cc23 )
simmeanMPD4540 <- data.frame(apply(simMPD4540,2,mean))
PIMPD4540 <- t(data.frame(apply(simMPD4540,2,PI,prob=0.89)))
F23MPDSim4540 <- data.frame(cbind(simmeanMPD4540,PIMPD4540))
F23MPDSim4540$PlotCN <- all4540cc23  $PlotCN
F23MPDSim4540 <- F23MPDSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4540) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8540 <- link(MPDSplit23, data=all8540cc23 )
simmeanMPD8540 <- data.frame(apply(simMPD8540,2,mean))
PIMPD8540 <- t(data.frame(apply(simMPD8540,2,PI,prob=0.89)))
F23MPDSim8540 <- data.frame(cbind(simmeanMPD8540,PIMPD8540))
F23MPDSim8540$PlotCN <- all8540cc23  $PlotCN
F23MPDSim8540 <- F23MPDSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8540) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2645 <- link(MPDSplit23, data=all2645cc23 )
simmeanMPD2645 <- data.frame(apply(simMPD2645,2,mean))
PIMPD2645 <- t(data.frame(apply(simMPD2645,2,PI,prob=0.89)))
F23MPDSim2645 <- data.frame(cbind(simmeanMPD2645,PIMPD2645))
F23MPDSim2645$PlotCN <- all2645cc23  $PlotCN
F23MPDSim2645 <- F23MPDSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2645) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4545 <- link(MPDSplit23, data=all4545cc23 )
simmeanMPD4545 <- data.frame(apply(simMPD4545,2,mean))
PIMPD4545 <- t(data.frame(apply(simMPD4545,2,PI,prob=0.89)))
F23MPDSim4545 <- data.frame(cbind(simmeanMPD4545,PIMPD4545))
F23MPDSim4545$PlotCN <- all4545cc23  $PlotCN
F23MPDSim4545 <- F23MPDSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4545) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8545 <- link(MPDSplit23, data=all8545cc23 )
simmeanMPD8545 <- data.frame(apply(simMPD8545,2,mean))
PIMPD8545 <- t(data.frame(apply(simMPD8545,2,PI,prob=0.89)))
F23MPDSim8545 <- data.frame(cbind(simmeanMPD8545,PIMPD8545))
F23MPDSim8545$PlotCN <- all8545cc23  $PlotCN
F23MPDSim8545 <- F23MPDSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8545) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2650 <- link(MPDSplit23, data=all2650cc23 )
simmeanMPD2650 <- data.frame(apply(simMPD2650,2,mean))
PIMPD2650 <- t(data.frame(apply(simMPD2650,2,PI,prob=0.89)))
F23MPDSim2650 <- data.frame(cbind(simmeanMPD2650,PIMPD2650))
F23MPDSim2650$PlotCN <- all2650cc23  $PlotCN
F23MPDSim2650 <- F23MPDSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2650) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4550 <- link(MPDSplit23, data=all4550cc23 )
simmeanMPD4550 <- data.frame(apply(simMPD4550,2,mean))
PIMPD4550 <- t(data.frame(apply(simMPD4550,2,PI,prob=0.89)))
F23MPDSim4550 <- data.frame(cbind(simmeanMPD4550,PIMPD4550))
F23MPDSim4550$PlotCN <- all4550cc23  $PlotCN
F23MPDSim4550 <- F23MPDSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4550) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8550 <- link(MPDSplit23, data=all8550cc23 )
simmeanMPD8550 <- data.frame(apply(simMPD8550,2,mean))
PIMPD8550 <- t(data.frame(apply(simMPD8550,2,PI,prob=0.89)))
F23MPDSim8550 <- data.frame(cbind(simmeanMPD8550,PIMPD8550))
F23MPDSim8550$PlotCN <- all8550cc23  $PlotCN
F23MPDSim8550 <- F23MPDSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8550) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2655 <- link(MPDSplit23, data=all2655cc23 )
simmeanMPD2655 <- data.frame(apply(simMPD2655,2,mean))
PIMPD2655 <- t(data.frame(apply(simMPD2655,2,PI,prob=0.89)))
F23MPDSim2655 <- data.frame(cbind(simmeanMPD2655,PIMPD2655))
F23MPDSim2655$PlotCN <- all2655cc23  $PlotCN
F23MPDSim2655 <- F23MPDSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2655) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4555 <- link(MPDSplit23, data=all4555cc23 )
simmeanMPD4555 <- data.frame(apply(simMPD4555,2,mean))
PIMPD4555 <- t(data.frame(apply(simMPD4555,2,PI,prob=0.89)))
F23MPDSim4555 <- data.frame(cbind(simmeanMPD4555,PIMPD4555))
F23MPDSim4555$PlotCN <- all4555cc23  $PlotCN
F23MPDSim4555 <- F23MPDSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4555) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8555 <- link(MPDSplit23, data=all8555cc23 )
simmeanMPD8555 <- data.frame(apply(simMPD8555,2,mean))
PIMPD8555 <- t(data.frame(apply(simMPD8555,2,PI,prob=0.89)))
F23MPDSim8555 <- data.frame(cbind(simmeanMPD8555,PIMPD8555))
F23MPDSim8555$PlotCN <- all8555cc23  $PlotCN
F23MPDSim8555 <- F23MPDSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8555) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2660 <- link(MPDSplit23, data=all2660cc23 )
simmeanMPD2660 <- data.frame(apply(simMPD2660,2,mean))
PIMPD2660 <- t(data.frame(apply(simMPD2660,2,PI,prob=0.89)))
F23MPDSim2660 <- data.frame(cbind(simmeanMPD2660,PIMPD2660))
F23MPDSim2660$PlotCN <- all2660cc23  $PlotCN
F23MPDSim2660 <- F23MPDSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2660) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4560 <- link(MPDSplit23, data=all4560cc23 )
simmeanMPD4560 <- data.frame(apply(simMPD4560,2,mean))
PIMPD4560 <- t(data.frame(apply(simMPD4560,2,PI,prob=0.89)))
F23MPDSim4560 <- data.frame(cbind(simmeanMPD4560,PIMPD4560))
F23MPDSim4560$PlotCN <- all4560cc23  $PlotCN
F23MPDSim4560 <- F23MPDSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4560) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8560 <- link(MPDSplit23, data=all8560cc23 )
simmeanMPD8560 <- data.frame(apply(simMPD8560,2,mean))
PIMPD8560 <- t(data.frame(apply(simMPD8560,2,PI,prob=0.89)))
F23MPDSim8560 <- data.frame(cbind(simmeanMPD8560,PIMPD8560))
F23MPDSim8560$PlotCN <- all8560cc23  $PlotCN
F23MPDSim8560 <- F23MPDSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8560) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2665 <- link(MPDSplit23, data=all2665cc23 )
simmeanMPD2665 <- data.frame(apply(simMPD2665,2,mean))
PIMPD2665 <- t(data.frame(apply(simMPD2665,2,PI,prob=0.89)))
F23MPDSim2665 <- data.frame(cbind(simmeanMPD2665,PIMPD2665))
F23MPDSim2665$PlotCN <- all2665cc23  $PlotCN
F23MPDSim2665 <- F23MPDSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2665) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4565 <- link(MPDSplit23, data=all4565cc23 )
simmeanMPD4565 <- data.frame(apply(simMPD4565,2,mean))
PIMPD4565 <- t(data.frame(apply(simMPD4565,2,PI,prob=0.89)))
F23MPDSim4565 <- data.frame(cbind(simmeanMPD4565,PIMPD4565))
F23MPDSim4565$PlotCN <- all4565cc23  $PlotCN
F23MPDSim4565 <- F23MPDSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4565) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8565 <- link(MPDSplit23, data=all8565cc23 )
simmeanMPD8565 <- data.frame(apply(simMPD8565,2,mean))
PIMPD8565 <- t(data.frame(apply(simMPD8565,2,PI,prob=0.89)))
F23MPDSim8565 <- data.frame(cbind(simmeanMPD8565,PIMPD8565))
F23MPDSim8565$PlotCN <- all8565cc23  $PlotCN
F23MPDSim8565 <- F23MPDSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8565) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2670 <- link(MPDSplit23, data=all2670cc23 )
simmeanMPD2670 <- data.frame(apply(simMPD2670,2,mean))
PIMPD2670 <- t(data.frame(apply(simMPD2670,2,PI,prob=0.89)))
F23MPDSim2670 <- data.frame(cbind(simmeanMPD2670,PIMPD2670))
F23MPDSim2670$PlotCN <- all2670cc23  $PlotCN
F23MPDSim2670 <- F23MPDSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2670) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4570 <- link(MPDSplit23, data=all4570cc23 )
simmeanMPD4570 <- data.frame(apply(simMPD4570,2,mean))
PIMPD4570 <- t(data.frame(apply(simMPD4570,2,PI,prob=0.89)))
F23MPDSim4570 <- data.frame(cbind(simmeanMPD4570,PIMPD4570))
F23MPDSim4570$PlotCN <- all4570cc23  $PlotCN
F23MPDSim4570 <- F23MPDSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4570) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8570 <- link(MPDSplit23, data=all8570cc23 )
simmeanMPD8570 <- data.frame(apply(simMPD8570,2,mean))
PIMPD8570 <- t(data.frame(apply(simMPD8570,2,PI,prob=0.89)))
F23MPDSim8570 <- data.frame(cbind(simmeanMPD8570,PIMPD8570))
F23MPDSim8570$PlotCN <- all8570cc23  $PlotCN
F23MPDSim8570 <- F23MPDSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8570) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2675 <- link(MPDSplit23, data=all2675cc23 )
simmeanMPD2675 <- data.frame(apply(simMPD2675,2,mean))
PIMPD2675 <- t(data.frame(apply(simMPD2675,2,PI,prob=0.89)))
F23MPDSim2675 <- data.frame(cbind(simmeanMPD2675,PIMPD2675))
F23MPDSim2675$PlotCN <- all2675cc23  $PlotCN
F23MPDSim2675 <- F23MPDSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2675) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4575 <- link(MPDSplit23, data=all4575cc23 )
simmeanMPD4575 <- data.frame(apply(simMPD4575,2,mean))
PIMPD4575 <- t(data.frame(apply(simMPD4575,2,PI,prob=0.89)))
F23MPDSim4575 <- data.frame(cbind(simmeanMPD4575,PIMPD4575))
F23MPDSim4575$PlotCN <- all4575cc23  $PlotCN
F23MPDSim4575 <- F23MPDSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4575) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8575 <- link(MPDSplit23, data=all8575cc23 )
simmeanMPD8575 <- data.frame(apply(simMPD8575,2,mean))
PIMPD8575 <- t(data.frame(apply(simMPD8575,2,PI,prob=0.89)))
F23MPDSim8575 <- data.frame(cbind(simmeanMPD8575,PIMPD8575))
F23MPDSim8575$PlotCN <- all8575cc23  $PlotCN
F23MPDSim8575 <- F23MPDSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8575) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2680 <- link(MPDSplit23, data=all2680cc23 )
simmeanMPD2680 <- data.frame(apply(simMPD2680,2,mean))
PIMPD2680 <- t(data.frame(apply(simMPD2680,2,PI,prob=0.89)))
F23MPDSim2680 <- data.frame(cbind(simmeanMPD2680,PIMPD2680))
F23MPDSim2680$PlotCN <- all2680cc23  $PlotCN
F23MPDSim2680 <- F23MPDSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim2680) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4580 <- link(MPDSplit23, data=all4580cc23 )
simmeanMPD4580 <- data.frame(apply(simMPD4580,2,mean))
PIMPD4580 <- t(data.frame(apply(simMPD4580,2,PI,prob=0.89)))
F23MPDSim4580 <- data.frame(cbind(simmeanMPD4580,PIMPD4580))
F23MPDSim4580$PlotCN <- all4580cc23  $PlotCN
F23MPDSim4580 <- F23MPDSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim4580) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8580 <- link(MPDSplit23, data=all8580cc23 )
simmeanMPD8580 <- data.frame(apply(simMPD8580,2,mean))
PIMPD8580 <- t(data.frame(apply(simMPD8580,2,PI,prob=0.89)))
F23MPDSim8580 <- data.frame(cbind(simmeanMPD8580,PIMPD8580))
F23MPDSim8580$PlotCN <- all8580cc23  $PlotCN
F23MPDSim8580 <- F23MPDSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F23MPDSim8580) <- c("PlotCN","MPDPerAcre","5CI","95CI")

#create vector of new colnames
MPDSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("MPD", "5CI", "95CI"), x)))
MPDSimnames<- append(MPDSimnames,"PlotCN",after=0)

#group data by RCP pathway
F23MPDSim26 <- F23MPDSim2625 %>% cbind(F23MPDSim2630[,2:4]) %>%
  cbind(F23MPDSim2635[,2:4]) %>% cbind(F23MPDSim2640[,2:4]) %>% cbind(F23MPDSim2645[,2:4]) %>%
  cbind(F23MPDSim2650[,2:4]) %>% cbind(F23MPDSim2655[,2:4]) %>% cbind(F23MPDSim2660[,2:4]) %>%
  cbind(F23MPDSim2665[,2:4]) %>% cbind(F23MPDSim2670[,2:4]) %>% cbind(F23MPDSim2675[,2:4]) %>%
  cbind(F23MPDSim2680[,2:4])
colnames(F23MPDSim26) <- MPDSimnames
write.csv(F23MPDSim26,file="F23MPDPred26.csv")

F23MPDSim45 <- F23MPDSim4525 %>% cbind(F23MPDSim4530[,2:4]) %>%
  cbind(F23MPDSim4535[,2:4]) %>% cbind(F23MPDSim4540[,2:4]) %>% cbind(F23MPDSim4545[,2:4]) %>%
  cbind(F23MPDSim4550[,2:4]) %>% cbind(F23MPDSim4555[,2:4]) %>% cbind(F23MPDSim4560[,2:4]) %>%
  cbind(F23MPDSim4565[,2:4]) %>% cbind(F23MPDSim4570[,2:4]) %>% cbind(F23MPDSim4575[,2:4]) %>%
  cbind(F23MPDSim4580[,2:4])
colnames(F23MPDSim45) <- MPDSimnames
write.csv(F23MPDSim45,file="F23MPDPred45.csv")

F23MPDSim85 <- F23MPDSim8525 %>% cbind(F23MPDSim8530[,2:4]) %>%
  cbind(F23MPDSim8535[,2:4]) %>% cbind(F23MPDSim8540[,2:4]) %>% cbind(F23MPDSim8545[,2:4]) %>%
  cbind(F23MPDSim8550[,2:4]) %>% cbind(F23MPDSim8555[,2:4]) %>% cbind(F23MPDSim8560[,2:4]) %>%
  cbind(F23MPDSim8565[,2:4]) %>% cbind(F23MPDSim8570[,2:4]) %>% cbind(F23MPDSim8575[,2:4]) %>%
  cbind(F23MPDSim8580[,2:4])
colnames(F23MPDSim85) <- MPDSimnames
write.csv(F23MPDSim85,file="F23MPDPred85.csv")


#fgroup 24
simMPD2625 <- link(MPDSplit24, data=all2625cc24 )
simmeanMPD2625 <- data.frame(apply(simMPD2625,2,mean))
PIMPD2625 <- t(data.frame(apply(simMPD2625,2,PI,prob=0.89)))
F24MPDSim2625 <- data.frame(cbind(simmeanMPD2625,PIMPD2625))
F24MPDSim2625$PlotCN <- all2625cc24  $PlotCN
F24MPDSim2625 <- F24MPDSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2625) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4525 <- link(MPDSplit24, data=all4525cc24 )
simmeanMPD4525 <- data.frame(apply(simMPD4525,2,mean))
PIMPD4525 <- t(data.frame(apply(simMPD4525,2,PI,prob=0.89)))
F24MPDSim4525 <- data.frame(cbind(simmeanMPD4525,PIMPD4525))
F24MPDSim4525$PlotCN <- all4525cc24  $PlotCN
F24MPDSim4525 <- F24MPDSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4525) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8525 <- link(MPDSplit24, data=all8525cc24 )
simmeanMPD8525 <- data.frame(apply(simMPD8525,2,mean))
PIMPD8525 <- t(data.frame(apply(simMPD8525,2,PI,prob=0.89)))
F24MPDSim8525 <- data.frame(cbind(simmeanMPD8525,PIMPD8525))
F24MPDSim8525$PlotCN <- all8525cc24  $PlotCN
F24MPDSim8525 <- F24MPDSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8525) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2630 <- link(MPDSplit24, data=all2630cc24 )
simmeanMPD2630 <- data.frame(apply(simMPD2630,2,mean))
PIMPD2630 <- t(data.frame(apply(simMPD2630,2,PI,prob=0.89)))
F24MPDSim2630 <- data.frame(cbind(simmeanMPD2630,PIMPD2630))
F24MPDSim2630$PlotCN <- all2630cc24  $PlotCN
F24MPDSim2630 <- F24MPDSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2630) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4530 <- link(MPDSplit24, data=all4530cc24 )
simmeanMPD4530 <- data.frame(apply(simMPD4530,2,mean))
PIMPD4530 <- t(data.frame(apply(simMPD4530,2,PI,prob=0.89)))
F24MPDSim4530 <- data.frame(cbind(simmeanMPD4530,PIMPD4530))
F24MPDSim4530$PlotCN <- all4530cc24  $PlotCN
F24MPDSim4530 <- F24MPDSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4530) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8530 <- link(MPDSplit24, data=all8530cc24 )
simmeanMPD8530 <- data.frame(apply(simMPD8530,2,mean))
PIMPD8530 <- t(data.frame(apply(simMPD8530,2,PI,prob=0.89)))
F24MPDSim8530 <- data.frame(cbind(simmeanMPD8530,PIMPD8530))
F24MPDSim8530$PlotCN <- all8530cc24  $PlotCN
F24MPDSim8530 <- F24MPDSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8530) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2635 <- link(MPDSplit24, data=all2635cc24 )
simmeanMPD2635 <- data.frame(apply(simMPD2635,2,mean))
PIMPD2635 <- t(data.frame(apply(simMPD2635,2,PI,prob=0.89)))
F24MPDSim2635 <- data.frame(cbind(simmeanMPD2635,PIMPD2635))
F24MPDSim2635$PlotCN <- all2635cc24  $PlotCN
F24MPDSim2635 <- F24MPDSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2635) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4535 <- link(MPDSplit24, data=all4535cc24 )
simmeanMPD4535 <- data.frame(apply(simMPD4535,2,mean))
PIMPD4535 <- t(data.frame(apply(simMPD4535,2,PI,prob=0.89)))
F24MPDSim4535 <- data.frame(cbind(simmeanMPD4535,PIMPD4535))
F24MPDSim4535$PlotCN <- all4535cc24  $PlotCN
F24MPDSim4535 <- F24MPDSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4535) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8535 <- link(MPDSplit24, data=all8535cc24 )
simmeanMPD8535 <- data.frame(apply(simMPD8535,2,mean))
PIMPD8535 <- t(data.frame(apply(simMPD8535,2,PI,prob=0.89)))
F24MPDSim8535 <- data.frame(cbind(simmeanMPD8535,PIMPD8535))
F24MPDSim8535$PlotCN <- all8535cc24  $PlotCN
F24MPDSim8535 <- F24MPDSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8535) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2640 <- link(MPDSplit24, data=all2640cc24 )
simmeanMPD2640 <- data.frame(apply(simMPD2640,2,mean))
PIMPD2640 <- t(data.frame(apply(simMPD2640,2,PI,prob=0.89)))
F24MPDSim2640 <- data.frame(cbind(simmeanMPD2640,PIMPD2640))
F24MPDSim2640$PlotCN <- all2640cc24  $PlotCN
F24MPDSim2640 <- F24MPDSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2640) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4540 <- link(MPDSplit24, data=all4540cc24 )
simmeanMPD4540 <- data.frame(apply(simMPD4540,2,mean))
PIMPD4540 <- t(data.frame(apply(simMPD4540,2,PI,prob=0.89)))
F24MPDSim4540 <- data.frame(cbind(simmeanMPD4540,PIMPD4540))
F24MPDSim4540$PlotCN <- all4540cc24  $PlotCN
F24MPDSim4540 <- F24MPDSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4540) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8540 <- link(MPDSplit24, data=all8540cc24 )
simmeanMPD8540 <- data.frame(apply(simMPD8540,2,mean))
PIMPD8540 <- t(data.frame(apply(simMPD8540,2,PI,prob=0.89)))
F24MPDSim8540 <- data.frame(cbind(simmeanMPD8540,PIMPD8540))
F24MPDSim8540$PlotCN <- all8540cc24  $PlotCN
F24MPDSim8540 <- F24MPDSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8540) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2645 <- link(MPDSplit24, data=all2645cc24 )
simmeanMPD2645 <- data.frame(apply(simMPD2645,2,mean))
PIMPD2645 <- t(data.frame(apply(simMPD2645,2,PI,prob=0.89)))
F24MPDSim2645 <- data.frame(cbind(simmeanMPD2645,PIMPD2645))
F24MPDSim2645$PlotCN <- all2645cc24  $PlotCN
F24MPDSim2645 <- F24MPDSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2645) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4545 <- link(MPDSplit24, data=all4545cc24 )
simmeanMPD4545 <- data.frame(apply(simMPD4545,2,mean))
PIMPD4545 <- t(data.frame(apply(simMPD4545,2,PI,prob=0.89)))
F24MPDSim4545 <- data.frame(cbind(simmeanMPD4545,PIMPD4545))
F24MPDSim4545$PlotCN <- all4545cc24  $PlotCN
F24MPDSim4545 <- F24MPDSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4545) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8545 <- link(MPDSplit24, data=all8545cc24 )
simmeanMPD8545 <- data.frame(apply(simMPD8545,2,mean))
PIMPD8545 <- t(data.frame(apply(simMPD8545,2,PI,prob=0.89)))
F24MPDSim8545 <- data.frame(cbind(simmeanMPD8545,PIMPD8545))
F24MPDSim8545$PlotCN <- all8545cc24  $PlotCN
F24MPDSim8545 <- F24MPDSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8545) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2650 <- link(MPDSplit24, data=all2650cc24 )
simmeanMPD2650 <- data.frame(apply(simMPD2650,2,mean))
PIMPD2650 <- t(data.frame(apply(simMPD2650,2,PI,prob=0.89)))
F24MPDSim2650 <- data.frame(cbind(simmeanMPD2650,PIMPD2650))
F24MPDSim2650$PlotCN <- all2650cc24  $PlotCN
F24MPDSim2650 <- F24MPDSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2650) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4550 <- link(MPDSplit24, data=all4550cc24 )
simmeanMPD4550 <- data.frame(apply(simMPD4550,2,mean))
PIMPD4550 <- t(data.frame(apply(simMPD4550,2,PI,prob=0.89)))
F24MPDSim4550 <- data.frame(cbind(simmeanMPD4550,PIMPD4550))
F24MPDSim4550$PlotCN <- all4550cc24  $PlotCN
F24MPDSim4550 <- F24MPDSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4550) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8550 <- link(MPDSplit24, data=all8550cc24 )
simmeanMPD8550 <- data.frame(apply(simMPD8550,2,mean))
PIMPD8550 <- t(data.frame(apply(simMPD8550,2,PI,prob=0.89)))
F24MPDSim8550 <- data.frame(cbind(simmeanMPD8550,PIMPD8550))
F24MPDSim8550$PlotCN <- all8550cc24  $PlotCN
F24MPDSim8550 <- F24MPDSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8550) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2655 <- link(MPDSplit24, data=all2655cc24 )
simmeanMPD2655 <- data.frame(apply(simMPD2655,2,mean))
PIMPD2655 <- t(data.frame(apply(simMPD2655,2,PI,prob=0.89)))
F24MPDSim2655 <- data.frame(cbind(simmeanMPD2655,PIMPD2655))
F24MPDSim2655$PlotCN <- all2655cc24  $PlotCN
F24MPDSim2655 <- F24MPDSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2655) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4555 <- link(MPDSplit24, data=all4555cc24 )
simmeanMPD4555 <- data.frame(apply(simMPD4555,2,mean))
PIMPD4555 <- t(data.frame(apply(simMPD4555,2,PI,prob=0.89)))
F24MPDSim4555 <- data.frame(cbind(simmeanMPD4555,PIMPD4555))
F24MPDSim4555$PlotCN <- all4555cc24  $PlotCN
F24MPDSim4555 <- F24MPDSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4555) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8555 <- link(MPDSplit24, data=all8555cc24 )
simmeanMPD8555 <- data.frame(apply(simMPD8555,2,mean))
PIMPD8555 <- t(data.frame(apply(simMPD8555,2,PI,prob=0.89)))
F24MPDSim8555 <- data.frame(cbind(simmeanMPD8555,PIMPD8555))
F24MPDSim8555$PlotCN <- all8555cc24  $PlotCN
F24MPDSim8555 <- F24MPDSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8555) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2660 <- link(MPDSplit24, data=all2660cc24 )
simmeanMPD2660 <- data.frame(apply(simMPD2660,2,mean))
PIMPD2660 <- t(data.frame(apply(simMPD2660,2,PI,prob=0.89)))
F24MPDSim2660 <- data.frame(cbind(simmeanMPD2660,PIMPD2660))
F24MPDSim2660$PlotCN <- all2660cc24  $PlotCN
F24MPDSim2660 <- F24MPDSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2660) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4560 <- link(MPDSplit24, data=all4560cc24 )
simmeanMPD4560 <- data.frame(apply(simMPD4560,2,mean))
PIMPD4560 <- t(data.frame(apply(simMPD4560,2,PI,prob=0.89)))
F24MPDSim4560 <- data.frame(cbind(simmeanMPD4560,PIMPD4560))
F24MPDSim4560$PlotCN <- all4560cc24  $PlotCN
F24MPDSim4560 <- F24MPDSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4560) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8560 <- link(MPDSplit24, data=all8560cc24 )
simmeanMPD8560 <- data.frame(apply(simMPD8560,2,mean))
PIMPD8560 <- t(data.frame(apply(simMPD8560,2,PI,prob=0.89)))
F24MPDSim8560 <- data.frame(cbind(simmeanMPD8560,PIMPD8560))
F24MPDSim8560$PlotCN <- all8560cc24  $PlotCN
F24MPDSim8560 <- F24MPDSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8560) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2665 <- link(MPDSplit24, data=all2665cc24 )
simmeanMPD2665 <- data.frame(apply(simMPD2665,2,mean))
PIMPD2665 <- t(data.frame(apply(simMPD2665,2,PI,prob=0.89)))
F24MPDSim2665 <- data.frame(cbind(simmeanMPD2665,PIMPD2665))
F24MPDSim2665$PlotCN <- all2665cc24  $PlotCN
F24MPDSim2665 <- F24MPDSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2665) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4565 <- link(MPDSplit24, data=all4565cc24 )
simmeanMPD4565 <- data.frame(apply(simMPD4565,2,mean))
PIMPD4565 <- t(data.frame(apply(simMPD4565,2,PI,prob=0.89)))
F24MPDSim4565 <- data.frame(cbind(simmeanMPD4565,PIMPD4565))
F24MPDSim4565$PlotCN <- all4565cc24  $PlotCN
F24MPDSim4565 <- F24MPDSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4565) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8565 <- link(MPDSplit24, data=all8565cc24 )
simmeanMPD8565 <- data.frame(apply(simMPD8565,2,mean))
PIMPD8565 <- t(data.frame(apply(simMPD8565,2,PI,prob=0.89)))
F24MPDSim8565 <- data.frame(cbind(simmeanMPD8565,PIMPD8565))
F24MPDSim8565$PlotCN <- all8565cc24  $PlotCN
F24MPDSim8565 <- F24MPDSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8565) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2670 <- link(MPDSplit24, data=all2670cc24 )
simmeanMPD2670 <- data.frame(apply(simMPD2670,2,mean))
PIMPD2670 <- t(data.frame(apply(simMPD2670,2,PI,prob=0.89)))
F24MPDSim2670 <- data.frame(cbind(simmeanMPD2670,PIMPD2670))
F24MPDSim2670$PlotCN <- all2670cc24  $PlotCN
F24MPDSim2670 <- F24MPDSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2670) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4570 <- link(MPDSplit24, data=all4570cc24 )
simmeanMPD4570 <- data.frame(apply(simMPD4570,2,mean))
PIMPD4570 <- t(data.frame(apply(simMPD4570,2,PI,prob=0.89)))
F24MPDSim4570 <- data.frame(cbind(simmeanMPD4570,PIMPD4570))
F24MPDSim4570$PlotCN <- all4570cc24  $PlotCN
F24MPDSim4570 <- F24MPDSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4570) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8570 <- link(MPDSplit24, data=all8570cc24 )
simmeanMPD8570 <- data.frame(apply(simMPD8570,2,mean))
PIMPD8570 <- t(data.frame(apply(simMPD8570,2,PI,prob=0.89)))
F24MPDSim8570 <- data.frame(cbind(simmeanMPD8570,PIMPD8570))
F24MPDSim8570$PlotCN <- all8570cc24  $PlotCN
F24MPDSim8570 <- F24MPDSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8570) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2675 <- link(MPDSplit24, data=all2675cc24 )
simmeanMPD2675 <- data.frame(apply(simMPD2675,2,mean))
PIMPD2675 <- t(data.frame(apply(simMPD2675,2,PI,prob=0.89)))
F24MPDSim2675 <- data.frame(cbind(simmeanMPD2675,PIMPD2675))
F24MPDSim2675$PlotCN <- all2675cc24  $PlotCN
F24MPDSim2675 <- F24MPDSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2675) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4575 <- link(MPDSplit24, data=all4575cc24 )
simmeanMPD4575 <- data.frame(apply(simMPD4575,2,mean))
PIMPD4575 <- t(data.frame(apply(simMPD4575,2,PI,prob=0.89)))
F24MPDSim4575 <- data.frame(cbind(simmeanMPD4575,PIMPD4575))
F24MPDSim4575$PlotCN <- all4575cc24  $PlotCN
F24MPDSim4575 <- F24MPDSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4575) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8575 <- link(MPDSplit24, data=all8575cc24 )
simmeanMPD8575 <- data.frame(apply(simMPD8575,2,mean))
PIMPD8575 <- t(data.frame(apply(simMPD8575,2,PI,prob=0.89)))
F24MPDSim8575 <- data.frame(cbind(simmeanMPD8575,PIMPD8575))
F24MPDSim8575$PlotCN <- all8575cc24  $PlotCN
F24MPDSim8575 <- F24MPDSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8575) <- c("PlotCN","MPDPerAcre","5CI","95CI")

simMPD2680 <- link(MPDSplit24, data=all2680cc24 )
simmeanMPD2680 <- data.frame(apply(simMPD2680,2,mean))
PIMPD2680 <- t(data.frame(apply(simMPD2680,2,PI,prob=0.89)))
F24MPDSim2680 <- data.frame(cbind(simmeanMPD2680,PIMPD2680))
F24MPDSim2680$PlotCN <- all2680cc24  $PlotCN
F24MPDSim2680 <- F24MPDSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim2680) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD4580 <- link(MPDSplit24, data=all4580cc24 )
simmeanMPD4580 <- data.frame(apply(simMPD4580,2,mean))
PIMPD4580 <- t(data.frame(apply(simMPD4580,2,PI,prob=0.89)))
F24MPDSim4580 <- data.frame(cbind(simmeanMPD4580,PIMPD4580))
F24MPDSim4580$PlotCN <- all4580cc24  $PlotCN
F24MPDSim4580 <- F24MPDSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim4580) <- c("PlotCN","MPDPerAcre","5CI","95CI")
simMPD8580 <- link(MPDSplit24, data=all8580cc24 )
simmeanMPD8580 <- data.frame(apply(simMPD8580,2,mean))
PIMPD8580 <- t(data.frame(apply(simMPD8580,2,PI,prob=0.89)))
F24MPDSim8580 <- data.frame(cbind(simmeanMPD8580,PIMPD8580))
F24MPDSim8580$PlotCN <- all8580cc24  $PlotCN
F24MPDSim8580 <- F24MPDSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F24MPDSim8580) <- c("PlotCN","MPDPerAcre","5CI","95CI")

#create vector of new colnames
MPDSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("MPD", "5CI", "95CI"), x)))
MPDSimnames<- append(MPDSimnames,"PlotCN",after=0)

#group data by RCP pathway
F24MPDSim26 <- F24MPDSim2625 %>% cbind(F24MPDSim2630[,2:4]) %>%
  cbind(F24MPDSim2635[,2:4]) %>% cbind(F24MPDSim2640[,2:4]) %>% cbind(F24MPDSim2645[,2:4]) %>%
  cbind(F24MPDSim2650[,2:4]) %>% cbind(F24MPDSim2655[,2:4]) %>% cbind(F24MPDSim2660[,2:4]) %>%
  cbind(F24MPDSim2665[,2:4]) %>% cbind(F24MPDSim2670[,2:4]) %>% cbind(F24MPDSim2675[,2:4]) %>%
  cbind(F24MPDSim2680[,2:4])
colnames(F24MPDSim26) <- MPDSimnames
write.csv(F24MPDSim26,file="F24MPDPred26.csv")

F24MPDSim45 <- F24MPDSim4525 %>% cbind(F24MPDSim4530[,2:4]) %>%
  cbind(F24MPDSim4535[,2:4]) %>% cbind(F24MPDSim4540[,2:4]) %>% cbind(F24MPDSim4545[,2:4]) %>%
  cbind(F24MPDSim4550[,2:4]) %>% cbind(F24MPDSim4555[,2:4]) %>% cbind(F24MPDSim4560[,2:4]) %>%
  cbind(F24MPDSim4565[,2:4]) %>% cbind(F24MPDSim4570[,2:4]) %>% cbind(F24MPDSim4575[,2:4]) %>%
  cbind(F24MPDSim4580[,2:4])
colnames(F24MPDSim45) <- MPDSimnames
write.csv(F24MPDSim45,file="F24MPDPred45.csv")

F24MPDSim85 <- F24MPDSim8525 %>% cbind(F24MPDSim8530[,2:4]) %>%
  cbind(F24MPDSim8535[,2:4]) %>% cbind(F24MPDSim8540[,2:4]) %>% cbind(F24MPDSim8545[,2:4]) %>%
  cbind(F24MPDSim8550[,2:4]) %>% cbind(F24MPDSim8555[,2:4]) %>% cbind(F24MPDSim8560[,2:4]) %>%
  cbind(F24MPDSim8565[,2:4]) %>% cbind(F24MPDSim8570[,2:4]) %>% cbind(F24MPDSim8575[,2:4]) %>%
  cbind(F24MPDSim8580[,2:4])
colnames(F24MPDSim85) <- MPDSimnames
write.csv(F24MPDSim85,file="F24MPDPred85.csv")


#Mortality predictions
#fgroup 1
simMort2625 <- link(MortSplit1_beta, data=all2625cc1)
simmeanMort2625 <- data.frame(apply(simMort2625,2,mean))
PIMort2625 <- t(data.frame(apply(simMort2625,2,PI,prob=0.89)))
F1MortSim2625 <- data.frame(cbind(simmeanMort2625,PIMort2625))
F1MortSim2625$PlotCN <- all2625cc1 $PlotCN
F1MortSim2625 <- F1MortSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2625) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4525 <- link(MortSplit1_beta, data=all4525cc1)
simmeanMort4525 <- data.frame(apply(simMort4525,2,mean))
PIMort4525 <- t(data.frame(apply(simMort4525,2,PI,prob=0.89)))
F1MortSim4525 <- data.frame(cbind(simmeanMort4525,PIMort4525))
F1MortSim4525$PlotCN <- all4525cc1 $PlotCN
F1MortSim4525 <- F1MortSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4525) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8525 <- link(MortSplit1_beta, data=all8525cc1)
simmeanMort8525 <- data.frame(apply(simMort8525,2,mean))
PIMort8525 <- t(data.frame(apply(simMort8525,2,PI,prob=0.89)))
F1MortSim8525 <- data.frame(cbind(simmeanMort8525,PIMort8525))
F1MortSim8525$PlotCN <- all8525cc1 $PlotCN
F1MortSim8525 <- F1MortSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8525) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2630 <- link(MortSplit1_beta, data=all2630cc1)
simmeanMort2630 <- data.frame(apply(simMort2630,2,mean))
PIMort2630 <- t(data.frame(apply(simMort2630,2,PI,prob=0.89)))
F1MortSim2630 <- data.frame(cbind(simmeanMort2630,PIMort2630))
F1MortSim2630$PlotCN <- all2630cc1 $PlotCN
F1MortSim2630 <- F1MortSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2630) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4530 <- link(MortSplit1_beta, data=all4530cc1)
simmeanMort4530 <- data.frame(apply(simMort4530,2,mean))
PIMort4530 <- t(data.frame(apply(simMort4530,2,PI,prob=0.89)))
F1MortSim4530 <- data.frame(cbind(simmeanMort4530,PIMort4530))
F1MortSim4530$PlotCN <- all4530cc1 $PlotCN
F1MortSim4530 <- F1MortSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4530) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8530 <- link(MortSplit1_beta, data=all8530cc1)
simmeanMort8530 <- data.frame(apply(simMort8530,2,mean))
PIMort8530 <- t(data.frame(apply(simMort8530,2,PI,prob=0.89)))
F1MortSim8530 <- data.frame(cbind(simmeanMort8530,PIMort8530))
F1MortSim8530$PlotCN <- all8530cc1 $PlotCN
F1MortSim8530 <- F1MortSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8530) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2635 <- link(MortSplit1_beta, data=all2635cc1)
simmeanMort2635 <- data.frame(apply(simMort2635,2,mean))
PIMort2635 <- t(data.frame(apply(simMort2635,2,PI,prob=0.89)))
F1MortSim2635 <- data.frame(cbind(simmeanMort2635,PIMort2635))
F1MortSim2635$PlotCN <- all2635cc1 $PlotCN
F1MortSim2635 <- F1MortSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2635) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4535 <- link(MortSplit1_beta, data=all4535cc1)
simmeanMort4535 <- data.frame(apply(simMort4535,2,mean))
PIMort4535 <- t(data.frame(apply(simMort4535,2,PI,prob=0.89)))
F1MortSim4535 <- data.frame(cbind(simmeanMort4535,PIMort4535))
F1MortSim4535$PlotCN <- all4535cc1 $PlotCN
F1MortSim4535 <- F1MortSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4535) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8535 <- link(MortSplit1_beta, data=all8535cc1)
simmeanMort8535 <- data.frame(apply(simMort8535,2,mean))
PIMort8535 <- t(data.frame(apply(simMort8535,2,PI,prob=0.89)))
F1MortSim8535 <- data.frame(cbind(simmeanMort8535,PIMort8535))
F1MortSim8535$PlotCN <- all8535cc1 $PlotCN
F1MortSim8535 <- F1MortSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8535) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2640 <- link(MortSplit1_beta, data=all2640cc1)
simmeanMort2640 <- data.frame(apply(simMort2640,2,mean))
PIMort2640 <- t(data.frame(apply(simMort2640,2,PI,prob=0.89)))
F1MortSim2640 <- data.frame(cbind(simmeanMort2640,PIMort2640))
F1MortSim2640$PlotCN <- all2640cc1 $PlotCN
F1MortSim2640 <- F1MortSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2640) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4540 <- link(MortSplit1_beta, data=all4540cc1)
simmeanMort4540 <- data.frame(apply(simMort4540,2,mean))
PIMort4540 <- t(data.frame(apply(simMort4540,2,PI,prob=0.89)))
F1MortSim4540 <- data.frame(cbind(simmeanMort4540,PIMort4540))
F1MortSim4540$PlotCN <- all4540cc1 $PlotCN
F1MortSim4540 <- F1MortSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4540) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8540 <- link(MortSplit1_beta, data=all8540cc1)
simmeanMort8540 <- data.frame(apply(simMort8540,2,mean))
PIMort8540 <- t(data.frame(apply(simMort8540,2,PI,prob=0.89)))
F1MortSim8540 <- data.frame(cbind(simmeanMort8540,PIMort8540))
F1MortSim8540$PlotCN <- all8540cc1 $PlotCN
F1MortSim8540 <- F1MortSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8540) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2645 <- link(MortSplit1_beta, data=all2645cc1)
simmeanMort2645 <- data.frame(apply(simMort2645,2,mean))
PIMort2645 <- t(data.frame(apply(simMort2645,2,PI,prob=0.89)))
F1MortSim2645 <- data.frame(cbind(simmeanMort2645,PIMort2645))
F1MortSim2645$PlotCN <- all2645cc1 $PlotCN
F1MortSim2645 <- F1MortSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2645) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4545 <- link(MortSplit1_beta, data=all4545cc1)
simmeanMort4545 <- data.frame(apply(simMort4545,2,mean))
PIMort4545 <- t(data.frame(apply(simMort4545,2,PI,prob=0.89)))
F1MortSim4545 <- data.frame(cbind(simmeanMort4545,PIMort4545))
F1MortSim4545$PlotCN <- all4545cc1 $PlotCN
F1MortSim4545 <- F1MortSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4545) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8545 <- link(MortSplit1_beta, data=all8545cc1)
simmeanMort8545 <- data.frame(apply(simMort8545,2,mean))
PIMort8545 <- t(data.frame(apply(simMort8545,2,PI,prob=0.89)))
F1MortSim8545 <- data.frame(cbind(simmeanMort8545,PIMort8545))
F1MortSim8545$PlotCN <- all8545cc1 $PlotCN
F1MortSim8545 <- F1MortSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8545) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2650 <- link(MortSplit1_beta, data=all2650cc1)
simmeanMort2650 <- data.frame(apply(simMort2650,2,mean))
PIMort2650 <- t(data.frame(apply(simMort2650,2,PI,prob=0.89)))
F1MortSim2650 <- data.frame(cbind(simmeanMort2650,PIMort2650))
F1MortSim2650$PlotCN <- all2650cc1 $PlotCN
F1MortSim2650 <- F1MortSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2650) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4550 <- link(MortSplit1_beta, data=all4550cc1)
simmeanMort4550 <- data.frame(apply(simMort4550,2,mean))
PIMort4550 <- t(data.frame(apply(simMort4550,2,PI,prob=0.89)))
F1MortSim4550 <- data.frame(cbind(simmeanMort4550,PIMort4550))
F1MortSim4550$PlotCN <- all4550cc1 $PlotCN
F1MortSim4550 <- F1MortSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4550) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8550 <- link(MortSplit1_beta, data=all8550cc1)
simmeanMort8550 <- data.frame(apply(simMort8550,2,mean))
PIMort8550 <- t(data.frame(apply(simMort8550,2,PI,prob=0.89)))
F1MortSim8550 <- data.frame(cbind(simmeanMort8550,PIMort8550))
F1MortSim8550$PlotCN <- all8550cc1 $PlotCN
F1MortSim8550 <- F1MortSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8550) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2655 <- link(MortSplit1_beta, data=all2655cc1)
simmeanMort2655 <- data.frame(apply(simMort2655,2,mean))
PIMort2655 <- t(data.frame(apply(simMort2655,2,PI,prob=0.89)))
F1MortSim2655 <- data.frame(cbind(simmeanMort2655,PIMort2655))
F1MortSim2655$PlotCN <- all2655cc1 $PlotCN
F1MortSim2655 <- F1MortSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2655) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4555 <- link(MortSplit1_beta, data=all4555cc1)
simmeanMort4555 <- data.frame(apply(simMort4555,2,mean))
PIMort4555 <- t(data.frame(apply(simMort4555,2,PI,prob=0.89)))
F1MortSim4555 <- data.frame(cbind(simmeanMort4555,PIMort4555))
F1MortSim4555$PlotCN <- all4555cc1 $PlotCN
F1MortSim4555 <- F1MortSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4555) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8555 <- link(MortSplit1_beta, data=all8555cc1)
simmeanMort8555 <- data.frame(apply(simMort8555,2,mean))
PIMort8555 <- t(data.frame(apply(simMort8555,2,PI,prob=0.89)))
F1MortSim8555 <- data.frame(cbind(simmeanMort8555,PIMort8555))
F1MortSim8555$PlotCN <- all8555cc1 $PlotCN
F1MortSim8555 <- F1MortSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8555) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2660 <- link(MortSplit1_beta, data=all2660cc1)
simmeanMort2660 <- data.frame(apply(simMort2660,2,mean))
PIMort2660 <- t(data.frame(apply(simMort2660,2,PI,prob=0.89)))
F1MortSim2660 <- data.frame(cbind(simmeanMort2660,PIMort2660))
F1MortSim2660$PlotCN <- all2660cc1 $PlotCN
F1MortSim2660 <- F1MortSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2660) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4560 <- link(MortSplit1_beta, data=all4560cc1)
simmeanMort4560 <- data.frame(apply(simMort4560,2,mean))
PIMort4560 <- t(data.frame(apply(simMort4560,2,PI,prob=0.89)))
F1MortSim4560 <- data.frame(cbind(simmeanMort4560,PIMort4560))
F1MortSim4560$PlotCN <- all4560cc1 $PlotCN
F1MortSim4560 <- F1MortSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4560) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8560 <- link(MortSplit1_beta, data=all8560cc1)
simmeanMort8560 <- data.frame(apply(simMort8560,2,mean))
PIMort8560 <- t(data.frame(apply(simMort8560,2,PI,prob=0.89)))
F1MortSim8560 <- data.frame(cbind(simmeanMort8560,PIMort8560))
F1MortSim8560$PlotCN <- all8560cc1 $PlotCN
F1MortSim8560 <- F1MortSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8560) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2665 <- link(MortSplit1_beta, data=all2665cc1)
simmeanMort2665 <- data.frame(apply(simMort2665,2,mean))
PIMort2665 <- t(data.frame(apply(simMort2665,2,PI,prob=0.89)))
F1MortSim2665 <- data.frame(cbind(simmeanMort2665,PIMort2665))
F1MortSim2665$PlotCN <- all2665cc1 $PlotCN
F1MortSim2665 <- F1MortSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2665) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4565 <- link(MortSplit1_beta, data=all4565cc1)
simmeanMort4565 <- data.frame(apply(simMort4565,2,mean))
PIMort4565 <- t(data.frame(apply(simMort4565,2,PI,prob=0.89)))
F1MortSim4565 <- data.frame(cbind(simmeanMort4565,PIMort4565))
F1MortSim4565$PlotCN <- all4565cc1 $PlotCN
F1MortSim4565 <- F1MortSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4565) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8565 <- link(MortSplit1_beta, data=all8565cc1)
simmeanMort8565 <- data.frame(apply(simMort8565,2,mean))
PIMort8565 <- t(data.frame(apply(simMort8565,2,PI,prob=0.89)))
F1MortSim8565 <- data.frame(cbind(simmeanMort8565,PIMort8565))
F1MortSim8565$PlotCN <- all8565cc1 $PlotCN
F1MortSim8565 <- F1MortSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8565) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2670 <- link(MortSplit1_beta, data=all2670cc1)
simmeanMort2670 <- data.frame(apply(simMort2670,2,mean))
PIMort2670 <- t(data.frame(apply(simMort2670,2,PI,prob=0.89)))
F1MortSim2670 <- data.frame(cbind(simmeanMort2670,PIMort2670))
F1MortSim2670$PlotCN <- all2670cc1 $PlotCN
F1MortSim2670 <- F1MortSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2670) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4570 <- link(MortSplit1_beta, data=all4570cc1)
simmeanMort4570 <- data.frame(apply(simMort4570,2,mean))
PIMort4570 <- t(data.frame(apply(simMort4570,2,PI,prob=0.89)))
F1MortSim4570 <- data.frame(cbind(simmeanMort4570,PIMort4570))
F1MortSim4570$PlotCN <- all4570cc1 $PlotCN
F1MortSim4570 <- F1MortSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4570) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8570 <- link(MortSplit1_beta, data=all8570cc1)
simmeanMort8570 <- data.frame(apply(simMort8570,2,mean))
PIMort8570 <- t(data.frame(apply(simMort8570,2,PI,prob=0.89)))
F1MortSim8570 <- data.frame(cbind(simmeanMort8570,PIMort8570))
F1MortSim8570$PlotCN <- all8570cc1 $PlotCN
F1MortSim8570 <- F1MortSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8570) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2675 <- link(MortSplit1_beta, data=all2675cc1)
simmeanMort2675 <- data.frame(apply(simMort2675,2,mean))
PIMort2675 <- t(data.frame(apply(simMort2675,2,PI,prob=0.89)))
F1MortSim2675 <- data.frame(cbind(simmeanMort2675,PIMort2675))
F1MortSim2675$PlotCN <- all2675cc1 $PlotCN
F1MortSim2675 <- F1MortSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2675) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4575 <- link(MortSplit1_beta, data=all4575cc1)
simmeanMort4575 <- data.frame(apply(simMort4575,2,mean))
PIMort4575 <- t(data.frame(apply(simMort4575,2,PI,prob=0.89)))
F1MortSim4575 <- data.frame(cbind(simmeanMort4575,PIMort4575))
F1MortSim4575$PlotCN <- all4575cc1 $PlotCN
F1MortSim4575 <- F1MortSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4575) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8575 <- link(MortSplit1_beta, data=all8575cc1)
simmeanMort8575 <- data.frame(apply(simMort8575,2,mean))
PIMort8575 <- t(data.frame(apply(simMort8575,2,PI,prob=0.89)))
F1MortSim8575 <- data.frame(cbind(simmeanMort8575,PIMort8575))
F1MortSim8575$PlotCN <- all8575cc1 $PlotCN
F1MortSim8575 <- F1MortSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8575) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2680 <- link(MortSplit1_beta, data=all2680cc1)
simmeanMort2680 <- data.frame(apply(simMort2680,2,mean))
PIMort2680 <- t(data.frame(apply(simMort2680,2,PI,prob=0.89)))
F1MortSim2680 <- data.frame(cbind(simmeanMort2680,PIMort2680))
F1MortSim2680$PlotCN <- all2680cc1 $PlotCN
F1MortSim2680 <- F1MortSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim2680) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4580 <- link(MortSplit1_beta, data=all4580cc1)
simmeanMort4580 <- data.frame(apply(simMort4580,2,mean))
PIMort4580 <- t(data.frame(apply(simMort4580,2,PI,prob=0.89)))
F1MortSim4580 <- data.frame(cbind(simmeanMort4580,PIMort4580))
F1MortSim4580$PlotCN <- all4580cc1 $PlotCN
F1MortSim4580 <- F1MortSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim4580) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8580 <- link(MortSplit1_beta, data=all8580cc1)
simmeanMort8580 <- data.frame(apply(simMort8580,2,mean))
PIMort8580 <- t(data.frame(apply(simMort8580,2,PI,prob=0.89)))
F1MortSim8580 <- data.frame(cbind(simmeanMort8580,PIMort8580))
F1MortSim8580$PlotCN <- all8580cc1 $PlotCN
F1MortSim8580 <- F1MortSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F1MortSim8580) <- c("PlotCN","MortPerAcre","5CI","95CI")


#create vector of new colnames
MortSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Mort", "5CI", "95CI"), x)))
MortSimnames<- append(MortSimnames,"PlotCN",after=0)

#group data by RCP pathway
F1MortSim26B <- F1MortSim2625 %>% cbind(F1MortSim2630[,2:4]) %>%
  cbind(F1MortSim2635[,2:4]) %>% cbind(F1MortSim2640[,2:4]) %>% cbind(F1MortSim2645[,2:4]) %>%
  cbind(F1MortSim2650[,2:4]) %>% cbind(F1MortSim2655[,2:4]) %>% cbind(F1MortSim2660[,2:4]) %>%
  cbind(F1MortSim2665[,2:4]) %>% cbind(F1MortSim2670[,2:4]) %>% cbind(F1MortSim2675[,2:4]) %>%
  cbind(F1MortSim2680[,2:4])
colnames(F1MortSim26B) <- MortSimnames
write.csv(F1MortSim26B,file="F1MortPred26B.csv")

F1MortSim45B <- F1MortSim4525 %>% cbind(F1MortSim4530[,2:4]) %>%
  cbind(F1MortSim4535[,2:4]) %>% cbind(F1MortSim4540[,2:4]) %>% cbind(F1MortSim4545[,2:4]) %>%
  cbind(F1MortSim4550[,2:4]) %>% cbind(F1MortSim4555[,2:4]) %>% cbind(F1MortSim4560[,2:4]) %>%
  cbind(F1MortSim4565[,2:4]) %>% cbind(F1MortSim4570[,2:4]) %>% cbind(F1MortSim4575[,2:4]) %>%
  cbind(F1MortSim4580[,2:4])
colnames(F1MortSim45B) <- MortSimnames
write.csv(F1MortSim45B,file="F1MortPred45B.csv")

F1MortSim85B <- F1MortSim8525 %>% cbind(F1MortSim8530[,2:4]) %>%
  cbind(F1MortSim8535[,2:4]) %>% cbind(F1MortSim8540[,2:4]) %>% cbind(F1MortSim8545[,2:4]) %>%
  cbind(F1MortSim8550[,2:4]) %>% cbind(F1MortSim8555[,2:4]) %>% cbind(F1MortSim8560[,2:4]) %>%
  cbind(F1MortSim8565[,2:4]) %>% cbind(F1MortSim8570[,2:4]) %>% cbind(F1MortSim8575[,2:4]) %>%
  cbind(F1MortSim8580[,2:4])
colnames(F1MortSim85B) <- MortSimnames
write.csv(F1MortSim85B,file="F1MortPred85B.csv")


#fgroup 5
simMort2625 <- link(MortSplit5_beta, data=all2625cc5 )
simmeanMort2625 <- data.frame(apply(simMort2625,2,mean))
PIMort2625 <- t(data.frame(apply(simMort2625,2,PI,prob=0.89)))
F5MortSim2625 <- data.frame(cbind(simmeanMort2625,PIMort2625))
F5MortSim2625$PlotCN <- all2625cc5  $PlotCN
F5MortSim2625 <- F5MortSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2625) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4525 <- link(MortSplit5_beta, data=all4525cc5 )
simmeanMort4525 <- data.frame(apply(simMort4525,2,mean))
PIMort4525 <- t(data.frame(apply(simMort4525,2,PI,prob=0.89)))
F5MortSim4525 <- data.frame(cbind(simmeanMort4525,PIMort4525))
F5MortSim4525$PlotCN <- all4525cc5  $PlotCN
F5MortSim4525 <- F5MortSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4525) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8525 <- link(MortSplit5_beta, data=all8525cc5 )
simmeanMort8525 <- data.frame(apply(simMort8525,2,mean))
PIMort8525 <- t(data.frame(apply(simMort8525,2,PI,prob=0.89)))
F5MortSim8525 <- data.frame(cbind(simmeanMort8525,PIMort8525))
F5MortSim8525$PlotCN <- all8525cc5  $PlotCN
F5MortSim8525 <- F5MortSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8525) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2630 <- link(MortSplit5_beta, data=all2630cc5 )
simmeanMort2630 <- data.frame(apply(simMort2630,2,mean))
PIMort2630 <- t(data.frame(apply(simMort2630,2,PI,prob=0.89)))
F5MortSim2630 <- data.frame(cbind(simmeanMort2630,PIMort2630))
F5MortSim2630$PlotCN <- all2630cc5  $PlotCN
F5MortSim2630 <- F5MortSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2630) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4530 <- link(MortSplit5_beta, data=all4530cc5 )
simmeanMort4530 <- data.frame(apply(simMort4530,2,mean))
PIMort4530 <- t(data.frame(apply(simMort4530,2,PI,prob=0.89)))
F5MortSim4530 <- data.frame(cbind(simmeanMort4530,PIMort4530))
F5MortSim4530$PlotCN <- all4530cc5  $PlotCN
F5MortSim4530 <- F5MortSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4530) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8530 <- link(MortSplit5_beta, data=all8530cc5 )
simmeanMort8530 <- data.frame(apply(simMort8530,2,mean))
PIMort8530 <- t(data.frame(apply(simMort8530,2,PI,prob=0.89)))
F5MortSim8530 <- data.frame(cbind(simmeanMort8530,PIMort8530))
F5MortSim8530$PlotCN <- all8530cc5  $PlotCN
F5MortSim8530 <- F5MortSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8530) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2635 <- link(MortSplit5_beta, data=all2635cc5 )
simmeanMort2635 <- data.frame(apply(simMort2635,2,mean))
PIMort2635 <- t(data.frame(apply(simMort2635,2,PI,prob=0.89)))
F5MortSim2635 <- data.frame(cbind(simmeanMort2635,PIMort2635))
F5MortSim2635$PlotCN <- all2635cc5  $PlotCN
F5MortSim2635 <- F5MortSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2635) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4535 <- link(MortSplit5_beta, data=all4535cc5 )
simmeanMort4535 <- data.frame(apply(simMort4535,2,mean))
PIMort4535 <- t(data.frame(apply(simMort4535,2,PI,prob=0.89)))
F5MortSim4535 <- data.frame(cbind(simmeanMort4535,PIMort4535))
F5MortSim4535$PlotCN <- all4535cc5  $PlotCN
F5MortSim4535 <- F5MortSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4535) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8535 <- link(MortSplit5_beta, data=all8535cc5 )
simmeanMort8535 <- data.frame(apply(simMort8535,2,mean))
PIMort8535 <- t(data.frame(apply(simMort8535,2,PI,prob=0.89)))
F5MortSim8535 <- data.frame(cbind(simmeanMort8535,PIMort8535))
F5MortSim8535$PlotCN <- all8535cc5  $PlotCN
F5MortSim8535 <- F5MortSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8535) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2640 <- link(MortSplit5_beta, data=all2640cc5 )
simmeanMort2640 <- data.frame(apply(simMort2640,2,mean))
PIMort2640 <- t(data.frame(apply(simMort2640,2,PI,prob=0.89)))
F5MortSim2640 <- data.frame(cbind(simmeanMort2640,PIMort2640))
F5MortSim2640$PlotCN <- all2640cc5  $PlotCN
F5MortSim2640 <- F5MortSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2640) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4540 <- link(MortSplit5_beta, data=all4540cc5 )
simmeanMort4540 <- data.frame(apply(simMort4540,2,mean))
PIMort4540 <- t(data.frame(apply(simMort4540,2,PI,prob=0.89)))
F5MortSim4540 <- data.frame(cbind(simmeanMort4540,PIMort4540))
F5MortSim4540$PlotCN <- all4540cc5  $PlotCN
F5MortSim4540 <- F5MortSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4540) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8540 <- link(MortSplit5_beta, data=all8540cc5 )
simmeanMort8540 <- data.frame(apply(simMort8540,2,mean))
PIMort8540 <- t(data.frame(apply(simMort8540,2,PI,prob=0.89)))
F5MortSim8540 <- data.frame(cbind(simmeanMort8540,PIMort8540))
F5MortSim8540$PlotCN <- all8540cc5  $PlotCN
F5MortSim8540 <- F5MortSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8540) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2645 <- link(MortSplit5_beta, data=all2645cc5 )
simmeanMort2645 <- data.frame(apply(simMort2645,2,mean))
PIMort2645 <- t(data.frame(apply(simMort2645,2,PI,prob=0.89)))
F5MortSim2645 <- data.frame(cbind(simmeanMort2645,PIMort2645))
F5MortSim2645$PlotCN <- all2645cc5  $PlotCN
F5MortSim2645 <- F5MortSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2645) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4545 <- link(MortSplit5_beta, data=all4545cc5 )
simmeanMort4545 <- data.frame(apply(simMort4545,2,mean))
PIMort4545 <- t(data.frame(apply(simMort4545,2,PI,prob=0.89)))
F5MortSim4545 <- data.frame(cbind(simmeanMort4545,PIMort4545))
F5MortSim4545$PlotCN <- all4545cc5  $PlotCN
F5MortSim4545 <- F5MortSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4545) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8545 <- link(MortSplit5_beta, data=all8545cc5 )
simmeanMort8545 <- data.frame(apply(simMort8545,2,mean))
PIMort8545 <- t(data.frame(apply(simMort8545,2,PI,prob=0.89)))
F5MortSim8545 <- data.frame(cbind(simmeanMort8545,PIMort8545))
F5MortSim8545$PlotCN <- all8545cc5  $PlotCN
F5MortSim8545 <- F5MortSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8545) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2650 <- link(MortSplit5_beta, data=all2650cc5 )
simmeanMort2650 <- data.frame(apply(simMort2650,2,mean))
PIMort2650 <- t(data.frame(apply(simMort2650,2,PI,prob=0.89)))
F5MortSim2650 <- data.frame(cbind(simmeanMort2650,PIMort2650))
F5MortSim2650$PlotCN <- all2650cc5  $PlotCN
F5MortSim2650 <- F5MortSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2650) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4550 <- link(MortSplit5_beta, data=all4550cc5 )
simmeanMort4550 <- data.frame(apply(simMort4550,2,mean))
PIMort4550 <- t(data.frame(apply(simMort4550,2,PI,prob=0.89)))
F5MortSim4550 <- data.frame(cbind(simmeanMort4550,PIMort4550))
F5MortSim4550$PlotCN <- all4550cc5  $PlotCN
F5MortSim4550 <- F5MortSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4550) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8550 <- link(MortSplit5_beta, data=all8550cc5 )
simmeanMort8550 <- data.frame(apply(simMort8550,2,mean))
PIMort8550 <- t(data.frame(apply(simMort8550,2,PI,prob=0.89)))
F5MortSim8550 <- data.frame(cbind(simmeanMort8550,PIMort8550))
F5MortSim8550$PlotCN <- all8550cc5  $PlotCN
F5MortSim8550 <- F5MortSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8550) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2655 <- link(MortSplit5_beta, data=all2655cc5 )
simmeanMort2655 <- data.frame(apply(simMort2655,2,mean))
PIMort2655 <- t(data.frame(apply(simMort2655,2,PI,prob=0.89)))
F5MortSim2655 <- data.frame(cbind(simmeanMort2655,PIMort2655))
F5MortSim2655$PlotCN <- all2655cc5  $PlotCN
F5MortSim2655 <- F5MortSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2655) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4555 <- link(MortSplit5_beta, data=all4555cc5 )
simmeanMort4555 <- data.frame(apply(simMort4555,2,mean))
PIMort4555 <- t(data.frame(apply(simMort4555,2,PI,prob=0.89)))
F5MortSim4555 <- data.frame(cbind(simmeanMort4555,PIMort4555))
F5MortSim4555$PlotCN <- all4555cc5  $PlotCN
F5MortSim4555 <- F5MortSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4555) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8555 <- link(MortSplit5_beta, data=all8555cc5 )
simmeanMort8555 <- data.frame(apply(simMort8555,2,mean))
PIMort8555 <- t(data.frame(apply(simMort8555,2,PI,prob=0.89)))
F5MortSim8555 <- data.frame(cbind(simmeanMort8555,PIMort8555))
F5MortSim8555$PlotCN <- all8555cc5  $PlotCN
F5MortSim8555 <- F5MortSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8555) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2660 <- link(MortSplit5_beta, data=all2660cc5 )
simmeanMort2660 <- data.frame(apply(simMort2660,2,mean))
PIMort2660 <- t(data.frame(apply(simMort2660,2,PI,prob=0.89)))
F5MortSim2660 <- data.frame(cbind(simmeanMort2660,PIMort2660))
F5MortSim2660$PlotCN <- all2660cc5  $PlotCN
F5MortSim2660 <- F5MortSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2660) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4560 <- link(MortSplit5_beta, data=all4560cc5 )
simmeanMort4560 <- data.frame(apply(simMort4560,2,mean))
PIMort4560 <- t(data.frame(apply(simMort4560,2,PI,prob=0.89)))
F5MortSim4560 <- data.frame(cbind(simmeanMort4560,PIMort4560))
F5MortSim4560$PlotCN <- all4560cc5  $PlotCN
F5MortSim4560 <- F5MortSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4560) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8560 <- link(MortSplit5_beta, data=all8560cc5 )
simmeanMort8560 <- data.frame(apply(simMort8560,2,mean))
PIMort8560 <- t(data.frame(apply(simMort8560,2,PI,prob=0.89)))
F5MortSim8560 <- data.frame(cbind(simmeanMort8560,PIMort8560))
F5MortSim8560$PlotCN <- all8560cc5  $PlotCN
F5MortSim8560 <- F5MortSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8560) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2665 <- link(MortSplit5_beta, data=all2665cc5 )
simmeanMort2665 <- data.frame(apply(simMort2665,2,mean))
PIMort2665 <- t(data.frame(apply(simMort2665,2,PI,prob=0.89)))
F5MortSim2665 <- data.frame(cbind(simmeanMort2665,PIMort2665))
F5MortSim2665$PlotCN <- all2665cc5  $PlotCN
F5MortSim2665 <- F5MortSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2665) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4565 <- link(MortSplit5_beta, data=all4565cc5 )
simmeanMort4565 <- data.frame(apply(simMort4565,2,mean))
PIMort4565 <- t(data.frame(apply(simMort4565,2,PI,prob=0.89)))
F5MortSim4565 <- data.frame(cbind(simmeanMort4565,PIMort4565))
F5MortSim4565$PlotCN <- all4565cc5  $PlotCN
F5MortSim4565 <- F5MortSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4565) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8565 <- link(MortSplit5_beta, data=all8565cc5 )
simmeanMort8565 <- data.frame(apply(simMort8565,2,mean))
PIMort8565 <- t(data.frame(apply(simMort8565,2,PI,prob=0.89)))
F5MortSim8565 <- data.frame(cbind(simmeanMort8565,PIMort8565))
F5MortSim8565$PlotCN <- all8565cc5  $PlotCN
F5MortSim8565 <- F5MortSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8565) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2670 <- link(MortSplit5_beta, data=all2670cc5 )
simmeanMort2670 <- data.frame(apply(simMort2670,2,mean))
PIMort2670 <- t(data.frame(apply(simMort2670,2,PI,prob=0.89)))
F5MortSim2670 <- data.frame(cbind(simmeanMort2670,PIMort2670))
F5MortSim2670$PlotCN <- all2670cc5  $PlotCN
F5MortSim2670 <- F5MortSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2670) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4570 <- link(MortSplit5_beta, data=all4570cc5 )
simmeanMort4570 <- data.frame(apply(simMort4570,2,mean))
PIMort4570 <- t(data.frame(apply(simMort4570,2,PI,prob=0.89)))
F5MortSim4570 <- data.frame(cbind(simmeanMort4570,PIMort4570))
F5MortSim4570$PlotCN <- all4570cc5  $PlotCN
F5MortSim4570 <- F5MortSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4570) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8570 <- link(MortSplit5_beta, data=all8570cc5 )
simmeanMort8570 <- data.frame(apply(simMort8570,2,mean))
PIMort8570 <- t(data.frame(apply(simMort8570,2,PI,prob=0.89)))
F5MortSim8570 <- data.frame(cbind(simmeanMort8570,PIMort8570))
F5MortSim8570$PlotCN <- all8570cc5  $PlotCN
F5MortSim8570 <- F5MortSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8570) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2675 <- link(MortSplit5_beta, data=all2675cc5 )
simmeanMort2675 <- data.frame(apply(simMort2675,2,mean))
PIMort2675 <- t(data.frame(apply(simMort2675,2,PI,prob=0.89)))
F5MortSim2675 <- data.frame(cbind(simmeanMort2675,PIMort2675))
F5MortSim2675$PlotCN <- all2675cc5  $PlotCN
F5MortSim2675 <- F5MortSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2675) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4575 <- link(MortSplit5_beta, data=all4575cc5 )
simmeanMort4575 <- data.frame(apply(simMort4575,2,mean))
PIMort4575 <- t(data.frame(apply(simMort4575,2,PI,prob=0.89)))
F5MortSim4575 <- data.frame(cbind(simmeanMort4575,PIMort4575))
F5MortSim4575$PlotCN <- all4575cc5  $PlotCN
F5MortSim4575 <- F5MortSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4575) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8575 <- link(MortSplit5_beta, data=all8575cc5 )
simmeanMort8575 <- data.frame(apply(simMort8575,2,mean))
PIMort8575 <- t(data.frame(apply(simMort8575,2,PI,prob=0.89)))
F5MortSim8575 <- data.frame(cbind(simmeanMort8575,PIMort8575))
F5MortSim8575$PlotCN <- all8575cc5  $PlotCN
F5MortSim8575 <- F5MortSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8575) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2680 <- link(MortSplit5_beta, data=all2680cc5 )
simmeanMort2680 <- data.frame(apply(simMort2680,2,mean))
PIMort2680 <- t(data.frame(apply(simMort2680,2,PI,prob=0.89)))
F5MortSim2680 <- data.frame(cbind(simmeanMort2680,PIMort2680))
F5MortSim2680$PlotCN <- all2680cc5  $PlotCN
F5MortSim2680 <- F5MortSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim2680) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4580 <- link(MortSplit5_beta, data=all4580cc5 )
simmeanMort4580 <- data.frame(apply(simMort4580,2,mean))
PIMort4580 <- t(data.frame(apply(simMort4580,2,PI,prob=0.89)))
F5MortSim4580 <- data.frame(cbind(simmeanMort4580,PIMort4580))
F5MortSim4580$PlotCN <- all4580cc5  $PlotCN
F5MortSim4580 <- F5MortSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim4580) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8580 <- link(MortSplit5_beta, data=all8580cc5 )
simmeanMort8580 <- data.frame(apply(simMort8580,2,mean))
PIMort8580 <- t(data.frame(apply(simMort8580,2,PI,prob=0.89)))
F5MortSim8580 <- data.frame(cbind(simmeanMort8580,PIMort8580))
F5MortSim8580$PlotCN <- all8580cc5  $PlotCN
F5MortSim8580 <- F5MortSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F5MortSim8580) <- c("PlotCN","MortPerAcre","5CI","95CI")

#group data by RCP pathway
F5MortSim26B <- F5MortSim2625 %>% cbind(F5MortSim2630[,2:4]) %>%
  cbind(F5MortSim2635[,2:4]) %>% cbind(F5MortSim2640[,2:4]) %>% cbind(F5MortSim2645[,2:4]) %>%
  cbind(F5MortSim2650[,2:4]) %>% cbind(F5MortSim2655[,2:4]) %>% cbind(F5MortSim2660[,2:4]) %>%
  cbind(F5MortSim2665[,2:4]) %>% cbind(F5MortSim2670[,2:4]) %>% cbind(F5MortSim2675[,2:4]) %>%
  cbind(F5MortSim2680[,2:4])
colnames(F5MortSim26B) <- MortSimnames
write.csv(F5MortSim26B,file="F5MortPred26B.csv")

F5MortSim45B <- F5MortSim4525 %>% cbind(F5MortSim4530[,2:4]) %>%
  cbind(F5MortSim4535[,2:4]) %>% cbind(F5MortSim4540[,2:4]) %>% cbind(F5MortSim4545[,2:4]) %>%
  cbind(F5MortSim4550[,2:4]) %>% cbind(F5MortSim4555[,2:4]) %>% cbind(F5MortSim4560[,2:4]) %>%
  cbind(F5MortSim4565[,2:4]) %>% cbind(F5MortSim4570[,2:4]) %>% cbind(F5MortSim4575[,2:4]) %>%
  cbind(F5MortSim4580[,2:4])
colnames(F5MortSim45B) <- MortSimnames
write.csv(F5MortSim45B,file="F5MortPred45B.csv")

F5MortSim85B <- F5MortSim8525 %>% cbind(F5MortSim8530[,2:4]) %>%
  cbind(F5MortSim8535[,2:4]) %>% cbind(F5MortSim8540[,2:4]) %>% cbind(F5MortSim8545[,2:4]) %>%
  cbind(F5MortSim8550[,2:4]) %>% cbind(F5MortSim8555[,2:4]) %>% cbind(F5MortSim8560[,2:4]) %>%
  cbind(F5MortSim8565[,2:4]) %>% cbind(F5MortSim8570[,2:4]) %>% cbind(F5MortSim8575[,2:4]) %>%
  cbind(F5MortSim8580[,2:4])
colnames(F5MortSim85B) <- MortSimnames
write.csv(F5MortSim85B,file="F5MortPred85B.csv")


#fgroup 20
simMort2625 <- link(MortSplit20_beta, data=all2625cc20 )
simmeanMort2625 <- data.frame(apply(simMort2625,2,mean))
PIMort2625 <- t(data.frame(apply(simMort2625,2,PI,prob=0.89)))
F20MortSim2625 <- data.frame(cbind(simmeanMort2625,PIMort2625))
F20MortSim2625$PlotCN <- all2625cc20  $PlotCN
F20MortSim2625 <- F20MortSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2625) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4525 <- link(MortSplit20_beta, data=all4525cc20 )
simmeanMort4525 <- data.frame(apply(simMort4525,2,mean))
PIMort4525 <- t(data.frame(apply(simMort4525,2,PI,prob=0.89)))
F20MortSim4525 <- data.frame(cbind(simmeanMort4525,PIMort4525))
F20MortSim4525$PlotCN <- all4525cc20  $PlotCN
F20MortSim4525 <- F20MortSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4525) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8525 <- link(MortSplit20_beta, data=all8525cc20 )
simmeanMort8525 <- data.frame(apply(simMort8525,2,mean))
PIMort8525 <- t(data.frame(apply(simMort8525,2,PI,prob=0.89)))
F20MortSim8525 <- data.frame(cbind(simmeanMort8525,PIMort8525))
F20MortSim8525$PlotCN <- all8525cc20  $PlotCN
F20MortSim8525 <- F20MortSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8525) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2630 <- link(MortSplit20_beta, data=all2630cc20 )
simmeanMort2630 <- data.frame(apply(simMort2630,2,mean))
PIMort2630 <- t(data.frame(apply(simMort2630,2,PI,prob=0.89)))
F20MortSim2630 <- data.frame(cbind(simmeanMort2630,PIMort2630))
F20MortSim2630$PlotCN <- all2630cc20  $PlotCN
F20MortSim2630 <- F20MortSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2630) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4530 <- link(MortSplit20_beta, data=all4530cc20 )
simmeanMort4530 <- data.frame(apply(simMort4530,2,mean))
PIMort4530 <- t(data.frame(apply(simMort4530,2,PI,prob=0.89)))
F20MortSim4530 <- data.frame(cbind(simmeanMort4530,PIMort4530))
F20MortSim4530$PlotCN <- all4530cc20  $PlotCN
F20MortSim4530 <- F20MortSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4530) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8530 <- link(MortSplit20_beta, data=all8530cc20 )
simmeanMort8530 <- data.frame(apply(simMort8530,2,mean))
PIMort8530 <- t(data.frame(apply(simMort8530,2,PI,prob=0.89)))
F20MortSim8530 <- data.frame(cbind(simmeanMort8530,PIMort8530))
F20MortSim8530$PlotCN <- all8530cc20  $PlotCN
F20MortSim8530 <- F20MortSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8530) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2635 <- link(MortSplit20_beta, data=all2635cc20 )
simmeanMort2635 <- data.frame(apply(simMort2635,2,mean))
PIMort2635 <- t(data.frame(apply(simMort2635,2,PI,prob=0.89)))
F20MortSim2635 <- data.frame(cbind(simmeanMort2635,PIMort2635))
F20MortSim2635$PlotCN <- all2635cc20  $PlotCN
F20MortSim2635 <- F20MortSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2635) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4535 <- link(MortSplit20_beta, data=all4535cc20 )
simmeanMort4535 <- data.frame(apply(simMort4535,2,mean))
PIMort4535 <- t(data.frame(apply(simMort4535,2,PI,prob=0.89)))
F20MortSim4535 <- data.frame(cbind(simmeanMort4535,PIMort4535))
F20MortSim4535$PlotCN <- all4535cc20  $PlotCN
F20MortSim4535 <- F20MortSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4535) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8535 <- link(MortSplit20_beta, data=all8535cc20 )
simmeanMort8535 <- data.frame(apply(simMort8535,2,mean))
PIMort8535 <- t(data.frame(apply(simMort8535,2,PI,prob=0.89)))
F20MortSim8535 <- data.frame(cbind(simmeanMort8535,PIMort8535))
F20MortSim8535$PlotCN <- all8535cc20  $PlotCN
F20MortSim8535 <- F20MortSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8535) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2640 <- link(MortSplit20_beta, data=all2640cc20 )
simmeanMort2640 <- data.frame(apply(simMort2640,2,mean))
PIMort2640 <- t(data.frame(apply(simMort2640,2,PI,prob=0.89)))
F20MortSim2640 <- data.frame(cbind(simmeanMort2640,PIMort2640))
F20MortSim2640$PlotCN <- all2640cc20  $PlotCN
F20MortSim2640 <- F20MortSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2640) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4540 <- link(MortSplit20_beta, data=all4540cc20 )
simmeanMort4540 <- data.frame(apply(simMort4540,2,mean))
PIMort4540 <- t(data.frame(apply(simMort4540,2,PI,prob=0.89)))
F20MortSim4540 <- data.frame(cbind(simmeanMort4540,PIMort4540))
F20MortSim4540$PlotCN <- all4540cc20  $PlotCN
F20MortSim4540 <- F20MortSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4540) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8540 <- link(MortSplit20_beta, data=all8540cc20 )
simmeanMort8540 <- data.frame(apply(simMort8540,2,mean))
PIMort8540 <- t(data.frame(apply(simMort8540,2,PI,prob=0.89)))
F20MortSim8540 <- data.frame(cbind(simmeanMort8540,PIMort8540))
F20MortSim8540$PlotCN <- all8540cc20  $PlotCN
F20MortSim8540 <- F20MortSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8540) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2645 <- link(MortSplit20_beta, data=all2645cc20 )
simmeanMort2645 <- data.frame(apply(simMort2645,2,mean))
PIMort2645 <- t(data.frame(apply(simMort2645,2,PI,prob=0.89)))
F20MortSim2645 <- data.frame(cbind(simmeanMort2645,PIMort2645))
F20MortSim2645$PlotCN <- all2645cc20  $PlotCN
F20MortSim2645 <- F20MortSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2645) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4545 <- link(MortSplit20_beta, data=all4545cc20 )
simmeanMort4545 <- data.frame(apply(simMort4545,2,mean))
PIMort4545 <- t(data.frame(apply(simMort4545,2,PI,prob=0.89)))
F20MortSim4545 <- data.frame(cbind(simmeanMort4545,PIMort4545))
F20MortSim4545$PlotCN <- all4545cc20  $PlotCN
F20MortSim4545 <- F20MortSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4545) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8545 <- link(MortSplit20_beta, data=all8545cc20 )
simmeanMort8545 <- data.frame(apply(simMort8545,2,mean))
PIMort8545 <- t(data.frame(apply(simMort8545,2,PI,prob=0.89)))
F20MortSim8545 <- data.frame(cbind(simmeanMort8545,PIMort8545))
F20MortSim8545$PlotCN <- all8545cc20  $PlotCN
F20MortSim8545 <- F20MortSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8545) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2650 <- link(MortSplit20_beta, data=all2650cc20 )
simmeanMort2650 <- data.frame(apply(simMort2650,2,mean))
PIMort2650 <- t(data.frame(apply(simMort2650,2,PI,prob=0.89)))
F20MortSim2650 <- data.frame(cbind(simmeanMort2650,PIMort2650))
F20MortSim2650$PlotCN <- all2650cc20  $PlotCN
F20MortSim2650 <- F20MortSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2650) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4550 <- link(MortSplit20_beta, data=all4550cc20 )
simmeanMort4550 <- data.frame(apply(simMort4550,2,mean))
PIMort4550 <- t(data.frame(apply(simMort4550,2,PI,prob=0.89)))
F20MortSim4550 <- data.frame(cbind(simmeanMort4550,PIMort4550))
F20MortSim4550$PlotCN <- all4550cc20  $PlotCN
F20MortSim4550 <- F20MortSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4550) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8550 <- link(MortSplit20_beta, data=all8550cc20 )
simmeanMort8550 <- data.frame(apply(simMort8550,2,mean))
PIMort8550 <- t(data.frame(apply(simMort8550,2,PI,prob=0.89)))
F20MortSim8550 <- data.frame(cbind(simmeanMort8550,PIMort8550))
F20MortSim8550$PlotCN <- all8550cc20  $PlotCN
F20MortSim8550 <- F20MortSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8550) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2655 <- link(MortSplit20_beta, data=all2655cc20 )
simmeanMort2655 <- data.frame(apply(simMort2655,2,mean))
PIMort2655 <- t(data.frame(apply(simMort2655,2,PI,prob=0.89)))
F20MortSim2655 <- data.frame(cbind(simmeanMort2655,PIMort2655))
F20MortSim2655$PlotCN <- all2655cc20  $PlotCN
F20MortSim2655 <- F20MortSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2655) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4555 <- link(MortSplit20_beta, data=all4555cc20 )
simmeanMort4555 <- data.frame(apply(simMort4555,2,mean))
PIMort4555 <- t(data.frame(apply(simMort4555,2,PI,prob=0.89)))
F20MortSim4555 <- data.frame(cbind(simmeanMort4555,PIMort4555))
F20MortSim4555$PlotCN <- all4555cc20  $PlotCN
F20MortSim4555 <- F20MortSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4555) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8555 <- link(MortSplit20_beta, data=all8555cc20 )
simmeanMort8555 <- data.frame(apply(simMort8555,2,mean))
PIMort8555 <- t(data.frame(apply(simMort8555,2,PI,prob=0.89)))
F20MortSim8555 <- data.frame(cbind(simmeanMort8555,PIMort8555))
F20MortSim8555$PlotCN <- all8555cc20  $PlotCN
F20MortSim8555 <- F20MortSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8555) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2660 <- link(MortSplit20_beta, data=all2660cc20 )
simmeanMort2660 <- data.frame(apply(simMort2660,2,mean))
PIMort2660 <- t(data.frame(apply(simMort2660,2,PI,prob=0.89)))
F20MortSim2660 <- data.frame(cbind(simmeanMort2660,PIMort2660))
F20MortSim2660$PlotCN <- all2660cc20  $PlotCN
F20MortSim2660 <- F20MortSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2660) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4560 <- link(MortSplit20_beta, data=all4560cc20 )
simmeanMort4560 <- data.frame(apply(simMort4560,2,mean))
PIMort4560 <- t(data.frame(apply(simMort4560,2,PI,prob=0.89)))
F20MortSim4560 <- data.frame(cbind(simmeanMort4560,PIMort4560))
F20MortSim4560$PlotCN <- all4560cc20  $PlotCN
F20MortSim4560 <- F20MortSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4560) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8560 <- link(MortSplit20_beta, data=all8560cc20 )
simmeanMort8560 <- data.frame(apply(simMort8560,2,mean))
PIMort8560 <- t(data.frame(apply(simMort8560,2,PI,prob=0.89)))
F20MortSim8560 <- data.frame(cbind(simmeanMort8560,PIMort8560))
F20MortSim8560$PlotCN <- all8560cc20  $PlotCN
F20MortSim8560 <- F20MortSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8560) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2665 <- link(MortSplit20_beta, data=all2665cc20 )
simmeanMort2665 <- data.frame(apply(simMort2665,2,mean))
PIMort2665 <- t(data.frame(apply(simMort2665,2,PI,prob=0.89)))
F20MortSim2665 <- data.frame(cbind(simmeanMort2665,PIMort2665))
F20MortSim2665$PlotCN <- all2665cc20  $PlotCN
F20MortSim2665 <- F20MortSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2665) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4565 <- link(MortSplit20_beta, data=all4565cc20 )
simmeanMort4565 <- data.frame(apply(simMort4565,2,mean))
PIMort4565 <- t(data.frame(apply(simMort4565,2,PI,prob=0.89)))
F20MortSim4565 <- data.frame(cbind(simmeanMort4565,PIMort4565))
F20MortSim4565$PlotCN <- all4565cc20  $PlotCN
F20MortSim4565 <- F20MortSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4565) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8565 <- link(MortSplit20_beta, data=all8565cc20 )
simmeanMort8565 <- data.frame(apply(simMort8565,2,mean))
PIMort8565 <- t(data.frame(apply(simMort8565,2,PI,prob=0.89)))
F20MortSim8565 <- data.frame(cbind(simmeanMort8565,PIMort8565))
F20MortSim8565$PlotCN <- all8565cc20  $PlotCN
F20MortSim8565 <- F20MortSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8565) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2670 <- link(MortSplit20_beta, data=all2670cc20 )
simmeanMort2670 <- data.frame(apply(simMort2670,2,mean))
PIMort2670 <- t(data.frame(apply(simMort2670,2,PI,prob=0.89)))
F20MortSim2670 <- data.frame(cbind(simmeanMort2670,PIMort2670))
F20MortSim2670$PlotCN <- all2670cc20  $PlotCN
F20MortSim2670 <- F20MortSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2670) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4570 <- link(MortSplit20_beta, data=all4570cc20 )
simmeanMort4570 <- data.frame(apply(simMort4570,2,mean))
PIMort4570 <- t(data.frame(apply(simMort4570,2,PI,prob=0.89)))
F20MortSim4570 <- data.frame(cbind(simmeanMort4570,PIMort4570))
F20MortSim4570$PlotCN <- all4570cc20  $PlotCN
F20MortSim4570 <- F20MortSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4570) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8570 <- link(MortSplit20_beta, data=all8570cc20 )
simmeanMort8570 <- data.frame(apply(simMort8570,2,mean))
PIMort8570 <- t(data.frame(apply(simMort8570,2,PI,prob=0.89)))
F20MortSim8570 <- data.frame(cbind(simmeanMort8570,PIMort8570))
F20MortSim8570$PlotCN <- all8570cc20  $PlotCN
F20MortSim8570 <- F20MortSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8570) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2675 <- link(MortSplit20_beta, data=all2675cc20 )
simmeanMort2675 <- data.frame(apply(simMort2675,2,mean))
PIMort2675 <- t(data.frame(apply(simMort2675,2,PI,prob=0.89)))
F20MortSim2675 <- data.frame(cbind(simmeanMort2675,PIMort2675))
F20MortSim2675$PlotCN <- all2675cc20  $PlotCN
F20MortSim2675 <- F20MortSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2675) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4575 <- link(MortSplit20_beta, data=all4575cc20 )
simmeanMort4575 <- data.frame(apply(simMort4575,2,mean))
PIMort4575 <- t(data.frame(apply(simMort4575,2,PI,prob=0.89)))
F20MortSim4575 <- data.frame(cbind(simmeanMort4575,PIMort4575))
F20MortSim4575$PlotCN <- all4575cc20  $PlotCN
F20MortSim4575 <- F20MortSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4575) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8575 <- link(MortSplit20_beta, data=all8575cc20 )
simmeanMort8575 <- data.frame(apply(simMort8575,2,mean))
PIMort8575 <- t(data.frame(apply(simMort8575,2,PI,prob=0.89)))
F20MortSim8575 <- data.frame(cbind(simmeanMort8575,PIMort8575))
F20MortSim8575$PlotCN <- all8575cc20  $PlotCN
F20MortSim8575 <- F20MortSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8575) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2680 <- link(MortSplit20_beta, data=all2680cc20 )
simmeanMort2680 <- data.frame(apply(simMort2680,2,mean))
PIMort2680 <- t(data.frame(apply(simMort2680,2,PI,prob=0.89)))
F20MortSim2680 <- data.frame(cbind(simmeanMort2680,PIMort2680))
F20MortSim2680$PlotCN <- all2680cc20  $PlotCN
F20MortSim2680 <- F20MortSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim2680) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4580 <- link(MortSplit20_beta, data=all4580cc20 )
simmeanMort4580 <- data.frame(apply(simMort4580,2,mean))
PIMort4580 <- t(data.frame(apply(simMort4580,2,PI,prob=0.89)))
F20MortSim4580 <- data.frame(cbind(simmeanMort4580,PIMort4580))
F20MortSim4580$PlotCN <- all4580cc20  $PlotCN
F20MortSim4580 <- F20MortSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim4580) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8580 <- link(MortSplit20_beta, data=all8580cc20 )
simmeanMort8580 <- data.frame(apply(simMort8580,2,mean))
PIMort8580 <- t(data.frame(apply(simMort8580,2,PI,prob=0.89)))
F20MortSim8580 <- data.frame(cbind(simmeanMort8580,PIMort8580))
F20MortSim8580$PlotCN <- all8580cc20  $PlotCN
F20MortSim8580 <- F20MortSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F20MortSim8580) <- c("PlotCN","MortPerAcre","5CI","95CI")


#group data by RCP pathway
F20MortSim26B <- F20MortSim2625 %>% cbind(F20MortSim2630[,2:4]) %>%
  cbind(F20MortSim2635[,2:4]) %>% cbind(F20MortSim2640[,2:4]) %>% cbind(F20MortSim2645[,2:4]) %>%
  cbind(F20MortSim2650[,2:4]) %>% cbind(F20MortSim2655[,2:4]) %>% cbind(F20MortSim2660[,2:4]) %>%
  cbind(F20MortSim2665[,2:4]) %>% cbind(F20MortSim2670[,2:4]) %>% cbind(F20MortSim2675[,2:4]) %>%
  cbind(F20MortSim2680[,2:4])
colnames(F20MortSim26B) <- MortSimnames
write.csv(F20MortSim26B,file="F20MortPred26B.csv")

F20MortSim45B <- F20MortSim4525 %>% cbind(F20MortSim4530[,2:4]) %>%
  cbind(F20MortSim4535[,2:4]) %>% cbind(F20MortSim4540[,2:4]) %>% cbind(F20MortSim4545[,2:4]) %>%
  cbind(F20MortSim4550[,2:4]) %>% cbind(F20MortSim4555[,2:4]) %>% cbind(F20MortSim4560[,2:4]) %>%
  cbind(F20MortSim4565[,2:4]) %>% cbind(F20MortSim4570[,2:4]) %>% cbind(F20MortSim4575[,2:4]) %>%
  cbind(F20MortSim4580[,2:4])
colnames(F20MortSim45B) <- MortSimnames
write.csv(F20MortSim45B,file="F20MortPred45B.csv")

F20MortSim85B <- F20MortSim8525 %>% cbind(F20MortSim8530[,2:4]) %>%
  cbind(F20MortSim8535[,2:4]) %>% cbind(F20MortSim8540[,2:4]) %>% cbind(F20MortSim8545[,2:4]) %>%
  cbind(F20MortSim8550[,2:4]) %>% cbind(F20MortSim8555[,2:4]) %>% cbind(F20MortSim8560[,2:4]) %>%
  cbind(F20MortSim8565[,2:4]) %>% cbind(F20MortSim8570[,2:4]) %>% cbind(F20MortSim8575[,2:4]) %>%
  cbind(F20MortSim8580[,2:4])
colnames(F20MortSim85B) <- MortSimnames
write.csv(F20MortSim85B,file="F20MortPred85B.csv")


#fgroup 21
simMort2625 <- link(MortSplit21_beta, data=all2625cc21 )
simmeanMort2625 <- data.frame(apply(simMort2625,2,mean))
PIMort2625 <- t(data.frame(apply(simMort2625,2,PI,prob=0.89)))
F21MortSim2625 <- data.frame(cbind(simmeanMort2625,PIMort2625))
F21MortSim2625$PlotCN <- all2625cc21  $PlotCN
F21MortSim2625 <- F21MortSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2625) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4525 <- link(MortSplit21_beta, data=all4525cc21 )
simmeanMort4525 <- data.frame(apply(simMort4525,2,mean))
PIMort4525 <- t(data.frame(apply(simMort4525,2,PI,prob=0.89)))
F21MortSim4525 <- data.frame(cbind(simmeanMort4525,PIMort4525))
F21MortSim4525$PlotCN <- all4525cc21  $PlotCN
F21MortSim4525 <- F21MortSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4525) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8525 <- link(MortSplit21_beta, data=all8525cc21 )
simmeanMort8525 <- data.frame(apply(simMort8525,2,mean))
PIMort8525 <- t(data.frame(apply(simMort8525,2,PI,prob=0.89)))
F21MortSim8525 <- data.frame(cbind(simmeanMort8525,PIMort8525))
F21MortSim8525$PlotCN <- all8525cc21  $PlotCN
F21MortSim8525 <- F21MortSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8525) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2630 <- link(MortSplit21_beta, data=all2630cc21 )
simmeanMort2630 <- data.frame(apply(simMort2630,2,mean))
PIMort2630 <- t(data.frame(apply(simMort2630,2,PI,prob=0.89)))
F21MortSim2630 <- data.frame(cbind(simmeanMort2630,PIMort2630))
F21MortSim2630$PlotCN <- all2630cc21  $PlotCN
F21MortSim2630 <- F21MortSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2630) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4530 <- link(MortSplit21_beta, data=all4530cc21 )
simmeanMort4530 <- data.frame(apply(simMort4530,2,mean))
PIMort4530 <- t(data.frame(apply(simMort4530,2,PI,prob=0.89)))
F21MortSim4530 <- data.frame(cbind(simmeanMort4530,PIMort4530))
F21MortSim4530$PlotCN <- all4530cc21  $PlotCN
F21MortSim4530 <- F21MortSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4530) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8530 <- link(MortSplit21_beta, data=all8530cc21 )
simmeanMort8530 <- data.frame(apply(simMort8530,2,mean))
PIMort8530 <- t(data.frame(apply(simMort8530,2,PI,prob=0.89)))
F21MortSim8530 <- data.frame(cbind(simmeanMort8530,PIMort8530))
F21MortSim8530$PlotCN <- all8530cc21  $PlotCN
F21MortSim8530 <- F21MortSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8530) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2635 <- link(MortSplit21_beta, data=all2635cc21 )
simmeanMort2635 <- data.frame(apply(simMort2635,2,mean))
PIMort2635 <- t(data.frame(apply(simMort2635,2,PI,prob=0.89)))
F21MortSim2635 <- data.frame(cbind(simmeanMort2635,PIMort2635))
F21MortSim2635$PlotCN <- all2635cc21  $PlotCN
F21MortSim2635 <- F21MortSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2635) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4535 <- link(MortSplit21_beta, data=all4535cc21 )
simmeanMort4535 <- data.frame(apply(simMort4535,2,mean))
PIMort4535 <- t(data.frame(apply(simMort4535,2,PI,prob=0.89)))
F21MortSim4535 <- data.frame(cbind(simmeanMort4535,PIMort4535))
F21MortSim4535$PlotCN <- all4535cc21  $PlotCN
F21MortSim4535 <- F21MortSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4535) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8535 <- link(MortSplit21_beta, data=all8535cc21 )
simmeanMort8535 <- data.frame(apply(simMort8535,2,mean))
PIMort8535 <- t(data.frame(apply(simMort8535,2,PI,prob=0.89)))
F21MortSim8535 <- data.frame(cbind(simmeanMort8535,PIMort8535))
F21MortSim8535$PlotCN <- all8535cc21  $PlotCN
F21MortSim8535 <- F21MortSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8535) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2640 <- link(MortSplit21_beta, data=all2640cc21 )
simmeanMort2640 <- data.frame(apply(simMort2640,2,mean))
PIMort2640 <- t(data.frame(apply(simMort2640,2,PI,prob=0.89)))
F21MortSim2640 <- data.frame(cbind(simmeanMort2640,PIMort2640))
F21MortSim2640$PlotCN <- all2640cc21  $PlotCN
F21MortSim2640 <- F21MortSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2640) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4540 <- link(MortSplit21_beta, data=all4540cc21 )
simmeanMort4540 <- data.frame(apply(simMort4540,2,mean))
PIMort4540 <- t(data.frame(apply(simMort4540,2,PI,prob=0.89)))
F21MortSim4540 <- data.frame(cbind(simmeanMort4540,PIMort4540))
F21MortSim4540$PlotCN <- all4540cc21  $PlotCN
F21MortSim4540 <- F21MortSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4540) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8540 <- link(MortSplit21_beta, data=all8540cc21 )
simmeanMort8540 <- data.frame(apply(simMort8540,2,mean))
PIMort8540 <- t(data.frame(apply(simMort8540,2,PI,prob=0.89)))
F21MortSim8540 <- data.frame(cbind(simmeanMort8540,PIMort8540))
F21MortSim8540$PlotCN <- all8540cc21  $PlotCN
F21MortSim8540 <- F21MortSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8540) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2645 <- link(MortSplit21_beta, data=all2645cc21 )
simmeanMort2645 <- data.frame(apply(simMort2645,2,mean))
PIMort2645 <- t(data.frame(apply(simMort2645,2,PI,prob=0.89)))
F21MortSim2645 <- data.frame(cbind(simmeanMort2645,PIMort2645))
F21MortSim2645$PlotCN <- all2645cc21  $PlotCN
F21MortSim2645 <- F21MortSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2645) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4545 <- link(MortSplit21_beta, data=all4545cc21 )
simmeanMort4545 <- data.frame(apply(simMort4545,2,mean))
PIMort4545 <- t(data.frame(apply(simMort4545,2,PI,prob=0.89)))
F21MortSim4545 <- data.frame(cbind(simmeanMort4545,PIMort4545))
F21MortSim4545$PlotCN <- all4545cc21  $PlotCN
F21MortSim4545 <- F21MortSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4545) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8545 <- link(MortSplit21_beta, data=all8545cc21 )
simmeanMort8545 <- data.frame(apply(simMort8545,2,mean))
PIMort8545 <- t(data.frame(apply(simMort8545,2,PI,prob=0.89)))
F21MortSim8545 <- data.frame(cbind(simmeanMort8545,PIMort8545))
F21MortSim8545$PlotCN <- all8545cc21  $PlotCN
F21MortSim8545 <- F21MortSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8545) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2650 <- link(MortSplit21_beta, data=all2650cc21 )
simmeanMort2650 <- data.frame(apply(simMort2650,2,mean))
PIMort2650 <- t(data.frame(apply(simMort2650,2,PI,prob=0.89)))
F21MortSim2650 <- data.frame(cbind(simmeanMort2650,PIMort2650))
F21MortSim2650$PlotCN <- all2650cc21  $PlotCN
F21MortSim2650 <- F21MortSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2650) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4550 <- link(MortSplit21_beta, data=all4550cc21 )
simmeanMort4550 <- data.frame(apply(simMort4550,2,mean))
PIMort4550 <- t(data.frame(apply(simMort4550,2,PI,prob=0.89)))
F21MortSim4550 <- data.frame(cbind(simmeanMort4550,PIMort4550))
F21MortSim4550$PlotCN <- all4550cc21  $PlotCN
F21MortSim4550 <- F21MortSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4550) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8550 <- link(MortSplit21_beta, data=all8550cc21 )
simmeanMort8550 <- data.frame(apply(simMort8550,2,mean))
PIMort8550 <- t(data.frame(apply(simMort8550,2,PI,prob=0.89)))
F21MortSim8550 <- data.frame(cbind(simmeanMort8550,PIMort8550))
F21MortSim8550$PlotCN <- all8550cc21  $PlotCN
F21MortSim8550 <- F21MortSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8550) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2655 <- link(MortSplit21_beta, data=all2655cc21 )
simmeanMort2655 <- data.frame(apply(simMort2655,2,mean))
PIMort2655 <- t(data.frame(apply(simMort2655,2,PI,prob=0.89)))
F21MortSim2655 <- data.frame(cbind(simmeanMort2655,PIMort2655))
F21MortSim2655$PlotCN <- all2655cc21  $PlotCN
F21MortSim2655 <- F21MortSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2655) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4555 <- link(MortSplit21_beta, data=all4555cc21 )
simmeanMort4555 <- data.frame(apply(simMort4555,2,mean))
PIMort4555 <- t(data.frame(apply(simMort4555,2,PI,prob=0.89)))
F21MortSim4555 <- data.frame(cbind(simmeanMort4555,PIMort4555))
F21MortSim4555$PlotCN <- all4555cc21  $PlotCN
F21MortSim4555 <- F21MortSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4555) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8555 <- link(MortSplit21_beta, data=all8555cc21 )
simmeanMort8555 <- data.frame(apply(simMort8555,2,mean))
PIMort8555 <- t(data.frame(apply(simMort8555,2,PI,prob=0.89)))
F21MortSim8555 <- data.frame(cbind(simmeanMort8555,PIMort8555))
F21MortSim8555$PlotCN <- all8555cc21  $PlotCN
F21MortSim8555 <- F21MortSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8555) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2660 <- link(MortSplit21_beta, data=all2660cc21 )
simmeanMort2660 <- data.frame(apply(simMort2660,2,mean))
PIMort2660 <- t(data.frame(apply(simMort2660,2,PI,prob=0.89)))
F21MortSim2660 <- data.frame(cbind(simmeanMort2660,PIMort2660))
F21MortSim2660$PlotCN <- all2660cc21  $PlotCN
F21MortSim2660 <- F21MortSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2660) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4560 <- link(MortSplit21_beta, data=all4560cc21 )
simmeanMort4560 <- data.frame(apply(simMort4560,2,mean))
PIMort4560 <- t(data.frame(apply(simMort4560,2,PI,prob=0.89)))
F21MortSim4560 <- data.frame(cbind(simmeanMort4560,PIMort4560))
F21MortSim4560$PlotCN <- all4560cc21  $PlotCN
F21MortSim4560 <- F21MortSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4560) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8560 <- link(MortSplit21_beta, data=all8560cc21 )
simmeanMort8560 <- data.frame(apply(simMort8560,2,mean))
PIMort8560 <- t(data.frame(apply(simMort8560,2,PI,prob=0.89)))
F21MortSim8560 <- data.frame(cbind(simmeanMort8560,PIMort8560))
F21MortSim8560$PlotCN <- all8560cc21  $PlotCN
F21MortSim8560 <- F21MortSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8560) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2665 <- link(MortSplit21_beta, data=all2665cc21 )
simmeanMort2665 <- data.frame(apply(simMort2665,2,mean))
PIMort2665 <- t(data.frame(apply(simMort2665,2,PI,prob=0.89)))
F21MortSim2665 <- data.frame(cbind(simmeanMort2665,PIMort2665))
F21MortSim2665$PlotCN <- all2665cc21  $PlotCN
F21MortSim2665 <- F21MortSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2665) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4565 <- link(MortSplit21_beta, data=all4565cc21 )
simmeanMort4565 <- data.frame(apply(simMort4565,2,mean))
PIMort4565 <- t(data.frame(apply(simMort4565,2,PI,prob=0.89)))
F21MortSim4565 <- data.frame(cbind(simmeanMort4565,PIMort4565))
F21MortSim4565$PlotCN <- all4565cc21  $PlotCN
F21MortSim4565 <- F21MortSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4565) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8565 <- link(MortSplit21_beta, data=all8565cc21 )
simmeanMort8565 <- data.frame(apply(simMort8565,2,mean))
PIMort8565 <- t(data.frame(apply(simMort8565,2,PI,prob=0.89)))
F21MortSim8565 <- data.frame(cbind(simmeanMort8565,PIMort8565))
F21MortSim8565$PlotCN <- all8565cc21  $PlotCN
F21MortSim8565 <- F21MortSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8565) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2670 <- link(MortSplit21_beta, data=all2670cc21 )
simmeanMort2670 <- data.frame(apply(simMort2670,2,mean))
PIMort2670 <- t(data.frame(apply(simMort2670,2,PI,prob=0.89)))
F21MortSim2670 <- data.frame(cbind(simmeanMort2670,PIMort2670))
F21MortSim2670$PlotCN <- all2670cc21  $PlotCN
F21MortSim2670 <- F21MortSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2670) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4570 <- link(MortSplit21_beta, data=all4570cc21 )
simmeanMort4570 <- data.frame(apply(simMort4570,2,mean))
PIMort4570 <- t(data.frame(apply(simMort4570,2,PI,prob=0.89)))
F21MortSim4570 <- data.frame(cbind(simmeanMort4570,PIMort4570))
F21MortSim4570$PlotCN <- all4570cc21  $PlotCN
F21MortSim4570 <- F21MortSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4570) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8570 <- link(MortSplit21_beta, data=all8570cc21 )
simmeanMort8570 <- data.frame(apply(simMort8570,2,mean))
PIMort8570 <- t(data.frame(apply(simMort8570,2,PI,prob=0.89)))
F21MortSim8570 <- data.frame(cbind(simmeanMort8570,PIMort8570))
F21MortSim8570$PlotCN <- all8570cc21  $PlotCN
F21MortSim8570 <- F21MortSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8570) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2675 <- link(MortSplit21_beta, data=all2675cc21 )
simmeanMort2675 <- data.frame(apply(simMort2675,2,mean))
PIMort2675 <- t(data.frame(apply(simMort2675,2,PI,prob=0.89)))
F21MortSim2675 <- data.frame(cbind(simmeanMort2675,PIMort2675))
F21MortSim2675$PlotCN <- all2675cc21  $PlotCN
F21MortSim2675 <- F21MortSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2675) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4575 <- link(MortSplit21_beta, data=all4575cc21 )
simmeanMort4575 <- data.frame(apply(simMort4575,2,mean))
PIMort4575 <- t(data.frame(apply(simMort4575,2,PI,prob=0.89)))
F21MortSim4575 <- data.frame(cbind(simmeanMort4575,PIMort4575))
F21MortSim4575$PlotCN <- all4575cc21  $PlotCN
F21MortSim4575 <- F21MortSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4575) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8575 <- link(MortSplit21_beta, data=all8575cc21 )
simmeanMort8575 <- data.frame(apply(simMort8575,2,mean))
PIMort8575 <- t(data.frame(apply(simMort8575,2,PI,prob=0.89)))
F21MortSim8575 <- data.frame(cbind(simmeanMort8575,PIMort8575))
F21MortSim8575$PlotCN <- all8575cc21  $PlotCN
F21MortSim8575 <- F21MortSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8575) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2680 <- link(MortSplit21_beta, data=all2680cc21 )
simmeanMort2680 <- data.frame(apply(simMort2680,2,mean))
PIMort2680 <- t(data.frame(apply(simMort2680,2,PI,prob=0.89)))
F21MortSim2680 <- data.frame(cbind(simmeanMort2680,PIMort2680))
F21MortSim2680$PlotCN <- all2680cc21  $PlotCN
F21MortSim2680 <- F21MortSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim2680) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4580 <- link(MortSplit21_beta, data=all4580cc21 )
simmeanMort4580 <- data.frame(apply(simMort4580,2,mean))
PIMort4580 <- t(data.frame(apply(simMort4580,2,PI,prob=0.89)))
F21MortSim4580 <- data.frame(cbind(simmeanMort4580,PIMort4580))
F21MortSim4580$PlotCN <- all4580cc21  $PlotCN
F21MortSim4580 <- F21MortSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim4580) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8580 <- link(MortSplit21_beta, data=all8580cc21 )
simmeanMort8580 <- data.frame(apply(simMort8580,2,mean))
PIMort8580 <- t(data.frame(apply(simMort8580,2,PI,prob=0.89)))
F21MortSim8580 <- data.frame(cbind(simmeanMort8580,PIMort8580))
F21MortSim8580$PlotCN <- all8580cc21  $PlotCN
F21MortSim8580 <- F21MortSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F21MortSim8580) <- c("PlotCN","MortPerAcre","5CI","95CI")


#create vector of new colnames
MortSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Mort", "5CI", "95CI"), x)))
MortSimnames<- append(MortSimnames,"PlotCN",after=0)

#group data by RCP pathway
F21MortSim26B <- F21MortSim2625 %>% cbind(F21MortSim2630[,2:4]) %>%
  cbind(F21MortSim2635[,2:4]) %>% cbind(F21MortSim2640[,2:4]) %>% cbind(F21MortSim2645[,2:4]) %>%
  cbind(F21MortSim2650[,2:4]) %>% cbind(F21MortSim2655[,2:4]) %>% cbind(F21MortSim2660[,2:4]) %>%
  cbind(F21MortSim2665[,2:4]) %>% cbind(F21MortSim2670[,2:4]) %>% cbind(F21MortSim2675[,2:4]) %>%
  cbind(F21MortSim2680[,2:4])
colnames(F21MortSim26B) <- MortSimnames
write.csv(F21MortSim26B,file="F21MortPred26B.csv")

F21MortSim45B <- F21MortSim4525 %>% cbind(F21MortSim4530[,2:4]) %>%
  cbind(F21MortSim4535[,2:4]) %>% cbind(F21MortSim4540[,2:4]) %>% cbind(F21MortSim4545[,2:4]) %>%
  cbind(F21MortSim4550[,2:4]) %>% cbind(F21MortSim4555[,2:4]) %>% cbind(F21MortSim4560[,2:4]) %>%
  cbind(F21MortSim4565[,2:4]) %>% cbind(F21MortSim4570[,2:4]) %>% cbind(F21MortSim4575[,2:4]) %>%
  cbind(F21MortSim4580[,2:4])
colnames(F21MortSim45B) <- MortSimnames
write.csv(F21MortSim45B,file="F21MortPred45B.csv")

F21MortSim85B <- F21MortSim8525 %>% cbind(F21MortSim8530[,2:4]) %>%
  cbind(F21MortSim8535[,2:4]) %>% cbind(F21MortSim8540[,2:4]) %>% cbind(F21MortSim8545[,2:4]) %>%
  cbind(F21MortSim8550[,2:4]) %>% cbind(F21MortSim8555[,2:4]) %>% cbind(F21MortSim8560[,2:4]) %>%
  cbind(F21MortSim8565[,2:4]) %>% cbind(F21MortSim8570[,2:4]) %>% cbind(F21MortSim8575[,2:4]) %>%
  cbind(F21MortSim8580[,2:4])
colnames(F21MortSim85B) <- MortSimnames
write.csv(F21MortSim85B,file="F21MortPred85B.csv")


#fgroup 23
simMort2625 <- link(MortSplit23_beta, data=all2625cc23 )
simmeanMort2625 <- data.frame(apply(simMort2625,2,mean))
PIMort2625 <- t(data.frame(apply(simMort2625,2,PI,prob=0.89)))
F23MortSim2625 <- data.frame(cbind(simmeanMort2625,PIMort2625))
F23MortSim2625$PlotCN <- all2625cc23  $PlotCN
F23MortSim2625 <- F23MortSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2625) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4525 <- link(MortSplit23_beta, data=all4525cc23 )
simmeanMort4525 <- data.frame(apply(simMort4525,2,mean))
PIMort4525 <- t(data.frame(apply(simMort4525,2,PI,prob=0.89)))
F23MortSim4525 <- data.frame(cbind(simmeanMort4525,PIMort4525))
F23MortSim4525$PlotCN <- all4525cc23  $PlotCN
F23MortSim4525 <- F23MortSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4525) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8525 <- link(MortSplit23_beta, data=all8525cc23 )
simmeanMort8525 <- data.frame(apply(simMort8525,2,mean))
PIMort8525 <- t(data.frame(apply(simMort8525,2,PI,prob=0.89)))
F23MortSim8525 <- data.frame(cbind(simmeanMort8525,PIMort8525))
F23MortSim8525$PlotCN <- all8525cc23  $PlotCN
F23MortSim8525 <- F23MortSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8525) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2630 <- link(MortSplit23_beta, data=all2630cc23 )
simmeanMort2630 <- data.frame(apply(simMort2630,2,mean))
PIMort2630 <- t(data.frame(apply(simMort2630,2,PI,prob=0.89)))
F23MortSim2630 <- data.frame(cbind(simmeanMort2630,PIMort2630))
F23MortSim2630$PlotCN <- all2630cc23  $PlotCN
F23MortSim2630 <- F23MortSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2630) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4530 <- link(MortSplit23_beta, data=all4530cc23 )
simmeanMort4530 <- data.frame(apply(simMort4530,2,mean))
PIMort4530 <- t(data.frame(apply(simMort4530,2,PI,prob=0.89)))
F23MortSim4530 <- data.frame(cbind(simmeanMort4530,PIMort4530))
F23MortSim4530$PlotCN <- all4530cc23  $PlotCN
F23MortSim4530 <- F23MortSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4530) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8530 <- link(MortSplit23_beta, data=all8530cc23 )
simmeanMort8530 <- data.frame(apply(simMort8530,2,mean))
PIMort8530 <- t(data.frame(apply(simMort8530,2,PI,prob=0.89)))
F23MortSim8530 <- data.frame(cbind(simmeanMort8530,PIMort8530))
F23MortSim8530$PlotCN <- all8530cc23  $PlotCN
F23MortSim8530 <- F23MortSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8530) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2635 <- link(MortSplit23_beta, data=all2635cc23 )
simmeanMort2635 <- data.frame(apply(simMort2635,2,mean))
PIMort2635 <- t(data.frame(apply(simMort2635,2,PI,prob=0.89)))
F23MortSim2635 <- data.frame(cbind(simmeanMort2635,PIMort2635))
F23MortSim2635$PlotCN <- all2635cc23  $PlotCN
F23MortSim2635 <- F23MortSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2635) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4535 <- link(MortSplit23_beta, data=all4535cc23 )
simmeanMort4535 <- data.frame(apply(simMort4535,2,mean))
PIMort4535 <- t(data.frame(apply(simMort4535,2,PI,prob=0.89)))
F23MortSim4535 <- data.frame(cbind(simmeanMort4535,PIMort4535))
F23MortSim4535$PlotCN <- all4535cc23  $PlotCN
F23MortSim4535 <- F23MortSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4535) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8535 <- link(MortSplit23_beta, data=all8535cc23 )
simmeanMort8535 <- data.frame(apply(simMort8535,2,mean))
PIMort8535 <- t(data.frame(apply(simMort8535,2,PI,prob=0.89)))
F23MortSim8535 <- data.frame(cbind(simmeanMort8535,PIMort8535))
F23MortSim8535$PlotCN <- all8535cc23  $PlotCN
F23MortSim8535 <- F23MortSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8535) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2640 <- link(MortSplit23_beta, data=all2640cc23 )
simmeanMort2640 <- data.frame(apply(simMort2640,2,mean))
PIMort2640 <- t(data.frame(apply(simMort2640,2,PI,prob=0.89)))
F23MortSim2640 <- data.frame(cbind(simmeanMort2640,PIMort2640))
F23MortSim2640$PlotCN <- all2640cc23  $PlotCN
F23MortSim2640 <- F23MortSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2640) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4540 <- link(MortSplit23_beta, data=all4540cc23 )
simmeanMort4540 <- data.frame(apply(simMort4540,2,mean))
PIMort4540 <- t(data.frame(apply(simMort4540,2,PI,prob=0.89)))
F23MortSim4540 <- data.frame(cbind(simmeanMort4540,PIMort4540))
F23MortSim4540$PlotCN <- all4540cc23  $PlotCN
F23MortSim4540 <- F23MortSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4540) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8540 <- link(MortSplit23_beta, data=all8540cc23 )
simmeanMort8540 <- data.frame(apply(simMort8540,2,mean))
PIMort8540 <- t(data.frame(apply(simMort8540,2,PI,prob=0.89)))
F23MortSim8540 <- data.frame(cbind(simmeanMort8540,PIMort8540))
F23MortSim8540$PlotCN <- all8540cc23  $PlotCN
F23MortSim8540 <- F23MortSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8540) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2645 <- link(MortSplit23_beta, data=all2645cc23 )
simmeanMort2645 <- data.frame(apply(simMort2645,2,mean))
PIMort2645 <- t(data.frame(apply(simMort2645,2,PI,prob=0.89)))
F23MortSim2645 <- data.frame(cbind(simmeanMort2645,PIMort2645))
F23MortSim2645$PlotCN <- all2645cc23  $PlotCN
F23MortSim2645 <- F23MortSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2645) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4545 <- link(MortSplit23_beta, data=all4545cc23 )
simmeanMort4545 <- data.frame(apply(simMort4545,2,mean))
PIMort4545 <- t(data.frame(apply(simMort4545,2,PI,prob=0.89)))
F23MortSim4545 <- data.frame(cbind(simmeanMort4545,PIMort4545))
F23MortSim4545$PlotCN <- all4545cc23  $PlotCN
F23MortSim4545 <- F23MortSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4545) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8545 <- link(MortSplit23_beta, data=all8545cc23 )
simmeanMort8545 <- data.frame(apply(simMort8545,2,mean))
PIMort8545 <- t(data.frame(apply(simMort8545,2,PI,prob=0.89)))
F23MortSim8545 <- data.frame(cbind(simmeanMort8545,PIMort8545))
F23MortSim8545$PlotCN <- all8545cc23  $PlotCN
F23MortSim8545 <- F23MortSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8545) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2650 <- link(MortSplit23_beta, data=all2650cc23 )
simmeanMort2650 <- data.frame(apply(simMort2650,2,mean))
PIMort2650 <- t(data.frame(apply(simMort2650,2,PI,prob=0.89)))
F23MortSim2650 <- data.frame(cbind(simmeanMort2650,PIMort2650))
F23MortSim2650$PlotCN <- all2650cc23  $PlotCN
F23MortSim2650 <- F23MortSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2650) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4550 <- link(MortSplit23_beta, data=all4550cc23 )
simmeanMort4550 <- data.frame(apply(simMort4550,2,mean))
PIMort4550 <- t(data.frame(apply(simMort4550,2,PI,prob=0.89)))
F23MortSim4550 <- data.frame(cbind(simmeanMort4550,PIMort4550))
F23MortSim4550$PlotCN <- all4550cc23  $PlotCN
F23MortSim4550 <- F23MortSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4550) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8550 <- link(MortSplit23_beta, data=all8550cc23 )
simmeanMort8550 <- data.frame(apply(simMort8550,2,mean))
PIMort8550 <- t(data.frame(apply(simMort8550,2,PI,prob=0.89)))
F23MortSim8550 <- data.frame(cbind(simmeanMort8550,PIMort8550))
F23MortSim8550$PlotCN <- all8550cc23  $PlotCN
F23MortSim8550 <- F23MortSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8550) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2655 <- link(MortSplit23_beta, data=all2655cc23 )
simmeanMort2655 <- data.frame(apply(simMort2655,2,mean))
PIMort2655 <- t(data.frame(apply(simMort2655,2,PI,prob=0.89)))
F23MortSim2655 <- data.frame(cbind(simmeanMort2655,PIMort2655))
F23MortSim2655$PlotCN <- all2655cc23  $PlotCN
F23MortSim2655 <- F23MortSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2655) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4555 <- link(MortSplit23_beta, data=all4555cc23 )
simmeanMort4555 <- data.frame(apply(simMort4555,2,mean))
PIMort4555 <- t(data.frame(apply(simMort4555,2,PI,prob=0.89)))
F23MortSim4555 <- data.frame(cbind(simmeanMort4555,PIMort4555))
F23MortSim4555$PlotCN <- all4555cc23  $PlotCN
F23MortSim4555 <- F23MortSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4555) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8555 <- link(MortSplit23_beta, data=all8555cc23 )
simmeanMort8555 <- data.frame(apply(simMort8555,2,mean))
PIMort8555 <- t(data.frame(apply(simMort8555,2,PI,prob=0.89)))
F23MortSim8555 <- data.frame(cbind(simmeanMort8555,PIMort8555))
F23MortSim8555$PlotCN <- all8555cc23  $PlotCN
F23MortSim8555 <- F23MortSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8555) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2660 <- link(MortSplit23_beta, data=all2660cc23 )
simmeanMort2660 <- data.frame(apply(simMort2660,2,mean))
PIMort2660 <- t(data.frame(apply(simMort2660,2,PI,prob=0.89)))
F23MortSim2660 <- data.frame(cbind(simmeanMort2660,PIMort2660))
F23MortSim2660$PlotCN <- all2660cc23  $PlotCN
F23MortSim2660 <- F23MortSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2660) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4560 <- link(MortSplit23_beta, data=all4560cc23 )
simmeanMort4560 <- data.frame(apply(simMort4560,2,mean))
PIMort4560 <- t(data.frame(apply(simMort4560,2,PI,prob=0.89)))
F23MortSim4560 <- data.frame(cbind(simmeanMort4560,PIMort4560))
F23MortSim4560$PlotCN <- all4560cc23  $PlotCN
F23MortSim4560 <- F23MortSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4560) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8560 <- link(MortSplit23_beta, data=all8560cc23 )
simmeanMort8560 <- data.frame(apply(simMort8560,2,mean))
PIMort8560 <- t(data.frame(apply(simMort8560,2,PI,prob=0.89)))
F23MortSim8560 <- data.frame(cbind(simmeanMort8560,PIMort8560))
F23MortSim8560$PlotCN <- all8560cc23  $PlotCN
F23MortSim8560 <- F23MortSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8560) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2665 <- link(MortSplit23_beta, data=all2665cc23 )
simmeanMort2665 <- data.frame(apply(simMort2665,2,mean))
PIMort2665 <- t(data.frame(apply(simMort2665,2,PI,prob=0.89)))
F23MortSim2665 <- data.frame(cbind(simmeanMort2665,PIMort2665))
F23MortSim2665$PlotCN <- all2665cc23  $PlotCN
F23MortSim2665 <- F23MortSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2665) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4565 <- link(MortSplit23_beta, data=all4565cc23 )
simmeanMort4565 <- data.frame(apply(simMort4565,2,mean))
PIMort4565 <- t(data.frame(apply(simMort4565,2,PI,prob=0.89)))
F23MortSim4565 <- data.frame(cbind(simmeanMort4565,PIMort4565))
F23MortSim4565$PlotCN <- all4565cc23  $PlotCN
F23MortSim4565 <- F23MortSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4565) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8565 <- link(MortSplit23_beta, data=all8565cc23 )
simmeanMort8565 <- data.frame(apply(simMort8565,2,mean))
PIMort8565 <- t(data.frame(apply(simMort8565,2,PI,prob=0.89)))
F23MortSim8565 <- data.frame(cbind(simmeanMort8565,PIMort8565))
F23MortSim8565$PlotCN <- all8565cc23  $PlotCN
F23MortSim8565 <- F23MortSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8565) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2670 <- link(MortSplit23_beta, data=all2670cc23 )
simmeanMort2670 <- data.frame(apply(simMort2670,2,mean))
PIMort2670 <- t(data.frame(apply(simMort2670,2,PI,prob=0.89)))
F23MortSim2670 <- data.frame(cbind(simmeanMort2670,PIMort2670))
F23MortSim2670$PlotCN <- all2670cc23  $PlotCN
F23MortSim2670 <- F23MortSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2670) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4570 <- link(MortSplit23_beta, data=all4570cc23 )
simmeanMort4570 <- data.frame(apply(simMort4570,2,mean))
PIMort4570 <- t(data.frame(apply(simMort4570,2,PI,prob=0.89)))
F23MortSim4570 <- data.frame(cbind(simmeanMort4570,PIMort4570))
F23MortSim4570$PlotCN <- all4570cc23  $PlotCN
F23MortSim4570 <- F23MortSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4570) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8570 <- link(MortSplit23_beta, data=all8570cc23 )
simmeanMort8570 <- data.frame(apply(simMort8570,2,mean))
PIMort8570 <- t(data.frame(apply(simMort8570,2,PI,prob=0.89)))
F23MortSim8570 <- data.frame(cbind(simmeanMort8570,PIMort8570))
F23MortSim8570$PlotCN <- all8570cc23  $PlotCN
F23MortSim8570 <- F23MortSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8570) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2675 <- link(MortSplit23_beta, data=all2675cc23 )
simmeanMort2675 <- data.frame(apply(simMort2675,2,mean))
PIMort2675 <- t(data.frame(apply(simMort2675,2,PI,prob=0.89)))
F23MortSim2675 <- data.frame(cbind(simmeanMort2675,PIMort2675))
F23MortSim2675$PlotCN <- all2675cc23  $PlotCN
F23MortSim2675 <- F23MortSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2675) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4575 <- link(MortSplit23_beta, data=all4575cc23 )
simmeanMort4575 <- data.frame(apply(simMort4575,2,mean))
PIMort4575 <- t(data.frame(apply(simMort4575,2,PI,prob=0.89)))
F23MortSim4575 <- data.frame(cbind(simmeanMort4575,PIMort4575))
F23MortSim4575$PlotCN <- all4575cc23  $PlotCN
F23MortSim4575 <- F23MortSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4575) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8575 <- link(MortSplit23_beta, data=all8575cc23 )
simmeanMort8575 <- data.frame(apply(simMort8575,2,mean))
PIMort8575 <- t(data.frame(apply(simMort8575,2,PI,prob=0.89)))
F23MortSim8575 <- data.frame(cbind(simmeanMort8575,PIMort8575))
F23MortSim8575$PlotCN <- all8575cc23  $PlotCN
F23MortSim8575 <- F23MortSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8575) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2680 <- link(MortSplit23_beta, data=all2680cc23 )
simmeanMort2680 <- data.frame(apply(simMort2680,2,mean))
PIMort2680 <- t(data.frame(apply(simMort2680,2,PI,prob=0.89)))
F23MortSim2680 <- data.frame(cbind(simmeanMort2680,PIMort2680))
F23MortSim2680$PlotCN <- all2680cc23  $PlotCN
F23MortSim2680 <- F23MortSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim2680) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4580 <- link(MortSplit23_beta, data=all4580cc23 )
simmeanMort4580 <- data.frame(apply(simMort4580,2,mean))
PIMort4580 <- t(data.frame(apply(simMort4580,2,PI,prob=0.89)))
F23MortSim4580 <- data.frame(cbind(simmeanMort4580,PIMort4580))
F23MortSim4580$PlotCN <- all4580cc23  $PlotCN
F23MortSim4580 <- F23MortSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim4580) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8580 <- link(MortSplit23_beta, data=all8580cc23 )
simmeanMort8580 <- data.frame(apply(simMort8580,2,mean))
PIMort8580 <- t(data.frame(apply(simMort8580,2,PI,prob=0.89)))
F23MortSim8580 <- data.frame(cbind(simmeanMort8580,PIMort8580))
F23MortSim8580$PlotCN <- all8580cc23  $PlotCN
F23MortSim8580 <- F23MortSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F23MortSim8580) <- c("PlotCN","MortPerAcre","5CI","95CI")

#create vector of new colnames
MortSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Mort", "5CI", "95CI"), x)))
MortSimnames<- append(MortSimnames,"PlotCN",after=0)

#group data by RCP pathway
F23MortSim26B <- F23MortSim2625 %>% cbind(F23MortSim2630[,2:4]) %>%
  cbind(F23MortSim2635[,2:4]) %>% cbind(F23MortSim2640[,2:4]) %>% cbind(F23MortSim2645[,2:4]) %>%
  cbind(F23MortSim2650[,2:4]) %>% cbind(F23MortSim2655[,2:4]) %>% cbind(F23MortSim2660[,2:4]) %>%
  cbind(F23MortSim2665[,2:4]) %>% cbind(F23MortSim2670[,2:4]) %>% cbind(F23MortSim2675[,2:4]) %>%
  cbind(F23MortSim2680[,2:4])
colnames(F23MortSim26B) <- MortSimnames
write.csv(F23MortSim26B,file="F23MortPred26B.csv")

F23MortSim45B <- F23MortSim4525 %>% cbind(F23MortSim4530[,2:4]) %>%
  cbind(F23MortSim4535[,2:4]) %>% cbind(F23MortSim4540[,2:4]) %>% cbind(F23MortSim4545[,2:4]) %>%
  cbind(F23MortSim4550[,2:4]) %>% cbind(F23MortSim4555[,2:4]) %>% cbind(F23MortSim4560[,2:4]) %>%
  cbind(F23MortSim4565[,2:4]) %>% cbind(F23MortSim4570[,2:4]) %>% cbind(F23MortSim4575[,2:4]) %>%
  cbind(F23MortSim4580[,2:4])
colnames(F23MortSim45B) <- MortSimnames
write.csv(F23MortSim45B,file="F23MortPred45B.csv")

F23MortSim85B <- F23MortSim8525 %>% cbind(F23MortSim8530[,2:4]) %>%
  cbind(F23MortSim8535[,2:4]) %>% cbind(F23MortSim8540[,2:4]) %>% cbind(F23MortSim8545[,2:4]) %>%
  cbind(F23MortSim8550[,2:4]) %>% cbind(F23MortSim8555[,2:4]) %>% cbind(F23MortSim8560[,2:4]) %>%
  cbind(F23MortSim8565[,2:4]) %>% cbind(F23MortSim8570[,2:4]) %>% cbind(F23MortSim8575[,2:4]) %>%
  cbind(F23MortSim8580[,2:4])
colnames(F23MortSim85B) <- MortSimnames
write.csv(F23MortSim85B,file="F23MortPred85B.csv")


#fgroup 24
simMort2625 <- link(MortSplit24_beta, data=all2625cc24 )
simmeanMort2625 <- data.frame(apply(simMort2625,2,mean))
PIMort2625 <- t(data.frame(apply(simMort2625,2,PI,prob=0.89)))
F24MortSim2625 <- data.frame(cbind(simmeanMort2625,PIMort2625))
F24MortSim2625$PlotCN <- all2625cc24  $PlotCN
F24MortSim2625 <- F24MortSim2625 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2625) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4525 <- link(MortSplit24_beta, data=all4525cc24 )
simmeanMort4525 <- data.frame(apply(simMort4525,2,mean))
PIMort4525 <- t(data.frame(apply(simMort4525,2,PI,prob=0.89)))
F24MortSim4525 <- data.frame(cbind(simmeanMort4525,PIMort4525))
F24MortSim4525$PlotCN <- all4525cc24  $PlotCN
F24MortSim4525 <- F24MortSim4525 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4525) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8525 <- link(MortSplit24_beta, data=all8525cc24 )
simmeanMort8525 <- data.frame(apply(simMort8525,2,mean))
PIMort8525 <- t(data.frame(apply(simMort8525,2,PI,prob=0.89)))
F24MortSim8525 <- data.frame(cbind(simmeanMort8525,PIMort8525))
F24MortSim8525$PlotCN <- all8525cc24  $PlotCN
F24MortSim8525 <- F24MortSim8525 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8525) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2630 <- link(MortSplit24_beta, data=all2630cc24 )
simmeanMort2630 <- data.frame(apply(simMort2630,2,mean))
PIMort2630 <- t(data.frame(apply(simMort2630,2,PI,prob=0.89)))
F24MortSim2630 <- data.frame(cbind(simmeanMort2630,PIMort2630))
F24MortSim2630$PlotCN <- all2630cc24  $PlotCN
F24MortSim2630 <- F24MortSim2630 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2630) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4530 <- link(MortSplit24_beta, data=all4530cc24 )
simmeanMort4530 <- data.frame(apply(simMort4530,2,mean))
PIMort4530 <- t(data.frame(apply(simMort4530,2,PI,prob=0.89)))
F24MortSim4530 <- data.frame(cbind(simmeanMort4530,PIMort4530))
F24MortSim4530$PlotCN <- all4530cc24  $PlotCN
F24MortSim4530 <- F24MortSim4530 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4530) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8530 <- link(MortSplit24_beta, data=all8530cc24 )
simmeanMort8530 <- data.frame(apply(simMort8530,2,mean))
PIMort8530 <- t(data.frame(apply(simMort8530,2,PI,prob=0.89)))
F24MortSim8530 <- data.frame(cbind(simmeanMort8530,PIMort8530))
F24MortSim8530$PlotCN <- all8530cc24  $PlotCN
F24MortSim8530 <- F24MortSim8530 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8530) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2635 <- link(MortSplit24_beta, data=all2635cc24 )
simmeanMort2635 <- data.frame(apply(simMort2635,2,mean))
PIMort2635 <- t(data.frame(apply(simMort2635,2,PI,prob=0.89)))
F24MortSim2635 <- data.frame(cbind(simmeanMort2635,PIMort2635))
F24MortSim2635$PlotCN <- all2635cc24  $PlotCN
F24MortSim2635 <- F24MortSim2635 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2635) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4535 <- link(MortSplit24_beta, data=all4535cc24 )
simmeanMort4535 <- data.frame(apply(simMort4535,2,mean))
PIMort4535 <- t(data.frame(apply(simMort4535,2,PI,prob=0.89)))
F24MortSim4535 <- data.frame(cbind(simmeanMort4535,PIMort4535))
F24MortSim4535$PlotCN <- all4535cc24  $PlotCN
F24MortSim4535 <- F24MortSim4535 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4535) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8535 <- link(MortSplit24_beta, data=all8535cc24 )
simmeanMort8535 <- data.frame(apply(simMort8535,2,mean))
PIMort8535 <- t(data.frame(apply(simMort8535,2,PI,prob=0.89)))
F24MortSim8535 <- data.frame(cbind(simmeanMort8535,PIMort8535))
F24MortSim8535$PlotCN <- all8535cc24  $PlotCN
F24MortSim8535 <- F24MortSim8535 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8535) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2640 <- link(MortSplit24_beta, data=all2640cc24 )
simmeanMort2640 <- data.frame(apply(simMort2640,2,mean))
PIMort2640 <- t(data.frame(apply(simMort2640,2,PI,prob=0.89)))
F24MortSim2640 <- data.frame(cbind(simmeanMort2640,PIMort2640))
F24MortSim2640$PlotCN <- all2640cc24  $PlotCN
F24MortSim2640 <- F24MortSim2640 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2640) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4540 <- link(MortSplit24_beta, data=all4540cc24 )
simmeanMort4540 <- data.frame(apply(simMort4540,2,mean))
PIMort4540 <- t(data.frame(apply(simMort4540,2,PI,prob=0.89)))
F24MortSim4540 <- data.frame(cbind(simmeanMort4540,PIMort4540))
F24MortSim4540$PlotCN <- all4540cc24  $PlotCN
F24MortSim4540 <- F24MortSim4540 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4540) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8540 <- link(MortSplit24_beta, data=all8540cc24 )
simmeanMort8540 <- data.frame(apply(simMort8540,2,mean))
PIMort8540 <- t(data.frame(apply(simMort8540,2,PI,prob=0.89)))
F24MortSim8540 <- data.frame(cbind(simmeanMort8540,PIMort8540))
F24MortSim8540$PlotCN <- all8540cc24  $PlotCN
F24MortSim8540 <- F24MortSim8540 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8540) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2645 <- link(MortSplit24_beta, data=all2645cc24 )
simmeanMort2645 <- data.frame(apply(simMort2645,2,mean))
PIMort2645 <- t(data.frame(apply(simMort2645,2,PI,prob=0.89)))
F24MortSim2645 <- data.frame(cbind(simmeanMort2645,PIMort2645))
F24MortSim2645$PlotCN <- all2645cc24  $PlotCN
F24MortSim2645 <- F24MortSim2645 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2645) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4545 <- link(MortSplit24_beta, data=all4545cc24 )
simmeanMort4545 <- data.frame(apply(simMort4545,2,mean))
PIMort4545 <- t(data.frame(apply(simMort4545,2,PI,prob=0.89)))
F24MortSim4545 <- data.frame(cbind(simmeanMort4545,PIMort4545))
F24MortSim4545$PlotCN <- all4545cc24  $PlotCN
F24MortSim4545 <- F24MortSim4545 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4545) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8545 <- link(MortSplit24_beta, data=all8545cc24 )
simmeanMort8545 <- data.frame(apply(simMort8545,2,mean))
PIMort8545 <- t(data.frame(apply(simMort8545,2,PI,prob=0.89)))
F24MortSim8545 <- data.frame(cbind(simmeanMort8545,PIMort8545))
F24MortSim8545$PlotCN <- all8545cc24  $PlotCN
F24MortSim8545 <- F24MortSim8545 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8545) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2650 <- link(MortSplit24_beta, data=all2650cc24 )
simmeanMort2650 <- data.frame(apply(simMort2650,2,mean))
PIMort2650 <- t(data.frame(apply(simMort2650,2,PI,prob=0.89)))
F24MortSim2650 <- data.frame(cbind(simmeanMort2650,PIMort2650))
F24MortSim2650$PlotCN <- all2650cc24  $PlotCN
F24MortSim2650 <- F24MortSim2650 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2650) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4550 <- link(MortSplit24_beta, data=all4550cc24 )
simmeanMort4550 <- data.frame(apply(simMort4550,2,mean))
PIMort4550 <- t(data.frame(apply(simMort4550,2,PI,prob=0.89)))
F24MortSim4550 <- data.frame(cbind(simmeanMort4550,PIMort4550))
F24MortSim4550$PlotCN <- all4550cc24  $PlotCN
F24MortSim4550 <- F24MortSim4550 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4550) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8550 <- link(MortSplit24_beta, data=all8550cc24 )
simmeanMort8550 <- data.frame(apply(simMort8550,2,mean))
PIMort8550 <- t(data.frame(apply(simMort8550,2,PI,prob=0.89)))
F24MortSim8550 <- data.frame(cbind(simmeanMort8550,PIMort8550))
F24MortSim8550$PlotCN <- all8550cc24  $PlotCN
F24MortSim8550 <- F24MortSim8550 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8550) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2655 <- link(MortSplit24_beta, data=all2655cc24 )
simmeanMort2655 <- data.frame(apply(simMort2655,2,mean))
PIMort2655 <- t(data.frame(apply(simMort2655,2,PI,prob=0.89)))
F24MortSim2655 <- data.frame(cbind(simmeanMort2655,PIMort2655))
F24MortSim2655$PlotCN <- all2655cc24  $PlotCN
F24MortSim2655 <- F24MortSim2655 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2655) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4555 <- link(MortSplit24_beta, data=all4555cc24 )
simmeanMort4555 <- data.frame(apply(simMort4555,2,mean))
PIMort4555 <- t(data.frame(apply(simMort4555,2,PI,prob=0.89)))
F24MortSim4555 <- data.frame(cbind(simmeanMort4555,PIMort4555))
F24MortSim4555$PlotCN <- all4555cc24  $PlotCN
F24MortSim4555 <- F24MortSim4555 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4555) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8555 <- link(MortSplit24_beta, data=all8555cc24 )
simmeanMort8555 <- data.frame(apply(simMort8555,2,mean))
PIMort8555 <- t(data.frame(apply(simMort8555,2,PI,prob=0.89)))
F24MortSim8555 <- data.frame(cbind(simmeanMort8555,PIMort8555))
F24MortSim8555$PlotCN <- all8555cc24  $PlotCN
F24MortSim8555 <- F24MortSim8555 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8555) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2660 <- link(MortSplit24_beta, data=all2660cc24 )
simmeanMort2660 <- data.frame(apply(simMort2660,2,mean))
PIMort2660 <- t(data.frame(apply(simMort2660,2,PI,prob=0.89)))
F24MortSim2660 <- data.frame(cbind(simmeanMort2660,PIMort2660))
F24MortSim2660$PlotCN <- all2660cc24  $PlotCN
F24MortSim2660 <- F24MortSim2660 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2660) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4560 <- link(MortSplit24_beta, data=all4560cc24 )
simmeanMort4560 <- data.frame(apply(simMort4560,2,mean))
PIMort4560 <- t(data.frame(apply(simMort4560,2,PI,prob=0.89)))
F24MortSim4560 <- data.frame(cbind(simmeanMort4560,PIMort4560))
F24MortSim4560$PlotCN <- all4560cc24  $PlotCN
F24MortSim4560 <- F24MortSim4560 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4560) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8560 <- link(MortSplit24_beta, data=all8560cc24 )
simmeanMort8560 <- data.frame(apply(simMort8560,2,mean))
PIMort8560 <- t(data.frame(apply(simMort8560,2,PI,prob=0.89)))
F24MortSim8560 <- data.frame(cbind(simmeanMort8560,PIMort8560))
F24MortSim8560$PlotCN <- all8560cc24  $PlotCN
F24MortSim8560 <- F24MortSim8560 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8560) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2665 <- link(MortSplit24_beta, data=all2665cc24 )
simmeanMort2665 <- data.frame(apply(simMort2665,2,mean))
PIMort2665 <- t(data.frame(apply(simMort2665,2,PI,prob=0.89)))
F24MortSim2665 <- data.frame(cbind(simmeanMort2665,PIMort2665))
F24MortSim2665$PlotCN <- all2665cc24  $PlotCN
F24MortSim2665 <- F24MortSim2665 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2665) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4565 <- link(MortSplit24_beta, data=all4565cc24 )
simmeanMort4565 <- data.frame(apply(simMort4565,2,mean))
PIMort4565 <- t(data.frame(apply(simMort4565,2,PI,prob=0.89)))
F24MortSim4565 <- data.frame(cbind(simmeanMort4565,PIMort4565))
F24MortSim4565$PlotCN <- all4565cc24  $PlotCN
F24MortSim4565 <- F24MortSim4565 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4565) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8565 <- link(MortSplit24_beta, data=all8565cc24 )
simmeanMort8565 <- data.frame(apply(simMort8565,2,mean))
PIMort8565 <- t(data.frame(apply(simMort8565,2,PI,prob=0.89)))
F24MortSim8565 <- data.frame(cbind(simmeanMort8565,PIMort8565))
F24MortSim8565$PlotCN <- all8565cc24  $PlotCN
F24MortSim8565 <- F24MortSim8565 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8565) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2670 <- link(MortSplit24_beta, data=all2670cc24 )
simmeanMort2670 <- data.frame(apply(simMort2670,2,mean))
PIMort2670 <- t(data.frame(apply(simMort2670,2,PI,prob=0.89)))
F24MortSim2670 <- data.frame(cbind(simmeanMort2670,PIMort2670))
F24MortSim2670$PlotCN <- all2670cc24  $PlotCN
F24MortSim2670 <- F24MortSim2670 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2670) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4570 <- link(MortSplit24_beta, data=all4570cc24 )
simmeanMort4570 <- data.frame(apply(simMort4570,2,mean))
PIMort4570 <- t(data.frame(apply(simMort4570,2,PI,prob=0.89)))
F24MortSim4570 <- data.frame(cbind(simmeanMort4570,PIMort4570))
F24MortSim4570$PlotCN <- all4570cc24  $PlotCN
F24MortSim4570 <- F24MortSim4570 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4570) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8570 <- link(MortSplit24_beta, data=all8570cc24 )
simmeanMort8570 <- data.frame(apply(simMort8570,2,mean))
PIMort8570 <- t(data.frame(apply(simMort8570,2,PI,prob=0.89)))
F24MortSim8570 <- data.frame(cbind(simmeanMort8570,PIMort8570))
F24MortSim8570$PlotCN <- all8570cc24  $PlotCN
F24MortSim8570 <- F24MortSim8570 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8570) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2675 <- link(MortSplit24_beta, data=all2675cc24 )
simmeanMort2675 <- data.frame(apply(simMort2675,2,mean))
PIMort2675 <- t(data.frame(apply(simMort2675,2,PI,prob=0.89)))
F24MortSim2675 <- data.frame(cbind(simmeanMort2675,PIMort2675))
F24MortSim2675$PlotCN <- all2675cc24  $PlotCN
F24MortSim2675 <- F24MortSim2675 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2675) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4575 <- link(MortSplit24_beta, data=all4575cc24 )
simmeanMort4575 <- data.frame(apply(simMort4575,2,mean))
PIMort4575 <- t(data.frame(apply(simMort4575,2,PI,prob=0.89)))
F24MortSim4575 <- data.frame(cbind(simmeanMort4575,PIMort4575))
F24MortSim4575$PlotCN <- all4575cc24  $PlotCN
F24MortSim4575 <- F24MortSim4575 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4575) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8575 <- link(MortSplit24_beta, data=all8575cc24 )
simmeanMort8575 <- data.frame(apply(simMort8575,2,mean))
PIMort8575 <- t(data.frame(apply(simMort8575,2,PI,prob=0.89)))
F24MortSim8575 <- data.frame(cbind(simmeanMort8575,PIMort8575))
F24MortSim8575$PlotCN <- all8575cc24  $PlotCN
F24MortSim8575 <- F24MortSim8575 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8575) <- c("PlotCN","MortPerAcre","5CI","95CI")

simMort2680 <- link(MortSplit24_beta, data=all2680cc24 )
simmeanMort2680 <- data.frame(apply(simMort2680,2,mean))
PIMort2680 <- t(data.frame(apply(simMort2680,2,PI,prob=0.89)))
F24MortSim2680 <- data.frame(cbind(simmeanMort2680,PIMort2680))
F24MortSim2680$PlotCN <- all2680cc24  $PlotCN
F24MortSim2680 <- F24MortSim2680 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim2680) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort4580 <- link(MortSplit24_beta, data=all4580cc24 )
simmeanMort4580 <- data.frame(apply(simMort4580,2,mean))
PIMort4580 <- t(data.frame(apply(simMort4580,2,PI,prob=0.89)))
F24MortSim4580 <- data.frame(cbind(simmeanMort4580,PIMort4580))
F24MortSim4580$PlotCN <- all4580cc24  $PlotCN
F24MortSim4580 <- F24MortSim4580 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim4580) <- c("PlotCN","MortPerAcre","5CI","95CI")
simMort8580 <- link(MortSplit24_beta, data=all8580cc24 )
simmeanMort8580 <- data.frame(apply(simMort8580,2,mean))
PIMort8580 <- t(data.frame(apply(simMort8580,2,PI,prob=0.89)))
F24MortSim8580 <- data.frame(cbind(simmeanMort8580,PIMort8580))
F24MortSim8580$PlotCN <- all8580cc24  $PlotCN
F24MortSim8580 <- F24MortSim8580 %>% dplyr::select(PlotCN, everything())
colnames(F24MortSim8580) <- c("PlotCN","MortPerAcre","5CI","95CI")

#create vector of new colnames
MortSimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("Mort", "5CI", "95CI"), x)))
MortSimnames<- append(MortSimnames,"PlotCN",after=0)

#group data by RCP pathway
F24MortSim26B <- F24MortSim2625 %>% cbind(F24MortSim2630[,2:4]) %>%
  cbind(F24MortSim2635[,2:4]) %>% cbind(F24MortSim2640[,2:4]) %>% cbind(F24MortSim2645[,2:4]) %>%
  cbind(F24MortSim2650[,2:4]) %>% cbind(F24MortSim2655[,2:4]) %>% cbind(F24MortSim2660[,2:4]) %>%
  cbind(F24MortSim2665[,2:4]) %>% cbind(F24MortSim2670[,2:4]) %>% cbind(F24MortSim2675[,2:4]) %>%
  cbind(F24MortSim2680[,2:4])
colnames(F24MortSim26B) <- MortSimnames
write.csv(F24MortSim26B,file="F24MortPred26B.csv")

F24MortSim45B <- F24MortSim4525 %>% cbind(F24MortSim4530[,2:4]) %>%
  cbind(F24MortSim4535[,2:4]) %>% cbind(F24MortSim4540[,2:4]) %>% cbind(F24MortSim4545[,2:4]) %>%
  cbind(F24MortSim4550[,2:4]) %>% cbind(F24MortSim4555[,2:4]) %>% cbind(F24MortSim4560[,2:4]) %>%
  cbind(F24MortSim4565[,2:4]) %>% cbind(F24MortSim4570[,2:4]) %>% cbind(F24MortSim4575[,2:4]) %>%
  cbind(F24MortSim4580[,2:4])
colnames(F24MortSim45B) <- MortSimnames
write.csv(F24MortSim45B,file="F24MortPred45B.csv")

F24MortSim85B <- F24MortSim8525 %>% cbind(F24MortSim8530[,2:4]) %>%
  cbind(F24MortSim8535[,2:4]) %>% cbind(F24MortSim8540[,2:4]) %>% cbind(F24MortSim8545[,2:4]) %>%
  cbind(F24MortSim8550[,2:4]) %>% cbind(F24MortSim8555[,2:4]) %>% cbind(F24MortSim8560[,2:4]) %>%
  cbind(F24MortSim8565[,2:4]) %>% cbind(F24MortSim8570[,2:4]) %>% cbind(F24MortSim8575[,2:4]) %>%
  cbind(F24MortSim8580[,2:4])
colnames(F24MortSim85B) <- MortSimnames
write.csv(F24MortSim85B,file="F24MortPred85B.csv")


#AQI Predictions
#fgroup 1
simAQI2625 <- data.frame(link(AQISplit1, data=all2625cc1))
simmeanAQI2625 <- data.frame(apply(simAQI2625,2,mean))
PIAQI2625 <- t(data.frame(apply(simAQI2625,2,PI,prob=0.89)))
F1AQISim2625 <- data.frame(cbind(simmeanAQI2625,PIAQI2625))
F1AQISim2625$PlotCN <- all2625cc1 $PlotCN
F1AQISim2625 <- F1AQISim2625 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2625) <- c("PlotCN","AQI","5CI","95CI")
simAQI4525 <- link(AQISplit1, data=all4525cc1)
simmeanAQI4525 <- data.frame(apply(simAQI4525,2,mean))
PIAQI4525 <- t(data.frame(apply(simAQI4525,2,PI,prob=0.89)))
F1AQISim4525 <- data.frame(cbind(simmeanAQI4525,PIAQI4525))
F1AQISim4525$PlotCN <- all4525cc1 $PlotCN
F1AQISim4525 <- F1AQISim4525 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4525) <- c("PlotCN","AQI","5CI","95CI")
simAQI8525 <- link(AQISplit1, data=all8525cc1)
simmeanAQI8525 <- data.frame(apply(simAQI8525,2,mean))
PIAQI8525 <- t(data.frame(apply(simAQI8525,2,PI,prob=0.89)))
F1AQISim8525 <- data.frame(cbind(simmeanAQI8525,PIAQI8525))
F1AQISim8525$PlotCN <- all8525cc1 $PlotCN
F1AQISim8525 <- F1AQISim8525 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8525) <- c("PlotCN","AQI","5CI","95CI")

simAQI2630 <- link(AQISplit1, data=all2630cc1)
simmeanAQI2630 <- data.frame(apply(simAQI2630,2,mean))
PIAQI2630 <- t(data.frame(apply(simAQI2630,2,PI,prob=0.89)))
F1AQISim2630 <- data.frame(cbind(simmeanAQI2630,PIAQI2630))
F1AQISim2630$PlotCN <- all2630cc1 $PlotCN
F1AQISim2630 <- F1AQISim2630 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2630) <- c("PlotCN","AQI","5CI","95CI")
simAQI4530 <- link(AQISplit1, data=all4530cc1)
simmeanAQI4530 <- data.frame(apply(simAQI4530,2,mean))
PIAQI4530 <- t(data.frame(apply(simAQI4530,2,PI,prob=0.89)))
F1AQISim4530 <- data.frame(cbind(simmeanAQI4530,PIAQI4530))
F1AQISim4530$PlotCN <- all4530cc1 $PlotCN
F1AQISim4530 <- F1AQISim4530 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4530) <- c("PlotCN","AQI","5CI","95CI")
simAQI8530 <- link(AQISplit1, data=all8530cc1)
simmeanAQI8530 <- data.frame(apply(simAQI8530,2,mean))
PIAQI8530 <- t(data.frame(apply(simAQI8530,2,PI,prob=0.89)))
F1AQISim8530 <- data.frame(cbind(simmeanAQI8530,PIAQI8530))
F1AQISim8530$PlotCN <- all8530cc1 $PlotCN
F1AQISim8530 <- F1AQISim8530 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8530) <- c("PlotCN","AQI","5CI","95CI")

simAQI2635 <- link(AQISplit1, data=all2635cc1)
simmeanAQI2635 <- data.frame(apply(simAQI2635,2,mean))
PIAQI2635 <- t(data.frame(apply(simAQI2635,2,PI,prob=0.89)))
F1AQISim2635 <- data.frame(cbind(simmeanAQI2635,PIAQI2635))
F1AQISim2635$PlotCN <- all2635cc1 $PlotCN
F1AQISim2635 <- F1AQISim2635 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2635) <- c("PlotCN","AQI","5CI","95CI")
simAQI4535 <- link(AQISplit1, data=all4535cc1)
simmeanAQI4535 <- data.frame(apply(simAQI4535,2,mean))
PIAQI4535 <- t(data.frame(apply(simAQI4535,2,PI,prob=0.89)))
F1AQISim4535 <- data.frame(cbind(simmeanAQI4535,PIAQI4535))
F1AQISim4535$PlotCN <- all4535cc1 $PlotCN
F1AQISim4535 <- F1AQISim4535 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4535) <- c("PlotCN","AQI","5CI","95CI")
simAQI8535 <- link(AQISplit1, data=all8535cc1)
simmeanAQI8535 <- data.frame(apply(simAQI8535,2,mean))
PIAQI8535 <- t(data.frame(apply(simAQI8535,2,PI,prob=0.89)))
F1AQISim8535 <- data.frame(cbind(simmeanAQI8535,PIAQI8535))
F1AQISim8535$PlotCN <- all8535cc1 $PlotCN
F1AQISim8535 <- F1AQISim8535 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8535) <- c("PlotCN","AQI","5CI","95CI")

simAQI2640 <- link(AQISplit1, data=all2640cc1)
simmeanAQI2640 <- data.frame(apply(simAQI2640,2,mean))
PIAQI2640 <- t(data.frame(apply(simAQI2640,2,PI,prob=0.89)))
F1AQISim2640 <- data.frame(cbind(simmeanAQI2640,PIAQI2640))
F1AQISim2640$PlotCN <- all2640cc1 $PlotCN
F1AQISim2640 <- F1AQISim2640 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2640) <- c("PlotCN","AQI","5CI","95CI")
simAQI4540 <- link(AQISplit1, data=all4540cc1)
simmeanAQI4540 <- data.frame(apply(simAQI4540,2,mean))
PIAQI4540 <- t(data.frame(apply(simAQI4540,2,PI,prob=0.89)))
F1AQISim4540 <- data.frame(cbind(simmeanAQI4540,PIAQI4540))
F1AQISim4540$PlotCN <- all4540cc1 $PlotCN
F1AQISim4540 <- F1AQISim4540 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4540) <- c("PlotCN","AQI","5CI","95CI")
simAQI8540 <- link(AQISplit1, data=all8540cc1)
simmeanAQI8540 <- data.frame(apply(simAQI8540,2,mean))
PIAQI8540 <- t(data.frame(apply(simAQI8540,2,PI,prob=0.89)))
F1AQISim8540 <- data.frame(cbind(simmeanAQI8540,PIAQI8540))
F1AQISim8540$PlotCN <- all8540cc1 $PlotCN
F1AQISim8540 <- F1AQISim8540 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8540) <- c("PlotCN","AQI","5CI","95CI")

simAQI2645 <- link(AQISplit1, data=all2645cc1)
simmeanAQI2645 <- data.frame(apply(simAQI2645,2,mean))
PIAQI2645 <- t(data.frame(apply(simAQI2645,2,PI,prob=0.89)))
F1AQISim2645 <- data.frame(cbind(simmeanAQI2645,PIAQI2645))
F1AQISim2645$PlotCN <- all2645cc1 $PlotCN
F1AQISim2645 <- F1AQISim2645 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2645) <- c("PlotCN","AQI","5CI","95CI")
simAQI4545 <- link(AQISplit1, data=all4545cc1)
simmeanAQI4545 <- data.frame(apply(simAQI4545,2,mean))
PIAQI4545 <- t(data.frame(apply(simAQI4545,2,PI,prob=0.89)))
F1AQISim4545 <- data.frame(cbind(simmeanAQI4545,PIAQI4545))
F1AQISim4545$PlotCN <- all4545cc1 $PlotCN
F1AQISim4545 <- F1AQISim4545 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4545) <- c("PlotCN","AQI","5CI","95CI")
simAQI8545 <- link(AQISplit1, data=all8545cc1)
simmeanAQI8545 <- data.frame(apply(simAQI8545,2,mean))
PIAQI8545 <- t(data.frame(apply(simAQI8545,2,PI,prob=0.89)))
F1AQISim8545 <- data.frame(cbind(simmeanAQI8545,PIAQI8545))
F1AQISim8545$PlotCN <- all8545cc1 $PlotCN
F1AQISim8545 <- F1AQISim8545 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8545) <- c("PlotCN","AQI","5CI","95CI")

simAQI2650 <- link(AQISplit1, data=all2650cc1)
simmeanAQI2650 <- data.frame(apply(simAQI2650,2,mean))
PIAQI2650 <- t(data.frame(apply(simAQI2650,2,PI,prob=0.89)))
F1AQISim2650 <- data.frame(cbind(simmeanAQI2650,PIAQI2650))
F1AQISim2650$PlotCN <- all2650cc1 $PlotCN
F1AQISim2650 <- F1AQISim2650 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2650) <- c("PlotCN","AQI","5CI","95CI")
simAQI4550 <- link(AQISplit1, data=all4550cc1)
simmeanAQI4550 <- data.frame(apply(simAQI4550,2,mean))
PIAQI4550 <- t(data.frame(apply(simAQI4550,2,PI,prob=0.89)))
F1AQISim4550 <- data.frame(cbind(simmeanAQI4550,PIAQI4550))
F1AQISim4550$PlotCN <- all4550cc1 $PlotCN
F1AQISim4550 <- F1AQISim4550 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4550) <- c("PlotCN","AQI","5CI","95CI")
simAQI8550 <- link(AQISplit1, data=all8550cc1)
simmeanAQI8550 <- data.frame(apply(simAQI8550,2,mean))
PIAQI8550 <- t(data.frame(apply(simAQI8550,2,PI,prob=0.89)))
F1AQISim8550 <- data.frame(cbind(simmeanAQI8550,PIAQI8550))
F1AQISim8550$PlotCN <- all8550cc1 $PlotCN
F1AQISim8550 <- F1AQISim8550 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8550) <- c("PlotCN","AQI","5CI","95CI")

simAQI2655 <- link(AQISplit1, data=all2655cc1)
simmeanAQI2655 <- data.frame(apply(simAQI2655,2,mean))
PIAQI2655 <- t(data.frame(apply(simAQI2655,2,PI,prob=0.89)))
F1AQISim2655 <- data.frame(cbind(simmeanAQI2655,PIAQI2655))
F1AQISim2655$PlotCN <- all2655cc1 $PlotCN
F1AQISim2655 <- F1AQISim2655 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2655) <- c("PlotCN","AQI","5CI","95CI")
simAQI4555 <- link(AQISplit1, data=all4555cc1)
simmeanAQI4555 <- data.frame(apply(simAQI4555,2,mean))
PIAQI4555 <- t(data.frame(apply(simAQI4555,2,PI,prob=0.89)))
F1AQISim4555 <- data.frame(cbind(simmeanAQI4555,PIAQI4555))
F1AQISim4555$PlotCN <- all4555cc1 $PlotCN
F1AQISim4555 <- F1AQISim4555 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4555) <- c("PlotCN","AQI","5CI","95CI")
simAQI8555 <- link(AQISplit1, data=all8555cc1)
simmeanAQI8555 <- data.frame(apply(simAQI8555,2,mean))
PIAQI8555 <- t(data.frame(apply(simAQI8555,2,PI,prob=0.89)))
F1AQISim8555 <- data.frame(cbind(simmeanAQI8555,PIAQI8555))
F1AQISim8555$PlotCN <- all8555cc1 $PlotCN
F1AQISim8555 <- F1AQISim8555 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8555) <- c("PlotCN","AQI","5CI","95CI")

simAQI2660 <- link(AQISplit1, data=all2660cc1)
simmeanAQI2660 <- data.frame(apply(simAQI2660,2,mean))
PIAQI2660 <- t(data.frame(apply(simAQI2660,2,PI,prob=0.89)))
F1AQISim2660 <- data.frame(cbind(simmeanAQI2660,PIAQI2660))
F1AQISim2660$PlotCN <- all2660cc1 $PlotCN
F1AQISim2660 <- F1AQISim2660 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2660) <- c("PlotCN","AQI","5CI","95CI")
simAQI4560 <- link(AQISplit1, data=all4560cc1)
simmeanAQI4560 <- data.frame(apply(simAQI4560,2,mean))
PIAQI4560 <- t(data.frame(apply(simAQI4560,2,PI,prob=0.89)))
F1AQISim4560 <- data.frame(cbind(simmeanAQI4560,PIAQI4560))
F1AQISim4560$PlotCN <- all4560cc1 $PlotCN
F1AQISim4560 <- F1AQISim4560 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4560) <- c("PlotCN","AQI","5CI","95CI")
simAQI8560 <- link(AQISplit1, data=all8560cc1)
simmeanAQI8560 <- data.frame(apply(simAQI8560,2,mean))
PIAQI8560 <- t(data.frame(apply(simAQI8560,2,PI,prob=0.89)))
F1AQISim8560 <- data.frame(cbind(simmeanAQI8560,PIAQI8560))
F1AQISim8560$PlotCN <- all8560cc1 $PlotCN
F1AQISim8560 <- F1AQISim8560 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8560) <- c("PlotCN","AQI","5CI","95CI")

simAQI2665 <- link(AQISplit1, data=all2665cc1)
simmeanAQI2665 <- data.frame(apply(simAQI2665,2,mean))
PIAQI2665 <- t(data.frame(apply(simAQI2665,2,PI,prob=0.89)))
F1AQISim2665 <- data.frame(cbind(simmeanAQI2665,PIAQI2665))
F1AQISim2665$PlotCN <- all2665cc1 $PlotCN
F1AQISim2665 <- F1AQISim2665 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2665) <- c("PlotCN","AQI","5CI","95CI")
simAQI4565 <- link(AQISplit1, data=all4565cc1)
simmeanAQI4565 <- data.frame(apply(simAQI4565,2,mean))
PIAQI4565 <- t(data.frame(apply(simAQI4565,2,PI,prob=0.89)))
F1AQISim4565 <- data.frame(cbind(simmeanAQI4565,PIAQI4565))
F1AQISim4565$PlotCN <- all4565cc1 $PlotCN
F1AQISim4565 <- F1AQISim4565 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4565) <- c("PlotCN","AQI","5CI","95CI")
simAQI8565 <- link(AQISplit1, data=all8565cc1)
simmeanAQI8565 <- data.frame(apply(simAQI8565,2,mean))
PIAQI8565 <- t(data.frame(apply(simAQI8565,2,PI,prob=0.89)))
F1AQISim8565 <- data.frame(cbind(simmeanAQI8565,PIAQI8565))
F1AQISim8565$PlotCN <- all8565cc1 $PlotCN
F1AQISim8565 <- F1AQISim8565 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8565) <- c("PlotCN","AQI","5CI","95CI")

simAQI2670 <- link(AQISplit1, data=all2670cc1)
simmeanAQI2670 <- data.frame(apply(simAQI2670,2,mean))
PIAQI2670 <- t(data.frame(apply(simAQI2670,2,PI,prob=0.89)))
F1AQISim2670 <- data.frame(cbind(simmeanAQI2670,PIAQI2670))
F1AQISim2670$PlotCN <- all2670cc1 $PlotCN
F1AQISim2670 <- F1AQISim2670 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2670) <- c("PlotCN","AQI","5CI","95CI")
simAQI4570 <- link(AQISplit1, data=all4570cc1)
simmeanAQI4570 <- data.frame(apply(simAQI4570,2,mean))
PIAQI4570 <- t(data.frame(apply(simAQI4570,2,PI,prob=0.89)))
F1AQISim4570 <- data.frame(cbind(simmeanAQI4570,PIAQI4570))
F1AQISim4570$PlotCN <- all4570cc1 $PlotCN
F1AQISim4570 <- F1AQISim4570 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4570) <- c("PlotCN","AQI","5CI","95CI")
simAQI8570 <- link(AQISplit1, data=all8570cc1)
simmeanAQI8570 <- data.frame(apply(simAQI8570,2,mean))
PIAQI8570 <- t(data.frame(apply(simAQI8570,2,PI,prob=0.89)))
F1AQISim8570 <- data.frame(cbind(simmeanAQI8570,PIAQI8570))
F1AQISim8570$PlotCN <- all8570cc1 $PlotCN
F1AQISim8570 <- F1AQISim8570 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8570) <- c("PlotCN","AQI","5CI","95CI")

simAQI2675 <- link(AQISplit1, data=all2675cc1)
simmeanAQI2675 <- data.frame(apply(simAQI2675,2,mean))
PIAQI2675 <- t(data.frame(apply(simAQI2675,2,PI,prob=0.89)))
F1AQISim2675 <- data.frame(cbind(simmeanAQI2675,PIAQI2675))
F1AQISim2675$PlotCN <- all2675cc1 $PlotCN
F1AQISim2675 <- F1AQISim2675 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2675) <- c("PlotCN","AQI","5CI","95CI")
simAQI4575 <- link(AQISplit1, data=all4575cc1)
simmeanAQI4575 <- data.frame(apply(simAQI4575,2,mean))
PIAQI4575 <- t(data.frame(apply(simAQI4575,2,PI,prob=0.89)))
F1AQISim4575 <- data.frame(cbind(simmeanAQI4575,PIAQI4575))
F1AQISim4575$PlotCN <- all4575cc1 $PlotCN
F1AQISim4575 <- F1AQISim4575 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4575) <- c("PlotCN","AQI","5CI","95CI")
simAQI8575 <- link(AQISplit1, data=all8575cc1)
simmeanAQI8575 <- data.frame(apply(simAQI8575,2,mean))
PIAQI8575 <- t(data.frame(apply(simAQI8575,2,PI,prob=0.89)))
F1AQISim8575 <- data.frame(cbind(simmeanAQI8575,PIAQI8575))
F1AQISim8575$PlotCN <- all8575cc1 $PlotCN
F1AQISim8575 <- F1AQISim8575 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8575) <- c("PlotCN","AQI","5CI","95CI")

simAQI2680 <- link(AQISplit1, data=all2680cc1)
simmeanAQI2680 <- data.frame(apply(simAQI2680,2,mean))
PIAQI2680 <- t(data.frame(apply(simAQI2680,2,PI,prob=0.89)))
F1AQISim2680 <- data.frame(cbind(simmeanAQI2680,PIAQI2680))
F1AQISim2680$PlotCN <- all2680cc1 $PlotCN
F1AQISim2680 <- F1AQISim2680 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim2680) <- c("PlotCN","AQI","5CI","95CI")
simAQI4580 <- link(AQISplit1, data=all4580cc1)
simmeanAQI4580 <- data.frame(apply(simAQI4580,2,mean))
PIAQI4580 <- t(data.frame(apply(simAQI4580,2,PI,prob=0.89)))
F1AQISim4580 <- data.frame(cbind(simmeanAQI4580,PIAQI4580))
F1AQISim4580$PlotCN <- all4580cc1 $PlotCN
F1AQISim4580 <- F1AQISim4580 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim4580) <- c("PlotCN","AQI","5CI","95CI")
simAQI8580 <- link(AQISplit1, data=all8580cc1)
simmeanAQI8580 <- data.frame(apply(simAQI8580,2,mean))
PIAQI8580 <- t(data.frame(apply(simAQI8580,2,PI,prob=0.89)))
F1AQISim8580 <- data.frame(cbind(simmeanAQI8580,PIAQI8580))
F1AQISim8580$PlotCN <- all8580cc1 $PlotCN
F1AQISim8580 <- F1AQISim8580 %>% dplyr::select(PlotCN, everything())
colnames(F1AQISim8580) <- c("PlotCN","AQI","5CI","95CI")


#create vector of new colnames
AQISimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("AQI", "5CI", "95CI"), x)))
AQISimnames<- append(AQISimnames,"PlotCN",after=0)

#group data by RCP pathway
F1AQISim26Time <- F1AQISim2625 %>% cbind(F1AQISim2630[,2:4]) %>%
  cbind(F1AQISim2635[,2:4]) %>% cbind(F1AQISim2640[,2:4]) %>% cbind(F1AQISim2645[,2:4]) %>%
  cbind(F1AQISim2650[,2:4]) %>% cbind(F1AQISim2655[,2:4]) %>% cbind(F1AQISim2660[,2:4]) %>%
  cbind(F1AQISim2665[,2:4]) %>% cbind(F1AQISim2670[,2:4]) %>% cbind(F1AQISim2675[,2:4]) %>%
  cbind(F1AQISim2680[,2:4])
colnames(F1AQISim26Time) <- AQISimnames
write.csv(F1AQISim26Time,file="F1AQIPred26T.csv")

F1AQISim45Time <- F1AQISim4525 %>% cbind(F1AQISim4530[,2:4]) %>%
  cbind(F1AQISim4535[,2:4]) %>% cbind(F1AQISim4540[,2:4]) %>% cbind(F1AQISim4545[,2:4]) %>%
  cbind(F1AQISim4550[,2:4]) %>% cbind(F1AQISim4555[,2:4]) %>% cbind(F1AQISim4560[,2:4]) %>%
  cbind(F1AQISim4565[,2:4]) %>% cbind(F1AQISim4570[,2:4]) %>% cbind(F1AQISim4575[,2:4]) %>%
  cbind(F1AQISim4580[,2:4])
colnames(F1AQISim45Time) <- AQISimnames
write.csv(F1AQISim45Time,file="F1AQIPred45T.csv")

F1AQISim85Time <- F1AQISim8525 %>% cbind(F1AQISim8530[,2:4]) %>%
  cbind(F1AQISim8535[,2:4]) %>% cbind(F1AQISim8540[,2:4]) %>% cbind(F1AQISim8545[,2:4]) %>%
  cbind(F1AQISim8550[,2:4]) %>% cbind(F1AQISim8555[,2:4]) %>% cbind(F1AQISim8560[,2:4]) %>%
  cbind(F1AQISim8565[,2:4]) %>% cbind(F1AQISim8570[,2:4]) %>% cbind(F1AQISim8575[,2:4]) %>%
  cbind(F1AQISim8580[,2:4])
colnames(F1AQISim85Time) <- AQISimnames
write.csv(F1AQISim85Time,file="F1AQIPred85T.csv")


#fgroup 5
simAQI2625 <- link(AQISplit5, data=all2625cc5 )
simmeanAQI2625 <- data.frame(apply(simAQI2625,2,mean))
PIAQI2625 <- t(data.frame(apply(simAQI2625,2,PI,prob=0.89)))
F5AQISim2625 <- data.frame(cbind(simmeanAQI2625,PIAQI2625))
F5AQISim2625$PlotCN <- all2625cc5  $PlotCN
F5AQISim2625 <- F5AQISim2625 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2625) <- c("PlotCN","AQI","5CI","95CI")
simAQI4525 <- link(AQISplit5, data=all4525cc5 )
simmeanAQI4525 <- data.frame(apply(simAQI4525,2,mean))
PIAQI4525 <- t(data.frame(apply(simAQI4525,2,PI,prob=0.89)))
F5AQISim4525 <- data.frame(cbind(simmeanAQI4525,PIAQI4525))
F5AQISim4525$PlotCN <- all4525cc5  $PlotCN
F5AQISim4525 <- F5AQISim4525 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4525) <- c("PlotCN","AQI","5CI","95CI")
simAQI8525 <- link(AQISplit5, data=all8525cc5 )
simmeanAQI8525 <- data.frame(apply(simAQI8525,2,mean))
PIAQI8525 <- t(data.frame(apply(simAQI8525,2,PI,prob=0.89)))
F5AQISim8525 <- data.frame(cbind(simmeanAQI8525,PIAQI8525))
F5AQISim8525$PlotCN <- all8525cc5  $PlotCN
F5AQISim8525 <- F5AQISim8525 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8525) <- c("PlotCN","AQI","5CI","95CI")

simAQI2630 <- link(AQISplit5, data=all2630cc5 )
simmeanAQI2630 <- data.frame(apply(simAQI2630,2,mean))
PIAQI2630 <- t(data.frame(apply(simAQI2630,2,PI,prob=0.89)))
F5AQISim2630 <- data.frame(cbind(simmeanAQI2630,PIAQI2630))
F5AQISim2630$PlotCN <- all2630cc5  $PlotCN
F5AQISim2630 <- F5AQISim2630 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2630) <- c("PlotCN","AQI","5CI","95CI")
simAQI4530 <- link(AQISplit5, data=all4530cc5 )
simmeanAQI4530 <- data.frame(apply(simAQI4530,2,mean))
PIAQI4530 <- t(data.frame(apply(simAQI4530,2,PI,prob=0.89)))
F5AQISim4530 <- data.frame(cbind(simmeanAQI4530,PIAQI4530))
F5AQISim4530$PlotCN <- all4530cc5  $PlotCN
F5AQISim4530 <- F5AQISim4530 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4530) <- c("PlotCN","AQI","5CI","95CI")
simAQI8530 <- link(AQISplit5, data=all8530cc5 )
simmeanAQI8530 <- data.frame(apply(simAQI8530,2,mean))
PIAQI8530 <- t(data.frame(apply(simAQI8530,2,PI,prob=0.89)))
F5AQISim8530 <- data.frame(cbind(simmeanAQI8530,PIAQI8530))
F5AQISim8530$PlotCN <- all8530cc5  $PlotCN
F5AQISim8530 <- F5AQISim8530 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8530) <- c("PlotCN","AQI","5CI","95CI")

simAQI2635 <- link(AQISplit5, data=all2635cc5 )
simmeanAQI2635 <- data.frame(apply(simAQI2635,2,mean))
PIAQI2635 <- t(data.frame(apply(simAQI2635,2,PI,prob=0.89)))
F5AQISim2635 <- data.frame(cbind(simmeanAQI2635,PIAQI2635))
F5AQISim2635$PlotCN <- all2635cc5  $PlotCN
F5AQISim2635 <- F5AQISim2635 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2635) <- c("PlotCN","AQI","5CI","95CI")
simAQI4535 <- link(AQISplit5, data=all4535cc5 )
simmeanAQI4535 <- data.frame(apply(simAQI4535,2,mean))
PIAQI4535 <- t(data.frame(apply(simAQI4535,2,PI,prob=0.89)))
F5AQISim4535 <- data.frame(cbind(simmeanAQI4535,PIAQI4535))
F5AQISim4535$PlotCN <- all4535cc5  $PlotCN
F5AQISim4535 <- F5AQISim4535 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4535) <- c("PlotCN","AQI","5CI","95CI")
simAQI8535 <- link(AQISplit5, data=all8535cc5 )
simmeanAQI8535 <- data.frame(apply(simAQI8535,2,mean))
PIAQI8535 <- t(data.frame(apply(simAQI8535,2,PI,prob=0.89)))
F5AQISim8535 <- data.frame(cbind(simmeanAQI8535,PIAQI8535))
F5AQISim8535$PlotCN <- all8535cc5  $PlotCN
F5AQISim8535 <- F5AQISim8535 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8535) <- c("PlotCN","AQI","5CI","95CI")

simAQI2640 <- link(AQISplit5, data=all2640cc5 )
simmeanAQI2640 <- data.frame(apply(simAQI2640,2,mean))
PIAQI2640 <- t(data.frame(apply(simAQI2640,2,PI,prob=0.89)))
F5AQISim2640 <- data.frame(cbind(simmeanAQI2640,PIAQI2640))
F5AQISim2640$PlotCN <- all2640cc5  $PlotCN
F5AQISim2640 <- F5AQISim2640 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2640) <- c("PlotCN","AQI","5CI","95CI")
simAQI4540 <- link(AQISplit5, data=all4540cc5 )
simmeanAQI4540 <- data.frame(apply(simAQI4540,2,mean))
PIAQI4540 <- t(data.frame(apply(simAQI4540,2,PI,prob=0.89)))
F5AQISim4540 <- data.frame(cbind(simmeanAQI4540,PIAQI4540))
F5AQISim4540$PlotCN <- all4540cc5  $PlotCN
F5AQISim4540 <- F5AQISim4540 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4540) <- c("PlotCN","AQI","5CI","95CI")
simAQI8540 <- link(AQISplit5, data=all8540cc5 )
simmeanAQI8540 <- data.frame(apply(simAQI8540,2,mean))
PIAQI8540 <- t(data.frame(apply(simAQI8540,2,PI,prob=0.89)))
F5AQISim8540 <- data.frame(cbind(simmeanAQI8540,PIAQI8540))
F5AQISim8540$PlotCN <- all8540cc5  $PlotCN
F5AQISim8540 <- F5AQISim8540 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8540) <- c("PlotCN","AQI","5CI","95CI")

simAQI2645 <- link(AQISplit5, data=all2645cc5 )
simmeanAQI2645 <- data.frame(apply(simAQI2645,2,mean))
PIAQI2645 <- t(data.frame(apply(simAQI2645,2,PI,prob=0.89)))
F5AQISim2645 <- data.frame(cbind(simmeanAQI2645,PIAQI2645))
F5AQISim2645$PlotCN <- all2645cc5  $PlotCN
F5AQISim2645 <- F5AQISim2645 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2645) <- c("PlotCN","AQI","5CI","95CI")
simAQI4545 <- link(AQISplit5, data=all4545cc5 )
simmeanAQI4545 <- data.frame(apply(simAQI4545,2,mean))
PIAQI4545 <- t(data.frame(apply(simAQI4545,2,PI,prob=0.89)))
F5AQISim4545 <- data.frame(cbind(simmeanAQI4545,PIAQI4545))
F5AQISim4545$PlotCN <- all4545cc5  $PlotCN
F5AQISim4545 <- F5AQISim4545 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4545) <- c("PlotCN","AQI","5CI","95CI")
simAQI8545 <- link(AQISplit5, data=all8545cc5 )
simmeanAQI8545 <- data.frame(apply(simAQI8545,2,mean))
PIAQI8545 <- t(data.frame(apply(simAQI8545,2,PI,prob=0.89)))
F5AQISim8545 <- data.frame(cbind(simmeanAQI8545,PIAQI8545))
F5AQISim8545$PlotCN <- all8545cc5  $PlotCN
F5AQISim8545 <- F5AQISim8545 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8545) <- c("PlotCN","AQI","5CI","95CI")

simAQI2650 <- link(AQISplit5, data=all2650cc5 )
simmeanAQI2650 <- data.frame(apply(simAQI2650,2,mean))
PIAQI2650 <- t(data.frame(apply(simAQI2650,2,PI,prob=0.89)))
F5AQISim2650 <- data.frame(cbind(simmeanAQI2650,PIAQI2650))
F5AQISim2650$PlotCN <- all2650cc5  $PlotCN
F5AQISim2650 <- F5AQISim2650 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2650) <- c("PlotCN","AQI","5CI","95CI")
simAQI4550 <- link(AQISplit5, data=all4550cc5 )
simmeanAQI4550 <- data.frame(apply(simAQI4550,2,mean))
PIAQI4550 <- t(data.frame(apply(simAQI4550,2,PI,prob=0.89)))
F5AQISim4550 <- data.frame(cbind(simmeanAQI4550,PIAQI4550))
F5AQISim4550$PlotCN <- all4550cc5  $PlotCN
F5AQISim4550 <- F5AQISim4550 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4550) <- c("PlotCN","AQI","5CI","95CI")
simAQI8550 <- link(AQISplit5, data=all8550cc5 )
simmeanAQI8550 <- data.frame(apply(simAQI8550,2,mean))
PIAQI8550 <- t(data.frame(apply(simAQI8550,2,PI,prob=0.89)))
F5AQISim8550 <- data.frame(cbind(simmeanAQI8550,PIAQI8550))
F5AQISim8550$PlotCN <- all8550cc5  $PlotCN
F5AQISim8550 <- F5AQISim8550 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8550) <- c("PlotCN","AQI","5CI","95CI")

simAQI2655 <- link(AQISplit5, data=all2655cc5 )
simmeanAQI2655 <- data.frame(apply(simAQI2655,2,mean))
PIAQI2655 <- t(data.frame(apply(simAQI2655,2,PI,prob=0.89)))
F5AQISim2655 <- data.frame(cbind(simmeanAQI2655,PIAQI2655))
F5AQISim2655$PlotCN <- all2655cc5  $PlotCN
F5AQISim2655 <- F5AQISim2655 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2655) <- c("PlotCN","AQI","5CI","95CI")
simAQI4555 <- link(AQISplit5, data=all4555cc5 )
simmeanAQI4555 <- data.frame(apply(simAQI4555,2,mean))
PIAQI4555 <- t(data.frame(apply(simAQI4555,2,PI,prob=0.89)))
F5AQISim4555 <- data.frame(cbind(simmeanAQI4555,PIAQI4555))
F5AQISim4555$PlotCN <- all4555cc5  $PlotCN
F5AQISim4555 <- F5AQISim4555 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4555) <- c("PlotCN","AQI","5CI","95CI")
simAQI8555 <- link(AQISplit5, data=all8555cc5 )
simmeanAQI8555 <- data.frame(apply(simAQI8555,2,mean))
PIAQI8555 <- t(data.frame(apply(simAQI8555,2,PI,prob=0.89)))
F5AQISim8555 <- data.frame(cbind(simmeanAQI8555,PIAQI8555))
F5AQISim8555$PlotCN <- all8555cc5  $PlotCN
F5AQISim8555 <- F5AQISim8555 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8555) <- c("PlotCN","AQI","5CI","95CI")

simAQI2660 <- link(AQISplit5, data=all2660cc5 )
simmeanAQI2660 <- data.frame(apply(simAQI2660,2,mean))
PIAQI2660 <- t(data.frame(apply(simAQI2660,2,PI,prob=0.89)))
F5AQISim2660 <- data.frame(cbind(simmeanAQI2660,PIAQI2660))
F5AQISim2660$PlotCN <- all2660cc5  $PlotCN
F5AQISim2660 <- F5AQISim2660 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2660) <- c("PlotCN","AQI","5CI","95CI")
simAQI4560 <- link(AQISplit5, data=all4560cc5 )
simmeanAQI4560 <- data.frame(apply(simAQI4560,2,mean))
PIAQI4560 <- t(data.frame(apply(simAQI4560,2,PI,prob=0.89)))
F5AQISim4560 <- data.frame(cbind(simmeanAQI4560,PIAQI4560))
F5AQISim4560$PlotCN <- all4560cc5  $PlotCN
F5AQISim4560 <- F5AQISim4560 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4560) <- c("PlotCN","AQI","5CI","95CI")
simAQI8560 <- link(AQISplit5, data=all8560cc5 )
simmeanAQI8560 <- data.frame(apply(simAQI8560,2,mean))
PIAQI8560 <- t(data.frame(apply(simAQI8560,2,PI,prob=0.89)))
F5AQISim8560 <- data.frame(cbind(simmeanAQI8560,PIAQI8560))
F5AQISim8560$PlotCN <- all8560cc5  $PlotCN
F5AQISim8560 <- F5AQISim8560 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8560) <- c("PlotCN","AQI","5CI","95CI")

simAQI2665 <- link(AQISplit5, data=all2665cc5 )
simmeanAQI2665 <- data.frame(apply(simAQI2665,2,mean))
PIAQI2665 <- t(data.frame(apply(simAQI2665,2,PI,prob=0.89)))
F5AQISim2665 <- data.frame(cbind(simmeanAQI2665,PIAQI2665))
F5AQISim2665$PlotCN <- all2665cc5  $PlotCN
F5AQISim2665 <- F5AQISim2665 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2665) <- c("PlotCN","AQI","5CI","95CI")
simAQI4565 <- link(AQISplit5, data=all4565cc5 )
simmeanAQI4565 <- data.frame(apply(simAQI4565,2,mean))
PIAQI4565 <- t(data.frame(apply(simAQI4565,2,PI,prob=0.89)))
F5AQISim4565 <- data.frame(cbind(simmeanAQI4565,PIAQI4565))
F5AQISim4565$PlotCN <- all4565cc5  $PlotCN
F5AQISim4565 <- F5AQISim4565 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4565) <- c("PlotCN","AQI","5CI","95CI")
simAQI8565 <- link(AQISplit5, data=all8565cc5 )
simmeanAQI8565 <- data.frame(apply(simAQI8565,2,mean))
PIAQI8565 <- t(data.frame(apply(simAQI8565,2,PI,prob=0.89)))
F5AQISim8565 <- data.frame(cbind(simmeanAQI8565,PIAQI8565))
F5AQISim8565$PlotCN <- all8565cc5  $PlotCN
F5AQISim8565 <- F5AQISim8565 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8565) <- c("PlotCN","AQI","5CI","95CI")

simAQI2670 <- link(AQISplit5, data=all2670cc5 )
simmeanAQI2670 <- data.frame(apply(simAQI2670,2,mean))
PIAQI2670 <- t(data.frame(apply(simAQI2670,2,PI,prob=0.89)))
F5AQISim2670 <- data.frame(cbind(simmeanAQI2670,PIAQI2670))
F5AQISim2670$PlotCN <- all2670cc5  $PlotCN
F5AQISim2670 <- F5AQISim2670 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2670) <- c("PlotCN","AQI","5CI","95CI")
simAQI4570 <- link(AQISplit5, data=all4570cc5 )
simmeanAQI4570 <- data.frame(apply(simAQI4570,2,mean))
PIAQI4570 <- t(data.frame(apply(simAQI4570,2,PI,prob=0.89)))
F5AQISim4570 <- data.frame(cbind(simmeanAQI4570,PIAQI4570))
F5AQISim4570$PlotCN <- all4570cc5  $PlotCN
F5AQISim4570 <- F5AQISim4570 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4570) <- c("PlotCN","AQI","5CI","95CI")
simAQI8570 <- link(AQISplit5, data=all8570cc5 )
simmeanAQI8570 <- data.frame(apply(simAQI8570,2,mean))
PIAQI8570 <- t(data.frame(apply(simAQI8570,2,PI,prob=0.89)))
F5AQISim8570 <- data.frame(cbind(simmeanAQI8570,PIAQI8570))
F5AQISim8570$PlotCN <- all8570cc5  $PlotCN
F5AQISim8570 <- F5AQISim8570 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8570) <- c("PlotCN","AQI","5CI","95CI")

simAQI2675 <- link(AQISplit5, data=all2675cc5 )
simmeanAQI2675 <- data.frame(apply(simAQI2675,2,mean))
PIAQI2675 <- t(data.frame(apply(simAQI2675,2,PI,prob=0.89)))
F5AQISim2675 <- data.frame(cbind(simmeanAQI2675,PIAQI2675))
F5AQISim2675$PlotCN <- all2675cc5  $PlotCN
F5AQISim2675 <- F5AQISim2675 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2675) <- c("PlotCN","AQI","5CI","95CI")
simAQI4575 <- link(AQISplit5, data=all4575cc5 )
simmeanAQI4575 <- data.frame(apply(simAQI4575,2,mean))
PIAQI4575 <- t(data.frame(apply(simAQI4575,2,PI,prob=0.89)))
F5AQISim4575 <- data.frame(cbind(simmeanAQI4575,PIAQI4575))
F5AQISim4575$PlotCN <- all4575cc5  $PlotCN
F5AQISim4575 <- F5AQISim4575 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4575) <- c("PlotCN","AQI","5CI","95CI")
simAQI8575 <- link(AQISplit5, data=all8575cc5 )
simmeanAQI8575 <- data.frame(apply(simAQI8575,2,mean))
PIAQI8575 <- t(data.frame(apply(simAQI8575,2,PI,prob=0.89)))
F5AQISim8575 <- data.frame(cbind(simmeanAQI8575,PIAQI8575))
F5AQISim8575$PlotCN <- all8575cc5  $PlotCN
F5AQISim8575 <- F5AQISim8575 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8575) <- c("PlotCN","AQI","5CI","95CI")

simAQI2680 <- link(AQISplit5, data=all2680cc5 )
simmeanAQI2680 <- data.frame(apply(simAQI2680,2,mean))
PIAQI2680 <- t(data.frame(apply(simAQI2680,2,PI,prob=0.89)))
F5AQISim2680 <- data.frame(cbind(simmeanAQI2680,PIAQI2680))
F5AQISim2680$PlotCN <- all2680cc5  $PlotCN
F5AQISim2680 <- F5AQISim2680 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim2680) <- c("PlotCN","AQI","5CI","95CI")
simAQI4580 <- link(AQISplit5, data=all4580cc5 )
simmeanAQI4580 <- data.frame(apply(simAQI4580,2,mean))
PIAQI4580 <- t(data.frame(apply(simAQI4580,2,PI,prob=0.89)))
F5AQISim4580 <- data.frame(cbind(simmeanAQI4580,PIAQI4580))
F5AQISim4580$PlotCN <- all4580cc5  $PlotCN
F5AQISim4580 <- F5AQISim4580 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim4580) <- c("PlotCN","AQI","5CI","95CI")
simAQI8580 <- link(AQISplit5, data=all8580cc5 )
simmeanAQI8580 <- data.frame(apply(simAQI8580,2,mean))
PIAQI8580 <- t(data.frame(apply(simAQI8580,2,PI,prob=0.89)))
F5AQISim8580 <- data.frame(cbind(simmeanAQI8580,PIAQI8580))
F5AQISim8580$PlotCN <- all8580cc5  $PlotCN
F5AQISim8580 <- F5AQISim8580 %>% dplyr::select(PlotCN, everything())
colnames(F5AQISim8580) <- c("PlotCN","AQI","5CI","95CI")

#group data by RCP pathway
F5AQISim26 <- F5AQISim2625 %>% cbind(F5AQISim2630[,2:4]) %>%
  cbind(F5AQISim2635[,2:4]) %>% cbind(F5AQISim2640[,2:4]) %>% cbind(F5AQISim2645[,2:4]) %>%
  cbind(F5AQISim2650[,2:4]) %>% cbind(F5AQISim2655[,2:4]) %>% cbind(F5AQISim2660[,2:4]) %>%
  cbind(F5AQISim2665[,2:4]) %>% cbind(F5AQISim2670[,2:4]) %>% cbind(F5AQISim2675[,2:4]) %>%
  cbind(F5AQISim2680[,2:4])
colnames(F5AQISim26) <- AQISimnames
write.csv(F5AQISim26,file="F5AQIPred26T.csv")

F5AQISim45 <- F5AQISim4525 %>% cbind(F5AQISim4530[,2:4]) %>%
  cbind(F5AQISim4535[,2:4]) %>% cbind(F5AQISim4540[,2:4]) %>% cbind(F5AQISim4545[,2:4]) %>%
  cbind(F5AQISim4550[,2:4]) %>% cbind(F5AQISim4555[,2:4]) %>% cbind(F5AQISim4560[,2:4]) %>%
  cbind(F5AQISim4565[,2:4]) %>% cbind(F5AQISim4570[,2:4]) %>% cbind(F5AQISim4575[,2:4]) %>%
  cbind(F5AQISim4580[,2:4])
colnames(F5AQISim45) <- AQISimnames
write.csv(F5AQISim45,file="F5AQIPred45T.csv")

F5AQISim85 <- F5AQISim8525 %>% cbind(F5AQISim8530[,2:4]) %>%
  cbind(F5AQISim8535[,2:4]) %>% cbind(F5AQISim8540[,2:4]) %>% cbind(F5AQISim8545[,2:4]) %>%
  cbind(F5AQISim8550[,2:4]) %>% cbind(F5AQISim8555[,2:4]) %>% cbind(F5AQISim8560[,2:4]) %>%
  cbind(F5AQISim8565[,2:4]) %>% cbind(F5AQISim8570[,2:4]) %>% cbind(F5AQISim8575[,2:4]) %>%
  cbind(F5AQISim8580[,2:4])
colnames(F5AQISim85) <- AQISimnames
write.csv(F5AQISim85,file="F5AQIPred85T.csv")


#fgroup 20
simAQI2625 <- link(AQISplit20, data=all2625cc20 )
simmeanAQI2625 <- data.frame(apply(simAQI2625,2,mean))
PIAQI2625 <- t(data.frame(apply(simAQI2625,2,PI,prob=0.89)))
F20AQISim2625 <- data.frame(cbind(simmeanAQI2625,PIAQI2625))
F20AQISim2625$PlotCN <- all2625cc20  $PlotCN
F20AQISim2625 <- F20AQISim2625 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2625) <- c("PlotCN","AQI","5CI","95CI")
simAQI4525 <- link(AQISplit20, data=all4525cc20 )
simmeanAQI4525 <- data.frame(apply(simAQI4525,2,mean))
PIAQI4525 <- t(data.frame(apply(simAQI4525,2,PI,prob=0.89)))
F20AQISim4525 <- data.frame(cbind(simmeanAQI4525,PIAQI4525))
F20AQISim4525$PlotCN <- all4525cc20  $PlotCN
F20AQISim4525 <- F20AQISim4525 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4525) <- c("PlotCN","AQI","5CI","95CI")
simAQI8525 <- link(AQISplit20, data=all8525cc20 )
simmeanAQI8525 <- data.frame(apply(simAQI8525,2,mean))
PIAQI8525 <- t(data.frame(apply(simAQI8525,2,PI,prob=0.89)))
F20AQISim8525 <- data.frame(cbind(simmeanAQI8525,PIAQI8525))
F20AQISim8525$PlotCN <- all8525cc20  $PlotCN
F20AQISim8525 <- F20AQISim8525 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8525) <- c("PlotCN","AQI","5CI","95CI")

simAQI2630 <- link(AQISplit20, data=all2630cc20 )
simmeanAQI2630 <- data.frame(apply(simAQI2630,2,mean))
PIAQI2630 <- t(data.frame(apply(simAQI2630,2,PI,prob=0.89)))
F20AQISim2630 <- data.frame(cbind(simmeanAQI2630,PIAQI2630))
F20AQISim2630$PlotCN <- all2630cc20  $PlotCN
F20AQISim2630 <- F20AQISim2630 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2630) <- c("PlotCN","AQI","5CI","95CI")
simAQI4530 <- link(AQISplit20, data=all4530cc20 )
simmeanAQI4530 <- data.frame(apply(simAQI4530,2,mean))
PIAQI4530 <- t(data.frame(apply(simAQI4530,2,PI,prob=0.89)))
F20AQISim4530 <- data.frame(cbind(simmeanAQI4530,PIAQI4530))
F20AQISim4530$PlotCN <- all4530cc20  $PlotCN
F20AQISim4530 <- F20AQISim4530 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4530) <- c("PlotCN","AQI","5CI","95CI")
simAQI8530 <- link(AQISplit20, data=all8530cc20 )
simmeanAQI8530 <- data.frame(apply(simAQI8530,2,mean))
PIAQI8530 <- t(data.frame(apply(simAQI8530,2,PI,prob=0.89)))
F20AQISim8530 <- data.frame(cbind(simmeanAQI8530,PIAQI8530))
F20AQISim8530$PlotCN <- all8530cc20  $PlotCN
F20AQISim8530 <- F20AQISim8530 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8530) <- c("PlotCN","AQI","5CI","95CI")

simAQI2635 <- link(AQISplit20, data=all2635cc20 )
simmeanAQI2635 <- data.frame(apply(simAQI2635,2,mean))
PIAQI2635 <- t(data.frame(apply(simAQI2635,2,PI,prob=0.89)))
F20AQISim2635 <- data.frame(cbind(simmeanAQI2635,PIAQI2635))
F20AQISim2635$PlotCN <- all2635cc20  $PlotCN
F20AQISim2635 <- F20AQISim2635 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2635) <- c("PlotCN","AQI","5CI","95CI")
simAQI4535 <- link(AQISplit20, data=all4535cc20 )
simmeanAQI4535 <- data.frame(apply(simAQI4535,2,mean))
PIAQI4535 <- t(data.frame(apply(simAQI4535,2,PI,prob=0.89)))
F20AQISim4535 <- data.frame(cbind(simmeanAQI4535,PIAQI4535))
F20AQISim4535$PlotCN <- all4535cc20  $PlotCN
F20AQISim4535 <- F20AQISim4535 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4535) <- c("PlotCN","AQI","5CI","95CI")
simAQI8535 <- link(AQISplit20, data=all8535cc20 )
simmeanAQI8535 <- data.frame(apply(simAQI8535,2,mean))
PIAQI8535 <- t(data.frame(apply(simAQI8535,2,PI,prob=0.89)))
F20AQISim8535 <- data.frame(cbind(simmeanAQI8535,PIAQI8535))
F20AQISim8535$PlotCN <- all8535cc20  $PlotCN
F20AQISim8535 <- F20AQISim8535 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8535) <- c("PlotCN","AQI","5CI","95CI")

simAQI2640 <- link(AQISplit20, data=all2640cc20 )
simmeanAQI2640 <- data.frame(apply(simAQI2640,2,mean))
PIAQI2640 <- t(data.frame(apply(simAQI2640,2,PI,prob=0.89)))
F20AQISim2640 <- data.frame(cbind(simmeanAQI2640,PIAQI2640))
F20AQISim2640$PlotCN <- all2640cc20  $PlotCN
F20AQISim2640 <- F20AQISim2640 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2640) <- c("PlotCN","AQI","5CI","95CI")
simAQI4540 <- link(AQISplit20, data=all4540cc20 )
simmeanAQI4540 <- data.frame(apply(simAQI4540,2,mean))
PIAQI4540 <- t(data.frame(apply(simAQI4540,2,PI,prob=0.89)))
F20AQISim4540 <- data.frame(cbind(simmeanAQI4540,PIAQI4540))
F20AQISim4540$PlotCN <- all4540cc20  $PlotCN
F20AQISim4540 <- F20AQISim4540 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4540) <- c("PlotCN","AQI","5CI","95CI")
simAQI8540 <- link(AQISplit20, data=all8540cc20 )
simmeanAQI8540 <- data.frame(apply(simAQI8540,2,mean))
PIAQI8540 <- t(data.frame(apply(simAQI8540,2,PI,prob=0.89)))
F20AQISim8540 <- data.frame(cbind(simmeanAQI8540,PIAQI8540))
F20AQISim8540$PlotCN <- all8540cc20  $PlotCN
F20AQISim8540 <- F20AQISim8540 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8540) <- c("PlotCN","AQI","5CI","95CI")

simAQI2645 <- link(AQISplit20, data=all2645cc20 )
simmeanAQI2645 <- data.frame(apply(simAQI2645,2,mean))
PIAQI2645 <- t(data.frame(apply(simAQI2645,2,PI,prob=0.89)))
F20AQISim2645 <- data.frame(cbind(simmeanAQI2645,PIAQI2645))
F20AQISim2645$PlotCN <- all2645cc20  $PlotCN
F20AQISim2645 <- F20AQISim2645 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2645) <- c("PlotCN","AQI","5CI","95CI")
simAQI4545 <- link(AQISplit20, data=all4545cc20 )
simmeanAQI4545 <- data.frame(apply(simAQI4545,2,mean))
PIAQI4545 <- t(data.frame(apply(simAQI4545,2,PI,prob=0.89)))
F20AQISim4545 <- data.frame(cbind(simmeanAQI4545,PIAQI4545))
F20AQISim4545$PlotCN <- all4545cc20  $PlotCN
F20AQISim4545 <- F20AQISim4545 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4545) <- c("PlotCN","AQI","5CI","95CI")
simAQI8545 <- link(AQISplit20, data=all8545cc20 )
simmeanAQI8545 <- data.frame(apply(simAQI8545,2,mean))
PIAQI8545 <- t(data.frame(apply(simAQI8545,2,PI,prob=0.89)))
F20AQISim8545 <- data.frame(cbind(simmeanAQI8545,PIAQI8545))
F20AQISim8545$PlotCN <- all8545cc20  $PlotCN
F20AQISim8545 <- F20AQISim8545 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8545) <- c("PlotCN","AQI","5CI","95CI")

simAQI2650 <- link(AQISplit20, data=all2650cc20 )
simmeanAQI2650 <- data.frame(apply(simAQI2650,2,mean))
PIAQI2650 <- t(data.frame(apply(simAQI2650,2,PI,prob=0.89)))
F20AQISim2650 <- data.frame(cbind(simmeanAQI2650,PIAQI2650))
F20AQISim2650$PlotCN <- all2650cc20  $PlotCN
F20AQISim2650 <- F20AQISim2650 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2650) <- c("PlotCN","AQI","5CI","95CI")
simAQI4550 <- link(AQISplit20, data=all4550cc20 )
simmeanAQI4550 <- data.frame(apply(simAQI4550,2,mean))
PIAQI4550 <- t(data.frame(apply(simAQI4550,2,PI,prob=0.89)))
F20AQISim4550 <- data.frame(cbind(simmeanAQI4550,PIAQI4550))
F20AQISim4550$PlotCN <- all4550cc20  $PlotCN
F20AQISim4550 <- F20AQISim4550 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4550) <- c("PlotCN","AQI","5CI","95CI")
simAQI8550 <- link(AQISplit20, data=all8550cc20 )
simmeanAQI8550 <- data.frame(apply(simAQI8550,2,mean))
PIAQI8550 <- t(data.frame(apply(simAQI8550,2,PI,prob=0.89)))
F20AQISim8550 <- data.frame(cbind(simmeanAQI8550,PIAQI8550))
F20AQISim8550$PlotCN <- all8550cc20  $PlotCN
F20AQISim8550 <- F20AQISim8550 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8550) <- c("PlotCN","AQI","5CI","95CI")

simAQI2655 <- link(AQISplit20, data=all2655cc20 )
simmeanAQI2655 <- data.frame(apply(simAQI2655,2,mean))
PIAQI2655 <- t(data.frame(apply(simAQI2655,2,PI,prob=0.89)))
F20AQISim2655 <- data.frame(cbind(simmeanAQI2655,PIAQI2655))
F20AQISim2655$PlotCN <- all2655cc20  $PlotCN
F20AQISim2655 <- F20AQISim2655 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2655) <- c("PlotCN","AQI","5CI","95CI")
simAQI4555 <- link(AQISplit20, data=all4555cc20 )
simmeanAQI4555 <- data.frame(apply(simAQI4555,2,mean))
PIAQI4555 <- t(data.frame(apply(simAQI4555,2,PI,prob=0.89)))
F20AQISim4555 <- data.frame(cbind(simmeanAQI4555,PIAQI4555))
F20AQISim4555$PlotCN <- all4555cc20  $PlotCN
F20AQISim4555 <- F20AQISim4555 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4555) <- c("PlotCN","AQI","5CI","95CI")
simAQI8555 <- link(AQISplit20, data=all8555cc20 )
simmeanAQI8555 <- data.frame(apply(simAQI8555,2,mean))
PIAQI8555 <- t(data.frame(apply(simAQI8555,2,PI,prob=0.89)))
F20AQISim8555 <- data.frame(cbind(simmeanAQI8555,PIAQI8555))
F20AQISim8555$PlotCN <- all8555cc20  $PlotCN
F20AQISim8555 <- F20AQISim8555 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8555) <- c("PlotCN","AQI","5CI","95CI")

simAQI2660 <- link(AQISplit20, data=all2660cc20 )
simmeanAQI2660 <- data.frame(apply(simAQI2660,2,mean))
PIAQI2660 <- t(data.frame(apply(simAQI2660,2,PI,prob=0.89)))
F20AQISim2660 <- data.frame(cbind(simmeanAQI2660,PIAQI2660))
F20AQISim2660$PlotCN <- all2660cc20  $PlotCN
F20AQISim2660 <- F20AQISim2660 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2660) <- c("PlotCN","AQI","5CI","95CI")
simAQI4560 <- link(AQISplit20, data=all4560cc20 )
simmeanAQI4560 <- data.frame(apply(simAQI4560,2,mean))
PIAQI4560 <- t(data.frame(apply(simAQI4560,2,PI,prob=0.89)))
F20AQISim4560 <- data.frame(cbind(simmeanAQI4560,PIAQI4560))
F20AQISim4560$PlotCN <- all4560cc20  $PlotCN
F20AQISim4560 <- F20AQISim4560 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4560) <- c("PlotCN","AQI","5CI","95CI")
simAQI8560 <- link(AQISplit20, data=all8560cc20 )
simmeanAQI8560 <- data.frame(apply(simAQI8560,2,mean))
PIAQI8560 <- t(data.frame(apply(simAQI8560,2,PI,prob=0.89)))
F20AQISim8560 <- data.frame(cbind(simmeanAQI8560,PIAQI8560))
F20AQISim8560$PlotCN <- all8560cc20  $PlotCN
F20AQISim8560 <- F20AQISim8560 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8560) <- c("PlotCN","AQI","5CI","95CI")

simAQI2665 <- link(AQISplit20, data=all2665cc20 )
simmeanAQI2665 <- data.frame(apply(simAQI2665,2,mean))
PIAQI2665 <- t(data.frame(apply(simAQI2665,2,PI,prob=0.89)))
F20AQISim2665 <- data.frame(cbind(simmeanAQI2665,PIAQI2665))
F20AQISim2665$PlotCN <- all2665cc20  $PlotCN
F20AQISim2665 <- F20AQISim2665 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2665) <- c("PlotCN","AQI","5CI","95CI")
simAQI4565 <- link(AQISplit20, data=all4565cc20 )
simmeanAQI4565 <- data.frame(apply(simAQI4565,2,mean))
PIAQI4565 <- t(data.frame(apply(simAQI4565,2,PI,prob=0.89)))
F20AQISim4565 <- data.frame(cbind(simmeanAQI4565,PIAQI4565))
F20AQISim4565$PlotCN <- all4565cc20  $PlotCN
F20AQISim4565 <- F20AQISim4565 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4565) <- c("PlotCN","AQI","5CI","95CI")
simAQI8565 <- link(AQISplit20, data=all8565cc20 )
simmeanAQI8565 <- data.frame(apply(simAQI8565,2,mean))
PIAQI8565 <- t(data.frame(apply(simAQI8565,2,PI,prob=0.89)))
F20AQISim8565 <- data.frame(cbind(simmeanAQI8565,PIAQI8565))
F20AQISim8565$PlotCN <- all8565cc20  $PlotCN
F20AQISim8565 <- F20AQISim8565 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8565) <- c("PlotCN","AQI","5CI","95CI")

simAQI2670 <- link(AQISplit20, data=all2670cc20 )
simmeanAQI2670 <- data.frame(apply(simAQI2670,2,mean))
PIAQI2670 <- t(data.frame(apply(simAQI2670,2,PI,prob=0.89)))
F20AQISim2670 <- data.frame(cbind(simmeanAQI2670,PIAQI2670))
F20AQISim2670$PlotCN <- all2670cc20  $PlotCN
F20AQISim2670 <- F20AQISim2670 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2670) <- c("PlotCN","AQI","5CI","95CI")
simAQI4570 <- link(AQISplit20, data=all4570cc20 )
simmeanAQI4570 <- data.frame(apply(simAQI4570,2,mean))
PIAQI4570 <- t(data.frame(apply(simAQI4570,2,PI,prob=0.89)))
F20AQISim4570 <- data.frame(cbind(simmeanAQI4570,PIAQI4570))
F20AQISim4570$PlotCN <- all4570cc20  $PlotCN
F20AQISim4570 <- F20AQISim4570 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4570) <- c("PlotCN","AQI","5CI","95CI")
simAQI8570 <- link(AQISplit20, data=all8570cc20 )
simmeanAQI8570 <- data.frame(apply(simAQI8570,2,mean))
PIAQI8570 <- t(data.frame(apply(simAQI8570,2,PI,prob=0.89)))
F20AQISim8570 <- data.frame(cbind(simmeanAQI8570,PIAQI8570))
F20AQISim8570$PlotCN <- all8570cc20  $PlotCN
F20AQISim8570 <- F20AQISim8570 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8570) <- c("PlotCN","AQI","5CI","95CI")

simAQI2675 <- link(AQISplit20, data=all2675cc20 )
simmeanAQI2675 <- data.frame(apply(simAQI2675,2,mean))
PIAQI2675 <- t(data.frame(apply(simAQI2675,2,PI,prob=0.89)))
F20AQISim2675 <- data.frame(cbind(simmeanAQI2675,PIAQI2675))
F20AQISim2675$PlotCN <- all2675cc20  $PlotCN
F20AQISim2675 <- F20AQISim2675 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2675) <- c("PlotCN","AQI","5CI","95CI")
simAQI4575 <- link(AQISplit20, data=all4575cc20 )
simmeanAQI4575 <- data.frame(apply(simAQI4575,2,mean))
PIAQI4575 <- t(data.frame(apply(simAQI4575,2,PI,prob=0.89)))
F20AQISim4575 <- data.frame(cbind(simmeanAQI4575,PIAQI4575))
F20AQISim4575$PlotCN <- all4575cc20  $PlotCN
F20AQISim4575 <- F20AQISim4575 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4575) <- c("PlotCN","AQI","5CI","95CI")
simAQI8575 <- link(AQISplit20, data=all8575cc20 )
simmeanAQI8575 <- data.frame(apply(simAQI8575,2,mean))
PIAQI8575 <- t(data.frame(apply(simAQI8575,2,PI,prob=0.89)))
F20AQISim8575 <- data.frame(cbind(simmeanAQI8575,PIAQI8575))
F20AQISim8575$PlotCN <- all8575cc20  $PlotCN
F20AQISim8575 <- F20AQISim8575 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8575) <- c("PlotCN","AQI","5CI","95CI")

simAQI2680 <- link(AQISplit20, data=all2680cc20 )
simmeanAQI2680 <- data.frame(apply(simAQI2680,2,mean))
PIAQI2680 <- t(data.frame(apply(simAQI2680,2,PI,prob=0.89)))
F20AQISim2680 <- data.frame(cbind(simmeanAQI2680,PIAQI2680))
F20AQISim2680$PlotCN <- all2680cc20  $PlotCN
F20AQISim2680 <- F20AQISim2680 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim2680) <- c("PlotCN","AQI","5CI","95CI")
simAQI4580 <- link(AQISplit20, data=all4580cc20 )
simmeanAQI4580 <- data.frame(apply(simAQI4580,2,mean))
PIAQI4580 <- t(data.frame(apply(simAQI4580,2,PI,prob=0.89)))
F20AQISim4580 <- data.frame(cbind(simmeanAQI4580,PIAQI4580))
F20AQISim4580$PlotCN <- all4580cc20  $PlotCN
F20AQISim4580 <- F20AQISim4580 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim4580) <- c("PlotCN","AQI","5CI","95CI")
simAQI8580 <- link(AQISplit20, data=all8580cc20 )
simmeanAQI8580 <- data.frame(apply(simAQI8580,2,mean))
PIAQI8580 <- t(data.frame(apply(simAQI8580,2,PI,prob=0.89)))
F20AQISim8580 <- data.frame(cbind(simmeanAQI8580,PIAQI8580))
F20AQISim8580$PlotCN <- all8580cc20  $PlotCN
F20AQISim8580 <- F20AQISim8580 %>% dplyr::select(PlotCN, everything())
colnames(F20AQISim8580) <- c("PlotCN","AQI","5CI","95CI")


#group data by RCP pathway
F20AQISim26 <- F20AQISim2625 %>% cbind(F20AQISim2630[,2:4]) %>%
  cbind(F20AQISim2635[,2:4]) %>% cbind(F20AQISim2640[,2:4]) %>% cbind(F20AQISim2645[,2:4]) %>%
  cbind(F20AQISim2650[,2:4]) %>% cbind(F20AQISim2655[,2:4]) %>% cbind(F20AQISim2660[,2:4]) %>%
  cbind(F20AQISim2665[,2:4]) %>% cbind(F20AQISim2670[,2:4]) %>% cbind(F20AQISim2675[,2:4]) %>%
  cbind(F20AQISim2680[,2:4])
colnames(F20AQISim26) <- AQISimnames
write.csv(F20AQISim26,file="F20AQIPred26T.csv")

F20AQISim45 <- F20AQISim4525 %>% cbind(F20AQISim4530[,2:4]) %>%
  cbind(F20AQISim4535[,2:4]) %>% cbind(F20AQISim4540[,2:4]) %>% cbind(F20AQISim4545[,2:4]) %>%
  cbind(F20AQISim4550[,2:4]) %>% cbind(F20AQISim4555[,2:4]) %>% cbind(F20AQISim4560[,2:4]) %>%
  cbind(F20AQISim4565[,2:4]) %>% cbind(F20AQISim4570[,2:4]) %>% cbind(F20AQISim4575[,2:4]) %>%
  cbind(F20AQISim4580[,2:4])
colnames(F20AQISim45) <- AQISimnames
write.csv(F20AQISim45,file="F20AQIPred45T.csv")

F20AQISim85 <- F20AQISim8525 %>% cbind(F20AQISim8530[,2:4]) %>%
  cbind(F20AQISim8535[,2:4]) %>% cbind(F20AQISim8540[,2:4]) %>% cbind(F20AQISim8545[,2:4]) %>%
  cbind(F20AQISim8550[,2:4]) %>% cbind(F20AQISim8555[,2:4]) %>% cbind(F20AQISim8560[,2:4]) %>%
  cbind(F20AQISim8565[,2:4]) %>% cbind(F20AQISim8570[,2:4]) %>% cbind(F20AQISim8575[,2:4]) %>%
  cbind(F20AQISim8580[,2:4])
colnames(F20AQISim85) <- AQISimnames
write.csv(F20AQISim85,file="F20AQIPred85T.csv")


#fgroup 21
simAQI2625 <- link(AQISplit21, data=all2625cc21 )
simmeanAQI2625 <- data.frame(apply(simAQI2625,2,mean))
PIAQI2625 <- t(data.frame(apply(simAQI2625,2,PI,prob=0.89)))
F21AQISim2625 <- data.frame(cbind(simmeanAQI2625,PIAQI2625))
F21AQISim2625$PlotCN <- all2625cc21  $PlotCN
F21AQISim2625 <- F21AQISim2625 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2625) <- c("PlotCN","AQI","5CI","95CI")
simAQI4525 <- link(AQISplit21, data=all4525cc21 )
simmeanAQI4525 <- data.frame(apply(simAQI4525,2,mean))
PIAQI4525 <- t(data.frame(apply(simAQI4525,2,PI,prob=0.89)))
F21AQISim4525 <- data.frame(cbind(simmeanAQI4525,PIAQI4525))
F21AQISim4525$PlotCN <- all4525cc21  $PlotCN
F21AQISim4525 <- F21AQISim4525 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4525) <- c("PlotCN","AQI","5CI","95CI")
simAQI8525 <- link(AQISplit21, data=all8525cc21 )
simmeanAQI8525 <- data.frame(apply(simAQI8525,2,mean))
PIAQI8525 <- t(data.frame(apply(simAQI8525,2,PI,prob=0.89)))
F21AQISim8525 <- data.frame(cbind(simmeanAQI8525,PIAQI8525))
F21AQISim8525$PlotCN <- all8525cc21  $PlotCN
F21AQISim8525 <- F21AQISim8525 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8525) <- c("PlotCN","AQI","5CI","95CI")

simAQI2630 <- link(AQISplit21, data=all2630cc21 )
simmeanAQI2630 <- data.frame(apply(simAQI2630,2,mean))
PIAQI2630 <- t(data.frame(apply(simAQI2630,2,PI,prob=0.89)))
F21AQISim2630 <- data.frame(cbind(simmeanAQI2630,PIAQI2630))
F21AQISim2630$PlotCN <- all2630cc21  $PlotCN
F21AQISim2630 <- F21AQISim2630 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2630) <- c("PlotCN","AQI","5CI","95CI")
simAQI4530 <- link(AQISplit21, data=all4530cc21 )
simmeanAQI4530 <- data.frame(apply(simAQI4530,2,mean))
PIAQI4530 <- t(data.frame(apply(simAQI4530,2,PI,prob=0.89)))
F21AQISim4530 <- data.frame(cbind(simmeanAQI4530,PIAQI4530))
F21AQISim4530$PlotCN <- all4530cc21  $PlotCN
F21AQISim4530 <- F21AQISim4530 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4530) <- c("PlotCN","AQI","5CI","95CI")
simAQI8530 <- link(AQISplit21, data=all8530cc21 )
simmeanAQI8530 <- data.frame(apply(simAQI8530,2,mean))
PIAQI8530 <- t(data.frame(apply(simAQI8530,2,PI,prob=0.89)))
F21AQISim8530 <- data.frame(cbind(simmeanAQI8530,PIAQI8530))
F21AQISim8530$PlotCN <- all8530cc21  $PlotCN
F21AQISim8530 <- F21AQISim8530 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8530) <- c("PlotCN","AQI","5CI","95CI")

simAQI2635 <- link(AQISplit21, data=all2635cc21 )
simmeanAQI2635 <- data.frame(apply(simAQI2635,2,mean))
PIAQI2635 <- t(data.frame(apply(simAQI2635,2,PI,prob=0.89)))
F21AQISim2635 <- data.frame(cbind(simmeanAQI2635,PIAQI2635))
F21AQISim2635$PlotCN <- all2635cc21  $PlotCN
F21AQISim2635 <- F21AQISim2635 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2635) <- c("PlotCN","AQI","5CI","95CI")
simAQI4535 <- link(AQISplit21, data=all4535cc21 )
simmeanAQI4535 <- data.frame(apply(simAQI4535,2,mean))
PIAQI4535 <- t(data.frame(apply(simAQI4535,2,PI,prob=0.89)))
F21AQISim4535 <- data.frame(cbind(simmeanAQI4535,PIAQI4535))
F21AQISim4535$PlotCN <- all4535cc21  $PlotCN
F21AQISim4535 <- F21AQISim4535 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4535) <- c("PlotCN","AQI","5CI","95CI")
simAQI8535 <- link(AQISplit21, data=all8535cc21 )
simmeanAQI8535 <- data.frame(apply(simAQI8535,2,mean))
PIAQI8535 <- t(data.frame(apply(simAQI8535,2,PI,prob=0.89)))
F21AQISim8535 <- data.frame(cbind(simmeanAQI8535,PIAQI8535))
F21AQISim8535$PlotCN <- all8535cc21  $PlotCN
F21AQISim8535 <- F21AQISim8535 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8535) <- c("PlotCN","AQI","5CI","95CI")

simAQI2640 <- link(AQISplit21, data=all2640cc21 )
simmeanAQI2640 <- data.frame(apply(simAQI2640,2,mean))
PIAQI2640 <- t(data.frame(apply(simAQI2640,2,PI,prob=0.89)))
F21AQISim2640 <- data.frame(cbind(simmeanAQI2640,PIAQI2640))
F21AQISim2640$PlotCN <- all2640cc21  $PlotCN
F21AQISim2640 <- F21AQISim2640 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2640) <- c("PlotCN","AQI","5CI","95CI")
simAQI4540 <- link(AQISplit21, data=all4540cc21 )
simmeanAQI4540 <- data.frame(apply(simAQI4540,2,mean))
PIAQI4540 <- t(data.frame(apply(simAQI4540,2,PI,prob=0.89)))
F21AQISim4540 <- data.frame(cbind(simmeanAQI4540,PIAQI4540))
F21AQISim4540$PlotCN <- all4540cc21  $PlotCN
F21AQISim4540 <- F21AQISim4540 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4540) <- c("PlotCN","AQI","5CI","95CI")
simAQI8540 <- link(AQISplit21, data=all8540cc21 )
simmeanAQI8540 <- data.frame(apply(simAQI8540,2,mean))
PIAQI8540 <- t(data.frame(apply(simAQI8540,2,PI,prob=0.89)))
F21AQISim8540 <- data.frame(cbind(simmeanAQI8540,PIAQI8540))
F21AQISim8540$PlotCN <- all8540cc21  $PlotCN
F21AQISim8540 <- F21AQISim8540 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8540) <- c("PlotCN","AQI","5CI","95CI")

simAQI2645 <- link(AQISplit21, data=all2645cc21 )
simmeanAQI2645 <- data.frame(apply(simAQI2645,2,mean))
PIAQI2645 <- t(data.frame(apply(simAQI2645,2,PI,prob=0.89)))
F21AQISim2645 <- data.frame(cbind(simmeanAQI2645,PIAQI2645))
F21AQISim2645$PlotCN <- all2645cc21  $PlotCN
F21AQISim2645 <- F21AQISim2645 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2645) <- c("PlotCN","AQI","5CI","95CI")
simAQI4545 <- link(AQISplit21, data=all4545cc21 )
simmeanAQI4545 <- data.frame(apply(simAQI4545,2,mean))
PIAQI4545 <- t(data.frame(apply(simAQI4545,2,PI,prob=0.89)))
F21AQISim4545 <- data.frame(cbind(simmeanAQI4545,PIAQI4545))
F21AQISim4545$PlotCN <- all4545cc21  $PlotCN
F21AQISim4545 <- F21AQISim4545 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4545) <- c("PlotCN","AQI","5CI","95CI")
simAQI8545 <- link(AQISplit21, data=all8545cc21 )
simmeanAQI8545 <- data.frame(apply(simAQI8545,2,mean))
PIAQI8545 <- t(data.frame(apply(simAQI8545,2,PI,prob=0.89)))
F21AQISim8545 <- data.frame(cbind(simmeanAQI8545,PIAQI8545))
F21AQISim8545$PlotCN <- all8545cc21  $PlotCN
F21AQISim8545 <- F21AQISim8545 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8545) <- c("PlotCN","AQI","5CI","95CI")

simAQI2650 <- link(AQISplit21, data=all2650cc21 )
simmeanAQI2650 <- data.frame(apply(simAQI2650,2,mean))
PIAQI2650 <- t(data.frame(apply(simAQI2650,2,PI,prob=0.89)))
F21AQISim2650 <- data.frame(cbind(simmeanAQI2650,PIAQI2650))
F21AQISim2650$PlotCN <- all2650cc21  $PlotCN
F21AQISim2650 <- F21AQISim2650 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2650) <- c("PlotCN","AQI","5CI","95CI")
simAQI4550 <- link(AQISplit21, data=all4550cc21 )
simmeanAQI4550 <- data.frame(apply(simAQI4550,2,mean))
PIAQI4550 <- t(data.frame(apply(simAQI4550,2,PI,prob=0.89)))
F21AQISim4550 <- data.frame(cbind(simmeanAQI4550,PIAQI4550))
F21AQISim4550$PlotCN <- all4550cc21  $PlotCN
F21AQISim4550 <- F21AQISim4550 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4550) <- c("PlotCN","AQI","5CI","95CI")
simAQI8550 <- link(AQISplit21, data=all8550cc21 )
simmeanAQI8550 <- data.frame(apply(simAQI8550,2,mean))
PIAQI8550 <- t(data.frame(apply(simAQI8550,2,PI,prob=0.89)))
F21AQISim8550 <- data.frame(cbind(simmeanAQI8550,PIAQI8550))
F21AQISim8550$PlotCN <- all8550cc21  $PlotCN
F21AQISim8550 <- F21AQISim8550 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8550) <- c("PlotCN","AQI","5CI","95CI")

simAQI2655 <- link(AQISplit21, data=all2655cc21 )
simmeanAQI2655 <- data.frame(apply(simAQI2655,2,mean))
PIAQI2655 <- t(data.frame(apply(simAQI2655,2,PI,prob=0.89)))
F21AQISim2655 <- data.frame(cbind(simmeanAQI2655,PIAQI2655))
F21AQISim2655$PlotCN <- all2655cc21  $PlotCN
F21AQISim2655 <- F21AQISim2655 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2655) <- c("PlotCN","AQI","5CI","95CI")
simAQI4555 <- link(AQISplit21, data=all4555cc21 )
simmeanAQI4555 <- data.frame(apply(simAQI4555,2,mean))
PIAQI4555 <- t(data.frame(apply(simAQI4555,2,PI,prob=0.89)))
F21AQISim4555 <- data.frame(cbind(simmeanAQI4555,PIAQI4555))
F21AQISim4555$PlotCN <- all4555cc21  $PlotCN
F21AQISim4555 <- F21AQISim4555 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4555) <- c("PlotCN","AQI","5CI","95CI")
simAQI8555 <- link(AQISplit21, data=all8555cc21 )
simmeanAQI8555 <- data.frame(apply(simAQI8555,2,mean))
PIAQI8555 <- t(data.frame(apply(simAQI8555,2,PI,prob=0.89)))
F21AQISim8555 <- data.frame(cbind(simmeanAQI8555,PIAQI8555))
F21AQISim8555$PlotCN <- all8555cc21  $PlotCN
F21AQISim8555 <- F21AQISim8555 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8555) <- c("PlotCN","AQI","5CI","95CI")

simAQI2660 <- link(AQISplit21, data=all2660cc21 )
simmeanAQI2660 <- data.frame(apply(simAQI2660,2,mean))
PIAQI2660 <- t(data.frame(apply(simAQI2660,2,PI,prob=0.89)))
F21AQISim2660 <- data.frame(cbind(simmeanAQI2660,PIAQI2660))
F21AQISim2660$PlotCN <- all2660cc21  $PlotCN
F21AQISim2660 <- F21AQISim2660 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2660) <- c("PlotCN","AQI","5CI","95CI")
simAQI4560 <- link(AQISplit21, data=all4560cc21 )
simmeanAQI4560 <- data.frame(apply(simAQI4560,2,mean))
PIAQI4560 <- t(data.frame(apply(simAQI4560,2,PI,prob=0.89)))
F21AQISim4560 <- data.frame(cbind(simmeanAQI4560,PIAQI4560))
F21AQISim4560$PlotCN <- all4560cc21  $PlotCN
F21AQISim4560 <- F21AQISim4560 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4560) <- c("PlotCN","AQI","5CI","95CI")
simAQI8560 <- link(AQISplit21, data=all8560cc21 )
simmeanAQI8560 <- data.frame(apply(simAQI8560,2,mean))
PIAQI8560 <- t(data.frame(apply(simAQI8560,2,PI,prob=0.89)))
F21AQISim8560 <- data.frame(cbind(simmeanAQI8560,PIAQI8560))
F21AQISim8560$PlotCN <- all8560cc21  $PlotCN
F21AQISim8560 <- F21AQISim8560 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8560) <- c("PlotCN","AQI","5CI","95CI")

simAQI2665 <- link(AQISplit21, data=all2665cc21 )
simmeanAQI2665 <- data.frame(apply(simAQI2665,2,mean))
PIAQI2665 <- t(data.frame(apply(simAQI2665,2,PI,prob=0.89)))
F21AQISim2665 <- data.frame(cbind(simmeanAQI2665,PIAQI2665))
F21AQISim2665$PlotCN <- all2665cc21  $PlotCN
F21AQISim2665 <- F21AQISim2665 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2665) <- c("PlotCN","AQI","5CI","95CI")
simAQI4565 <- link(AQISplit21, data=all4565cc21 )
simmeanAQI4565 <- data.frame(apply(simAQI4565,2,mean))
PIAQI4565 <- t(data.frame(apply(simAQI4565,2,PI,prob=0.89)))
F21AQISim4565 <- data.frame(cbind(simmeanAQI4565,PIAQI4565))
F21AQISim4565$PlotCN <- all4565cc21  $PlotCN
F21AQISim4565 <- F21AQISim4565 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4565) <- c("PlotCN","AQI","5CI","95CI")
simAQI8565 <- link(AQISplit21, data=all8565cc21 )
simmeanAQI8565 <- data.frame(apply(simAQI8565,2,mean))
PIAQI8565 <- t(data.frame(apply(simAQI8565,2,PI,prob=0.89)))
F21AQISim8565 <- data.frame(cbind(simmeanAQI8565,PIAQI8565))
F21AQISim8565$PlotCN <- all8565cc21  $PlotCN
F21AQISim8565 <- F21AQISim8565 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8565) <- c("PlotCN","AQI","5CI","95CI")

simAQI2670 <- link(AQISplit21, data=all2670cc21 )
simmeanAQI2670 <- data.frame(apply(simAQI2670,2,mean))
PIAQI2670 <- t(data.frame(apply(simAQI2670,2,PI,prob=0.89)))
F21AQISim2670 <- data.frame(cbind(simmeanAQI2670,PIAQI2670))
F21AQISim2670$PlotCN <- all2670cc21  $PlotCN
F21AQISim2670 <- F21AQISim2670 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2670) <- c("PlotCN","AQI","5CI","95CI")
simAQI4570 <- link(AQISplit21, data=all4570cc21 )
simmeanAQI4570 <- data.frame(apply(simAQI4570,2,mean))
PIAQI4570 <- t(data.frame(apply(simAQI4570,2,PI,prob=0.89)))
F21AQISim4570 <- data.frame(cbind(simmeanAQI4570,PIAQI4570))
F21AQISim4570$PlotCN <- all4570cc21  $PlotCN
F21AQISim4570 <- F21AQISim4570 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4570) <- c("PlotCN","AQI","5CI","95CI")
simAQI8570 <- link(AQISplit21, data=all8570cc21 )
simmeanAQI8570 <- data.frame(apply(simAQI8570,2,mean))
PIAQI8570 <- t(data.frame(apply(simAQI8570,2,PI,prob=0.89)))
F21AQISim8570 <- data.frame(cbind(simmeanAQI8570,PIAQI8570))
F21AQISim8570$PlotCN <- all8570cc21  $PlotCN
F21AQISim8570 <- F21AQISim8570 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8570) <- c("PlotCN","AQI","5CI","95CI")

simAQI2675 <- link(AQISplit21, data=all2675cc21 )
simmeanAQI2675 <- data.frame(apply(simAQI2675,2,mean))
PIAQI2675 <- t(data.frame(apply(simAQI2675,2,PI,prob=0.89)))
F21AQISim2675 <- data.frame(cbind(simmeanAQI2675,PIAQI2675))
F21AQISim2675$PlotCN <- all2675cc21  $PlotCN
F21AQISim2675 <- F21AQISim2675 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2675) <- c("PlotCN","AQI","5CI","95CI")
simAQI4575 <- link(AQISplit21, data=all4575cc21 )
simmeanAQI4575 <- data.frame(apply(simAQI4575,2,mean))
PIAQI4575 <- t(data.frame(apply(simAQI4575,2,PI,prob=0.89)))
F21AQISim4575 <- data.frame(cbind(simmeanAQI4575,PIAQI4575))
F21AQISim4575$PlotCN <- all4575cc21  $PlotCN
F21AQISim4575 <- F21AQISim4575 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4575) <- c("PlotCN","AQI","5CI","95CI")
simAQI8575 <- link(AQISplit21, data=all8575cc21 )
simmeanAQI8575 <- data.frame(apply(simAQI8575,2,mean))
PIAQI8575 <- t(data.frame(apply(simAQI8575,2,PI,prob=0.89)))
F21AQISim8575 <- data.frame(cbind(simmeanAQI8575,PIAQI8575))
F21AQISim8575$PlotCN <- all8575cc21  $PlotCN
F21AQISim8575 <- F21AQISim8575 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8575) <- c("PlotCN","AQI","5CI","95CI")

simAQI2680 <- link(AQISplit21, data=all2680cc21 )
simmeanAQI2680 <- data.frame(apply(simAQI2680,2,mean))
PIAQI2680 <- t(data.frame(apply(simAQI2680,2,PI,prob=0.89)))
F21AQISim2680 <- data.frame(cbind(simmeanAQI2680,PIAQI2680))
F21AQISim2680$PlotCN <- all2680cc21  $PlotCN
F21AQISim2680 <- F21AQISim2680 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim2680) <- c("PlotCN","AQI","5CI","95CI")
simAQI4580 <- link(AQISplit21, data=all4580cc21 )
simmeanAQI4580 <- data.frame(apply(simAQI4580,2,mean))
PIAQI4580 <- t(data.frame(apply(simAQI4580,2,PI,prob=0.89)))
F21AQISim4580 <- data.frame(cbind(simmeanAQI4580,PIAQI4580))
F21AQISim4580$PlotCN <- all4580cc21  $PlotCN
F21AQISim4580 <- F21AQISim4580 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim4580) <- c("PlotCN","AQI","5CI","95CI")
simAQI8580 <- link(AQISplit21, data=all8580cc21 )
simmeanAQI8580 <- data.frame(apply(simAQI8580,2,mean))
PIAQI8580 <- t(data.frame(apply(simAQI8580,2,PI,prob=0.89)))
F21AQISim8580 <- data.frame(cbind(simmeanAQI8580,PIAQI8580))
F21AQISim8580$PlotCN <- all8580cc21  $PlotCN
F21AQISim8580 <- F21AQISim8580 %>% dplyr::select(PlotCN, everything())
colnames(F21AQISim8580) <- c("PlotCN","AQI","5CI","95CI")


#create vector of new colnames
AQISimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("AQI", "5CI", "95CI"), x)))
AQISimnames<- append(AQISimnames,"PlotCN",after=0)

#group data by RCP pathway
F21AQISim26 <- F21AQISim2625 %>% cbind(F21AQISim2630[,2:4]) %>%
  cbind(F21AQISim2635[,2:4]) %>% cbind(F21AQISim2640[,2:4]) %>% cbind(F21AQISim2645[,2:4]) %>%
  cbind(F21AQISim2650[,2:4]) %>% cbind(F21AQISim2655[,2:4]) %>% cbind(F21AQISim2660[,2:4]) %>%
  cbind(F21AQISim2665[,2:4]) %>% cbind(F21AQISim2670[,2:4]) %>% cbind(F21AQISim2675[,2:4]) %>%
  cbind(F21AQISim2680[,2:4])
colnames(F21AQISim26) <- AQISimnames
write.csv(F21AQISim26,file="F21AQIPred26T.csv")

F21AQISim45 <- F21AQISim4525 %>% cbind(F21AQISim4530[,2:4]) %>%
  cbind(F21AQISim4535[,2:4]) %>% cbind(F21AQISim4540[,2:4]) %>% cbind(F21AQISim4545[,2:4]) %>%
  cbind(F21AQISim4550[,2:4]) %>% cbind(F21AQISim4555[,2:4]) %>% cbind(F21AQISim4560[,2:4]) %>%
  cbind(F21AQISim4565[,2:4]) %>% cbind(F21AQISim4570[,2:4]) %>% cbind(F21AQISim4575[,2:4]) %>%
  cbind(F21AQISim4580[,2:4])
colnames(F21AQISim45) <- AQISimnames
write.csv(F21AQISim45,file="F21AQIPred45T.csv")

F21AQISim85 <- F21AQISim8525 %>% cbind(F21AQISim8530[,2:4]) %>%
  cbind(F21AQISim8535[,2:4]) %>% cbind(F21AQISim8540[,2:4]) %>% cbind(F21AQISim8545[,2:4]) %>%
  cbind(F21AQISim8550[,2:4]) %>% cbind(F21AQISim8555[,2:4]) %>% cbind(F21AQISim8560[,2:4]) %>%
  cbind(F21AQISim8565[,2:4]) %>% cbind(F21AQISim8570[,2:4]) %>% cbind(F21AQISim8575[,2:4]) %>%
  cbind(F21AQISim8580[,2:4])
colnames(F21AQISim85) <- AQISimnames
write.csv(F21AQISim85,file="F21AQIPred85T.csv")


#fgroup 23
simAQI2625 <- link(AQISplit23, data=all2625cc23 )
simmeanAQI2625 <- data.frame(apply(simAQI2625,2,mean))
PIAQI2625 <- t(data.frame(apply(simAQI2625,2,PI,prob=0.89)))
F23AQISim2625 <- data.frame(cbind(simmeanAQI2625,PIAQI2625))
F23AQISim2625$PlotCN <- all2625cc23  $PlotCN
F23AQISim2625 <- F23AQISim2625 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2625) <- c("PlotCN","AQI","5CI","95CI")
simAQI4525 <- link(AQISplit23, data=all4525cc23 )
simmeanAQI4525 <- data.frame(apply(simAQI4525,2,mean))
PIAQI4525 <- t(data.frame(apply(simAQI4525,2,PI,prob=0.89)))
F23AQISim4525 <- data.frame(cbind(simmeanAQI4525,PIAQI4525))
F23AQISim4525$PlotCN <- all4525cc23  $PlotCN
F23AQISim4525 <- F23AQISim4525 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4525) <- c("PlotCN","AQI","5CI","95CI")
simAQI8525 <- link(AQISplit23, data=all8525cc23 )
simmeanAQI8525 <- data.frame(apply(simAQI8525,2,mean))
PIAQI8525 <- t(data.frame(apply(simAQI8525,2,PI,prob=0.89)))
F23AQISim8525 <- data.frame(cbind(simmeanAQI8525,PIAQI8525))
F23AQISim8525$PlotCN <- all8525cc23  $PlotCN
F23AQISim8525 <- F23AQISim8525 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8525) <- c("PlotCN","AQI","5CI","95CI")

simAQI2630 <- link(AQISplit23, data=all2630cc23 )
simmeanAQI2630 <- data.frame(apply(simAQI2630,2,mean))
PIAQI2630 <- t(data.frame(apply(simAQI2630,2,PI,prob=0.89)))
F23AQISim2630 <- data.frame(cbind(simmeanAQI2630,PIAQI2630))
F23AQISim2630$PlotCN <- all2630cc23  $PlotCN
F23AQISim2630 <- F23AQISim2630 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2630) <- c("PlotCN","AQI","5CI","95CI")
simAQI4530 <- link(AQISplit23, data=all4530cc23 )
simmeanAQI4530 <- data.frame(apply(simAQI4530,2,mean))
PIAQI4530 <- t(data.frame(apply(simAQI4530,2,PI,prob=0.89)))
F23AQISim4530 <- data.frame(cbind(simmeanAQI4530,PIAQI4530))
F23AQISim4530$PlotCN <- all4530cc23  $PlotCN
F23AQISim4530 <- F23AQISim4530 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4530) <- c("PlotCN","AQI","5CI","95CI")
simAQI8530 <- link(AQISplit23, data=all8530cc23 )
simmeanAQI8530 <- data.frame(apply(simAQI8530,2,mean))
PIAQI8530 <- t(data.frame(apply(simAQI8530,2,PI,prob=0.89)))
F23AQISim8530 <- data.frame(cbind(simmeanAQI8530,PIAQI8530))
F23AQISim8530$PlotCN <- all8530cc23  $PlotCN
F23AQISim8530 <- F23AQISim8530 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8530) <- c("PlotCN","AQI","5CI","95CI")

simAQI2635 <- link(AQISplit23, data=all2635cc23 )
simmeanAQI2635 <- data.frame(apply(simAQI2635,2,mean))
PIAQI2635 <- t(data.frame(apply(simAQI2635,2,PI,prob=0.89)))
F23AQISim2635 <- data.frame(cbind(simmeanAQI2635,PIAQI2635))
F23AQISim2635$PlotCN <- all2635cc23  $PlotCN
F23AQISim2635 <- F23AQISim2635 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2635) <- c("PlotCN","AQI","5CI","95CI")
simAQI4535 <- link(AQISplit23, data=all4535cc23 )
simmeanAQI4535 <- data.frame(apply(simAQI4535,2,mean))
PIAQI4535 <- t(data.frame(apply(simAQI4535,2,PI,prob=0.89)))
F23AQISim4535 <- data.frame(cbind(simmeanAQI4535,PIAQI4535))
F23AQISim4535$PlotCN <- all4535cc23  $PlotCN
F23AQISim4535 <- F23AQISim4535 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4535) <- c("PlotCN","AQI","5CI","95CI")
simAQI8535 <- link(AQISplit23, data=all8535cc23 )
simmeanAQI8535 <- data.frame(apply(simAQI8535,2,mean))
PIAQI8535 <- t(data.frame(apply(simAQI8535,2,PI,prob=0.89)))
F23AQISim8535 <- data.frame(cbind(simmeanAQI8535,PIAQI8535))
F23AQISim8535$PlotCN <- all8535cc23  $PlotCN
F23AQISim8535 <- F23AQISim8535 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8535) <- c("PlotCN","AQI","5CI","95CI")

simAQI2640 <- link(AQISplit23, data=all2640cc23 )
simmeanAQI2640 <- data.frame(apply(simAQI2640,2,mean))
PIAQI2640 <- t(data.frame(apply(simAQI2640,2,PI,prob=0.89)))
F23AQISim2640 <- data.frame(cbind(simmeanAQI2640,PIAQI2640))
F23AQISim2640$PlotCN <- all2640cc23  $PlotCN
F23AQISim2640 <- F23AQISim2640 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2640) <- c("PlotCN","AQI","5CI","95CI")
simAQI4540 <- link(AQISplit23, data=all4540cc23 )
simmeanAQI4540 <- data.frame(apply(simAQI4540,2,mean))
PIAQI4540 <- t(data.frame(apply(simAQI4540,2,PI,prob=0.89)))
F23AQISim4540 <- data.frame(cbind(simmeanAQI4540,PIAQI4540))
F23AQISim4540$PlotCN <- all4540cc23  $PlotCN
F23AQISim4540 <- F23AQISim4540 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4540) <- c("PlotCN","AQI","5CI","95CI")
simAQI8540 <- link(AQISplit23, data=all8540cc23 )
simmeanAQI8540 <- data.frame(apply(simAQI8540,2,mean))
PIAQI8540 <- t(data.frame(apply(simAQI8540,2,PI,prob=0.89)))
F23AQISim8540 <- data.frame(cbind(simmeanAQI8540,PIAQI8540))
F23AQISim8540$PlotCN <- all8540cc23  $PlotCN
F23AQISim8540 <- F23AQISim8540 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8540) <- c("PlotCN","AQI","5CI","95CI")

simAQI2645 <- link(AQISplit23, data=all2645cc23 )
simmeanAQI2645 <- data.frame(apply(simAQI2645,2,mean))
PIAQI2645 <- t(data.frame(apply(simAQI2645,2,PI,prob=0.89)))
F23AQISim2645 <- data.frame(cbind(simmeanAQI2645,PIAQI2645))
F23AQISim2645$PlotCN <- all2645cc23  $PlotCN
F23AQISim2645 <- F23AQISim2645 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2645) <- c("PlotCN","AQI","5CI","95CI")
simAQI4545 <- link(AQISplit23, data=all4545cc23 )
simmeanAQI4545 <- data.frame(apply(simAQI4545,2,mean))
PIAQI4545 <- t(data.frame(apply(simAQI4545,2,PI,prob=0.89)))
F23AQISim4545 <- data.frame(cbind(simmeanAQI4545,PIAQI4545))
F23AQISim4545$PlotCN <- all4545cc23  $PlotCN
F23AQISim4545 <- F23AQISim4545 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4545) <- c("PlotCN","AQI","5CI","95CI")
simAQI8545 <- link(AQISplit23, data=all8545cc23 )
simmeanAQI8545 <- data.frame(apply(simAQI8545,2,mean))
PIAQI8545 <- t(data.frame(apply(simAQI8545,2,PI,prob=0.89)))
F23AQISim8545 <- data.frame(cbind(simmeanAQI8545,PIAQI8545))
F23AQISim8545$PlotCN <- all8545cc23  $PlotCN
F23AQISim8545 <- F23AQISim8545 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8545) <- c("PlotCN","AQI","5CI","95CI")

simAQI2650 <- link(AQISplit23, data=all2650cc23 )
simmeanAQI2650 <- data.frame(apply(simAQI2650,2,mean))
PIAQI2650 <- t(data.frame(apply(simAQI2650,2,PI,prob=0.89)))
F23AQISim2650 <- data.frame(cbind(simmeanAQI2650,PIAQI2650))
F23AQISim2650$PlotCN <- all2650cc23  $PlotCN
F23AQISim2650 <- F23AQISim2650 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2650) <- c("PlotCN","AQI","5CI","95CI")
simAQI4550 <- link(AQISplit23, data=all4550cc23 )
simmeanAQI4550 <- data.frame(apply(simAQI4550,2,mean))
PIAQI4550 <- t(data.frame(apply(simAQI4550,2,PI,prob=0.89)))
F23AQISim4550 <- data.frame(cbind(simmeanAQI4550,PIAQI4550))
F23AQISim4550$PlotCN <- all4550cc23  $PlotCN
F23AQISim4550 <- F23AQISim4550 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4550) <- c("PlotCN","AQI","5CI","95CI")
simAQI8550 <- link(AQISplit23, data=all8550cc23 )
simmeanAQI8550 <- data.frame(apply(simAQI8550,2,mean))
PIAQI8550 <- t(data.frame(apply(simAQI8550,2,PI,prob=0.89)))
F23AQISim8550 <- data.frame(cbind(simmeanAQI8550,PIAQI8550))
F23AQISim8550$PlotCN <- all8550cc23  $PlotCN
F23AQISim8550 <- F23AQISim8550 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8550) <- c("PlotCN","AQI","5CI","95CI")

simAQI2655 <- link(AQISplit23, data=all2655cc23 )
simmeanAQI2655 <- data.frame(apply(simAQI2655,2,mean))
PIAQI2655 <- t(data.frame(apply(simAQI2655,2,PI,prob=0.89)))
F23AQISim2655 <- data.frame(cbind(simmeanAQI2655,PIAQI2655))
F23AQISim2655$PlotCN <- all2655cc23  $PlotCN
F23AQISim2655 <- F23AQISim2655 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2655) <- c("PlotCN","AQI","5CI","95CI")
simAQI4555 <- link(AQISplit23, data=all4555cc23 )
simmeanAQI4555 <- data.frame(apply(simAQI4555,2,mean))
PIAQI4555 <- t(data.frame(apply(simAQI4555,2,PI,prob=0.89)))
F23AQISim4555 <- data.frame(cbind(simmeanAQI4555,PIAQI4555))
F23AQISim4555$PlotCN <- all4555cc23  $PlotCN
F23AQISim4555 <- F23AQISim4555 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4555) <- c("PlotCN","AQI","5CI","95CI")
simAQI8555 <- link(AQISplit23, data=all8555cc23 )
simmeanAQI8555 <- data.frame(apply(simAQI8555,2,mean))
PIAQI8555 <- t(data.frame(apply(simAQI8555,2,PI,prob=0.89)))
F23AQISim8555 <- data.frame(cbind(simmeanAQI8555,PIAQI8555))
F23AQISim8555$PlotCN <- all8555cc23  $PlotCN
F23AQISim8555 <- F23AQISim8555 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8555) <- c("PlotCN","AQI","5CI","95CI")

simAQI2660 <- link(AQISplit23, data=all2660cc23 )
simmeanAQI2660 <- data.frame(apply(simAQI2660,2,mean))
PIAQI2660 <- t(data.frame(apply(simAQI2660,2,PI,prob=0.89)))
F23AQISim2660 <- data.frame(cbind(simmeanAQI2660,PIAQI2660))
F23AQISim2660$PlotCN <- all2660cc23  $PlotCN
F23AQISim2660 <- F23AQISim2660 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2660) <- c("PlotCN","AQI","5CI","95CI")
simAQI4560 <- link(AQISplit23, data=all4560cc23 )
simmeanAQI4560 <- data.frame(apply(simAQI4560,2,mean))
PIAQI4560 <- t(data.frame(apply(simAQI4560,2,PI,prob=0.89)))
F23AQISim4560 <- data.frame(cbind(simmeanAQI4560,PIAQI4560))
F23AQISim4560$PlotCN <- all4560cc23  $PlotCN
F23AQISim4560 <- F23AQISim4560 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4560) <- c("PlotCN","AQI","5CI","95CI")
simAQI8560 <- link(AQISplit23, data=all8560cc23 )
simmeanAQI8560 <- data.frame(apply(simAQI8560,2,mean))
PIAQI8560 <- t(data.frame(apply(simAQI8560,2,PI,prob=0.89)))
F23AQISim8560 <- data.frame(cbind(simmeanAQI8560,PIAQI8560))
F23AQISim8560$PlotCN <- all8560cc23  $PlotCN
F23AQISim8560 <- F23AQISim8560 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8560) <- c("PlotCN","AQI","5CI","95CI")

simAQI2665 <- link(AQISplit23, data=all2665cc23 )
simmeanAQI2665 <- data.frame(apply(simAQI2665,2,mean))
PIAQI2665 <- t(data.frame(apply(simAQI2665,2,PI,prob=0.89)))
F23AQISim2665 <- data.frame(cbind(simmeanAQI2665,PIAQI2665))
F23AQISim2665$PlotCN <- all2665cc23  $PlotCN
F23AQISim2665 <- F23AQISim2665 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2665) <- c("PlotCN","AQI","5CI","95CI")
simAQI4565 <- link(AQISplit23, data=all4565cc23 )
simmeanAQI4565 <- data.frame(apply(simAQI4565,2,mean))
PIAQI4565 <- t(data.frame(apply(simAQI4565,2,PI,prob=0.89)))
F23AQISim4565 <- data.frame(cbind(simmeanAQI4565,PIAQI4565))
F23AQISim4565$PlotCN <- all4565cc23  $PlotCN
F23AQISim4565 <- F23AQISim4565 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4565) <- c("PlotCN","AQI","5CI","95CI")
simAQI8565 <- link(AQISplit23, data=all8565cc23 )
simmeanAQI8565 <- data.frame(apply(simAQI8565,2,mean))
PIAQI8565 <- t(data.frame(apply(simAQI8565,2,PI,prob=0.89)))
F23AQISim8565 <- data.frame(cbind(simmeanAQI8565,PIAQI8565))
F23AQISim8565$PlotCN <- all8565cc23  $PlotCN
F23AQISim8565 <- F23AQISim8565 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8565) <- c("PlotCN","AQI","5CI","95CI")

simAQI2670 <- link(AQISplit23, data=all2670cc23 )
simmeanAQI2670 <- data.frame(apply(simAQI2670,2,mean))
PIAQI2670 <- t(data.frame(apply(simAQI2670,2,PI,prob=0.89)))
F23AQISim2670 <- data.frame(cbind(simmeanAQI2670,PIAQI2670))
F23AQISim2670$PlotCN <- all2670cc23  $PlotCN
F23AQISim2670 <- F23AQISim2670 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2670) <- c("PlotCN","AQI","5CI","95CI")
simAQI4570 <- link(AQISplit23, data=all4570cc23 )
simmeanAQI4570 <- data.frame(apply(simAQI4570,2,mean))
PIAQI4570 <- t(data.frame(apply(simAQI4570,2,PI,prob=0.89)))
F23AQISim4570 <- data.frame(cbind(simmeanAQI4570,PIAQI4570))
F23AQISim4570$PlotCN <- all4570cc23  $PlotCN
F23AQISim4570 <- F23AQISim4570 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4570) <- c("PlotCN","AQI","5CI","95CI")
simAQI8570 <- link(AQISplit23, data=all8570cc23 )
simmeanAQI8570 <- data.frame(apply(simAQI8570,2,mean))
PIAQI8570 <- t(data.frame(apply(simAQI8570,2,PI,prob=0.89)))
F23AQISim8570 <- data.frame(cbind(simmeanAQI8570,PIAQI8570))
F23AQISim8570$PlotCN <- all8570cc23  $PlotCN
F23AQISim8570 <- F23AQISim8570 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8570) <- c("PlotCN","AQI","5CI","95CI")

simAQI2675 <- link(AQISplit23, data=all2675cc23 )
simmeanAQI2675 <- data.frame(apply(simAQI2675,2,mean))
PIAQI2675 <- t(data.frame(apply(simAQI2675,2,PI,prob=0.89)))
F23AQISim2675 <- data.frame(cbind(simmeanAQI2675,PIAQI2675))
F23AQISim2675$PlotCN <- all2675cc23  $PlotCN
F23AQISim2675 <- F23AQISim2675 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2675) <- c("PlotCN","AQI","5CI","95CI")
simAQI4575 <- link(AQISplit23, data=all4575cc23 )
simmeanAQI4575 <- data.frame(apply(simAQI4575,2,mean))
PIAQI4575 <- t(data.frame(apply(simAQI4575,2,PI,prob=0.89)))
F23AQISim4575 <- data.frame(cbind(simmeanAQI4575,PIAQI4575))
F23AQISim4575$PlotCN <- all4575cc23  $PlotCN
F23AQISim4575 <- F23AQISim4575 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4575) <- c("PlotCN","AQI","5CI","95CI")
simAQI8575 <- link(AQISplit23, data=all8575cc23 )
simmeanAQI8575 <- data.frame(apply(simAQI8575,2,mean))
PIAQI8575 <- t(data.frame(apply(simAQI8575,2,PI,prob=0.89)))
F23AQISim8575 <- data.frame(cbind(simmeanAQI8575,PIAQI8575))
F23AQISim8575$PlotCN <- all8575cc23  $PlotCN
F23AQISim8575 <- F23AQISim8575 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8575) <- c("PlotCN","AQI","5CI","95CI")

simAQI2680 <- link(AQISplit23, data=all2680cc23 )
simmeanAQI2680 <- data.frame(apply(simAQI2680,2,mean))
PIAQI2680 <- t(data.frame(apply(simAQI2680,2,PI,prob=0.89)))
F23AQISim2680 <- data.frame(cbind(simmeanAQI2680,PIAQI2680))
F23AQISim2680$PlotCN <- all2680cc23  $PlotCN
F23AQISim2680 <- F23AQISim2680 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim2680) <- c("PlotCN","AQI","5CI","95CI")
simAQI4580 <- link(AQISplit23, data=all4580cc23 )
simmeanAQI4580 <- data.frame(apply(simAQI4580,2,mean))
PIAQI4580 <- t(data.frame(apply(simAQI4580,2,PI,prob=0.89)))
F23AQISim4580 <- data.frame(cbind(simmeanAQI4580,PIAQI4580))
F23AQISim4580$PlotCN <- all4580cc23  $PlotCN
F23AQISim4580 <- F23AQISim4580 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim4580) <- c("PlotCN","AQI","5CI","95CI")
simAQI8580 <- link(AQISplit23, data=all8580cc23 )
simmeanAQI8580 <- data.frame(apply(simAQI8580,2,mean))
PIAQI8580 <- t(data.frame(apply(simAQI8580,2,PI,prob=0.89)))
F23AQISim8580 <- data.frame(cbind(simmeanAQI8580,PIAQI8580))
F23AQISim8580$PlotCN <- all8580cc23  $PlotCN
F23AQISim8580 <- F23AQISim8580 %>% dplyr::select(PlotCN, everything())
colnames(F23AQISim8580) <- c("PlotCN","AQI","5CI","95CI")

#create vector of new colnames
AQISimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("AQI", "5CI", "95CI"), x)))
AQISimnames<- append(AQISimnames,"PlotCN",after=0)

#group data by RCP pathway
F23AQISim26 <- F23AQISim2625 %>% cbind(F23AQISim2630[,2:4]) %>%
  cbind(F23AQISim2635[,2:4]) %>% cbind(F23AQISim2640[,2:4]) %>% cbind(F23AQISim2645[,2:4]) %>%
  cbind(F23AQISim2650[,2:4]) %>% cbind(F23AQISim2655[,2:4]) %>% cbind(F23AQISim2660[,2:4]) %>%
  cbind(F23AQISim2665[,2:4]) %>% cbind(F23AQISim2670[,2:4]) %>% cbind(F23AQISim2675[,2:4]) %>%
  cbind(F23AQISim2680[,2:4])
colnames(F23AQISim26) <- AQISimnames
write.csv(F23AQISim26,file="F23AQIPred26T.csv")

F23AQISim45 <- F23AQISim4525 %>% cbind(F23AQISim4530[,2:4]) %>%
  cbind(F23AQISim4535[,2:4]) %>% cbind(F23AQISim4540[,2:4]) %>% cbind(F23AQISim4545[,2:4]) %>%
  cbind(F23AQISim4550[,2:4]) %>% cbind(F23AQISim4555[,2:4]) %>% cbind(F23AQISim4560[,2:4]) %>%
  cbind(F23AQISim4565[,2:4]) %>% cbind(F23AQISim4570[,2:4]) %>% cbind(F23AQISim4575[,2:4]) %>%
  cbind(F23AQISim4580[,2:4])
colnames(F23AQISim45) <- AQISimnames
write.csv(F23AQISim45,file="F23AQIPred45T.csv")

F23AQISim85 <- F23AQISim8525 %>% cbind(F23AQISim8530[,2:4]) %>%
  cbind(F23AQISim8535[,2:4]) %>% cbind(F23AQISim8540[,2:4]) %>% cbind(F23AQISim8545[,2:4]) %>%
  cbind(F23AQISim8550[,2:4]) %>% cbind(F23AQISim8555[,2:4]) %>% cbind(F23AQISim8560[,2:4]) %>%
  cbind(F23AQISim8565[,2:4]) %>% cbind(F23AQISim8570[,2:4]) %>% cbind(F23AQISim8575[,2:4]) %>%
  cbind(F23AQISim8580[,2:4])
colnames(F23AQISim85) <- AQISimnames
write.csv(F23AQISim85,file="F23AQIPred85T.csv")


#fgroup 24
simAQI2625 <- link(AQISplit24, data=all2625cc24 )
simmeanAQI2625 <- data.frame(apply(simAQI2625,2,mean))
PIAQI2625 <- t(data.frame(apply(simAQI2625,2,PI,prob=0.89)))
F24AQISim2625 <- data.frame(cbind(simmeanAQI2625,PIAQI2625))
F24AQISim2625$PlotCN <- all2625cc24  $PlotCN
F24AQISim2625 <- F24AQISim2625 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2625) <- c("PlotCN","AQI","5CI","95CI")
simAQI4525 <- link(AQISplit24, data=all4525cc24 )
simmeanAQI4525 <- data.frame(apply(simAQI4525,2,mean))
PIAQI4525 <- t(data.frame(apply(simAQI4525,2,PI,prob=0.89)))
F24AQISim4525 <- data.frame(cbind(simmeanAQI4525,PIAQI4525))
F24AQISim4525$PlotCN <- all4525cc24  $PlotCN
F24AQISim4525 <- F24AQISim4525 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4525) <- c("PlotCN","AQI","5CI","95CI")
simAQI8525 <- link(AQISplit24, data=all8525cc24 )
simmeanAQI8525 <- data.frame(apply(simAQI8525,2,mean))
PIAQI8525 <- t(data.frame(apply(simAQI8525,2,PI,prob=0.89)))
F24AQISim8525 <- data.frame(cbind(simmeanAQI8525,PIAQI8525))
F24AQISim8525$PlotCN <- all8525cc24  $PlotCN
F24AQISim8525 <- F24AQISim8525 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8525) <- c("PlotCN","AQI","5CI","95CI")

simAQI2630 <- link(AQISplit24, data=all2630cc24 )
simmeanAQI2630 <- data.frame(apply(simAQI2630,2,mean))
PIAQI2630 <- t(data.frame(apply(simAQI2630,2,PI,prob=0.89)))
F24AQISim2630 <- data.frame(cbind(simmeanAQI2630,PIAQI2630))
F24AQISim2630$PlotCN <- all2630cc24  $PlotCN
F24AQISim2630 <- F24AQISim2630 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2630) <- c("PlotCN","AQI","5CI","95CI")
simAQI4530 <- link(AQISplit24, data=all4530cc24 )
simmeanAQI4530 <- data.frame(apply(simAQI4530,2,mean))
PIAQI4530 <- t(data.frame(apply(simAQI4530,2,PI,prob=0.89)))
F24AQISim4530 <- data.frame(cbind(simmeanAQI4530,PIAQI4530))
F24AQISim4530$PlotCN <- all4530cc24  $PlotCN
F24AQISim4530 <- F24AQISim4530 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4530) <- c("PlotCN","AQI","5CI","95CI")
simAQI8530 <- link(AQISplit24, data=all8530cc24 )
simmeanAQI8530 <- data.frame(apply(simAQI8530,2,mean))
PIAQI8530 <- t(data.frame(apply(simAQI8530,2,PI,prob=0.89)))
F24AQISim8530 <- data.frame(cbind(simmeanAQI8530,PIAQI8530))
F24AQISim8530$PlotCN <- all8530cc24  $PlotCN
F24AQISim8530 <- F24AQISim8530 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8530) <- c("PlotCN","AQI","5CI","95CI")

simAQI2635 <- link(AQISplit24, data=all2635cc24 )
simmeanAQI2635 <- data.frame(apply(simAQI2635,2,mean))
PIAQI2635 <- t(data.frame(apply(simAQI2635,2,PI,prob=0.89)))
F24AQISim2635 <- data.frame(cbind(simmeanAQI2635,PIAQI2635))
F24AQISim2635$PlotCN <- all2635cc24  $PlotCN
F24AQISim2635 <- F24AQISim2635 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2635) <- c("PlotCN","AQI","5CI","95CI")
simAQI4535 <- link(AQISplit24, data=all4535cc24 )
simmeanAQI4535 <- data.frame(apply(simAQI4535,2,mean))
PIAQI4535 <- t(data.frame(apply(simAQI4535,2,PI,prob=0.89)))
F24AQISim4535 <- data.frame(cbind(simmeanAQI4535,PIAQI4535))
F24AQISim4535$PlotCN <- all4535cc24  $PlotCN
F24AQISim4535 <- F24AQISim4535 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4535) <- c("PlotCN","AQI","5CI","95CI")
simAQI8535 <- link(AQISplit24, data=all8535cc24 )
simmeanAQI8535 <- data.frame(apply(simAQI8535,2,mean))
PIAQI8535 <- t(data.frame(apply(simAQI8535,2,PI,prob=0.89)))
F24AQISim8535 <- data.frame(cbind(simmeanAQI8535,PIAQI8535))
F24AQISim8535$PlotCN <- all8535cc24  $PlotCN
F24AQISim8535 <- F24AQISim8535 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8535) <- c("PlotCN","AQI","5CI","95CI")

simAQI2640 <- link(AQISplit24, data=all2640cc24 )
simmeanAQI2640 <- data.frame(apply(simAQI2640,2,mean))
PIAQI2640 <- t(data.frame(apply(simAQI2640,2,PI,prob=0.89)))
F24AQISim2640 <- data.frame(cbind(simmeanAQI2640,PIAQI2640))
F24AQISim2640$PlotCN <- all2640cc24  $PlotCN
F24AQISim2640 <- F24AQISim2640 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2640) <- c("PlotCN","AQI","5CI","95CI")
simAQI4540 <- link(AQISplit24, data=all4540cc24 )
simmeanAQI4540 <- data.frame(apply(simAQI4540,2,mean))
PIAQI4540 <- t(data.frame(apply(simAQI4540,2,PI,prob=0.89)))
F24AQISim4540 <- data.frame(cbind(simmeanAQI4540,PIAQI4540))
F24AQISim4540$PlotCN <- all4540cc24  $PlotCN
F24AQISim4540 <- F24AQISim4540 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4540) <- c("PlotCN","AQI","5CI","95CI")
simAQI8540 <- link(AQISplit24, data=all8540cc24 )
simmeanAQI8540 <- data.frame(apply(simAQI8540,2,mean))
PIAQI8540 <- t(data.frame(apply(simAQI8540,2,PI,prob=0.89)))
F24AQISim8540 <- data.frame(cbind(simmeanAQI8540,PIAQI8540))
F24AQISim8540$PlotCN <- all8540cc24  $PlotCN
F24AQISim8540 <- F24AQISim8540 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8540) <- c("PlotCN","AQI","5CI","95CI")

simAQI2645 <- link(AQISplit24, data=all2645cc24 )
simmeanAQI2645 <- data.frame(apply(simAQI2645,2,mean))
PIAQI2645 <- t(data.frame(apply(simAQI2645,2,PI,prob=0.89)))
F24AQISim2645 <- data.frame(cbind(simmeanAQI2645,PIAQI2645))
F24AQISim2645$PlotCN <- all2645cc24  $PlotCN
F24AQISim2645 <- F24AQISim2645 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2645) <- c("PlotCN","AQI","5CI","95CI")
simAQI4545 <- link(AQISplit24, data=all4545cc24 )
simmeanAQI4545 <- data.frame(apply(simAQI4545,2,mean))
PIAQI4545 <- t(data.frame(apply(simAQI4545,2,PI,prob=0.89)))
F24AQISim4545 <- data.frame(cbind(simmeanAQI4545,PIAQI4545))
F24AQISim4545$PlotCN <- all4545cc24  $PlotCN
F24AQISim4545 <- F24AQISim4545 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4545) <- c("PlotCN","AQI","5CI","95CI")
simAQI8545 <- link(AQISplit24, data=all8545cc24 )
simmeanAQI8545 <- data.frame(apply(simAQI8545,2,mean))
PIAQI8545 <- t(data.frame(apply(simAQI8545,2,PI,prob=0.89)))
F24AQISim8545 <- data.frame(cbind(simmeanAQI8545,PIAQI8545))
F24AQISim8545$PlotCN <- all8545cc24  $PlotCN
F24AQISim8545 <- F24AQISim8545 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8545) <- c("PlotCN","AQI","5CI","95CI")

simAQI2650 <- link(AQISplit24, data=all2650cc24 )
simmeanAQI2650 <- data.frame(apply(simAQI2650,2,mean))
PIAQI2650 <- t(data.frame(apply(simAQI2650,2,PI,prob=0.89)))
F24AQISim2650 <- data.frame(cbind(simmeanAQI2650,PIAQI2650))
F24AQISim2650$PlotCN <- all2650cc24  $PlotCN
F24AQISim2650 <- F24AQISim2650 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2650) <- c("PlotCN","AQI","5CI","95CI")
simAQI4550 <- link(AQISplit24, data=all4550cc24 )
simmeanAQI4550 <- data.frame(apply(simAQI4550,2,mean))
PIAQI4550 <- t(data.frame(apply(simAQI4550,2,PI,prob=0.89)))
F24AQISim4550 <- data.frame(cbind(simmeanAQI4550,PIAQI4550))
F24AQISim4550$PlotCN <- all4550cc24  $PlotCN
F24AQISim4550 <- F24AQISim4550 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4550) <- c("PlotCN","AQI","5CI","95CI")
simAQI8550 <- link(AQISplit24, data=all8550cc24 )
simmeanAQI8550 <- data.frame(apply(simAQI8550,2,mean))
PIAQI8550 <- t(data.frame(apply(simAQI8550,2,PI,prob=0.89)))
F24AQISim8550 <- data.frame(cbind(simmeanAQI8550,PIAQI8550))
F24AQISim8550$PlotCN <- all8550cc24  $PlotCN
F24AQISim8550 <- F24AQISim8550 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8550) <- c("PlotCN","AQI","5CI","95CI")

simAQI2655 <- link(AQISplit24, data=all2655cc24 )
simmeanAQI2655 <- data.frame(apply(simAQI2655,2,mean))
PIAQI2655 <- t(data.frame(apply(simAQI2655,2,PI,prob=0.89)))
F24AQISim2655 <- data.frame(cbind(simmeanAQI2655,PIAQI2655))
F24AQISim2655$PlotCN <- all2655cc24  $PlotCN
F24AQISim2655 <- F24AQISim2655 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2655) <- c("PlotCN","AQI","5CI","95CI")
simAQI4555 <- link(AQISplit24, data=all4555cc24 )
simmeanAQI4555 <- data.frame(apply(simAQI4555,2,mean))
PIAQI4555 <- t(data.frame(apply(simAQI4555,2,PI,prob=0.89)))
F24AQISim4555 <- data.frame(cbind(simmeanAQI4555,PIAQI4555))
F24AQISim4555$PlotCN <- all4555cc24  $PlotCN
F24AQISim4555 <- F24AQISim4555 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4555) <- c("PlotCN","AQI","5CI","95CI")
simAQI8555 <- link(AQISplit24, data=all8555cc24 )
simmeanAQI8555 <- data.frame(apply(simAQI8555,2,mean))
PIAQI8555 <- t(data.frame(apply(simAQI8555,2,PI,prob=0.89)))
F24AQISim8555 <- data.frame(cbind(simmeanAQI8555,PIAQI8555))
F24AQISim8555$PlotCN <- all8555cc24  $PlotCN
F24AQISim8555 <- F24AQISim8555 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8555) <- c("PlotCN","AQI","5CI","95CI")

simAQI2660 <- link(AQISplit24, data=all2660cc24 )
simmeanAQI2660 <- data.frame(apply(simAQI2660,2,mean))
PIAQI2660 <- t(data.frame(apply(simAQI2660,2,PI,prob=0.89)))
F24AQISim2660 <- data.frame(cbind(simmeanAQI2660,PIAQI2660))
F24AQISim2660$PlotCN <- all2660cc24  $PlotCN
F24AQISim2660 <- F24AQISim2660 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2660) <- c("PlotCN","AQI","5CI","95CI")
simAQI4560 <- link(AQISplit24, data=all4560cc24 )
simmeanAQI4560 <- data.frame(apply(simAQI4560,2,mean))
PIAQI4560 <- t(data.frame(apply(simAQI4560,2,PI,prob=0.89)))
F24AQISim4560 <- data.frame(cbind(simmeanAQI4560,PIAQI4560))
F24AQISim4560$PlotCN <- all4560cc24  $PlotCN
F24AQISim4560 <- F24AQISim4560 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4560) <- c("PlotCN","AQI","5CI","95CI")
simAQI8560 <- link(AQISplit24, data=all8560cc24 )
simmeanAQI8560 <- data.frame(apply(simAQI8560,2,mean))
PIAQI8560 <- t(data.frame(apply(simAQI8560,2,PI,prob=0.89)))
F24AQISim8560 <- data.frame(cbind(simmeanAQI8560,PIAQI8560))
F24AQISim8560$PlotCN <- all8560cc24  $PlotCN
F24AQISim8560 <- F24AQISim8560 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8560) <- c("PlotCN","AQI","5CI","95CI")

simAQI2665 <- link(AQISplit24, data=all2665cc24 )
simmeanAQI2665 <- data.frame(apply(simAQI2665,2,mean))
PIAQI2665 <- t(data.frame(apply(simAQI2665,2,PI,prob=0.89)))
F24AQISim2665 <- data.frame(cbind(simmeanAQI2665,PIAQI2665))
F24AQISim2665$PlotCN <- all2665cc24  $PlotCN
F24AQISim2665 <- F24AQISim2665 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2665) <- c("PlotCN","AQI","5CI","95CI")
simAQI4565 <- link(AQISplit24, data=all4565cc24 )
simmeanAQI4565 <- data.frame(apply(simAQI4565,2,mean))
PIAQI4565 <- t(data.frame(apply(simAQI4565,2,PI,prob=0.89)))
F24AQISim4565 <- data.frame(cbind(simmeanAQI4565,PIAQI4565))
F24AQISim4565$PlotCN <- all4565cc24  $PlotCN
F24AQISim4565 <- F24AQISim4565 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4565) <- c("PlotCN","AQI","5CI","95CI")
simAQI8565 <- link(AQISplit24, data=all8565cc24 )
simmeanAQI8565 <- data.frame(apply(simAQI8565,2,mean))
PIAQI8565 <- t(data.frame(apply(simAQI8565,2,PI,prob=0.89)))
F24AQISim8565 <- data.frame(cbind(simmeanAQI8565,PIAQI8565))
F24AQISim8565$PlotCN <- all8565cc24  $PlotCN
F24AQISim8565 <- F24AQISim8565 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8565) <- c("PlotCN","AQI","5CI","95CI")

simAQI2670 <- link(AQISplit24, data=all2670cc24 )
simmeanAQI2670 <- data.frame(apply(simAQI2670,2,mean))
PIAQI2670 <- t(data.frame(apply(simAQI2670,2,PI,prob=0.89)))
F24AQISim2670 <- data.frame(cbind(simmeanAQI2670,PIAQI2670))
F24AQISim2670$PlotCN <- all2670cc24  $PlotCN
F24AQISim2670 <- F24AQISim2670 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2670) <- c("PlotCN","AQI","5CI","95CI")
simAQI4570 <- link(AQISplit24, data=all4570cc24 )
simmeanAQI4570 <- data.frame(apply(simAQI4570,2,mean))
PIAQI4570 <- t(data.frame(apply(simAQI4570,2,PI,prob=0.89)))
F24AQISim4570 <- data.frame(cbind(simmeanAQI4570,PIAQI4570))
F24AQISim4570$PlotCN <- all4570cc24  $PlotCN
F24AQISim4570 <- F24AQISim4570 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4570) <- c("PlotCN","AQI","5CI","95CI")
simAQI8570 <- link(AQISplit24, data=all8570cc24 )
simmeanAQI8570 <- data.frame(apply(simAQI8570,2,mean))
PIAQI8570 <- t(data.frame(apply(simAQI8570,2,PI,prob=0.89)))
F24AQISim8570 <- data.frame(cbind(simmeanAQI8570,PIAQI8570))
F24AQISim8570$PlotCN <- all8570cc24  $PlotCN
F24AQISim8570 <- F24AQISim8570 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8570) <- c("PlotCN","AQI","5CI","95CI")

simAQI2675 <- link(AQISplit24, data=all2675cc24 )
simmeanAQI2675 <- data.frame(apply(simAQI2675,2,mean))
PIAQI2675 <- t(data.frame(apply(simAQI2675,2,PI,prob=0.89)))
F24AQISim2675 <- data.frame(cbind(simmeanAQI2675,PIAQI2675))
F24AQISim2675$PlotCN <- all2675cc24  $PlotCN
F24AQISim2675 <- F24AQISim2675 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2675) <- c("PlotCN","AQI","5CI","95CI")
simAQI4575 <- link(AQISplit24, data=all4575cc24 )
simmeanAQI4575 <- data.frame(apply(simAQI4575,2,mean))
PIAQI4575 <- t(data.frame(apply(simAQI4575,2,PI,prob=0.89)))
F24AQISim4575 <- data.frame(cbind(simmeanAQI4575,PIAQI4575))
F24AQISim4575$PlotCN <- all4575cc24  $PlotCN
F24AQISim4575 <- F24AQISim4575 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4575) <- c("PlotCN","AQI","5CI","95CI")
simAQI8575 <- link(AQISplit24, data=all8575cc24 )
simmeanAQI8575 <- data.frame(apply(simAQI8575,2,mean))
PIAQI8575 <- t(data.frame(apply(simAQI8575,2,PI,prob=0.89)))
F24AQISim8575 <- data.frame(cbind(simmeanAQI8575,PIAQI8575))
F24AQISim8575$PlotCN <- all8575cc24  $PlotCN
F24AQISim8575 <- F24AQISim8575 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8575) <- c("PlotCN","AQI","5CI","95CI")

simAQI2680 <- link(AQISplit24, data=all2680cc24 )
simmeanAQI2680 <- data.frame(apply(simAQI2680,2,mean))
PIAQI2680 <- t(data.frame(apply(simAQI2680,2,PI,prob=0.89)))
F24AQISim2680 <- data.frame(cbind(simmeanAQI2680,PIAQI2680))
F24AQISim2680$PlotCN <- all2680cc24  $PlotCN
F24AQISim2680 <- F24AQISim2680 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim2680) <- c("PlotCN","AQI","5CI","95CI")
simAQI4580 <- link(AQISplit24, data=all4580cc24 )
simmeanAQI4580 <- data.frame(apply(simAQI4580,2,mean))
PIAQI4580 <- t(data.frame(apply(simAQI4580,2,PI,prob=0.89)))
F24AQISim4580 <- data.frame(cbind(simmeanAQI4580,PIAQI4580))
F24AQISim4580$PlotCN <- all4580cc24  $PlotCN
F24AQISim4580 <- F24AQISim4580 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim4580) <- c("PlotCN","AQI","5CI","95CI")
simAQI8580 <- link(AQISplit24, data=all8580cc24 )
simmeanAQI8580 <- data.frame(apply(simAQI8580,2,mean))
PIAQI8580 <- t(data.frame(apply(simAQI8580,2,PI,prob=0.89)))
F24AQISim8580 <- data.frame(cbind(simmeanAQI8580,PIAQI8580))
F24AQISim8580$PlotCN <- all8580cc24  $PlotCN
F24AQISim8580 <- F24AQISim8580 %>% dplyr::select(PlotCN, everything())
colnames(F24AQISim8580) <- c("PlotCN","AQI","5CI","95CI")

#create vector of new colnames
AQISimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("AQI", "5CI", "95CI"), x)))
AQISimnames<- append(AQISimnames,"PlotCN",after=0)

#group data by RCP pathway
F24AQISim26 <- F24AQISim2625 %>% cbind(F24AQISim2630[,2:4]) %>%
  cbind(F24AQISim2635[,2:4]) %>% cbind(F24AQISim2640[,2:4]) %>% cbind(F24AQISim2645[,2:4]) %>%
  cbind(F24AQISim2650[,2:4]) %>% cbind(F24AQISim2655[,2:4]) %>% cbind(F24AQISim2660[,2:4]) %>%
  cbind(F24AQISim2665[,2:4]) %>% cbind(F24AQISim2670[,2:4]) %>% cbind(F24AQISim2675[,2:4]) %>%
  cbind(F24AQISim2680[,2:4])
colnames(F24AQISim26) <- AQISimnames
write.csv(F24AQISim26,file="F24AQIPred26T.csv")

F24AQISim45 <- F24AQISim4525 %>% cbind(F24AQISim4530[,2:4]) %>%
  cbind(F24AQISim4535[,2:4]) %>% cbind(F24AQISim4540[,2:4]) %>% cbind(F24AQISim4545[,2:4]) %>%
  cbind(F24AQISim4550[,2:4]) %>% cbind(F24AQISim4555[,2:4]) %>% cbind(F24AQISim4560[,2:4]) %>%
  cbind(F24AQISim4565[,2:4]) %>% cbind(F24AQISim4570[,2:4]) %>% cbind(F24AQISim4575[,2:4]) %>%
  cbind(F24AQISim4580[,2:4])
colnames(F24AQISim45) <- AQISimnames
write.csv(F24AQISim45,file="F24AQIPred45T.csv")

F24AQISim85 <- F24AQISim8525 %>% cbind(F24AQISim8530[,2:4]) %>%
  cbind(F24AQISim8535[,2:4]) %>% cbind(F24AQISim8540[,2:4]) %>% cbind(F24AQISim8545[,2:4]) %>%
  cbind(F24AQISim8550[,2:4]) %>% cbind(F24AQISim8555[,2:4]) %>% cbind(F24AQISim8560[,2:4]) %>%
  cbind(F24AQISim8565[,2:4]) %>% cbind(F24AQISim8570[,2:4]) %>% cbind(F24AQISim8575[,2:4]) %>%
  cbind(F24AQISim8580[,2:4])
colnames(F24AQISim85) <- AQISimnames
write.csv(F24AQISim85,file="F24AQIPred85T.csv")

#Soil Quality Index

#Combine clim and rcp
SQI2625 <- merge(Clim26 [,c(1,2,14,26,38)], rcp26 [,c(1,16)])
SQI4525 <- merge(Clim45 [,c(1,2,14,26,38)], rcp45 [,c(1,16)])
SQI8525 <- merge(Clim85 [,c(1,2,14,26,38)], rcp85 [,c(1,16)])
SQI2630 <- merge(Clim26[,c(1,3,15,27,39)],rcp26[,c(1,17)])
SQI4530 <- merge(Clim45[,c(1,3,15,27,39)],rcp45[,c(1,17)])
SQI8530 <- merge(Clim85[,c(1,3,15,27,39)],rcp85[,c(1,17)])
SQI2635 <- merge(Clim26[,c(1,4,16,28,40)],rcp26[,c(1,18)])
SQI4535 <- merge(Clim45[,c(1,4,16,28,40)],rcp45[,c(1,18)])
SQI8535 <- merge(Clim85[,c(1,4,16,28,40)],rcp85[,c(1,18)])
SQI2640 <- merge(Clim26[,c(1,5,17,29,41)],rcp26[,c(1,19)])
SQI4540 <- merge(Clim45[,c(1,5,17,29,41)],rcp45[,c(1,19)])
SQI8540 <- merge(Clim85[,c(1,5,17,29,41)],rcp85[,c(1,19)])
SQI2645 <- merge(Clim26[,c(1,6,18,30,42)],rcp26[,c(1,20)])
SQI4545 <- merge(Clim45[,c(1,6,18,30,42)],rcp45[,c(1,20)])
SQI8545 <- merge(Clim85[,c(1,6,18,30,42)],rcp85[,c(1,20)])
SQI2650 <- merge(Clim26[,c(1,7,19,31,43)],rcp26[,c(1,21)])
SQI4550 <- merge(Clim45[,c(1,7,19,31,43)],rcp45[,c(1,21)])
SQI8550 <- merge(Clim85[,c(1,7,19,31,43)],rcp85[,c(1,21)])
SQI2655 <- merge(Clim26[,c(1,8,20,32,44)],rcp26[,c(1,22)])
SQI4555 <- merge(Clim45[,c(1,8,20,32,44)],rcp45[,c(1,22)])
SQI8555 <- merge(Clim85[,c(1,8,20,32,44)],rcp85[,c(1,22)])
SQI2660 <- merge(Clim26[,c(1,9,21,33,45)],rcp26[,c(1,23)])
SQI4560 <- merge(Clim45[,c(1,9,21,33,45)],rcp45[,c(1,23)])
SQI8560 <- merge(Clim85[,c(1,9,21,33,45)],rcp85[,c(1,23)])
SQI2665 <- merge(Clim26[,c(1,10,22,34,46)],rcp26[,c(1,24)])
SQI4565 <- merge(Clim45[,c(1,10,22,34,46)],rcp45[,c(1,24)])
SQI8565 <- merge(Clim85[,c(1,10,22,34,46)],rcp85[,c(1,24)])
SQI2670 <- merge(Clim26[,c(1,11,23,35,47)],rcp26[,c(1,25)])
SQI4570 <- merge(Clim45[,c(1,11,23,35,47)],rcp45[,c(1,25)])
SQI8570 <- merge(Clim85[,c(1,11,23,35,47)],rcp85[,c(1,25)])
SQI2675 <- merge(Clim26[,c(1,12,24,36,48)],rcp26[,c(1,26)])
SQI4575 <- merge(Clim45[,c(1,12,24,36,48)],rcp45[,c(1,26)])
SQI8575 <- merge(Clim85[,c(1,12,24,36,48)],rcp85[,c(1,26)])
SQI2680 <- merge(Clim26[,c(1,13,25,37,49)],rcp26[,c(1,27)])
SQI4580 <- merge(Clim45[,c(1,13,25,37,49)],rcp45[,c(1,27)])
SQI8580 <- merge(Clim85[,c(1,13,25,37,49)],rcp85[,c(1,27)])

colnames(SQI2625) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4525) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8525) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI2630) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4530) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8530) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI2635) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4535) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8535) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI2640) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4540) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8540) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI2645) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4545) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8545) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI2650) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4550) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8550) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI2655) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4555) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8555) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI2660) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4560) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8560) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI2665) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4565) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8565) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI2670) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4570) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8570) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI2675) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4575) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8575) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI2680) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI4580) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
colnames(SQI8580) <- c("PlotCN","MAT","PPT","RHUM","RAD","FAD")
#add other variables
SQI2625 <- merge(SQI2625,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4525 <- merge(SQI4525,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8525 <- merge(SQI8525,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI2630 <- merge(SQI2630,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4530 <- merge(SQI4530,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8530 <- merge(SQI8530,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI2635 <- merge(SQI2635,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4535 <- merge(SQI4535,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8535 <- merge(SQI8535,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI2640 <- merge(SQI2640,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4540 <- merge(SQI4540,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8540 <- merge(SQI8540,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI2645 <- merge(SQI2645,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4545 <- merge(SQI4545,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8545 <- merge(SQI8545,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI2650 <- merge(SQI2650,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4550 <- merge(SQI4550,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8550 <- merge(SQI8550,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI2655 <- merge(SQI2655,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4555 <- merge(SQI4555,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8555 <- merge(SQI8555,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI2660 <- merge(SQI2660,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4560 <- merge(SQI4560,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8560 <- merge(SQI8560,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI2665 <- merge(SQI2665,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4565 <- merge(SQI4565,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8565 <- merge(SQI8565,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI2670 <- merge(SQI2670,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4570 <- merge(SQI4570,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8570 <- merge(SQI8570,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI2675 <- merge(SQI2675,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4575 <- merge(SQI4575,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8575 <- merge(SQI8575,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI2680 <- merge(SQI2680,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI4580 <- merge(SQI4580,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
SQI8580 <- merge(SQI8580,SQIdata[c("PlotCN","soil_order","ForestType","Elevation")],by="PlotCN",all.x=TRUE)
#remove NAs
SQI2625 <- SQI2625 [complete.cases(SQI2625 []),]
SQI4525 <- SQI4525 [complete.cases(SQI4525 []),]
SQI8525 <- SQI8525 [complete.cases(SQI8525 []),]
SQI2630 <- SQI2630 [complete.cases(SQI2630 []),]
SQI4530 <- SQI4530 [complete.cases(SQI4530 []),]
SQI8530 <- SQI8530 [complete.cases(SQI8530 []),]
SQI2635 <- SQI2635 [complete.cases(SQI2635 []),]
SQI4535 <- SQI4535 [complete.cases(SQI4535 []),]
SQI8535 <- SQI8535 [complete.cases(SQI8535 []),]
SQI2640 <- SQI2640 [complete.cases(SQI2640 []),]
SQI4540 <- SQI4540 [complete.cases(SQI4540 []),]
SQI8540 <- SQI8540 [complete.cases(SQI8540 []),]
SQI2645 <- SQI2645 [complete.cases(SQI2645 []),]
SQI4545 <- SQI4545 [complete.cases(SQI4545 []),]
SQI8545 <- SQI8545 [complete.cases(SQI8545 []),]
SQI2650 <- SQI2650 [complete.cases(SQI2650 []),]
SQI4550 <- SQI4550 [complete.cases(SQI4550 []),]
SQI8550 <- SQI8550 [complete.cases(SQI8550 []),]
SQI2655 <- SQI2655 [complete.cases(SQI2655 []),]
SQI4555 <- SQI4555 [complete.cases(SQI4555 []),]
SQI8555 <- SQI8555 [complete.cases(SQI8555 []),]
SQI2660 <- SQI2660 [complete.cases(SQI2660 []),]
SQI4560 <- SQI4560 [complete.cases(SQI4560 []),]
SQI8560 <- SQI8560 [complete.cases(SQI8560 []),]
SQI2665 <- SQI2665 [complete.cases(SQI2665 []),]
SQI4565 <- SQI4565 [complete.cases(SQI4565 []),]
SQI8565 <- SQI8565 [complete.cases(SQI8565 []),]
SQI2670 <- SQI2670 [complete.cases(SQI2670 []),]
SQI4570 <- SQI4570 [complete.cases(SQI4570 []),]
SQI8570 <- SQI8570 [complete.cases(SQI8570 []),]
SQI2675 <- SQI2675 [complete.cases(SQI2675 []),]
SQI4575 <- SQI4575 [complete.cases(SQI4575 []),]
SQI8575 <- SQI8575 [complete.cases(SQI8575 []),]
SQI2680 <- SQI2680 [complete.cases(SQI2680 []),]
SQI4580 <- SQI4580 [complete.cases(SQI4580 []),]
SQI8580 <- SQI8580 [complete.cases(SQI8580 []),]

SQI2625 <- SQI2625 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4525 <- SQI4525 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8525 <- SQI8525 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI2630 <- SQI2630 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4530 <- SQI4530 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8530 <- SQI8530 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI2635 <- SQI2635 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4535 <- SQI4535 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8535 <- SQI8535 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI2640 <- SQI2640 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4540 <- SQI4540 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8540 <- SQI8540 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI2645 <- SQI2645 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4545 <- SQI4545 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8545 <- SQI8545 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI2650 <- SQI2650 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4550 <- SQI4550 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8550 <- SQI8550 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI2655 <- SQI2655 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4555 <- SQI4555 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8555 <- SQI8555 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI2660 <- SQI2660 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4560 <- SQI4560 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8560 <- SQI8560 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI2665 <- SQI2665 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4565 <- SQI4565 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8565 <- SQI8565 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI2670 <- SQI2670 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4570 <- SQI4570 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8570 <- SQI8570 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI2675 <- SQI2675 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4575 <- SQI4575 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8575 <- SQI8575 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI2680 <- SQI2680 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI4580 <- SQI4580 %>% filter(PlotCN%in%SQIdata$PlotCN)
SQI8580 <- SQI8580 %>% filter(PlotCN%in%SQIdata$PlotCN)
#remove duplicates
SQI2625 <- unique(SQI2625)
SQI4525 <- unique(SQI4525) 
SQI8525 <- unique(SQI8525) 
SQI2630 <- unique(SQI2630) 
SQI4530 <- unique(SQI4530) 
SQI8530 <- unique(SQI8530) 
SQI2635 <- unique(SQI2635) 
SQI4535 <- unique(SQI4535) 
SQI8535 <- unique(SQI8535) 
SQI2640 <- unique(SQI2640) 
SQI4540 <- unique(SQI4540) 
SQI8540 <- unique(SQI8540) 
SQI2645 <- unique(SQI2645) 
SQI4545 <- unique(SQI4545) 
SQI8545 <- unique(SQI8545) 
SQI2650 <- unique(SQI2650)
SQI4550 <- unique(SQI4550) 
SQI8550 <- unique(SQI8550) 
SQI2655 <- unique(SQI2655) 
SQI4555 <- unique(SQI4555) 
SQI8555 <- unique(SQI8555) 
SQI2660 <- unique(SQI2660) 
SQI4560 <- unique(SQI4560) 
SQI8560 <- unique(SQI8560) 
SQI2665 <- unique(SQI2665) 
SQI4565 <- unique(SQI4565) 
SQI8565 <- unique(SQI8565) 
SQI2670 <- unique(SQI2670) 
SQI4570 <- unique(SQI4570) 
SQI8570 <- unique(SQI8570) 
SQI2675 <- unique(SQI2675) 
SQI4575 <- unique(SQI4575) 
SQI8575 <- unique(SQI8575) 
SQI2680 <- unique(SQI2680) 
SQI4580 <- unique(SQI4580) 
SQI8580 <- unique(SQI8580) 
#factor/integer for soil order
SQI2625$Soil <- as.factor(SQI2625$soil_order)
SQI2625$Soil <- as.integer(SQI2625$Soil)
SQI4525$Soil <- as.factor(SQI4525$soil_order)
SQI4525$Soil <- as.integer(SQI4525$Soil)
SQI8525$Soil <- as.factor(SQI8525$soil_order)
SQI8525$Soil <- as.integer(SQI8525$Soil)
SQI2630$Soil <- as.factor(SQI2630$soil_order)
SQI2630$Soil <- as.integer(SQI2630$Soil)
SQI4530$Soil <- as.factor(SQI4530$soil_order)
SQI4530$Soil <- as.integer(SQI4530$Soil)
SQI8530$Soil <- as.factor(SQI8530$soil_order)
SQI8530$Soil <- as.integer(SQI8530$Soil)
SQI2635$Soil <- as.factor(SQI2635$soil_order)
SQI2635$Soil <- as.integer(SQI2635$Soil)
SQI4535$Soil <- as.factor(SQI4535$soil_order)
SQI4535$Soil <- as.integer(SQI4535$Soil)
SQI8535$Soil <- as.factor(SQI8535$soil_order)
SQI8535$Soil <- as.integer(SQI8535$Soil)
SQI2640$Soil <- as.factor(SQI2640$soil_order)
SQI2640$Soil <- as.integer(SQI2640$Soil)
SQI4540$Soil <- as.factor(SQI4540$soil_order)
SQI4540$Soil <- as.integer(SQI4540$Soil)
SQI8540$Soil <- as.factor(SQI8540$soil_order)
SQI8540$Soil <- as.integer(SQI8540$Soil)
SQI2645$Soil <- as.factor(SQI2645$soil_order)
SQI2645$Soil <- as.integer(SQI2645$Soil)
SQI4545$Soil <- as.factor(SQI4545$soil_order)
SQI4545$Soil <- as.integer(SQI4545$Soil)
SQI8545$Soil <- as.factor(SQI8545$soil_order)
SQI8545$Soil <- as.integer(SQI8545$Soil)
SQI2650$Soil <- as.factor(SQI2650$soil_order)
SQI2650$Soil <- as.integer(SQI2650$Soil)
SQI4550$Soil <- as.factor(SQI4550$soil_order)
SQI4550$Soil <- as.integer(SQI4550$Soil)
SQI8550$Soil <- as.factor(SQI8550$soil_order)
SQI8550$Soil <- as.integer(SQI8550$Soil)
SQI2655$Soil <- as.factor(SQI2655$soil_order)
SQI2655$Soil <- as.integer(SQI2655$Soil)
SQI4555$Soil <- as.factor(SQI4555$soil_order)
SQI4555$Soil <- as.integer(SQI4555$Soil)
SQI8555$Soil <- as.factor(SQI8555$soil_order)
SQI8555$Soil <- as.integer(SQI8555$Soil)
SQI2660$Soil <- as.factor(SQI2660$soil_order)
SQI2660$Soil <- as.integer(SQI2660$Soil)
SQI4560$Soil <- as.factor(SQI4560$soil_order)
SQI4560$Soil <- as.integer(SQI4560$Soil)
SQI8560$Soil <- as.factor(SQI8560$soil_order)
SQI8560$Soil <- as.integer(SQI8560$Soil)
SQI2665$Soil <- as.factor(SQI2665$soil_order)
SQI2665$Soil <- as.integer(SQI2665$Soil)
SQI4565$Soil <- as.factor(SQI4565$soil_order)
SQI4565$Soil <- as.integer(SQI4565$Soil)
SQI8565$Soil <- as.factor(SQI8565$soil_order)
SQI8565$Soil <- as.integer(SQI8565$Soil)
SQI2670$Soil <- as.factor(SQI2670$soil_order)
SQI2670$Soil <- as.integer(SQI2670$Soil)
SQI4570$Soil <- as.factor(SQI4570$soil_order)
SQI4570$Soil <- as.integer(SQI4570$Soil)
SQI8570$Soil <- as.factor(SQI8570$soil_order)
SQI8570$Soil <- as.integer(SQI8570$Soil)
SQI2675$Soil <- as.factor(SQI2675$soil_order)
SQI2675$Soil <- as.integer(SQI2675$Soil)
SQI4575$Soil <- as.factor(SQI4575$soil_order)
SQI4575$Soil <- as.integer(SQI4575$Soil)
SQI8575$Soil <- as.factor(SQI8575$soil_order)
SQI8575$Soil <- as.integer(SQI8575$Soil)
SQI2680$Soil <- as.factor(SQI2680$soil_order)
SQI2680$Soil <- as.integer(SQI2680$Soil)
SQI4580$Soil <- as.factor(SQI4580$soil_order)
SQI4580$Soil <- as.integer(SQI4580$Soil)
SQI8580$Soil <- as.factor(SQI8580$soil_order)
SQI8580$Soil <- as.integer(SQI8580$Soil)
#add forest group
SQI2625 <- SQI2625 %>% mutate(FGroup = Flabels[findInterval(SQI2625$ForestType,Fgroups)])
SQI4525 <- SQI4525 %>% mutate(FGroup = Flabels[findInterval(SQI4525$ForestType,Fgroups)])
SQI8525 <- SQI8525 %>% mutate(FGroup = Flabels[findInterval(SQI8525$ForestType,Fgroups)])
SQI2630 <- SQI2630 %>% mutate(FGroup = Flabels[findInterval(SQI2630$ForestType,Fgroups)])
SQI4530 <- SQI4530 %>% mutate(FGroup = Flabels[findInterval(SQI4530$ForestType,Fgroups)])
SQI8530 <- SQI8530 %>% mutate(FGroup = Flabels[findInterval(SQI8530$ForestType,Fgroups)])
SQI2635 <- SQI2635 %>% mutate(FGroup = Flabels[findInterval(SQI2635$ForestType,Fgroups)])
SQI4535 <- SQI4535 %>% mutate(FGroup = Flabels[findInterval(SQI4535$ForestType,Fgroups)])
SQI8535 <- SQI8535 %>% mutate(FGroup = Flabels[findInterval(SQI8535$ForestType,Fgroups)])
SQI2640 <- SQI2640 %>% mutate(FGroup = Flabels[findInterval(SQI2640$ForestType,Fgroups)])
SQI4540 <- SQI4540 %>% mutate(FGroup = Flabels[findInterval(SQI4540$ForestType,Fgroups)])
SQI8540 <- SQI8540 %>% mutate(FGroup = Flabels[findInterval(SQI8540$ForestType,Fgroups)])
SQI2645 <- SQI2645 %>% mutate(FGroup = Flabels[findInterval(SQI2645$ForestType,Fgroups)])
SQI4545 <- SQI4545 %>% mutate(FGroup = Flabels[findInterval(SQI4545$ForestType,Fgroups)])
SQI8545 <- SQI8545 %>% mutate(FGroup = Flabels[findInterval(SQI8545$ForestType,Fgroups)])
SQI2650 <- SQI2650 %>% mutate(FGroup = Flabels[findInterval(SQI2650$ForestType,Fgroups)])
SQI4550 <- SQI4550 %>% mutate(FGroup = Flabels[findInterval(SQI4550$ForestType,Fgroups)])
SQI8550 <- SQI8550 %>% mutate(FGroup = Flabels[findInterval(SQI8550$ForestType,Fgroups)])
SQI2655 <- SQI2655 %>% mutate(FGroup = Flabels[findInterval(SQI2655$ForestType,Fgroups)])
SQI4555 <- SQI4555 %>% mutate(FGroup = Flabels[findInterval(SQI4555$ForestType,Fgroups)])
SQI8555 <- SQI8555 %>% mutate(FGroup = Flabels[findInterval(SQI8555$ForestType,Fgroups)])
SQI2660 <- SQI2660 %>% mutate(FGroup = Flabels[findInterval(SQI2660$ForestType,Fgroups)])
SQI4560 <- SQI4560 %>% mutate(FGroup = Flabels[findInterval(SQI4560$ForestType,Fgroups)])
SQI8560 <- SQI8560 %>% mutate(FGroup = Flabels[findInterval(SQI8560$ForestType,Fgroups)])
SQI2665 <- SQI2665 %>% mutate(FGroup = Flabels[findInterval(SQI2665$ForestType,Fgroups)])
SQI4565 <- SQI4565 %>% mutate(FGroup = Flabels[findInterval(SQI4565$ForestType,Fgroups)])
SQI8565 <- SQI8565 %>% mutate(FGroup = Flabels[findInterval(SQI8565$ForestType,Fgroups)])
SQI2670 <- SQI2670 %>% mutate(FGroup = Flabels[findInterval(SQI2670$ForestType,Fgroups)])
SQI4570 <- SQI4570 %>% mutate(FGroup = Flabels[findInterval(SQI4570$ForestType,Fgroups)])
SQI8570 <- SQI8570 %>% mutate(FGroup = Flabels[findInterval(SQI8570$ForestType,Fgroups)])
SQI2675 <- SQI2675 %>% mutate(FGroup = Flabels[findInterval(SQI2675$ForestType,Fgroups)])
SQI4575 <- SQI4575 %>% mutate(FGroup = Flabels[findInterval(SQI4575$ForestType,Fgroups)])
SQI8575 <- SQI8575 %>% mutate(FGroup = Flabels[findInterval(SQI8575$ForestType,Fgroups)])
SQI2680 <- SQI2680 %>% mutate(FGroup = Flabels[findInterval(SQI2680$ForestType,Fgroups)])
SQI4580 <- SQI4580 %>% mutate(FGroup = Flabels[findInterval(SQI4580$ForestType,Fgroups)])
SQI8580 <- SQI8580 %>% mutate(FGroup = Flabels[findInterval(SQI8580$ForestType,Fgroups)])

#covert FGroup to integer/factor
SQI2625$FGroup<- as.integer(as.factor(SQI2625$FGroup))
SQI4525$FGroup<- as.integer(as.factor(SQI4525$FGroup))
SQI8525$FGroup<- as.integer(as.factor(SQI8525$FGroup))
SQI2630$FGroup <- as.integer(as.factor(SQI2630$FGroup))
SQI4530$FGroup <- as.integer(as.factor(SQI4530$FGroup))
SQI8530$FGroup <- as.integer(as.factor(SQI8530$FGroup))
SQI2635$FGroup<- as.integer(as.factor(SQI2635$FGroup))
SQI4535$FGroup<- as.integer(as.factor(SQI4535$FGroup))
SQI8535$FGroup<- as.integer(as.factor(SQI8535$FGroup))
SQI2640$FGroup <- as.integer(as.factor(SQI2640$FGroup))
SQI4540$FGroup <- as.integer(as.factor(SQI4540$FGroup))
SQI8540$FGroup <- as.integer(as.factor(SQI8540$FGroup))
SQI2645$FGroup<- as.integer(as.factor(SQI2645$FGroup))
SQI4545$FGroup<- as.integer(as.factor(SQI4545$FGroup))
SQI8545$FGroup<- as.integer(as.factor(SQI8545$FGroup))
SQI2650$FGroup <- as.integer(as.factor(SQI2650$FGroup))
SQI4550$FGroup <- as.integer(as.factor(SQI4550$FGroup))
SQI8550$FGroup <- as.integer(as.factor(SQI8550$FGroup))
SQI2655$FGroup<- as.integer(as.factor(SQI2655$FGroup))
SQI4555$FGroup<- as.integer(as.factor(SQI4555$FGroup))
SQI8555$FGroup<- as.integer(as.factor(SQI8555$FGroup))
SQI2660$FGroup <- as.integer(as.factor(SQI2660$FGroup))
SQI4560$FGroup <- as.integer(as.factor(SQI4560$FGroup))
SQI8560$FGroup <- as.integer(as.factor(SQI8560$FGroup))
SQI2665$FGroup<- as.integer(as.factor(SQI2665$FGroup))
SQI4565$FGroup<- as.integer(as.factor(SQI4565$FGroup))
SQI8565$FGroup<- as.integer(as.factor(SQI8565$FGroup))
SQI2670$FGroup <- as.integer(as.factor(SQI2670$FGroup))
SQI4570$FGroup <- as.integer(as.factor(SQI4570$FGroup))
SQI8570$FGroup <- as.integer(as.factor(SQI8570$FGroup))
SQI2675$FGroup<- as.integer(as.factor(SQI2675$FGroup))
SQI4575$FGroup<- as.integer(as.factor(SQI4575$FGroup))
SQI8575$FGroup<- as.integer(as.factor(SQI8575$FGroup))
SQI2680$FGroup <- as.integer(as.factor(SQI2680$FGroup))
SQI4580$FGroup <- as.integer(as.factor(SQI4580$FGroup))
SQI8580$FGroup <- as.integer(as.factor(SQI8580$FGroup))

#borrow scaling from model inputs, datSQI, MAT,PPT,RHUM,RAD,Elevation

#MAT
SQI2625$MAT<- as.data.frame(scale(SQI2625$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4525$MAT<- as.data.frame(scale(SQI4525$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8525$MAT<- as.data.frame(scale(SQI8525$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI2630$MAT <- as.data.frame(scale(SQI2630$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4530$MAT <- as.data.frame(scale(SQI4530$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8530$MAT <- as.data.frame(scale(SQI8530$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI2635$MAT<- as.data.frame(scale(SQI2635$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4535$MAT<- as.data.frame(scale(SQI4535$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8535$MAT<- as.data.frame(scale(SQI8535$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI2640$MAT <- as.data.frame(scale(SQI2640$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4540$MAT <- as.data.frame(scale(SQI4540$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8540$MAT <- as.data.frame(scale(SQI8540$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI2645$MAT<- as.data.frame(scale(SQI2645$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4545$MAT<- as.data.frame(scale(SQI4545$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8545$MAT<- as.data.frame(scale(SQI8545$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI2650$MAT <- as.data.frame(scale(SQI2650$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4550$MAT <- as.data.frame(scale(SQI4550$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8550$MAT <- as.data.frame(scale(SQI8550$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI2655$MAT<- as.data.frame(scale(SQI2655$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4555$MAT<- as.data.frame(scale(SQI4555$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8555$MAT<- as.data.frame(scale(SQI8555$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI2660$MAT <- as.data.frame(scale(SQI2660$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4560$MAT <- as.data.frame(scale(SQI4560$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8560$MAT <- as.data.frame(scale(SQI8560$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI2665$MAT<- as.data.frame(scale(SQI2665$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4565$MAT<- as.data.frame(scale(SQI4565$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8565$MAT<- as.data.frame(scale(SQI8565$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI2670$MAT <- as.data.frame(scale(SQI2670$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4570$MAT <- as.data.frame(scale(SQI4570$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8570$MAT <- as.data.frame(scale(SQI8570$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI2675$MAT<- as.data.frame(scale(SQI2675$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4575$MAT<- as.data.frame(scale(SQI4575$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8575$MAT<- as.data.frame(scale(SQI8575$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI2680$MAT <- as.data.frame(scale(SQI2680$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI4580$MAT <- as.data.frame(scale(SQI4580$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
SQI8580$MAT <- as.data.frame(scale(SQI8580$MAT, center=centerMATSQI,scale=scaleMATSQI),col.names="MAT")[, 1]
#PPT
SQI2625$PPT<- as.data.frame(scale(SQI2625$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4525$PPT<- as.data.frame(scale(SQI4525$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8525$PPT<- as.data.frame(scale(SQI8525$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI2630$PPT <- as.data.frame(scale(SQI2630$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4530$PPT <- as.data.frame(scale(SQI4530$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8530$PPT <- as.data.frame(scale(SQI8530$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI2635$PPT<- as.data.frame(scale(SQI2635$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4535$PPT<- as.data.frame(scale(SQI4535$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8535$PPT<- as.data.frame(scale(SQI8535$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI2640$PPT <- as.data.frame(scale(SQI2640$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4540$PPT <- as.data.frame(scale(SQI4540$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8540$PPT <- as.data.frame(scale(SQI8540$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI2645$PPT<- as.data.frame(scale(SQI2645$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4545$PPT<- as.data.frame(scale(SQI4545$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8545$PPT<- as.data.frame(scale(SQI8545$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI2650$PPT <- as.data.frame(scale(SQI2650$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4550$PPT <- as.data.frame(scale(SQI4550$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8550$PPT <- as.data.frame(scale(SQI8550$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI2655$PPT<- as.data.frame(scale(SQI2655$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4555$PPT<- as.data.frame(scale(SQI4555$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8555$PPT<- as.data.frame(scale(SQI8555$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI2660$PPT <- as.data.frame(scale(SQI2660$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4560$PPT <- as.data.frame(scale(SQI4560$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8560$PPT <- as.data.frame(scale(SQI8560$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI2665$PPT<- as.data.frame(scale(SQI2665$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4565$PPT<- as.data.frame(scale(SQI4565$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8565$PPT<- as.data.frame(scale(SQI8565$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI2670$PPT <- as.data.frame(scale(SQI2670$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4570$PPT <- as.data.frame(scale(SQI4570$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8570$PPT <- as.data.frame(scale(SQI8570$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI2675$PPT<- as.data.frame(scale(SQI2675$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4575$PPT<- as.data.frame(scale(SQI4575$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8575$PPT<- as.data.frame(scale(SQI8575$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI2680$PPT <- as.data.frame(scale(SQI2680$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI4580$PPT <- as.data.frame(scale(SQI4580$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
SQI8580$PPT <- as.data.frame(scale(SQI8580$PPT, center=centerPPTSQI,scale=scalePPTSQI),col.names="PPT")[, 1]
#RHUM
SQI2625$RHUM<- as.data.frame(scale(SQI2625$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4525$RHUM<- as.data.frame(scale(SQI4525$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8525$RHUM<- as.data.frame(scale(SQI8525$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI2630$RHUM <- as.data.frame(scale(SQI2630$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4530$RHUM <- as.data.frame(scale(SQI4530$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8530$RHUM <- as.data.frame(scale(SQI8530$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI2635$RHUM<- as.data.frame(scale(SQI2635$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4535$RHUM<- as.data.frame(scale(SQI4535$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8535$RHUM<- as.data.frame(scale(SQI8535$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI2640$RHUM <- as.data.frame(scale(SQI2640$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4540$RHUM <- as.data.frame(scale(SQI4540$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8540$RHUM <- as.data.frame(scale(SQI8540$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI2645$RHUM<- as.data.frame(scale(SQI2645$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4545$RHUM<- as.data.frame(scale(SQI4545$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8545$RHUM<- as.data.frame(scale(SQI8545$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI2650$RHUM <- as.data.frame(scale(SQI2650$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4550$RHUM <- as.data.frame(scale(SQI4550$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8550$RHUM <- as.data.frame(scale(SQI8550$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI2655$RHUM<- as.data.frame(scale(SQI2655$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4555$RHUM<- as.data.frame(scale(SQI4555$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8555$RHUM<- as.data.frame(scale(SQI8555$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI2660$RHUM <- as.data.frame(scale(SQI2660$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4560$RHUM <- as.data.frame(scale(SQI4560$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8560$RHUM <- as.data.frame(scale(SQI8560$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI2665$RHUM<- as.data.frame(scale(SQI2665$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4565$RHUM<- as.data.frame(scale(SQI4565$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8565$RHUM<- as.data.frame(scale(SQI8565$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI2670$RHUM <- as.data.frame(scale(SQI2670$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4570$RHUM <- as.data.frame(scale(SQI4570$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8570$RHUM <- as.data.frame(scale(SQI8570$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI2675$RHUM<- as.data.frame(scale(SQI2675$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4575$RHUM<- as.data.frame(scale(SQI4575$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8575$RHUM<- as.data.frame(scale(SQI8575$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI2680$RHUM <- as.data.frame(scale(SQI2680$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI4580$RHUM <- as.data.frame(scale(SQI4580$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
SQI8580$RHUM <- as.data.frame(scale(SQI8580$RHUM, center=centerRHUMSQI,scale=scaleRHUMSQI),col.names="RHUM")[, 1]
#RAD
SQI2625$RAD<- as.data.frame(scale(SQI2625$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4525$RAD<- as.data.frame(scale(SQI4525$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8525$RAD<- as.data.frame(scale(SQI8525$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI2630$RAD <- as.data.frame(scale(SQI2630$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4530$RAD <- as.data.frame(scale(SQI4530$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8530$RAD <- as.data.frame(scale(SQI8530$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI2635$RAD<- as.data.frame(scale(SQI2635$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4535$RAD<- as.data.frame(scale(SQI4535$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8535$RAD<- as.data.frame(scale(SQI8535$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI2640$RAD <- as.data.frame(scale(SQI2640$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4540$RAD <- as.data.frame(scale(SQI4540$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8540$RAD <- as.data.frame(scale(SQI8540$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI2645$RAD<- as.data.frame(scale(SQI2645$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4545$RAD<- as.data.frame(scale(SQI4545$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8545$RAD<- as.data.frame(scale(SQI8545$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI2650$RAD <- as.data.frame(scale(SQI2650$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4550$RAD <- as.data.frame(scale(SQI4550$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8550$RAD <- as.data.frame(scale(SQI8550$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI2655$RAD<- as.data.frame(scale(SQI2655$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4555$RAD<- as.data.frame(scale(SQI4555$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8555$RAD<- as.data.frame(scale(SQI8555$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI2660$RAD <- as.data.frame(scale(SQI2660$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4560$RAD <- as.data.frame(scale(SQI4560$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8560$RAD <- as.data.frame(scale(SQI8560$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI2665$RAD<- as.data.frame(scale(SQI2665$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4565$RAD<- as.data.frame(scale(SQI4565$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8565$RAD<- as.data.frame(scale(SQI8565$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI2670$RAD <- as.data.frame(scale(SQI2670$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4570$RAD <- as.data.frame(scale(SQI4570$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8570$RAD <- as.data.frame(scale(SQI8570$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI2675$RAD<- as.data.frame(scale(SQI2675$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4575$RAD<- as.data.frame(scale(SQI4575$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8575$RAD<- as.data.frame(scale(SQI8575$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI2680$RAD <- as.data.frame(scale(SQI2680$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI4580$RAD <- as.data.frame(scale(SQI4580$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
SQI8580$RAD <- as.data.frame(scale(SQI8580$RAD, center=centerRADSQI,scale=scaleRADSQI),col.names="RAD")[, 1]
#Elevation
SQI2625$Elevation<- as.data.frame(scale(SQI2625$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4525$Elevation<- as.data.frame(scale(SQI4525$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8525$Elevation<- as.data.frame(scale(SQI8525$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI2630$Elevation <- as.data.frame(scale(SQI2630$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4530$Elevation <- as.data.frame(scale(SQI4530$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8530$Elevation <- as.data.frame(scale(SQI8530$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI2635$Elevation<- as.data.frame(scale(SQI2635$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4535$Elevation<- as.data.frame(scale(SQI4535$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8535$Elevation<- as.data.frame(scale(SQI8535$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI2640$Elevation <- as.data.frame(scale(SQI2640$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4540$Elevation <- as.data.frame(scale(SQI4540$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8540$Elevation <- as.data.frame(scale(SQI8540$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI2645$Elevation<- as.data.frame(scale(SQI2645$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4545$Elevation<- as.data.frame(scale(SQI4545$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8545$Elevation<- as.data.frame(scale(SQI8545$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI2650$Elevation <- as.data.frame(scale(SQI2650$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4550$Elevation <- as.data.frame(scale(SQI4550$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8550$Elevation <- as.data.frame(scale(SQI8550$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI2655$Elevation<- as.data.frame(scale(SQI2655$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4555$Elevation<- as.data.frame(scale(SQI4555$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8555$Elevation<- as.data.frame(scale(SQI8555$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI2660$Elevation <- as.data.frame(scale(SQI2660$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4560$Elevation <- as.data.frame(scale(SQI4560$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8560$Elevation <- as.data.frame(scale(SQI8560$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI2665$Elevation<- as.data.frame(scale(SQI2665$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4565$Elevation<- as.data.frame(scale(SQI4565$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8565$Elevation<- as.data.frame(scale(SQI8565$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI2670$Elevation <- as.data.frame(scale(SQI2670$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4570$Elevation <- as.data.frame(scale(SQI4570$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8570$Elevation <- as.data.frame(scale(SQI8570$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI2675$Elevation<- as.data.frame(scale(SQI2675$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4575$Elevation<- as.data.frame(scale(SQI4575$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8575$Elevation<- as.data.frame(scale(SQI8575$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI2680$Elevation <- as.data.frame(scale(SQI2680$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI4580$Elevation <- as.data.frame(scale(SQI4580$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]
SQI8580$Elevation <- as.data.frame(scale(SQI8580$Elevation, center=centerElevationSQI,scale=scaleElevationSQI),col.names="Elevation")[, 1]

SQI2625$time_weight <- 1.156119
SQI4525$time_weight <- 1.156119
SQI8525$time_weight <- 1.156119
SQI2630$time_weight <- 1.156119
SQI4530$time_weight <- 1.156119
SQI8530$time_weight <- 1.156119
SQI2635$time_weight <- 1.156119
SQI4535$time_weight <- 1.156119
SQI8535$time_weight <- 1.156119
SQI2640$time_weight <- 1.156119
SQI4540$time_weight <- 1.156119
SQI8540$time_weight <- 1.156119
SQI2645$time_weight <- 1.156119
SQI4545$time_weight <- 1.156119
SQI8545$time_weight <- 1.156119
SQI2650$time_weight <- 1.156119
SQI4550$time_weight <- 1.156119
SQI8550$time_weight <- 1.156119
SQI2655$time_weight <- 1.156119
SQI4555$time_weight <- 1.156119
SQI8555$time_weight <- 1.156119
SQI2660$time_weight <- 1.156119
SQI4560$time_weight <- 1.156119
SQI8560$time_weight <- 1.156119
SQI2665$time_weight <- 1.156119
SQI4565$time_weight <- 1.156119
SQI8565$time_weight <- 1.156119
SQI2670$time_weight <- 1.156119
SQI4570$time_weight <- 1.156119
SQI8570$time_weight <- 1.156119
SQI2675$time_weight <- 1.156119
SQI4575$time_weight <- 1.156119
SQI8575$time_weight <- 1.156119
SQI2680$time_weight <- 1.156119
SQI4580$time_weight <- 1.156119
SQI8580$time_weight <- 1.156119


#run simulations
simSQI2625 <- link(SQIMod_beta, data=SQI2625)
simmeanSQI2625 <- data.frame(apply(simSQI2625,2,mean))
PISQI2625 <- t(data.frame(apply(simSQI2625,2,PI,prob=0.89)))
SQISim2625 <- data.frame(cbind(simmeanSQI2625,PISQI2625))
SQISim2625$PlotCN <- SQI2625$PlotCN
SQISim2625 <- SQISim2625 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2625) <- c("PlotCN","SQI","5CI","95CI")
simSQI4525 <- link(SQIMod_beta, data=SQI4525)
simmeanSQI4525 <- data.frame(apply(simSQI4525,2,mean))
PISQI4525 <- t(data.frame(apply(simSQI4525,2,PI,prob=0.89)))
SQISim4525 <- data.frame(cbind(simmeanSQI4525,PISQI4525))
SQISim4525$PlotCN <- SQI4525$PlotCN
SQISim4525 <- SQISim4525 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4525) <- c("PlotCN","SQI","5CI","95CI")
simSQI8525 <- link(SQIMod_beta, data=SQI8525)
simmeanSQI8525 <- data.frame(apply(simSQI8525,2,mean))
PISQI8525 <- t(data.frame(apply(simSQI8525,2,PI,prob=0.89)))
SQISim8525 <- data.frame(cbind(simmeanSQI8525,PISQI8525))
SQISim8525$PlotCN <- SQI8525$PlotCN
SQISim8525 <- SQISim8525 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8525) <- c("PlotCN","SQI","5CI","95CI")

simSQI2630 <- link(SQIMod_beta, data=SQI2630)
simmeanSQI2630 <- data.frame(apply(simSQI2630,2,mean))
PISQI2630 <- t(data.frame(apply(simSQI2630,2,PI,prob=0.89)))
SQISim2630 <- data.frame(cbind(simmeanSQI2630,PISQI2630))
SQISim2630$PlotCN <- SQI2630$PlotCN
SQISim2630 <- SQISim2630 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2630) <- c("PlotCN","SQI","5CI","95CI")
simSQI4530 <- link(SQIMod_beta, data=SQI4530)
simmeanSQI4530 <- data.frame(apply(simSQI4530,2,mean))
PISQI4530 <- t(data.frame(apply(simSQI4530,2,PI,prob=0.89)))
SQISim4530 <- data.frame(cbind(simmeanSQI4530,PISQI4530))
SQISim4530$PlotCN <- SQI4530$PlotCN
SQISim4530 <- SQISim4530 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4530) <- c("PlotCN","SQI","5CI","95CI")
simSQI8530 <- link(SQIMod_beta, data=SQI8530)
simmeanSQI8530 <- data.frame(apply(simSQI8530,2,mean))
PISQI8530 <- t(data.frame(apply(simSQI8530,2,PI,prob=0.89)))
SQISim8530 <- data.frame(cbind(simmeanSQI8530,PISQI8530))
SQISim8530$PlotCN <- SQI8530$PlotCN
SQISim8530 <- SQISim8530 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8530) <- c("PlotCN","SQI","5CI","95CI")

simSQI2635 <- link(SQIMod_beta, data=SQI2635)
simmeanSQI2635 <- data.frame(apply(simSQI2635,2,mean))
PISQI2635 <- t(data.frame(apply(simSQI2635,2,PI,prob=0.89)))
SQISim2635 <- data.frame(cbind(simmeanSQI2635,PISQI2635))
SQISim2635$PlotCN <- SQI2635$PlotCN
SQISim2635 <- SQISim2635 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2635) <- c("PlotCN","SQI","5CI","95CI")
simSQI4535 <- link(SQIMod_beta, data=SQI4535)
simmeanSQI4535 <- data.frame(apply(simSQI4535,2,mean))
PISQI4535 <- t(data.frame(apply(simSQI4535,2,PI,prob=0.89)))
SQISim4535 <- data.frame(cbind(simmeanSQI4535,PISQI4535))
SQISim4535$PlotCN <- SQI4535$PlotCN
SQISim4535 <- SQISim4535 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4535) <- c("PlotCN","SQI","5CI","95CI")
simSQI8535 <- link(SQIMod_beta, data=SQI8535)
simmeanSQI8535 <- data.frame(apply(simSQI8535,2,mean))
PISQI8535 <- t(data.frame(apply(simSQI8535,2,PI,prob=0.89)))
SQISim8535 <- data.frame(cbind(simmeanSQI8535,PISQI8535))
SQISim8535$PlotCN <- SQI8535$PlotCN
SQISim8535 <- SQISim8535 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8535) <- c("PlotCN","SQI","5CI","95CI")

simSQI2640 <- link(SQIMod_beta, data=SQI2640)
simmeanSQI2640 <- data.frame(apply(simSQI2640,2,mean))
PISQI2640 <- t(data.frame(apply(simSQI2640,2,PI,prob=0.89)))
SQISim2640 <- data.frame(cbind(simmeanSQI2640,PISQI2640))
SQISim2640$PlotCN <- SQI2640$PlotCN
SQISim2640 <- SQISim2640 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2640) <- c("PlotCN","SQI","5CI","95CI")
simSQI4540 <- link(SQIMod_beta, data=SQI4540)
simmeanSQI4540 <- data.frame(apply(simSQI4540,2,mean))
PISQI4540 <- t(data.frame(apply(simSQI4540,2,PI,prob=0.89)))
SQISim4540 <- data.frame(cbind(simmeanSQI4540,PISQI4540))
SQISim4540$PlotCN <- SQI4540$PlotCN
SQISim4540 <- SQISim4540 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4540) <- c("PlotCN","SQI","5CI","95CI")
simSQI8540 <- link(SQIMod_beta, data=SQI8540)
simmeanSQI8540 <- data.frame(apply(simSQI8540,2,mean))
PISQI8540 <- t(data.frame(apply(simSQI8540,2,PI,prob=0.89)))
SQISim8540 <- data.frame(cbind(simmeanSQI8540,PISQI8540))
SQISim8540$PlotCN <- SQI8540$PlotCN
SQISim8540 <- SQISim8540 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8540) <- c("PlotCN","SQI","5CI","95CI")

simSQI2645 <- link(SQIMod_beta, data=SQI2645)
simmeanSQI2645 <- data.frame(apply(simSQI2645,2,mean))
PISQI2645 <- t(data.frame(apply(simSQI2645,2,PI,prob=0.89)))
SQISim2645 <- data.frame(cbind(simmeanSQI2645,PISQI2645))
SQISim2645$PlotCN <- SQI2645$PlotCN
SQISim2645 <- SQISim2645 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2645) <- c("PlotCN","SQI","5CI","95CI")
simSQI4545 <- link(SQIMod_beta, data=SQI4545)
simmeanSQI4545 <- data.frame(apply(simSQI4545,2,mean))
PISQI4545 <- t(data.frame(apply(simSQI4545,2,PI,prob=0.89)))
SQISim4545 <- data.frame(cbind(simmeanSQI4545,PISQI4545))
SQISim4545$PlotCN <- SQI4545$PlotCN
SQISim4545 <- SQISim4545 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4545) <- c("PlotCN","SQI","5CI","95CI")
simSQI8545 <- link(SQIMod_beta, data=SQI8545)
simmeanSQI8545 <- data.frame(apply(simSQI8545,2,mean))
PISQI8545 <- t(data.frame(apply(simSQI8545,2,PI,prob=0.89)))
SQISim8545 <- data.frame(cbind(simmeanSQI8545,PISQI8545))
SQISim8545$PlotCN <- SQI8545$PlotCN
SQISim8545 <- SQISim8545 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8545) <- c("PlotCN","SQI","5CI","95CI")

simSQI2650 <- link(SQIMod_beta, data=SQI2650)
simmeanSQI2650 <- data.frame(apply(simSQI2650,2,mean))
PISQI2650 <- t(data.frame(apply(simSQI2650,2,PI,prob=0.89)))
SQISim2650 <- data.frame(cbind(simmeanSQI2650,PISQI2650))
SQISim2650$PlotCN <- SQI2650$PlotCN
SQISim2650 <- SQISim2650 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2650) <- c("PlotCN","SQI","5CI","95CI")
simSQI4550 <- link(SQIMod_beta, data=SQI4550)
simmeanSQI4550 <- data.frame(apply(simSQI4550,2,mean))
PISQI4550 <- t(data.frame(apply(simSQI4550,2,PI,prob=0.89)))
SQISim4550 <- data.frame(cbind(simmeanSQI4550,PISQI4550))
SQISim4550$PlotCN <- SQI4550$PlotCN
SQISim4550 <- SQISim4550 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4550) <- c("PlotCN","SQI","5CI","95CI")
simSQI8550 <- link(SQIMod_beta, data=SQI8550)
simmeanSQI8550 <- data.frame(apply(simSQI8550,2,mean))
PISQI8550 <- t(data.frame(apply(simSQI8550,2,PI,prob=0.89)))
SQISim8550 <- data.frame(cbind(simmeanSQI8550,PISQI8550))
SQISim8550$PlotCN <- SQI8550$PlotCN
SQISim8550 <- SQISim8550 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8550) <- c("PlotCN","SQI","5CI","95CI")

simSQI2655 <- link(SQIMod_beta, data=SQI2655)
simmeanSQI2655 <- data.frame(apply(simSQI2655,2,mean))
PISQI2655 <- t(data.frame(apply(simSQI2655,2,PI,prob=0.89)))
SQISim2655 <- data.frame(cbind(simmeanSQI2655,PISQI2655))
SQISim2655$PlotCN <- SQI2655$PlotCN
SQISim2655 <- SQISim2655 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2655) <- c("PlotCN","SQI","5CI","95CI")
simSQI4555 <- link(SQIMod_beta, data=SQI4555)
simmeanSQI4555 <- data.frame(apply(simSQI4555,2,mean))
PISQI4555 <- t(data.frame(apply(simSQI4555,2,PI,prob=0.89)))
SQISim4555 <- data.frame(cbind(simmeanSQI4555,PISQI4555))
SQISim4555$PlotCN <- SQI4555$PlotCN
SQISim4555 <- SQISim4555 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4555) <- c("PlotCN","SQI","5CI","95CI")
simSQI8555 <- link(SQIMod_beta, data=SQI8555)
simmeanSQI8555 <- data.frame(apply(simSQI8555,2,mean))
PISQI8555 <- t(data.frame(apply(simSQI8555,2,PI,prob=0.89)))
SQISim8555 <- data.frame(cbind(simmeanSQI8555,PISQI8555))
SQISim8555$PlotCN <- SQI8555$PlotCN
SQISim8555 <- SQISim8555 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8555) <- c("PlotCN","SQI","5CI","95CI")

simSQI2660 <- link(SQIMod_beta, data=SQI2660)
simmeanSQI2660 <- data.frame(apply(simSQI2660,2,mean))
PISQI2660 <- t(data.frame(apply(simSQI2660,2,PI,prob=0.89)))
SQISim2660 <- data.frame(cbind(simmeanSQI2660,PISQI2660))
SQISim2660$PlotCN <- SQI2660$PlotCN
SQISim2660 <- SQISim2660 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2660) <- c("PlotCN","SQI","5CI","95CI")
simSQI4560 <- link(SQIMod_beta, data=SQI4560)
simmeanSQI4560 <- data.frame(apply(simSQI4560,2,mean))
PISQI4560 <- t(data.frame(apply(simSQI4560,2,PI,prob=0.89)))
SQISim4560 <- data.frame(cbind(simmeanSQI4560,PISQI4560))
SQISim4560$PlotCN <- SQI4560$PlotCN
SQISim4560 <- SQISim4560 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4560) <- c("PlotCN","SQI","5CI","95CI")
simSQI8560 <- link(SQIMod_beta, data=SQI8560)
simmeanSQI8560 <- data.frame(apply(simSQI8560,2,mean))
PISQI8560 <- t(data.frame(apply(simSQI8560,2,PI,prob=0.89)))
SQISim8560 <- data.frame(cbind(simmeanSQI8560,PISQI8560))
SQISim8560$PlotCN <- SQI8560$PlotCN
SQISim8560 <- SQISim8560 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8560) <- c("PlotCN","SQI","5CI","95CI")

simSQI2665 <- link(SQIMod_beta, data=SQI2665)
simmeanSQI2665 <- data.frame(apply(simSQI2665,2,mean))
PISQI2665 <- t(data.frame(apply(simSQI2665,2,PI,prob=0.89)))
SQISim2665 <- data.frame(cbind(simmeanSQI2665,PISQI2665))
SQISim2665$PlotCN <- SQI2665$PlotCN
SQISim2665 <- SQISim2665 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2665) <- c("PlotCN","SQI","5CI","95CI")
simSQI4565 <- link(SQIMod_beta, data=SQI4565)
simmeanSQI4565 <- data.frame(apply(simSQI4565,2,mean))
PISQI4565 <- t(data.frame(apply(simSQI4565,2,PI,prob=0.89)))
SQISim4565 <- data.frame(cbind(simmeanSQI4565,PISQI4565))
SQISim4565$PlotCN <- SQI4565$PlotCN
SQISim4565 <- SQISim4565 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4565) <- c("PlotCN","SQI","5CI","95CI")
simSQI8565 <- link(SQIMod_beta, data=SQI8565)
simmeanSQI8565 <- data.frame(apply(simSQI8565,2,mean))
PISQI8565 <- t(data.frame(apply(simSQI8565,2,PI,prob=0.89)))
SQISim8565 <- data.frame(cbind(simmeanSQI8565,PISQI8565))
SQISim8565$PlotCN <- SQI8565$PlotCN
SQISim8565 <- SQISim8565 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8565) <- c("PlotCN","SQI","5CI","95CI")

simSQI2670 <- link(SQIMod_beta, data=SQI2670)
simmeanSQI2670 <- data.frame(apply(simSQI2670,2,mean))
PISQI2670 <- t(data.frame(apply(simSQI2670,2,PI,prob=0.89)))
SQISim2670 <- data.frame(cbind(simmeanSQI2670,PISQI2670))
SQISim2670$PlotCN <- SQI2670$PlotCN
SQISim2670 <- SQISim2670 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2670) <- c("PlotCN","SQI","5CI","95CI")
simSQI4570 <- link(SQIMod_beta, data=SQI4570)
simmeanSQI4570 <- data.frame(apply(simSQI4570,2,mean))
PISQI4570 <- t(data.frame(apply(simSQI4570,2,PI,prob=0.89)))
SQISim4570 <- data.frame(cbind(simmeanSQI4570,PISQI4570))
SQISim4570$PlotCN <- SQI4570$PlotCN
SQISim4570 <- SQISim4570 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4570) <- c("PlotCN","SQI","5CI","95CI")
simSQI8570 <- link(SQIMod_beta, data=SQI8570)
simmeanSQI8570 <- data.frame(apply(simSQI8570,2,mean))
PISQI8570 <- t(data.frame(apply(simSQI8570,2,PI,prob=0.89)))
SQISim8570 <- data.frame(cbind(simmeanSQI8570,PISQI8570))
SQISim8570$PlotCN <- SQI8570$PlotCN
SQISim8570 <- SQISim8570 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8570) <- c("PlotCN","SQI","5CI","95CI")

simSQI2675 <- link(SQIMod_beta, data=SQI2675)
simmeanSQI2675 <- data.frame(apply(simSQI2675,2,mean))
PISQI2675 <- t(data.frame(apply(simSQI2675,2,PI,prob=0.89)))
SQISim2675 <- data.frame(cbind(simmeanSQI2675,PISQI2675))
SQISim2675$PlotCN <- SQI2675$PlotCN
SQISim2675 <- SQISim2675 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2675) <- c("PlotCN","SQI","5CI","95CI")
simSQI4575 <- link(SQIMod_beta, data=SQI4575)
simmeanSQI4575 <- data.frame(apply(simSQI4575,2,mean))
PISQI4575 <- t(data.frame(apply(simSQI4575,2,PI,prob=0.89)))
SQISim4575 <- data.frame(cbind(simmeanSQI4575,PISQI4575))
SQISim4575$PlotCN <- SQI4575$PlotCN
SQISim4575 <- SQISim4575 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4575) <- c("PlotCN","SQI","5CI","95CI")
simSQI8575 <- link(SQIMod_beta, data=SQI8575)
simmeanSQI8575 <- data.frame(apply(simSQI8575,2,mean))
PISQI8575 <- t(data.frame(apply(simSQI8575,2,PI,prob=0.89)))
SQISim8575 <- data.frame(cbind(simmeanSQI8575,PISQI8575))
SQISim8575$PlotCN <- SQI8575$PlotCN
SQISim8575 <- SQISim8575 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8575) <- c("PlotCN","SQI","5CI","95CI")

simSQI2680 <- link(SQIMod_beta, data=SQI2680)
simmeanSQI2680 <- data.frame(apply(simSQI2680,2,mean))
PISQI2680 <- t(data.frame(apply(simSQI2680,2,PI,prob=0.89)))
SQISim2680 <- data.frame(cbind(simmeanSQI2680,PISQI2680))
SQISim2680$PlotCN <- SQI2680$PlotCN
SQISim2680 <- SQISim2680 %>% dplyr::select(PlotCN, everything())
colnames(SQISim2680) <- c("PlotCN","SQI","5CI","95CI")
simSQI4580 <- link(SQIMod_beta, data=SQI4580)
simmeanSQI4580 <- data.frame(apply(simSQI4580,2,mean))
PISQI4580 <- t(data.frame(apply(simSQI4580,2,PI,prob=0.89)))
SQISim4580 <- data.frame(cbind(simmeanSQI4580,PISQI4580))
SQISim4580$PlotCN <- SQI4580$PlotCN
SQISim4580 <- SQISim4580 %>% dplyr::select(PlotCN, everything())
colnames(SQISim4580) <- c("PlotCN","SQI","5CI","95CI")
simSQI8580 <- link(SQIMod_beta, data=SQI8580)
simmeanSQI8580 <- data.frame(apply(simSQI8580,2,mean))
PISQI8580 <- t(data.frame(apply(simSQI8580,2,PI,prob=0.89)))
SQISim8580 <- data.frame(cbind(simmeanSQI8580,PISQI8580))
SQISim8580$PlotCN <- SQI8580$PlotCN
SQISim8580 <- SQISim8580 %>% dplyr::select(PlotCN, everything())
colnames(SQISim8580) <- c("PlotCN","SQI","5CI","95CI")


#create vector of new colnames
SQISimnames<-as.vector(sapply(seq(25, 80, by = 5), function(x) paste0(c("SQI", "5CI", "95CI"), x)))
SQISimnames<- append(SQISimnames,"PlotCN",after=0)

#group data by RCP pathway
SQISim26T <- SQISim2625 %>% cbind(SQISim2630[,2:4]) %>%
  cbind(SQISim2635[,2:4]) %>% cbind(SQISim2640[,2:4]) %>% cbind(SQISim2645[,2:4]) %>%
  cbind(SQISim2650[,2:4]) %>% cbind(SQISim2655[,2:4]) %>% cbind(SQISim2660[,2:4]) %>%
  cbind(SQISim2665[,2:4]) %>% cbind(SQISim2670[,2:4]) %>% cbind(SQISim2675[,2:4]) %>%
  cbind(SQISim2680[,2:4])
colnames(SQISim26T) <- SQISimnames
write.csv(SQISim26T,file="SQIPred26T.csv")

SQISim45T <- SQISim4525 %>% cbind(SQISim4530[,2:4]) %>%
  cbind(SQISim4535[,2:4]) %>% cbind(SQISim4540[,2:4]) %>% cbind(SQISim4545[,2:4]) %>%
  cbind(SQISim4550[,2:4]) %>% cbind(SQISim4555[,2:4]) %>% cbind(SQISim4560[,2:4]) %>%
  cbind(SQISim4565[,2:4]) %>% cbind(SQISim4570[,2:4]) %>% cbind(SQISim4575[,2:4]) %>%
  cbind(SQISim4580[,2:4])
colnames(SQISim45T) <- SQISimnames
write.csv(SQISim45T,file="SQIPred45T.csv")

SQISim85T <- SQISim8525 %>% cbind(SQISim8530[,2:4]) %>%
  cbind(SQISim8535[,2:4]) %>% cbind(SQISim8540[,2:4]) %>% cbind(SQISim8545[,2:4]) %>%
  cbind(SQISim8550[,2:4]) %>% cbind(SQISim8555[,2:4]) %>% cbind(SQISim8560[,2:4]) %>%
  cbind(SQISim8565[,2:4]) %>% cbind(SQISim8570[,2:4]) %>% cbind(SQISim8575[,2:4]) %>%
  cbind(SQISim8580[,2:4])
colnames(SQISim85T) <- SQISimnames
write.csv(SQISim85T,file="SQIPred85T.csv")

Hill24t <- read.csv("HillLink24.csv")
hill24look <- FgroupSplitTest[["24"]] %>% filter(ForestType%in%c(801,805))
Hill24t <- Hill24t %>% filter(Plot%in%hill24look$PlotCN)
plot(Hill24t$HillPred~Hill24t$Hill,xlab="Observed Hill Shannon",ylab="Predicted Hill Shannon",
     main="Predicted Hill Shannon vs Observed Hill Shannon, Maple,Beech,Birch,801+805 Only")
abline(lm(Hill24t$HillPred~Hill24t$Hill, data=Hill24t), col = "red")

Hill24t <- read.csv("HillLink24.csv")
hill24look <- FgroupSplitTest[["24"]] %>% filter(ForestType%in%c(802,809))
Hill24t <- Hill24t %>% filter(Plot%in%hill24look$PlotCN)
plot(Hill24t$HillPred~Hill24t$Hill,xlab="Observed Hill Shannon",ylab="Predicted Hill Shannon",
     main="Predicted Hill Shannon vs Observed Hill Shannon, Maple,Beech,Birch,802+809 Only")
abline(lm(Hill24t$HillPred~Hill24t$Hill, data=Hill24t), col = "red")

