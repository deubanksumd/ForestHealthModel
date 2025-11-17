#Model Predictions for Test data set. Run After Model Code.

# model comparisons, starting with carbon models
#create testing data list
CSplit1 <- list(
  Plot = FgroupSplitTest[["1"]]$PlotCN,
  CarbSum = scale(as.numeric(FgroupSplitTest[["1"]]$CarbonPerAcre) ,center=centerCarb1,scale=scaleCarb1),
  PPT = scale(as.numeric(FgroupSplitTest[["1"]]$PPT),center=centerPPT1,scale=scalePPT1),
  MAT = scale(as.numeric(FgroupSplitTest[["1"]]$MAT),center=centerMAT1,scale=scaleMAT1),
  FAD = as.integer(FgroupSplitTest[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["1"]]$Elevation, center=centerElevation1,scale=scaleElevation1),
  RHUM = scale(FgroupSplitTest[["1"]]$RHUM,center=centerRHUM1,scale=scaleRHUM1),
  RAD = scale(FgroupSplitTest[["1"]]$RAD, center=centerRAD1,scale=scaleRAD1)
)
#make data frame
CSplit1 <- as.data.frame(CSplit1)
#run link, columns 3-10 are the necessary data for the model
CarbFgroup1 <- link(CarbSplit1, data=CSplit1[,3:10],n=10000)
linkmeanCarbF1 <- data.frame(apply(CarbFgroup1,2,mean))
PICarbF1 <- t(data.frame(apply(CarbFgroup1,2,PI,prob=0.89)))
CarbLinkF1 <- data.frame(cbind(linkmeanCarbF1,PICarbF1))
CarbLinkF1$Plot <- CSplit1$Plot
CarbLinkF1 <- CarbLinkF1 %>% dplyr::select(Plot, everything())
colnames(CarbLinkF1) <- c("Plot","CarbPerAcre","5CI","95CI")
#add true values, then calculate RMSE
CarbLinkF1 <- merge(CarbLinkF1,CSplit1[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> carbsum , Carbperacre
rmse(CarbLinkF1$CarbSum,CarbLinkF1$CarbPerAcre) #RMSE= 1.049884
par(mar=c(5,6,4,2)+.1)
plot(CarbLinkF1$CarbSum,CarbLinkF1$CarbPerAcre, xlab="Observed Carbon Storage Values", ylab="Predicted Carbon Storage Values",
     main="Carbon Storage Out Of Sample Predictions White/Red/Jack Pine Group", cex.main=2.0,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(CarbLinkF1$`5CI`),max(CarbLinkF1$`95CI`)))
abline(lm(CarbLinkF1$CarbPerAcre~CarbLinkF1$CarbSum, data=CarbLinkF1), col = "red")
for ( i in 1:nrow(CarbLinkF1) ) lines( rep(CarbLinkF1$CarbSum[i],2) , CarbLinkF1[i,3:4] , col=rangi2 )
summary(lm(CarbLinkF1$CarbPerAcre~CarbLinkF1$CarbSum))$adj.r.squared #r square= 0.09320536
#repeat for other forest groups
#Fgroup 5
CSplit5 <- list(
  Plot = FgroupSplitTest[["5"]]$PlotCN,
  CarbSum = scale(as.numeric(FgroupSplitTest[["5"]]$CarbonPerAcre),center=centerCarb5,scale=scaleCarb5),
  PPT = scale(as.numeric(FgroupSplitTest[["5"]]$PPT),center=centerPPT5,scale=scalePPT5),
  MAT = scale(as.numeric(FgroupSplitTest[["5"]]$MAT),center=centerMAT5,scale=scaleMAT5),
  FAD = as.integer(FgroupSplitTest[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["5"]]$Elevation ,center=centerElevation5,scale=scaleElevation5),
  RHUM = scale(FgroupSplitTest[["5"]]$RHUM ,center=centerRHUM5,scale=scaleRHUM5),
  RAD = scale(FgroupSplitTest[["5"]]$RAD ,center=centerRAD5,scale=scaleRAD5)
)
#make data frame
CSplit5 <- as.data.frame(CSplit5)
#run link, columns 3-10 are the necessary data for the model
CarbFgroup5 <- link(CarbSplit5, data=CSplit5[,3:10],n=10000)
linkmeanCarbF5 <- data.frame(apply(CarbFgroup5,2,mean))
PICarbF5 <- t(data.frame(apply(CarbFgroup5,2,PI,prob=0.89)))
CarbLinkF5 <- data.frame(cbind(linkmeanCarbF5,PICarbF5))
CarbLinkF5$Plot <- CSplit5$Plot
CarbLinkF5 <- CarbLinkF5 %>% dplyr::select(Plot, everything())
colnames(CarbLinkF5) <- c("Plot","CarbPerAcre","5CI","95CI")
#add true values, then calculate RMSE
CarbLinkF5 <- merge(CarbLinkF5,CSplit5[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> carbsum , Carbperacre
rmse(CarbLinkF5$CarbSum,CarbLinkF5$CarbPerAcre) #RMSE= 1.137522
par(mar=c(5,6,4,4)+.1)
plot(CarbLinkF5$CarbSum,CarbLinkF5$CarbPerAcre, xlab="Observed Carbon Storage Values", ylab="Predicted Carbon Storage Values",
     main="Carbon Storage Out Of Sample Predictions Loblolly/Short Leaf Pine Group", cex.main=2.0,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(CarbLinkF5$`5CI`),max(CarbLinkF5$`95CI`)))
abline(lm(CarbLinkF5$CarbPerAcre~CarbLinkF5$CarbSum, data=CarbLinkF5), col = "red")
for ( i in 1:nrow(CarbLinkF5) ) lines( rep(CarbLinkF5$CarbSum[i],2) , CarbLinkF5[i,3:4] , col=rangi2 )
summary(lm(CarbLinkF5$CarbPerAcre~CarbLinkF5$CarbSum))$adj.r.squared #r square= 0.02972859
#f group 20
CSplit20 <- list(
  Plot = FgroupSplitTest[["20"]]$PlotCN,
  CarbSum = scale(as.numeric(FgroupSplitTest[["20"]]$CarbonPerAcre),center=centerCarb20,scale=scaleCarb20),
  PPT = scale(as.numeric(FgroupSplitTest[["20"]]$PPT),center=centerPPT20,scale=scalePPT20),
  MAT = scale(as.numeric(FgroupSplitTest[["20"]]$MAT),center=centerMAT20,scale=scaleMAT20),
  FAD = as.integer(FgroupSplitTest[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["20"]]$Elevation ,center=centerElevation20,scale=scaleElevation20),
  RHUM = scale(FgroupSplitTest[["20"]]$RHUM ,center=centerRHUM20,scale=scaleRHUM20),
  RAD = scale(FgroupSplitTest[["20"]]$RAD ,center=centerRAD20,scale=scaleRAD20)
)
#make data frame
CSplit20 <- as.data.frame(CSplit20)
#run link, columns 3-10 are the necessary data for the model
CarbFgroup20 <- link(CarbSplit20, data=CSplit20[,3:10],n=10000)
linkmeanCarbF20 <- data.frame(apply(CarbFgroup20,2,mean))
PICarbF20 <- t(data.frame(apply(CarbFgroup20,2,PI,prob=0.89)))
CarbLinkF20 <- data.frame(cbind(linkmeanCarbF20,PICarbF20))
CarbLinkF20$Plot <- CSplit20$Plot
CarbLinkF20 <- CarbLinkF20 %>% dplyr::select(Plot, everything())
colnames(CarbLinkF20) <- c("Plot","CarbPerAcre","5CI","95CI")
#add true values, then calculate RMSE
CarbLinkF20 <- merge(CarbLinkF20,CSplit20[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> carbsum , Carbperacre
rmse(CarbLinkF20$CarbSum,CarbLinkF20$CarbPerAcre) #RMSE=0.9707544
plot(CarbLinkF20$CarbSum,CarbLinkF20$CarbPerAcre, xlab="Observed Carbon Storage Values", ylab="Predicted Carbon Storage Values",
     main="Carbon Storage Out Of Sample Predictions Oak/Pine Group", cex.main=2.0,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(CarbLinkF20$`5CI`),max(CarbLinkF20$`95CI`)))
abline(lm(CarbLinkF20$CarbPerAcre~CarbLinkF20$CarbSum, data=CarbLinkF20), col = "red")
for ( i in 1:nrow(CarbLinkF20) ) lines( rep(CarbLinkF20$CarbSum[i],2) , CarbLinkF20[i,3:4] , col=rangi2 )
summary(lm(CarbLinkF20$CarbPerAcre~CarbLinkF20$CarbSum))$adj.r.squared #r square= 0.006221115

#fgroup 21 -
CSplit21 <- list(
  Plot = FgroupSplitTest[["21"]]$PlotCN,
  CarbSum = scale(as.numeric(FgroupSplitTest[["21"]]$CarbonPerAcre) ,center=centerCarb21,scale=scaleCarb21),
  PPT = scale(as.numeric(FgroupSplitTest[["21"]]$PPT),center=centerPPT21,scale=scalePPT21),
  MAT = scale(as.numeric(FgroupSplitTest[["21"]]$MAT),center=centerMAT21,scale=scaleMAT21),
  FAD = as.integer(FgroupSplitTest[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["21"]]$Elevation, center=centerElevation21,scale=scaleElevation21),
  RHUM = scale(FgroupSplitTest[["21"]]$RHUM,center=centerRHUM21,scale=scaleRHUM21),
  RAD = scale(FgroupSplitTest[["21"]]$RAD, center=centerRAD21,scale=scaleRAD21)
)
#make data frame
CSplit21 <- as.data.frame(CSplit21)
#run link, columns 3-10 are the necessary data for the model
CarbFgroup21 <- link(CarbSplit21, data=CSplit21[,3:10],n=10000)
linkmeanCarbF21 <- data.frame(apply(CarbFgroup21,2,mean))
PICarbF21 <- t(data.frame(apply(CarbFgroup21,2,PI,prob=0.89)))
CarbLinkF21 <- data.frame(cbind(linkmeanCarbF21,PICarbF21))
CarbLinkF21$Plot <- CSplit21$Plot
CarbLinkF21 <- CarbLinkF21 %>% dplyr::select(Plot, everything())
colnames(CarbLinkF21) <- c("Plot","CarbPerAcre","5CI","95CI")
#add true values, then calculate RMSE
CarbLinkF21 <- merge(CarbLinkF21,CSplit21[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> carbsum , Carbperacre
rmse(CarbLinkF21$CarbSum,CarbLinkF21$CarbPerAcre) #RMSE= 0.9109925
plot(CarbLinkF21$CarbSum,CarbLinkF21$CarbPerAcre, xlab="Observed Carbon Storage Values", ylab="Predicted Carbon Storage Values",
     main="Carbon Storage Out Of Sample Predictions Oak/Hickory Group", cex.main=2.0,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(CarbLinkF21$`5CI`),max(CarbLinkF21$`95CI`)))
abline(lm(CarbLinkF21$CarbPerAcre~CarbLinkF21$CarbSum, data=CarbLinkF21), col = "red")
for ( i in 1:nrow(CarbLinkF21) ) lines( rep(CarbLinkF21$CarbSum[i],2) , CarbLinkF21[i,3:4] , col=rangi2 )
summary(lm(CarbLinkF21$CarbPerAcre~CarbLinkF21$CarbSum))$adj.r.squared #r square= 0.1503157

#Fgroup 23
CSplit23 <- list(
  Plot = FgroupSplitTest[["23"]]$PlotCN,
  CarbSum = scale(as.numeric(FgroupSplitTest[["23"]]$CarbonPerAcre),center=centerCarb23,scale=scaleCarb23),
  PPT = scale(as.numeric(FgroupSplitTest[["23"]]$PPT),center=centerPPT23,scale=scalePPT23),
  MAT = scale(as.numeric(FgroupSplitTest[["23"]]$MAT),center=centerMAT23,scale=scaleMAT23),
  FAD = as.integer(FgroupSplitTest[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["23"]]$Elevation ,center=centerElevation23,scale=scaleElevation23),
  RHUM = scale(FgroupSplitTest[["23"]]$RHUM ,center=centerRHUM23,scale=scaleRHUM23),
  RAD = scale(FgroupSplitTest[["23"]]$RAD ,center=centerRAD23,scale=scaleRAD23)
)
#make data frame
CSplit23 <- as.data.frame(CSplit23)
#run link, columns 3-10 are the necessary data for the model
CarbFgroup23 <- link(CarbSplit23, data=CSplit23[,3:10],n=10000)
linkmeanCarbF23 <- data.frame(apply(CarbFgroup23,2,mean))
PICarbF23 <- t(data.frame(apply(CarbFgroup23,2,PI,prob=0.89)))
CarbLinkF23 <- data.frame(cbind(linkmeanCarbF23,PICarbF23))
CarbLinkF23$Plot <- CSplit23$Plot
CarbLinkF23 <- CarbLinkF23 %>% dplyr::select(Plot, everything())
colnames(CarbLinkF23) <- c("Plot","CarbPerAcre","5CI","95CI")
#add true values, then calculate RMSE
CarbLinkF23 <- merge(CarbLinkF23,CSplit23[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> carbsum , Carbperacre
rmse(CarbLinkF23$CarbSum,CarbLinkF23$CarbPerAcre) #RMSE=0.9481697
par(mar=c(5,6,4,4)+.1)
plot(CarbLinkF23$CarbSum,CarbLinkF23$CarbPerAcre, xlab="Observed Carbon Storage Values", ylab="Predicted Carbon Storage Values",
     main="Carbon Storage Out Of Sample Predictions Elm/Ash/Cottonwood Group", cex.main=2.0,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(CarbLinkF23$`5CI`),max(CarbLinkF23$`95CI`)))
abline(lm(CarbLinkF23$CarbPerAcre~CarbLinkF23$CarbSum, data=CarbLinkF23), col = "red")
for ( i in 1:nrow(CarbLinkF23) ) lines( rep(CarbLinkF23$CarbSum[i],2) , CarbLinkF23[i,3:4] , col=rangi2 )
summary(lm(CarbLinkF23$CarbPerAcre~CarbLinkF23$CarbSum))$adj.r.squared #r square= 0.1808458
#f group 24
CSplit24 <- list(
  Plot = FgroupSplitTest[["24"]]$PlotCN,
  CarbSum = scale(as.numeric(FgroupSplitTest[["24"]]$CarbonPerAcre),center=centerCarb24,scale=scaleCarb24),
  PPT = scale(as.numeric(FgroupSplitTest[["24"]]$PPT),center=centerPPT24,scale=scalePPT24),
  MAT = scale(as.numeric(FgroupSplitTest[["24"]]$MAT),center=centerMAT24,scale=scaleMAT24),
  FAD = as.integer(FgroupSplitTest[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["24"]]$Elevation ,center=centerElevation24,scale=scaleElevation24),
  RHUM = scale(FgroupSplitTest[["24"]]$RHUM ,center=centerRHUM24,scale=scaleRHUM24),
  RAD = scale(FgroupSplitTest[["24"]]$RAD ,center=centerRAD24,scale=scaleRAD24)
)
#make data frame
CSplit24 <- as.data.frame(CSplit24)
#run link, columns 3-10 are the necessary data for the model
CarbFgroup24 <- link(CarbSplit24, data=CSplit24[,3:10],n=10000)
linkmeanCarbF24 <- data.frame(apply(CarbFgroup24,2,mean))
PICarbF24 <- t(data.frame(apply(CarbFgroup24,2,PI,prob=0.89)))
CarbLinkF24 <- data.frame(cbind(linkmeanCarbF24,PICarbF24))
CarbLinkF24$Plot <- CSplit24$Plot
CarbLinkF24 <- CarbLinkF24 %>% dplyr::select(Plot, everything())
colnames(CarbLinkF24) <- c("Plot","CarbPerAcre","5CI","95CI")
#add true values, then calculate RMSE
CarbLinkF24 <- merge(CarbLinkF24,CSplit24[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> carbsum , Carbperacre
rmse(CarbLinkF24$CarbSum,CarbLinkF24$CarbPerAcre) #RMSE= 0.8614644
plot(CarbLinkF24$CarbSum,CarbLinkF24$CarbPerAcre, xlab="Observed Carbon Storage Values", ylab="Predicted Carbon Storage Values",
     main="Carbon Storage Out Of Sample Predictions Maple/Beech/Birch Group", cex.main=2.0,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(CarbLinkF24$`5CI`),max(CarbLinkF24$`95CI`)))
abline(lm(CarbLinkF24$CarbPerAcre~CarbLinkF24$CarbSum, data=CarbLinkF24), col = "red")
for ( i in 1:nrow(CarbLinkF24) ) lines( rep(CarbLinkF24$CarbSum[i],2) , CarbLinkF24[i,3:4] , col=rangi2 )
summary(lm(CarbLinkF24$CarbPerAcre~CarbLinkF24$CarbSum))$adj.r.squared #r square= 0.2806764

#Hill Shannon Models
HSplit1 <- list(
  Plot = FgroupSplitTest[["1"]]$PlotCN,
  Hill = as.numeric(FgroupSplitTest[["1"]]$HillShannonIndex),
  PPT = scale(as.numeric(FgroupSplitTest[["1"]]$PPT),center=centerPPT1,scale=scalePPT1),
  MAT = scale(as.numeric(FgroupSplitTest[["1"]]$MAT),center=centerMAT1,scale=scaleMAT1),
  FAD = as.integer(FgroupSplitTest[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["1"]]$Elevation ,center=centerElevation1,scale=scaleElevation1),
  RHUM = scale(FgroupSplitTest[["1"]]$RHUM ,center=centerRHUM1,scale=scaleRHUM1),
  RAD = scale(FgroupSplitTest[["1"]]$RAD ,center=centerRAD1,scale=scaleRAD1)
)
#make data frame
HSplit1 <- as.data.frame(HSplit1)
#run link, columns 3-10 are the necessary data for the model
HillFgroup1 <- link(HillSplit1_G, data=HSplit1[,3:10],n=10000)
linkmeanHillF1 <- data.frame(apply(HillFgroup1,2,mean))
PIHillF1 <- t(data.frame(apply(HillFgroup1,2,PI,prob=0.89)))
HillLinkF1 <- data.frame(cbind(linkmeanHillF1,PIHillF1))
HillLinkF1$Plot <- HSplit1$Plot
HillLinkF1 <- HillLinkF1 %>% dplyr::select(Plot, everything())
colnames(HillLinkF1) <- c("Plot","HillPred","5CI","95CI")
#add true values, then calculate RMSE
HillLinkF1 <- merge(HillLinkF1,HSplit1[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> hill , HillPred
rmse(HillLinkF1$Hill,HillLinkF1$HillPred) #RMSE= 1.583202
plot(HillLinkF1$Hill,HillLinkF1$HillPred, xlab="Observed Hill Shannon’s Index Values", ylab="Predicted Hill Shannon’s Index Values",
     main="Hill Shannon's Index Out Of Sample Predictions White/Red/Jack Pine Group", cex.main=2.0,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(HillLinkF1$`5CI`),max(HillLinkF1$`95CI`)))
abline(lm(HillLinkF1$HillPred~HillLinkF1$Hill, data=HillLinkF1), col = "red")
for ( i in 1:nrow(HillLinkF1) ) lines( rep(HillLinkF1$Hill[i],2) , HillLinkF1[i,3:4] , col=rangi2 )
summary(lm(HillLinkF1$HillPred~HillLinkF1$Hill))$adj.r.squared #r square= 0.06543856

#Fgroup 5, 
HSplit5 <- list(
  Plot = FgroupSplitTest[["5"]]$PlotCN,
  Hill = as.numeric(FgroupSplitTest[["5"]]$HillShannonIndex),
  PPT = scale(as.numeric(FgroupSplitTest[["5"]]$PPT) ,center=centerPPT5,scale=scalePPT5),
  MAT = scale(as.numeric(FgroupSplitTest[["5"]]$MAT) ,center=centerMAT5,scale=scaleMAT5),
  FAD = as.integer(FgroupSplitTest[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["5"]]$Elevation),
  RHUM = scale(FgroupSplitTest[["5"]]$RHUM ,center=centerRHUM5,scale=scaleRHUM5),
  RAD = scale(FgroupSplitTest[["5"]]$RAD ,center=centerRAD5,scale=scaleRAD5)
)
#make data frame
HSplit5 <- as.data.frame(HSplit5)
#run link, columns 3-10 are the necessary data for the model
HillFgroup5 <- link(HillSplit5_G, data=HSplit5[,3:10],n=10000)
#one strange outlier at position 9. basically 0. Highest elevation, other variables normal
HillFgroup5 <- HillFgroup5[,-9]
HSplit5 <- HSplit5[-9,]
linkmeanHillF5 <- data.frame(apply(HillFgroup5,2,mean))
PIHillF5 <- t(data.frame(apply(HillFgroup5,2,PI,prob=0.89)))
HillLinkF5 <- data.frame(cbind(linkmeanHillF5,PIHillF5))
HillLinkF5$Plot <- HSplit5$Plot
HillLinkF5 <- HillLinkF5 %>% dplyr::select(Plot, everything())
colnames(HillLinkF5) <- c("Plot","HillPred","5CI","95CI")
#add true values, then calculate RMSE
HillLinkF5 <- merge(HillLinkF5,HSplit5[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> hill , HillPred
rmse(HillLinkF5$Hill,HillLinkF5$HillPred) #RMSE=1.484964
par(mar=c(5,6,4,5)+.1)
plot(HillLinkF5$Hill,HillLinkF5$HillPred, xlab="Observed Hill Shannon’s Index Values", ylab="Predicted Hill Shannon’s Index Values",
     main="Hill Shannon's Index Out Of Sample Predictions Loblolly/Short Leaf Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(HillLinkF5$`5CI`),max(HillLinkF5$`95CI`)))
abline(lm(HillLinkF5$HillPred~HillLinkF5$Hill, data=HillLinkF5), col = "red")
for ( i in 1:nrow(HillLinkF5) ) lines( rep(HillLinkF5$Hill[i],2) , HillLinkF5[i,3:4] , col=rangi2 )
summary(lm(HillLinkF5$HillPred~HillLinkF5$Hill))$adj.r.squared #r square= 0.1495564

HSplit20 <- list(
  Plot = FgroupSplitTest[["20"]]$PlotCN,
  Hill = as.numeric(FgroupSplitTest[["20"]]$HillShannonIndex),
  PPT = scale(as.numeric(FgroupSplitTest[["20"]]$PPT) ,center=centerPPT20,scale=scalePPT20),
  MAT = scale(as.numeric(FgroupSplitTest[["20"]]$MAT) ,center=centerMAT20,scale=scaleMAT20),
  FAD = as.integer(FgroupSplitTest[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["20"]]$Elevation ,center=centerElevation20,scale=scaleElevation20),
  RHUM = scale(FgroupSplitTest[["20"]]$RHUM ,center=centerRHUM20,scale=scaleRHUM20),
  RAD = scale(FgroupSplitTest[["20"]]$RAD ,center=centerRAD20,scale=scaleRAD20)
)
#make data frame
HSplit20 <- as.data.frame(HSplit20)
#few Group have NA
HSplit20 <- na.omit(HSplit20)
#run link, columns 3-10 are the necessary data for the model
HillFgroup20 <- link(HillSplit20_G, data=HSplit20[,3:10],n=10000)
linkmeanHillF20 <- data.frame(apply(HillFgroup20,2,mean))
PIHillF20 <- t(data.frame(apply(HillFgroup20,2,PI,prob=0.89)))
HillLinkF20 <- data.frame(cbind(linkmeanHillF20,PIHillF20))
HillLinkF20$Plot <- HSplit20$Plot
HillLinkF20 <- HillLinkF20 %>% dplyr::select(Plot, everything())
colnames(HillLinkF20) <- c("Plot","HillPred","5CI","95CI")
#add true values, then calculate RMSE
HillLinkF20 <- merge(HillLinkF20,HSplit20[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> hill , HillPred
rmse(HillLinkF20$Hill,HillLinkF20$HillPred) #RMSE= 1.448379
plot(HillLinkF20$Hill,HillLinkF20$HillPred, xlab="Observed Hill Shannon’s Index Values", ylab="Predicted Hill Shannon’s Index Values",
     main="Hill Shannon's Index Out Of Sample Predictions Oak/Pine Group", cex.main=2.0,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(HillLinkF20$`5CI`),max(HillLinkF20$`95CI`)))
abline(lm(HillLinkF20$HillPred~HillLinkF20$Hill, data=HillLinkF20), col = "red")
for ( i in 1:nrow(HillLinkF20) ) lines( rep(HillLinkF20$Hill[i],2) , HillLinkF20[i,3:4] , col=rangi2 )
summary(lm(HillLinkF20$HillPred~HillLinkF20$Hill))$adj.r.squared #r square= 0.0127793

HSplit21 <- list(
  Plot = FgroupSplitTest[["21"]]$PlotCN,
  Hill = as.numeric(FgroupSplitTest[["21"]]$HillShannonIndex),
  PPT = scale(as.numeric(FgroupSplitTest[["21"]]$PPT),center=centerPPT21,scale=scalePPT21),
  MAT = scale(as.numeric(FgroupSplitTest[["21"]]$MAT),center=centerMAT21,scale=scaleMAT21),
  FAD = as.integer(FgroupSplitTest[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["21"]]$Elevation ,center=centerElevation21,scale=scaleElevation21),
  RHUM = scale(FgroupSplitTest[["21"]]$RHUM ,center=centerRHUM21,scale=scaleRHUM21),
  RAD = scale(FgroupSplitTest[["21"]]$RAD ,center=centerRAD21,scale=scaleRAD21)
)
#make data frame
HSplit21 <- as.data.frame(HSplit21)
#run link, columns 3-10 are the necessary data for the model
HillFgroup21 <- link(HillSplit21_G, data=HSplit21[,3:10],n=10000)
linkmeanHillF21 <- data.frame(apply(HillFgroup21,2,mean))
PIHillF21 <- t(data.frame(apply(HillFgroup21,2,PI,prob=0.89)))
HillLinkF21 <- data.frame(cbind(linkmeanHillF21,PIHillF21))
HillLinkF21$Plot <- HSplit21$Plot
HillLinkF21 <- HillLinkF21 %>% dplyr::select(Plot, everything())
colnames(HillLinkF21) <- c("Plot","HillPred","5CI","95CI")
#add true values, then calculate RMSE
HillLinkF21 <- merge(HillLinkF21,HSplit21[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> hill , HillPred
rmse(HillLinkF21$Hill,HillLinkF21$HillPred) #RMSE= 1.823019
plot(HillLinkF21$Hill,HillLinkF21$HillPred, xlab="Observed Hill Shannon’s Index Values", ylab="Predicted Hill Shannon’s Index Values",
     main="Hill Shannon's Index Out Of Sample Predictions Oak/Hickory Group", cex.main=2.0,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(HillLinkF21$`5CI`),max(HillLinkF21$`95CI`)))
abline(lm(HillLinkF21$HillPred~HillLinkF21$Hill, data=HillLinkF21), col = "red")
for ( i in 1:nrow(HillLinkF21) ) lines( rep(HillLinkF21$Hill[i],2) , HillLinkF21[i,3:4] , col=rangi2 )
summary(lm(HillLinkF21$HillPred~HillLinkF21$Hill))$adj.r.squared #r square= 0.1189551

#Fgroup 23
HSplit23 <- list(
  Plot = FgroupSplitTest[["23"]]$PlotCN,
  Hill = as.numeric(FgroupSplitTest[["23"]]$HillShannonIndex),
  PPT = scale(as.numeric(FgroupSplitTest[["23"]]$PPT) ,center=centerPPT23,scale=scalePPT23),
  MAT = scale(as.numeric(FgroupSplitTest[["23"]]$MAT) ,center=centerMAT23,scale=scaleMAT23),
  FAD = as.integer(FgroupSplitTest[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["23"]]$Elevation),
  RHUM = scale(FgroupSplitTest[["23"]]$RHUM ,center=centerRHUM23,scale=scaleRHUM23),
  RAD = scale(FgroupSplitTest[["23"]]$RAD ,center=centerRAD23,scale=scaleRAD23)
)
#make data frame
HSplit23 <- as.data.frame(HSplit23)
#run link, columns 3-10 are the necessary data for the model
HillFgroup23 <- link(HillSplit23_G, data=HSplit23[,3:10],n=10000)
linkmeanHillF23 <- data.frame(apply(HillFgroup23,2,mean))
PIHillF23 <- t(data.frame(apply(HillFgroup23,2,PI,prob=0.89)))
HillLinkF23 <- data.frame(cbind(linkmeanHillF23,PIHillF23))
HillLinkF23$Plot <- HSplit23$Plot
HillLinkF23 <- HillLinkF23 %>% dplyr::select(Plot, everything())
colnames(HillLinkF23) <- c("Plot","HillPred","5CI","95CI")
#add true values, then calculate RMSE
HillLinkF23 <- merge(HillLinkF23,HSplit23[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> hill , HillPred
rmse(HillLinkF23$Hill,HillLinkF23$HillPred) #RMSE= 1.594992
par(mar=c(5,6,4,5)+.1)
plot(HillLinkF23$Hill,HillLinkF23$HillPred, xlab="Observed Hill Shannon’s Index Values", ylab="Predicted Hill Shannon’s Index Values",
     main="Hill Shannon's Index Out Of Sample Predictions Elm/Ash/Cottonwood Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(HillLinkF23$`5CI`),max(HillLinkF23$`95CI`)))
abline(lm(HillLinkF23$HillPred~HillLinkF23$Hill, data=HillLinkF23), col = "red")
for ( i in 1:nrow(HillLinkF23) ) lines( rep(HillLinkF23$Hill[i],2) , HillLinkF23[i,3:4] , col=rangi2 )
summary(lm(HillLinkF23$HillPred~HillLinkF23$Hill))$adj.r.squared #r square= 0.003494036

HSplit24 <- list(
  Plot = FgroupSplitTest[["24"]]$PlotCN,
  Hill = as.numeric(FgroupSplitTest[["24"]]$HillShannonIndex),
  PPT = scale(as.numeric(FgroupSplitTest[["24"]]$PPT) ,center=centerPPT24,scale=scalePPT24),
  MAT = scale(as.numeric(FgroupSplitTest[["24"]]$MAT) ,center=centerMAT24,scale=scaleMAT24),
  FAD = as.integer(FgroupSplitTest[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["24"]]$Elevation ,center=centerElevation24,scale=scaleElevation24),
  RHUM = scale(FgroupSplitTest[["24"]]$RHUM ,center=centerRHUM24,scale=scaleRHUM24),
  RAD = scale(FgroupSplitTest[["24"]]$RAD ,center=centerRAD24,scale=scaleRAD24)
)
#make data frame
HSplit24 <- as.data.frame(HSplit24)
#few Group have NA
HSplit24 <- na.omit(HSplit24)
#run link, columns 3-10 are the necessary data for the model
HillFgroup24 <- link(HillSplit24_G, data=HSplit24[,3:10],n=10000)
linkmeanHillF24 <- data.frame(apply(HillFgroup24,2,mean))
PIHillF24 <- t(data.frame(apply(HillFgroup24,2,PI,prob=0.89)))
HillLinkF24 <- data.frame(cbind(linkmeanHillF24,PIHillF24))
HillLinkF24$Plot <- HSplit24$Plot
HillLinkF24 <- HillLinkF24 %>% dplyr::select(Plot, everything())
colnames(HillLinkF24) <- c("Plot","HillPred","5CI","95CI")
#add true values, then calculate RMSE
HillLinkF24 <- merge(HillLinkF24,HSplit24[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> hill , HillPred
rmse(HillLinkF24$Hill,HillLinkF24$HillPred) #RMSE= 1.417766
plot(HillLinkF24$Hill,HillLinkF24$HillPred, xlab="Observed Hill Shannon’s Index Values", ylab="Predicted Hill Shannon’s Index Values",
     main="Hill Shannon's Index Out Of Sample Predictions Maple/Beech/Birch Group", cex.main=2.0,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(HillLinkF24$`5CI`),max(HillLinkF24$`95CI`)))
abline(lm(HillLinkF24$HillPred~HillLinkF24$Hill, data=HillLinkF24), col = "red")
for ( i in 1:nrow(HillLinkF24) ) lines( rep(HillLinkF24$Hill[i],2) , HillLinkF24[i,3:4] , col=rangi2 )
summary(lm(HillLinkF24$HillPred~HillLinkF24$Hill))$adj.r.squared #r square= 0.1174453

#Jaccards Model
Jsplit1 <- list(
  Plot = FgroupSplitTest[["1"]]$PlotCN,
  Jacc = as.numeric(FgroupSplitTest[["1"]]$JaccardMean),
  PPT = scale(as.numeric(FgroupSplitTest[["1"]]$PPT) ,center=centerPPT1,scale=scalePPT1),
  MAT = scale(as.numeric(FgroupSplitTest[["1"]]$MAT) ,center=centerMAT1,scale=scaleMAT1),
  FAD = as.integer(FgroupSplitTest[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["1"]]$Elevation ,center=centerElevation1,scale=scaleElevation1),
  RHUM = scale(FgroupSplitTest[["1"]]$RHUM ,center=centerRHUM1,scale=scaleRHUM1),
  RAD = scale(FgroupSplitTest[["1"]]$RAD ,center=centerRAD1,scale=scaleRAD1)
)
#make data frame
Jsplit1 <- as.data.frame(Jsplit1)
#run link, columns 3-11 are the necessary data for the model
JaccFgroup1 <- link(JaccSplit1_beta, data=Jsplit1[,3:10],n=10000)
linkmeanJaccF1 <- data.frame(apply(JaccFgroup1,2,mean))
PIJaccF1 <- t(data.frame(apply(JaccFgroup1,2,PI,prob=0.89)))
JaccLinkF1 <- data.frame(cbind(linkmeanJaccF1,PIJaccF1))
JaccLinkF1$Plot <- Jsplit1$Plot
JaccLinkF1 <- JaccLinkF1 %>% dplyr::select(Plot, everything())
colnames(JaccLinkF1) <- c("Plot","JaccPred","5CI","95CI")
#add true values, then calculate RMSE
JaccLinkF1 <- merge(JaccLinkF1,Jsplit1[,1:2],by="Plot",all.x=TRUE)
rmse(JaccLinkF1$Jacc,JaccLinkF1$JaccPred) #RMSE= 0.05225628
summary(lm(JaccLinkF1$JaccPred~JaccLinkF1$Jacc))$adj.r.squared #r-squared =0.001098336
par(mar=c(6,6,6,2)+.1)
plot(JaccLinkF1$Jacc,JaccLinkF1$JaccPred, xlab="Observed Jaccards Similarity Index", ylab="Predicted Jaccards Similarity Index",
     main="Jaccards Similarity Index Out of Sample Predictions White/Red/Jack Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(JaccLinkF1$`5CI`),max(JaccLinkF1$`95CI`)))
abline(lm(JaccLinkF1$JaccPred~JaccLinkF1$Jacc, data=JaccLinkF1), col = "red")
for ( i in 1:nrow(JaccLinkF1) ) lines( rep(JaccLinkF1$Jacc[i],2) , JaccLinkF1[i,3:4] , col=rangi2 )


#Fgroup 5
Jsplit5 <- list(
  Plot = FgroupSplitTest[["5"]]$PlotCN,
  Jacc = as.numeric(FgroupSplitTest[["5"]]$JaccardMean),
  PPT = scale(as.numeric(FgroupSplitTest[["5"]]$PPT) ,center=centerPPT5,scale=scalePPT5),
  MAT = scale(as.numeric(FgroupSplitTest[["5"]]$MAT) ,center=centerMAT5,scale=scaleMAT5),
  FAD = as.integer(FgroupSplitTest[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["5"]]$Elevation ,center=centerElevation5,scale=scaleElevation5),
  RHUM = scale(FgroupSplitTest[["5"]]$RHUM ,center=centerRHUM5,scale=scaleRHUM5),
  RAD = scale(FgroupSplitTest[["5"]]$RAD ,center=centerRAD5,scale=scaleRAD5)
)
#make data frame
Jsplit5 <- as.data.frame(Jsplit5)
#run link, columns 3-11 are the necessary data for the model
JaccFgroup5 <- link(JaccSplit5_beta, data=Jsplit5[,3:10],n=10000)
linkmeanJaccF5 <- data.frame(apply(JaccFgroup5,2,mean))
PIJaccF5 <- t(data.frame(apply(JaccFgroup5,2,PI,prob=0.89)))
JaccLinkF5 <- data.frame(cbind(linkmeanJaccF5,PIJaccF5))
JaccLinkF5$Plot <- Jsplit5$Plot
JaccLinkF5 <- JaccLinkF5 %>% dplyr::select(Plot, everything())
colnames(JaccLinkF5) <- c("Plot","JaccPred","5CI","95CI")
#add true values, then calculate RMSE
JaccLinkF5 <- merge(JaccLinkF5,Jsplit5[,1:2],by="Plot",all.x=TRUE)
rmse(JaccLinkF5$Jacc,JaccLinkF5$JaccPred) #RMSE=  0.04483476
summary(lm(JaccLinkF5$JaccPred~JaccLinkF5$Jacc))$adj.r.squared #rsquare =  0.001038339
plot(JaccLinkF5$Jacc,JaccLinkF5$JaccPred, xlab="Observed Jaccards Similarity Index", ylab="Predicted Jaccards Similarity Index",
     main="Jaccards Similarity Index Out of Sample Predictions Loblolly/Short Leaf Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(JaccLinkF5$`5CI`),max(JaccLinkF5$`95CI`)))
abline(lm(JaccLinkF5$JaccPred~JaccLinkF5$Jacc, data=JaccLinkF5), col = "red")
for ( i in 1:nrow(JaccLinkF5) ) lines( rep(JaccLinkF5$Jacc[i],2) , JaccLinkF5[i,3:4] , col=rangi2 )


Jsplit20 <- list(
  Plot = FgroupSplitTest[["20"]]$PlotCN,
  Jacc = as.numeric(FgroupSplitTest[["20"]]$JaccardMean),
  PPT = scale(as.numeric(FgroupSplitTest[["20"]]$PPT) ,center=centerPPT20,scale=scalePPT20),
  MAT = scale(as.numeric(FgroupSplitTest[["20"]]$MAT) ,center=centerMAT20,scale=scaleMAT20),
  FAD = as.integer(FgroupSplitTest[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["20"]]$Elevation ,center=centerElevation20,scale=scaleElevation20),
  RHUM = scale(FgroupSplitTest[["20"]]$RHUM ,center=centerRHUM20,scale=scaleRHUM20),
  RAD = scale(FgroupSplitTest[["20"]]$RAD ,center=centerRAD20,scale=scaleRAD20)
)
#make data frame
Jsplit20 <- as.data.frame(Jsplit20)
#run link, columns 3-11 are the necessary data for the model
JaccFgroup20 <- link(JaccSplit20_beta, data=Jsplit20[,3:10],n=10000)
linkmeanJaccF20 <- data.frame(apply(JaccFgroup20,2,mean))
PIJaccF20 <- t(data.frame(apply(JaccFgroup20,2,PI,prob=0.89)))
JaccLinkF20 <- data.frame(cbind(linkmeanJaccF20,PIJaccF20))
JaccLinkF20$Plot <- Jsplit20$Plot
JaccLinkF20 <- JaccLinkF20 %>% dplyr::select(Plot, everything())
colnames(JaccLinkF20) <- c("Plot","JaccPred","5CI","95CI")
#add true values, then calculate RMSE
JaccLinkF20 <- merge(JaccLinkF20,Jsplit20[,1:2],by="Plot",all.x=TRUE)
rmse(JaccLinkF20$Jacc,JaccLinkF20$JaccPred) #RMSE=   0.04494095
summary(lm(JaccLinkF20$JaccPred~JaccLinkF20$Jacc))$adj.r.squared #rsquare = 0.2386773
plot(JaccLinkF20$Jacc,JaccLinkF20$JaccPred, xlab="Observed Jaccards Similarity Index", ylab="Predicted Jaccards Similarity Index",
     main="Jaccards Similarity Index Out of Sample Predictions Oak/Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(JaccLinkF20$`5CI`),max(JaccLinkF20$`95CI`)))
abline(lm(JaccLinkF20$JaccPred~JaccLinkF20$Jacc, data=JaccLinkF20), col = "red")
for ( i in 1:nrow(JaccLinkF20) ) lines( rep(JaccLinkF20$Jacc[i],2) , JaccLinkF20[i,3:4] , col=rangi2 )


# prediction and RMSE
Jsplit21 <- list(
  Plot = FgroupSplitTest[["21"]]$PlotCN,
  Jacc = as.numeric(FgroupSplitTest[["21"]]$JaccardMean),
  PPT = scale(as.numeric(FgroupSplitTest[["21"]]$PPT) ,center=centerPPT21,scale=scalePPT21),
  MAT = scale(as.numeric(FgroupSplitTest[["21"]]$MAT) ,center=centerMAT21,scale=scaleMAT21),
  FAD = as.integer(FgroupSplitTest[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["21"]]$Elevation ,center=centerElevation21,scale=scaleElevation21),
  RHUM = scale(FgroupSplitTest[["21"]]$RHUM ,center=centerRHUM21,scale=scaleRHUM21),
  RAD = scale(FgroupSplitTest[["21"]]$RAD ,center=centerRAD21,scale=scaleRAD21)
)
#make data frame
Jsplit21 <- as.data.frame(Jsplit21)
#run link, columns 3-11 are the necessary data for the model
JaccFgroup21 <- link(JaccSplit21_beta, data=Jsplit21[,3:10],n=10000)
linkmeanJaccF21 <- data.frame(apply(JaccFgroup21,2,mean))
PIJaccF21 <- t(data.frame(apply(JaccFgroup21,2,PI,prob=0.89)))
JaccLinkF21 <- data.frame(cbind(linkmeanJaccF21,PIJaccF21))
JaccLinkF21$Plot <- Jsplit21$Plot
JaccLinkF21 <- JaccLinkF21 %>% dplyr::select(Plot, everything())
colnames(JaccLinkF21) <- c("Plot","JaccPred","5CI","95CI")
#add true values, then calculate RMSE
JaccLinkF21 <- merge(JaccLinkF21,Jsplit21[,1:2],by="Plot",all.x=TRUE)
rmse(JaccLinkF21$Jacc,JaccLinkF21$JaccPred) #RMSE=0.04291177
summary(lm(JaccLinkF21$JaccPred~JaccLinkF21$Jacc))$adj.r.squared #r-squared = 0.09981137
plot(JaccLinkF21$Jacc,JaccLinkF21$JaccPred, xlab="Observed Jaccards Similarity Index", ylab="Predicted Jaccards Similarity Index",
     main="Jaccards Similarity Index Out of Sample Predictions Oak/Hickory Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(JaccLinkF21$`5CI`),max(JaccLinkF21$`95CI`)))
abline(lm(JaccLinkF21$JaccPred~JaccLinkF21$Jacc, data=JaccLinkF21), col = "red")
for ( i in 1:nrow(JaccLinkF21) ) lines( rep(JaccLinkF21$Jacc[i],2) , JaccLinkF21[i,3:4] , col=rangi2 )

#Fgroup 23
Jsplit23 <- list(
  Plot = FgroupSplitTest[["23"]]$PlotCN,
  Jacc = as.numeric(FgroupSplitTest[["23"]]$JaccardMean),
  PPT = scale(as.numeric(FgroupSplitTest[["23"]]$PPT) ,center=centerPPT23,scale=scalePPT23),
  MAT = scale(as.numeric(FgroupSplitTest[["23"]]$MAT) ,center=centerMAT23,scale=scaleMAT23),
  FAD = as.integer(FgroupSplitTest[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["23"]]$Elevation ,center=centerElevation23,scale=scaleElevation23),
  RHUM = scale(FgroupSplitTest[["23"]]$RHUM ,center=centerRHUM23,scale=scaleRHUM23),
  RAD = scale(FgroupSplitTest[["23"]]$RAD ,center=centerRAD23,scale=scaleRAD23)
)
#make data frame
Jsplit23 <- as.data.frame(Jsplit23)
#run link, columns 3-11 are the necessary data for the model
JaccFgroup23 <- link(JaccSplit23_beta, data=Jsplit23[,3:10],n=10000)
linkmeanJaccF23 <- data.frame(apply(JaccFgroup23,2,mean))
PIJaccF23 <- t(data.frame(apply(JaccFgroup23,2,PI,prob=0.89)))
JaccLinkF23 <- data.frame(cbind(linkmeanJaccF23,PIJaccF23))
JaccLinkF23$Plot <- Jsplit23$Plot
JaccLinkF23 <- JaccLinkF23 %>% dplyr::select(Plot, everything())
colnames(JaccLinkF23) <- c("Plot","JaccPred","5CI","95CI")
#add true values, then calculate RMSE
JaccLinkF23 <- merge(JaccLinkF23,Jsplit23[,1:2],by="Plot",all.x=TRUE)
rmse(JaccLinkF23$Jacc,JaccLinkF23$JaccPred) #RMSE= 0.04928357
summary(lm(JaccLinkF23$JaccPred~JaccLinkF23$Jacc))$adj.r.squared #rsquare =  0.05382653
plot(JaccLinkF23$Jacc,JaccLinkF23$JaccPred, xlab="Observed Jaccards Similarity Index", ylab="Predicted Jaccards Similarity Index",
     main="Jaccards Similarity Index Out of Sample Predictions Elm/Ash/Cottonwood Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(JaccLinkF23$`5CI`),max(JaccLinkF23$`95CI`)))
abline(lm(JaccLinkF23$JaccPred~JaccLinkF23$Jacc, data=JaccLinkF23), col = "red")
for ( i in 1:nrow(JaccLinkF23) ) lines( rep(JaccLinkF23$Jacc[i],2) , JaccLinkF23[i,3:4] , col=rangi2 )


Jsplit24 <- list(
  Plot = FgroupSplitTest[["24"]]$PlotCN,
  Jacc = as.numeric(FgroupSplitTest[["24"]]$JaccardMean),
  PPT = scale(as.numeric(FgroupSplitTest[["24"]]$PPT) ,center=centerPPT24,scale=scalePPT24),
  MAT = scale(as.numeric(FgroupSplitTest[["24"]]$MAT) ,center=centerMAT24,scale=scaleMAT24),
  FAD = as.integer(FgroupSplitTest[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["24"]]$Elevation ,center=centerElevation24,scale=scaleElevation24),
  RHUM = scale(FgroupSplitTest[["24"]]$RHUM ,center=centerRHUM24,scale=scaleRHUM24),
  RAD = scale(FgroupSplitTest[["24"]]$RAD ,center=centerRAD24,scale=scaleRAD24)
)
#make data frame
Jsplit24 <- as.data.frame(Jsplit24)
#run link, columns 3-11 are the necessary data for the model
JaccFgroup24 <- link(JaccSplit24_beta, data=Jsplit24[,3:10],n=10000)
linkmeanJaccF24 <- data.frame(apply(JaccFgroup24,2,mean))
PIJaccF24 <- t(data.frame(apply(JaccFgroup24,2,PI,prob=0.89)))
JaccLinkF24 <- data.frame(cbind(linkmeanJaccF24,PIJaccF24))
JaccLinkF24$Plot <- Jsplit24$Plot
JaccLinkF24 <- JaccLinkF24 %>% dplyr::select(Plot, everything())
colnames(JaccLinkF24) <- c("Plot","JaccPred","5CI","95CI")
#add true values, then calculate RMSE
JaccLinkF24 <- merge(JaccLinkF24,Jsplit24[,1:2],by="Plot",all.x=TRUE)
rmse(JaccLinkF24$Jacc,JaccLinkF24$JaccPred) #RMSE=  0.03945913
summary(lm(JaccLinkF24$JaccPred~JaccLinkF24$Jacc))$adj.r.squared #rsquare =0.2714893
plot(JaccLinkF24$Jacc,JaccLinkF24$JaccPred, xlab="Observed Jaccards Similarity Index", ylab="Predicted Jaccards Similarity Index",
     main="Jaccards Similarity Index Out of Sample Predictions Maple/Beech/Birch Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(JaccLinkF24$`5CI`),max(JaccLinkF24$`95CI`)))
abline(lm(JaccLinkF24$JaccPred~JaccLinkF24$Jacc, data=JaccLinkF24), col = "red")
for ( i in 1:nrow(JaccLinkF24) ) lines( rep(JaccLinkF24$Jacc[i],2) , JaccLinkF24[i,3:4] , col=rangi2)

#Mean Pairwise Distance Model
MPDdat1 <- list(
  Plot = FgroupSplitTest[["1"]]$PlotCN,
  MPD = scale(as.numeric(FgroupSplitTest[["1"]]$MeanPairwiseDistance) ,center=centerMPD1,scale=scaleMPD1),
  PPT = scale(as.numeric(FgroupSplitTest[["1"]]$PPT) ,center=centerPPT1,scale=scalePPT1),
  MAT = scale(as.numeric(FgroupSplitTest[["1"]]$MAT) ,center=centerMAT1,scale=scaleMAT1),
  FAD = as.integer(FgroupSplitTest[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["1"]]$Elevation ,center=centerElevation1,scale=scaleElevation1),
  RHUM = scale(FgroupSplitTest[["1"]]$RHUM ,center=centerRHUM1,scale=scaleRHUM1),
  RAD = scale(FgroupSplitTest[["1"]]$RAD ,center=centerRAD1,scale=scaleRAD1)
)
#make data frame
MPDdat1 <- as.data.frame(MPDdat1)
#run link, columns 3-10 are the necessary data for the model
MPDFgroup1 <- link(MPDSplit1, data=MPDdat1[,3:10],n=10000)
linkmeanMPDF1 <- data.frame(apply(MPDFgroup1,2,mean))
PIMPDF1 <- t(data.frame(apply(MPDFgroup1,2,PI,prob=0.89)))
MPDLinkF1 <- data.frame(cbind(linkmeanMPDF1,PIMPDF1))
MPDLinkF1$Plot <- MPDdat1$Plot
MPDLinkF1 <- MPDLinkF1 %>% dplyr::select(Plot, everything())
colnames(MPDLinkF1) <- c("Plot","MPDPred","5CI","95CI")
#add true values, then calculate RMSE
MPDLinkF1 <- merge(MPDLinkF1,MPDdat1[,1:2],by="Plot",all.x=TRUE)
rmse(MPDLinkF1$MPD,MPDLinkF1$MPDPred) #RMSE=0.9981267
summary(lm(MPDLinkF1$MPDPred~MPDLinkF1$MPD))$adj.r.squared #r square = 0.1280022
par(mar=c(5,6,4,5)+.1)
plot(MPDLinkF1$MPD,MPDLinkF1$MPDPred, xlab="Observed Mean Pairwise Distance", ylab="Predicted Mean Pairwise Distance",
     main="Mean Pairwise Distance Out of Sample Predictions White/Red/Jack Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MPDLinkF1$`5CI`),max(MPDLinkF1$`95CI`)))
abline(lm(MPDLinkF1$MPDPred~MPDLinkF1$MPD, data=MPDLinkF1), col = "red")
for ( i in 1:nrow(MPDLinkF1) ) lines( rep(MPDLinkF1$MPD[i],2) , MPDLinkF1[i,3:4] , col=rangi2 )

#Fgroup 5
MPDdat5 <- list(
  Plot = FgroupSplitTest[["5"]]$PlotCN,
  MPD = scale(as.numeric(FgroupSplitTest[["5"]]$MeanPairwiseDistance) ,center=centerMPD5,scale=scaleMPD5),
  PPT = scale(as.numeric(FgroupSplitTest[["5"]]$PPT) ,center=centerPPT5,scale=scalePPT5),
  MAT = scale(as.numeric(FgroupSplitTest[["5"]]$MAT) ,center=centerMAT5,scale=scaleMAT5),
  FAD = as.integer(FgroupSplitTest[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["5"]]$Elevation ,center=centerElevation5,scale=scaleElevation5),
  RHUM = scale(FgroupSplitTest[["5"]]$RHUM ,center=centerRHUM5,scale=scaleRHUM5),
  RAD = scale(FgroupSplitTest[["5"]]$RAD ,center=centerRAD5,scale=scaleRAD5)
)
#make data frame
MPDdat5 <- as.data.frame(MPDdat5)
#run link, columns 3-10 are the necessary data for the model
MPDFgroup5 <- link(MPDSplit5, data=MPDdat5[,3:10],n=10000)
linkmeanMPDF5 <- data.frame(apply(MPDFgroup5,2,mean))
PIMPDF5 <- t(data.frame(apply(MPDFgroup5,2,PI,prob=0.89)))
MPDLinkF5 <- data.frame(cbind(linkmeanMPDF5,PIMPDF5))
MPDLinkF5$Plot <- MPDdat5$Plot
MPDLinkF5 <- MPDLinkF5 %>% dplyr::select(Plot, everything())
colnames(MPDLinkF5) <- c("Plot","MPDPred","5CI","95CI")
#add true values, then calculate RMSE
MPDLinkF5 <- merge(MPDLinkF5,MPDdat5[,1:2],by="Plot",all.x=TRUE)

rmse(MPDLinkF5$MPD,MPDLinkF5$MPDPred) #RMSE= 0.916823
summary(lm(MPDLinkF5$MPDPred~MPDLinkF5$MPD))$adj.r.squared #r square = 0.003237774
plot(MPDLinkF5$MPD,MPDLinkF5$MPDPred, xlab="Observed Mean Pairwise Distance", ylab="Predicted Mean Pairwise Distance",
     main="Mean Pairwise Distance Out of Sample Predictions Loblolly/Short Leaf Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MPDLinkF5$`5CI`),max(MPDLinkF5$`95CI`)))
abline(lm(MPDLinkF5$MPDPred~MPDLinkF5$MPD, data=MPDLinkF5), col = "red")
for ( i in 1:nrow(MPDLinkF5) ) lines( rep(MPDLinkF5$MPD[i],2) , MPDLinkF5[i,3:4] , col=rangi2 )

MPDdat20 <- list(
  Plot = FgroupSplitTest[["20"]]$PlotCN,
  MPD = scale(as.numeric(FgroupSplitTest[["20"]]$MeanPairwiseDistance) ,center=centerMPD20,scale=scaleMPD20),
  PPT = scale(as.numeric(FgroupSplitTest[["20"]]$PPT) ,center=centerPPT20,scale=scalePPT20),
  MAT = scale(as.numeric(FgroupSplitTest[["20"]]$MAT) ,center=centerMAT20,scale=scaleMAT20),
  FAD = as.integer(FgroupSplitTest[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["20"]]$Elevation ,center=centerElevation20,scale=scaleElevation20),
  RHUM = scale(FgroupSplitTest[["20"]]$RHUM ,center=centerRHUM20,scale=scaleRHUM20),
  RAD = scale(FgroupSplitTest[["20"]]$RAD ,center=centerRAD20,scale=scaleRAD20)
)
#make data frame
MPDdat20 <- as.data.frame(MPDdat20)
#run link, columns 3-10 are the necessary data for the model
MPDFgroup20 <- link(MPDSplit20, data=MPDdat20[,3:10],n=10000)
linkmeanMPDF20 <- data.frame(apply(MPDFgroup20,2,mean))
PIMPDF20 <- t(data.frame(apply(MPDFgroup20,2,PI,prob=0.89)))
MPDLinkF20 <- data.frame(cbind(linkmeanMPDF20,PIMPDF20))
MPDLinkF20$Plot <- MPDdat20$Plot
MPDLinkF20 <- MPDLinkF20 %>% dplyr::select(Plot, everything())
colnames(MPDLinkF20) <- c("Plot","MPDPred","5CI","95CI")
#add true values, then calculate RMSE
MPDLinkF20 <- merge(MPDLinkF20,MPDdat20[,1:2],by="Plot",all.x=TRUE)
rmse(MPDLinkF20$MPD,MPDLinkF20$MPDPred) #RMSE= 0.8176588
summary(lm(MPDLinkF20$MPDPred~MPDLinkF20$MPD))$adj.r.squared # r square =0.008670748
plot(MPDLinkF20$MPD,MPDLinkF20$MPDPred, xlab="Observed Mean Pairwise Distance", ylab="Predicted Mean Pairwise Distance",
     main="Mean Pairwise Distance Out of Sample Predictions Oak/Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MPDLinkF20$`5CI`),max(MPDLinkF20$`95CI`)))
abline(lm(MPDLinkF20$MPDPred~MPDLinkF20$MPD, data=MPDLinkF20), col = "red")
for ( i in 1:nrow(MPDLinkF20) ) lines( rep(MPDLinkF20$MPD[i],2) , MPDLinkF20[i,3:4] , col=rangi2 )

MPDdat21 <- list(
  Plot = FgroupSplitTest[["21"]]$PlotCN,
  MPD = scale(as.numeric(FgroupSplitTest[["21"]]$MeanPairwiseDistance) ,center=centerMPD21,scale=scaleMPD21),
  PPT = scale(as.numeric(FgroupSplitTest[["21"]]$PPT) ,center=centerPPT21,scale=scalePPT21),
  MAT = scale(as.numeric(FgroupSplitTest[["21"]]$MAT) ,center=centerMAT21,scale=scaleMAT21),
  FAD = as.integer(FgroupSplitTest[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["21"]]$Elevation ,center=centerElevation21,scale=scaleElevation21),
  RHUM = scale(FgroupSplitTest[["21"]]$RHUM ,center=centerRHUM21,scale=scaleRHUM21),
  RAD = scale(FgroupSplitTest[["21"]]$RAD ,center=centerRAD21,scale=scaleRAD21)
)
#make data frame
MPDdat21 <- as.data.frame(MPDdat21)
#run link, columns 3-10 are the necessary data for the model
MPDFgroup21 <- link(MPDSplit21, data=MPDdat21[,3:10],n=10000)
linkmeanMPDF21 <- data.frame(apply(MPDFgroup21,2,mean))
PIMPDF21 <- t(data.frame(apply(MPDFgroup21,2,PI,prob=0.89)))
MPDLinkF21 <- data.frame(cbind(linkmeanMPDF21,PIMPDF21))
MPDLinkF21$Plot <- MPDdat21$Plot
MPDLinkF21 <- MPDLinkF21 %>% dplyr::select(Plot, everything())
colnames(MPDLinkF21) <- c("Plot","MPDPred","5CI","95CI")
#add true values, then calculate RMSE
MPDLinkF21 <- merge(MPDLinkF21,MPDdat21[,1:2],by="Plot",all.x=TRUE)

rmse(MPDLinkF21$MPD,MPDLinkF21$MPDPred) #RMSE=1.384441
summary(lm(MPDLinkF21$MPDPred~MPDLinkF21$MPD))$adj.r.squared #r square ~ 0
plot(MPDLinkF21$MPD,MPDLinkF21$MPDPred, xlab="Observed Mean Pairwise Distance", ylab="Predicted Mean Pairwise Distance",
     main="Mean Pairwise Distance Out of Sample Predictions Oak/Hickory Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MPDLinkF21$`5CI`),max(MPDLinkF21$`95CI`)))
abline(lm(MPDLinkF21$MPDPred~MPDLinkF21$MPD, data=MPDLinkF21), col = "red")
for ( i in 1:nrow(MPDLinkF21) ) lines( rep(MPDLinkF21$MPD[i],2) , MPDLinkF21[i,3:4] , col=rangi2 )

#Fgroup 23
MPDdat23 <- list(
  Plot = FgroupSplitTest[["23"]]$PlotCN,
  MPD = scale(as.numeric(FgroupSplitTest[["23"]]$MeanPairwiseDistance) ,center=centerMPD23,scale=scaleMPD23),
  PPT = scale(as.numeric(FgroupSplitTest[["23"]]$PPT) ,center=centerPPT23,scale=scalePPT23),
  MAT = scale(as.numeric(FgroupSplitTest[["23"]]$MAT) ,center=centerMAT23,scale=scaleMAT23),
  FAD = as.integer(FgroupSplitTest[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["23"]]$Elevation ,center=centerElevation23,scale=scaleElevation23),
  RHUM = scale(FgroupSplitTest[["23"]]$RHUM ,center=centerRHUM23,scale=scaleRHUM23),
  RAD = scale(FgroupSplitTest[["23"]]$RAD ,center=centerRAD23,scale=scaleRAD23)
)
#make data frame
MPDdat23 <- as.data.frame(MPDdat23)
#run link, columns 3-10 are the necessary data for the model
MPDFgroup23 <- link(MPDSplit23, data=MPDdat23[,3:10],n=10000)
linkmeanMPDF23 <- data.frame(apply(MPDFgroup23,2,mean))
PIMPDF23 <- t(data.frame(apply(MPDFgroup23,2,PI,prob=0.89)))
MPDLinkF23 <- data.frame(cbind(linkmeanMPDF23,PIMPDF23))
MPDLinkF23$Plot <- MPDdat23$Plot
MPDLinkF23 <- MPDLinkF23 %>% dplyr::select(Plot, everything())
colnames(MPDLinkF23) <- c("Plot","MPDPred","5CI","95CI")
#add true values, then calculate RMSE
MPDLinkF23 <- merge(MPDLinkF23,MPDdat23[,1:2],by="Plot",all.x=TRUE)

rmse(MPDLinkF23$MPD,MPDLinkF23$MPDPred) #RMSE=0.9900063
summary(lm(MPDLinkF23$MPDPred~MPDLinkF23$MPD))$adj.r.squared #r square = 0.080005
plot(MPDLinkF23$MPD,MPDLinkF23$MPDPred, xlab="Observed Mean Pairwise Distance", ylab="Predicted Mean Pairwise Distance",
     main="Mean Pairwise Distance Out of Sample Predictions Elm/Ash/Cottonwood Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MPDLinkF23$`5CI`),max(MPDLinkF23$`95CI`)))
abline(lm(MPDLinkF23$MPDPred~MPDLinkF23$MPD, data=MPDLinkF23), col = "red")
for ( i in 1:nrow(MPDLinkF23) ) lines( rep(MPDLinkF23$MPD[i],2) , MPDLinkF23[i,3:4] , col=rangi2 )

MPDdat24 <- list(
  Plot = FgroupSplitTest[["24"]]$PlotCN,
  MPD = scale(as.numeric(FgroupSplitTest[["24"]]$MeanPairwiseDistance) ,center=centerMPD24,scale=scaleMPD24),
  PPT = scale(as.numeric(FgroupSplitTest[["24"]]$PPT) ,center=centerPPT24,scale=scalePPT24),
  MAT = scale(as.numeric(FgroupSplitTest[["24"]]$MAT) ,center=centerMAT24,scale=scaleMAT24),
  FAD = as.integer(FgroupSplitTest[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["24"]]$Elevation ,center=centerElevation24,scale=scaleElevation24),
  RHUM = scale(FgroupSplitTest[["24"]]$RHUM ,center=centerRHUM24,scale=scaleRHUM24),
  RAD = scale(FgroupSplitTest[["24"]]$RAD ,center=centerRAD24,scale=scaleRAD24)
)
#make data frame
MPDdat24 <- as.data.frame(MPDdat24)
#run link, columns 3-10 are the necessary data for the model
MPDFgroup24 <- link(MPDSplit24, data=MPDdat24[,3:10],n=10000)
linkmeanMPDF24 <- data.frame(apply(MPDFgroup24,2,mean))
PIMPDF24 <- t(data.frame(apply(MPDFgroup24,2,PI,prob=0.89)))
MPDLinkF24 <- data.frame(cbind(linkmeanMPDF24,PIMPDF24))
MPDLinkF24$Plot <- MPDdat24$Plot
MPDLinkF24 <- MPDLinkF24 %>% dplyr::select(Plot, everything())
colnames(MPDLinkF24) <- c("Plot","MPDPred","5CI","95CI")
#add true values, then calculate RMSE
MPDLinkF24 <- merge(MPDLinkF24,MPDdat24[,1:2],by="Plot",all.x=TRUE)
rmse(MPDLinkF24$MPD,MPDLinkF24$MPDPred) #RMSE= 0.954109
summary(lm(MPDLinkF24$MPDPred~MPDLinkF24$MPD))$adj.r.squared # r square =0.09550743
plot(MPDLinkF24$MPD,MPDLinkF24$MPDPred, xlab="Observed Mean Pairwise Distance", ylab="Predicted Mean Pairwise Distance",
     main="Mean Pairwise Distance Out of Sample Predictions Maple/Beech/Birch Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MPDLinkF24$`5CI`),max(MPDLinkF24$`95CI`)))
abline(lm(MPDLinkF24$MPDPred~MPDLinkF24$MPD, data=MPDLinkF24), col = "red")
for ( i in 1:nrow(MPDLinkF24) ) lines( rep(MPDLinkF24$MPD[i],2) , MPDLinkF24[i,3:4] , col=rangi2 )

#Tree mortality models
MSplit1 <- list(
  Plot = FgroupSplitTest[["1"]]$PlotCN,
  Mort = as.numeric(FgroupSplitTest[["1"]]$MortRate),
  PPT = scale(as.numeric(FgroupSplitTest[["1"]]$PPT) ,center=centerPPT1,scale=scalePPT1),
  MAT = scale(as.numeric(FgroupSplitTest[["1"]]$MAT) ,center=centerMAT1,scale=scaleMAT1),
  FAD = as.integer(FgroupSplitTest[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["1"]]$Elevation ,center=centerElevation1,scale=scaleElevation1),
  RHUM = scale(FgroupSplitTest[["1"]]$RHUM ,center=centerRHUM1,scale=scaleRHUM1),
  RAD = scale(FgroupSplitTest[["1"]]$RAD ,center=centerRAD1,scale=scaleRAD1),
  BA = scale(FgroupSplitTest[["1"]]$BAdead ,center=centerBA1,scale=scaleBA1)
)
#make data frame
MSplit1 <- as.data.frame(MSplit1)
#run link, columns 3-11 are the necessary data for the model
MortFgroup1 <- link(MortSplit1_beta, data=MSplit1[,3:11],n=10000)
linkmeanMortF1 <- data.frame(apply(MortFgroup1,2,mean))
PIMortF1 <- t(data.frame(apply(MortFgroup1,2,PI,prob=0.89)))
MortLinkF1 <- data.frame(cbind(linkmeanMortF1,PIMortF1))
MortLinkF1$Plot <- MSplit1$Plot
MortLinkF1 <- MortLinkF1 %>% dplyr::select(Plot, everything())
colnames(MortLinkF1) <- c("Plot","MortPred","5CI","95CI")
#add true values, then calculate RMSE
MortLinkF1 <- merge(MortLinkF1,MSplit1[,1:2],by="Plot",all.x=TRUE)
rmse(MortLinkF1$Mort,MortLinkF1$MortPred) #RMSE= 0.1252776
summary(lm(MortLinkF1$MortPred~MortLinkF1$Mort))$adj.r.squared #r-squared = 0.4697137
par(mar=c(6,6,6,2)+.1)
plot(MortLinkF1$Mort,MortLinkF1$MortPred, xlab="Observed Tree Mortality Rate", ylab="Predicted Tree Mortality Rate",
     main="Tree Mortality Out of Sample Predictions White/Red/Jack Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MortLinkF1$`5CI`),max(MortLinkF1$`95CI`)))
abline(lm(MortLinkF1$MortPred~MortLinkF1$Mort, data=MortLinkF1), col = "red")
for ( i in 1:nrow(MortLinkF1) ) lines( rep(MortLinkF1$Mort[i],2) , MortLinkF1[i,3:4] , col=rangi2 )

#Fgroup 5
MSplit5 <- list(
  Plot = FgroupSplitTest[["5"]]$PlotCN,
  Mort = as.numeric(FgroupSplitTest[["5"]]$MortRate),
  PPT = scale(as.numeric(FgroupSplitTest[["5"]]$PPT) ,center=centerPPT5,scale=scalePPT5),
  MAT = scale(as.numeric(FgroupSplitTest[["5"]]$MAT) ,center=centerMAT5,scale=scaleMAT5),
  FAD = as.integer(FgroupSplitTest[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["5"]]$Elevation ,center=centerElevation5,scale=scaleElevation5),
  RHUM = scale(FgroupSplitTest[["5"]]$RHUM ,center=centerRHUM5,scale=scaleRHUM5),
  RAD = scale(FgroupSplitTest[["5"]]$RAD ,center=centerRAD5,scale=scaleRAD5),
  BA = scale(FgroupSplitTest[["5"]]$BAdead ,center=centerBA5,scale=scaleBA5)
)
#make data frame
MSplit5 <- as.data.frame(MSplit5)
#run link, columns 3-11 are the necessary data for the model
MortFgroup5 <- link(MortSplit5_beta, data=MSplit5[,3:11],n=10000)
linkmeanMortF5 <- data.frame(apply(MortFgroup5,2,mean))
PIMortF5 <- t(data.frame(apply(MortFgroup5,2,PI,prob=0.89)))
MortLinkF5 <- data.frame(cbind(linkmeanMortF5,PIMortF5))
MortLinkF5$Plot <- MSplit5$Plot
MortLinkF5 <- MortLinkF5 %>% dplyr::select(Plot, everything())
colnames(MortLinkF5) <- c("Plot","MortPred","5CI","95CI")
#add true values, then calculate RMSE
MortLinkF5 <- merge(MortLinkF5,MSplit5[,1:2],by="Plot",all.x=TRUE)
rmse(MortLinkF5$Mort,MortLinkF5$MortPred) #RMSE= 0.06706296
summary(lm(MortLinkF5$MortPred~MortLinkF5$Mort))$adj.r.squared #rsquare =  0.4058525
plot(MortLinkF5$Mort,MortLinkF5$MortPred, xlab="Observed Tree Mortality Rate", ylab="Predicted Tree Mortality Rate",
     main="Tree Mortality Out of Sample Predictions Loblolly/Short Leaf Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MortLinkF5$`5CI`),max(MortLinkF5$`95CI`)))
abline(lm(MortLinkF5$MortPred~MortLinkF5$Mort, data=MortLinkF5), col = "red")
for ( i in 1:nrow(MortLinkF5) ) lines( rep(MortLinkF5$Mort[i],2) , MortLinkF5[i,3:4] , col=rangi2 )


MSplit20 <- list(
  Plot = FgroupSplitTest[["20"]]$PlotCN,
  Mort = as.numeric(FgroupSplitTest[["20"]]$MortRate),
  PPT = scale(as.numeric(FgroupSplitTest[["20"]]$PPT) ,center=centerPPT20,scale=scalePPT20),
  MAT = scale(as.numeric(FgroupSplitTest[["20"]]$MAT) ,center=centerMAT20,scale=scaleMAT20),
  FAD = as.integer(FgroupSplitTest[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["20"]]$Elevation ,center=centerElevation20,scale=scaleElevation20),
  RHUM = scale(FgroupSplitTest[["20"]]$RHUM ,center=centerRHUM20,scale=scaleRHUM20),
  RAD = scale(FgroupSplitTest[["20"]]$RAD ,center=centerRAD20,scale=scaleRAD20),
  BA = scale(FgroupSplitTest[["20"]]$BAdead ,center=centerBA20,scale=scaleBA20)
)
#make data frame
MSplit20 <- as.data.frame(MSplit20)
#run link, columns 3-11 are the necessary data for the model
MortFgroup20 <- link(MortSplit20_beta, data=MSplit20[,3:11],n=10000)
linkmeanMortF20 <- data.frame(apply(MortFgroup20,2,mean))
PIMortF20 <- t(data.frame(apply(MortFgroup20,2,PI,prob=0.89)))
MortLinkF20 <- data.frame(cbind(linkmeanMortF20,PIMortF20))
MortLinkF20$Plot <- MSplit20$Plot
MortLinkF20 <- MortLinkF20 %>% dplyr::select(Plot, everything())
colnames(MortLinkF20) <- c("Plot","MortPred","5CI","95CI")
#add true values, then calculate RMSE
MortLinkF20 <- merge(MortLinkF20,MSplit20[,1:2],by="Plot",all.x=TRUE)
rmse(MortLinkF20$Mort,MortLinkF20$MortPred) #RMSE= 0.2958845
summary(lm(MortLinkF20$MortPred~MortLinkF20$Mort))$adj.r.squared #rsquare = 0.1242376
plot(MortLinkF20$Mort,MortLinkF20$MortPred, xlab="Observed Tree Mortality Rate", ylab="Predicted Tree Mortality Rate",
     main="Tree Mortality Out of Sample Predictions Oak/Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MortLinkF20$`5CI`),max(MortLinkF20$`95CI`)))
abline(lm(MortLinkF20$MortPred~MortLinkF20$Mort, data=MortLinkF20), col = "red")
for ( i in 1:nrow(MortLinkF20) ) lines( rep(MortLinkF20$Mort[i],2) , MortLinkF20[i,3:4] , col=rangi2 )


# prediction and RMSE
MSplit21 <- list(
  Plot = FgroupSplitTest[["21"]]$PlotCN,
  Mort = as.numeric(FgroupSplitTest[["21"]]$MortRate),
  PPT = scale(as.numeric(FgroupSplitTest[["21"]]$PPT) ,center=centerPPT21,scale=scalePPT21),
  MAT = scale(as.numeric(FgroupSplitTest[["21"]]$MAT) ,center=centerMAT21,scale=scaleMAT21),
  FAD = as.integer(FgroupSplitTest[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["21"]]$Elevation ,center=centerElevation21,scale=scaleElevation21),
  RHUM = scale(FgroupSplitTest[["21"]]$RHUM ,center=centerRHUM21,scale=scaleRHUM21),
  RAD = scale(FgroupSplitTest[["21"]]$RAD ,center=centerRAD21,scale=scaleRAD21),
  BA = scale(FgroupSplitTest[["21"]]$BAdead ,center=centerBA21,scale=scaleBA21)
)
#make data frame
MSplit21 <- as.data.frame(MSplit21)
#run link, columns 3-11 are the necessary data for the model
MortFgroup21 <- link(MortSplit21_beta, data=MSplit21[,3:11],n=10000)
linkmeanMortF21 <- data.frame(apply(MortFgroup21,2,mean))
PIMortF21 <- t(data.frame(apply(MortFgroup21,2,PI,prob=0.89)))
MortLinkF21 <- data.frame(cbind(linkmeanMortF21,PIMortF21))
MortLinkF21$Plot <- MSplit21$Plot
MortLinkF21 <- MortLinkF21 %>% dplyr::select(Plot, everything())
colnames(MortLinkF21) <- c("Plot","MortPred","5CI","95CI")
#add true values, then calculate RMSE
MortLinkF21 <- merge(MortLinkF21,MSplit21[,1:2],by="Plot",all.x=TRUE)
rmse(MortLinkF21$Mort,MortLinkF21$MortPred) #RMSE=0.08231854
summary(lm(MortLinkF21$MortPred~MortLinkF21$Mort))$adj.r.squared #r-squared = 0.2621541
plot(MortLinkF21$Mort,MortLinkF21$MortPred, xlab="Observed Tree Mortality Rate", ylab="Predicted Tree Mortality Rate",
     main="Tree Mortality Out of Sample Predictions Oak/Hickory Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MortLinkF21$`5CI`),max(MortLinkF21$`95CI`)))
abline(lm(MortLinkF21$MortPred~MortLinkF21$Mort, data=MortLinkF21), col = "red")
for ( i in 1:nrow(MortLinkF21) ) lines( rep(MortLinkF21$Mort[i],2) , MortLinkF21[i,3:4] , col=rangi2 )

#Fgroup 23
MSplit23 <- list(
  Plot = FgroupSplitTest[["23"]]$PlotCN,
  Mort = as.numeric(FgroupSplitTest[["23"]]$MortRate),
  PPT = scale(as.numeric(FgroupSplitTest[["23"]]$PPT) ,center=centerPPT23,scale=scalePPT23),
  MAT = scale(as.numeric(FgroupSplitTest[["23"]]$MAT) ,center=centerMAT23,scale=scaleMAT23),
  FAD = as.integer(FgroupSplitTest[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["23"]]$Elevation ,center=centerElevation23,scale=scaleElevation23),
  RHUM = scale(FgroupSplitTest[["23"]]$RHUM ,center=centerRHUM23,scale=scaleRHUM23),
  RAD = scale(FgroupSplitTest[["23"]]$RAD ,center=centerRAD23,scale=scaleRAD23),
  BA = scale(FgroupSplitTest[["23"]]$BAdead ,center=centerBA23,scale=scaleBA23)
)
#make data frame
MSplit23 <- as.data.frame(MSplit23)
#run link, columns 3-11 are the necessary data for the model
MortFgroup23 <- link(MortSplit23_beta, data=MSplit23[,3:11],n=10000)
linkmeanMortF23 <- data.frame(apply(MortFgroup23,2,mean))
PIMortF23 <- t(data.frame(apply(MortFgroup23,2,PI,prob=0.89)))
MortLinkF23 <- data.frame(cbind(linkmeanMortF23,PIMortF23))
MortLinkF23$Plot <- MSplit23$Plot
MortLinkF23 <- MortLinkF23 %>% dplyr::select(Plot, everything())
colnames(MortLinkF23) <- c("Plot","MortPred","5CI","95CI")
#add true values, then calculate RMSE
MortLinkF23 <- merge(MortLinkF23,MSplit23[,1:2],by="Plot",all.x=TRUE)
rmse(MortLinkF23$Mort,MortLinkF23$MortPred) #RMSE=  0.1299602
summary(lm(MortLinkF23$MortPred~MortLinkF23$Mort))$adj.r.squared #rsquare =  0.1769274
plot(MortLinkF23$Mort,MortLinkF23$MortPred, xlab="Observed Tree Mortality Rate", ylab="Predicted Tree Mortality Rate",
     main="Tree Mortality Out of Sample Predictions Elm/Ash/Cottonwood Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MortLinkF23$`5CI`),max(MortLinkF23$`95CI`)))
abline(lm(MortLinkF23$MortPred~MortLinkF23$Mort, data=MortLinkF23), col = "red")
for ( i in 1:nrow(MortLinkF23) ) lines( rep(MortLinkF23$Mort[i],2) , MortLinkF23[i,3:4] , col=rangi2 )

MSplit24 <- list(
  Plot = FgroupSplitTest[["24"]]$PlotCN,
  Mort = as.numeric(FgroupSplitTest[["24"]]$MortRate),
  PPT = scale(as.numeric(FgroupSplitTest[["24"]]$PPT) ,center=centerPPT24,scale=scalePPT24),
  MAT = scale(as.numeric(FgroupSplitTest[["24"]]$MAT) ,center=centerMAT24,scale=scaleMAT24),
  FAD = as.integer(FgroupSplitTest[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["24"]]$Elevation ,center=centerElevation24,scale=scaleElevation24),
  RHUM = scale(FgroupSplitTest[["24"]]$RHUM ,center=centerRHUM24,scale=scaleRHUM24),
  RAD = scale(FgroupSplitTest[["24"]]$RAD ,center=centerRAD24,scale=scaleRAD24),
  BA = scale(FgroupSplitTest[["24"]]$BAdead ,center=centerBA24,scale=scaleBA24)
)
#make data frame
MSplit24 <- as.data.frame(MSplit24)
#run link, columns 3-11 are the necessary data for the model
MortFgroup24 <- link(MortSplit24_beta, data=MSplit24[,3:11],n=10000)
linkmeanMortF24 <- data.frame(apply(MortFgroup24,2,mean))
PIMortF24 <- t(data.frame(apply(MortFgroup24,2,PI,prob=0.89)))
MortLinkF24 <- data.frame(cbind(linkmeanMortF24,PIMortF24))
MortLinkF24$Plot <- MSplit24$Plot
MortLinkF24 <- MortLinkF24 %>% dplyr::select(Plot, everything())
colnames(MortLinkF24) <- c("Plot","MortPred","5CI","95CI")
#add true values, then calculate RMSE
MortLinkF24 <- merge(MortLinkF24,MSplit24[,1:2],by="Plot",all.x=TRUE)
rmse(MortLinkF24$Mort,MortLinkF24$MortPred) #RMSE= 0.09125216
summary(lm(MortLinkF24$MortPred~MortLinkF24$Mort))$adj.r.squared #rsquare = 0.2053145
plot(MortLinkF24$Mort,MortLinkF24$MortPred, xlab="Observed Tree Mortality Rate", ylab="Predicted Tree Mortality Rate",
     main="Tree Mortality Out of Sample Predictions Maple/Beech/Birch Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(MortLinkF24$`5CI`),max(MortLinkF24$`95CI`)))
abline(lm(MortLinkF24$MortPred~MortLinkF24$Mort, data=MortLinkF24), col = "red")
for ( i in 1:nrow(MortLinkF24) ) lines( rep(MortLinkF24$Mort[i],2) , MortLinkF24[i,3:4] , col=rangi2 )

#Air Quality Models
ASplit1 <- list(
  Plot = FgroupSplitTest[["1"]]$PlotCN,
  AQI = scale(as.numeric(FgroupSplitTest[["1"]]$MaxAQI) ,center=centerAQI1,scale=scaleAQI1),
  PPT = scale(as.numeric(FgroupSplitTest[["1"]]$PPT) ,center=centerPPT1,scale=scalePPT1),
  MAT = scale(as.numeric(FgroupSplitTest[["1"]]$MAT) ,center=centerMAT1,scale=scaleMAT1),
  FAD = as.integer(FgroupSplitTest[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["1"]]$Elevation ,center=centerElevation1,scale=scaleElevation1),
  RHUM = scale(FgroupSplitTest[["1"]]$RHUM ,center=centerRHUM1,scale=scaleRHUM1),
  RAD = scale(FgroupSplitTest[["1"]]$RAD ,center=centerRAD1,scale=scaleRAD1),
  Year = FgroupSplitTest[["1"]]$Year
)
#make data frame
ASplit1 <- as.data.frame(ASplit1)
ASplit1 <- merge(ASplit1,F1time_weight,by="Year",all.x=TRUE)
ASplit1 <- distinct(ASplit1)
#run link, columns 3-10 are the necessary data for the model
AQIFgroup1 <- link(AQISplit1, data=ASplit1[,3:12],n=10000)
linkmeanAQIF1 <- data.frame(apply(AQIFgroup1,2,mean))
PIAQIF1 <- t(data.frame(apply(AQIFgroup1,2,PI,prob=0.89)))
AQILinkF1 <- data.frame(cbind(linkmeanAQIF1,PIAQIF1))
AQILinkF1$Plot <- ASplit1$Plot
AQILinkF1 <- AQILinkF1 %>% dplyr::select(Plot, everything())
colnames(AQILinkF1) <- c("Plot","AQIPred","5CI","95CI")
#add true values, then calculate RMSE
AQILinkF1 <- merge(AQILinkF1,ASplit1[,1:3],by="Plot",all.x=TRUE)
#actual,predicted
par(mar=c(5,6,4,5)+.1)
rmse(AQILinkF1$AQI,AQILinkF1$AQIPred) #RMSE=  0.7970714
summary(lm(AQILinkF1$AQIPred~AQILinkF1$AQI))$adj.r.squared #r square =0.3396977
plot(AQILinkF1$AQI,AQILinkF1$AQIPred, xlab="Observed Air Quality Index", ylab="Predicted Air Quality Index",
     main="Air Quality Index Out of Sample Predictions White/Red/Jack Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(AQILinkF1$`5CI`),max(AQILinkF1$`95CI`)))
abline(lm(AQILinkF1$AQIPred~AQILinkF1$AQI, data=AQILinkF1), col = "red")
for ( i in 1:nrow(AQILinkF1) ) lines( rep(AQILinkF1$AQI[i],2) , AQILinkF1[i,3:4] , col=rangi2)

#Fgroup 5
ASplit5 <- list(
  Plot = FgroupSplitTest[["5"]]$PlotCN,
  AQI = scale(as.numeric(FgroupSplitTest[["5"]]$MaxAQI) ,center=centerAQI5,scale=scaleAQI5),
  PPT = scale(as.numeric(FgroupSplitTest[["5"]]$PPT) ,center=centerPPT5,scale=scalePPT5),
  MAT = scale(as.numeric(FgroupSplitTest[["5"]]$MAT) ,center=centerMAT5,scale=scaleMAT5),
  FAD = as.integer(FgroupSplitTest[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["5"]]$Elevation ,center=centerElevation5,scale=scaleElevation5),
  RHUM = scale(FgroupSplitTest[["5"]]$RHUM ,center=centerRHUM5,scale=scaleRHUM5),
  RAD = scale(FgroupSplitTest[["5"]]$RAD ,center=centerRAD5,scale=scaleRAD5),
  Year = FgroupSplitTest[["5"]]$Year
)
#make data frame
ASplit5 <- as.data.frame(ASplit5)
ASplit5 <- merge(ASplit5,F5time_weight,by="Year",all.x=TRUE)
ASplit5 <- distinct(ASplit5)
#run link, columns 3-12 are the necessary data for the model
AQIFgroup5 <- link(AQISplit5, data=ASplit5[,3:12],n=10000)
linkmeanAQIF5 <- data.frame(apply(AQIFgroup5,2,mean))
PIAQIF5 <- t(data.frame(apply(AQIFgroup5,2,PI,prob=0.89)))
AQILinkF5 <- data.frame(cbind(linkmeanAQIF5,PIAQIF5))
AQILinkF5$Plot <- ASplit5$Plot
AQILinkF5 <- AQILinkF5 %>% dplyr::select(Plot, everything())
colnames(AQILinkF5) <- c("Plot","AQIPred","5CI","95CI")
#add true values, then calculate RMSE
AQILinkF5 <- merge(AQILinkF5,ASplit5[,1:3],by="Plot",all.x=TRUE)
#actual,predicted
par(mar=c(5,6,4,2)+.1)
rmse(AQILinkF5$AQI,AQILinkF5$AQIPred) #RMSE=0.8643893
summary(lm(AQILinkF5$AQIPred~AQILinkF5$AQI))$adj.r.squared #r square = 0.4579113
plot(AQILinkF5$AQI,AQILinkF5$AQIPred, xlab="Observed Air Quality Index", ylab="Predicted Air Quality Index",
     main="Air Quality Index Out of Sample Predictions Loblolly/Short Leaf Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(AQILinkF5$`5CI`),max(AQILinkF5$`95CI`)))
abline(lm(AQILinkF5$AQIPred~AQILinkF5$AQI, data=AQILinkF5), col = "red")
for ( i in 1:nrow(AQILinkF5) ) lines( rep(AQILinkF5$AQI[i],2) , AQILinkF5[i,3:4] , col=rangi2 )


ASplit20 <- list(
  Plot = FgroupSplitTest[["20"]]$PlotCN,
  AQI = scale(as.numeric(FgroupSplitTest[["20"]]$MaxAQI) ,center=centerAQI20,scale=scaleAQI20),
  PPT = scale(as.numeric(FgroupSplitTest[["20"]]$PPT) ,center=centerPPT20,scale=scalePPT20),
  MAT = scale(as.numeric(FgroupSplitTest[["20"]]$MAT) ,center=centerMAT20,scale=scaleMAT20),
  FAD = as.integer(FgroupSplitTest[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["20"]]$Elevation ,center=centerElevation20,scale=scaleElevation20),
  RHUM = scale(FgroupSplitTest[["20"]]$RHUM ,center=centerRHUM20,scale=scaleRHUM20),
  RAD = scale(FgroupSplitTest[["20"]]$RAD ,center=centerRAD20,scale=scaleRAD20),
  Year = FgroupSplitTest[["20"]]$Year
)
#make data frame
ASplit20 <- as.data.frame(ASplit20)
ASplit20 <- merge(ASplit20,F20time_weight,by="Year",all.x=TRUE)
ASplit20 <- distinct(ASplit20)
#run link, columns 3-12 are the necessary data for the model
AQIFgroup20 <- link(AQISplit20, data=ASplit20[,3:12],n=10000)
linkmeanAQIF20 <- data.frame(apply(AQIFgroup20,2,mean))
PIAQIF20 <- t(data.frame(apply(AQIFgroup20,2,PI,prob=0.89)))
AQILinkF20 <- data.frame(cbind(linkmeanAQIF20,PIAQIF20))
AQILinkF20$Plot <- ASplit20$Plot
AQILinkF20 <- AQILinkF20 %>% dplyr::select(Plot, everything())
colnames(AQILinkF20) <- c("Plot","AQIPred","5CI","95CI")
#add true values, then calculate RMSE
AQILinkF20 <- merge(AQILinkF20,ASplit20[,1:3],by="Plot",all.x=TRUE)
#actual,predicted
par(mar=c(5,6,4,2)+.1)
rmse(AQILinkF20$AQI,AQILinkF20$AQIPred) #RMSE= 0.8844117
summary(lm(AQILinkF20$AQIPred~AQILinkF20$AQI))$adj.r.squared #r square =0.2979763
plot(AQILinkF20$AQI,AQILinkF20$AQIPred, xlab="Observed Air Quality Index", ylab="Predicted Air Quality Index",
     main="Air Quality Index Out of Sample Predictions Oak/Pine Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,
     mgp=c(3,1,0),ylim=c(min(AQILinkF20$`5CI`),max(AQILinkF20$`95CI`)))
abline(lm(AQILinkF20$AQIPred~AQILinkF20$AQI, data=AQILinkF20), col = "red")
for ( i in 1:nrow(AQILinkF20) ) lines( rep(AQILinkF20$AQI[i],2) , AQILinkF20[i,3:4] , col=rangi2 )

# prediction and RMSE
ASplit21 <- list(
  Plot = FgroupSplitTest[["21"]]$PlotCN,
  AQI = scale(as.numeric(FgroupSplitTest[["21"]]$MaxAQI) ,center=centerAQI21,scale=scaleAQI21),
  PPT = scale(as.numeric(FgroupSplitTest[["21"]]$PPT) ,center=centerPPT21,scale=scalePPT21),
  MAT = scale(as.numeric(FgroupSplitTest[["21"]]$MAT) ,center=centerMAT21,scale=scaleMAT21),
  FAD = as.integer(FgroupSplitTest[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["21"]]$Elevation ,center=centerElevation21,scale=scaleElevation21),
  RHUM = scale(FgroupSplitTest[["21"]]$RHUM ,center=centerRHUM21,scale=scaleRHUM21),
  RAD = scale(FgroupSplitTest[["21"]]$RAD ,center=centerRAD21,scale=scaleRAD21),
  Year = FgroupSplitTest[["21"]]$Year
)
#make data frame
ASplit21 <- as.data.frame(ASplit21)
ASplit21 <- merge(ASplit21,F21time_weight,by="Year",all.x=TRUE)
ASplit21 <- distinct(ASplit21)
#run link, columns 3-12 are the necessary data for the model
AQIFgroup21 <- link(AQISplit21, data=ASplit21[,3:12],n=10000)
linkmeanAQIF21 <- data.frame(apply(AQIFgroup21,2,mean))
PIAQIF21 <- t(data.frame(apply(AQIFgroup21,2,PI,prob=0.89)))
AQILinkF21 <- data.frame(cbind(linkmeanAQIF21,PIAQIF21))
AQILinkF21$Plot <- ASplit21$Plot
AQILinkF21 <- AQILinkF21 %>% dplyr::select(Plot, everything())
colnames(AQILinkF21) <- c("Plot","AQIPred","5CI","95CI")
#add true values, then calculate RMSE
AQILinkF21 <- merge(AQILinkF21,ASplit21[,1:3],by="Plot",all.x=TRUE)
#actual,predicted
par(mar=c(5,6,4,2)+.1)
rmse(AQILinkF21$AQI,AQILinkF21$AQIPred) #RMSE= 0.8315048
summary(lm(AQILinkF21$AQIPred~AQILinkF21$AQI))$adj.r.squared #r square = 0.5333611
plot(AQILinkF21$AQI,AQILinkF21$AQIPred, xlab="Observed Air Quality Index", ylab="Predicted Air Quality Index",
     main="Air Quality Index Out of Sample Predictions Oak/Hickory Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(AQILinkF21$`5CI`),max(AQILinkF21$`95CI`)))
abline(lm(AQILinkF21$AQIPred~AQILinkF21$AQI, data=AQILinkF21), col = "red")
for ( i in 1:nrow(AQILinkF21) ) lines( rep(AQILinkF21$AQI[i],2) , AQILinkF21[i,3:4] , col=rangi2 )

#Fgroup 23
ASplit23 <- list(
  Plot = FgroupSplitTest[["23"]]$PlotCN,
  AQI = scale(as.numeric(FgroupSplitTest[["23"]]$MaxAQI) ,center=centerAQI23,scale=scaleAQI23),
  PPT = scale(as.numeric(FgroupSplitTest[["23"]]$PPT) ,center=centerPPT23,scale=scalePPT23),
  MAT = scale(as.numeric(FgroupSplitTest[["23"]]$MAT) ,center=centerMAT23,scale=scaleMAT23),
  FAD = as.integer(FgroupSplitTest[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["23"]]$Elevation ,center=centerElevation23,scale=scaleElevation23),
  RHUM = scale(FgroupSplitTest[["23"]]$RHUM ,center=centerRHUM23,scale=scaleRHUM23),
  RAD = scale(FgroupSplitTest[["23"]]$RAD ,center=centerRAD23,scale=scaleRAD23),
  Year = FgroupSplitTest[["23"]]$Year
)
#make data frame
ASplit23 <- as.data.frame(ASplit23)
ASplit23 <- merge(ASplit23,F23time_weight,by="Year",all.x=TRUE)
ASplit23 <- distinct(ASplit23)
#run link, columns 3-12 are the necessary data for the model
AQIFgroup23 <- link(AQISplit23, data=ASplit23[,3:12],n=10000)
linkmeanAQIF23 <- data.frame(apply(AQIFgroup23,2,mean))
PIAQIF23 <- t(data.frame(apply(AQIFgroup23,2,PI,prob=0.89)))
AQILinkF23 <- data.frame(cbind(linkmeanAQIF23,PIAQIF23))
AQILinkF23$Plot <- ASplit23$Plot
AQILinkF23 <- AQILinkF23 %>% dplyr::select(Plot, everything())
colnames(AQILinkF23) <- c("Plot","AQIPred","5CI","95CI")
#add true values, then calculate RMSE
AQILinkF23 <- merge(AQILinkF23,ASplit23[,1:3],by="Plot",all.x=TRUE)
#actual,predicted
par(mar=c(5,6,4,2)+.1)
rmse(AQILinkF23$AQI,AQILinkF23$AQIPred) #RMSE=0.9385458
summary(lm(AQILinkF23$AQIPred~AQILinkF23$AQI))$adj.r.squared #r square = 0.3068894
plot(AQILinkF23$AQI,AQILinkF23$AQIPred, xlab="Observed Air Quality Index", ylab="Predicted Air Quality Index",
     main="Air Quality Index Out of Sample Predictions Elm/Ash/Cottonwood Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(AQILinkF23$`5CI`),max(AQILinkF23$`95CI`)))
abline(lm(AQILinkF23$AQIPred~AQILinkF23$AQI, data=AQILinkF23), col = "red")
for ( i in 1:nrow(AQILinkF23) ) lines( rep(AQILinkF23$AQI[i],2) , AQILinkF23[i,3:4] , col=rangi2)


ASplit24 <- list(
  Plot = FgroupSplitTest[["24"]]$PlotCN,
  AQI = scale(as.numeric(FgroupSplitTest[["24"]]$MaxAQI) ,center=centerAQI24,scale=scaleAQI24),
  PPT = scale(as.numeric(FgroupSplitTest[["24"]]$PPT) ,center=centerPPT24,scale=scalePPT24),
  MAT = scale(as.numeric(FgroupSplitTest[["24"]]$MAT) ,center=centerMAT24,scale=scaleMAT24),
  FAD = as.integer(FgroupSplitTest[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTest[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTest[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTest[["24"]]$Elevation ,center=centerElevation24,scale=scaleElevation24),
  RHUM = scale(FgroupSplitTest[["24"]]$RHUM ,center=centerRHUM24,scale=scaleRHUM24),
  RAD = scale(FgroupSplitTest[["24"]]$RAD ,center=centerRAD24,scale=scaleRAD24),
  Year = FgroupSplitTest[["24"]]$Year
)
#make data frame
ASplit24 <- as.data.frame(ASplit24)
ASplit24 <- merge(ASplit24,F24time_weight,by="Year",all.x=TRUE)
ASplit24 <- distinct(ASplit24)
#run link, columns 3-12 are the necessary data for the model
AQIFgroup24 <- link(AQISplit24, data=ASplit24[,3:12],n=10000)
linkmeanAQIF24 <- data.frame(apply(AQIFgroup24,2,mean))
PIAQIF24 <- t(data.frame(apply(AQIFgroup24,2,PI,prob=0.89)))
AQILinkF24 <- data.frame(cbind(linkmeanAQIF24,PIAQIF24))
AQILinkF24$Plot <- ASplit24$Plot
AQILinkF24 <- AQILinkF24 %>% dplyr::select(Plot, everything())
colnames(AQILinkF24) <- c("Plot","AQIPred","5CI","95CI")
#add true values, then calculate RMSE
AQILinkF24 <- merge(AQILinkF24,ASplit24[,1:3],by="Plot",all.x=TRUE)
#actual,predicted
par(mar=c(5,6,4,2)+.1)
rmse(AQILinkF24$AQI,AQILinkF24$AQIPred) #RMSE= 0.8726476
summary(lm(AQILinkF24$AQIPred~AQILinkF24$AQI))$adj.r.squared #r square=0.4578658
plot(AQILinkF24$AQI,AQILinkF24$AQIPred, xlab="Observed Air Quality Index", ylab="Predicted Air Quality Index",
     main="Air Quality Index Out of Sample Predictions Maple/Beech/Birch Group", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(AQILinkF24$`5CI`),max(AQILinkF24$`95CI`)))
abline(lm(AQILinkF24$AQIPred~AQILinkF24$AQI, data=AQILinkF24), col = "red")
for ( i in 1:nrow(AQILinkF24) ) lines( rep(AQILinkF24$AQI[i],2) , AQILinkF24[i,3:4] , col=rangi2 )

SQISplit <- list(
  Plot = SQItest$PlotCN,
  SQI = as.numeric(SQItest$SQIPCT)/100,
  PPT = scale(as.numeric(SQItest$PPT),center=centerPPTSQI,scale=scalePPTSQI),
  MAT = scale(as.numeric(SQItest$MAT),center=centerMATSQI,scale=scaleMATSQI),
  FAD = as.integer(SQItest$FADClass),
  Soil = as.integer(as.factor(SQItest$SoilID)),
  FGroup = as.integer(as.factor(SQItest$ForestGroupID)),
  Elevation = scale(SQItest$Elevation,center=centerElevationSQI,scale=scaleElevationSQI),
  RHUM = scale(SQItest$RHUM ,center=centerRHUMSQI,scale=scaleRHUMSQI),
  RAD = scale(SQItest$RAD ,center=centerRADSQI,scale=scaleRADSQI)
)
SQISplit <- as.data.frame(SQISplit)
SQIlink <- link(SQIMod_beta, data= SQISplit[,3:10],n=10000)
linkmeanSQI <- data.frame(apply(SQIlink,2,mean))
PISQI <- t(data.frame(apply(SQIlink,2,PI,prob=0.89)))
SQILink <- data.frame(cbind(linkmeanSQI,PISQI))
SQILink$Plot <- SQISplit$Plot
SQILink <- SQILink %>% dplyr::select(Plot, everything())
colnames(SQILink) <- c("Plot","SQIPred","5CI","95CI")
#add true values, then calculate RMSE
SQILink <- merge(SQILink,SQISplit[,1:2],by="Plot",all.x=TRUE)
#actual,predicted -> SQI , SQIPred
rmse(SQILink$SQI,SQILink$SQIPred) #RMSE=0.8449718
summary(lm(SQILink$SQIPred~SQILink$SQI))$adj.r.square #r square= 0.2488706
par(mar=c(5,6,4,5)+.1)
plot(SQILink$SQI,SQILink$SQIPred, xlab="Observed Soil Quality Index", ylab="Predicted Soil Quality Index",
     main="Soil Quality Index Out of Sample Predictions All Groups", cex.main=1.6,cex.lab=2.0,cex.axis=1.6,col=rangi2,mgp=c(3,1,0),ylim=c(min(SQILink$`5CI`),max(SQILink$`95CI`)))
abline(lm(SQILink$SQIPred~SQILink$SQI, data=SQILink), col = "red")
for ( i in 1:nrow(SQILink) ) lines( rep(SQILink$SQI[i],2) , SQILink[i,3:4] , col=rangi2 )
