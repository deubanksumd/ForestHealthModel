#Mid Atlantic FIA Bayes Models
setwd("~/Desktop/FIAModelCode") #change to your directory
library(dplyr)
library(rethinking)
library(Metrics)
library(purrr)
library(tidyr)

#read in data
allFIA <- read.csv("allFIAfinal.csv")
allFIA <- allFIA[-c(1)]
allFIA[allFIA == 0] <- 0.00000000001
SQIdata <- read.csv("allSQIfinal.csv")
SQIdata <- SQIdata[-c(1)]
SQIdata <- na.omit(SQIdata)
#Create forest groups
Fgroups <- c(100, 120, 140, 150, 160, 170, 180, 200, 220, 240, 260, 280, 300, 320, 
             340, 360, 370, 380, 390, 400, 500, 600, 700, 800, 900, 910, 920, 940, 
             960, 970, 980, 990, 999, Inf)
Flabels <- seq_along(Fgroups[-1])
allFIA <- allFIA %>% mutate(ForestGroup = Flabels[findInterval(allFIA$ForestType,Fgroups)])
SQIdata <- SQIdata %>% mutate(ForestGroup = Flabels[findInterval(SQIdata$ForestType,Fgroups)])
#factorize soil order and forest type
allFIA$SoilID <- as.integer(as.factor(allFIA$soil_order))
allFIA$ForestID <- as.integer(as.factor(allFIA$ForestType))
SQIdata$SoilID <- as.integer(as.factor(SQIdata$soil_order))
SQIdata$ForestID <- as.integer(as.factor(SQIdata$ForestType))

#Separate by forest group
FgroupSplit <- split(allFIA, allFIA$ForestGroup)
#groups with adequate numbers to model - 1,5,20,21,23,24

#create out of sample dataset, 15% of data
set.seed(123)
#function to create training data and testing data
train_data <- function(df) {df %>%mutate(split = sample(c(rep("train", floor(0.85 * nrow(.))),rep("test", nrow(.) - floor(0.85 * nrow(.))))))}
FgroupSplitAll <- map(FgroupSplit, ~ tryCatch(train_data(.x)))
FgroupSplitTrain  <- map(FgroupSplitAll, ~ filter(.x, split == "train") %>% dplyr::select(-split))
FgroupSplitTest  <- map(FgroupSplitAll, ~ filter(.x, split == "test") %>% dplyr::select(-split))

#create SQIdata with plot means - some plots have two soil measures during the same survey
SQIdata1 <- SQIdata %>% group_by(PlotCN) %>% mutate(SQIPCT = mean(SQIPCT)) %>% ungroup() %>% distinct(PlotCN, .keep_all = TRUE)

#time weighting for air quality index
F1years <- as.integer(FgroupSplitTrain[["1"]]$Year)
F1max_year <- max(F1years)
F1time_diff <- F1max_year - F1years

F1time_weight <- exp(-0.1 * F1time_diff)
F1time_weight <- F1time_weight / mean(F1time_weight)
F1time_weight <- data.frame(F1years,F1time_weight)
names(F1time_weight) <- c("Year","time_weight")

F5years <- as.integer(FgroupSplitTrain[["5"]]$Year)
F5max_year <- max(F5years)
F5time_diff <- F5max_year - F5years

F5time_weight <- exp(-0.1 * F5time_diff)
F5time_weight <- F5time_weight / mean(F5time_weight)
F5time_weight <- data.frame(F5years,F5time_weight)
names(F5time_weight) <- c("Year","time_weight")

F20years <- as.integer(FgroupSplitTrain[["20"]]$Year)
F20max_year <- max(F20years)
F20time_diff <- F20max_year - F20years

F20time_weight <- exp(-0.1 * F20time_diff)
F20time_weight <- F20time_weight / mean(F20time_weight)
F20time_weight <- data.frame(F20years,F20time_weight)
names(F20time_weight) <- c("Year","time_weight")

F21years <- as.integer(FgroupSplitTrain[["21"]]$Year)
F21max_year <- max(F21years)
F21time_diff <- F21max_year - F21years

F21time_weight <- exp(-0.1 * F21time_diff)
F21time_weight <- F21time_weight / mean(F21time_weight)
F21time_weight <- data.frame(F21years,F21time_weight)
names(F21time_weight) <- c("Year","time_weight")

F23years <- as.integer(FgroupSplitTrain[["23"]]$Year)
F23max_year <- max(F23years)
F23time_diff <- F23max_year - F23years

F23time_weight <- exp(-0.1 * F23time_diff)
F23time_weight <- F23time_weight / mean(F23time_weight)
F23time_weight <- data.frame(F23years,F23time_weight)
names(F23time_weight) <- c("Year","time_weight")

F24years <- as.integer(FgroupSplitTrain[["24"]]$Year)
F24max_year <- max(F24years)
F24time_diff <- F24max_year - F24years

F24time_weight <- exp(-0.1 * F24time_diff)
F24time_weight <- F24time_weight / mean(F24time_weight)
F24time_weight <- data.frame(F24years,F24time_weight)
names(F24time_weight) <- c("Year","time_weight")

#Carbon Model
#create data sets for each forest group, 1-24
datCarbSplit1 <- list(
  CarbSum = scale(as.numeric(FgroupSplitTrain[["1"]]$CarbonPerAcre)),
  PPT = scale(as.numeric(FgroupSplitTrain[["1"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["1"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["1"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["1"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["1"]]$RAD)
)
datCarbSplit5 <- list(
  CarbSum = scale(as.numeric(FgroupSplitTrain[["5"]]$CarbonPerAcre)),
  PPT = scale(as.numeric(FgroupSplitTrain[["5"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["5"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["5"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["5"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["5"]]$RAD)
)

datCarbSplit20 <- list(
  CarbSum = scale(as.numeric(FgroupSplitTrain[["20"]]$CarbonPerAcre)),
  PPT = scale(as.numeric(FgroupSplitTrain[["20"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["20"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["20"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["20"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["20"]]$RAD)
)
datCarbSplit21 <- list(
  CarbSum = scale(as.numeric(FgroupSplitTrain[["21"]]$CarbonPerAcre)),
  PPT = scale(as.numeric(FgroupSplitTrain[["21"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["21"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["21"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["21"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["21"]]$RAD)
)
datCarbSplit23 <- list(
  CarbSum = scale(as.numeric(FgroupSplitTrain[["23"]]$CarbonPerAcre)),
  PPT = scale(as.numeric(FgroupSplitTrain[["23"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["23"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["23"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["23"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["23"]]$RAD)
)
datCarbSplit24 <- list(
  CarbSum = scale(as.numeric(FgroupSplitTrain[["24"]]$CarbonPerAcre)),
  PPT = scale(as.numeric(FgroupSplitTrain[["24"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["24"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["24"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["24"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["24"]]$RAD)
)

#save center and scaling for out of sample predictions, can use clim and plot variables for all models except SQI
centerCarb1 <- attr(datCarbSplit1$CarbSum, "scaled:center")
scaleCarb1 <- attr(datCarbSplit1$CarbSum, "scaled:scale")
centerMAT1 <- attr(datCarbSplit1$MAT, "scaled:center")
scaleMAT1 <- attr(datCarbSplit1$MAT, "scaled:scale")
centerPPT1 <- attr(datCarbSplit1$PPT, "scaled:center")
scalePPT1 <- attr(datCarbSplit1$PPT, "scaled:scale")
centerRHUM1 <- attr(datCarbSplit1$RHUM, "scaled:center")
scaleRHUM1 <- attr(datCarbSplit1$RHUM, "scaled:scale")
centerRAD1 <- attr(datCarbSplit1$RAD, "scaled:center")
scaleRAD1 <- attr(datCarbSplit1$RAD, "scaled:scale")
centerElevation1 <- attr(datCarbSplit1$Elevation, "scaled:center")
scaleElevation1 <- attr(datCarbSplit1$Elevation, "scaled:scale")
centerCarb5 <- attr(datCarbSplit5$CarbSum, "scaled:center")
scaleCarb5 <- attr(datCarbSplit5$CarbSum, "scaled:scale")
centerMAT5 <- attr(datCarbSplit5$MAT, "scaled:center")
scaleMAT5 <- attr(datCarbSplit5$MAT, "scaled:scale")
centerPPT5 <- attr(datCarbSplit5$PPT, "scaled:center")
scalePPT5 <- attr(datCarbSplit5$PPT, "scaled:scale")
centerRHUM5 <- attr(datCarbSplit5$RHUM, "scaled:center")
scaleRHUM5 <- attr(datCarbSplit5$RHUM, "scaled:scale")
centerRAD5 <- attr(datCarbSplit5$RAD, "scaled:center")
scaleRAD5 <- attr(datCarbSplit5$RAD, "scaled:scale")
centerElevation5 <- attr(datCarbSplit5$Elevation, "scaled:center")
scaleElevation5 <- attr(datCarbSplit5$Elevation, "scaled:scale")
centerCarb20 <- attr(datCarbSplit20$CarbSum, "scaled:center")
scaleCarb20 <- attr(datCarbSplit20$CarbSum, "scaled:scale")
centerMAT20 <- attr(datCarbSplit20$MAT, "scaled:center")
scaleMAT20 <- attr(datCarbSplit20$MAT, "scaled:scale")
centerPPT20 <- attr(datCarbSplit20$PPT, "scaled:center")
scalePPT20 <- attr(datCarbSplit20$PPT, "scaled:scale")
centerRHUM20 <- attr(datCarbSplit20$RHUM, "scaled:center")
scaleRHUM20 <- attr(datCarbSplit20$RHUM, "scaled:scale")
centerRAD20 <- attr(datCarbSplit20$RAD, "scaled:center")
scaleRAD20 <- attr(datCarbSplit20$RAD, "scaled:scale")
centerElevation20 <- attr(datCarbSplit20$Elevation, "scaled:center")
scaleElevation20 <- attr(datCarbSplit20$Elevation, "scaled:scale")
centerCarb21 <- attr(datCarbSplit21$CarbSum, "scaled:center")
scaleCarb21 <- attr(datCarbSplit21$CarbSum, "scaled:scale")
centerMAT21 <- attr(datCarbSplit21$MAT, "scaled:center")
scaleMAT21 <- attr(datCarbSplit21$MAT, "scaled:scale")
centerPPT21 <- attr(datCarbSplit21$PPT, "scaled:center")
scalePPT21 <- attr(datCarbSplit21$PPT, "scaled:scale")
centerRHUM21 <- attr(datCarbSplit21$RHUM, "scaled:center")
scaleRHUM21 <- attr(datCarbSplit21$RHUM, "scaled:scale")
centerRAD21 <- attr(datCarbSplit21$RAD, "scaled:center")
scaleRAD21 <- attr(datCarbSplit21$RAD, "scaled:scale")
centerElevation21 <- attr(datCarbSplit21$Elevation, "scaled:center")
scaleElevation21 <- attr(datCarbSplit21$Elevation, "scaled:scale")
centerCarb23 <- attr(datCarbSplit23$CarbSum, "scaled:center")
scaleCarb23 <- attr(datCarbSplit23$CarbSum, "scaled:scale")
centerMAT23 <- attr(datCarbSplit23$MAT, "scaled:center")
scaleMAT23 <- attr(datCarbSplit23$MAT, "scaled:scale")
centerPPT23 <- attr(datCarbSplit23$PPT, "scaled:center")
scalePPT23 <- attr(datCarbSplit23$PPT, "scaled:scale")
centerRHUM23 <- attr(datCarbSplit23$RHUM, "scaled:center")
scaleRHUM23 <- attr(datCarbSplit23$RHUM, "scaled:scale")
centerRAD23 <- attr(datCarbSplit23$RAD, "scaled:center")
scaleRAD23 <- attr(datCarbSplit23$RAD, "scaled:scale")
centerElevation23 <- attr(datCarbSplit23$Elevation, "scaled:center")
scaleElevation23 <- attr(datCarbSplit23$Elevation, "scaled:scale")
centerCarb24 <- attr(datCarbSplit24$CarbSum, "scaled:center")
scaleCarb24 <- attr(datCarbSplit24$CarbSum, "scaled:scale")
centerMAT24 <- attr(datCarbSplit24$MAT, "scaled:center")
scaleMAT24 <- attr(datCarbSplit24$MAT, "scaled:scale")
centerPPT24 <- attr(datCarbSplit24$PPT, "scaled:center")
scalePPT24 <- attr(datCarbSplit24$PPT, "scaled:scale")
centerRHUM24 <- attr(datCarbSplit24$RHUM, "scaled:center")
scaleRHUM24 <- attr(datCarbSplit24$RHUM, "scaled:scale")
centerRAD24 <- attr(datCarbSplit24$RAD, "scaled:center")
scaleRAD24 <- attr(datCarbSplit24$RAD, "scaled:scale")
centerElevation24 <- attr(datCarbSplit24$Elevation, "scaled:center")
scaleElevation24 <- attr(datCarbSplit24$Elevation, "scaled:scale")

#Carbon Model, repeat for groups 1-24
CarbSplit1 <- ulam(
  alist(
    CarbSum ~ dnorm(mu,sigma),
    mu <- a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD],
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datCarbSplit1, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#Labels for posterior means plot
F1Labels <- c("RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4",
              "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4",
              "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4",
              "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4",
              "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4",
              "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4",
              "RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
              "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
              "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD Soil",
              "SD FAD","SD Model")
par(mar=c(6,6,6,2)+.1)
#plot posterior means
#plot(precis(CarbSplit1, depth=2),labels=F1Labels,main="Model Parameter Estimates, Carbon Storage White/Red/Jack Pine Group")
#write and save to plot posterior means of predictor varibles
precisvaluesCarb1 <- data.frame(precis(CarbSplit1, depth=2))
write.csv(precisvaluesCarb1, "precisValsCarb1.csv")
#repeat all steps
CarbSplit5 <- ulam(
  alist(
    CarbSum ~ dnorm(mu,sigma),
    mu <- a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD],
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datCarbSplit5, log_lik = TRUE, chains=4, cores=8, iter=10000
)

F5Labels <- c("RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4",
              "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4",
              "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4",
              "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4",
              "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4",
              "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4",
              "RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
              "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
              "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD Soil",
              "SD FAD","SD Model")
par(mar=c(6,6,6,2)+.1)
#plot(precis(CarbSplit5, depth=2),labels=F5Labels,main="Model Parameter Estimates, Carbon Storage Loblolly/Short Leaf Pine Group")
precisvaluesCarb5 <- data.frame(precis(CarbSplit5, depth=2))
write.csv(precisvaluesCarb5, "precisValsCarb5.csv")

CarbSplit20 <- ulam(
  alist(
    CarbSum ~ dnorm(mu,sigma),
    mu <- a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD],
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datCarbSplit20, log_lik = TRUE, chains=4, cores=8, iter=10000
)

F20Labels <- c("RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4","RAD x ForestType5","RAD x ForestType6",
               "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4","RHUM x ForestType5","RHUM x ForestType6",
               "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4","Elevation x ForestType5","Elevation x ForestType6",
               "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4","PPT x ForestType5","PPT x ForestType6",
               "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4","MAT x ForestType5","MAT x ForestType6",
               "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4","Intercept x ForestType5","Intercept x ForestType6",
               "RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
               "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
               "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD Soil",
               "SD FAD","SD Model")
par(mar=c(6,6,6,2)+.1)
#plot(precis(CarbSplit20, depth=2),labels=F20Labels,main="Model Parameter Estimates, Carbon Storage Oak/Pine Group")
precisvaluesCarb20 <- data.frame(precis(CarbSplit20, depth=2))
write.csv(precisvaluesCarb20, "precisValsCarb20.csv")

CarbSplit21 <- ulam(
  alist(
    CarbSum ~ dnorm(mu,sigma),
    mu <- a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD],
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datCarbSplit21, log_lik = TRUE, chains=4, cores=8, iter=10000
)

F21Labels <- c("RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4","RAD x ForestType5","RAD x ForestType6","RAD x ForestType7","RAD x ForestType8",
               "RAD x ForestType9","RAD x ForestType10","RAD x ForestType11","RAD x ForestType12","RAD x ForestType13","RAD x ForestType14","RAD x ForestType15","RAD x ForestType16",
               "RAD x ForestType17","RAD x ForestType18","RAD x ForestType19",
               "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4","RHUM x ForestType5","RHUM x ForestType6","RHUM x ForestType7","RHUM x ForestType8",
               "RHUM x ForestType9","RHUM x ForestType10","RHUM x ForestType11","RHUM x ForestType12","RHUM x ForestType13","RHUM x ForestType14","RHUM x ForestType15","RHUM x ForestType16",
               "RHUM x ForestType17","RHUM x ForestType18","RHUM x ForestType19",
               "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4","Elevation x ForestType5","Elevation x ForestType6",
               "Elevation x ForestType7","Elevation x ForestType8","Elevation x ForestType9","Elevation x ForestType10","Elevation x ForestType11","Elevation x ForestType12",
               "Elevation x ForestType13","Elevation x ForestType14","Elevation x ForestType15","Elevation x ForestType16","Elevation x ForestType17","Elevation x ForestType18","Elevation x ForestType19",
               "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4","PPT x ForestType5","PPT x ForestType6","PPT x ForestType7","PPT x ForestType8",
               "PPT x ForestType9","PPT x ForestType10","PPT x ForestType11","PPT x ForestType12","PPT x ForestType13","PPT x ForestType14","PPT x ForestType15","PPT x ForestType16",
               "PPT x ForestType17","PPT x ForestType18","PPT x ForestType19",
               "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4","MAT x ForestType5","MAT x ForestType6","MAT x ForestType7","MAT x ForestType8",
               "MAT x ForestType9","MAT x ForestType10","MAT x ForestType11","MAT x ForestType12","MAT x ForestType13","MAT x ForestType14","MAT x ForestType15","MAT x ForestType16",
               "MAT x ForestType17","MAT x ForestType18","MAT x ForestType19",
               "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4","Intercept x ForestType5","Intercept x ForestType6",
               "Intercept x ForestType7","Intercept x ForestType8","Intercept x ForestType9","Intercept x ForestType10","Intercept x ForestType11","Intercept x ForestType12",
               "Intercept x ForestType13","Intercept x ForestType14","Intercept x ForestType15","Intercept x ForestType16","Intercept x ForestType17","Intercept x ForestType18","Intercept x ForestType19",
               "RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
               "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","Soil Order 7 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
               "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD Soil",
               "SD FAD","SD Model")
par(mar=c(8,6,6,2)+.1)
#plot(precis(CarbSplit21, depth=2),labels=F21Labels,cex.lab=0.7,main="Model Parameter Estimates, Carbon Storage Oak/Hickory Group")
precisvaluesCarb21 <- data.frame(precis(CarbSplit21, depth=2))
write.csv(precisvaluesCarb21, "precisValsCarb21.csv")

CarbSplit23 <- ulam(
  alist(
    CarbSum ~ dnorm(mu,sigma),
    mu <- a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD],
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datCarbSplit23, log_lik = TRUE, chains=4, cores=8, iter=10000
)
F23Labels <- c("RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4","RAD x ForestType5","RAD x ForestType6","RAD x ForestType7","RAD x ForestType8",
               "RAD x ForestType9",
               "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4","RHUM x ForestType5","RHUM x ForestType6","RHUM x ForestType7","RHUM x ForestType8",
               "RHUM x ForestType9",
               "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4","Elevation x ForestType5","Elevation x ForestType6",
               "Elevation x ForestType7","Elevation x ForestType8","Elevation x ForestType9",
               "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4","PPT x ForestType5","PPT x ForestType6","PPT x ForestType7","PPT x ForestType8",
               "PPT x ForestType9",
               "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4","MAT x ForestType5","MAT x ForestType6","MAT x ForestType7","MAT x ForestType8",
               "MAT x ForestType9",
               "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4","Intercept x ForestType5","Intercept x ForestType6",
               "Intercept x ForestType7","Intercept x ForestType8","Intercept x ForestType9",
               "RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
               "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
               "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD Soil",
               "SD FAD","SD Model")
par(mar=c(8,6,6,2)+.1)
#plot(precis(CarbSplit23, depth=2),labels=F23Labels,cex.lab=0.7,main="Model Parameter Estimates, Carbon Storage Elm/Ash/Cottonwood Group")
precisvaluesCarb23 <- data.frame(precis(CarbSplit23, depth=2))
write.csv(precisvaluesCarb23, "precisValsCarb23.csv")

CarbSplit24 <- ulam(
  alist(
    CarbSum ~ dnorm(mu,sigma),
    mu <- (a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD]),
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datCarbSplit24, log_lik = TRUE, chains=4, cores=8, iter=10000
)
F24Labels <- c("RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4",
               "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4",
               "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4",
               "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4",
               "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4",
               "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4",
               "RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
               "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
               "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD Soil",
               "SD FAD","SD Model")
par(mar=c(8,6,6,2)+.1)
#plot(precis(CarbSplit24, depth=2),labels=F24Labels,cex.lab=0.7,main="Model Parameter Estimates, Carbon Storage Maple/Beech/Birch Group")
precisvaluesCarb24 <- data.frame(precis(CarbSplit24, depth=2))
write.csv(precisvaluesCarb24, "precisValsCarb24.csv")

#Repeat this code with CarbSplit 1-24 for visualization
#posterior predictive checks, link to simulate model, then calculate mean point estimates and 95/5 confidence intervals, run one model at a time.
Carbmu <- link(CarbSplit1)
Carbmu_mean <- apply(Carbmu , 2 , mean )
Carbmu_PI <- apply(Carbmu , 2 , PI )
#group and scale data to prepare for plotting
Carblinkvals <-rbind(Carbmu_mean,Carbmu_PI)
Carblinkvals <- data.frame(t(Carblinkvals))
Carbline <- data.frame(cbind(FgroupSplitTrain[["1"]]$CarbonPerAcre,Carblinkvals[,1:3]))
names(Carbline) <- c("ActualVals","PredVals","CI5","CI95")
Carbline$ActualVals <- scale(Carbline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomCarb <- sample_n(Carbline, 200)
RandCarbPI <- RandomCarb[,3:4]
RandCarbPI <- t(RandCarbPI)
#Plot model predictions vs actual values
Cline <- lm(Carbline$PredVals~(Carbline$ActualVals), data = Carbline)
par(mar=c(5,6,4,2)+.1)
plot(y=RandomCarb$PredVals , x=(RandomCarb$ActualVals) , col=rangi2 , ylim=c(min(RandomCarb$CI5),max(RandomCarb$CI95)) ,
     xlab="Observed Carbon Storage" , ylab="Predicted Carbon Storage",  main = 'Predicted Carbon Storage vs Observed Carbon Storage White/Red/Jack Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomCarb) ) lines( rep(RandomCarb$ActualVals[i],2) , RandCarbPI[,i] , col=rangi2 )
abline(Cline, col = "red")
#reports p-value and adjusted R^2
summary(Cline) #r2 0.4258 
#reports root mean square error
rmse(Carbline$ActualVals,Carbline$PredVals) #rmse 0.7574934

#repeat steps for forest group 5
Carbmu <- link(CarbSplit5)
Carbmu_mean <- apply(Carbmu , 2 , mean )
Carbmu_PI <- apply(Carbmu , 2 , PI )

Carblinkvals <-rbind(Carbmu_mean,Carbmu_PI)
Carblinkvals <- data.frame(t(Carblinkvals))
Carbline <- data.frame(cbind(FgroupSplitTrain[["5"]]$CarbonPerAcre,Carblinkvals[,1:3]))
names(Carbline) <- c("ActualVals","PredVals","CI5","CI95")
Carbline$ActualVals <- scale(Carbline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomCarb <- sample_n(Carbline, 200)
RandCarbPI <- RandomCarb[,3:4]
RandCarbPI <- t(RandCarbPI)

Cline <- lm(Carbline$PredVals~(Carbline$ActualVals), data = Carbline)
par(mar=c(5,6,4,2)+.1)
plot(y=RandomCarb$PredVals , x=(RandomCarb$ActualVals) , col=rangi2 , ylim=c(min(RandomCarb$CI5),max(RandomCarb$CI95)) ,
     xlab="Observed Carbon Storage" , ylab="Predicted Carbon Storage",  main = 'Predicted Carbon Storage vs Observed Carbon Storage Loblolly/Short Leaf Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomCarb) ) lines( rep(RandomCarb$ActualVals[i],2) , RandCarbPI[,i] , col=rangi2 )
abline(Cline, col = "red")
summary(Cline) #r2 0.1542 
rmse(Carbline$ActualVals,Carbline$PredVals)#rmse 0.9198543


#repeat steps for forest group 20
Carbmu <- link(CarbSplit20)
Carbmu_mean <- apply(Carbmu , 2 , mean )
Carbmu_PI <- apply(Carbmu , 2 , PI )

Carblinkvals <-rbind(Carbmu_mean,Carbmu_PI)
Carblinkvals <- data.frame(t(Carblinkvals))
Carbline <- data.frame(cbind(FgroupSplitTrain[["20"]]$CarbonPerAcre,Carblinkvals[,1:3]))
names(Carbline) <- c("ActualVals","PredVals","CI5","CI95")
Carbline$ActualVals <- scale(Carbline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomCarb <- sample_n(Carbline, 200)
RandCarbPI <- RandomCarb[,3:4]
RandCarbPI <- t(RandCarbPI)

Cline <- lm(Carbline$PredVals~(Carbline$ActualVals), data = Carbline)
par(mar=c(5,6,4,2)+.1)
plot(y=RandomCarb$PredVals , x=(RandomCarb$ActualVals) , col=rangi2 , ylim=c(min(RandomCarb$CI5),max(RandomCarb$CI95)) ,
     xlab="Observed Carbon Storage" , ylab="Predicted Carbon Storage",  main = 'Predicted Carbon Storage vs Observed Carbon Storage Oak/Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomCarb) ) lines( rep(RandomCarb$ActualVals[i],2) , RandCarbPI[,i] , col=rangi2 )
abline(Cline, col = "red")
summary(Cline) #r2 0.2087 
rmse(Carbline$ActualVals,Carbline$PredVals) #rmse 0.8898229

#repeat steps for forest group 21
Carbmu <- link(CarbSplit21)
Carbmu_mean <- apply(Carbmu , 2 , mean )
Carbmu_PI <- apply(Carbmu , 2 , PI )

Carblinkvals <-rbind(Carbmu_mean,Carbmu_PI)
Carblinkvals <- data.frame(t(Carblinkvals))
Carbline <- data.frame(cbind(FgroupSplitTrain[["21"]]$CarbonPerAcre,Carblinkvals[,1:3]))
names(Carbline) <- c("ActualVals","PredVals","CI5","CI95")
Carbline$ActualVals <- scale(Carbline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomCarb <- sample_n(Carbline, 200)
RandCarbPI <- RandomCarb[,3:4]
RandCarbPI <- t(RandCarbPI)

Cline <- lm(Carbline$PredVals~(Carbline$ActualVals), data = Carbline)
par(mar=c(5,6,4,2)+.1)
plot(y=RandomCarb$PredVals , x=(RandomCarb$ActualVals) , col=rangi2 , ylim=c(min(RandomCarb$CI5),max(RandomCarb$CI95)) ,
     xlab="Observed Carbon Storage" , ylab="Predicted Carbon Storage",  main = 'Predicted Carbon Storage vs Observed Carbon Storage Oak/Hickory Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomCarb) ) lines( rep(RandomCarb$ActualVals[i],2) , RandCarbPI[,i] , col=rangi2 )
abline(Cline, col = "red")
summary(Cline) #r2  0.2915 
rmse(Carbline$ActualVals,Carbline$PredVals) #rmse 0.8416861

#repeat steps for forest group 23
Carbmu <- link(CarbSplit23)
Carbmu_mean <- apply(Carbmu , 2 , mean )
Carbmu_PI <- apply(Carbmu , 2 , PI )

Carblinkvals <-rbind(Carbmu_mean,Carbmu_PI)
Carblinkvals <- data.frame(t(Carblinkvals))
Carbline <- data.frame(cbind(FgroupSplitTrain[["23"]]$CarbonPerAcre,Carblinkvals[,1:3]))
names(Carbline) <- c("ActualVals","PredVals","CI5","CI95")
Carbline$ActualVals <- scale(Carbline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomCarb <- sample_n(Carbline, 200)
RandCarbPI <- RandomCarb[,3:4]
RandCarbPI <- t(RandCarbPI)

Cline <- lm(Carbline$PredVals~(Carbline$ActualVals), data = Carbline)
par(mar=c(5,6,4,2)+.1)
plot(y=RandomCarb$PredVals , x=(RandomCarb$ActualVals) , col=rangi2 , ylim=c(min(RandomCarb$CI5),max(RandomCarb$CI95)) ,
     xlab="Observed Carbon Storage" , ylab="Predicted Carbon Storage",  main = 'Predicted Carbon Storage vs Observed Carbon Storage Elm/Ash/Cottonwood Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomCarb) ) lines( rep(RandomCarb$ActualVals[i],2) , RandCarbPI[,i] , col=rangi2 )
abline(Cline, col = "red")
summary(Cline) #r2 0.1875 
rmse(Carbline$ActualVals,Carbline$PredVals) #rmse 0.9069733

#repeat steps for forest group 24
Carbmu <- link(CarbSplit24)
Carbmu_mean <- apply(Carbmu , 2 , mean )
Carbmu_PI <- apply(Carbmu , 2 , PI )

Carblinkvals <-rbind(Carbmu_mean,Carbmu_PI)
Carblinkvals <- data.frame(t(Carblinkvals))
Carbline <- data.frame(cbind(FgroupSplitTrain[["24"]]$CarbonPerAcre,Carblinkvals[,1:3]))
names(Carbline) <- c("ActualVals","PredVals","CI5","CI95")
Carbline$ActualVals <- scale(Carbline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomCarb <- sample_n(Carbline, 200)
RandCarbPI <- RandomCarb[,3:4]
RandCarbPI <- t(RandCarbPI)

Cline <- lm(Carbline$PredVals~(Carbline$ActualVals), data = Carbline)
par(mar=c(5,6,4,2)+.1)
plot(y=RandomCarb$PredVals , x=(RandomCarb$ActualVals) , col=rangi2 , ylim=c(min(RandomCarb$CI5),max(RandomCarb$CI95)) ,
     xlab="Observed Carbon Storage" , ylab="Predicted Carbon Storage",  main = 'Predicted Carbon Storage vs Observed Carbon Storage Maple/Beech/Birch Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomCarb) ) lines( rep(RandomCarb$ActualVals[i],2) , RandCarbPI[,i] , col=rangi2 )
abline(Cline, col = "red")
summary(Cline) #r2 0.2611 
rmse(Carbline$ActualVals,Carbline$PredVals) #0.8594082

#Hill Shannon- repeat same steps as Carbon storage model, except use a gamma distribution instead of normal
datHillSplit1 <- list(
  Hill = (as.numeric(FgroupSplitTrain[["1"]]$HillShannonIndex)),
  PPT = scale(as.numeric(FgroupSplitTrain[["1"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["1"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["1"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["1"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["1"]]$RAD)
)
datHillSplit5 <- list(
  Hill = (as.numeric(FgroupSplitTrain[["5"]]$HillShannonIndex)),
  PPT = scale(as.numeric(FgroupSplitTrain[["5"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["5"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["5"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["5"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["5"]]$RAD)
)

datHillSplit20 <- list(
  Hill = (as.numeric(FgroupSplitTrain[["20"]]$HillShannonIndex)),
  PPT = scale(as.numeric(FgroupSplitTrain[["20"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["20"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["20"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["20"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["20"]]$RAD)
)
datHillSplit21 <- list(
  Hill = (as.numeric(FgroupSplitTrain[["21"]]$HillShannonIndex)),
  PPT = scale(as.numeric(FgroupSplitTrain[["21"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["21"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["21"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["21"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["21"]]$RAD)
)
datHillSplit23 <- list(
  Hill = (as.numeric(FgroupSplitTrain[["23"]]$HillShannonIndex)),
  PPT = scale(as.numeric(FgroupSplitTrain[["23"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["23"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["23"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["23"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["23"]]$RAD)
)
datHillSplit24 <- list(
  Hill = (as.numeric(FgroupSplitTrain[["24"]]$HillShannonIndex)),
  PPT = scale(as.numeric(FgroupSplitTrain[["24"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["24"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["24"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["24"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["24"]]$RAD)
)

HillSplit1_G <- ulam(
  alist(
    Hill ~ dgamma2(mu, phi),
    log(mu) <- a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT + 
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +
      b_ftype5[FType]*RAD + a2[Soil] + a3[FAD],
    
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2),
    phi ~ dexp(1) 
  ),
  data = datHillSplit1, 
  log_lik = TRUE, 
  chains = 4, 
  cores = 8, 
  iter = 10000,
  control = list(adapt_delta = 0.95) 
)
#plot(precis(HillSplit1_G, depth=2),labels=F1Labels, main="Model Parameter Estimates, Hill Shannon’s Index White/Red/Jack Pine Group")
precisvaluesHill1 <- data.frame(precis(HillSplit1_G, depth=2))
write.csv(precisvaluesHill1, "precisValsHill1.csv")

HillSplit5_G <- ulam(
  alist(
    Hill ~ dgamma2(mu, phi),
    log(mu) <- a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT + 
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +
      b_ftype5[FType]*RAD + a2[Soil] + a3[FAD],
    
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2),
    phi ~ dexp(1) 
  ),
  data = datHillSplit5, 
  log_lik = TRUE, 
  chains = 4, 
  cores = 8, 
  iter = 10000,
  control = list(adapt_delta = 0.95) 
)
#plot(precis(HillSplit5_G, depth=2),labels=F5Labels, main="Model Parameter Estimates, Hill Shannon’s Index Loblolly/Short Leaf Pine Group")
precisvaluesHill5 <- data.frame(precis(HillSplit5_G, depth=2))
write.csv(precisvaluesHill5, "precisValsHill5.csv")

HillSplit20_G <- ulam(
  alist(
    Hill ~ dgamma2(mu, phi),
    log(mu) <- a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT + 
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +
      b_ftype5[FType]*RAD + a2[Soil] + a3[FAD],
    
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2),
    phi ~ dexp(1) 
  ),
  data = datHillSplit20, 
  log_lik = TRUE, 
  chains = 4, 
  cores = 8, 
  iter = 10000,
  control = list(adapt_delta = 0.95) 
)
#plot(precis(HillSplit20_G, depth=2),labels=F20Labels, main="Model Parameter Estimates, Hill Shannon’s Index Oak/Pine Group")
precisvaluesHill20 <- data.frame(precis(HillSplit20_G, depth=2))
write.csv(precisvaluesHill20, "precisValsHill20.csv")

HillSplit21_G <- ulam(
  alist(
    Hill ~ dgamma2(mu, phi),
    log(mu) <- a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT + 
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +
      b_ftype5[FType]*RAD + a2[Soil] + a3[FAD],
    
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2),
    phi ~ dexp(1) 
  ),
  data = datHillSplit21, 
  log_lik = TRUE, 
  chains = 4, 
  cores = 8, 
  iter = 10000,
  control = list(adapt_delta = 0.95) 
)
#plot(precis(HillSplit21_G, depth=2),labels=F21Labels, main="Model Parameter Estimates, Hill Shannon’s Index Oak/Hickory Group")
precisvaluesHill21 <- data.frame(precis(HillSplit21_G, depth=2))
write.csv(precisvaluesHill21, "precisValsHill21.csv")

HillSplit23_G <- ulam(
  alist(
    Hill ~ dgamma2(mu, phi),
    log(mu) <- a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT + 
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +
      b_ftype5[FType]*RAD + a2[Soil] + a3[FAD],
    
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2),
    phi ~ dexp(1) 
  ),
  data = datHillSplit23, 
  log_lik = TRUE, 
  chains = 4, 
  cores = 8, 
  iter = 10000,
  control = list(adapt_delta = 0.95) 
)
#plot(precis(HillSplit23_G, depth=2),labels=F23Labels, main="Model Parameter Estimates, Hill Shannon’s Index Elm/Ash/Cottonwood Group")
precisvaluesHill23 <- data.frame(precis(HillSplit23_G, depth=2))
write.csv(precisvaluesHill23, "precisValsHill23.csv")

HillSplit24_G <- ulam(
  alist(
    Hill ~ dgamma2(mu, phi),
    log(mu) <- a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT + 
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +
      b_ftype5[FType]*RAD + a2[Soil] + a3[FAD],
    
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2),
    phi ~ dexp(1) 
  ),
  data = datHillSplit24, 
  log_lik = TRUE, 
  chains = 4, 
  cores = 8, 
  iter = 10000,
  control = list(adapt_delta = 0.95) 
)
#plot(precis(HillSplit24_G, depth=2),labels=F24Labels, main="Model Parameter Estimates, Hill Shannon’s Index Maple/Beech/Birch Group")
precisvaluesHill24 <- data.frame(precis(HillSplit24_G, depth=2))
write.csv(precisvaluesHill24, "precisValsHill24.csv")

#posterior predictive checks, link to simulate model, then calculate mean point estimates and 95/5 confidence intervals, run one model at a time.
Hillmu <- link( HillSplit1_G)
Hillmu_mean <- apply( Hillmu , 2 , mean )
Hillmu_GI <- apply( Hillmu , 2 , PI )

Hilllinkvals <-rbind(Hillmu_mean,Hillmu_GI)
Hilllinkvals <- data.frame(t(Hilllinkvals))
Hillline <- data.frame(cbind(FgroupSplitTrain[["1"]]$HillShannonIndex,Hilllinkvals[,1:3]))
names(Hillline) <- c("ActualVals","PredVals","CI5","CI95")

#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomHill <- sample_n(Hillline, 200)
RandHillPI <- RandomHill[,3:4]
RandHillPI <- t(RandHillPI)

Hline <- lm(Hillline$PredVals~(Hillline$ActualVals), data = Hillline)
par(mar=c(5,6,4,6)+.1)
plot(y=RandomHill$PredVals , x=(RandomHill$ActualVals) , col=rangi2 , ylim=c(min(RandomHill$CI5),max(RandomHill$CI95)) ,
     xlab="Observed Hill Shannon’s Index " , ylab="Predicted Hill Shannon’s Index ",  main = 'Predicted Hill Shannon’s Index vs Observed Hill Shannon’s Index White/Red/Jack Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomHill) ) lines( rep(RandomHill$ActualVals[i],2) , RandHillPI[,i] , col=rangi2 )
abline(Hline, col = "red")
summary(Hline)
rmse(Hillline$ActualVals,Hillline$PredVals) #RMSE=1.183406

Hillmu <- link( HillSplit5_G)
Hillmu_mean <- apply( Hillmu , 2 , mean )
Hillmu_GI <- apply( Hillmu , 2 , PI )

Hilllinkvals <-rbind(Hillmu_mean,Hillmu_GI)
Hilllinkvals <- data.frame(t(Hilllinkvals))
Hillline <- data.frame(cbind(FgroupSplitTrain[["5"]]$HillShannonIndex,Hilllinkvals[,1:3]))
names(Hillline) <- c("ActualVals","PredVals","CI5","CI95")

#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomHill <- sample_n(Hillline, 200)
RandHillPI <- RandomHill[,3:4]
RandHillPI <- t(RandHillPI)

Hline <- lm(Hillline$PredVals~(Hillline$ActualVals), data = Hillline)
par(mar=c(5,6,4,2)+.1)
plot(y=RandomHill$PredVals , x=(RandomHill$ActualVals) , col=rangi2 , ylim=c(min(RandomHill$CI5),max(RandomHill$CI95)) ,
     xlab="Observed Hill Shannon’s Index" , ylab="Predicted Hill Shannon’s Index",  main = 'Predicted Hill Shannon’s Index vs Observed Hill Shannon’s Index Loblolly/Short Leaf Pine Group',
     cex.main=1.8,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomHill) ) lines( rep(RandomHill$ActualVals[i],2) , RandHillPI[,i] , col=rangi2 )
abline(Hline, col = "red")
summary(Hline)
rmse(Hillline$ActualVals,Hillline$PredVals)

Hillmu <- link( HillSplit20_G)
Hillmu_mean <- apply( Hillmu , 2 , mean )
Hillmu_GI <- apply( Hillmu , 2 , PI )

Hilllinkvals <-rbind(Hillmu_mean,Hillmu_GI)
Hilllinkvals <- data.frame(t(Hilllinkvals))
Hillline <- data.frame(cbind(FgroupSplitTrain[["20"]]$HillShannonIndex,Hilllinkvals[,1:3]))
names(Hillline) <- c("ActualVals","PredVals","CI5","CI95")

#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomHill <- sample_n(Hillline, 200)
RandHillPI <- RandomHill[,3:4]
RandHillPI <- t(RandHillPI)

Hline <- lm(Hillline$PredVals~(Hillline$ActualVals), data = Hillline)
par(mar=c(5,6,4,2)+.1)
plot(y=RandomHill$PredVals , x=(RandomHill$ActualVals) , col=rangi2 , ylim=c(min(RandomHill$CI5),max(RandomHill$CI95)) ,
     xlab="Observed Hill Shannon’s Index" , ylab="Predicted Hill Shannon’s Index",  main = 'Predicted Hill Shannon’s Index vs Observed Hill Shannon’s Index Oak/Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomHill) ) lines( rep(RandomHill$ActualVals[i],2) , RandHillPI[,i] , col=rangi2 )
abline(Hline, col = "red")
summary(Hline)
rmse(Hillline$ActualVals,Hillline$PredVals)

Hillmu <- link( HillSplit21_G)
Hillmu_mean <- apply( Hillmu , 2 , mean )
Hillmu_GI <- apply( Hillmu , 2 , PI )

Hilllinkvals <-rbind(Hillmu_mean,Hillmu_GI)
Hilllinkvals <- data.frame(t(Hilllinkvals))
Hillline <- data.frame(cbind(FgroupSplitTrain[["21"]]$HillShannonIndex,Hilllinkvals[,1:3]))
names(Hillline) <- c("ActualVals","PredVals","CI5","CI95")

#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomHill <- sample_n(Hillline, 200)
RandHillPI <- RandomHill[,3:4]
RandHillPI <- t(RandHillPI)

Hline <- lm(Hillline$PredVals~(Hillline$ActualVals), data = Hillline)
par(mar=c(5,6,4,2)+.1)
plot(y=RandomHill$PredVals , x=(RandomHill$ActualVals) , col=rangi2 , ylim=c(min(RandomHill$CI5),max(RandomHill$CI95)) ,
     xlab="Observed Hill Shannon’s Index" , ylab="Predicted Hill Shannon’s Index",  main = 'Predicted Hill Shannon’s Index vs Observed Hill Shannon’s Index Oak/Hickory Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomHill) ) lines( rep(RandomHill$ActualVals[i],2) , RandHillPI[,i] , col=rangi2 )
abline(Hline, col = "red")
summary(Hline)
rmse(Hillline$ActualVals,Hillline$PredVals)

Hillmu <- link( HillSplit23_G)
Hillmu_mean <- apply( Hillmu , 2 , mean )
Hillmu_GI <- apply( Hillmu , 2 , PI )

Hilllinkvals <-rbind(Hillmu_mean,Hillmu_GI)
Hilllinkvals <- data.frame(t(Hilllinkvals))
Hillline <- data.frame(cbind(FgroupSplitTrain[["23"]]$HillShannonIndex,Hilllinkvals[,1:3]))
names(Hillline) <- c("ActualVals","PredVals","CI5","CI95")

#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomHill <- sample_n(Hillline, 200)
RandHillPI <- RandomHill[,3:4]
RandHillPI <- t(RandHillPI)

Hline <- lm(Hillline$PredVals~(Hillline$ActualVals), data = Hillline)
par(mar=c(5,6,4,6)+.1)
plot(y=RandomHill$PredVals , x=(RandomHill$ActualVals) , col=rangi2 , ylim=c(min(RandomHill$CI5),max(RandomHill$CI95)) ,
     xlab="Observed Hill Shannon’s Index" , ylab="Predicted Hill Shannon’s Index",  main = 'Predicted Hill Shannon’s Index vs Observed Hill Shannon’s Index Elm/Ash/Cottonwood Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomHill) ) lines( rep(RandomHill$ActualVals[i],2) , RandHillPI[,i] , col=rangi2 )
abline(Hline, col = "red")
summary(Hline)
rmse(Hillline$ActualVals,Hillline$PredVals)

Hillmu <- link( HillSplit24_G)
Hillmu_mean <- apply( Hillmu , 2 , mean )
Hillmu_GI <- apply( Hillmu , 2 , PI )

Hilllinkvals <-rbind(Hillmu_mean,Hillmu_GI)
Hilllinkvals <- data.frame(t(Hilllinkvals))
Hillline <- data.frame(cbind(FgroupSplitTrain[["24"]]$HillShannonIndex,Hilllinkvals[,1:3]))
names(Hillline) <- c("ActualVals","PredVals","CI5","CI95")

#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomHill <- sample_n(Hillline, 200)
RandHillPI <- RandomHill[,3:4]
RandHillPI <- t(RandHillPI)

Hline <- lm(Hillline$PredVals~(Hillline$ActualVals), data = Hillline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomHill$PredVals , x=(RandomHill$ActualVals) , col=rangi2 , ylim=c(min(RandomHill$CI5),max(RandomHill$CI95)) ,
     xlab="Observed Hill Shannon’s Index" , ylab="Predicted Hill Shannon’s Index",  main = 'Predicted Hill Shannon’s Index vs Observed Hill Shannon’s Index Maple/Beech/Birch Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomHill) ) lines( rep(RandomHill$ActualVals[i],2) , RandHillPI[,i] , col=rangi2 )
abline(Hline, col = "red")
summary(Hline)
rmse(Hillline$ActualVals,Hillline$PredVals)

#Jaccards Similarity Index Model- repeat same steps as above model. Beta distribution instead of normal
datJaccSplit1 <- list(
  Jacc = (as.numeric(FgroupSplitTrain[["1"]]$JaccardMean)),
  PPT = scale(as.numeric(FgroupSplitTrain[["1"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["1"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["1"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["1"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["1"]]$RAD) 
)
datJaccSplit5 <- list(
  Jacc = (as.numeric(FgroupSplitTrain[["5"]]$JaccardMean)),
  PPT = scale(as.numeric(FgroupSplitTrain[["5"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["5"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["5"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["5"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["5"]]$RAD) 
)

datJaccSplit20 <- list(
  Jacc = (as.numeric(FgroupSplitTrain[["20"]]$JaccardMean)),
  PPT = scale(as.numeric(FgroupSplitTrain[["20"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["20"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["20"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["20"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["20"]]$RAD)
)
datJaccSplit21 <- list(
  Jacc = (as.numeric(FgroupSplitTrain[["21"]]$JaccardMean)),
  PPT = scale(as.numeric(FgroupSplitTrain[["21"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["21"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["21"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["21"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["21"]]$RAD)
)
datJaccSplit23 <- list(
  Jacc = (as.numeric(FgroupSplitTrain[["23"]]$JaccardMean)),
  PPT = scale(as.numeric(FgroupSplitTrain[["23"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["23"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["23"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["23"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["23"]]$RAD) 
)
datJaccSplit24 <- list(
  Jacc = (as.numeric(FgroupSplitTrain[["24"]]$JaccardMean)),
  PPT = scale(as.numeric(FgroupSplitTrain[["24"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["24"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["24"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["24"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["24"]]$RAD)
)

JaccSplit1_beta <- ulam(
  alist(
    Jacc ~ dbeta2(mu, phi),
    logit(mu) <- a + a_ftype[FType] + 
      b_ftype1[FType]*MAT + 
      b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + 
      b_ftype4[FType]*RHUM +
      b_ftype5[FType]*RAD + 
      a2[Soil] + 
      a3[FAD],
    
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 1),
    z ~ dnorm(0, 1),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ), data = datJaccSplit1, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(JaccSplit1_beta, depth=2),labels=F1Labels,main="Model Parameter Estimates, Jaccards Similarity Index White/Red/Jack Pine Group")
precisvaluesJacc1 <- data.frame(precis(JaccSplit1_beta, depth=2))
write.csv(precisvaluesJacc1, "precisValsJacc1.csv")

JaccSplit5_beta <- ulam(
  alist(
    Jacc ~ dbeta2(mu, phi),
    logit(mu) <- a + a_ftype[FType] + 
      b_ftype1[FType]*MAT + 
      b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + 
      b_ftype4[FType]*RHUM +
      b_ftype5[FType]*RAD + 
      a2[Soil] + 
      a3[FAD],
    
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 1),
    z ~ dnorm(0, 1),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ), data = datJaccSplit5, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(JaccSplit5_beta, depth=2),labels=F5Labels,main="Model Parameter Estimates, Jaccards Similarity Index Loblolly/Short Leaf Pine Group")
precisvaluesJacc5 <- data.frame(precis(JaccSplit5_beta, depth=2))
write.csv(precisvaluesJacc5, "precisValsJacc5.csv")

JaccSplit20_beta <- ulam(
  alist(
    Jacc ~ dbeta2(mu, phi),
    logit(mu) <- a + a_ftype[FType] + 
      b_ftype1[FType]*MAT + 
      b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + 
      b_ftype4[FType]*RHUM +
      b_ftype5[FType]*RAD + 
      a2[Soil] + 
      a3[FAD],
  
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 1),
    z ~ dnorm(0, 1),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ), data = datJaccSplit20, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(JaccSplit20_beta, depth=2),labels=F20Labels,main="Model Parameter Estimates, Jaccards Similarity Index Oak/Pine Group")
precisvaluesJacc20 <- data.frame(precis(JaccSplit20_beta, depth=2))
write.csv(precisvaluesJacc20, "precisValsJacc20.csv")

JaccSplit21_beta <- ulam(
  alist(
    Jacc ~ dbeta2(mu, phi),
    logit(mu) <- a + a_ftype[FType] + 
      b_ftype1[FType]*MAT + 
      b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + 
      b_ftype4[FType]*RHUM +
      b_ftype5[FType]*RAD + 
      a2[Soil] + 
      a3[FAD],
    
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 1),
    z ~ dnorm(0, 1),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ), data = datJaccSplit21, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(JaccSplit21_beta, depth=2),labels=F21Labels,main="Model Parameter Estimates, Jaccards Similarity Index Oak/Hickory Group")
precisvaluesJacc21 <- data.frame(precis(JaccSplit21_beta, depth=2))
write.csv(precisvaluesJacc21, "precisValsJacc21.csv")

JaccSplit23_beta <- ulam(
  alist(
    Jacc ~ dbeta2(mu, phi),
    logit(mu) <- a + a_ftype[FType] + 
      b_ftype1[FType]*MAT + 
      b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + 
      b_ftype4[FType]*RHUM +
      b_ftype5[FType]*RAD + 
      a2[Soil] + 
      a3[FAD],
    
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 1),
    z ~ dnorm(0, 1),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ), data = datJaccSplit23, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(JaccSplit23_beta, depth=2),labels=F23Labels,main="Model Parameter Estimates, Jaccards Similarity Index Elm/Ash/Cottonwood Group")
precisvaluesJacc23 <- data.frame(precis(JaccSplit23_beta, depth=2))
write.csv(precisvaluesJacc23, "precisValsJacc23.csv")

JaccSplit24_beta <- ulam(
  alist(
    Jacc ~ dbeta2(mu, phi),
    logit(mu) <- (a + a_ftype[FType] + 
                    b_ftype1[FType]*MAT + 
                    b_ftype2[FType]*PPT +
                    b_ftype3[FType]*Elevation + 
                    b_ftype4[FType]*RHUM +
                    b_ftype5[FType]*RAD + 
                    a2[Soil] + 
                    a3[FAD]),
    
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType),
    
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 1),
    z ~ dnorm(0, 1),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ), data = datJaccSplit24, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(JaccSplit24_beta, depth=2),labels=F24Labels,main="Model Parameter Estimates, Jaccards Similarity Index Maple/Beech/Birch Group")
precisvaluesJacc24 <- data.frame(precis(JaccSplit24_beta, depth=2))
write.csv(precisvaluesJacc24, "precisValsJacc24.csv")

#posterior predictive checks, link to simulate model, then calculate mean point estimates and 95/5 confidence intervals, run one model at a time.
Jaccmu <- link( JaccSplit1_beta)
Jaccmu_mean <- apply( Jaccmu , 2 , mean )
Jaccmu_PI <- apply( Jaccmu , 2 , PI )

Jacclinkvals <-rbind(Jaccmu_mean,Jaccmu_PI)
Jacclinkvals <- data.frame(t(Jacclinkvals))
Jaccline <- data.frame(cbind(FgroupSplitTrain[["1"]]$JaccardMean,Jacclinkvals[,1:3]))
names(Jaccline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomJacc <- sample_n(Jaccline, 200)
RandJaccPI <- RandomJacc[,3:4]
RandJaccPI <- t(RandJaccPI)

Jline <- lm(Jaccline$PredVals~(Jaccline$ActualVals), data = Jaccline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomJacc$PredVals , x=(RandomJacc$ActualVals) , col=rangi2 , ylim=c(min(RandomJacc$CI5),max(RandomJacc$CI95)) ,
     xlab="Observed Jaccards Similarity Index" , ylab="Predicted Jaccards Similarity Index",  main = 'Predicted Jaccards Similarity Index vs Observed Jaccards Similarity Index White/Red/Jack Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomJacc) ) lines( rep(RandomJacc$ActualVals[i],2) , RandJaccPI[,i] , col=rangi2 )
abline(Jline, col = "red")
summary(Jline) #R2 0.2208 
rmse(Jaccline$ActualVals,Jaccline$PredVals)#RMSE = 0.04496423

Jaccmu <- link( JaccSplit5_beta)
Jaccmu_mean <- apply( Jaccmu , 2 , mean )
Jaccmu_PI <- apply( Jaccmu , 2 , PI )

Jacclinkvals <-rbind(Jaccmu_mean,Jaccmu_PI)
Jacclinkvals <- data.frame(t(Jacclinkvals))
Jaccline <- data.frame(cbind(FgroupSplitTrain[["5"]]$JaccardMean,Jacclinkvals[,1:3]))
names(Jaccline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomJacc <- sample_n(Jaccline, 200)
RandJaccPI <- RandomJacc[,3:4]
RandJaccPI <- t(RandJaccPI)

Jline <- lm(Jaccline$PredVals~(Jaccline$ActualVals), data = Jaccline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomJacc$PredVals , x=(RandomJacc$ActualVals) , col=rangi2 , ylim=c(min(RandomJacc$CI5),max(RandomJacc$CI95)) ,
     xlab="Observed Jaccards Similarity Index" , ylab="Predicted Jaccards Similarity Index",  main = 'Predicted Jaccards Similarity Index vs Observed Jaccards Similarity Index Loblolly/Short Leaf Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomJacc) ) lines( rep(RandomJacc$ActualVals[i],2) , RandJaccPI[,i] , col=rangi2 )
abline(Jline, col = "red")
summary(Jline) #r2 0.2589 
rmse(Jaccline$ActualVals,Jaccline$PredVals) #RMSE = 0.03635427


Jaccmu <- link( JaccSplit20_beta)
Jaccmu_mean <- apply( Jaccmu , 2 , mean )
Jaccmu_PI <- apply( Jaccmu , 2 , PI )

Jacclinkvals <-rbind(Jaccmu_mean,Jaccmu_PI)
Jacclinkvals <- data.frame(t(Jacclinkvals))
Jaccline <- data.frame(cbind(FgroupSplitTrain[["20"]]$JaccardMean,Jacclinkvals[,1:3]))
names(Jaccline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomJacc <- sample_n(Jaccline, 200)
RandJaccPI <- RandomJacc[,3:4]
RandJaccPI <- t(RandJaccPI)

Jline <- lm(Jaccline$PredVals~(Jaccline$ActualVals), data = Jaccline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomJacc$PredVals , x=(RandomJacc$ActualVals) , col=rangi2 , ylim=c(min(RandomJacc$CI5),max(RandomJacc$CI95)) ,
     xlab="Observed Jaccards Similarity Index" , ylab="Predicted Jaccards Similarity Index",  main = 'Predicted Jaccards Similarity Index vs Observed Jaccards Similarity Index Oak/Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomJacc) ) lines( rep(RandomJacc$ActualVals[i],2) , RandJaccPI[,i] , col=rangi2 )
abline(Jline, col = "red")
summary(Jline) #R2 0.4838 
rmse(Jaccline$ActualVals,Jaccline$PredVals) #RMSE = 0.0359775

Jaccmu <- link( JaccSplit21_beta)
Jaccmu_mean <- apply( Jaccmu , 2 , mean )
Jaccmu_PI <- apply( Jaccmu , 2 , PI )

Jacclinkvals <-rbind(Jaccmu_mean,Jaccmu_PI)
Jacclinkvals <- data.frame(t(Jacclinkvals))
Jaccline <- data.frame(cbind(FgroupSplitTrain[["21"]]$JaccardMean,Jacclinkvals[,1:3]))
names(Jaccline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomJacc <- sample_n(Jaccline, 200)
RandJaccPI <- RandomJacc[,3:4]
RandJaccPI <- t(RandJaccPI)

Jline <- lm(Jaccline$PredVals~(Jaccline$ActualVals), data = Jaccline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomJacc$PredVals , x=(RandomJacc$ActualVals) , col=rangi2 , ylim=c(min(RandomJacc$CI5),max(RandomJacc$CI95)) ,
     xlab="Observed Jaccards Similarity Index" , ylab="Predicted Jaccards Similarity Index",  main = 'Predicted Jaccards Similarity Index vs Observed Jaccards Similarity Index Oak/Hickory Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomJacc) ) lines( rep(RandomJacc$ActualVals[i],2) , RandJaccPI[,i] , col=rangi2 )
abline(Jline, col = "red")
summary(Jline) #R2  0.3186 
rmse(Jaccline$ActualVals,Jaccline$PredVals) #RMSE = 0.03553173


Jaccmu <- link( JaccSplit23_beta)
Jaccmu_mean <- apply( Jaccmu , 2 , mean )
Jaccmu_PI <- apply( Jaccmu , 2 , PI )

Jacclinkvals <-rbind(Jaccmu_mean,Jaccmu_PI)
Jacclinkvals <- data.frame(t(Jacclinkvals))
Jaccline <- data.frame(cbind(FgroupSplitTrain[["23"]]$JaccardMean,Jacclinkvals[,1:3]))
names(Jaccline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomJacc <- sample_n(Jaccline, 200)
RandJaccPI <- RandomJacc[,3:4]
RandJaccPI <- t(RandJaccPI)

Jline <- lm(Jaccline$PredVals~(Jaccline$ActualVals), data = Jaccline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomJacc$PredVals , x=(RandomJacc$ActualVals) , col=rangi2 , ylim=c(min(RandomJacc$CI5),max(RandomJacc$CI95)) ,
     xlab="Observed Jaccards Similarity Index" , ylab="Predicted Jaccards Similarity Index",  main = 'Predicted Jaccards Similarity Index vs Observed Jaccards Similarity Index Elm/Ash/Cottonwood Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomJacc) ) lines( rep(RandomJacc$ActualVals[i],2) , RandJaccPI[,i] , col=rangi2 )
abline(Jline, col = "red")
summary(Jline) #r2 0.4992 
rmse(Jaccline$ActualVals,Jaccline$PredVals) #RMSE =0.0375488


Jaccmu <- link( JaccSplit24_beta)
Jaccmu_mean <- apply( Jaccmu , 2 , mean )
Jaccmu_PI <- apply( Jaccmu , 2 , PI )

Jacclinkvals <-rbind(Jaccmu_mean,Jaccmu_PI)
Jacclinkvals <- data.frame(t(Jacclinkvals))
Jaccline <- data.frame(cbind(FgroupSplitTrain[["24"]]$JaccardMean,Jacclinkvals[,1:3]))
names(Jaccline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomJacc <- sample_n(Jaccline, 200)
RandJaccPI <- RandomJacc[,3:4]
RandJaccPI <- t(RandJaccPI)

Jline <- lm(Jaccline$PredVals~(Jaccline$ActualVals), data = Jaccline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomJacc$PredVals , x=(RandomJacc$ActualVals) , col=rangi2 , ylim=c(min(RandomJacc$CI5),max(RandomJacc$CI95)) ,
     xlab="Observed Jaccards Similarity Index" , ylab="Predicted Jaccards Similarity Index",  main = 'Predicted Jaccards Similarity Index vs Observed Jaccards Similarity Index Maple/Beech/Birch Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomJacc) ) lines( rep(RandomJacc$ActualVals[i],2) , RandJaccPI[,i] , col=rangi2 )
abline(Jline, col = "red")
summary(Jline) #R2 0.2521 
rmse(Jaccline$ActualVals,Jaccline$PredVals) #RMSE = 0.04074528

#Mean Pairwise Distance
datMPDSplit1 <- list(
  MPD = scale(as.numeric(FgroupSplitTrain[["1"]]$MeanPairwiseDistance)),
  PPT = scale(as.numeric(FgroupSplitTrain[["1"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["1"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["1"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["1"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["1"]]$RAD)
)
datMPDSplit5 <- list(
  MPD = scale(as.numeric(FgroupSplitTrain[["5"]]$MeanPairwiseDistance)),
  PPT = scale(as.numeric(FgroupSplitTrain[["5"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["5"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["5"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["5"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["5"]]$RAD)
)

datMPDSplit20 <- list(
  MPD = scale(as.numeric(FgroupSplitTrain[["20"]]$MeanPairwiseDistance)),
  PPT = scale(as.numeric(FgroupSplitTrain[["20"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["20"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["20"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["20"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["20"]]$RAD)
)
datMPDSplit21 <- list(
  MPD = scale(as.numeric(FgroupSplitTrain[["21"]]$MeanPairwiseDistance)),
  PPT = scale(as.numeric(FgroupSplitTrain[["21"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["21"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["21"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["21"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["21"]]$RAD)
)
datMPDSplit23 <- list(
  MPD = scale(as.numeric(FgroupSplitTrain[["23"]]$MeanPairwiseDistance)),
  PPT = scale(as.numeric(FgroupSplitTrain[["23"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["23"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["23"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["23"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["23"]]$RAD)
)
datMPDSplit24 <- list(
  MPD = scale(as.numeric(FgroupSplitTrain[["24"]]$MeanPairwiseDistance)),
  PPT = scale(as.numeric(FgroupSplitTrain[["24"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["24"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["24"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["24"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["24"]]$RAD)
)
#scale and center for out of sample test
centerMPD1 <- attr(datMPDSplit1$MPD, "scaled:center")
scaleMPD1 <- attr(datMPDSplit1$MPD, "scaled:scale")
centerMPD5 <- attr(datMPDSplit5$MPD, "scaled:center")
scaleMPD5 <- attr(datMPDSplit5$MPD, "scaled:scale")
centerMPD20 <- attr(datMPDSplit20$MPD, "scaled:center")
scaleMPD20 <- attr(datMPDSplit20$MPD, "scaled:scale")
centerMPD21 <- attr(datMPDSplit21$MPD, "scaled:center")
scaleMPD21 <- attr(datMPDSplit21$MPD, "scaled:scale")
centerMPD23 <- attr(datMPDSplit23$MPD, "scaled:center")
scaleMPD23 <- attr(datMPDSplit23$MPD, "scaled:scale")
centerMPD24 <- attr(datMPDSplit24$MPD, "scaled:center")
scaleMPD24 <- attr(datMPDSplit24$MPD, "scaled:scale")

MPDSplit1 <- ulam(
  alist(
    MPD ~ dnorm(mu,sigma),
    mu <- a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD],
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datMPDSplit1, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#plot(precis(MPDSplit1, depth=2),labels=F1Labels,main="Model Parameter Estimates, Mean Pairwise Distance White/Red/Jack Pine Group")
precisvaluesMPD1 <- data.frame(precis(MPDSplit1, depth=2))
write.csv(precisvaluesMPD1, "precisValsMPD1.csv")

MPDSplit5 <- ulam(
  alist(
    MPD ~ dnorm(mu,sigma),
    mu <- a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD],
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datMPDSplit5, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#plot(precis(MPDSplit5, depth=2),labels=F5Labels,main="Model Parameter Estimates, Mean Pairwise Distance Loblolly/Short Leaf Pine Group")
precisvaluesMPD5 <- data.frame(precis(MPDSplit5, depth=2))
write.csv(precisvaluesMPD5, "precisValsMPD5.csv")

MPDSplit20 <- ulam(
  alist(
    MPD ~ dnorm(mu,sigma),
    mu <- a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD],
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datMPDSplit20, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#plot(precis(MPDSplit20, depth=2),labels=F20Labels,main="Model Parameter Estimates, Mean Pairwise Distance Oak/Pine Group")
precisvaluesMPD20 <- data.frame(precis(MPDSplit20, depth=2))
write.csv(precisvaluesMPD20, "precisValsMPD20.csv")

MPDSplit21 <- ulam(
  alist(
    MPD ~ dnorm(mu,sigma),
    mu <- a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD],
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datMPDSplit21, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#plot(precis(MPDSplit21, depth=2),labels=F21Labels,cex.lab=0.7,main="Model Parameter Estimates, Mean Pairwise Distance Oak/Hickory Group")
precisvaluesMPD21 <- data.frame(precis(MPDSplit21, depth=2))
write.csv(precisvaluesMPD21, "precisValsMPD21.csv")

MPDSplit23 <- ulam(
  alist(
    MPD ~ dnorm(mu,sigma),
    mu <- a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD],
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datMPDSplit23, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#plot(precis(MPDSplit23, depth=2),labels=F23Labels,cex.lab=0.7,main="Model Parameter Estimates, Mean Pairwise Distance Elm/Ash/Cottonwood Group")
precisvaluesMPD23 <- data.frame(precis(MPDSplit23, depth=2))
write.csv(precisvaluesMPD23, "precisValsMPD23.csv")

MPDSplit24 <- ulam(
  alist(
    MPD ~ dnorm(mu,sigma),
    mu <- a+a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
      b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM +b_ftype5[FType]*RAD+ a2[Soil] + a3[FAD],
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ multi_normal( c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x,sigma_soil),
    a3[FAD] ~ dnorm(z,sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dcauchy(0,2),
    sigma_soil ~ dcauchy(0,2),
    sigma_fad ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ), data = datMPDSplit24, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#plot(precis(MPDSplit24, depth=2),labels=F24Labels,cex.lab=0.7,main="Model Parameter Estimates, Mean Pairwise Distance Maple/Beech/Birch Group")
precisvaluesMPD24 <- data.frame(precis(MPDSplit24, depth=2))
write.csv(precisvaluesMPD24, "precisValsMPD24.csv")

#posterior predictive checks, link to simulate model, then calculate mean point estimates and 95/5 confidence intervals, run one model at a time.
MPDmu <- link( MPDSplit1)
MPDmu_mean <- apply( MPDmu , 2 , mean )
MPDmu_PI <- apply( MPDmu , 2 , PI )

MPDlinkvals <-rbind(MPDmu_mean,MPDmu_PI)
MPDlinkvals <- data.frame(t(MPDlinkvals))
MPDline <- data.frame(cbind(FgroupSplitTrain[["1"]]$MeanPairwiseDistance,MPDlinkvals[,1:3]))
names(MPDline) <- c("ActualVals","PredVals","CI5","CI95")
MPDline$ActualVals <- scale(MPDline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomMPD <- sample_n(MPDline, 200)
RandMPDPI <- RandomMPD[,3:4]
RandMPDPI <- t(RandMPDPI)

MPline <- lm(MPDline$PredVals~(MPDline$ActualVals), data = MPDline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMPD$PredVals , x=(RandomMPD$ActualVals) , col=rangi2 , ylim=c(min(RandomMPD$CI5),max(RandomMPD$CI95)) ,
     xlab="Observed Mean Pairwise Distance" , ylab="Predicted Mean Pairwise Distance",  main = 'Predicted Mean Pairwise Distance vs Observed Mean Pairwise Distance White/Red/Jack Pine Group',
     cex.main=1.6,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMPD) ) lines( rep(RandomMPD$ActualVals[i],2) , RandMPDPI[,i] , col=rangi2 )
abline(MPline, col = "red")
summary(MPline) 
rmse(MPDline$ActualVals,MPDline$PredVals)


MPDmu <- link( MPDSplit5)
MPDmu_mean <- apply( MPDmu , 2 , mean )
MPDmu_PI <- apply( MPDmu , 2 , PI )

MPDlinkvals <-rbind(MPDmu_mean,MPDmu_PI)
MPDlinkvals <- data.frame(t(MPDlinkvals))
MPDline <- data.frame(cbind(FgroupSplitTrain[["5"]]$MeanPairwiseDistance,MPDlinkvals[,1:3]))
names(MPDline) <- c("ActualVals","PredVals","CI5","CI95")
MPDline$ActualVals <- scale(MPDline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomMPD <- sample_n(MPDline, 200)
RandMPDPI <- RandomMPD[,3:4]
RandMPDPI <- t(RandMPDPI)

MPline <- lm(MPDline$PredVals~(MPDline$ActualVals), data = MPDline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMPD$PredVals , x=(RandomMPD$ActualVals) , col=rangi2 , ylim=c(min(RandomMPD$CI5),max(RandomMPD$CI95)) ,
     xlab="Observed Mean Pairwise Distance" , ylab="Predicted Mean Pairwise Distance",  main = 'Predicted Mean Pairwise Distance vs Observed Mean Pairwise Distance Loblolly/Short Leaf Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMPD) ) lines( rep(RandomMPD$ActualVals[i],2) , RandMPDPI[,i] , col=rangi2 )
abline(MPline, col = "red")
summary(MPline)
rmse(MPDline$ActualVals,MPDline$PredVals)


MPDmu <- link( MPDSplit20)
MPDmu_mean <- apply( MPDmu , 2 , mean )
MPDmu_PI <- apply( MPDmu , 2 , PI )

MPDlinkvals <-rbind(MPDmu_mean,MPDmu_PI)
MPDlinkvals <- data.frame(t(MPDlinkvals))
MPDline <- data.frame(cbind(FgroupSplitTrain[["20"]]$MeanPairwiseDistance,MPDlinkvals[,1:3]))
names(MPDline) <- c("ActualVals","PredVals","CI5","CI95")
MPDline$ActualVals <- scale(MPDline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomMPD <- sample_n(MPDline, 200)
RandMPDPI <- RandomMPD[,3:4]
RandMPDPI <- t(RandMPDPI)

MPline <- lm(MPDline$PredVals~(MPDline$ActualVals), data = MPDline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMPD$PredVals , x=(RandomMPD$ActualVals) , col=rangi2 , ylim=c(min(RandomMPD$CI5),max(RandomMPD$CI95)) ,
     xlab="Observed Mean Pairwise Distance" , ylab="Predicted Mean Pairwise Distance",  main = 'Predicted Mean Pairwise Distance vs Observed Mean Pairwise Distance Oak/Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMPD) ) lines( rep(RandomMPD$ActualVals[i],2) , RandMPDPI[,i] , col=rangi2 )
abline(MPline, col = "red")
summary(MPline)
rmse(MPDline$ActualVals,MPDline$PredVals)

MPDmu <- link( MPDSplit21)
MPDmu_mean <- apply( MPDmu , 2 , mean )
MPDmu_PI <- apply( MPDmu , 2 , PI )

MPDlinkvals <-rbind(MPDmu_mean,MPDmu_PI)
MPDlinkvals <- data.frame(t(MPDlinkvals))
MPDline <- data.frame(cbind(FgroupSplitTrain[["21"]]$MeanPairwiseDistance,MPDlinkvals[,1:3]))
names(MPDline) <- c("ActualVals","PredVals","CI5","CI95")
MPDline$ActualVals <- scale(MPDline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomMPD <- sample_n(MPDline, 200)
RandMPDPI <- RandomMPD[,3:4]
RandMPDPI <- t(RandMPDPI)

MPline <- lm(MPDline$PredVals~(MPDline$ActualVals), data = MPDline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMPD$PredVals , x=(RandomMPD$ActualVals) , col=rangi2 , ylim=c(min(RandomMPD$CI5),max(RandomMPD$CI95)) ,
     xlab="Observed Mean Pairwise Distance" , ylab="Predicted Mean Pairwise Distance",  main = 'Predicted Mean Pairwise Distance vs Observed Mean Pairwise Distance Oak/Hickory Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMPD) ) lines( rep(RandomMPD$ActualVals[i],2) , RandMPDPI[,i] , col=rangi2 )
abline(MPline, col = "red")
summary(MPline)
rmse(MPDline$ActualVals,MPDline$PredVals)

MPDmu <- link( MPDSplit23)
MPDmu_mean <- apply( MPDmu , 2 , mean )
MPDmu_PI <- apply( MPDmu , 2 , PI )

MPDlinkvals <-rbind(MPDmu_mean,MPDmu_PI)
MPDlinkvals <- data.frame(t(MPDlinkvals))
MPDline <- data.frame(cbind(FgroupSplitTrain[["23"]]$MeanPairwiseDistance,MPDlinkvals[,1:3]))
names(MPDline) <- c("ActualVals","PredVals","CI5","CI95")
MPDline$ActualVals <- scale(MPDline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomMPD <- sample_n(MPDline, 200)
RandMPDPI <- RandomMPD[,3:4]
RandMPDPI <- t(RandMPDPI)

MPline <- lm(MPDline$PredVals~(MPDline$ActualVals), data = MPDline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMPD$PredVals , x=(RandomMPD$ActualVals) , col=rangi2 , ylim=c(min(RandomMPD$CI5),max(RandomMPD$CI95)) ,
     xlab="Observed Mean Pairwise Distance" , ylab="Predicted Mean Pairwise Distance",  main = 'Predicted Mean Pairwise Distance vs Observed Mean Pairwise Distance Elm/Ash/Cottonwood Group',
     cex.main=1.6,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMPD) ) lines( rep(RandomMPD$ActualVals[i],2) , RandMPDPI[,i] , col=rangi2 )
abline(MPline, col = "red")
summary(MPline)
rmse(MPDline$ActualVals,MPDline$PredVals)

MPDmu <- link( MPDSplit24)
MPDmu_mean <- apply( MPDmu , 2 , mean )
MPDmu_PI <- apply( MPDmu , 2 , PI )

MPDlinkvals <-rbind(MPDmu_mean,MPDmu_PI)
MPDlinkvals <- data.frame(t(MPDlinkvals))
MPDline <- data.frame(cbind(FgroupSplitTrain[["24"]]$MeanPairwiseDistance,MPDlinkvals[,1:3]))
names(MPDline) <- c("ActualVals","PredVals","CI5","CI95")
MPDline$ActualVals <- scale(MPDline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomMPD <- sample_n(MPDline, 200)
RandMPDPI <- RandomMPD[,3:4]
RandMPDPI <- t(RandMPDPI)

MPline <- lm(MPDline$PredVals~(MPDline$ActualVals), data = MPDline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMPD$PredVals , x=(RandomMPD$ActualVals) , col=rangi2 , ylim=c(min(RandomMPD$CI5),max(RandomMPD$CI95)) ,
     xlab="Observed Mean Pairwise Distance" , ylab="Predicted Mean Pairwise Distance",  main = 'Predicted Mean Pairwise Distance vs Observed Mean Pairwise Distance Maple/Beech/Birch Group',
     cex.main=1.6,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMPD) ) lines( rep(RandomMPD$ActualVals[i],2) , RandMPDPI[,i] , col=rangi2 )
abline(MPline, col = "red")
summary(MPline)
rmse(MPDline$ActualVals,MPDline$PredVals)

#Tree Mortality, repeat as above but use beta distribution instead of normal. Also added basal area variable
datMortSplit1 <- list(
  Mort = (as.numeric(FgroupSplitTrain[["1"]]$MortRate)),
  PPT = scale(as.numeric(FgroupSplitTrain[["1"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["1"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["1"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["1"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["1"]]$RAD) ,
  BA = scale(FgroupSplitTrain[["1"]]$BAdead)
)
datMortSplit5 <- list(
  Mort = (as.numeric(FgroupSplitTrain[["5"]]$MortRate)),
  PPT = scale(as.numeric(FgroupSplitTrain[["5"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["5"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["5"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["5"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["5"]]$RAD) ,
  BA = scale(FgroupSplitTrain[["5"]]$BAdead)
)
datMortSplit20 <- list(
  Mort = (as.numeric(FgroupSplitTrain[["20"]]$MortRate)),
  PPT = scale(as.numeric(FgroupSplitTrain[["20"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["20"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["20"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["20"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["20"]]$RAD) ,
  BA = scale(FgroupSplitTrain[["20"]]$BAdead)
)
datMortSplit21 <- list(
  Mort = (as.numeric(FgroupSplitTrain[["21"]]$MortRate)),
  PPT = scale(as.numeric(FgroupSplitTrain[["21"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["21"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["21"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["21"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["21"]]$RAD) ,
  BA = scale(FgroupSplitTrain[["21"]]$BAdead)
)
datMortSplit23 <- list(
  Mort = (as.numeric(FgroupSplitTrain[["23"]]$MortRate)),
  PPT = scale(as.numeric(FgroupSplitTrain[["23"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["23"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["23"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["23"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["23"]]$RAD) ,
  BA = scale(FgroupSplitTrain[["23"]]$BAdead)
)
datMortSplit24 <- list(
  Mort = (as.numeric(FgroupSplitTrain[["24"]]$MortRate)),
  PPT = scale(as.numeric(FgroupSplitTrain[["24"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["24"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["24"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["24"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["24"]]$RAD) ,
  BA = scale(FgroupSplitTrain[["24"]]$BAdead)
)
#scale and center for out of sample test
centerMort1 <- attr(datMortSplit1$Mort, "scaled:center")
scaleMort1 <- attr(datMortSplit1$Mort, "scaled:scale")
centerMort5 <- attr(datMortSplit5$Mort, "scaled:center")
scaleMort5 <- attr(datMortSplit5$Mort, "scaled:scale")
centerMort20 <- attr(datMortSplit20$Mort, "scaled:center")
scaleMort20 <- attr(datMortSplit20$Mort, "scaled:scale")
centerMort21 <- attr(datMortSplit21$Mort, "scaled:center")
scaleMort21 <- attr(datMortSplit21$Mort, "scaled:scale")
centerMort23 <- attr(datMortSplit23$Mort, "scaled:center")
scaleMort23 <- attr(datMortSplit23$Mort, "scaled:scale")
centerMort24 <- attr(datMortSplit24$Mort, "scaled:center")
scaleMort24 <- attr(datMortSplit24$Mort, "scaled:scale")
centerBA1 <- attr(datMortSplit1$BA, "scaled:center")
scaleBA1 <- attr(datMortSplit1$BA, "scaled:scale")
centerBA5 <- attr(datMortSplit5$BA, "scaled:center")
scaleBA5 <- attr(datMortSplit5$BA, "scaled:scale")
centerBA20 <- attr(datMortSplit20$BA, "scaled:center")
scaleBA20 <- attr(datMortSplit20$BA, "scaled:scale")
centerBA21 <- attr(datMortSplit21$BA, "scaled:center")
scaleBA21 <- attr(datMortSplit21$BA, "scaled:scale")
centerBA23 <- attr(datMortSplit23$BA, "scaled:center")
scaleBA23 <- attr(datMortSplit23$BA, "scaled:scale")
centerBA24 <- attr(datMortSplit24$BA, "scaled:center")
scaleBA24 <- attr(datMortSplit24$BA, "scaled:scale")

#Add BA to labels for Mort Precis Plot
F1LabelsMort <- c("BA x ForestType1","BA x ForestType2","BA x ForestType3","BA x ForestType4",
                  "RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4",
              "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4",
              "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4",
              "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4",
              "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4",
              "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4","BA Slope",
              "RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
              "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
              "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD BA","SD Soil",
              "SD FAD","SD Model")
F5LabelsMort <- c("BA x ForestType1","BA x ForestType2","BA x ForestType3","BA x ForestType4",
                  "RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4",
                  "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4",
                  "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4",
                  "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4",
                  "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4",
                  "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4","BA Slope",
                  "RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
                  "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
                  "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD BA","SD Soil",
                  "SD FAD","SD Model")
F20LabelsMort <- c("BA x ForestType1","BA x ForestType2","BA x ForestType3","BA x ForestType4","BA x ForestType5","BA x ForestType6",
                   "RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4","RAD x ForestType5","RAD x ForestType6",
               "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4","RHUM x ForestType5","RHUM x ForestType6",
               "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4","Elevation x ForestType5","Elevation x ForestType6",
               "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4","PPT x ForestType5","PPT x ForestType6",
               "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4","MAT x ForestType5","MAT x ForestType6",
               "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4","Intercept x ForestType5","Intercept x ForestType6",
               "BA Slope","RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
               "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
               "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD BA","SD Soil",
               "SD FAD","SD Model")
F21LabelsMort <- c("BA x ForestType1","BA x ForestType2","BA x ForestType3","BA x ForestType4","BA x ForestType5","BA x ForestType6","BA x ForestType7","BA x ForestType8",
                   "BA x ForestType9","BA x ForestType10","BA x ForestType11","BA x ForestType12","BA x ForestType13","BA x ForestType14","BA x ForestType15","BA x ForestType16",
                   "BA x ForestType17","BA x ForestType18","BA x ForestType19","RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4","RAD x ForestType5","RAD x ForestType6","RAD x ForestType7","RAD x ForestType8",
               "RAD x ForestType9","RAD x ForestType10","RAD x ForestType11","RAD x ForestType12","RAD x ForestType13","RAD x ForestType14","RAD x ForestType15","RAD x ForestType16",
               "RAD x ForestType17","RAD x ForestType18","RAD x ForestType19",
               "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4","RHUM x ForestType5","RHUM x ForestType6","RHUM x ForestType7","RHUM x ForestType8",
               "RHUM x ForestType9","RHUM x ForestType10","RHUM x ForestType11","RHUM x ForestType12","RHUM x ForestType13","RHUM x ForestType14","RHUM x ForestType15","RHUM x ForestType16",
               "RHUM x ForestType17","RHUM x ForestType18","RHUM x ForestType19",
               "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4","Elevation x ForestType5","Elevation x ForestType6",
               "Elevation x ForestType7","Elevation x ForestType8","Elevation x ForestType9","Elevation x ForestType10","Elevation x ForestType11","Elevation x ForestType12",
               "Elevation x ForestType13","Elevation x ForestType14","Elevation x ForestType15","Elevation x ForestType16","Elevation x ForestType17","Elevation x ForestType18","Elevation x ForestType19",
               "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4","PPT x ForestType5","PPT x ForestType6","PPT x ForestType7","PPT x ForestType8",
               "PPT x ForestType9","PPT x ForestType10","PPT x ForestType11","PPT x ForestType12","PPT x ForestType13","PPT x ForestType14","PPT x ForestType15","PPT x ForestType16",
               "PPT x ForestType17","PPT x ForestType18","PPT x ForestType19",
               "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4","MAT x ForestType5","MAT x ForestType6","MAT x ForestType7","MAT x ForestType8",
               "MAT x ForestType9","MAT x ForestType10","MAT x ForestType11","MAT x ForestType12","MAT x ForestType13","MAT x ForestType14","MAT x ForestType15","MAT x ForestType16",
               "MAT x ForestType17","MAT x ForestType18","MAT x ForestType19",
               "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4","Intercept x ForestType5","Intercept x ForestType6",
               "Intercept x ForestType7","Intercept x ForestType8","Intercept x ForestType9","Intercept x ForestType10","Intercept x ForestType11","Intercept x ForestType12",
               "Intercept x ForestType13","Intercept x ForestType14","Intercept x ForestType15","Intercept x ForestType16","Intercept x ForestType17","Intercept x ForestType18","Intercept x ForestType19",
               "BA Slope","RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
               "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","Soil Order 7 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
               "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD BA","SD Soil",
               "SD FAD","SD Model")
F23LabelsMort <- c("BA x ForestType1","BA x ForestType2","BA x ForestType3","BA x ForestType4","BA x ForestType5","BA x ForestType6","BA x ForestType7","BA x ForestType8",
                   "BA x ForestType9","RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4","RAD x ForestType5","RAD x ForestType6","RAD x ForestType7","RAD x ForestType8",
               "RAD x ForestType9",
               "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4","RHUM x ForestType5","RHUM x ForestType6","RHUM x ForestType7","RHUM x ForestType8",
               "RHUM x ForestType9",
               "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4","Elevation x ForestType5","Elevation x ForestType6",
               "Elevation x ForestType7","Elevation x ForestType8","Elevation x ForestType9",
               "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4","PPT x ForestType5","PPT x ForestType6","PPT x ForestType7","PPT x ForestType8",
               "PPT x ForestType9",
               "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4","MAT x ForestType5","MAT x ForestType6","MAT x ForestType7","MAT x ForestType8",
               "MAT x ForestType9",
               "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4","Intercept x ForestType5","Intercept x ForestType6",
               "Intercept x ForestType7","Intercept x ForestType8","Intercept x ForestType9", "BA Slope",
               "RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
               "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
               "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD BA","SD Soil",
               "SD FAD","SD Model")
F24LabelsMort <- c("BA x ForestType1","BA x ForestType2","BA x ForestType3","BA x ForestType4",
                   "RAD x ForestType1","RAD x ForestType2","RAD x ForestType3","RAD x ForestType4",
                   "RHUM x ForestType1","RHUM x ForestType2","RHUM x ForestType3","RHUM x ForestType4",
                   "Elevation x ForestType1","Elevation x ForestType2","Elevation x ForestType3","Elevation x ForestType4",
                   "PPT x ForestType1","PPT x ForestType2","PPT x ForestType3","PPT x ForestType4",
                   "MAT x ForestType1","MAT x ForestType2","MAT x ForestType3","MAT x ForestType4",
                   "Intercept x ForestType1","Intercept x ForestType2","Intercept x ForestType3","Intercept x ForestType4","BA Slope",
                   "RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
                   "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
                   "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD BA","SD Soil",
                   "SD FAD","SD Model")

MortSplit1_beta <- ulam(
  alist(
    Mort ~ dbeta2(mu, phi),
    logit(mu) <- (a + a_ftype[FType] + 
                    b_ftype1[FType]*MAT + 
                    b_ftype2[FType]*PPT +
                    b_ftype3[FType]*Elevation + 
                    b_ftype4[FType]*RHUM +
                    b_ftype5[FType]*RAD + 
                    b_ftype6[FType]*BA + 
                    a2[Soil] + 
                    a3[FAD]),
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5,b_ftype6)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5,b6), Rho, sigma_FType),
    #fixed priors
    c(a,b1,b2,b3,b4,b5,b6) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data = datMortSplit1, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(MortSplit1_beta, depth=2),labels=F1LabelsMort,main="Model Parameter Estimates, Tree Mortality Rate White/Red/Jack Pine Group")
precisvaluesMort1 <- data.frame(precis(MortSplit1_beta, depth=2))
write.csv(precisvaluesMort1, "precisValsMort1.csv")

MortSplit5_beta <- ulam(
  alist(
    Mort ~ dbeta2(mu, phi),
    logit(mu) <- (a + a_ftype[FType] + 
                    b_ftype1[FType]*MAT + 
                    b_ftype2[FType]*PPT +
                    b_ftype3[FType]*Elevation + 
                    b_ftype4[FType]*RHUM +
                    b_ftype5[FType]*RAD + 
                    b_ftype6[FType]*BA + 
                    a2[Soil] + 
                    a3[FAD]),
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5,b_ftype6)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5,b6), Rho, sigma_FType),
    #fixed priors
    c(a,b1,b2,b3,b4,b5,b6) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data = datMortSplit5, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(MortSplit5_beta, depth=2),labels=F5LabelsMort,main="Model Parameter Estimates, Tree Mortality Rate Loblolly/Short Leaf Pine Group")
precisvaluesMort5 <- data.frame(precis(MortSplit5_beta, depth=2))
write.csv(precisvaluesMort5, "precisValsMort5.csv")

MortSplit20_beta <- ulam(
  alist(
    Mort ~ dbeta2(mu, phi),
    logit(mu) <- (a + a_ftype[FType] + 
                    b_ftype1[FType]*MAT + 
                    b_ftype2[FType]*PPT +
                    b_ftype3[FType]*Elevation + 
                    b_ftype4[FType]*RHUM +
                    b_ftype5[FType]*RAD + 
                    b_ftype6[FType]*BA + 
                    a2[Soil] + 
                    a3[FAD]),
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5,b_ftype6)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5,b6), Rho, sigma_FType),
    #primitive priors
    c(a,b1,b2,b3,b4,b5,b6) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data = datMortSplit20, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(MortSplit20_beta, depth=2),labels=F20LabelsMort,cex.lab=0.7,main="Model Parameter Estimates, Tree Mortality Rate Oak/Pine Group")
precisvaluesMort20 <- data.frame(precis(MortSplit20_beta, depth=2))
write.csv(precisvaluesMort20, "precisValsMort20.csv")

MortSplit21_beta <- ulam(
  alist(
    Mort ~ dbeta2(mu, phi),
    logit(mu) <- (a + a_ftype[FType] + 
                    b_ftype1[FType]*MAT + 
                    b_ftype2[FType]*PPT +
                    b_ftype3[FType]*Elevation + 
                    b_ftype4[FType]*RHUM +
                    b_ftype5[FType]*RAD + 
                    b_ftype6[FType]*BA + 
                    a2[Soil] + 
                    a3[FAD]),
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5,b_ftype6)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5,b6), Rho, sigma_FType),
    #fixed priors
    c(a,b1,b2,b3,b4,b5,b6) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data = datMortSplit21, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(MortSplit21_beta, depth=2),labels=F21LabelsMort,cex.lab=0.7,main="Model Parameter Estimates, Tree Mortality Rate Oak/Hickory Group")
precisvaluesMort21 <- data.frame(precis(MortSplit21_beta, depth=2))
write.csv(precisvaluesMort21, "precisValsMort21.csv")

MortSplit23_beta <- ulam(
  alist(
    Mort ~ dbeta2(mu, phi),
    logit(mu) <- (a + a_ftype[FType] + 
                    b_ftype1[FType]*MAT + 
                    b_ftype2[FType]*PPT +
                    b_ftype3[FType]*Elevation + 
                    b_ftype4[FType]*RHUM +
                    b_ftype5[FType]*RAD + 
                    b_ftype6[FType]*BA + 
                    a2[Soil] + 
                    a3[FAD]),
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5,b_ftype6)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5,b6), Rho, sigma_FType),
    #fixed priors
    c(a,b1,b2,b3,b4,b5,b6) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data = datMortSplit23, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(MortSplit23_beta, depth=2),labels=F23LabelsMort,cex.lab=0.7,main="Model Parameter Estimates, Tree Mortality Rate Elm/Ash/Cottonwood Group")
precisvaluesMort23 <- data.frame(precis(MortSplit23_beta, depth=2))
write.csv(precisvaluesMort23, "precisValsMort23.csv")

MortSplit24_beta <- ulam(
  alist(
    Mort ~ dbeta2(mu, phi),
    logit(mu) <- (a + a_ftype[FType] + 
                    b_ftype1[FType]*MAT + 
                    b_ftype2[FType]*PPT +
                    b_ftype3[FType]*Elevation + 
                    b_ftype4[FType]*RHUM +
                    b_ftype5[FType]*RAD + 
                    b_ftype6[FType]*BA + 
                    a2[Soil] + 
                    a3[FAD]),
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5,b_ftype6)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5,b6), Rho, sigma_FType),
    #fixed priors
    c(a,b1,b2,b3,b4,b5,b6) ~ dnorm(0, 0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 0.5),
    z ~ dnorm(0, 0.5),
    phi ~ dexp(1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data = datMortSplit24, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(MortSplit24_beta, depth=2),labels=F24LabelsMort,cex.lab=0.7,main="Model Parameter Estimates, Tree Mortality Rate Maple/Beech/Birch Group")
precisvaluesMort24 <- data.frame(precis(MortSplit24_beta, depth=2))
write.csv(precisvaluesMort24, "precisValsMort24.csv")

#posterior predictive checks, link to simulate model, then calculate mean point estimates and 95/5 confidence intervals, run one model at a time.
Mortmu <- link( MortSplit1_beta)
Mortmu_mean <- apply( Mortmu , 2 , mean )
Mortmu_PI <- apply( Mortmu , 2 , PI )

Mortlinkvals <-rbind(Mortmu_mean,Mortmu_PI)
Mortlinkvals <- data.frame(t(Mortlinkvals))
Mortline <- data.frame(cbind(FgroupSplitTrain[["1"]]$MortRate,Mortlinkvals[,1:3]))
names(Mortline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomMort <- sample_n(Mortline, 200)
RandMortPI <- RandomMort[,3:4]
RandMortPI <- t(RandMortPI)

Mline <- lm(Mortline$PredVals~(Mortline$ActualVals), data = Mortline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMort$PredVals , x=(RandomMort$ActualVals) , col=rangi2 , ylim=c(min(RandomMort$CI5),max(RandomMort$CI95)) ,
     xlab="Observed Tree Mortality Rate" , ylab="Predicted Tree Mortality Rate",  main = 'Predicted Tree Mortality Rate vs Observed Tree Mortality Rate White/Red/Jack Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMort) ) lines( rep(RandomMort$ActualVals[i],2) , RandMortPI[,i] , col=rangi2 )
abline(Mline, col = "red")
summary(Mline) #R2 0.5481 
rmse(Mortline$ActualVals,Mortline$PredVals) #RMSE=0.07476656

Mortmu <- link( MortSplit5_beta)
Mortmu_mean <- apply( Mortmu , 2 , mean )
Mortmu_PI <- apply( Mortmu , 2 , PI )

Mortlinkvals <-rbind(Mortmu_mean,Mortmu_PI)
Mortlinkvals <- data.frame(t(Mortlinkvals))
Mortline <- data.frame(cbind(FgroupSplitTrain[["5"]]$MortRate,Mortlinkvals[,1:3]))
names(Mortline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomMort <- sample_n(Mortline, 200)
RandMortPI <- RandomMort[,3:4]
RandMortPI <- t(RandMortPI)

Mline <- lm(Mortline$PredVals~(Mortline$ActualVals), data = Mortline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMort$PredVals , x=(RandomMort$ActualVals) , col=rangi2 , ylim=c(min(RandomMort$CI5),max(RandomMort$CI95)) ,
     xlab="Observed Tree Mortality Rate" , ylab="Predicted Tree Mortality Rate",  main = 'Predicted Tree Mortality Rate vs Observed Tree Mortality Rate Loblolly/Short Leaf Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMort) ) lines( rep(RandomMort$ActualVals[i],2) , RandMortPI[,i] , col=rangi2 )
abline(Mline, col = "red")
summary(Mline) #r2 0.5277 
rmse(Mortline$ActualVals,Mortline$PredVals) #RMSE=0.07318606

Mortmu <- link( MortSplit20_beta)
Mortmu_mean <- apply( Mortmu , 2 , mean )
Mortmu_PI <- apply( Mortmu , 2 , PI )

Mortlinkvals <-rbind(Mortmu_mean,Mortmu_PI)
Mortlinkvals <- data.frame(t(Mortlinkvals))
Mortline <- data.frame(cbind(FgroupSplitTrain[["20"]]$MortRate,Mortlinkvals[,1:3]))
names(Mortline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomMort <- sample_n(Mortline, 200)
RandMortPI <- RandomMort[,3:4]
RandMortPI <- t(RandMortPI)

Mline <- lm(Mortline$PredVals~(Mortline$ActualVals), data = Mortline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMort$PredVals , x=(RandomMort$ActualVals) , col=rangi2 , ylim=c(min(RandomMort$CI5),max(RandomMort$CI95)) ,
     xlab="Observed Tree Mortality Rate" , ylab="Predicted Tree Mortality Rate",  main = 'Predicted Tree Mortality Rate vs Observed Tree Mortality Rate Oak/Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMort) ) lines( rep(RandomMort$ActualVals[i],2) , RandMortPI[,i] , col=rangi2 )
abline(Mline, col = "red")
summary(Mline) #R2 0.4886 
rmse(Mortline$ActualVals,Mortline$PredVals) #RMSE=0.07121449

Mortmu <- link( MortSplit21_beta)
Mortmu_mean <- apply( Mortmu , 2 , mean )
Mortmu_PI <- apply( Mortmu , 2 , PI )

Mortlinkvals <-rbind(Mortmu_mean,Mortmu_PI)
Mortlinkvals <- data.frame(t(Mortlinkvals))
Mortline <- data.frame(cbind(FgroupSplitTrain[["21"]]$MortRate,Mortlinkvals[,1:3]))
names(Mortline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomMort <- sample_n(Mortline, 200)
RandMortPI <- RandomMort[,3:4]
RandMortPI <- t(RandMortPI)

Mline <- lm(Mortline$PredVals~(Mortline$ActualVals), data = Mortline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMort$PredVals , x=(RandomMort$ActualVals) , col=rangi2 , ylim=c(min(RandomMort$CI5),max(RandomMort$CI95)) ,
     xlab="Observed Tree Mortality Rate" , ylab="Predicted Tree Mortality Rate",  main = 'Predicted Tree Mortality Rate vs Observed Tree Mortality Rate Oak/Hickory Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMort) ) lines( rep(RandomMort$ActualVals[i],2) , RandMortPI[,i] , col=rangi2 )
abline(Mline, col = "red")
summary(Mline) #R2  0.29 
rmse(Mortline$ActualVals,Mortline$PredVals) #RMSE=0.08440895

Mortmu <- link( MortSplit23_beta)
Mortmu_mean <- apply( Mortmu , 2 , mean )
Mortmu_PI <- apply( Mortmu , 2 , PI )

Mortlinkvals <-rbind(Mortmu_mean,Mortmu_PI)
Mortlinkvals <- data.frame(t(Mortlinkvals))
Mortline <- data.frame(cbind(FgroupSplitTrain[["23"]]$MortRate,Mortlinkvals[,1:3]))
names(Mortline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomMort <- sample_n(Mortline, 200)
RandMortPI <- RandomMort[,3:4]
RandMortPI <- t(RandMortPI)

Mline <- lm(Mortline$PredVals~(Mortline$ActualVals), data = Mortline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMort$PredVals , x=(RandomMort$ActualVals) , col=rangi2 , ylim=c(min(RandomMort$CI5),max(RandomMort$CI95)) ,
     xlab="Observed Tree Mortality Rate" , ylab="Predicted Tree Mortality Rate",  main = 'Predicted Tree Mortality Rate vs Observed Tree Mortality Rate Elm/Ash/Cottonwood Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMort) ) lines( rep(RandomMort$ActualVals[i],2) , RandMortPI[,i] , col=rangi2 )
abline(Mline, col = "red")
summary(Mline) #R2 0.4082
rmse(Mortline$ActualVals,Mortline$PredVals) #RMSE=0.09826974


Mortmu <- link( MortSplit24_beta)
Mortmu_mean <- apply( Mortmu , 2 , mean )
Mortmu_PI <- apply( Mortmu , 2 , PI )

Mortlinkvals <-rbind(Mortmu_mean,Mortmu_PI)
Mortlinkvals <- data.frame(t(Mortlinkvals))
Mortline <- data.frame(cbind(FgroupSplitTrain[["24"]]$MortRate,Mortlinkvals[,1:3]))
names(Mortline) <- c("ActualVals","PredVals","CI5","CI95")
#Plot too messy with all values shown. Plot random sample of 200 Group
set.seed(20)
RandomMort <- sample_n(Mortline, 200)
RandMortPI <- RandomMort[,3:4]
RandMortPI <- t(RandMortPI)

Mline <- lm(Mortline$PredVals~(Mortline$ActualVals), data = Mortline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomMort$PredVals , x=(RandomMort$ActualVals) , col=rangi2 , ylim=c(min(RandomMort$CI5),max(RandomMort$CI95)) ,
     xlab="Observed Tree Mortality Rate" , ylab="Predicted Tree Mortality Rate",  main = 'Predicted Tree Mortality Rate vs Observed Tree Mortality Rate Maple/Beech/Birch Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomMort) ) lines( rep(RandomMort$ActualVals[i],2) , RandMortPI[,i] , col=rangi2 )
abline(Mline, col = "red")
summary(Mline) #R2 0.2812 
rmse(Mortline$ActualVals,Mortline$PredVals) #RMSE=0.08170673

#Air Quality Index, repeat the same as above, normal distribution, add time weighting variable to likelihood
datAQISplit1 <- list(
  AQI = scale(as.numeric(FgroupSplitTrain[["1"]]$MaxAQI)),
  PPT = scale(as.numeric(FgroupSplitTrain[["1"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["1"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["1"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["1"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["1"]]$RAD),
  Year = F1time_weight$Year,
  time_weight = F1time_weight$time_weight
)

datAQISplit5 <- list(
  AQI = scale(as.numeric(FgroupSplitTrain[["5"]]$MaxAQI)),
  PPT = scale(as.numeric(FgroupSplitTrain[["5"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["5"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["5"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["5"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["5"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["5"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["5"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["5"]]$RAD),
  Year = F5time_weight$Year,
  time_weight = F5time_weight$time_weight
)

datAQISplit20 <- list(
  AQI = scale(as.numeric(FgroupSplitTrain[["20"]]$MaxAQI)),
  PPT = scale(as.numeric(FgroupSplitTrain[["20"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["20"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["20"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["20"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["20"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["20"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["20"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["20"]]$RAD),
  Year = F20time_weight$Year,
  time_weight = F20time_weight$time_weight
)
datAQISplit21 <- list(
  AQI = scale(as.numeric(FgroupSplitTrain[["21"]]$MaxAQI)),
  PPT = scale(as.numeric(FgroupSplitTrain[["21"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["21"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["21"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["21"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["21"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["21"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["21"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["21"]]$RAD),
  Year = F21time_weight$Year,
  time_weight = F21time_weight$time_weight
)
datAQISplit23 <- list(
  AQI = scale(as.numeric(FgroupSplitTrain[["23"]]$MaxAQI)),
  PPT = scale(as.numeric(FgroupSplitTrain[["23"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["23"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["23"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["23"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["23"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["23"]]$RAD),
  Year = F23time_weight$Year,
  time_weight = F23time_weight$time_weight
)
datAQISplit24 <- list(
  AQI = scale(as.numeric(FgroupSplitTrain[["24"]]$MaxAQI)),
  PPT = scale(as.numeric(FgroupSplitTrain[["24"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["24"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["24"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["24"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["24"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["24"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["24"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["24"]]$RAD),
  Year = F24time_weight$Year,
  time_weight = F24time_weight$time_weight
)

#scale and center for out of sample test
centerAQI1 <- attr(datAQISplit1$AQI, "scaled:center")
scaleAQI1 <- attr(datAQISplit1$AQI, "scaled:scale")
centerAQI5 <- attr(datAQISplit5$AQI, "scaled:center")
scaleAQI5 <- attr(datAQISplit5$AQI, "scaled:scale")
centerAQI20 <- attr(datAQISplit20$AQI, "scaled:center")
scaleAQI20 <- attr(datAQISplit20$AQI, "scaled:scale")
centerAQI21 <- attr(datAQISplit21$AQI, "scaled:center")
scaleAQI21 <- attr(datAQISplit21$AQI, "scaled:scale")
centerAQI23 <- attr(datAQISplit23$AQI, "scaled:center")
scaleAQI23 <- attr(datAQISplit23$AQI, "scaled:scale")
centerAQI24 <- attr(datAQISplit24$AQI, "scaled:center")
scaleAQI24 <- attr(datAQISplit24$AQI, "scaled:scale")

AQISplit1 <- ulam(
  alist(
    AQI ~ dnorm(mu, sigma),
    mu <- (a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
             b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM + b_ftype5[FType]*RAD + 
             a2[Soil] + a3[FAD]) * time_weight,# Time-weighted likelihood
    # adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    # fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    sigma ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ), 
  data = datAQISplit1, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)
#plot(precis(AQISplit1, depth=2),labels=F1Labels,main="Model Parameter Estimates, Air Quality Index White/Red/Jack Pine Group")
precisvaluesAQI1 <- data.frame(precis(AQISplit1, depth=2))
write.csv(precisvaluesAQI1, "precisValsAQI1.csv")

AQISplit5 <- ulam(
  alist(
    AQI ~ dnorm(mu, sigma),
    mu <- (a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
             b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM + b_ftype5[FType]*RAD + 
             a2[Soil] + a3[FAD]) * time_weight,# Time-weighted likelihood
    # adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    # fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    sigma ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ), 
  data = datAQISplit5, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#plot(precis(AQISplit5, depth=2),labels=F5Labels,main="Model Parameter Estimates, Air Quality Index Loblolly/Short Leaf Pine Group")
precisvaluesAQI5 <- data.frame(precis(AQISplit5, depth=2))
write.csv(precisvaluesAQI5, "precisValsAQI5.csv")

AQISplit20 <- ulam(
  alist(
    AQI ~ dnorm(mu, sigma),
    mu <- (a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
             b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM + b_ftype5[FType]*RAD + 
             a2[Soil] + a3[FAD]) * time_weight,# Time-weighted likelihood
    # adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    # fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    sigma ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ),  
  data = datAQISplit20, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#plot(precis(AQISplit20, depth=2),labels=F20Labels,main="Model Parameter Estimates, Air Quality Index Oak/Pine Group")
precisvaluesAQI20 <- data.frame(precis(AQISplit20, depth=2))
write.csv(precisvaluesAQI20, "precisValsAQI20.csv")


AQISplit21 <- ulam(
  alist(
    AQI ~ dnorm(mu, sigma),
    mu <- (a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
             b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM + b_ftype5[FType]*RAD + 
             a2[Soil] + a3[FAD]) * time_weight,# Time-weighted likelihood
    # adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    # fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    sigma ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ), 
  data = datAQISplit21, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#plot(precis(AQISplit21, depth=2),labels=F21Labels,cex.lab=0.7,main="Model Parameter Estimates, Air Quality Index Oak/Hickory Group")
precisvaluesAQI21 <- data.frame(precis(AQISplit21, depth=2))
write.csv(precisvaluesAQI21, "precisValsAQI21.csv")

AQISplit23 <- ulam(
  alist(
    AQI ~ dnorm(mu, sigma),
    mu <- (a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
             b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM + b_ftype5[FType]*RAD + 
             a2[Soil] + a3[FAD]) * time_weight,# Time-weighted likelihood
    # adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    # fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,0.5),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0,0.5),
    z ~ dnorm(0,0.5),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    sigma ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ), 
  data = datAQISplit23, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#plot(precis(AQISplit23, depth=2),labels=F23Labels,cex.lab=0.7,main="Model Parameter Estimates, Air Quality Index Elm/Ash/Cottonwood Group")
precisvaluesAQI23 <- data.frame(precis(AQISplit23, depth=2))
write.csv(precisvaluesAQI23, "precisValsAQI23.csv")

AQISplit24 <- ulam(
  alist(
    AQI ~ dnorm(mu, sigma),
    mu <- (a + a_ftype[FType] + b_ftype1[FType]*MAT + b_ftype2[FType]*PPT +
             b_ftype3[FType]*Elevation + b_ftype4[FType]*RHUM + b_ftype5[FType]*RAD + 
             a2[Soil] + a3[FAD]) * time_weight,# Time-weighted likelihood
    # adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FType] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FType), 
    # fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0,1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0,1),
    z ~ dnorm(0,1),
    sigma_FType ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    sigma ~ dexp(1),
    Rho ~ dlkjcorr(2)
  ),  
  data = datAQISplit24, log_lik = TRUE, chains=4, cores=8, iter=10000
)
#plot(precis(AQISplit24, depth=2),labels=F24Labels,cex.lab=0.7,main="Model Parameter Estimates, Air Quality Index Maple/Beech/Birch Group")
precisvaluesAQI24 <- data.frame(precis(AQISplit24, depth=2))
write.csv(precisvaluesAQI24, "precisValsAQI24.csv")

AQImu <- link(AQISplit1)
AQImu_mean <- apply( AQImu , 2 , mean )
AQImu_PI <- apply( AQImu , 2 , PI )

AQIlinkvals <-rbind(AQImu_mean,AQImu_PI)
AQIlinkvals <- data.frame(t(AQIlinkvals))
AQIline <- data.frame(cbind(FgroupSplitTrain[["1"]]$MaxAQI,AQIlinkvals[,1:3]))
names(AQIline) <- c("ActualVals","PredVals","CI5","CI95")
AQIline$ActualVals <- scale(AQIline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomAQI <- sample_n(AQIline, 200)
RandAQIPI <- RandomAQI[,3:4]
RandAQIPI <- t(RandAQIPI)

AQline <- lm(AQIline$PredVals~(AQIline$ActualVals), data = AQIline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomAQI$PredVals , x=(RandomAQI$ActualVals) , col=rangi2 , ylim=c(min(RandomAQI$CI5),max(RandomAQI$CI95)) ,
     xlab="Observed Air Quality Index" , ylab="Predicted Air Quality Index",  main = 'Predicted Air Quality Index vs Observed Air Quality Index White/Red/Jack Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomAQI) ) lines( rep(RandomAQI$ActualVals[i],2) , RandAQIPI[,i] , col=rangi2 )
abline(AQline, col = "red")
summary(AQline) #adj R  0.5031 
rmse(AQIline$ActualVals,AQIline$PredVals) #rmse 0.8421532

AQImu <- link( AQISplit5)
AQImu_mean <- apply( AQImu , 2 , mean )
AQImu_PI <- apply( AQImu , 2 , PI )

AQIlinkvals <-rbind(AQImu_mean,AQImu_PI)
AQIlinkvals <- data.frame(t(AQIlinkvals))
AQIline <- data.frame(cbind(FgroupSplitTrain[["5"]]$MaxAQI,AQIlinkvals[,1:3]))
names(AQIline) <- c("ActualVals","PredVals","CI5","CI95")
AQIline$ActualVals <- scale(AQIline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomAQI <- sample_n(AQIline, 200)
RandAQIPI <- RandomAQI[,3:4]
RandAQIPI <- t(RandAQIPI)

AQline <- lm(AQIline$PredVals~(AQIline$ActualVals), data = AQIline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomAQI$PredVals , x=(RandomAQI$ActualVals) , col=rangi2 , ylim=c(min(RandomAQI$CI5),max(RandomAQI$CI95)) ,
     xlab="Observed Air Quality Index" , ylab="Predicted Air Quality Index",  main = 'Predicted Air Quality Index vs Observed Air Quality Index Loblolly/Short Leaf Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomAQI) ) lines( rep(RandomAQI$ActualVals[i],2) , RandAQIPI[,i] , col=rangi2 )
abline(AQline, col = "red")
summary(AQline) #adj r 0.4495
rmse(AQIline$ActualVals,AQIline$PredVals) #rmse 0.8062621

AQImu <- link( AQISplit20)
AQImu_mean <- apply( AQImu , 2 , mean )
AQImu_PI <- apply( AQImu , 2 , PI )

AQIlinkvals <-rbind(AQImu_mean,AQImu_PI)
AQIlinkvals <- data.frame(t(AQIlinkvals))
AQIline <- data.frame(cbind(FgroupSplitTrain[["20"]]$MaxAQI,AQIlinkvals[,1:3]))
names(AQIline) <- c("ActualVals","PredVals","CI5","CI95")
AQIline$ActualVals <- scale(AQIline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomAQI <- sample_n(AQIline, 200)
RandAQIPI <- RandomAQI[,3:4]
RandAQIPI <- t(RandAQIPI)

AQline <- lm(AQIline$PredVals~(AQIline$ActualVals), data = AQIline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomAQI$PredVals , x=(RandomAQI$ActualVals) , col=rangi2 , ylim=c(min(RandomAQI$CI5),max(RandomAQI$CI95)) ,
     xlab="Observed Air Quality Index" , ylab="Predicted Air Quality Index",  main = 'Predicted Air Quality Index vs Observed Air Quality Index Oak/Pine Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomAQI) ) lines( rep(RandomAQI$ActualVals[i],2) , RandAQIPI[,i] , col=rangi2 )
abline(AQline, col = "red")
summary(AQline) #adj r 0.4637 
rmse(AQIline$ActualVals,AQIline$PredVals) #rmse 0.8378687

AQImu <- link( AQISplit21)
AQImu_mean <- apply( AQImu , 2 , mean )
AQImu_PI <- apply( AQImu , 2 , PI )

AQIlinkvals <-rbind(AQImu_mean,AQImu_PI)
AQIlinkvals <- data.frame(t(AQIlinkvals))
AQIline <- data.frame(cbind(FgroupSplitTrain[["21"]]$MaxAQI,AQIlinkvals[,1:3]))
names(AQIline) <- c("ActualVals","PredVals","CI5","CI95")
AQIline$ActualVals <- scale(AQIline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomAQI <- sample_n(AQIline, 200)
RandAQIPI <- RandomAQI[,3:4]
RandAQIPI <- t(RandAQIPI)

AQline <- lm(AQIline$PredVals~(AQIline$ActualVals), data = AQIline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomAQI$PredVals , x=(RandomAQI$ActualVals) , col=rangi2 , ylim=c(min(RandomAQI$CI5),max(RandomAQI$CI95)) ,
     xlab="Observed Air Quality Index" , ylab="Predicted Air Quality Index",  main = 'Predicted Air Quality Index vs Observed Air Quality Index Oak/Hickory Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomAQI) ) lines( rep(RandomAQI$ActualVals[i],2) , RandAQIPI[,i] , col=rangi2 )
abline(AQline, col = "red")
summary(AQline) #adj r 0.4986 
rmse(AQIline$ActualVals,AQIline$PredVals) #rmse 0.8443705

AQImu <- link( AQISplit23)
AQImu_mean <- apply( AQImu , 2 , mean )
AQImu_PI <- apply( AQImu , 2 , PI )

AQIlinkvals <-rbind(AQImu_mean,AQImu_PI)
AQIlinkvals <- data.frame(t(AQIlinkvals))
AQIline <- data.frame(cbind(FgroupSplitTrain[["23"]]$MaxAQI,AQIlinkvals[,1:3]))
names(AQIline) <- c("ActualVals","PredVals","CI5","CI95")
AQIline$ActualVals <- scale(AQIline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomAQI <- sample_n(AQIline, 200)
RandAQIPI <- RandomAQI[,3:4]
RandAQIPI <- t(RandAQIPI)

AQline <- lm(AQIline$PredVals~(AQIline$ActualVals), data = AQIline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomAQI$PredVals , x=(RandomAQI$ActualVals) , col=rangi2 , ylim=c(min(RandomAQI$CI5),max(RandomAQI$CI95)) ,
     xlab="Observed Air Quality Index" , ylab="Predicted Air Quality Index",  main = 'Predicted Air Quality Index vs Observed Air Quality Index Elm/Ash/Cottonwood Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomAQI) ) lines( rep(RandomAQI$ActualVals[i],2) , RandAQIPI[,i] , col=rangi2 )
abline(AQline, col = "red")
summary(AQline) #adj R - 0.5294 
rmse(AQIline$ActualVals,AQIline$PredVals)#rmse 0.8762488

AQImu <- link( AQISplit24)
AQImu_mean <- apply( AQImu , 2 , mean )
AQImu_PI <- apply( AQImu , 2 , PI )

AQIlinkvals <-rbind(AQImu_mean,AQImu_PI)
AQIlinkvals <- data.frame(t(AQIlinkvals))
AQIline <- data.frame(cbind(FgroupSplitTrain[["24"]]$MaxAQI,AQIlinkvals[,1:3]))
names(AQIline) <- c("ActualVals","PredVals","CI5","CI95")
AQIline$ActualVals <- scale(AQIline$ActualVals)
#Plot too messy with all values shown. Plot random sample of 200 plots
set.seed(20)
RandomAQI <- sample_n(AQIline, 200)
RandAQIPI <- RandomAQI[,3:4]
RandAQIPI <- t(RandAQIPI)

AQline <- lm(AQIline$PredVals~(AQIline$ActualVals), data = AQIline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomAQI$PredVals , x=(RandomAQI$ActualVals) , col=rangi2 , ylim=c(min(RandomAQI$CI5),max(RandomAQI$CI95)) ,
     xlab="Observed Air Quality Index" , ylab="Predicted Air Quality Index",  main = 'Predicted Air Quality Index vs Observed Air Quality Index Maple/Beech/Birch Group',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomAQI) ) lines( rep(RandomAQI$ActualVals[i],2) , RandAQIPI[,i] , col=rangi2 )
abline(AQline, col = "red")
summary(AQline) #adj r 0.4769
rmse(AQIline$ActualVals,AQIline$PredVals) #rmse 0.8548604

#Soil Quality Index, model the same as above, but data not separated by forest type
SQIdata1$ForestGroupID <- as.integer(as.factor(SQIdata1$ForestGroup))
SQIdataL <- list(SQIdata1)
SQISplitAll <- map(SQIdataL, ~ tryCatch(train_data(.x)))
SQItrain  <- map(SQISplitAll, ~ filter(.x, split == "train") %>% dplyr::select(-split))
SQItest  <- map(SQISplitAll, ~ filter(.x, split == "test") %>% dplyr::select(-split))
SQItrain <- as.data.frame(SQItrain)
SQItest <- as.data.frame(SQItest)

datSQI <- list(
  SQI = as.numeric(SQItrain$SQIPCT)/100,
  PPT = scale(as.numeric(SQItrain$PPT)),
  MAT = scale(as.numeric(SQItrain$MAT)),
  FAD = as.integer(SQItrain$FADClass),
  Soil = as.integer(as.factor(SQItrain$SoilID)),
  FGroup =  as.integer(as.factor(SQItrain$ForestGroupID)),
  Elevation = scale(as.integer(SQItrain$Elevation)),
  RHUM = scale(SQItrain$RHUM),
  RAD = scale(SQItrain$RAD)
)

centerMATSQI <- attr(datSQI$MAT, "scaled:center")
scaleMATSQI <- attr(datSQI$MAT, "scaled:scale")
centerPPTSQI <- attr(datSQI$PPT, "scaled:center")
scalePPTSQI <- attr(datSQI$PPT, "scaled:scale")
centerRHUMSQI <- attr(datSQI$RHUM, "scaled:center")
scaleRHUMSQI <- attr(datSQI$RHUM, "scaled:scale")
centerRADSQI <- attr(datSQI$RAD, "scaled:center")
scaleRADSQI <- attr(datSQI$RAD, "scaled:scale")
centerElevationSQI <- attr(datSQI$Elevation, "scaled:center")
scaleElevationSQI <- attr(datSQI$Elevation, "scaled:scale")

SQIMod_beta <- ulam(
  alist(
    SQI ~ dbeta2(mu, phi),
    logit(mu) <- (a + a_ftype[FGroup] + 
      b_ftype1[FGroup]*MAT + 
      b_ftype2[FGroup]*PPT +
      b_ftype3[FGroup]*Elevation + 
      b_ftype4[FGroup]*RHUM +
      b_ftype5[FGroup]*RAD + 
      a2[Soil] + 
      a3[FAD]),
    #adaptive priors
    c(a_ftype,b_ftype1,b_ftype2,b_ftype3,b_ftype4,b_ftype5)[FGroup] ~ 
      multi_normal(c(a,b1,b2,b3,b4,b5), Rho, sigma_FGroup),
    #fixed priors
    c(a,b1,b2,b3,b4,b5) ~ dnorm(0, 1),
    a2[Soil] ~ dnorm(x, sigma_soil),
    a3[FAD] ~ dnorm(z, sigma_fad),
    x ~ dnorm(0, 1),
    z ~ dnorm(0, 1),
    phi ~ dexp(1),
    sigma_FGroup ~ dexp(1),
    sigma_soil ~ dexp(1),
    sigma_fad ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data = datSQI, log_lik = TRUE, chains = 4, cores = 8, iter = 10000
)

SQILabels <- c("RAD x ForestGroup1","RAD x ForestGroup2","RAD x ForestGroup3","RAD x ForestGroup4","RAD x ForestGroup5","RAD x ForestGroup6","RAD x ForestGroup7","RAD x ForestGroup8",
               "RAD x ForestGroup9","RAD x ForestGroup10","RAD x ForestGroup11","RAD x ForestGroup12",
               "RHUM x ForestGroup1","RHUM x ForestGroup2","RHUM x ForestGroup3","RHUM x ForestGroup4","RHUM x ForestGroup5","RHUM x ForestGroup6","RHUM x ForestGroup7","RHUM x ForestGroup8",
               "RHUM x ForestGroup9","RHUM x ForestGroup10","RHUM x ForestGroup11","RHUM x ForestGroup12",
               "Elevation x ForestGroup1","Elevation x ForestGroup2","Elevation x ForestGroup3","Elevation x ForestGroup4","Elevation x ForestGroup5","Elevation x ForestGroup6",
               "Elevation x ForestGroup7","Elevation x ForestGroup8","Elevation x ForestGroup9","Elevation x ForestGroup10","Elevation x ForestGroup11","Elevation x ForestGroup12",
               "PPT x ForestGroup1","PPT x ForestGroup2","PPT x ForestGroup3","PPT x ForestGroup4","PPT x ForestGroup5","PPT x ForestGroup6","PPT x ForestGroup7","PPT x ForestGroup8",
               "PPT x ForestGroup9","PPT x ForestGroup10","PPT x ForestGroup11","PPT x ForestGroup12",
               "MAT x ForestGroup1","MAT x ForestGroup2","MAT x ForestGroup3","MAT x ForestGroup4","MAT x ForestGroup5","MAT x ForestGroup6","MAT x ForestGroup7","MAT x ForestGroup8",
               "MAT x ForestGroup9","MAT x ForestGroup10","MAT x ForestGroup11","MAT x ForestGroup12",
               "Intercept x ForestGroup1","Intercept x ForestGroup2","Intercept x ForestGroup3","Intercept x ForestGroup4","Intercept x ForestGroup5","Intercept x ForestGroup6",
               "Intercept x ForestGroup7","Intercept x ForestGroup8","Intercept x ForestGroup9","Intercept x ForestGroup10","Intercept x ForestGroup11","Intercept x ForestGroup12",
               "RAD Slope", "RHUM Slope","Elevation Slope","PPT Slope","MAT Slope", "Intercept","Soil Order 1 Offset","Soil Order 2 Offset",
               "Soil Order 3 Offset","Soil Order 4 Offset","Soil Order 5 Offset","Soil Order 6 Offset","FAD 1 Offset","FAD 2 Offset","FAD 3 Offset",
               "FAD 4 Offset","FAD 5 Offset","Soil Mean","FAD Mean","SD Intercept","SD MAT","SD PPT","SD Elevation","SD RHUM","SD RAD","SD Soil",
               "SD FAD")
#plot(precis(SQIMod_beta, depth=2), labels=SQILabels, main= "Model Parameter Estimates, Soil Quality Index Model")
precisvaluesSQI <- data.frame(precis(SQIMod_beta, depth=2))
write.csv(precisvaluesSQI, "precisValsSQI.csv")

#posterior predictive plots for SQI
SQImu <- link( SQIMod_beta)
SQImu_mean <- apply( SQImu , 2 , mean )
SQImu_PI <- apply( SQImu , 2 , PI )

SQIlinkvals <-rbind(SQImu_mean,SQImu_PI)
SQIlinkvals <- data.frame(t(SQIlinkvals))
SQIline <- data.frame(cbind(SQItrain$SQIPCT,SQIlinkvals[,1:3]))
names(SQIline) <- c("ActualVals","PredVals","CI5","CI95")
#SQIline$ActualVals <- scale(SQIline$ActualVals)
SQIline$ActualVals <- (SQIline$ActualVals)/100
SQIPI <- SQIline[,3:4]
set.seed(20)
RandomSQI <- sample_n(SQIline, 182)
RandSQIPI <- RandomSQI[,3:4]
RandSQIPI <- t(RandSQIPI)

SQline <- lm(SQIline$PredVals~(SQIline$ActualVals), data = SQIline)
par(mar=c(5,6,4,5)+.1)
plot(y=RandomSQI$PredVals , x=(RandomSQI$ActualVals) , col=rangi2 , ylim=c(min(RandomSQI$CI5),max(RandomSQI$CI95)) ,
     xlab="Observed Soil Quality Index" , ylab="Predicted Soil Quality Index",  main = 'Predicted Soil Quality Index vs Observed Soil Quality Index',
     cex.main=2.0,cex.lab=2.0,cex.axis=1.6,mgp=c(3,1,0))
for ( i in 1:nrow(RandomSQI) ) lines( rep(RandomSQI$ActualVals[i],2) , RandSQIPI[,i] , col=rangi2 )
abline(SQline, col = "red")
summary(SQline)
rmse(SQIline$ActualVals,SQIline$PredVals)
