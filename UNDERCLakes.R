#Limno Data parsing
rm(list = ls())
setwd("~/Desktop/UNDERCLim/")
chloroUN <- read.csv(file='chlorophyll_UNDERC.csv')
colorUN <-read.csv(file='color_UNDERC.csv')
lakeUN <- read.csv(file='lakes_UNDERC.csv')
unitUN <- read.csv(file='units_UNDERC.csv')
chemUN <- read.csv(file='waterChemistry_UNDERC.csv')
#select lakes, bay, crampton, tenderfoot, roach, morris
projlakes <- filter(chemUN, lakeID %in% c("BA", "CR", "TF", "RH", "MO"))
projlakes <- projlakes[order(projlakes$lakeID,projlakes$dateSample),]
#select paramters, nitrate, TP, TN, SRP. in ug/L
projlakes <- filter(projlakes, parameter %in% c("TP", "TN", "nitrate", "SRP"))
#create annual mean values
Meanvals <- projlakes %>% group_by(lakeID, parameter, yearsample) %>% summarise(mean(parameterValue))
write.csv(Meanvals, 'lakemeanvalues.csv')
write.csv(projlakes, 'projlakes.csv')
Meanvals <- read.csv(file = 'lakemeanvalues.csv')
Meanvals <- Meanvals[complete.cases(Meanvals[,5]),]
totmeanvals <- Meanvals %>% group_by(lakeID, parameter) %>% summarise(mean(parameterValue))
write.csv(totmeanvals, 'MeanLakeChemValues.csv')

#ZachData
#Goal - Each row represents a tree with all associated values
#in excel I assigned each tree a lakeID value, and seperated the tabs into different spreadsheets to make it easier to read into R
#Also adjusted the Jones Sample spreadsheet
#read in files
Treedata <- read_xlsx("TreeSamples.xlsx")
LakeSamples <- read_xlsx("LakeSamples.xlsx")
JonesLakeData <- read_xlsx("JonesLakeData.xlsx")
#First create mean values for our lake samples pH + conductivity
library(dplyr)
meanlakepH <- LakeSamples %>% group_by(LakeID) %>% summarise(mean(pH))
meanlakecon <- LakeSamples %>% group_by(LakeID) %>% summarise(mean(Conductivity))
#merge data into one dataframe
Treedata <- merge(Treedata, meanlakepH, by = "LakeID", all.x = TRUE)
Treedata<- merge(Treedata, meanlakecon, by = "LakeID", all.x = TRUE)
Treedata <- merge(Treedata, JonesLakeData, by = "LakeID", all.x = TRUE)
#write finished dataframe into a csv file(openable in excel)
write.csv(Treedata, "AllTreeData.csv")
#data visualization
AllTree <- read.csv(file = "AllTreeData.csv")
BayData <- AllTree  %>% filter(AllTree$LakeID == "BA")
SLAvNitrate <- lm(SLA ~ nitrate, data = BayData)
plot(SLAvNitrate)
plot(BayData$SLA, BayData)
SLAcomp <- dplyr::select(AllTree, 4, 11, 17)
mod <- lm(SLA~nitrate, data=AllTree)
plot(AllTree$SLA~AllTree$nitrate)
abline(mod, col="red")
text(SLAcomp$SLA~SLAcomp$nitrate , labels = SLAcomp$ID, data = SLAcomp, font=1, pos=1)
summary(mod)

AllTree <- AllTree %>% rename( "pH" = `mean.PH.`)
AllTree <- AllTree %>% rename( "pH" = `mean.PH.`)
AllTree <- AllTree %>% rename( "pH" = `mean.PH.`)

mod2 <- lm(FLUORO.ETR.umol1m2s1~mean.PH, data = AllTree)
plot(AllTree$FLUORO.ETR.umol1m2s1~AllTree$mean.pH)
abline(mod2, col="red")

SLAAov <- aov(SLA~nitrate, data = AllTree)
summary(SLAAov)
