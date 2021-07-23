setwd("~/Desktop/NEON/NEON_litterfall/NeonLitter")
litcarb16 <- read.csv(file = 'NEON.litterCarbon2016-10.csv')
litcarb19 <- read.csv(file = 'NEON.litterCarbon2019-10.csv')
littrap <- read.csv(file = 'NEON.litterpertrap.csv')
litmass16.05 <- read.csv(file = 'NEON.littermass2016-05.csv')
litmass16.09 <- read.csv(file = 'NEON.littermass2016-09.csv')
litmass16.10 <- read.csv(file = 'NEON.littermass2016-10.csv')
litmass17.05 <- read.csv(file = 'NEON.littermass2017-05.csv')
litmass17.08 <- read.csv(file = 'NEON.littermass2017-08.csv')
litmass17.09 <- read.csv(file = 'NEON.littermass2017-09.csv')
litmass17.10 <- read.csv(file = 'NEON.littermass2017-10.csv')
litmass18.05 <- read.csv(file = 'NEON.littermass2018-05.csv')
litmass18.08 <- read.csv(file = 'NEON.littermass2018-08.csv')
litmass18.09 <- read.csv(file = 'NEON.littermass2018-09.csv')
litmass18.10 <- read.csv(file = 'NEON.littermass2018-10.csv')
litmass19.05 <- read.csv(file = 'NEON.littermass2019-05.csv')
litmass19.09 <- read.csv(file = 'NEON.littermass2019-09.csv')
litmass19.10 <- read.csv(file = 'NEON.littermass2019-10.csv')
litmass19.11 <- read.csv(file = 'NEON.littermass2019-11.csv')
litmass20.06 <- read.csv(file = 'NEON.littermass2020-06.csv')
litmass20.09 <- read.csv(file = 'NEON.littermass2020-09.csv')
litmass20.10 <- read.csv(file = 'NEON.littermass2020-10.csv')

litcarb16 <- litcarb16 %>%arrange(litcarb16[,6])
litcarb19 <- litcarb19 %>%arrange(litcarb19[,6])
litcarb16[nrow(litcarb16)+3,]
litcarb16 <- select(litcarb16, 6,18)
litcarb19 <- select(litcarb19, 6,18)
litcarball <- cbind(litcarb16, litcarb19)
#carbon percentage of litter calcuated as average for each plot from 2016 and 2019 surveys
meanlitcarb <- read.csv(file = 'NEON.litterCarbonMean.csv')

litmass16 <- rbind(litmass16.05, litmass16.09, litmass16.10)
litmass17 <- rbind(litmass17.05, litmass17.08, litmass17.09, litmass17.10)
litmass18 <- rbind(litmass18.05, litmass18.08, litmass18.09, litmass18.10)
litmass19 <- rbind(litmass19.05, litmass19.09, litmass19.10, litmass19.11)
litmass20 <- rbind(litmass20.06, litmass20.09, litmass20.10)

litmass16 <- dplyr::select(litmass16, 5,6,9,17,18)
litmass17 <- dplyr::select(litmass17, 5,6,9,17,18)
litmass18 <- dplyr::select(litmass18, 5,6,9,17,18)  
litmass19 <- dplyr::select(litmass19, 5,6,9,17,18)  
litmass20 <- dplyr::select(litmass20, 5,6,9,17,18)  

Cpercent16 <- merge(litmass16, meanlitcarb, by = "plotID", all.x = TRUE)
Cpercent17 <- merge(litmass17, meanlitcarb, by = "plotID", all.x =TRUE)
Cpercent18 <- merge(litmass18, meanlitcarb, by = "plotID", all.x = TRUE)
Cpercent19 <- merge(litmass19, meanlitcarb, by = "plotID", all.x = TRUE)
Cpercent20 <- merge(litmass20, meanlitcarb, by = "plotID", all.x = TRUE)

littrap <- dplyr::select(littrap, 7,21)

Cpercent16 <- merge(Cpercent16, littrap, by = "trapID", all.x = TRUE)
Cpercent17 <- merge(Cpercent17, littrap, by = "trapID", all.x =TRUE)
Cpercent18 <- merge(Cpercent18, littrap, by = "trapID", all.x = TRUE)
Cpercent19 <- merge(Cpercent19, littrap, by = "trapID", all.x = TRUE)
Cpercent20 <- merge(Cpercent20, littrap, by = "trapID", all.x = TRUE)

Cpercent16 <- Cpercent16[order(Cpercent16$trapID,Cpercent16$collectDate),]
Cpercent17 <- Cpercent17[order(Cpercent17$trapID,Cpercent17$collectDate),]
Cpercent18 <- Cpercent18[order(Cpercent18$trapID,Cpercent18$collectDate),]
Cpercent19 <- Cpercent19[order(Cpercent19$trapID,Cpercent19$collectDate),]
Cpercent20 <- Cpercent20[order(Cpercent20$trapID,Cpercent20$collectDate),]

Cpercent16$meanCarbonPercent <- (Cpercent16$meanCarbonPercent)/100
Cpercent17$meanCarbonPercent <- (Cpercent17$meanCarbonPercent)/100
Cpercent18$meanCarbonPercent <- (Cpercent18$meanCarbonPercent)/100
Cpercent19$meanCarbonPercent <- (Cpercent19$meanCarbonPercent)/100
Cpercent20$meanCarbonPercent <- (Cpercent20$meanCarbonPercent)/100


#calculate carbon in grams per meter
# C = Carbon%(Dry weight/trap area) + fine woody debris(not present)
carbcalc16 = matrix(NA,ncol=1,nrow=length(Cpercent16[,1]))
for(i in 1:length(Cpercent16[,1])){
  carbcalc16[i,1] = (Cpercent16[i,6]*(Cpercent16[i,5]/Cpercent16[i,7]))
}
colnames(carbcalc16)=("Litter Carbon")
LitterCarbon16 <- cbind(Cpercent16,carbcalc16)
#repeat for all years
#remove NAs
LitterCarbon16 <- LitterCarbon16[complete.cases(LitterCarbon16[,5]),]
LitterCarbon17 <- LitterCarbon17[complete.cases(LitterCarbon17[,5]),]
LitterCarbon18 <- LitterCarbon18[complete.cases(LitterCarbon18[,5]),]
LitterCarbon19 <- LitterCarbon19[complete.cases(LitterCarbon19[,5]),]
LitterCarbon20 <- LitterCarbon20[complete.cases(LitterCarbon20[,5]),]
#turn per trap to per meter- divide by trap size again
permetercarb16 = matrix(NA,ncol=1,nrow=length(LitterCarbon16[,1]))
for(i in 1:length(LitterCarbon16[,1])){
  permetercarb16[i,1] = (LitterCarbon16[i,8])/(LitterCarbon16[i,7])
}
colnames(permetercarb16)=("CarbonPerMeter")
LitterCarbon16 <- cbind(LitterCarbon16,permetercarb16)
#Group by date and trap ID, sum trap id, mean date
meanday <- LitterCarbon16 %>% group_by(plotID,collectDate,trapID) %>% summarise(sum(dryMass))
meanday <- meanday[order(meanday$trapID),]
meanday <- meanday %>% group_by(plotID, trapID) %>% summarise(mean(`sum(dryMass)`))
names(meanday)[3] <- "AverageCarbonPerTrap"
# repeat for other years
meanday17 <- LitterCarbon17 %>% group_by(plotID,collectDate,trapID) %>% summarise(sum(CarbonPerMeter))
meanday17 <- meanday17[order(meanday17$trapID),]
meanday17 <- meanday17 %>% group_by(plotID, trapID) %>% summarise(mean(`sum(CarbonPerMeter)`))
names(meanday17)[3] <- "AverageCarbonPMPT"
#Calculte per plot mean then multiply by plot size in meters(1600m for tower plots, 400m for distributed plots)
PlotAnnualCarb16 <- meanday16 %>% group_by(plotID) %>% summarise(mean(AverageCarbonPMPT))
PlotAnnualCarb17 <- meanday17 %>% group_by(plotID) %>% summarise(mean(AverageCarbonPMPT))
PlotAnnualCarb18 <- meanday18 %>% group_by(plotID) %>% summarise(mean(AverageCarbonPMPT))
PlotAnnualCarb19 <- meanday19 %>% group_by(plotID) %>% summarise(mean(AverageCarbonPMPT))
PlotAnnualCarb20 <- meanday20 %>% group_by(plotID) %>% summarise(mean(AverageCarbonPMPT))

PlotAnnualCarb16 <- cbind(PlotAnnualCarb16, 1600*(PlotAnnualCarb16$`mean(AverageCarbonPMPT)`))
PlotAnnualCarb17 <- cbind(PlotAnnualCarb17, 1600*(PlotAnnualCarb17$`mean(AverageCarbonPMPT)`))
PlotAnnualCarb18 <- cbind(PlotAnnualCarb18, 1600*(PlotAnnualCarb18$`mean(AverageCarbonPMPT)`))
PlotAnnualCarb19 <- cbind(PlotAnnualCarb19, 1600*(PlotAnnualCarb19$`mean(AverageCarbonPMPT)`))
PlotAnnualCarb20 <- cbind(PlotAnnualCarb20, 1600*(PlotAnnualCarb20$`mean(AverageCarbonPMPT)`))
names(PlotAnnualCarb16)[3] <- "TotalPlotLitterCarbonPM"
names(PlotAnnualCarb17)[3] <- "TotalPlotLitterCarbonPM"
names(PlotAnnualCarb18)[3] <- "TotalPlotLitterCarbonPM"
names(PlotAnnualCarb19)[3] <- "TotalPlotLitterCarbonPM"
names(PlotAnnualCarb20)[3] <- "TotalPlotLitterCarbonPM"
#covert grams to kg -> final litter by plot in kg

PlotAnnualCarb16$TotalPlotLitterCarbonPM <- (PlotAnnualCarb16$TotalPlotLitterCarbonPM)/1000
PlotAnnualCarb17$TotalPlotLitterCarbonPM <- (PlotAnnualCarb17$TotalPlotLitterCarbonPM)/1000
PlotAnnualCarb18$TotalPlotLitterCarbonPM <- (PlotAnnualCarb18$TotalPlotLitterCarbonPM)/1000
PlotAnnualCarb19$TotalPlotLitterCarbonPM <- (PlotAnnualCarb19$TotalPlotLitterCarbonPM)/1000
PlotAnnualCarb20$TotalPlotLitterCarbonPM <- (PlotAnnualCarb20$TotalPlotLitterCarbonPM)/1000

#writecsv
write.csv(PlotAnnualCarb16,"Litter Annual Carbon 16.csv")
write.csv(PlotAnnualCarb17,"Litter Annual Carbon 17.csv")
write.csv(PlotAnnualCarb18,"Litter Annual Carbon 18.csv")
write.csv(PlotAnnualCarb19,"Litter Annual Carbon 19.csv")
write.csv(PlotAnnualCarb20,"Litter Annual Carbon 20.csv")

#litter carbon per square meter x total site
#mean litter per m^2 across all plots x total site -> kg/m^2 of carbon
#total forested area = 23800348m^2
areacarb16 <- (mean(PlotAnnualCarb16$`mean(AverageCarbonPMPT)`)*23800348)/1000
areacarb17 <- (mean(PlotAnnualCarb17$`mean(AverageCarbonPMPT)`)*23800348)/1000
areacarb18 <- (mean(PlotAnnualCarb18$`mean(AverageCarbonPMPT)`)*23800348)/1000
areacarb19 <- (mean(PlotAnnualCarb19$`mean(AverageCarbonPMPT)`)*23800348)/1000
areacarb20 <- (mean(PlotAnnualCarb20$`mean(AverageCarbonPMPT)`)*23800348)/1000
totareacarb <- as.data.frame(rbind(areacarb16, areacarb17, areacarb18, areacarb19, areacarb20))
names(totareacarb)=("Annual litter carbon kg/m2")
write.csv(totareacarb, "AnnualTotalLitterCarbon.csv")


