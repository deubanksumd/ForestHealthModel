#Data visualization
setwd("~/Desktop/NEON")
rm(list = ls())
litter <- read.csv(file = "AnnualTotalLitterCarbon.csv")
tree <- read.csv(file = "AnnualTotalTreeCarbon.csv")
sprich <- read.csv(file = "AnnualSpeciesRichness.csv")
plottree16 <- read.csv(file = "PlotLevelCarb16.csv")
plottree17 <- read.csv(file = "PlotLevelCarb17.csv")
plottree18 <- read.csv(file = "PlotLevelCarb18.csv")
plottree19 <- read.csv(file = "PlotLevelCarb19.csv")
plottree20 <- read.csv(file = "PlotLevelCarb20.csv")
plotlitt16 <- read.csv(file = "Litter Annual Carbon 16.csv")
plotlitt17 <- read.csv(file = "Litter Annual Carbon 17.csv")
plotlitt18 <- read.csv(file = "Litter Annual Carbon 18.csv")
plotlitt19 <- read.csv(file = "Litter Annual Carbon 19.csv")
plotlitt20 <- read.csv(file = "Litter Annual Carbon 20.csv")
Species16 <- read.csv(file = "specieslist16.csv")
Species17 <- read.csv(file = "specieslist17.csv")
Species18 <- read.csv(file = "specieslist18.csv")
Species19 <- read.csv(file = "specieslist19.csv")
Species20 <- read.csv(file = "specieslist20.csv")
Temp16 <- read.csv(file = "2016temp.csv")
Temp17 <- read.csv(file = "2017temp.csv")
Temp18 <- read.csv(file = "2018temp.csv")
Temp19 <- read.csv(file = "2019temp.csv")
Temp20 <- read.csv(file = "2020temp.csv")
Precip17 <- read.csv(file = "2017precip.csv")
Precip18 <- read.csv(file = "2018precip.csv")
Precip19 <- read.csv(file = "2019precip.csv")
Precip20 <- read.csv(file = "2020precip.csv")
totprecip <- read.csv(file = "AnnualPrecip.csv")
#Compare tree, litter to mean temp and mean precip
#Tree v temp
tvt <- cbind(tree, meantempall)
ttmod <- lm(Annual.tree.carbon.kg.m2~V1, data = tvt)
plot(tvt$Annual.tree.carbon.kg.m2~tvt$V1)
abline(ttmod, col = "red")
summary(ttmod)
#tree v precip
tree$years <- c("2016","2017","2018","2019","2020")
totprecip$years <- c("2017", "2018", "2019", "2020")
tvp <-as.data.frame(merge(tree, totprecip, by = "years", all.y = TRUE))
tpmod <- lm(Annual.tree.carbon.kg.m2~totpercip, data = tvp)
plot(tvp$Annual.tree.carbon.kg.m2~tvp$totpercip)
abline(tpmod, col = "red")
summary(tpmod)
#litter v temp
lvt <- cbind(litter, meantempall)
ltmod <- lm(Annual.litter.carbon.kg.m2~V1, data = lvt)
plot(lvt$Annual.litter.carbon.kg.m2~lvt$V1)
abline(ltmod, col = "red")
summary(ltmod)
#litter v percip
litter$years <- c("2016", "2017", "2018", "2019", "2020")
lvp <- as.data.frame(merge(litter, totprecip, by="years", all.y = TRUE))
lpmod <- lm(Annual.litter.carbon.kg.m2~totpercip, data = lvp)
plot(lvp$Annual.litter.carbon.kg.m2~lvp$totpercip)
abline(lpmod, col="red")
summary(lpmod)

#species richness v temp
AnnSpec <- as.data.frame(rbind(length(Species16[,1]), length(Species17[,1]), length(Species18[,1]),
                 length(Species19[,1]), length(Species20[,1])))
AnnSpec$years <- c("2016", "2017", "2018", "2019", "2020")
Spvt <- as.data.frame(cbind(AnnSpec, meantempall))
names(Spvt)[1] = "SPR"
stmod <- lm(SPR~V1, data = Spvt)
plot(Spvt$SPR~Spvt$V1)
abline(stmod, col="red")
summary(stmod)

#species richness v precip
Spvp <- as.data.frame(merge(AnnSpec, totprecip, by="years", all.y=TRUE))
spmod <- lm(SPR~totpercip, data=Spvp)
plot(Spvp$SPR~Spvp$totpercip)
abline(spmod, col="red")
summary(spmod)

#species richness v tree
Spvtr <- as.data.frame(cbind(AnnSpec, tree))
sptmod <- lm(Annual.tree.carbon.kg.m2~SPR, data=Spvtr)
plot(Spvtr$Annual.tree.carbon.kg.m2~Spvtr$SPR)
abline(sptmod, col="red")
summary(sptmod)

#species richness v litter
Spvl <- as.data.frame(cbind(AnnSpec, litter))
splmod <- lm(Annual.litter.carbon.kg.m2~SPR, data = Spvl)
plot(Spvl$Annual.litter.carbon.kg.m2~Spvl$SPR)
abline(splmod, col="red")
summary(splmod)
                                        
#try it flipped
tvt <- cbind(tree, meantempall)
ttmod <- lm(V1~Annual.tree.carbon.kg.m2, data = tvt)
plot(tvt$V1~tvt$Annual.tree.carbon.kg.m2, main="Tree Carbon vs Mean Temperature",
     xlab="Above+Below Ground Tree Carbon in kg/m2", ylab="Mean Annual Temperature(C)")
tvt$years <- c("2016", "2017", "2018", "2019", "2020")
text(tvt$V1~tvt$Annual.tree.carbon.kg.m2 , labels = tvt$years, data = tvt, font=1, pos=1)
abline(ttmod, col = "red")
summary(ttmod)
#tree v precip
tree$years <- c("2016","2017","2018","2019","2020")
totprecip$years <- c("2017", "2018", "2019", "2020")
tvp <-as.data.frame(merge(tree, totprecip, by = "years", all.y = TRUE))
tpmod <- lm(totpercip~Annual.tree.carbon.kg.m2, data = tvp)
plot(tvp$totpercip~tvp$Annual.tree.carbon.kg.m2, main="Tree Carbon vs Annual Precipitation",
     xlab="Above+Below Ground Tree Carbon in kg/m2", ylab="Annual Precipitation(mm)")
text(tvp$totpercip~tvp$Annual.tree.carbon.kg.m2 , labels = tvp$years, data = tvp, font=1, pos=1)
abline(tpmod, col = "red")
summary(tpmod)
#litter v temp
lvt <- cbind(litter, meantempall)
ltmod <- lm(V1~Annual.litter.carbon.kg.m2, data = lvt)
lvt$years <- c("2016", "2017", "2018", "2019", "2020")
plot(lvt$V1~lvt$Annual.litter.carbon.kg.m2, main="Litter Carbon vs Mean Temperature",
     xlab="Litter Carbon in kg/m2", ylab="Mean Annual Temperature(C)")
text(lvt$V1~lvt$Annual.litter.carbon.kg.m2 , labels = lvt$years, data = lvt, font=1, pos=1)
abline(ltmod, col = "red")
summary(ltmod)
#litter v percip
litter$years <- c("2016", "2017", "2018", "2019", "2020")
lvp <- as.data.frame(merge(litter, totprecip, by="years", all.y = TRUE))
lpmod <- lm(totpercip~Annual.litter.carbon.kg.m2, data = lvp)
plot(lvp$totpercip~lvp$Annual.litter.carbon.kg.m2, main="Litter Carbon vs Annual Precipitaiton",
     xlab="Litter Carbon in kg/m2", ylab="Annual Precipitation(mm)")
text(lvp$totpercip~lvp$Annual.litter.carbon.kg.m2 , labels = lvp$years, data = lvp, font=1, pos=1)
abline(lpmod, col="red")
summary(lpmod)

#species richness v temp
AnnSpec <- as.data.frame(rbind(length(Species16[,1]), length(Species17[,1]), length(Species18[,1]),
                               length(Species19[,1]), length(Species20[,1])))
AnnSpec$years <- c("2016", "2017", "2018", "2019", "2020")
Spvt <- as.data.frame(cbind(AnnSpec, meantempall))
names(Spvt)[1] = "SPR"
stmod <- lm(meantempall~SPR, data = Spvt)
plot(Spvt$meantempall~Spvt$SPR, main="Species Richness v Mean Temperature",
     xlab="Species Richness", ylab="Mean Annual Temperature(C)")
text(Spvt$meantempall~Spvt$SPR , labels = Spvt$years, data = Spvt, font=1, pos=1)
abline(stmod, col="red")
summary(stmod)

#species richness v precip
Spvp <- as.data.frame(merge(AnnSpec, totprecip, by="years", all.y=TRUE))
spmod <- lm(totpercip~SPR, data=Spvp)
plot(Spvp$totpercip~Spvp$SPR)
abline(spmod, col="red")
summary(spmod)

#species richness v tree
Spvtr <- as.data.frame(cbind(AnnSpec, tree))
sptmod <- lm(V1~Annual.tree.carbon.kg.m2, data=Spvtr)
plot(Spvtr$V1~Spvtr$Annual.tree.carbon.kg.m2, main="Species Richness vs Tree Carbon",
     xlab="Above+Below Ground Tree Carbon in kg/m2", ylab="Species Richness")
text(Spvtr$V1~Spvtr$Annual.tree.carbon.kg.m2 , labels = Spvtr$years, data = Spvtr, font=1, pos=1)
abline(sptmod, col="red")
summary(sptmod)

#species richness v litter
Spvl <- as.data.frame(cbind(AnnSpec, litter))
splmod <- lm(SPR~Annual.litter.carbon.kg.m2, data = Spvl)
plot(Spvl$SPR~Spvl$Annual.litter.carbon.kg.m2)
abline(splmod, col="red")
summary(splmod)


