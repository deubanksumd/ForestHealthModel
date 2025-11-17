#Data collection and organization before running bayesian models
#set working document to where FIA files will/are be held
setwd("~/Desktop/FIA/")
#required packages
library(raster)
library(terra)
library(dismo)
library(geodata)
library(dplyr)
library(sf)
library(rFIA)
library(parallel)
library(hillR)
library(rlist)
library(purrr)
library(proxy)
library(vegan)
library(picante)
library(FedData)
library(landscapemetrics)
library(future)
library(furrr)
library(soilDB)
library(ncdf4)
library(sp)
library(con2aqi)
library(plyr)
library(tidyr)
library(ggplot2)
library(foreach)
library(doParallel)
library(ncdf4)
library(exactextractr)
library(ggplot2)
library(ggspatial)
#name FIA directory as current directory
fiadir <- getwd()
options(timeout=3600)
cores = 7
#read in mid atlantic states data
states = c('MD','DE','NJ','NY','PA')
getFIA(states='MD',dir=fiadir)
getFIA(states='DE',dir=fiadir)
getFIA(states='NJ',dir=fiadir)
getFIA(states='NY',dir=fiadir)
getFIA(states='PA',dir=fiadir)
midAtl <- readFIA(fiadir, states=states, inMemory = TRUE, nCores=cores)

#carbon estimates
Carb <- carbon(db=midAtl, method="annual",byPlot = TRUE, byComponent = TRUE)
#Sum Aboveground+below ground+soil and litter- dead wood
Carbpp <- Carb %>% group_by(YEAR, PLT_CN) %>% dplyr::summarise(CarbSum = sum(CARB_ACRE[POOL%in%c("AG_LIVE","BG_LIVE","LITTER","SOIL_ORG")])-
                                                                 sum(CARB_ACRE[POOL%in%c("DEAD_WOOD")]))
names(Carbpp) <- c('Year','PlotCN','CarbonPerAcre')

#HillShannon Calc
#arrange species with plot as row and species as columns
Species <- data.frame(midAtl$TREE$PLT_CN, midAtl$TREE$INVYR ,midAtl$TREE$STATUSCD,midAtl$TREE$SPCD)
#Select only living trees, Status=1
names(Species) <- c("PlotCN","Year","Status","SpeciesCode")
Species <- data.frame(base::subset(Species, Species$Status==1))
Species <- Species%>%group_by(PlotCN)
Species <- Species[-c(3)]
Species <- as.data.frame(subset(Species, Species$Year>1999))

tlist <- Species %>%
  select(PlotCN, col3 = 3) %>%       
  split(.$PlotCN) %>%       
  map(~ .x$col3)  

flist <- tlist[!duplicated(names(tlist))]
flist <- rlist::list.subset(flist, names(flist)%in%Carbpp$PlotCN) 

SpeciesCodes <- data.frame(unique(Species$SpeciesCode))
#number of plots and number of species
Hilldata <- data.frame(matrix(0,27155,176))
colnames(Hilldata) <- SpeciesCodes [,1]
rownames(Hilldata) <- names(flist)

#create species dataframe
specmatrix <- outer(
  flist, 
  names(Hilldata), 
  FUN = Vectorize(function(f, col) sum(f == col))
)
Hilldata[1:length(flist), ] <- specmatrix

write.csv(Hilldata, file="SpeciesMatrix.csv")
#Run hill_taxa() from Hill R package to calculate Hill Shannon diversity index, q=1 
Shannon <- data.frame(hill_taxa(comm = Hilldata, q=1))
Shannon$PlotCN <- rownames(Shannon)
names(Shannon)[1] = "HillShannonIndex"

#Jaccards Similarity
jacdataAll <- numeric(nrow(Hilldata))
names(jacdataAll) <- rownames(Hilldata)
# Calculate full distance matrix
dist_matrix <- as.matrix(proxy::dist(Hilldata, method = "Jaccard"))
# Calculate row means 
jacdataAll <- rowMeans(dist_matrix, na.rm = TRUE)
# Convert to data frame 
jacdataAll <- data.frame(JaccardMean = jacdataAll)
jacdataAll$PlotCN <- rownames(jacdataAll)

#tree height
Tree <- data.frame(base::subset(midAtl$TREE, midAtl$TREE$INVYR>1999))
TreeSub<- Tree[,c(2,4,13,14,18)]
TreeSub <- data.frame(base::subset(TreeSub, TreeSub$STATUSCD==1))
names(TreeSub)<- c("PlotCN","Year","Status","SpeciesCode","Height")
SpecMeanHT <- TreeSub%>%group_by(SpeciesCode) %>%summarise(
  AvgHeight = mean(Height, na.rm=TRUE)
)

#Mean Pairwise Distance
Hilldata1 <- select(Hilldata, -c("999"))
#Replace species codes with Scientific names
spcd <- as.data.frame(colnames(Hilldata1))
names(spcd)[1]<- "SPCD"
speccodesnames <- read.csv("REF_SPECIES.csv")
speccodesnames <- speccodesnames %>% mutate(SCI_NAME = paste(GENUS, SPECIES, VARIETY,sep=" "))
spcd1 <- merge(spcd, speccodesnames[,c(1,7)], by="SPCD", all.x=TRUE)
reorder<- match(spcd$SPCD,spcd1$SPCD)
names(spcd1)[1] <- "SpeciesCode"
colnames(Hilldata1)<-spcd1$SCI_NAME[reorder]
#remove sparse species
Hilldata1 <- Hilldata1%>%select_if(colSums(Hilldata1)>20)
#mean pairwise distance
#adjust trait matrix to all species
Trait <- read.csv("MDTraitMatrix.csv", row.names=1,check.names=FALSE)
Trait <- Trait[-c(5)]
Trait <- merge(Trait,SpecMeanHT, by="SpeciesCode", all.y=TRUE)
Trait <- merge(Trait,spcd1, by="SpeciesCode")
Trait <- Trait[-c(2)]
Trait1 <- subset(Trait, SCI_NAME%in%colnames(Hilldata1))
#write traits, fill from TRY database. TRY database excel format not compatible with R, but included in github data section.
write.csv(Trait1,"FIATraitMatrix.csv")
#Read back in and complete calculations
Trait1 <- read.csv("FIATraitMatrix.csv")
Trait1 <- Trait1[-c(1)] #remove empty column
Trait1CC <- Trait1[complete.cases(Trait1[]),]
Trait1CC <- Trait1CC[-c(1)] #remove species codes
rownames(Trait1CC) <- Trait1CC$SCI_NAME
Trait1CC <- Trait1CC[-c(4)] #remove SciName, only after naming rows
#log scale trait data to lower bias 
TraitScale <- apply(log(Trait1CC), MARGIN = 2, scale)
Tpc <- princomp(TraitScale)
pcScore <- Tpc$scores[,1:3]
rownames(pcScore) <- rownames(Trait1CC)
PCdist <- dist(pcScore, method="euclidean")
PCdistx <- as.matrix(PCdist)
#subset species data
SpecMPD <- Hilldata1[colnames(Hilldata1)[colnames(Hilldata1)%in%rownames(pcScore)]]
#Calculate Mean Pairwise Distance
mpd <- data.frame(mpd(SpecMPD, PCdistx, abundance.weighted = TRUE))
mpd$PlotCN <- rownames(SpecMPD)
names(mpd)[1] <- "MeanPairwiseDistance"

#combine carb/hill/jacc/mpd
allFIA <- merge(Carbpp,Shannon, by='PlotCN',all.x=TRUE) %>% 
  merge(jacdataAll, by='PlotCN',all.x=TRUE)%>% 
  merge(mpd, by= 'PlotCN',all.x=TRUE)
#pull coordinates and elevation
PlotData <- data.frame(midAtl$PLOT$CN,midAtl$PLOT$INVYR, midAtl$PLOT$PLOT,midAtl$PLOT$LAT, midAtl$PLOT$LON, midAtl$PLOT$ELEV)
names(PlotData) <- c("PlotCN","Year", "PlotNumber", "Latitude", "Longitude", "Elevation")
#match coordinates to plots, incl plot number
allFIA <- merge(allFIA,PlotData[,c(1,3:6)], all.x=TRUE)
#match forest type
ftype <- data.frame(midAtl$COND$PLT_CN,midAtl$COND$PLOT,midAtl$COND$INVYR, midAtl$COND$FORTYPCD,midAtl$COND$CONDPROP_UNADJ)
names(ftype) <- c("PlotCN","PlotNumber","Year","ForestType","Prop")
ftype <- na.omit(ftype)
#largest condition proportion takes forest type
ftype <- ftype %>% group_by(across(1:3)) %>% slice_max(order_by = Prop, n=1) %>% ungroup()
ftype <- distinct(ftype)
allFIA <- merge(allFIA, ftype[,1:4], by=c('PlotCN','PlotNumber','Year'), all.x=TRUE)
allFIA <- allFIA %>% group_by(PlotCN,PlotNumber,Year)
allFIA <- na.omit(allFIA)
#subset to 2000-2020
allFIA <- subset(allFIA, allFIA$Year<2021)
#make coordinates df, lon/lat
coords <- allFIA[,c(1,3,8,9)]
#pull geo data - nlcd, soil order, air quality, climate
plotyear00 <- filter(coords, Year %in% c("2000"))
plotyear01 <- filter(coords, Year %in% c("2001"))
plotyear02 <- filter(coords, Year %in% c("2002"))
plotyear03 <- filter(coords, Year %in% c("2003"))
plotyear04 <- filter(coords, Year %in% c("2004"))
plotyear05 <- filter(coords, Year %in% c("2005"))
plotyear06 <- filter(coords, Year %in% c("2006"))
plotyear07 <- filter(coords, Year %in% c("2007"))
plotyear08 <- filter(coords, Year %in% c("2008"))
plotyear09 <- filter(coords, Year %in% c("2009"))
plotyear10 <- filter(coords, Year %in% c("2010"))
plotyear11 <- filter(coords, Year %in% c("2011"))
plotyear12 <- filter(coords, Year %in% c("2012"))
plotyear13 <- filter(coords, Year %in% c("2013"))
plotyear14 <- filter(coords, Year %in% c("2014"))
plotyear15 <- filter(coords, Year %in% c("2015"))
plotyear16 <- filter(coords, Year %in% c("2016"))
plotyear17 <- filter(coords, Year %in% c("2017"))
plotyear18 <- filter(coords, Year %in% c("2018"))
plotyear19 <- filter(coords, Year %in% c("2019"))
plotyear20 <- filter(coords, Year %in% c("2020"))

#nlcd class data included in github data section
nlcdclass <- read.csv("nlcdclass.csv")
#include woody wetlands under forest designation
nlcdclass[15,1] <- nlcdclass[8,1]

crs84 <- '+proj=longlat +datum=WGS84'
#crs of nlcd spatraster'
x<-get_nlcd(template = FedData::meve, label = "meve", year = 2016)
xcrs <- crs(x)
#create spatial points data frames
SPDF00 <- SpatialPointsDataFrame(plotyear00[,4:3], data = plotyear00, proj4string =CRS(crs84))
SPDF01 <- SpatialPointsDataFrame(plotyear01[,4:3], data = plotyear01, proj4string =CRS(crs84))
SPDF02 <- SpatialPointsDataFrame(plotyear02[,4:3], data = plotyear02, proj4string =CRS(crs84))
SPDF03 <- SpatialPointsDataFrame(plotyear03[,4:3], data = plotyear03, proj4string =CRS(crs84))
SPDF04 <- SpatialPointsDataFrame(plotyear04[,4:3], data = plotyear04, proj4string =CRS(crs84))
SPDF05 <- SpatialPointsDataFrame(plotyear05[,4:3], data = plotyear05, proj4string =CRS(crs84))
SPDF06 <- SpatialPointsDataFrame(plotyear06[,4:3], data = plotyear06, proj4string =CRS(crs84))
SPDF07 <- SpatialPointsDataFrame(plotyear07[,4:3], data = plotyear07, proj4string =CRS(crs84))
SPDF08 <- SpatialPointsDataFrame(plotyear08[,4:3], data = plotyear08, proj4string =CRS(crs84))
SPDF09 <- SpatialPointsDataFrame(plotyear09[,4:3], data = plotyear09, proj4string =CRS(crs84))
SPDF10 <- SpatialPointsDataFrame(plotyear10[,4:3], data = plotyear10, proj4string =CRS(crs84))
SPDF11 <- SpatialPointsDataFrame(plotyear11[,4:3], data = plotyear11, proj4string =CRS(crs84))
SPDF12 <- SpatialPointsDataFrame(plotyear12[,4:3], data = plotyear12, proj4string =CRS(crs84))
SPDF13 <- SpatialPointsDataFrame(plotyear13[,4:3], data = plotyear13, proj4string =CRS(crs84))
SPDF14 <- SpatialPointsDataFrame(plotyear14[,4:3], data = plotyear14, proj4string =CRS(crs84))
SPDF15 <- SpatialPointsDataFrame(plotyear15[,4:3], data = plotyear15, proj4string =CRS(crs84))
SPDF16 <- SpatialPointsDataFrame(plotyear16[,4:3], data = plotyear16, proj4string =CRS(crs84))
SPDF17 <- SpatialPointsDataFrame(plotyear17[,4:3], data = plotyear17, proj4string =CRS(crs84))
SPDF18 <- SpatialPointsDataFrame(plotyear18[,4:3], data = plotyear18, proj4string =CRS(crs84))
SPDF19 <- SpatialPointsDataFrame(plotyear19[,4:3], data = plotyear19, proj4string =CRS(crs84))
SPDF20 <- SpatialPointsDataFrame(plotyear20[,4:3], data = plotyear20, proj4string =CRS(crs84))
#transform SPDF to correct crs
SPDF00 <- spTransform(SPDF00, crs(xcrs))
SPDF01 <- spTransform(SPDF01, crs(xcrs))
SPDF02 <- spTransform(SPDF02, crs(xcrs))
SPDF03 <- spTransform(SPDF03, crs(xcrs))
SPDF04 <- spTransform(SPDF04, crs(xcrs))
SPDF05 <- spTransform(SPDF05, crs(xcrs))
SPDF06 <- spTransform(SPDF06, crs(xcrs))
SPDF07 <- spTransform(SPDF07, crs(xcrs))
SPDF08 <- spTransform(SPDF08, crs(xcrs))
SPDF09 <- spTransform(SPDF09, crs(xcrs))
SPDF10 <- spTransform(SPDF10, crs(xcrs))
SPDF11 <- spTransform(SPDF11, crs(xcrs))
SPDF12 <- spTransform(SPDF12, crs(xcrs))
SPDF13 <- spTransform(SPDF13, crs(xcrs))
SPDF14 <- spTransform(SPDF14, crs(xcrs))
SPDF15 <- spTransform(SPDF15, crs(xcrs))
SPDF16 <- spTransform(SPDF16, crs(xcrs))
SPDF17 <- spTransform(SPDF17, crs(xcrs))
SPDF18 <- spTransform(SPDF18, crs(xcrs))
SPDF19 <- spTransform(SPDF19, crs(xcrs))
SPDF20 <- spTransform(SPDF20, crs(xcrs))
#convert SPDF to sf format to use st_buffer()
nlsp00 <- st_as_sf(SPDF00)
st_crs(nlsp00) <- st_crs(xcrs)
nlsp01 <- st_as_sf(SPDF01)
st_crs(nlsp01) <- st_crs(xcrs)
nlsp02 <- st_as_sf(SPDF02)
st_crs(nlsp02) <- st_crs(xcrs)
nlsp03 <- st_as_sf(SPDF03)
st_crs(nlsp03) <- st_crs(xcrs)
nlsp04 <- st_as_sf(SPDF04)
st_crs(nlsp04) <- st_crs(xcrs)
nlsp05 <- st_as_sf(SPDF05)
st_crs(nlsp05) <- st_crs(xcrs)
nlsp06 <- st_as_sf(SPDF06)
st_crs(nlsp06) <- st_crs(xcrs)
nlsp07 <- st_as_sf(SPDF07)
st_crs(nlsp07) <- st_crs(xcrs)
nlsp08 <- st_as_sf(SPDF08)
st_crs(nlsp08) <- st_crs(xcrs)
nlsp09 <- st_as_sf(SPDF09)
st_crs(nlsp09) <- st_crs(xcrs)
nlsp10 <- st_as_sf(SPDF10)
st_crs(nlsp10) <- st_crs(xcrs)
nlsp11 <- st_as_sf(SPDF11)
st_crs(nlsp11) <- st_crs(xcrs)
nlsp12 <- st_as_sf(SPDF12)
st_crs(nlsp12) <- st_crs(xcrs)
nlsp13 <- st_as_sf(SPDF13)
st_crs(nlsp13) <- st_crs(xcrs)
nlsp14 <- st_as_sf(SPDF14)
st_crs(nlsp14) <- st_crs(xcrs)
nlsp15 <- st_as_sf(SPDF15)
st_crs(nlsp15) <- st_crs(xcrs)
nlsp16 <- st_as_sf(SPDF16)
st_crs(nlsp16) <- st_crs(xcrs)
nlsp17 <- st_as_sf(SPDF17)
st_crs(nlsp17) <- st_crs(xcrs)
nlsp18 <- st_as_sf(SPDF18)
st_crs(nlsp18) <- st_crs(xcrs)
nlsp19 <- st_as_sf(SPDF19)
st_crs(nlsp19) <- st_crs(xcrs)
nlsp20 <- st_as_sf(SPDF20)
st_crs(nlsp20) <- st_crs(xcrs)
#add buffer, r=118.5m, a=4.41ha
nlsp01 <- st_buffer(nlsp01, dist = 118.5)
nlsp02 <- st_buffer(nlsp02, dist = 118.5)
nlsp03 <- st_buffer(nlsp03, dist = 118.5)
nlsp04 <- st_buffer(nlsp04, dist = 118.5)
nlsp05 <- st_buffer(nlsp05, dist = 118.5)
nlsp06 <- st_buffer(nlsp06, dist = 118.5)
nlsp07 <- st_buffer(nlsp07, dist = 118.5)
nlsp08 <- st_buffer(nlsp08, dist = 118.5)
nlsp09 <- st_buffer(nlsp09, dist = 118.5)
nlsp10 <- st_buffer(nlsp10, dist = 118.5)
nlsp11 <- st_buffer(nlsp11, dist = 118.5)
nlsp12 <- st_buffer(nlsp12, dist = 118.5)
nlsp13 <- st_buffer(nlsp13, dist = 118.5)
nlsp14 <- st_buffer(nlsp14, dist = 118.5)
nlsp15 <- st_buffer(nlsp15, dist = 118.5)
nlsp16 <- st_buffer(nlsp16, dist = 118.5)
nlsp17 <- st_buffer(nlsp17, dist = 118.5)
nlsp18 <- st_buffer(nlsp18, dist = 118.5)
nlsp19 <- st_buffer(nlsp19, dist = 118.5)
nlsp20 <- st_buffer(nlsp20, dist = 118.5)


#pull nlcd data, create 7x7 moving window
#As of May 2nd, 2025 the web host for get_nlcd_annual was frozen by the federal government. Temporary code fix can be found on FedData github
#https://github.com/ropensci/FedData/issues/125
#2000
movinglist <- list()
for(i in 1:length(nlsp00$PlotCN)){
  x <- get_nlcd_annual(template = nlsp00$geometry[i], label = nlsp00$PlotCN[i], year = 2000)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp00$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp00$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea00 <- data.frame(unlist(fadlist))
forestarea00$PlotCN <- names(fadlist)
names(forestarea00)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea00$FADClass <- ifelse(forestarea00$FAD >=90, 5,
                                ifelse(forestarea00$FAD >=60, 4,
                                       ifelse(forestarea00$FAD >=40, 3,
                                              ifelse(forestarea00$FAD>=10, 2, 1))))
forestarea00$PlotCN<- as.numeric(forestarea00$PlotCN)
#2001
movinglist <- list()
for(i in 1:length(nlsp01$PlotCN)){
  x <- get_nlcd_annual(template = nlsp01$geometry[i], label = nlsp01$PlotCN[i], year = 2001)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp01$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp01$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea01 <- data.frame(unlist(fadlist))
forestarea01$PlotCN <- names(fadlist)
names(forestarea01)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea01$FADClass <- ifelse(forestarea01$FAD >=90, 5,
                               ifelse(forestarea01$FAD >=60, 4,
                                      ifelse(forestarea01$FAD >=40, 3,
                                             ifelse(forestarea01$FAD>=10, 2, 1))))
forestarea01$PlotCN<- as.numeric(forestarea01$PlotCN)
#repeat for other years
movinglist <- list()
for(i in 1:length(nlsp02$PlotCN)){
  x <- get_nlcd_annual(template = nlsp02$geometry[i], label = nlsp02$PlotCN[i], year = 2002)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp02$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp02$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea02 <- data.frame(unlist(fadlist))
forestarea02$PlotCN <- names(fadlist)
names(forestarea02)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea02$FADClass <- ifelse(forestarea02$FAD >=90, 5,
                                ifelse(forestarea02$FAD >=60, 4,
                                       ifelse(forestarea02$FAD >=40, 3,
                                              ifelse(forestarea02$FAD>=10, 2, 1))))
forestarea02$PlotCN<- as.numeric(forestarea02$PlotCN)
#2003
movinglist <- list()
for(i in 1:length(nlsp03$PlotCN)){
  x <- get_nlcd_annual(template = nlsp03$geometry[i], label = nlsp03$PlotCN[i], year = 2003)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp03$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp03$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea03 <- data.frame(unlist(fadlist))
forestarea03$PlotCN <- names(fadlist)
names(forestarea03)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea03$FADClass <- ifelse(forestarea03$FAD >=90, 5,
                                ifelse(forestarea03$FAD >=60, 4,
                                       ifelse(forestarea03$FAD >=40, 3,
                                              ifelse(forestarea03$FAD>=10, 2, 1))))
forestarea03$PlotCN<- as.numeric(forestarea03$PlotCN)

#2004
movinglist <- list()
for(i in 1:length(nlsp04$PlotCN)){
  x <- get_nlcd_annual(template = nlsp04$geometry[i], label = nlsp04$PlotCN[i], year = 2004)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp04$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp04$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea04 <- data.frame(unlist(fadlist))
forestarea04$PlotCN <- names(fadlist)
names(forestarea04)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea04$FADClass <- ifelse(forestarea04$FAD >=90, 5,
                                ifelse(forestarea04$FAD >=60, 4,
                                       ifelse(forestarea04$FAD >=40, 3,
                                              ifelse(forestarea04$FAD>=10, 2, 1))))
forestarea04$PlotCN<- as.numeric(forestarea04$PlotCN)

#2005
movinglist <- list()
for(i in 1:length(nlsp05$PlotCN)){
  x <- get_nlcd_annual(template = nlsp05$geometry[i], label = nlsp05$PlotCN[i], year = 2005)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp05$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp05$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea05 <- data.frame(unlist(fadlist))
forestarea05$PlotCN <- names(fadlist)
names(forestarea05)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea05$FADClass <- ifelse(forestarea05$FAD >=90, 5,
                                ifelse(forestarea05$FAD >=60, 4,
                                       ifelse(forestarea05$FAD >=40, 3,
                                              ifelse(forestarea05$FAD>=10, 2, 1))))
forestarea05$PlotCN<- as.numeric(forestarea05$PlotCN)
#2006
movinglist <- list()
for(i in 1:length(nlsp06$PlotCN)){
  x <- get_nlcd_annual(template = nlsp06$geometry[i], label = nlsp06$PlotCN[i], year = 2006)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp06$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp06$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea06 <- data.frame(unlist(fadlist))
forestarea06$PlotCN <- names(fadlist)
names(forestarea06)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea06$FADClass <- ifelse(forestarea06$FAD >=90, 5,
                                ifelse(forestarea06$FAD >=60, 4,
                                       ifelse(forestarea06$FAD >=40, 3,
                                              ifelse(forestarea06$FAD>=10, 2, 1))))
forestarea06$PlotCN<- as.numeric(forestarea06$PlotCN)
#2007
movinglist <- list()
for(i in 1:length(nlsp07$PlotCN)){
  x <- get_nlcd_annual(template = nlsp07$geometry[i], label = nlsp07$PlotCN[i], year = 2007)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp07$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp07$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea07 <- data.frame(unlist(fadlist))
forestarea07$PlotCN <- names(fadlist)
names(forestarea07)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea07$FADClass <- ifelse(forestarea07$FAD >=90, 5,
                                ifelse(forestarea07$FAD >=60, 4,
                                       ifelse(forestarea07$FAD >=40, 3,
                                              ifelse(forestarea07$FAD>=10, 2, 1))))
forestarea07$PlotCN<- as.numeric(forestarea07$PlotCN)
#2008
movinglist <- list()
for(i in 1:length(nlsp08$PlotCN)){
  x <- get_nlcd_annual(template = nlsp08$geometry[i], label = nlsp08$PlotCN[i], year = 2008)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp08$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp08$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea08 <- data.frame(unlist(fadlist))
forestarea08$PlotCN <- names(fadlist)
names(forestarea08)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea08$FADClass <- ifelse(forestarea08$FAD >=90, 5,
                                ifelse(forestarea08$FAD >=60, 4,
                                       ifelse(forestarea08$FAD >=40, 3,
                                              ifelse(forestarea08$FAD>=10, 2, 1))))
forestarea08$PlotCN<- as.numeric(forestarea08$PlotCN)
#2009
movinglist <- list()
for(i in 1:length(nlsp09$PlotCN)){
  x <- get_nlcd_annual(template = nlsp09$geometry[i], label = nlsp09$PlotCN[i], year = 2009)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp09$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp09$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea09 <- data.frame(unlist(fadlist))
forestarea09$PlotCN <- names(fadlist)
names(forestarea09)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea09$FADClass <- ifelse(forestarea09$FAD >=90, 5,
                                ifelse(forestarea09$FAD >=60, 4,
                                       ifelse(forestarea09$FAD >=40, 3,
                                              ifelse(forestarea09$FAD>=10, 2, 1))))
forestarea09$PlotCN<- as.numeric(forestarea09$PlotCN)
#2010
movinglist <- list()
for(i in 1:length(nlsp10$PlotCN)){
  x <- get_nlcd_annual(template = nlsp10$geometry[i], label = nlsp10$PlotCN[i], year = 2010)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp10$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp10$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea10 <- data.frame(unlist(fadlist))
forestarea10$PlotCN <- names(fadlist)
names(forestarea10)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea10$FADClass <- ifelse(forestarea10$FAD >=90, 5,
                                ifelse(forestarea10$FAD >=60, 4,
                                       ifelse(forestarea10$FAD >=40, 3,
                                              ifelse(forestarea10$FAD>=10, 2, 1))))
forestarea10$PlotCN<- as.numeric(forestarea10$PlotCN)
#2011
movinglist <- list()
for(i in 1:length(nlsp11$PlotCN)){
  x <- get_nlcd_annual(template = nlsp11$geometry[i], label = nlsp11$PlotCN[i], year = 2011)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp11$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp11$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea11 <- data.frame(unlist(fadlist))
forestarea11$PlotCN <- names(fadlist)
names(forestarea11)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea11$FADClass <- ifelse(forestarea11$FAD >=90, 5,
                                ifelse(forestarea11$FAD >=60, 4,
                                       ifelse(forestarea11$FAD >=40, 3,
                                              ifelse(forestarea11$FAD>=10, 2, 1))))
forestarea11$PlotCN<- as.numeric(forestarea11$PlotCN)
#2012
movinglist <- list()
for(i in 1:length(nlsp12$PlotCN)){
  x <- get_nlcd_annual(template = nlsp12$geometry[i], label = nlsp12$PlotCN[i], year = 2012)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp12$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp12$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea12 <- data.frame(unlist(fadlist))
forestarea12$PlotCN <- names(fadlist)
names(forestarea12)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea12$FADClass <- ifelse(forestarea12$FAD >=90, 5,
                                ifelse(forestarea12$FAD >=60, 4,
                                       ifelse(forestarea12$FAD >=40, 3,
                                              ifelse(forestarea12$FAD>=10, 2, 1))))
forestarea12$PlotCN<- as.numeric(forestarea12$PlotCN)
#2013
movinglist <- list()
for(i in 1:length(nlsp13$PlotCN)){
  x <- get_nlcd_annual(template = nlsp13$geometry[i], label = nlsp13$PlotCN[i], year = 2013)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp13$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp13$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea13 <- data.frame(unlist(fadlist))
forestarea13$PlotCN <- names(fadlist)
names(forestarea13)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea13$FADClass <- ifelse(forestarea13$FAD >=90, 5,
                                ifelse(forestarea13$FAD >=60, 4,
                                       ifelse(forestarea13$FAD >=40, 3,
                                              ifelse(forestarea13$FAD>=10, 2, 1))))
forestarea13$PlotCN<- as.numeric(forestarea13$PlotCN)
#2014
movinglist <- list()
for(i in 1:length(nlsp14$PlotCN)){
  x <- get_nlcd_annual(template = nlsp14$geometry[i], label = nlsp14$PlotCN[i], year = 2014)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp14$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp14$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea14 <- data.frame(unlist(fadlist))
forestarea14$PlotCN <- names(fadlist)
names(forestarea14)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea14$FADClass <- ifelse(forestarea14$FAD >=90, 5,
                                ifelse(forestarea14$FAD >=60, 4,
                                       ifelse(forestarea14$FAD >=40, 3,
                                              ifelse(forestarea14$FAD>=10, 2, 1))))
forestarea14$PlotCN<- as.numeric(forestarea14$PlotCN)
#2015
movinglist <- list()
for(i in 1:length(nlsp15$PlotCN)){
  x <- get_nlcd_annual(template = nlsp15$geometry[i], label = nlsp15$PlotCN[i], year = 2015)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp15$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp15$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea15 <- data.frame(unlist(fadlist))
forestarea15$PlotCN <- names(fadlist)
names(forestarea15)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea15$FADClass <- ifelse(forestarea15$FAD >=90, 5,
                                ifelse(forestarea15$FAD >=60, 4,
                                       ifelse(forestarea15$FAD >=40, 3,
                                              ifelse(forestarea15$FAD>=10, 2, 1))))
forestarea15$PlotCN<- as.numeric(forestarea15$PlotCN)
#2016
movinglist <- list()
for(i in 1:length(nlsp16$PlotCN)){
  x <- get_nlcd_annual(template = nlsp16$geometry[i], label = nlsp16$PlotCN[i], year = 2016)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp16$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp16$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea16 <- data.frame(unlist(fadlist))
forestarea16$PlotCN <- names(fadlist)
names(forestarea16)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea16$FADClass <- ifelse(forestarea16$FAD >=90, 5,
                                ifelse(forestarea16$FAD >=60, 4,
                                       ifelse(forestarea16$FAD >=40, 3,
                                              ifelse(forestarea16$FAD>=10, 2, 1))))
forestarea16$PlotCN<- as.numeric(forestarea16$PlotCN)
#2017
movinglist <- list()
for(i in 1:length(nlsp17$PlotCN)){
  x <- get_nlcd_annual(template = nlsp17$geometry[i], label = nlsp17$PlotCN[i], year = 2017)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp17$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp17$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea17 <- data.frame(unlist(fadlist))
forestarea17$PlotCN <- names(fadlist)
names(forestarea17)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea17$FADClass <- ifelse(forestarea17$FAD >=90, 5,
                                ifelse(forestarea17$FAD >=60, 4,
                                       ifelse(forestarea17$FAD >=40, 3,
                                              ifelse(forestarea17$FAD>=10, 2, 1))))
forestarea17$PlotCN<- as.numeric(forestarea17$PlotCN)
#2018
movinglist <- list()
for(i in 1:length(nlsp18$PlotCN)){
  x <- get_nlcd_annual(template = nlsp18$geometry[i], label = nlsp18$PlotCN[i], year = 2018)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp18$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp18$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea18 <- data.frame(unlist(fadlist))
forestarea18$PlotCN <- names(fadlist)
names(forestarea18)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea18$FADClass <- ifelse(forestarea18$FAD >=90, 5,
                                ifelse(forestarea18$FAD >=60, 4,
                                       ifelse(forestarea18$FAD >=40, 3,
                                              ifelse(forestarea18$FAD>=10, 2, 1))))
forestarea18$PlotCN<- as.numeric(forestarea18$PlotCN)
#2019
movinglist <- list()
for(i in 1:length(nlsp19$PlotCN)){
  x <- get_nlcd_annual(template = nlsp19$geometry[i], label = nlsp19$PlotCN[i], year = 2019)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp19$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp19$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea19 <- data.frame(unlist(fadlist))
forestarea19$PlotCN <- names(fadlist)
names(forestarea19)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea19$FADClass <- ifelse(forestarea19$FAD >=90, 5,
                                ifelse(forestarea19$FAD >=60, 4,
                                       ifelse(forestarea19$FAD >=40, 3,
                                              ifelse(forestarea19$FAD>=10, 2, 1))))
forestarea19$PlotCN<- as.numeric(forestarea19$PlotCN)
#2020
movinglist <- list()
for(i in 1:length(nlsp20$PlotCN)){
  x <- get_nlcd_annual(template = nlsp20$geometry[i], label = nlsp20$PlotCN[i], year = 2020)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp20$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp20$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea20 <- data.frame(unlist(fadlist))
forestarea20$PlotCN <- names(fadlist)
names(forestarea20)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea20$FADClass <- ifelse(forestarea20$FAD >=90, 5,
                                ifelse(forestarea20$FAD >=60, 4,
                                       ifelse(forestarea20$FAD >=40, 3,
                                              ifelse(forestarea20$FAD>=10, 2, 1))))
forestarea20$PlotCN<- as.numeric(forestarea20$PlotCN)

#combine FAD dfs and add to all data
FADdf <- rbind(forestarea01,forestarea02,forestarea03,forestarea04,forestarea05,forestarea06,forestarea07,forestarea08,
               forestarea09,forestarea10,forestarea10,forestarea11,forestarea12,forestarea13,forestarea14,forestarea15,
               forestarea16,forestarea17,forestarea18,forestarea19,forestarea20)
allFIA <- merge(allFIA,FADdf[,2:3],by="PlotCN", all.x=TRUE)
allFIA <- distinct(allFIA)

#pull soil_order
get_soil_order <- function(point) {
  result <- tryCatch({
    SDA_spatialQuery(point, what = "mukey")
  }, error = function(e) {
    message("Error during SDA_spatialQuery: ", e$message)
    return(NULL)
  })
  
  if (is.null(result) || nrow(result) == 0) {
    return(NA)
  }
  
  mukey <- result$mukey
  query <- paste0(
    "SELECT DISTINCT c.mukey, c.compname, c.taxorder 
     FROM component AS c 
     WHERE c.mukey = '", mukey, "'"
  )
  
  soil_info <- tryCatch({
    SDA_query(query)
  }, error = function(e) {
    message("Error during SDA_query: ", e$message)
    return(NULL)
  })
  
  if (is.null(soil_info) || nrow(soil_info) == 0) {
    return(NA)
  }
  
  return(soil_info$taxorder[1])
}
coordssf <- st_as_sf(coords, coords = c("Longitude","Latitude"))

soil_order <- apply(st_coordinates(coordssf), 1, function(coords){
  point <- st_sfc(st_point(coords), crs=4326)
  get_soil_order(point)
})
#merge with all data
soil_order <- as.data.frame(soil_order)
soil_order$PlotCN <- coords$PlotCN
allFIA <- merge(allFIA,soil_order, by="PlotCN", all.x=TRUE)
allFIA <- distinct(allFIA)

#air quality setwd, data files available on github, these files are very large
setwd("~/Desktop/AirQuality/EQUATESNC")
#function to pull AQI from netCDF files, and limit to growing season.
pullAQI <- function(varname, newname, points, filepath, days = 91:273) {
  stack_data <- stack(filepath, varname = varname)
  stack_data <- raster::subset(stack_data, days)
  extent(stack_data) <- c(-180, 180, -90, 90)
  crs(stack_data) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  extracted <- raster::extract(stack_data, points)
  result <- as.data.frame(rowMeans(extracted, na.rm = TRUE))
  names(result) <- newname
  return(result)
}
#run by year, 2002
coords02 <- subset(coords, Year == 2002)
SP02 <- SpatialPointsDataFrame(coords = coords02[,3:4],data = coords02,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone02",
  PM25_AVG = "AvgPM2502",
  PM10_AVG = "AvgPM1002",
  CO_AVG = "AvgCO02",
  SO2_AVG = "AvgSO202",
  NO2_AVG = "AvgNO202"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP02, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2002.nc")
})
Con02 <- cbind(PlotCN = coords02$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con02$AvgOzone02 <- Con02$AvgOzone02/1000
#calculate AQI
Con02$AQIOzone <- (con2aqi("o3",con = Con02$AvgOzone02,type="8h"))
#PM25 and PM10 no conversion
Con02$AQIPM25 <- (con2aqi("pm25",con = Con02$AvgPM2502))
Con02$AQIPM10 <- (con2aqi("pm10",con = Con02$AvgPM1002))
#convert CO ppb to ppm
Con02$AvgCO02 <- Con02$AvgCO02/1000
Con02$AQICO <- (con2aqi("co",con = Con02$AvgCO02))
#SO2,NO2 no conversion
Con02$AQISO2 <- (con2aqi("so2",con = Con02$AvgSO202))
Con02$AQINO2 <- (con2aqi("no2",con = Con02$AvgNO202))
Con02$MaxAQI <- pmax(Con02$AQIOzone,Con02$AQIPM25,Con02$AQIPM10,Con02$AQICO,Con02$AQISO2,Con02$AQINO2)
#2003
coords03 <- subset(coords, Year == 2003)
SP03 <- SpatialPointsDataFrame(coords = coords03[,3:4],data = coords03,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone03",
  PM25_AVG = "AvgPM2503",
  PM10_AVG = "AvgPM1003",
  CO_AVG = "AvgCO03",
  SO2_AVG = "AvgSO203",
  NO2_AVG = "AvgNO203"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP03, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2003.nc")
})
Con03 <- cbind(PlotCN = coords03$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con03$AvgOzone03 <- Con03$AvgOzone03/1000
#calculate AQI
Con03$AQIOzone <- (con2aqi("o3",con = Con03$AvgOzone03,type="8h"))
#PM25 and PM10 no conversion
Con03$AQIPM25 <- (con2aqi("pm25",con = Con03$AvgPM2503))
Con03$AQIPM10 <- (con2aqi("pm10",con = Con03$AvgPM1003))
#convert CO ppb to ppm
Con03$AvgCO03 <- Con03$AvgCO03/1000
Con03$AQICO <- (con2aqi("co",con = Con03$AvgCO03))
#SO2,NO2 no conversion
Con03$AQISO2 <- (con2aqi("so2",con = Con03$AvgSO203))
Con03$AQINO2 <- (con2aqi("no2",con = Con03$AvgNO203))
Con03$MaxAQI <- pmax(Con03$AQIOzone,Con03$AQIPM25,Con03$AQIPM10,Con03$AQICO,Con03$AQISO2,Con03$AQINO2)
#2004
coords04 <- subset(coords, Year == 2004)
SP04 <- SpatialPointsDataFrame(coords = coords04[,3:4],data = coords04,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone04",
  PM25_AVG = "AvgPM2504",
  PM10_AVG = "AvgPM1004",
  CO_AVG = "AvgCO04",
  SO2_AVG = "AvgSO204",
  NO2_AVG = "AvgNO204"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP04, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2004.nc")
})
Con04 <- cbind(PlotCN = coords04$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con04$AvgOzone04 <- Con04$AvgOzone04/1000
#calculate AQI
Con04$AQIOzone <- (con2aqi("o3",con = Con04$AvgOzone04,type="8h"))
#PM25 and PM10 no conversion
Con04$AQIPM25 <- (con2aqi("pm25",con = Con04$AvgPM2504))
Con04$AQIPM10 <- (con2aqi("pm10",con = Con04$AvgPM1004))
#convert CO ppb to ppm
Con04$AvgCO04 <- Con04$AvgCO04/1000
Con04$AQICO <- (con2aqi("co",con = Con04$AvgCO04))
#SO2,NO2 no conversion
Con04$AQISO2 <- (con2aqi("so2",con = Con04$AvgSO204))
Con04$AQINO2 <- (con2aqi("no2",con = Con04$AvgNO204))
Con04$MaxAQI <- pmax(Con04$AQIOzone,Con04$AQIPM25,Con04$AQIPM10,Con04$AQICO,Con04$AQISO2,Con04$AQINO2)
#2005
coords05 <- subset(coords, Year == 2005)
SP05 <- SpatialPointsDataFrame(coords = coords05[,3:4],data = coords05,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone05",
  PM25_AVG = "AvgPM2505",
  PM10_AVG = "AvgPM1005",
  CO_AVG = "AvgCO05",
  SO2_AVG = "AvgSO205",
  NO2_AVG = "AvgNO205"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP05, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2005.nc")
})
Con05 <- cbind(PlotCN = coords05$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con05$AvgOzone05 <- Con05$AvgOzone05/1000
#calculate AQI
Con05$AQIOzone <- (con2aqi("o3",con = Con05$AvgOzone05,type="8h"))
#PM25 and PM10 no conversion
Con05$AQIPM25 <- (con2aqi("pm25",con = Con05$AvgPM2505))
Con05$AQIPM10 <- (con2aqi("pm10",con = Con05$AvgPM1005))
#convert CO ppb to ppm
Con05$AvgCO05 <- Con05$AvgCO05/1000
Con05$AQICO <- (con2aqi("co",con = Con05$AvgCO05))
#SO2,NO2 no conversion
Con05$AQISO2 <- (con2aqi("so2",con = Con05$AvgSO205))
Con05$AQINO2 <- (con2aqi("no2",con = Con05$AvgNO205))
Con05$MaxAQI <- pmax(Con05$AQIOzone,Con05$AQIPM25,Con05$AQIPM10,Con05$AQICO,Con05$AQISO2,Con05$AQINO2)
#2006
coords06 <- subset(coords, Year == 2006)
SP06 <- SpatialPointsDataFrame(coords = coords06[,3:4],data = coords06,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone06",
  PM25_AVG = "AvgPM2506",
  PM10_AVG = "AvgPM1006",
  CO_AVG = "AvgCO06",
  SO2_AVG = "AvgSO206",
  NO2_AVG = "AvgNO206"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP06, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2006.nc")
})
Con06 <- cbind(PlotCN = coords06$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con06$AvgOzone06 <- Con06$AvgOzone06/1000
#calculate AQI
Con06$AQIOzone <- (con2aqi("o3",con = Con06$AvgOzone06,type="8h"))
#PM25 and PM10 no conversion
Con06$AQIPM25 <- (con2aqi("pm25",con = Con06$AvgPM2506))
Con06$AQIPM10 <- (con2aqi("pm10",con = Con06$AvgPM1006))
#convert CO ppb to ppm
Con06$AvgCO06 <- Con06$AvgCO06/1000
Con06$AQICO <- (con2aqi("co",con = Con06$AvgCO06))
#SO2,NO2 no conversion
Con06$AQISO2 <- (con2aqi("so2",con = Con06$AvgSO206))
Con06$AQINO2 <- (con2aqi("no2",con = Con06$AvgNO206))
Con06$MaxAQI <- pmax(Con06$AQIOzone,Con06$AQIPM25,Con06$AQIPM10,Con06$AQICO,Con06$AQISO2,Con06$AQINO2)
#2007
coords07 <- subset(coords, Year == 2007)
SP07 <- SpatialPointsDataFrame(coords = coords07[,3:4],data = coords07,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone07",
  PM25_AVG = "AvgPM2507",
  PM10_AVG = "AvgPM1007",
  CO_AVG = "AvgCO07",
  SO2_AVG = "AvgSO207",
  NO2_AVG = "AvgNO207"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP07, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2007.nc")
})
Con07 <- cbind(PlotCN = coords07$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con07$AvgOzone07 <- Con07$AvgOzone07/1000
#calculate AQI
Con07$AQIOzone <- (con2aqi("o3",con = Con07$AvgOzone07,type="8h"))
#PM25 and PM10 no conversion
Con07$AQIPM25 <- (con2aqi("pm25",con = Con07$AvgPM2507))
Con07$AQIPM10 <- (con2aqi("pm10",con = Con07$AvgPM1007))
#convert CO ppb to ppm
Con07$AvgCO07 <- Con07$AvgCO07/1000
Con07$AQICO <- (con2aqi("co",con = Con07$AvgCO07))
#SO2,NO2 no conversion
Con07$AQISO2 <- (con2aqi("so2",con = Con07$AvgSO207))
Con07$AQINO2 <- (con2aqi("no2",con = Con07$AvgNO207))
Con07$MaxAQI <- pmax(Con07$AQIOzone,Con07$AQIPM25,Con07$AQIPM10,Con07$AQICO,Con07$AQISO2,Con07$AQINO2)
#2008
coords08 <- subset(coords, Year == 2008)
SP08 <- SpatialPointsDataFrame(coords = coords08[,3:4],data = coords08,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone08",
  PM25_AVG = "AvgPM2508",
  PM10_AVG = "AvgPM1008",
  CO_AVG = "AvgCO08",
  SO2_AVG = "AvgSO208",
  NO2_AVG = "AvgNO208"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP08, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2008.nc")
})
Con08 <- cbind(PlotCN = coords08$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con08$AvgOzone08 <- Con08$AvgOzone08/1000
#calculate AQI
Con08$AQIOzone <- (con2aqi("o3",con = Con08$AvgOzone08,type="8h"))
#PM25 and PM10 no conversion
Con08$AQIPM25 <- (con2aqi("pm25",con = Con08$AvgPM2508))
Con08$AQIPM10 <- (con2aqi("pm10",con = Con08$AvgPM1008))
#convert CO ppb to ppm
Con08$AvgCO08 <- Con08$AvgCO08/1000
Con08$AQICO <- (con2aqi("co",con = Con08$AvgCO08))
#SO2,NO2 no conversion
Con08$AQISO2 <- (con2aqi("so2",con = Con08$AvgSO208))
Con08$AQINO2 <- (con2aqi("no2",con = Con08$AvgNO208))
Con08$MaxAQI <- pmax(Con08$AQIOzone,Con08$AQIPM25,Con08$AQIPM10,Con08$AQICO,Con08$AQISO2,Con08$AQINO2)
#2009
coords09 <- subset(coords, Year == 2009)
SP09 <- SpatialPointsDataFrame(coords = coords09[,3:4],data = coords09,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone09",
  PM25_AVG = "AvgPM2509",
  PM10_AVG = "AvgPM1009",
  CO_AVG = "AvgCO09",
  SO2_AVG = "AvgSO209",
  NO2_AVG = "AvgNO209"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP09, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2009.nc")
})
Con09 <- cbind(PlotCN = coords09$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con09$AvgOzone09 <- Con09$AvgOzone09/1000
#calculate AQI
Con09$AQIOzone <- (con2aqi("o3",con = Con09$AvgOzone09,type="8h"))
#PM25 and PM10 no conversion
Con09$AQIPM25 <- (con2aqi("pm25",con = Con09$AvgPM2509))
Con09$AQIPM10 <- (con2aqi("pm10",con = Con09$AvgPM1009))
#convert CO ppb to ppm
Con09$AvgCO09 <- Con09$AvgCO09/1000
Con09$AQICO <- (con2aqi("co",con = Con09$AvgCO09))
#SO2,NO2 no conversion
Con09$AQISO2 <- (con2aqi("so2",con = Con09$AvgSO209))
Con09$AQINO2 <- (con2aqi("no2",con = Con09$AvgNO209))
Con09$MaxAQI <- pmax(Con09$AQIOzone,Con09$AQIPM25,Con09$AQIPM10,Con09$AQICO,Con09$AQISO2,Con09$AQINO2)
#2010
coords10 <- subset(coords, Year == 2010)
SP10 <- SpatialPointsDataFrame(coords = coords10[,3:4],data = coords10,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone10",
  PM25_AVG = "AvgPM2510",
  PM10_AVG = "AvgPM1010",
  CO_AVG = "AvgCO10",
  SO2_AVG = "AvgSO210",
  NO2_AVG = "AvgNO210"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP10, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2010.nc")
})
Con10 <- cbind(PlotCN = coords10$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con10$AvgOzone10 <- Con10$AvgOzone10/1000
#calculate AQI
Con10$AQIOzone <- (con2aqi("o3",con = Con10$AvgOzone10,type="8h"))
#PM25 and PM10 no conversion
Con10$AQIPM25 <- (con2aqi("pm25",con = Con10$AvgPM2510))
Con10$AQIPM10 <- (con2aqi("pm10",con = Con10$AvgPM1010))
#convert CO ppb to ppm
Con10$AvgCO10 <- Con10$AvgCO10/1000
Con10$AQICO <- (con2aqi("co",con = Con10$AvgCO10))
#SO2,NO2 no conversion
Con10$AQISO2 <- (con2aqi("so2",con = Con10$AvgSO210))
Con10$AQINO2 <- (con2aqi("no2",con = Con10$AvgNO210))
Con10$MaxAQI <- pmax(Con10$AQIOzone,Con10$AQIPM25,Con10$AQIPM10,Con10$AQICO,Con10$AQISO2,Con10$AQINO2)
#2011
coords11 <- subset(coords, Year == 2011)
SP11 <- SpatialPointsDataFrame(coords = coords11[,3:4],data = coords11,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone11",
  PM25_AVG = "AvgPM2511",
  PM10_AVG = "AvgPM1011",
  CO_AVG = "AvgCO11",
  SO2_AVG = "AvgSO211",
  NO2_AVG = "AvgNO211"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP11, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2011.nc")
})
Con11 <- cbind(PlotCN = coords11$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con11$AvgOzone11 <- Con11$AvgOzone11/1100
#calculate AQI
Con11$AQIOzone <- (con2aqi("o3",con = Con11$AvgOzone11,type="8h"))
#PM25 and PM10 no conversion
Con11$AQIPM25 <- (con2aqi("pm25",con = Con11$AvgPM2511))
Con11$AQIPM10 <- (con2aqi("pm10",con = Con11$AvgPM1011))
#convert CO ppb to ppm
Con11$AvgCO11 <- Con11$AvgCO11/1100
Con11$AQICO <- (con2aqi("co",con = Con11$AvgCO11))
#SO2,NO2 no conversion
Con11$AQISO2 <- (con2aqi("so2",con = Con11$AvgSO211))
Con11$AQINO2 <- (con2aqi("no2",con = Con11$AvgNO211))
Con11$MaxAQI <- pmax(Con11$AQIOzone,Con11$AQIPM25,Con11$AQIPM10,Con11$AQICO,Con11$AQISO2,Con11$AQINO2)
#2012
coords12 <- subset(coords, Year == 2012)
SP12 <- SpatialPointsDataFrame(coords = coords12[,3:4],data = coords12,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone12",
  PM25_AVG = "AvgPM2512",
  PM10_AVG = "AvgPM1012",
  CO_AVG = "AvgCO12",
  SO2_AVG = "AvgSO212",
  NO2_AVG = "AvgNO212"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP12, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2012.nc")
})
Con12 <- cbind(PlotCN = coords12$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con12$AvgOzone12 <- Con12$AvgOzone12/1200
#calculate AQI
Con12$AQIOzone <- (con2aqi("o3",con = Con12$AvgOzone12,type="8h"))
#PM25 and PM10 no conversion
Con12$AQIPM25 <- (con2aqi("pm25",con = Con12$AvgPM2512))
Con12$AQIPM10 <- (con2aqi("pm10",con = Con12$AvgPM1012))
#convert CO ppb to ppm
Con12$AvgCO12 <- Con12$AvgCO12/1200
Con12$AQICO <- (con2aqi("co",con = Con12$AvgCO12))
#SO2,NO2 no conversion
Con12$AQISO2 <- (con2aqi("so2",con = Con12$AvgSO212))
Con12$AQINO2 <- (con2aqi("no2",con = Con12$AvgNO212))
Con12$MaxAQI <- pmax(Con12$AQIOzone,Con12$AQIPM25,Con12$AQIPM10,Con12$AQICO,Con12$AQISO2,Con12$AQINO2)
#2013
#2013
coords13 <- subset(coords, Year == 2013)
SP13 <- SpatialPointsDataFrame(coords = coords13[,3:4],data = coords13,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone13",
  PM25_AVG = "AvgPM2513",
  PM10_AVG = "AvgPM1013",
  CO_AVG = "AvgCO13",
  SO2_AVG = "AvgSO213",
  NO2_AVG = "AvgNO213"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP13, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2013.nc")
})
Con13 <- cbind(PlotCN = coords13$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con13$AvgOzone13 <- Con13$AvgOzone13/1300
#calculate AQI
Con13$AQIOzone <- (con2aqi("o3",con = Con13$AvgOzone13,type="8h"))
#PM25 and PM10 no conversion
Con13$AQIPM25 <- (con2aqi("pm25",con = Con13$AvgPM2513))
Con13$AQIPM10 <- (con2aqi("pm10",con = Con13$AvgPM1013))
#convert CO ppb to ppm
Con13$AvgCO13 <- Con13$AvgCO13/1300
Con13$AQICO <- (con2aqi("co",con = Con13$AvgCO13))
#SO2,NO2 no conversion
Con13$AQISO2 <- (con2aqi("so2",con = Con13$AvgSO213))
Con13$AQINO2 <- (con2aqi("no2",con = Con13$AvgNO213))
Con13$MaxAQI <- pmax(Con13$AQIOzone,Con13$AQIPM25,Con13$AQIPM10,Con13$AQICO,Con13$AQISO2,Con13$AQINO2)
#2014
coords14 <- subset(coords, Year == 2014)
SP14 <- SpatialPointsDataFrame(coords = coords14[,3:4],data = coords14,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone14",
  PM25_AVG = "AvgPM2514",
  PM10_AVG = "AvgPM1014",
  CO_AVG = "AvgCO14",
  SO2_AVG = "AvgSO214",
  NO2_AVG = "AvgNO214"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP14, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2014.nc")
})
Con14 <- cbind(PlotCN = coords14$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con14$AvgOzone14 <- Con14$AvgOzone14/1400
#calculate AQI
Con14$AQIOzone <- (con2aqi("o3",con = Con14$AvgOzone14,type="8h"))
#PM25 and PM10 no conversion
Con14$AQIPM25 <- (con2aqi("pm25",con = Con14$AvgPM2514))
Con14$AQIPM10 <- (con2aqi("pm10",con = Con14$AvgPM1014))
#convert CO ppb to ppm
Con14$AvgCO14 <- Con14$AvgCO14/1400
Con14$AQICO <- (con2aqi("co",con = Con14$AvgCO14))
#SO2,NO2 no conversion
Con14$AQISO2 <- (con2aqi("so2",con = Con14$AvgSO214))
Con14$AQINO2 <- (con2aqi("no2",con = Con14$AvgNO214))
Con14$MaxAQI <- pmax(Con14$AQIOzone,Con14$AQIPM25,Con14$AQIPM10,Con14$AQICO,Con14$AQISO2,Con14$AQINO2)
#2015
coords15 <- subset(coords, Year == 2015)
SP15 <- SpatialPointsDataFrame(coords = coords15[,3:4],data = coords15,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone15",
  PM25_AVG = "AvgPM2515",
  PM10_AVG = "AvgPM1015",
  CO_AVG = "AvgCO15",
  SO2_AVG = "AvgSO215",
  NO2_AVG = "AvgNO215"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP15, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2015.nc")
})
Con15 <- cbind(PlotCN = coords15$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con15$AvgOzone15 <- Con15$AvgOzone15/1500
#calculate AQI
Con15$AQIOzone <- (con2aqi("o3",con = Con15$AvgOzone15,type="8h"))
#PM25 and PM10 no conversion
Con15$AQIPM25 <- (con2aqi("pm25",con = Con15$AvgPM2515))
Con15$AQIPM10 <- (con2aqi("pm10",con = Con15$AvgPM1015))
#convert CO ppb to ppm
Con15$AvgCO15 <- Con15$AvgCO15/1500
Con15$AQICO <- (con2aqi("co",con = Con15$AvgCO15))
#SO2,NO2 no conversion
Con15$AQISO2 <- (con2aqi("so2",con = Con15$AvgSO215))
Con15$AQINO2 <- (con2aqi("no2",con = Con15$AvgNO215))
Con15$MaxAQI <- pmax(Con15$AQIOzone,Con15$AQIPM25,Con15$AQIPM10,Con15$AQICO,Con15$AQISO2,Con15$AQINO2)
#2016
coords16 <- subset(coords, Year == 2016)
SP16 <- SpatialPointsDataFrame(coords = coords16[,3:4],data = coords16,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone16",
  PM25_AVG = "AvgPM2516",
  PM10_AVG = "AvgPM1016",
  CO_AVG = "AvgCO16",
  SO2_AVG = "AvgSO216",
  NO2_AVG = "AvgNO216"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP16, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2016.nc")
})
Con16 <- cbind(PlotCN = coords16$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con16$AvgOzone16 <- Con16$AvgOzone16/1600
#calculate AQI
Con16$AQIOzone <- (con2aqi("o3",con = Con16$AvgOzone16,type="8h"))
#PM25 and PM10 no conversion
Con16$AQIPM25 <- (con2aqi("pm25",con = Con16$AvgPM2516))
Con16$AQIPM10 <- (con2aqi("pm10",con = Con16$AvgPM1016))
#convert CO ppb to ppm
Con16$AvgCO16 <- Con16$AvgCO16/1600
Con16$AQICO <- (con2aqi("co",con = Con16$AvgCO16))
#SO2,NO2 no conversion
Con16$AQISO2 <- (con2aqi("so2",con = Con16$AvgSO216))
Con16$AQINO2 <- (con2aqi("no2",con = Con16$AvgNO216))
Con16$MaxAQI <- pmax(Con16$AQIOzone,Con16$AQIPM25,Con16$AQIPM10,Con16$AQICO,Con16$AQISO2,Con16$AQINO2)
#2017
coords17 <- subset(coords, Year == 2017)
SP17 <- SpatialPointsDataFrame(coords = coords17[,3:4],data = coords17,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone17",
  PM25_AVG = "AvgPM2517",
  PM10_AVG = "AvgPM1017",
  CO_AVG = "AvgCO17",
  SO2_AVG = "AvgSO217",
  NO2_AVG = "AvgNO217"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP17, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2017.nc")
})
Con17 <- cbind(PlotCN = coords17$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con17$AvgOzone17 <- Con17$AvgOzone17/1700
#calculate AQI
Con17$AQIOzone <- (con2aqi("o3",con = Con17$AvgOzone17,type="8h"))
#PM25 and PM10 no conversion
Con17$AQIPM25 <- (con2aqi("pm25",con = Con17$AvgPM2517))
Con17$AQIPM10 <- (con2aqi("pm10",con = Con17$AvgPM1017))
#convert CO ppb to ppm
Con17$AvgCO17 <- Con17$AvgCO17/1700
Con17$AQICO <- (con2aqi("co",con = Con17$AvgCO17))
#SO2,NO2 no conversion
Con17$AQISO2 <- (con2aqi("so2",con = Con17$AvgSO217))
Con17$AQINO2 <- (con2aqi("no2",con = Con17$AvgNO217))
Con17$MaxAQI <- pmax(Con17$AQIOzone,Con17$AQIPM25,Con17$AQIPM10,Con17$AQICO,Con17$AQISO2,Con17$AQINO2)
#2018
coords18 <- subset(coords, Year == 2018)
SP18 <- SpatialPointsDataFrame(coords = coords18[,3:4],data = coords18,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone18",
  PM25_AVG = "AvgPM2518",
  PM10_AVG = "AvgPM1018",
  CO_AVG = "AvgCO18",
  SO2_AVG = "AvgSO218",
  NO2_AVG = "AvgNO218"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP18, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2018.nc")
})
Con18 <- cbind(PlotCN = coords18$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con18$AvgOzone18 <- Con18$AvgOzone18/1800
#calculate AQI
Con18$AQIOzone <- (con2aqi("o3",con = Con18$AvgOzone18,type="8h"))
#PM25 and PM10 no conversion
Con18$AQIPM25 <- (con2aqi("pm25",con = Con18$AvgPM2518))
Con18$AQIPM10 <- (con2aqi("pm10",con = Con18$AvgPM1018))
#convert CO ppb to ppm
Con18$AvgCO18 <- Con18$AvgCO18/1800
Con18$AQICO <- (con2aqi("co",con = Con18$AvgCO18))
#SO2,NO2 no conversion
Con18$AQISO2 <- (con2aqi("so2",con = Con18$AvgSO218))
Con18$AQINO2 <- (con2aqi("no2",con = Con18$AvgNO218))
Con18$MaxAQI <- pmax(Con18$AQIOzone,Con18$AQIPM25,Con18$AQIPM10,Con18$AQICO,Con18$AQISO2,Con18$AQINO2)
#2019
coords19 <- subset(coords, Year == 2019)
SP19 <- SpatialPointsDataFrame(coords = coords19[,3:4],data = coords19,
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
pollutants <- list(
  O3_MDA8 = "AvgOzone19",
  PM25_AVG = "AvgPM2519",
  PM10_AVG = "AvgPM1019",
  CO_AVG = "AvgCO19",
  SO2_AVG = "AvgSO219",
  NO2_AVG = "AvgNO219"
)
results <- lapply(names(pollutants), function(p) {
  pullAQI(p, pollutants[[p]], SP19, "HR2DAY_LST_ACONC_EQUATES_v532_12US1_2019.nc")
})
Con19 <- cbind(PlotCN = coords19$PlotCN, do.call(cbind, results))
#convert o3 ppb to ppm
Con19$AvgOzone19 <- Con19$AvgOzone19/1900
#calculate AQI
Con19$AQIOzone <- (con2aqi("o3",con = Con19$AvgOzone19,type="8h"))
#PM25 and PM10 no conversion
Con19$AQIPM25 <- (con2aqi("pm25",con = Con19$AvgPM2519))
Con19$AQIPM10 <- (con2aqi("pm10",con = Con19$AvgPM1019))
#convert CO ppb to ppm
Con19$AvgCO19 <- Con19$AvgCO19/1900
Con19$AQICO <- (con2aqi("co",con = Con19$AvgCO19))
#SO2,NO2 no conversion
Con19$AQISO2 <- (con2aqi("so2",con = Con19$AvgSO219))
Con19$AQINO2 <- (con2aqi("no2",con = Con19$AvgNO219))
Con19$MaxAQI <- pmax(Con19$AQIOzone,Con19$AQIPM25,Con19$AQIPM10,Con19$AQICO,Con19$AQISO2,Con19$AQINO2)
#combine AQI data and join with all data
ConAll <- rbind(Con02[,c(1,14)],Con03[,c(1,14)],Con04[,c(1,14)],Con05[,c(1,14)],Con06[,c(1,14)],Con07[,c(1,14)],
                Con08[,c(1,14)],Con09[,c(1,14)],Con10[,c(1,14)],Con11[,c(1,14)],Con12[,c(1,14)],Con13[,c(1,14)],
                Con14[,c(1,14)],Con15[,c(1,14)],Con16[,c(1,14)],Con17[,c(1,14)],Con18[,c(1,14)],Con19[,c(1,14)])
allFIA <- merge(allFIA,ConAll,by="PlotCN", all.x=TRUE)
allFIA <- distinct(allFIA)

#tree mortality percentage
mortTree <- data.frame(midAtl$TREE$INVYR, midAtl$TREE$PLT_CN, midAtl$TREE$STATUSCD, midAtl$TREE$AGENTCD, midAtl$TREE$MORTYR)
names(mortTree)[1] = "Year"
names(mortTree)[2] = "PlotCN"
names(mortTree)[3] = "Status"
names(mortTree)[4] = "Agent"
names(mortTree)[5] = "MortYear"
mortTree <- mortTree %>% filter(PlotCN%in%allFIA$PlotCN)
#seperate status, 2 and 3. 2 is dead, 3 is cut. Cuts not included as "natural mortality"
mortTreeDead <- as.data.frame(mortTree%>%filter(mortTree$Status > 1))
names(Tree)[5] = "Year"
names(Tree)[9] = "PlotNumber"
names(Tree)[1] = "Trees"

TreeNum = matrix(1,ncol=1,nrow=length(mortTree[,1]))
TreeCount <- cbind(TreeNum, mortTree)
TreeTotal <- TreeCount %>% dplyr::count(Year, PlotCN, TreeNum)
TreeTotal <- TreeTotal[-c(3)]
TreeTotal <- as.data.frame(subset(TreeTotal, TreeTotal$Year>1999))
names(TreeTotal) <- c('Year', 'PlotCN', 'TreeTotal')

DeadTreePlot <- mortTreeDead %>% count(Year, PlotCN,Status)
CutTreePlot <- filter(DeadTreePlot, Status %in% c("3"))
DeadTreePlot <- filter(DeadTreePlot, Status %in% c("2"))
#Tree Mortality by type
MortAgent <- mortTreeDead %>% dplyr::count(Year, PlotCN, Agent)
MortAgent[is.na(MortAgent)] <- 0
names(MortAgent)[4] = "Death Count"

totAgent <- MortAgent %>% group_by(Year, Agent) %>% dplyr::summarise(sum(`Death Count`))
totAgent <- totAgent[complete.cases(totAgent[,2]),]
names(totAgent)[3] = "Deaths"
totAgent$Agent <- as.character(totAgent$Agent)
totAgent$Agent <- revalue(totAgent$Agent, c("0"="NoReport"))
totAgent$Agent <- revalue(totAgent$Agent, c("10"="Insect"))
totAgent$Agent <- revalue(totAgent$Agent, c("20"="Disease"))
totAgent$Agent <- revalue(totAgent$Agent, c("30"="Fire"))
totAgent$Agent <- revalue(totAgent$Agent, c("40"="Animal"))
totAgent$Agent <- revalue(totAgent$Agent, c("50"="Weather"))
totAgent$Agent <- revalue(totAgent$Agent, c("60"="Vegetation"))
totAgent$Agent <- revalue(totAgent$Agent, c("70"="Unknown"))
totAgent$Agent <- revalue(totAgent$Agent, c("80"="Cut"))

PlotAgent <- MortAgent %>% group_by(Year, PlotCN, Agent) %>% dplyr::summarise(sum(`Death Count`))
names(PlotAgent)[4] = "Deaths"
PlotAgent$Agent <- as.character(PlotAgent$Agent)
PlotAgent$Agent <- revalue(PlotAgent$Agent, c("0"="NoReport"))
PlotAgent$Agent <- revalue(PlotAgent$Agent, c("10"="Insect"))
PlotAgent$Agent <- revalue(PlotAgent$Agent, c("20"="Disease"))
PlotAgent$Agent <- revalue(PlotAgent$Agent, c("30"="Fire"))
PlotAgent$Agent <- revalue(PlotAgent$Agent, c("40"="Animal"))
PlotAgent$Agent <- revalue(PlotAgent$Agent, c("50"="Weather"))
PlotAgent$Agent <- revalue(PlotAgent$Agent, c("60"="Vegetation"))
PlotAgent$Agent <- revalue(PlotAgent$Agent, c("70"="Unknown"))
PlotAgent$Agent <- revalue(PlotAgent$Agent, c("80"="Cut"))

PlotAgent <- PlotAgent %>% pivot_wider(names_from = Agent, values_from = Deaths)
PlotAgent[is.na(PlotAgent)] <- 0
PlotAgent <- as.data.frame(subset(PlotAgent, PlotAgent$Year>1999))
#include all causes for death except Cut
PlotAgent$MortTotal <- rowSums(PlotAgent[,c(3:4,6:11)])
MortRate <- merge(PlotAgent,TreeTotal, by=c("PlotCN","Year"),all.y=TRUE)
MortRate[is.na(MortRate)] <- 0
MortRate$MortRate <- MortRate$MortTotal/MortRate$TreeTotal
allFIA <- merge(allFIA,MortRate, by=c("PlotCN","Year"), all.x=TRUE)

#Soil Quality Index Subset, read in files, included in github data section. Not accessible via rFIA
SoilChemMD <- read.csv(file = 'MD_SOILS_LAB.csv')
SoilChemDE <- read.csv(file = 'DE_SOILS_LAB.csv')
SoilChemNJ <- read.csv(file = 'NJ_SOILS_LAB.csv')
SoilChemPA <- read.csv(file = 'PA_SOILS_LAB.csv')
SoilChemNY <- read.csv(file = 'NY_SOILS_LAB.csv')
SoilChem <- rbind(SoilChemDE,SoilChemMD,SoilChemNJ,SoilChemNY,SoilChemPA)
#calculate each AQI category
#bulk density
bulkindex = matrix(NA,ncol=1,nrow=length(SoilChem$INVYR))
for(i in 1:length(SoilChem$BULK_DENSITY)){
  if(is.na(SoilChem$BULK_DENSITY[i])){bulkindex[i,1]=NA
  }else if(SoilChem$BULK_DENSITY[i]>1.5){bulkindex[i,1] = 0
  }else (bulkindex[i,1] = 1)
}
bulkindex<- as.data.frame(bulkindex)
#soil carbon
Carbonindex = matrix(NA,ncol=1,nrow=length(SoilChem$C_TOTAL_PCT))
for(i in 1:length(SoilChem$C_TOTAL_PCT)){
  if(is.na(SoilChem$C_TOTAL_PCT[i])){Carbonindex[i,1]=NA
  }else if(SoilChem$C_TOTAL_PCT[i]>5){Carbonindex[i,1] = 2
  }else if(SoilChem$C_TOTAL_PCT[i]<= 5 & SoilChem$C_TOTAL_PCT[i]>=1) {Carbonindex[i,1] = 1}
  else (Carbonindex[i,1]=0)
}
Carbonindex<- as.data.frame(Carbonindex)
#coarse soil fraction
Coarseindex = matrix(NA,ncol=1,nrow=length(SoilChem$COARSE_FRACTION_PCT))
for(i in 1:length(SoilChem$COARSE_FRACTION_PCT)){
  if(is.na(SoilChem$COARSE_FRACTION_PCT[i])){Coarseindex[i,1]=NA
  }else if(SoilChem$COARSE_FRACTION_PCT[i]>50){Coarseindex[i,1] = 0
  }
  else (Coarseindex[i,1]=1)
}
Coarseindex<- as.data.frame(Coarseindex)
#pH
PHindex = matrix(NA,ncol=1,nrow=length(SoilChem$PH_H2O))
for(i in 1:length(SoilChem$PH_H2O)){
  if(is.na(SoilChem$PH_H2O[i])){PHindex[i,1]=NA
  }else if(SoilChem$PH_H2O[i]<3){PHindex[i,1] = -1
  }else if(SoilChem$PH_H2O[i]<= 4 & SoilChem$PH_H2O[i]>=3.01){PHindex[i,1] = 0
  }else if(SoilChem$PH_H2O[i]<= 5.5 & SoilChem$PH_H2O[i]>=4.01) {PHindex[i,1] = 1
  }else if(SoilChem$PH_H2O[i]<= 7.2 & SoilChem$PH_H2O[i]>=5.51) {PHindex[i,1] = 2
  }else if(SoilChem$PH_H2O[i]<= 8.5 & SoilChem$PH_H2O[i]>=7.21) {PHindex[i,1] = 1
  }
  else (PHindex[i,1]=0)
}
PHindex<- as.data.frame(PHindex)
#nitrogen
Nitrogenindex = matrix(NA,ncol=1,nrow=length(SoilChem$N_TOTAL_PCT))
for(i in 1:length(SoilChem$N_TOTAL_PCT)){
  if(is.na(SoilChem$N_TOTAL_PCT[i])){Nitrogenindex[i,1]=NA
  }else if(SoilChem$N_TOTAL_PCT[i]>.5){Nitrogenindex[i,1] = 2
  }else if(SoilChem$N_TOTAL_PCT[i]<= .5 & SoilChem$N_TOTAL_PCT[i]>=.1) {Nitrogenindex[i,1] = 1}
  else (Nitrogenindex[i,1]=0)
}
Nitrogenindex<- as.data.frame(Nitrogenindex)
#Exchangable NA
NaPCT = matrix(NA, ncol=1,nrow=length(SoilChem$EXCHNG_NA))
for(i in 1:length(SoilChem$EXCHNG_NA)){
  if(is.na(SoilChem$EXCHNG_NA[i])){NaPCT[i,1]=NA}
  else(NaPCT[i,1]= (SoilChem$EXCHNG_NA[i]/SoilChem$ECEC[i]))*100}
NaPCT <- as.data.frame(NaPCT)
SoilChem <- cbind(SoilChem, NaPCT)
names(SoilChem)[48] = "NaPCT"
#NA percentage
NaPCTindex = matrix(NA,ncol=1,nrow=length(SoilChem$NaPCT))
for(i in 1:length(SoilChem$NaPCT)){
  if(is.na(SoilChem$NaPCT[i])){NaPCTindex[i,1]=NA
  }else if(SoilChem$NaPCT[i]>15){NaPCTindex[i,1] = 0
  }
  else (NaPCTindex[i,1]=1)
}
NaPCTindex<- as.data.frame(NaPCTindex)
#Exchangable K
Kindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_K))
for(i in 1:length(SoilChem$EXCHNG_K)){
  if(is.na(SoilChem$EXCHNG_K[i])){Kindex[i,1]=NA
  }else if(SoilChem$EXCHNG_K[i]>500){Kindex[i,1] = 2
  }else if(SoilChem$EXCHNG_K[i]<= 500 & SoilChem$EXCHNG_K[i]>=100) {Kindex[i,1] = 1}
  else (Kindex[i,1]=0)
}
Kindex<- as.data.frame(Kindex)
#Exchangable MG
MGindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_MG))
for(i in 1:length(SoilChem$EXCHNG_MG)){
  if(is.na(SoilChem$EXCHNG_MG[i])){MGindex[i,1]=NA
  }else if(SoilChem$EXCHNG_MG[i]>500){MGindex[i,1] = 2
  }else if(SoilChem$EXCHNG_MG[i]<= 500 & SoilChem$EXCHNG_MG[i]>=50) {MGindex[i,1] = 1}
  else (MGindex[i,1]=0)
}
MGindex<- as.data.frame(MGindex)
#Exchangable CA
CAindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_CA))
for(i in 1:length(SoilChem$EXCHNG_CA)){
  if(is.na(SoilChem$EXCHNG_CA[i])){CAindex[i,1]=NA
  }else if(SoilChem$EXCHNG_CA[i]>1000){CAindex[i,1] = 2
  }else if(SoilChem$EXCHNG_CA[i]<= 1000 & SoilChem$EXCHNG_CA[i]>=101){CAindex[i,1] = 1
  }else if(SoilChem$EXCHNG_CA[i]<= 100 & SoilChem$EXCHNG_CA[i]>=10) {CAindex[i,1] = 0
  }
  else (CAindex[i,1]=-1)
}
CAindex<- as.data.frame(CAindex)
#Exchangable AL
Alindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_AL))
for(i in 1:length(SoilChem$EXCHNG_AL)){
  if(is.na(SoilChem$EXCHNG_AL[i])){Alindex[i,1]=NA
  }else if(SoilChem$EXCHNG_AL[i]>100){Alindex[i,1] = 0
  }else if(SoilChem$EXCHNG_AL[i]<= 100 & SoilChem$EXCHNG_AL[i]>=10){Alindex[i,1] = 1
  }else if(SoilChem$EXCHNG_AL[i]<= 10 & SoilChem$EXCHNG_AL[i]>=1) {Alindex[i,1] = 2
  }
  else (CAindex[i,1]=2)
}
Alindex<- as.data.frame(Alindex)
#Exchanagable MN
Mnindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_MN))
for(i in 1:length(SoilChem$EXCHNG_MN)){
  if(is.na(SoilChem$EXCHNG_MN[i])){Mnindex[i,1]=NA
  }else if(SoilChem$EXCHNG_MN[i]>100){Mnindex[i,1] = 0
  }else if(SoilChem$EXCHNG_MN[i]<= 100 & SoilChem$EXCHNG_MN[i]>=10){Mnindex[i,1] = 1
  }else if(SoilChem$EXCHNG_MN[i]<= 10 & SoilChem$EXCHNG_MN[i]>=1) {Mnindex[i,1] = 1
  }
  else (Mnindex[i,1]=0)
}
Mnindex<- as.data.frame(Mnindex)
#Exchangable FE
Feindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_FE))
for(i in 1:length(SoilChem$EXCHNG_FE)){
  if(is.na(SoilChem$EXCHNG_FE[i])){Feindex[i,1]=NA
  }else if(SoilChem$EXCHNG_FE[i]>10){Feindex[i,1] = 1
  }else if(SoilChem$EXCHNG_FE[i]<= 10 & SoilChem$EXCHNG_FE[i]>=.1) {Feindex[i,1] = 1}
  else (Feindex[i,1]=0)
}
Feindex<- as.data.frame(Feindex)
#Exchangable NI
Niindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_NI))
for(i in 1:length(SoilChem$EXCHNG_NI)){
  if(is.na(SoilChem$EXCHNG_NI[i])){Niindex[i,1]=NA
  }else if(SoilChem$EXCHNG_NI[i]>5){Niindex[i,1] = 0
  }else if(SoilChem$EXCHNG_NI[i]<= 5 & SoilChem$EXCHNG_NI[i]>=.1) {Niindex[i,1] = 1}
  else (Niindex[i,1]=1)
}
Niindex<- as.data.frame(Niindex)
#Exchangable CU
Cuindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_CU))
for(i in 1:length(SoilChem$EXCHNG_CU)){
  if(is.na(SoilChem$EXCHNG_CU[i])){Cuindex[i,1]=NA
  }else if(SoilChem$EXCHNG_CU[i]>1){Cuindex[i,1] = 0
  }else if(SoilChem$EXCHNG_CU[i]<= 1 & SoilChem$EXCHNG_CU[i]>=.1) {Cuindex[i,1] = 1}
  else (Cuindex[i,1]=0)
}
Cuindex<- as.data.frame(Cuindex)
#Exchangable ZN
Znindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_ZN))
for(i in 1:length(SoilChem$EXCHNG_ZN)){
  if(is.na(SoilChem$EXCHNG_ZN[i])){Znindex[i,1]=NA
  }else if(SoilChem$EXCHNG_ZN[i]>10){Znindex[i,1] = 0
  }else if(SoilChem$EXCHNG_ZN[i]<= 10 & SoilChem$EXCHNG_ZN[i]>=1) {Znindex[i,1] = 1}
  else (Znindex[i,1]=0)
}
Znindex<- as.data.frame(Znindex)
#Exchangable CD
Cdindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_CD))
for(i in 1:length(SoilChem$EXCHNG_CD)){
  if(is.na(SoilChem$EXCHNG_CD[i])){Cdindex[i,1]=NA
  }else if(SoilChem$EXCHNG_CD[i]>.5){Cdindex[i,1] = 0
  }else if(SoilChem$EXCHNG_CD[i]<= .5 & SoilChem$EXCHNG_CD[i]>=.1) {Cdindex[i,1] = 1}
  else (Cdindex[i,1]=1)
}
Cdindex<- as.data.frame(Cdindex)
#Exchangable PB
Pbindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_PB))
for(i in 1:length(SoilChem$EXCHNG_PB)){
  if(is.na(SoilChem$EXCHNG_PB[i])){Pbindex[i,1]=NA
  }else if(SoilChem$EXCHNG_PB[i]>1){Pbindex[i,1] = 0
  }else if(SoilChem$EXCHNG_PB[i]<= 1 & SoilChem$EXCHNG_PB[i]>=.1) {Pbindex[i,1] = 1}
  else (Pbindex[i,1]=1)
}
Pbindex<- as.data.frame(Pbindex)
#Exchangable S
Sindex = matrix(NA,ncol=1,nrow=length(SoilChem$EXCHNG_S))
for(i in 1:length(SoilChem$EXCHNG_S)){
  if(is.na(SoilChem$EXCHNG_S[i])){Sindex[i,1]=NA
  }else if(SoilChem$EXCHNG_S[i]>100){Sindex[i,1] = 0
  }else if(SoilChem$EXCHNG_S[i]<= 100 & SoilChem$EXCHNG_S[i]>=1) {Sindex[i,1] = 1}
  else (Sindex[i,1]=0)
}
Sindex<- as.data.frame(Sindex)
#Bray P Index
BrayPindex = matrix(NA,ncol=1,nrow=length(SoilChem$BRAY1_P))
for(i in 1:length(SoilChem$BRAY1_P)){
  if(is.na(SoilChem$BRAY1_P[i])){BrayPindex[i,1]=NA
  }else if(SoilChem$BRAY1_P[i]>30){BrayPindex[i,1] = 1
  }else if(SoilChem$BRAY1_P[i]<= 30 & SoilChem$BRAY1_P[i]>=15) {BrayPindex[i,1] = 1}
  else (BrayPindex[i,1]=0)
}
BrayPindex<- as.data.frame(BrayPindex)
#Olsen P Index
OlsenPindex = matrix(NA,ncol=1,nrow=length(SoilChem$OLSEN_P))
for(i in 1:length(SoilChem$OLSEN_P)){
  if(is.na(SoilChem$OLSEN_P[i])){OlsenPindex[i,1]=NA
  }else if(SoilChem$OLSEN_P[i]>30){OlsenPindex[i,1] = 1
  }else if(SoilChem$OLSEN_P[i]<= 30 & SoilChem$OLSEN_P[i]>=10) {OlsenPindex[i,1] = 1}
  else (OlsenPindex[i,1]=0)
}
OlsenPindex<- as.data.frame(OlsenPindex)
#combine all
SoilQIndex <- cbind(bulkindex,Coarseindex,PHindex,Carbonindex,Nitrogenindex,NaPCTindex,
                    Kindex,MGindex,CAindex,Alindex,Mnindex,Feindex,Niindex,Cuindex,Znindex,
                    Cdindex, Pbindex,Sindex,BrayPindex,OlsenPindex)

colnames(SoilQIndex)<- c("bulkindex","Coarseindex","PHindex","Carbonindex","Nitrogenindex","NaPCTindex",
                         "Kindex","MGindex","CAindex","Alindex","Mnindex","Feindex","Niindex","Cuindex","Znindex",
                         "Cdindex", "Pbindex","Sindex","BrayPindex","OlsenPindex")
SoilQIndex <- cbind(SoilChem$INVYR, SoilChem$PLT_CN, SoilQIndex)
names(SoilQIndex)[1]= "Year"
names(SoilQIndex)[2]="PlotCN"

#MaxSQI 
#if NA then 0, if number then max index, then sum SQI and max, sum/max*100
SQIMAX = matrix(0,ncol=1,nrow=length(SoilQIndex$Year))
for(i in 1:length(SoilQIndex$Year)){
  if(is.na(SoilQIndex$bulkindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else (SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$Coarseindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else(SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$PHindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else (SQIMAX[i,1]=SQIMAX[i,1]+2)
  if(is.na(SoilQIndex$Carbonindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else(SQIMAX[i,1]=SQIMAX[i,1]+2)
  if(is.na(SoilQIndex$Nitrogenindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else (SQIMAX[i,1]=SQIMAX[i,1]+2)
  if(is.na(SoilQIndex$NaPCTindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else(SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$Kindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else (SQIMAX[i,1]=SQIMAX[i,1]+2)
  if(is.na(SoilQIndex$MGindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else(SQIMAX[i,1]=SQIMAX[i,1]+2)
  if(is.na(SoilQIndex$CAindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else (SQIMAX[i,1]=SQIMAX[i,1]+2)
  if(is.na(SoilQIndex$Alindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else(SQIMAX[i,1]=SQIMAX[i,1]+2)
  if(is.na(SoilQIndex$Mnindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else (SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$Feindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else(SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$Niindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else (SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$Cuindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else(SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$Znindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else (SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$Cdindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else(SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$Pbindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else (SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$Sindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else(SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$BrayPindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else (SQIMAX[i,1]=SQIMAX[i,1]+1)
  if(is.na(SoilQIndex$OlsenPindex[i])){SQIMAX[i,1]=SQIMAX[i,1]+0}
  else(SQIMAX[i,1]=SQIMAX[i,1]+1)
}
SQIMAX<- as.data.frame(SQIMAX)

SoilQIndex <- cbind(SoilQIndex, SQIMAX)
names(SoilQIndex)[23]="SQIMax"
#sum/max*100 to determine final SQI percent
SQIPCT = matrix(NA,ncol=1,nrow=length(SoilQIndex$Year))
for(i in 1:length(SoilQIndex$Year)){
  SQIPCT[i,1] = (sum(SoilQIndex[i,3:22],na.rm=TRUE)/SoilQIndex$SQIMax[i])*100}
SQIPCT<- as.data.frame(SQIPCT)
SoilQIndex<-cbind(SoilQIndex, SQIPCT)
names(SoilQIndex)[24]="SQIPCT"
#Cut entries below minimum SQI max
SQIPCTGood <- as.data.frame(SoilQIndex%>%filter(SoilQIndex$SQIMax>10))
SQIPCTGood <- cbind(SQIPCTGood$Year,SQIPCTGood$Plot,SQIPCTGood$SQIPCT)
SQIPCTGood <- as.data.frame(SQIPCTGood)
names(SQIPCTGood)[1]="Year"
names(SQIPCTGood)[2]="PlotCN"
names(SQIPCTGood)[3]="SQIPCT"
#add coordinates, elevation, forest type
SQIPCTGood <- merge(SQIPCTGood,PlotData[,c(1,4:6)], by="PlotCN", all.x=TRUE)
SQIPCTGood <- merge(SQIPCTGood,ftype[,c(1,4)], by="PlotCN", all.x=TRUE)
#add soil order
coordsSQI <- st_as_sf(SQIPCTGood, coords = c("Longitude","Latitude"))
soil_orderSQI <- apply(st_coordinates(coordsSQI), 1, function(coords){
  point <- st_sfc(st_point(coords), crs=4326)
  get_soil_order(point)
})
soil_orderSQI <- as.data.frame(soil_orderSQI)
soil_orderSQI$PlotCN <- coordsSQI$PlotCN
SQIPCTGood <- merge(SQIPCTGood,soil_orderSQI,by="PlotCN",all.x=TRUE)
names(SQIPCTGood)[8]<- "soil_order"

#add FAD Class for SQI plots
coordsSQI <- SQIPCTGood[,c(1,2,4,5)]
coordsSQI <- SQIdata[,c(1,2,4,5)]

plotyear00 <- filter(coordsSQI, Year %in% c("2000"))
plotyear01 <- filter(coordsSQI, Year %in% c("2001"))
plotyear02 <- filter(coordsSQI, Year %in% c("2002"))
plotyear03 <- filter(coordsSQI, Year %in% c("2003"))
plotyear04 <- filter(coordsSQI, Year %in% c("2004"))
plotyear05 <- filter(coordsSQI, Year %in% c("2005"))

nlcdclass <- read.csv("nlcdclass.csv")
#include woody wetlands under forest designation
nlcdclass[15,1] <- nlcdclass[8,1]

crs84 <- '+proj=longlat +datum=WGS84'
#crs of nlcd spatraster'
x<-get_nlcd(template = FedData::meve, label = "meve", year = 2016)
xcrs <- crs(x)

SPDF00 <- SpatialPointsDataFrame(plotyear00[,4:3], data = plotyear00, proj4string =CRS(crs84))
SPDF01 <- SpatialPointsDataFrame(plotyear01[,4:3], data = plotyear01, proj4string =CRS(crs84))
SPDF02 <- SpatialPointsDataFrame(plotyear02[,4:3], data = plotyear02, proj4string =CRS(crs84))
SPDF03 <- SpatialPointsDataFrame(plotyear03[,4:3], data = plotyear03, proj4string =CRS(crs84))
SPDF04 <- SpatialPointsDataFrame(plotyear04[,4:3], data = plotyear04, proj4string =CRS(crs84))
SPDF05 <- SpatialPointsDataFrame(plotyear05[,4:3], data = plotyear05, proj4string =CRS(crs84))

SPDF00 <- spTransform(SPDF00, crs(xcrs))
SPDF01 <- spTransform(SPDF01, crs(xcrs))
SPDF02 <- spTransform(SPDF02, crs(xcrs))
SPDF03 <- spTransform(SPDF03, crs(xcrs))
SPDF04 <- spTransform(SPDF04, crs(xcrs))
SPDF05 <- spTransform(SPDF05, crs(xcrs))

nlsp00 <- st_as_sf(SPDF00)
st_crs(nlsp00) <- st_crs(xcrs)
nlsp01 <- st_as_sf(SPDF01)
st_crs(nlsp01) <- st_crs(xcrs)
nlsp02 <- st_as_sf(SPDF02)
st_crs(nlsp02) <- st_crs(xcrs)
nlsp03 <- st_as_sf(SPDF03)
st_crs(nlsp03) <- st_crs(xcrs)
nlsp04 <- st_as_sf(SPDF04)
st_crs(nlsp04) <- st_crs(xcrs)
nlsp05 <- st_as_sf(SPDF05)
st_crs(nlsp05) <- st_crs(xcrs)

nlsp00 <- st_buffer(nlsp00, dist = 118.5)
nlsp01 <- st_buffer(nlsp01, dist = 118.5)
nlsp02 <- st_buffer(nlsp02, dist = 118.5)
nlsp03 <- st_buffer(nlsp03, dist = 118.5)
nlsp04 <- st_buffer(nlsp04, dist = 118.5)
nlsp05 <- st_buffer(nlsp05, dist = 118.5)

#2000
movinglist <- list()
for(i in 1:length(nlsp00$PlotCN)){
  x <- get_nlcd_annual(template = nlsp00$geometry[i], label = nlsp00$PlotCN[i], year = 2000)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp00$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp00$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea00 <- data.frame(unlist(fadlist))
forestarea00$PlotCN <- names(fadlist)
names(forestarea00)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea00$FADClass <- ifelse(forestarea00$FAD >=90, 5,
                                ifelse(forestarea00$FAD >=60, 4,
                                       ifelse(forestarea00$FAD >=40, 3,
                                              ifelse(forestarea00$FAD>=10, 2, 1))))
forestarea00$PlotCN<- as.numeric(forestarea00$PlotCN)
#2001
movinglist <- list()
for(i in 1:length(nlsp01$PlotCN)){
  x <- get_nlcd_annual(template = nlsp01$geometry[i], label = nlsp01$PlotCN[i], year = 2001)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp01$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp01$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea01 <- data.frame(unlist(fadlist))
forestarea01$PlotCN <- names(fadlist)
names(forestarea01)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea01$FADClass <- ifelse(forestarea01$FAD >=90, 5,
                                ifelse(forestarea01$FAD >=60, 4,
                                       ifelse(forestarea01$FAD >=40, 3,
                                              ifelse(forestarea01$FAD>=10, 2, 1))))
forestarea01$PlotCN<- as.numeric(forestarea01$PlotCN)
#repeat for other years
movinglist <- list()
for(i in 1:length(nlsp02$PlotCN)){
  x <- get_nlcd_annual(template = nlsp02$geometry[i], label = nlsp02$PlotCN[i], year = 2002)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp02$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp02$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea02 <- data.frame(unlist(fadlist))
forestarea02$PlotCN <- names(fadlist)
names(forestarea02)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea02$FADClass <- ifelse(forestarea02$FAD >=90, 5,
                                ifelse(forestarea02$FAD >=60, 4,
                                       ifelse(forestarea02$FAD >=40, 3,
                                              ifelse(forestarea02$FAD>=10, 2, 1))))
forestarea02$PlotCN<- as.numeric(forestarea02$PlotCN)
#2003
movinglist <- list()
for(i in 1:length(nlsp03$PlotCN)){
  x <- get_nlcd_annual(template = nlsp03$geometry[i], label = nlsp03$PlotCN[i], year = 2003)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp03$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp03$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea03 <- data.frame(unlist(fadlist))
forestarea03$PlotCN <- names(fadlist)
names(forestarea03)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea03$FADClass <- ifelse(forestarea03$FAD >=90, 5,
                                ifelse(forestarea03$FAD >=60, 4,
                                       ifelse(forestarea03$FAD >=40, 3,
                                              ifelse(forestarea03$FAD>=10, 2, 1))))
forestarea03$PlotCN<- as.numeric(forestarea03$PlotCN)

#2004
movinglist <- list()
for(i in 1:length(nlsp04$PlotCN)){
  x <- get_nlcd_annual(template = nlsp04$geometry[i], label = nlsp04$PlotCN[i], year = 2004)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp04$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp04$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea04 <- data.frame(unlist(fadlist))
forestarea04$PlotCN <- names(fadlist)
names(forestarea04)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea04$FADClass <- ifelse(forestarea04$FAD >=90, 5,
                                ifelse(forestarea04$FAD >=60, 4,
                                       ifelse(forestarea04$FAD >=40, 3,
                                              ifelse(forestarea04$FAD>=10, 2, 1))))
forestarea04$PlotCN<- as.numeric(forestarea04$PlotCN)

#2005
movinglist <- list()
for(i in 1:length(nlsp05$PlotCN)){
  x <- get_nlcd_annual(template = nlsp05$geometry[i], label = nlsp05$PlotCN[i], year = 2005)
  x <- rast(x$rast)
  c <- as_Spatial(nlsp05$geometry[i])
  r <- terra::crop(x, ext(c))
  movinglist[i]<- raster::focal(r, w=matrix(1,7,7),fun=modal)
}
names(movinglist) <- nlsp05$PlotCN

#run pland and process 
fadlist <- list()
for(i in 1:length(movinglist)){
  x1 <- data.frame(lsm_c_pland(movinglist[i], directions = 8))
  names(x1)[3] = "code"
  x1 <- merge(x1,nlcdclass, by = "code", all.x = TRUE)
  x1class <- aggregate(x1$value, by = list(x1$layer, x1$class), FUN = sum)
  colnames(x1class) <- c('layer', 'class', 'pland')
  x1class <- x1class %>% dplyr::select(pland, layer, class) %>% filter(class == "    forest ")
  fadlist[i] <- x1class
}
names(fadlist) <- names(movinglist)
fadlist[lengths(fadlist) == 0] <- 0
#unlist and calculate forest area density
forestarea05 <- data.frame(unlist(fadlist))
forestarea05$PlotCN <- names(fadlist)
names(forestarea05)[1] <- "FAD"

#categories 100 intact(6), 99-90-interior(5), 89-60 dominant(4), 59-40 transitional(3), 39-10 patchy(2), 9-1 rare(1), 0 none
forestarea05$FADClass <- ifelse(forestarea05$FAD >=90, 5,
                                ifelse(forestarea05$FAD >=60, 4,
                                       ifelse(forestarea05$FAD >=40, 3,
                                              ifelse(forestarea05$FAD>=10, 2, 1))))
forestarea05$PlotCN<- as.numeric(forestarea05$PlotCN)

FADdf <- rbind(forestarea01,forestarea02,forestarea03,forestarea04,forestarea05)

SQIPCTGood <- merge(SQIPCTGood,FADdf[,2:3],by="PlotCN", all.x=TRUE) # you run with SQIdata
SQIPCTGood <- distinct(SQIPCTGood) # you run with SQIdata

SQIdata <- merge(SQIdata,FADdf[,2:3],by="PlotCN", all.x=TRUE) # you run with SQIdata
SQIdata <- distinct(SQIdata) # you run with SQIdata
write.csv(SQIdata, file="allSQIfinal.csv")




#save in case of failure
setwd("~/Desktop/FIA/")
write.csv(allFIA, file="allFIAinc.csv")
write.csv(SQIPCTGood, file="midAtlSQI.csv")
#Current and Future Climate for all plots
#combine coords
coords <- allFIA[,c(1,2,8,9)]
allcoords <- rbind(coords,SQIPCTGood[,c(1:2,4:5)])
#allcoords1 for future climate
allcoords1 <- allcoords %>%distinct(Longitude, Latitude, .keep_all = TRUE)
allcoords <- vect(allcoords, geom=c("Longitude","Latitude"))
allcoords1 <- vect(allcoords1, geom=c("Longitude","Latitude"))

#Climate variables files included in git hub data section. Large files. Different folders for each model, GFDL,MIROC,NORESM
#historical first - 2000-2014
#air temp
#GFDL
#2000
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2000  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2000.nc", subds="tas")
GFDL2000  <- terra::rotate(GFDL2000)
GFDLTEMP2000<-data.frame(terra::extract(GFDL2000 ,allcoords[allcoords$Year == 2000,]))
GFDLTEMP2000$AvgTemp <- rowMeans(GFDLTEMP2000[,2:366])
#Add PlotCN
GFDLTEMP2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
#convert K  to C
GFDLTEMP2000$AvgTemp <- GFDLTEMP2000$AvgTemp-273.15
rm(GFDL2000)
#2001
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2001  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2001.nc", subds="tas")
GFDL2001  <- terra::rotate(GFDL2001)
GFDLTEMP2001<-data.frame(terra::extract(GFDL2001 ,allcoords[allcoords$Year == 2001,]))
GFDLTEMP2001$AvgTemp <- rowMeans(GFDLTEMP2001[,2:366])
#Add PlotCN
GFDLTEMP2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
#convert K  to C
GFDLTEMP2001$AvgTemp <- GFDLTEMP2001$AvgTemp-273.15
rm(GFDL2001)
#2002
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2002  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2002.nc", subds="tas")
GFDL2002  <- terra::rotate(GFDL2002)
GFDLTEMP2002<-data.frame(terra::extract(GFDL2002 ,allcoords[allcoords$Year == 2002,]))
GFDLTEMP2002$AvgTemp <- rowMeans(GFDLTEMP2002[,2:366])
#Add PlotCN
GFDLTEMP2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
#convert K  to C
GFDLTEMP2002$AvgTemp <- GFDLTEMP2002$AvgTemp-273.15
rm(GFDL2002)
#2003
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2003  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2003.nc", subds="tas")
GFDL2003  <- terra::rotate(GFDL2003)
GFDLTEMP2003<-data.frame(terra::extract(GFDL2003 ,allcoords[allcoords$Year == 2003,]))
GFDLTEMP2003$AvgTemp <- rowMeans(GFDLTEMP2003[,2:366])
#Add PlotCN
GFDLTEMP2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
#convert K  to C
GFDLTEMP2003$AvgTemp <- GFDLTEMP2003$AvgTemp-273.15
rm(GFDL2003)
#2004
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2004  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2004.nc", subds="tas")
GFDL2004  <- terra::rotate(GFDL2004)
GFDLTEMP2004<-data.frame(terra::extract(GFDL2004 ,allcoords[allcoords$Year == 2004,]))
GFDLTEMP2004$AvgTemp <- rowMeans(GFDLTEMP2004[,2:366])
#Add PlotCN
GFDLTEMP2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
#convert K  to C
GFDLTEMP2004$AvgTemp <- GFDLTEMP2004$AvgTemp-273.15
rm(GFDL2004)
#2005
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2005  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2005.nc", subds="tas")
GFDL2005  <- terra::rotate(GFDL2005)
GFDLTEMP2005<-data.frame(terra::extract(GFDL2005 ,allcoords[allcoords$Year == 2005,]))
GFDLTEMP2005$AvgTemp <- rowMeans(GFDLTEMP2005[,2:366])
#Add PlotCN
GFDLTEMP2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
#convert K  to C
GFDLTEMP2005$AvgTemp <- GFDLTEMP2005$AvgTemp-273.15
rm(GFDL2005)
#2006
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2006  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2006.nc", subds="tas")
GFDL2006  <- terra::rotate(GFDL2006)
GFDLTEMP2006<-data.frame(terra::extract(GFDL2006 ,allcoords[allcoords$Year == 2006,]))
GFDLTEMP2006$AvgTemp <- rowMeans(GFDLTEMP2006[,2:366])
#Add PlotCN
GFDLTEMP2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
#convert K  to C
GFDLTEMP2006$AvgTemp <- GFDLTEMP2006$AvgTemp-273.15
rm(GFDL2006)
#2007
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2007  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2007.nc", subds="tas")
GFDL2007  <- terra::rotate(GFDL2007)
GFDLTEMP2007<-data.frame(terra::extract(GFDL2007 ,allcoords[allcoords$Year == 2007,]))
GFDLTEMP2007$AvgTemp <- rowMeans(GFDLTEMP2007[,2:366])
#Add PlotCN
GFDLTEMP2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
#convert K  to C
GFDLTEMP2007$AvgTemp <- GFDLTEMP2007$AvgTemp-273.15
rm(GFDL2007)
#2008
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2008  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2008.nc", subds="tas")
GFDL2008  <- terra::rotate(GFDL2008)
GFDLTEMP2008<-data.frame(terra::extract(GFDL2008 ,allcoords[allcoords$Year == 2008,]))
GFDLTEMP2008$AvgTemp <- rowMeans(GFDLTEMP2008[,2:366])
#Add PlotCN
GFDLTEMP2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
#convert K  to C
GFDLTEMP2008$AvgTemp <- GFDLTEMP2008$AvgTemp-273.15
rm(GFDL2008)
#2009
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2009  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2009.nc", subds="tas")
GFDL2009  <- terra::rotate(GFDL2009)
GFDLTEMP2009<-data.frame(terra::extract(GFDL2009 ,allcoords[allcoords$Year == 2009,]))
GFDLTEMP2009$AvgTemp <- rowMeans(GFDLTEMP2009[,2:366])
#Add PlotCN
GFDLTEMP2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
#convert K  to C
GFDLTEMP2009$AvgTemp <- GFDLTEMP2009$AvgTemp-273.15
rm(GFDL2009)
#2010
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2010  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2010.nc", subds="tas")
GFDL2010  <- terra::rotate(GFDL2010)
GFDLTEMP2010<-data.frame(terra::extract(GFDL2010 ,allcoords[allcoords$Year == 2010,]))
GFDLTEMP2010$AvgTemp <- rowMeans(GFDLTEMP2010[,2:366])
#Add PlotCN
GFDLTEMP2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
#convert K  to C
GFDLTEMP2010$AvgTemp <- GFDLTEMP2010$AvgTemp-273.15
rm(GFDL2010)
#2011
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2011  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2011.nc", subds="tas")
GFDL2011  <- terra::rotate(GFDL2011)
GFDLTEMP2011<-data.frame(terra::extract(GFDL2011 ,allcoords[allcoords$Year == 2011,]))
GFDLTEMP2011$AvgTemp <- rowMeans(GFDLTEMP2011[,2:366])
#Add PlotCN
GFDLTEMP2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
#convert K  to C
GFDLTEMP2011$AvgTemp <- GFDLTEMP2011$AvgTemp-273.15
rm(GFDL2011)
#2012
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2012  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2012.nc", subds="tas")
GFDL2012  <- terra::rotate(GFDL2012)
GFDLTEMP2012<-data.frame(terra::extract(GFDL2012 ,allcoords[allcoords$Year == 2012,]))
GFDLTEMP2012$AvgTemp <- rowMeans(GFDLTEMP2012[,2:366])
#Add PlotCN
GFDLTEMP2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
#convert K  to C
GFDLTEMP2012$AvgTemp <- GFDLTEMP2012$AvgTemp-273.15
rm(GFDL2012)
#2013
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2013  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2013.nc", subds="tas")
GFDL2013  <- terra::rotate(GFDL2013)
GFDLTEMP2013<-data.frame(terra::extract(GFDL2013 ,allcoords[allcoords$Year == 2013,]))
GFDLTEMP2013$AvgTemp <- rowMeans(GFDLTEMP2013[,2:366])
#Add PlotCN
GFDLTEMP2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
#convert K  to C
GFDLTEMP2013$AvgTemp <- GFDLTEMP2013$AvgTemp-273.15
rm(GFDL2013)
#2014
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2014  <- rast("~/Desktop/NASAClim/GFDLH/GFDLTa2014.nc", subds="tas")
GFDL2014  <- terra::rotate(GFDL2014)
GFDLTEMP2014<-data.frame(terra::extract(GFDL2014 ,allcoords[allcoords$Year == 2014,]))
GFDLTEMP2014$AvgTemp <- rowMeans(GFDLTEMP2014[,2:366])
#Add PlotCN
GFDLTEMP2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
#convert K  to C
GFDLTEMP2014$AvgTemp <- GFDLTEMP2014$AvgTemp-273.15
rm(GFDL2014)

GFDLTempFull<- as.data.frame(rbind(GFDLTEMP2000[,367:368],GFDLTEMP2001[,367:368],GFDLTEMP2002[,367:368],GFDLTEMP2003[,367:368],GFDLTEMP2004[,367:368],GFDLTEMP2005[,367:368], GFDLTEMP2006[,367:368],
                                   GFDLTEMP2007[,367:368], GFDLTEMP2008[,367:368], GFDLTEMP2009[,367:368], GFDLTEMP2010[,367:368], GFDLTEMP2011[,367:368],
                                   GFDLTEMP2012[,367:368], GFDLTEMP2013[,367:368], GFDLTEMP2014[,367:368]))
GFDLTempFull <- GFDLTempFull[,2:1]

rm(GFDLTEMP2000,GFDLTEMP2001,GFDLTEMP2002,GFDLTEMP2003,GFDLTEMP2004,GFDLTEMP2005, 
   GFDLTEMP2006,GFDLTEMP2007, GFDLTEMP2008, GFDLTEMP2009, 
   GFDLTEMP2010, GFDLTEMP2011, GFDLTEMP2012, GFDLTEMP2013, GFDLTEMP2014)

#MIROC
#2000
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2000  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2000.nc", subds="tas")
MIROC2000  <- terra::rotate(MIROC2000)
MIROCTEMP2000<-data.frame(terra::extract(MIROC2000 ,allcoords[allcoords$Year == 2000,]))
MIROCTEMP2000$AvgTemp <- rowMeans(MIROCTEMP2000[,2:367])
#Add PlotCN
MIROCTEMP2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
#convert K  to C
MIROCTEMP2000$AvgTemp <- MIROCTEMP2000$AvgTemp-273.15
rm(MIROC2000)
#2001
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2001  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2001.nc", subds="tas")
MIROC2001  <- terra::rotate(MIROC2001)
MIROCTEMP2001<-data.frame(terra::extract(MIROC2001 ,allcoords[allcoords$Year == 2001,]))
MIROCTEMP2001$AvgTemp <- rowMeans(MIROCTEMP2001[,2:366])
#Add PlotCN
MIROCTEMP2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
#convert K  to C
MIROCTEMP2001$AvgTemp <- MIROCTEMP2001$AvgTemp-273.15
rm(MIROC2001)
#2002
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2002  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2002.nc", subds="tas")
MIROC2002  <- terra::rotate(MIROC2002)
MIROCTEMP2002<-data.frame(terra::extract(MIROC2002 ,allcoords[allcoords$Year == 2002,]))
MIROCTEMP2002$AvgTemp <- rowMeans(MIROCTEMP2002[,2:366])
#Add PlotCN
MIROCTEMP2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
#convert K  to C
MIROCTEMP2002$AvgTemp <- MIROCTEMP2002$AvgTemp-273.15
rm(MIROC2002)
#2003
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2003  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2003.nc", subds="tas")
MIROC2003  <- terra::rotate(MIROC2003)
MIROCTEMP2003<-data.frame(terra::extract(MIROC2003 ,allcoords[allcoords$Year == 2003,]))
MIROCTEMP2003$AvgTemp <- rowMeans(MIROCTEMP2003[,2:366])
#Add PlotCN
MIROCTEMP2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
#convert K  to C
MIROCTEMP2003$AvgTemp <- MIROCTEMP2003$AvgTemp-273.15
rm(MIROC2003)
#2004
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2004  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2004.nc", subds="tas")
MIROC2004  <- terra::rotate(MIROC2004)
MIROCTEMP2004<-data.frame(terra::extract(MIROC2004 ,allcoords[allcoords$Year == 2004,]))
MIROCTEMP2004$AvgTemp <- rowMeans(MIROCTEMP2004[,2:367])
#Add PlotCN
MIROCTEMP2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
#convert K  to C
MIROCTEMP2004$AvgTemp <- MIROCTEMP2004$AvgTemp-273.15
rm(MIROC2004)
#2005
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2005  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2005.nc", subds="tas")
MIROC2005  <- terra::rotate(MIROC2005)
MIROCTEMP2005<-data.frame(terra::extract(MIROC2005 ,allcoords[allcoords$Year == 2005,]))
MIROCTEMP2005$AvgTemp <- rowMeans(MIROCTEMP2005[,2:366])
#Add PlotCN
MIROCTEMP2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
#convert K  to C
MIROCTEMP2005$AvgTemp <- MIROCTEMP2005$AvgTemp-273.15
rm(MIROC2005)
#2006
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2006  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2006.nc", subds="tas")
MIROC2006  <- terra::rotate(MIROC2006)
MIROCTEMP2006<-data.frame(terra::extract(MIROC2006 ,allcoords[allcoords$Year == 2006,]))
MIROCTEMP2006$AvgTemp <- rowMeans(MIROCTEMP2006[,2:366])
#Add PlotCN
MIROCTEMP2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
#convert K  to C
MIROCTEMP2006$AvgTemp <- MIROCTEMP2006$AvgTemp-273.15
rm(MIROC2006)
#2007
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2007  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2007.nc", subds="tas")
MIROC2007  <- terra::rotate(MIROC2007)
MIROCTEMP2007<-data.frame(terra::extract(MIROC2007 ,allcoords[allcoords$Year == 2007,]))
MIROCTEMP2007$AvgTemp <- rowMeans(MIROCTEMP2007[,2:366])
#Add PlotCN
MIROCTEMP2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
#convert K  to C
MIROCTEMP2007$AvgTemp <- MIROCTEMP2007$AvgTemp-273.15
rm(MIROC2007)
#2008
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2008  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2008.nc", subds="tas")
MIROC2008  <- terra::rotate(MIROC2008)
MIROCTEMP2008<-data.frame(terra::extract(MIROC2008 ,allcoords[allcoords$Year == 2008,]))
MIROCTEMP2008$AvgTemp <- rowMeans(MIROCTEMP2008[,2:367])
#Add PlotCN
MIROCTEMP2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
#convert K  to C
MIROCTEMP2008$AvgTemp <- MIROCTEMP2008$AvgTemp-273.15
rm(MIROC2008)
#2009
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2009  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2009.nc", subds="tas")
MIROC2009  <- terra::rotate(MIROC2009)
MIROCTEMP2009<-data.frame(terra::extract(MIROC2009 ,allcoords[allcoords$Year == 2009,]))
MIROCTEMP2009$AvgTemp <- rowMeans(MIROCTEMP2009[,2:366])
#Add PlotCN
MIROCTEMP2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
#convert K  to C
MIROCTEMP2009$AvgTemp <- MIROCTEMP2009$AvgTemp-273.15
rm(MIROC2009)
#2010
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2010  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2010.nc", subds="tas")
MIROC2010  <- terra::rotate(MIROC2010)
MIROCTEMP2010<-data.frame(terra::extract(MIROC2010 ,allcoords[allcoords$Year == 2010,]))
MIROCTEMP2010$AvgTemp <- rowMeans(MIROCTEMP2010[,2:366])
#Add PlotCN
MIROCTEMP2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
#convert K  to C
MIROCTEMP2010$AvgTemp <- MIROCTEMP2010$AvgTemp-273.15
rm(MIROC2010)
#2011
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2011  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2011.nc", subds="tas")
MIROC2011  <- terra::rotate(MIROC2011)
MIROCTEMP2011<-data.frame(terra::extract(MIROC2011 ,allcoords[allcoords$Year == 2011,]))
MIROCTEMP2011$AvgTemp <- rowMeans(MIROCTEMP2011[,2:366])
#Add PlotCN
MIROCTEMP2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
#convert K  to C
MIROCTEMP2011$AvgTemp <- MIROCTEMP2011$AvgTemp-273.15
rm(MIROC2011)
#2012
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2012  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2012.nc", subds="tas")
MIROC2012  <- terra::rotate(MIROC2012)
MIROCTEMP2012<-data.frame(terra::extract(MIROC2012 ,allcoords[allcoords$Year == 2012,]))
MIROCTEMP2012$AvgTemp <- rowMeans(MIROCTEMP2012[,2:367])
#Add PlotCN
MIROCTEMP2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
#convert K  to C
MIROCTEMP2012$AvgTemp <- MIROCTEMP2012$AvgTemp-273.15
rm(MIROC2012)
#2013
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2013  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2013.nc", subds="tas")
MIROC2013  <- terra::rotate(MIROC2013)
MIROCTEMP2013<-data.frame(terra::extract(MIROC2013 ,allcoords[allcoords$Year == 2013,]))
MIROCTEMP2013$AvgTemp <- rowMeans(MIROCTEMP2013[,2:366])
#Add PlotCN
MIROCTEMP2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
#convert K  to C
MIROCTEMP2013$AvgTemp <- MIROCTEMP2013$AvgTemp-273.15
rm(MIROC2013)
#2014
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2014  <- rast("~/Desktop/NASAClim/MIROCH/MIROCTa2014.nc", subds="tas")
MIROC2014  <- terra::rotate(MIROC2014)
MIROCTEMP2014<-data.frame(terra::extract(MIROC2014 ,allcoords[allcoords$Year == 2014,]))
MIROCTEMP2014$AvgTemp <- rowMeans(MIROCTEMP2014[,2:366])
#Add PlotCN
MIROCTEMP2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
#convert K  to C
MIROCTEMP2014$AvgTemp <- MIROCTEMP2014$AvgTemp-273.15
rm(MIROC2014)
MIROCTempFull<- as.data.frame(rbind(MIROCTEMP2000[,368:369],MIROCTEMP2001[,367:368],MIROCTEMP2002[,367:368],MIROCTEMP2003[,367:368],MIROCTEMP2004[,368:369],MIROCTEMP2005[,367:368], MIROCTEMP2006[,367:368],
                                    MIROCTEMP2007[,367:368], MIROCTEMP2008[,368:369], MIROCTEMP2009[,367:368], MIROCTEMP2010[,367:368], MIROCTEMP2011[,367:368],
                                    MIROCTEMP2012[,368:369], MIROCTEMP2013[,367:368], MIROCTEMP2014[,367:368]))
MIROCTempFull <- MIROCTempFull[,2:1]

rm(MIROCTEMP2000,MIROCTEMP2001,MIROCTEMP2002,MIROCTEMP2003,MIROCTEMP2004,MIROCTEMP2005, 
   MIROCTEMP2006,MIROCTEMP2007, MIROCTEMP2008, MIROCTEMP2009, 
   MIROCTEMP2010, MIROCTEMP2011, MIROCTEMP2012, MIROCTEMP2013, MIROCTEMP2014)

#NORESM
#2000
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2000  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2000.nc", subds="tas")
NORESM2000  <- terra::rotate(NORESM2000)
NORESMTEMP2000<-data.frame(terra::extract(NORESM2000 ,allcoords[allcoords$Year == 2000,]))
NORESMTEMP2000$AvgTemp <- rowMeans(NORESMTEMP2000[,2:366])
#Add PlotCN
NORESMTEMP2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
#convert K  to C
NORESMTEMP2000$AvgTemp <- NORESMTEMP2000$AvgTemp-273.15
rm(NORESM2000)
#2001
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2001  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2001.nc", subds="tas")
NORESM2001  <- terra::rotate(NORESM2001)
NORESMTEMP2001<-data.frame(terra::extract(NORESM2001 ,allcoords[allcoords$Year == 2001,]))
NORESMTEMP2001$AvgTemp <- rowMeans(NORESMTEMP2001[,2:366])
#Add PlotCN
NORESMTEMP2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
#convert K  to C
NORESMTEMP2001$AvgTemp <- NORESMTEMP2001$AvgTemp-273.15
rm(NORESM2001)
#2002
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2002  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2002.nc", subds="tas")
NORESM2002  <- terra::rotate(NORESM2002)
NORESMTEMP2002<-data.frame(terra::extract(NORESM2002 ,allcoords[allcoords$Year == 2002,]))
NORESMTEMP2002$AvgTemp <- rowMeans(NORESMTEMP2002[,2:366])
#Add PlotCN
NORESMTEMP2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
#convert K  to C
NORESMTEMP2002$AvgTemp <- NORESMTEMP2002$AvgTemp-273.15
rm(NORESM2002)
#2003
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2003  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2003.nc", subds="tas")
NORESM2003  <- terra::rotate(NORESM2003)
NORESMTEMP2003<-data.frame(terra::extract(NORESM2003 ,allcoords[allcoords$Year == 2003,]))
NORESMTEMP2003$AvgTemp <- rowMeans(NORESMTEMP2003[,2:366])
#Add PlotCN
NORESMTEMP2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
#convert K  to C
NORESMTEMP2003$AvgTemp <- NORESMTEMP2003$AvgTemp-273.15
rm(NORESM2003)
#2004
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2004  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2004.nc", subds="tas")
NORESM2004  <- terra::rotate(NORESM2004)
NORESMTEMP2004<-data.frame(terra::extract(NORESM2004 ,allcoords[allcoords$Year == 2004,]))
NORESMTEMP2004$AvgTemp <- rowMeans(NORESMTEMP2004[,2:366])
#Add PlotCN
NORESMTEMP2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
#convert K  to C
NORESMTEMP2004$AvgTemp <- NORESMTEMP2004$AvgTemp-273.15
rm(NORESM2004)
#2005
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2005  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2005.nc", subds="tas")
NORESM2005  <- terra::rotate(NORESM2005)
NORESMTEMP2005<-data.frame(terra::extract(NORESM2005 ,allcoords[allcoords$Year == 2005,]))
NORESMTEMP2005$AvgTemp <- rowMeans(NORESMTEMP2005[,2:366])
#Add PlotCN
NORESMTEMP2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
#convert K  to C
NORESMTEMP2005$AvgTemp <- NORESMTEMP2005$AvgTemp-273.15
rm(NORESM2005)
#2006
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2006  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2006.nc", subds="tas")
NORESM2006  <- terra::rotate(NORESM2006)
NORESMTEMP2006<-data.frame(terra::extract(NORESM2006 ,allcoords[allcoords$Year == 2006,]))
NORESMTEMP2006$AvgTemp <- rowMeans(NORESMTEMP2006[,2:366])
#Add PlotCN
NORESMTEMP2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
#convert K  to C
NORESMTEMP2006$AvgTemp <- NORESMTEMP2006$AvgTemp-273.15
rm(NORESM2006)
#2007
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2007  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2007.nc", subds="tas")
NORESM2007  <- terra::rotate(NORESM2007)
NORESMTEMP2007<-data.frame(terra::extract(NORESM2007 ,allcoords[allcoords$Year == 2007,]))
NORESMTEMP2007$AvgTemp <- rowMeans(NORESMTEMP2007[,2:366])
#Add PlotCN
NORESMTEMP2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
#convert K  to C
NORESMTEMP2007$AvgTemp <- NORESMTEMP2007$AvgTemp-273.15
rm(NORESM2007)
#2008
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2008  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2008.nc", subds="tas")
NORESM2008  <- terra::rotate(NORESM2008)
NORESMTEMP2008<-data.frame(terra::extract(NORESM2008 ,allcoords[allcoords$Year == 2008,]))
NORESMTEMP2008$AvgTemp <- rowMeans(NORESMTEMP2008[,2:366])
#Add PlotCN
NORESMTEMP2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
#convert K  to C
NORESMTEMP2008$AvgTemp <- NORESMTEMP2008$AvgTemp-273.15
rm(NORESM2008)
#2009
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2009  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2009.nc", subds="tas")
NORESM2009  <- terra::rotate(NORESM2009)
NORESMTEMP2009<-data.frame(terra::extract(NORESM2009 ,allcoords[allcoords$Year == 2009,]))
NORESMTEMP2009$AvgTemp <- rowMeans(NORESMTEMP2009[,2:366])
#Add PlotCN
NORESMTEMP2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
#convert K  to C
NORESMTEMP2009$AvgTemp <- NORESMTEMP2009$AvgTemp-273.15
rm(NORESM2009)
#2010
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2010  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2010.nc", subds="tas")
NORESM2010  <- terra::rotate(NORESM2010)
NORESMTEMP2010<-data.frame(terra::extract(NORESM2010 ,allcoords[allcoords$Year == 2010,]))
NORESMTEMP2010$AvgTemp <- rowMeans(NORESMTEMP2010[,2:366])
#Add PlotCN
NORESMTEMP2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
#convert K  to C
NORESMTEMP2010$AvgTemp <- NORESMTEMP2010$AvgTemp-273.15
rm(NORESM2010)
#2011
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2011  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2011.nc", subds="tas")
NORESM2011  <- terra::rotate(NORESM2011)
NORESMTEMP2011<-data.frame(terra::extract(NORESM2011 ,allcoords[allcoords$Year == 2011,]))
NORESMTEMP2011$AvgTemp <- rowMeans(NORESMTEMP2011[,2:366])
#Add PlotCN
NORESMTEMP2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
#convert K  to C
NORESMTEMP2011$AvgTemp <- NORESMTEMP2011$AvgTemp-273.15
rm(NORESM2011)
#NORESM
#2012
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2012  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2012.nc", subds="tas")
NORESM2012  <- terra::rotate(NORESM2012)
NORESMTEMP2012<-data.frame(terra::extract(NORESM2012 ,allcoords[allcoords$Year == 2012,]))
NORESMTEMP2012$AvgTemp <- rowMeans(NORESMTEMP2012[,2:366])
#Add PlotCN
NORESMTEMP2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
#convert K  to C
NORESMTEMP2012$AvgTemp <- NORESMTEMP2012$AvgTemp-273.15
rm(NORESM2012)
#2013
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2013  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2013.nc", subds="tas")
NORESM2013  <- terra::rotate(NORESM2013)
NORESMTEMP2013<-data.frame(terra::extract(NORESM2013 ,allcoords[allcoords$Year == 2013,]))
NORESMTEMP2013$AvgTemp <- rowMeans(NORESMTEMP2013[,2:366])
#Add PlotCN
NORESMTEMP2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
#convert K  to C
NORESMTEMP2013$AvgTemp <- NORESMTEMP2013$AvgTemp-273.15
rm(NORESM2013)
#2014
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2014  <- rast("~/Desktop/NASAClim/NORESMH/NORESMTa2014.nc", subds="tas")
NORESM2014  <- terra::rotate(NORESM2014)
NORESMTEMP2014<-data.frame(terra::extract(NORESM2014 ,allcoords[allcoords$Year == 2014,]))
NORESMTEMP2014$AvgTemp <- rowMeans(NORESMTEMP2014[,2:366])
#Add PlotCN
NORESMTEMP2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
#convert K  to C
NORESMTEMP2014$AvgTemp <- NORESMTEMP2014$AvgTemp-273.15
rm(NORESM2014)
NORESMTempFull<- as.data.frame(rbind(NORESMTEMP2000[,367:368],NORESMTEMP2001[,367:368],NORESMTEMP2002[,367:368],NORESMTEMP2003[,367:368],NORESMTEMP2004[,367:368],NORESMTEMP2005[,367:368], NORESMTEMP2006[,367:368],
                                     NORESMTEMP2007[,367:368], NORESMTEMP2008[,367:368], NORESMTEMP2009[,367:368], NORESMTEMP2010[,367:368], NORESMTEMP2011[,367:368],
                                     NORESMTEMP2012[,367:368], NORESMTEMP2013[,367:368], NORESMTEMP2014[,367:368]))
NORESMTempFull <- NORESMTempFull[,2:1]

rm(NORESMTEMP2000,NORESMTEMP2001,NORESMTEMP2002,NORESMTEMP2003,NORESMTEMP2004,NORESMTEMP2005, 
   NORESMTEMP2006,NORESMTEMP2007, NORESMTEMP2008, NORESMTEMP2009, 
   NORESMTEMP2010, NORESMTEMP2011, NORESMTEMP2012, NORESMTEMP2013, NORESMTEMP2014)

#prec
#GFDL	
#2000
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2000  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2000.nc", subds="pr")
GFDL2000  <- terra::rotate(GFDL2000)
GFDLPREC2000<-data.frame(terra::extract(GFDL2000 ,allcoords[allcoords$Year == 2000,]))
GFDLPREC2000$PrecSum <- rowSums(GFDLPREC2000[,2:366])
#Add PlotCN
GFDLPREC2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
##covert to mm
GFDLPREC2000$PrecSum <- GFDLPREC2000$PrecSum*86400
rm(GFDL2000)
#2001
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2001  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2001.nc", subds="pr")
GFDL2001  <- terra::rotate(GFDL2001)
GFDLPREC2001<-data.frame(terra::extract(GFDL2001 ,allcoords[allcoords$Year == 2001,]))
GFDLPREC2001$PrecSum <- rowSums(GFDLPREC2001[,2:366])
#Add PlotCN
GFDLPREC2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
##covert to mm
GFDLPREC2001$PrecSum <- GFDLPREC2001$PrecSum*86400
rm(GFDL2001)
#2002
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2002  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2002.nc", subds="pr")
GFDL2002  <- terra::rotate(GFDL2002)
GFDLPREC2002<-data.frame(terra::extract(GFDL2002 ,allcoords[allcoords$Year == 2002,]))
GFDLPREC2002$PrecSum <- rowSums(GFDLPREC2002[,2:366])
#Add PlotCN
GFDLPREC2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
##covert to mm
GFDLPREC2002$PrecSum <- GFDLPREC2002$PrecSum*86400
rm(GFDL2002)
#2003
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2003  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2003.nc", subds="pr")
GFDL2003  <- terra::rotate(GFDL2003)
GFDLPREC2003<-data.frame(terra::extract(GFDL2003 ,allcoords[allcoords$Year == 2003,]))
GFDLPREC2003$PrecSum <- rowSums(GFDLPREC2003[,2:366])
#Add PlotCN
GFDLPREC2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
##covert to mm
GFDLPREC2003$PrecSum <- GFDLPREC2003$PrecSum*86400
rm(GFDL2003)
#2004
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2004  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2004.nc", subds="pr")
GFDL2004  <- terra::rotate(GFDL2004)
GFDLPREC2004<-data.frame(terra::extract(GFDL2004 ,allcoords[allcoords$Year == 2004,]))
GFDLPREC2004$PrecSum <- rowSums(GFDLPREC2004[,2:366])
#Add PlotCN
GFDLPREC2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
##covert to mm
GFDLPREC2004$PrecSum <- GFDLPREC2004$PrecSum*86400
rm(GFDL2004)
#2005
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2005  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2005.nc", subds="pr")
GFDL2005  <- terra::rotate(GFDL2005)
GFDLPREC2005<-data.frame(terra::extract(GFDL2005 ,allcoords[allcoords$Year == 2005,]))
GFDLPREC2005$PrecSum <- rowSums(GFDLPREC2005[,2:366])
#Add PlotCN
GFDLPREC2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
##covert to mm
GFDLPREC2005$PrecSum <- GFDLPREC2005$PrecSum*86400
rm(GFDL2005)
#2006
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2006  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2006.nc", subds="pr")
GFDL2006  <- terra::rotate(GFDL2006)
GFDLPREC2006<-data.frame(terra::extract(GFDL2006 ,allcoords[allcoords$Year == 2006,]))
GFDLPREC2006$PrecSum <- rowSums(GFDLPREC2006[,2:366])
#Add PlotCN
GFDLPREC2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
##covert to mm
GFDLPREC2006$PrecSum <- GFDLPREC2006$PrecSum*86400
rm(GFDL2006)
#2007
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2007  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2007.nc", subds="pr")
GFDL2007  <- terra::rotate(GFDL2007)
GFDLPREC2007<-data.frame(terra::extract(GFDL2007 ,allcoords[allcoords$Year == 2007,]))
GFDLPREC2007$PrecSum <- rowSums(GFDLPREC2007[,2:366])
#Add PlotCN
GFDLPREC2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
##covert to mm
GFDLPREC2007$PrecSum <- GFDLPREC2007$PrecSum*86400
rm(GFDL2007)
#2008
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2008  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2008.nc", subds="pr")
GFDL2008  <- terra::rotate(GFDL2008)
GFDLPREC2008<-data.frame(terra::extract(GFDL2008 ,allcoords[allcoords$Year == 2008,]))
GFDLPREC2008$PrecSum <- rowSums(GFDLPREC2008[,2:366])
#Add PlotCN
GFDLPREC2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
##covert to mm
GFDLPREC2008$PrecSum <- GFDLPREC2008$PrecSum*86400
rm(GFDL2008)
#2009
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2009  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2009.nc", subds="pr")
GFDL2009  <- terra::rotate(GFDL2009)
GFDLPREC2009<-data.frame(terra::extract(GFDL2009 ,allcoords[allcoords$Year == 2009,]))
GFDLPREC2009$PrecSum <- rowSums(GFDLPREC2009[,2:366])
#Add PlotCN
GFDLPREC2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
##covert to mm
GFDLPREC2009$PrecSum <- GFDLPREC2009$PrecSum*86400
rm(GFDL2009)
#2010
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2010  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2010.nc", subds="pr")
GFDL2010  <- terra::rotate(GFDL2010)
GFDLPREC2010<-data.frame(terra::extract(GFDL2010 ,allcoords[allcoords$Year == 2010,]))
GFDLPREC2010$PrecSum <- rowSums(GFDLPREC2010[,2:366])
#Add PlotCN
GFDLPREC2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
##covert to mm
GFDLPREC2010$PrecSum <- GFDLPREC2010$PrecSum*86400
rm(GFDL2010)
#2011
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2011  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2011.nc", subds="pr")
GFDL2011  <- terra::rotate(GFDL2011)
GFDLPREC2011<-data.frame(terra::extract(GFDL2011 ,allcoords[allcoords$Year == 2011,]))
GFDLPREC2011$PrecSum <- rowSums(GFDLPREC2011[,2:366])
#Add PlotCN
GFDLPREC2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
##covert to mm
GFDLPREC2011$PrecSum <- GFDLPREC2011$PrecSum*86400
rm(GFDL2011)
#2012
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2012  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2012.nc", subds="pr")
GFDL2012  <- terra::rotate(GFDL2012)
GFDLPREC2012<-data.frame(terra::extract(GFDL2012 ,allcoords[allcoords$Year == 2012,]))
GFDLPREC2012$PrecSum <- rowSums(GFDLPREC2012[,2:366])
#Add PlotCN
GFDLPREC2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
##covert to mm
GFDLPREC2012$PrecSum <- GFDLPREC2012$PrecSum*86400
rm(GFDL2012)
#2013
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2013  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2013.nc", subds="pr")
GFDL2013  <- terra::rotate(GFDL2013)
GFDLPREC2013<-data.frame(terra::extract(GFDL2013 ,allcoords[allcoords$Year == 2013,]))
GFDLPREC2013$PrecSum <- rowSums(GFDLPREC2013[,2:366])
#Add PlotCN
GFDLPREC2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
##covert to mm
GFDLPREC2013$PrecSum <- GFDLPREC2013$PrecSum*86400
rm(GFDL2013)
#2014
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2014  <- rast("~/Desktop/NASAClim/GFDLH/GFDLPr2014.nc", subds="pr")
GFDL2014  <- terra::rotate(GFDL2014)
GFDLPREC2014<-data.frame(terra::extract(GFDL2014 ,allcoords[allcoords$Year == 2014,]))
GFDLPREC2014$PrecSum <- rowSums(GFDLPREC2014[,2:366])
#Add PlotCN
GFDLPREC2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
##covert to mm
GFDLPREC2014$PrecSum <- GFDLPREC2014$PrecSum*86400
rm(GFDL2014)

GFDLPRECFull<- as.data.frame(rbind(GFDLPREC2000[,367:368],GFDLPREC2001[,367:368],GFDLPREC2002[,367:368],GFDLPREC2003[,367:368],GFDLPREC2004[,367:368],GFDLPREC2005[,367:368], GFDLPREC2006[,367:368],
                                   GFDLPREC2007[,367:368], GFDLPREC2008[,367:368], GFDLPREC2009[,367:368], GFDLPREC2010[,367:368], GFDLPREC2011[,367:368],
                                   GFDLPREC2012[,367:368], GFDLPREC2013[,367:368], GFDLPREC2014[,367:368]))
GFDLPRECFull <- GFDLPRECFull[,2:1]

rm(GFDLPREC2000,GFDLPREC2001,GFDLPREC2002,GFDLPREC2003,GFDLPREC2004,GFDLPREC2005, 
   GFDLPREC2006,GFDLPREC2007, GFDLPREC2008, GFDLPREC2009, 
   GFDLPREC2010, GFDLPREC2011, GFDLPREC2012, GFDLPREC2013, GFDLPREC2014)


#MIROC
#2000
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2000  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2000.nc", subds="pr")
MIROC2000  <- terra::rotate(MIROC2000)
MIROCPREC2000<-data.frame(terra::extract(MIROC2000 ,allcoords[allcoords$Year == 2000,]))
MIROCPREC2000$PrecSum <- rowSums(MIROCPREC2000[,2:367])
#Add PlotCN
MIROCPREC2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
##covert to mm
MIROCPREC2000$PrecSum <- MIROCPREC2000$PrecSum*86400
rm(MIROC2000)
#2001
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2001  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2001.nc", subds="pr")
MIROC2001  <- terra::rotate(MIROC2001)
MIROCPREC2001<-data.frame(terra::extract(MIROC2001 ,allcoords[allcoords$Year == 2001,]))
MIROCPREC2001$PrecSum <- rowSums(MIROCPREC2001[,2:366])
#Add PlotCN
MIROCPREC2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
##covert to mm
MIROCPREC2001$PrecSum <- MIROCPREC2001$PrecSum*86400
rm(MIROC2001)
#2002
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2002  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2002.nc", subds="pr")
MIROC2002  <- terra::rotate(MIROC2002)
MIROCPREC2002<-data.frame(terra::extract(MIROC2002 ,allcoords[allcoords$Year == 2002,]))
MIROCPREC2002$PrecSum <- rowSums(MIROCPREC2002[,2:366])
#Add PlotCN
MIROCPREC2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
##covert to mm
MIROCPREC2002$PrecSum <- MIROCPREC2002$PrecSum*86400
rm(MIROC2002)
#2003
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2003  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2003.nc", subds="pr")
MIROC2003  <- terra::rotate(MIROC2003)
MIROCPREC2003<-data.frame(terra::extract(MIROC2003 ,allcoords[allcoords$Year == 2003,]))
MIROCPREC2003$PrecSum <- rowSums(MIROCPREC2003[,2:366])
#Add PlotCN
MIROCPREC2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
##covert to mm
MIROCPREC2003$PrecSum <- MIROCPREC2003$PrecSum*86400
rm(MIROC2003)
#2004
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2004  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2004.nc", subds="pr")
MIROC2004  <- terra::rotate(MIROC2004)
MIROCPREC2004<-data.frame(terra::extract(MIROC2004 ,allcoords[allcoords$Year == 2004,]))
MIROCPREC2004$PrecSum <- rowSums(MIROCPREC2004[,2:367])
#Add PlotCN
MIROCPREC2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
##covert to mm
MIROCPREC2004$PrecSum <- MIROCPREC2004$PrecSum*86400
rm(MIROC2004)
#2005
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2005  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2005.nc", subds="pr")
MIROC2005  <- terra::rotate(MIROC2005)
MIROCPREC2005<-data.frame(terra::extract(MIROC2005 ,allcoords[allcoords$Year == 2005,]))
MIROCPREC2005$PrecSum <- rowSums(MIROCPREC2005[,2:366])
#Add PlotCN
MIROCPREC2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
##covert to mm
MIROCPREC2005$PrecSum <- MIROCPREC2005$PrecSum*86400
rm(MIROC2005)
#2006
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2006  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2006.nc", subds="pr")
MIROC2006  <- terra::rotate(MIROC2006)
MIROCPREC2006<-data.frame(terra::extract(MIROC2006 ,allcoords[allcoords$Year == 2006,]))
MIROCPREC2006$PrecSum <- rowSums(MIROCPREC2006[,2:366])
#Add PlotCN
MIROCPREC2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
##covert to mm
MIROCPREC2006$PrecSum <- MIROCPREC2006$PrecSum*86400
rm(MIROC2006)
#2007
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2007  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2007.nc", subds="pr")
MIROC2007  <- terra::rotate(MIROC2007)
MIROCPREC2007<-data.frame(terra::extract(MIROC2007 ,allcoords[allcoords$Year == 2007,]))
MIROCPREC2007$PrecSum <- rowSums(MIROCPREC2007[,2:366])
#Add PlotCN
MIROCPREC2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
##covert to mm
MIROCPREC2007$PrecSum <- MIROCPREC2007$PrecSum*86400
rm(MIROC2007)
#2008
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2008  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2008.nc", subds="pr")
MIROC2008  <- terra::rotate(MIROC2008)
MIROCPREC2008<-data.frame(terra::extract(MIROC2008 ,allcoords[allcoords$Year == 2008,]))
MIROCPREC2008$PrecSum <- rowSums(MIROCPREC2008[,2:367])
#Add PlotCN
MIROCPREC2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
##covert to mm
MIROCPREC2008$PrecSum <- MIROCPREC2008$PrecSum*86400
rm(MIROC2008)
#2009
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2009  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2009.nc", subds="pr")
MIROC2009  <- terra::rotate(MIROC2009)
MIROCPREC2009<-data.frame(terra::extract(MIROC2009 ,allcoords[allcoords$Year == 2009,]))
MIROCPREC2009$PrecSum <- rowSums(MIROCPREC2009[,2:366])
#Add PlotCN
MIROCPREC2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
##covert to mm
MIROCPREC2009$PrecSum <- MIROCPREC2009$PrecSum*86400
rm(MIROC2009)
#2010
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2010  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2010.nc", subds="pr")
MIROC2010  <- terra::rotate(MIROC2010)
MIROCPREC2010<-data.frame(terra::extract(MIROC2010 ,allcoords[allcoords$Year == 2010,]))
MIROCPREC2010$PrecSum <- rowSums(MIROCPREC2010[,2:366])
#Add PlotCN
MIROCPREC2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
##covert to mm
MIROCPREC2010$PrecSum <- MIROCPREC2010$PrecSum*86400
rm(MIROC2010)
#2011
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2011  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2011.nc", subds="pr")
MIROC2011  <- terra::rotate(MIROC2011)
MIROCPREC2011<-data.frame(terra::extract(MIROC2011 ,allcoords[allcoords$Year == 2011,]))
MIROCPREC2011$PrecSum <- rowSums(MIROCPREC2011[,2:366])
#Add PlotCN
MIROCPREC2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
##covert to mm
MIROCPREC2011$PrecSum <- MIROCPREC2011$PrecSum*86400
rm(MIROC2011)
#2012
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2012  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2012.nc", subds="pr")
MIROC2012  <- terra::rotate(MIROC2012)
MIROCPREC2012<-data.frame(terra::extract(MIROC2012 ,allcoords[allcoords$Year == 2012,]))
MIROCPREC2012$PrecSum <- rowSums(MIROCPREC2012[,2:367])
#Add PlotCN
MIROCPREC2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
##covert to mm
MIROCPREC2012$PrecSum <- MIROCPREC2012$PrecSum*86400
rm(MIROC2012)
#2013
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2013  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2013.nc", subds="pr")
MIROC2013  <- terra::rotate(MIROC2013)
MIROCPREC2013<-data.frame(terra::extract(MIROC2013 ,allcoords[allcoords$Year == 2013,]))
MIROCPREC2013$PrecSum <- rowSums(MIROCPREC2013[,2:366])
#Add PlotCN
MIROCPREC2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
##covert to mm
MIROCPREC2013$PrecSum <- MIROCPREC2013$PrecSum*86400
rm(MIROC2013)
#2014
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2014  <- rast("~/Desktop/NASAClim/MIROCH/MIROCPr2014.nc", subds="pr")
MIROC2014  <- terra::rotate(MIROC2014)
MIROCPREC2014<-data.frame(terra::extract(MIROC2014 ,allcoords[allcoords$Year == 2014,]))
MIROCPREC2014$PrecSum <- rowSums(MIROCPREC2014[,2:366])
#Add PlotCN
MIROCPREC2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
##covert to mm
MIROCPREC2014$PrecSum <- MIROCPREC2014$PrecSum*86400
rm(MIROC2014)

MIROCPRECFull<- as.data.frame(rbind(MIROCPREC2000[,368:369],MIROCPREC2001[,367:368],MIROCPREC2002[,367:368],MIROCPREC2003[,367:368],MIROCPREC2004[,368:369],MIROCPREC2005[,367:368], MIROCPREC2006[,367:368],
                                    MIROCPREC2007[,367:368], MIROCPREC2008[,368:369], MIROCPREC2009[,367:368], MIROCPREC2010[,367:368], MIROCPREC2011[,367:368],
                                    MIROCPREC2012[,368:369], MIROCPREC2013[,367:368], MIROCPREC2014[,367:368]))
MIROCPRECFull <- MIROCPRECFull[,2:1]

rm(MIROCPREC2000,MIROCPREC2001,MIROCPREC2002,MIROCPREC2003,MIROCPREC2004,MIROCPREC2005, 
   MIROCPREC2006,MIROCPREC2007, MIROCPREC2008, MIROCPREC2009, 
   MIROCPREC2010, MIROCPREC2011, MIROCPREC2012, MIROCPREC2013, MIROCPREC2014)

#NORESM
#2000
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2000  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2000.nc", subds="pr")
NORESM2000  <- terra::rotate(NORESM2000)
NORESMPREC2000<-data.frame(terra::extract(NORESM2000 ,allcoords[allcoords$Year == 2000,]))
NORESMPREC2000$PrecSum <- rowSums(NORESMPREC2000[,2:366])
#Add PlotCN
NORESMPREC2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
##covert to mm
NORESMPREC2000$PrecSum <- NORESMPREC2000$PrecSum*86400
rm(NORESM2000)
#2001
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2001  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2001.nc", subds="pr")
NORESM2001  <- terra::rotate(NORESM2001)
NORESMPREC2001<-data.frame(terra::extract(NORESM2001 ,allcoords[allcoords$Year == 2001,]))
NORESMPREC2001$PrecSum <- rowSums(NORESMPREC2001[,2:366])
#Add PlotCN
NORESMPREC2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
##covert to mm
NORESMPREC2001$PrecSum <- NORESMPREC2001$PrecSum*86400
rm(NORESM2001)
#2002
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2002  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2002.nc", subds="pr")
NORESM2002  <- terra::rotate(NORESM2002)
NORESMPREC2002<-data.frame(terra::extract(NORESM2002 ,allcoords[allcoords$Year == 2002,]))
NORESMPREC2002$PrecSum <- rowSums(NORESMPREC2002[,2:366])
#Add PlotCN
NORESMPREC2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
##covert to mm
NORESMPREC2002$PrecSum <- NORESMPREC2002$PrecSum*86400
rm(NORESM2002)
#2003
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2003  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2003.nc", subds="pr")
NORESM2003  <- terra::rotate(NORESM2003)
NORESMPREC2003<-data.frame(terra::extract(NORESM2003 ,allcoords[allcoords$Year == 2003,]))
NORESMPREC2003$PrecSum <- rowSums(NORESMPREC2003[,2:366])
#Add PlotCN
NORESMPREC2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
##covert to mm
NORESMPREC2003$PrecSum <- NORESMPREC2003$PrecSum*86400
rm(NORESM2003)
#2004
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2004  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2004.nc", subds="pr")
NORESM2004  <- terra::rotate(NORESM2004)
NORESMPREC2004<-data.frame(terra::extract(NORESM2004 ,allcoords[allcoords$Year == 2004,]))
NORESMPREC2004$PrecSum <- rowSums(NORESMPREC2004[,2:366])
#Add PlotCN
NORESMPREC2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
##covert to mm
NORESMPREC2004$PrecSum <- NORESMPREC2004$PrecSum*86400
rm(NORESM2004)
#2005
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2005  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2005.nc", subds="pr")
NORESM2005  <- terra::rotate(NORESM2005)
NORESMPREC2005<-data.frame(terra::extract(NORESM2005 ,allcoords[allcoords$Year == 2005,]))
NORESMPREC2005$PrecSum <- rowSums(NORESMPREC2005[,2:366])
#Add PlotCN
NORESMPREC2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
##covert to mm
NORESMPREC2005$PrecSum <- NORESMPREC2005$PrecSum*86400
rm(NORESM2005)
#2006
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2006  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2006.nc", subds="pr")
NORESM2006  <- terra::rotate(NORESM2006)
NORESMPREC2006<-data.frame(terra::extract(NORESM2006 ,allcoords[allcoords$Year == 2006,]))
NORESMPREC2006$PrecSum <- rowSums(NORESMPREC2006[,2:366])
#Add PlotCN
NORESMPREC2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
##covert to mm
NORESMPREC2006$PrecSum <- NORESMPREC2006$PrecSum*86400
rm(NORESM2006)
#2007
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2007  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2007.nc", subds="pr")
NORESM2007  <- terra::rotate(NORESM2007)
NORESMPREC2007<-data.frame(terra::extract(NORESM2007 ,allcoords[allcoords$Year == 2007,]))
NORESMPREC2007$PrecSum <- rowSums(NORESMPREC2007[,2:366])
#Add PlotCN
NORESMPREC2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
##covert to mm
NORESMPREC2007$PrecSum <- NORESMPREC2007$PrecSum*86400
rm(NORESM2007)
#2008
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2008  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2008.nc", subds="pr")
NORESM2008  <- terra::rotate(NORESM2008)
NORESMPREC2008<-data.frame(terra::extract(NORESM2008 ,allcoords[allcoords$Year == 2008,]))
NORESMPREC2008$PrecSum <- rowSums(NORESMPREC2008[,2:366])
#Add PlotCN
NORESMPREC2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
##covert to mm
NORESMPREC2008$PrecSum <- NORESMPREC2008$PrecSum*86400
rm(NORESM2008)
#2009
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2009  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2009.nc", subds="pr")
NORESM2009  <- terra::rotate(NORESM2009)
NORESMPREC2009<-data.frame(terra::extract(NORESM2009 ,allcoords[allcoords$Year == 2009,]))
NORESMPREC2009$PrecSum <- rowSums(NORESMPREC2009[,2:366])
#Add PlotCN
NORESMPREC2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
##covert to mm
NORESMPREC2009$PrecSum <- NORESMPREC2009$PrecSum*86400
rm(NORESM2009)
#2010
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2010  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2010.nc", subds="pr")
NORESM2010  <- terra::rotate(NORESM2010)
NORESMPREC2010<-data.frame(terra::extract(NORESM2010 ,allcoords[allcoords$Year == 2010,]))
NORESMPREC2010$PrecSum <- rowSums(NORESMPREC2010[,2:366])
#Add PlotCN
NORESMPREC2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
##covert to mm
NORESMPREC2010$PrecSum <- NORESMPREC2010$PrecSum*86400
rm(NORESM2010)
#2011
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2011  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2011.nc", subds="pr")
NORESM2011  <- terra::rotate(NORESM2011)
NORESMPREC2011<-data.frame(terra::extract(NORESM2011 ,allcoords[allcoords$Year == 2011,]))
NORESMPREC2011$PrecSum <- rowSums(NORESMPREC2011[,2:366])
#Add PlotCN
NORESMPREC2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
##covert to mm
NORESMPREC2011$PrecSum <- NORESMPREC2011$PrecSum*86400
rm(NORESM2011)
#2012
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2012  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2012.nc", subds="pr")
NORESM2012  <- terra::rotate(NORESM2012)
NORESMPREC2012<-data.frame(terra::extract(NORESM2012 ,allcoords[allcoords$Year == 2012,]))
NORESMPREC2012$PrecSum <- rowSums(NORESMPREC2012[,2:366])
#Add PlotCN
NORESMPREC2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
##covert to mm
NORESMPREC2012$PrecSum <- NORESMPREC2012$PrecSum*86400
rm(NORESM2012)
#2013
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2013  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2013.nc", subds="pr")
NORESM2013  <- terra::rotate(NORESM2013)
NORESMPREC2013<-data.frame(terra::extract(NORESM2013 ,allcoords[allcoords$Year == 2013,]))
NORESMPREC2013$PrecSum <- rowSums(NORESMPREC2013[,2:366])
#Add PlotCN
NORESMPREC2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
##covert to mm
NORESMPREC2013$PrecSum <- NORESMPREC2013$PrecSum*86400
rm(NORESM2013)
#2014
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2014  <- rast("~/Desktop/NASAClim/NORESMH/NORESMPr2014.nc", subds="pr")
NORESM2014  <- terra::rotate(NORESM2014)
NORESMPREC2014<-data.frame(terra::extract(NORESM2014 ,allcoords[allcoords$Year == 2014,]))
NORESMPREC2014$PrecSum <- rowSums(NORESMPREC2014[,2:366])
#Add PlotCN
NORESMPREC2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
##covert to mm
NORESMPREC2014$PrecSum <- NORESMPREC2014$PrecSum*86400
rm(NORESM2014)

NORESMPRECFull<- as.data.frame(rbind(NORESMPREC2000[,367:368],NORESMPREC2001[,367:368],NORESMPREC2002[,367:368],NORESMPREC2003[,367:368],NORESMPREC2004[,367:368],NORESMPREC2005[,367:368], NORESMPREC2006[,367:368],
                                     NORESMPREC2007[,367:368], NORESMPREC2008[,367:368], NORESMPREC2009[,367:368], NORESMPREC2010[,367:368], NORESMPREC2011[,367:368],
                                     NORESMPREC2012[,367:368], NORESMPREC2013[,367:368], NORESMPREC2014[,367:368]))
NORESMPRECFull <- NORESMPRECFull[,2:1]

rm(NORESMPREC2000,NORESMPREC2001,NORESMPREC2002,NORESMPREC2003,NORESMPREC2004,NORESMPREC2005, 
   NORESMPREC2006,NORESMPREC2007, NORESMPREC2008, NORESMPREC2009, 
   NORESMPREC2010, NORESMPREC2011, NORESMPREC2012, NORESMPREC2013, NORESMPREC2014)


#relative humidity
#GFDL
#2000
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2000  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2000.nc", subds="hurs")
GFDL2000  <- terra::rotate(GFDL2000)
GFDLHUM2000<-data.frame(terra::extract(GFDL2000 ,allcoords[allcoords$Year == 2000,]))
GFDLHUM2000$HUMAvg <- rowMeans(GFDLHUM2000[,2:366])
#Add PlotCN
GFDLHUM2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
rm(GFDL2000)
#2001
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2001  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2001.nc", subds="hurs")
GFDL2001  <- terra::rotate(GFDL2001)
GFDLHUM2001<-data.frame(terra::extract(GFDL2001 ,allcoords[allcoords$Year == 2001,]))
GFDLHUM2001$HUMAvg <- rowMeans(GFDLHUM2001[,2:366])
#Add PlotCN
GFDLHUM2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
rm(GFDL2001)
#2002
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2002  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2002.nc", subds="hurs")
GFDL2002  <- terra::rotate(GFDL2002)
GFDLHUM2002<-data.frame(terra::extract(GFDL2002 ,allcoords[allcoords$Year == 2002,]))
GFDLHUM2002$HUMAvg <- rowMeans(GFDLHUM2002[,2:366])
#Add PlotCN
GFDLHUM2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
rm(GFDL2002)
#2003
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2003  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2003.nc", subds="hurs")
GFDL2003  <- terra::rotate(GFDL2003)
GFDLHUM2003<-data.frame(terra::extract(GFDL2003 ,allcoords[allcoords$Year == 2003,]))
GFDLHUM2003$HUMAvg <- rowMeans(GFDLHUM2003[,2:366])
#Add PlotCN
GFDLHUM2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
rm(GFDL2003)
#2004
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2004  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2004.nc", subds="hurs")
GFDL2004  <- terra::rotate(GFDL2004)
GFDLHUM2004<-data.frame(terra::extract(GFDL2004 ,allcoords[allcoords$Year == 2004,]))
GFDLHUM2004$HUMAvg <- rowMeans(GFDLHUM2004[,2:366])
#Add PlotCN
GFDLHUM2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
rm(GFDL2004)
#2005
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2005  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2005.nc", subds="hurs")
GFDL2005  <- terra::rotate(GFDL2005)
#Repeat for 
GFDLHUM2005<-data.frame(terra::extract(GFDL2005 ,allcoords[allcoords$Year == 2005,]))
GFDLHUM2005$HUMAvg <- rowMeans(GFDLHUM2005[,2:366])
#Add PlotCN
GFDLHUM2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
rm(GFDL2005)
#2006
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2006  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2006.nc", subds="hurs")
GFDL2006  <- terra::rotate(GFDL2006)
GFDLHUM2006<-data.frame(terra::extract(GFDL2006 ,allcoords[allcoords$Year == 2006,]))
GFDLHUM2006$HUMAvg <- rowMeans(GFDLHUM2006[,2:366])
#Add PlotCN
GFDLHUM2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
rm(GFDL2006)
#2007
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2007  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2007.nc", subds="hurs")
GFDL2007  <- terra::rotate(GFDL2007)
GFDLHUM2007<-data.frame(terra::extract(GFDL2007 ,allcoords[allcoords$Year == 2007,]))
GFDLHUM2007$HUMAvg <- rowMeans(GFDLHUM2007[,2:366])
#Add PlotCN
GFDLHUM2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
rm(GFDL2007)
#2008
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2008  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2008.nc", subds="hurs")
GFDL2008  <- terra::rotate(GFDL2008)
GFDLHUM2008<-data.frame(terra::extract(GFDL2008 ,allcoords[allcoords$Year == 2008,]))
GFDLHUM2008$HUMAvg <- rowMeans(GFDLHUM2008[,2:366])
#Add PlotCN
GFDLHUM2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
rm(GFDL2008)
#2009
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2009  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2009.nc", subds="hurs")
GFDL2009  <- terra::rotate(GFDL2009)
GFDLHUM2009<-data.frame(terra::extract(GFDL2009 ,allcoords[allcoords$Year == 2009,]))
GFDLHUM2009$HUMAvg <- rowMeans(GFDLHUM2009[,2:366])
#Add PlotCN
GFDLHUM2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
rm(GFDL2009)
#2010
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2010  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2010.nc", subds="hurs")
GFDL2010  <- terra::rotate(GFDL2010)
GFDLHUM2010<-data.frame(terra::extract(GFDL2010 ,allcoords[allcoords$Year == 2010,]))
GFDLHUM2010$HUMAvg <- rowMeans(GFDLHUM2010[,2:366])
#Add PlotCN
GFDLHUM2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
rm(GFDL2010)
#2011
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2011  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2011.nc", subds="hurs")
GFDL2011  <- terra::rotate(GFDL2011)
#Repeat for 
GFDLHUM2011<-data.frame(terra::extract(GFDL2011 ,allcoords[allcoords$Year == 2011,]))
GFDLHUM2011$HUMAvg <- rowMeans(GFDLHUM2011[,2:366])
#Add PlotCN
GFDLHUM2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
rm(GFDL2011)
#2012
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2012  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2012.nc", subds="hurs")
GFDL2012  <- terra::rotate(GFDL2012)
GFDLHUM2012<-data.frame(terra::extract(GFDL2012 ,allcoords[allcoords$Year == 2012,]))
GFDLHUM2012$HUMAvg <- rowMeans(GFDLHUM2012[,2:366])
#Add PlotCN
GFDLHUM2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
rm(GFDL2012)
#2013
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2013  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2013.nc", subds="hurs")
GFDL2013  <- terra::rotate(GFDL2013)
GFDLHUM2013<-data.frame(terra::extract(GFDL2013 ,allcoords[allcoords$Year == 2013,]))
GFDLHUM2013$HUMAvg <- rowMeans(GFDLHUM2013[,2:366])
#Add PlotCN
GFDLHUM2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
rm(GFDL2013)
#2014
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2014  <- rast("~/Desktop/NASAClim/GFDLH/GFDLHurs2014.nc", subds="hurs")
GFDL2014  <- terra::rotate(GFDL2014)
GFDLHUM2014<-data.frame(terra::extract(GFDL2014 ,allcoords[allcoords$Year == 2014,]))
GFDLHUM2014$HUMAvg <- rowMeans(GFDLHUM2014[,2:366])
#Add PlotCN
GFDLHUM2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
rm(GFDL2014)

GFDLHUMFull<- as.data.frame(rbind(GFDLHUM2000[,367:368],GFDLHUM2001[,367:368],GFDLHUM2002[,367:368],GFDLHUM2003[,367:368],GFDLHUM2004[,367:368],GFDLHUM2005[,367:368], GFDLHUM2006[,367:368],
                                  GFDLHUM2007[,367:368], GFDLHUM2008[,367:368], GFDLHUM2009[,367:368], GFDLHUM2010[,367:368], GFDLHUM2011[,367:368],
                                  GFDLHUM2012[,367:368], GFDLHUM2013[,367:368], GFDLHUM2014[,367:368]))
GFDLHUMFull <- GFDLHUMFull[,2:1]

rm(GFDLHUM2000,GFDLHUM2001,GFDLHUM2002,GFDLHUM2003,GFDLHUM2004,GFDLHUM2005, 
   GFDLHUM2006,GFDLHUM2007, GFDLHUM2008, GFDLHUM2009, 
   GFDLHUM2010, GFDLHUM2011, GFDLHUM2012, GFDLHUM2013, GFDLHUM2014)


#MIROC
#2000
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2000  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2000.nc", subds="hurs")
MIROC2000  <- terra::rotate(MIROC2000)
MIROCHUM2000<-data.frame(terra::extract(MIROC2000 ,allcoords[allcoords$Year == 2000,]))
MIROCHUM2000$HUMAvg <- rowMeans(MIROCHUM2000[,2:367])
#Add PlotCN
MIROCHUM2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
rm(MIROC2000)
#2001
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2001  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2001.nc", subds="hurs")
MIROC2001  <- terra::rotate(MIROC2001)
MIROCHUM2001<-data.frame(terra::extract(MIROC2001 ,allcoords[allcoords$Year == 2001,]))
MIROCHUM2001$HUMAvg <- rowMeans(MIROCHUM2001[,2:366])
#Add PlotCN
MIROCHUM2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
rm(MIROC2001)
#2002
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2002  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2002.nc", subds="hurs")
MIROC2002  <- terra::rotate(MIROC2002)
MIROCHUM2002<-data.frame(terra::extract(MIROC2002 ,allcoords[allcoords$Year == 2002,]))
MIROCHUM2002$HUMAvg <- rowMeans(MIROCHUM2002[,2:366])
#Add PlotCN
MIROCHUM2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
rm(MIROC2002)
#2003
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2003  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2003.nc", subds="hurs")
MIROC2003  <- terra::rotate(MIROC2003)
MIROCHUM2003<-data.frame(terra::extract(MIROC2003 ,allcoords[allcoords$Year == 2003,]))
MIROCHUM2003$HUMAvg <- rowMeans(MIROCHUM2003[,2:366])
#Add PlotCN
MIROCHUM2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
rm(MIROC2003)
#2004
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2004  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2004.nc", subds="hurs")
MIROC2004  <- terra::rotate(MIROC2004)
MIROCHUM2004<-data.frame(terra::extract(MIROC2004 ,allcoords[allcoords$Year == 2004,]))
MIROCHUM2004$HUMAvg <- rowMeans(MIROCHUM2004[,2:367])
#Add PlotCN
MIROCHUM2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
rm(MIROC2004)
#2005
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2005  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2005.nc", subds="hurs")
MIROC2005  <- terra::rotate(MIROC2005)
#Repeat for 
MIROCHUM2005<-data.frame(terra::extract(MIROC2005 ,allcoords[allcoords$Year == 2005,]))
MIROCHUM2005$HUMAvg <- rowMeans(MIROCHUM2005[,2:366])
#Add PlotCN
MIROCHUM2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
rm(MIROC2005)
#2006
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2006  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2006.nc", subds="hurs")
MIROC2006  <- terra::rotate(MIROC2006)
MIROCHUM2006<-data.frame(terra::extract(MIROC2006 ,allcoords[allcoords$Year == 2006,]))
MIROCHUM2006$HUMAvg <- rowMeans(MIROCHUM2006[,2:366])
#Add PlotCN
MIROCHUM2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
rm(MIROC2006)
#2007
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2007  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2007.nc", subds="hurs")
MIROC2007  <- terra::rotate(MIROC2007)
MIROCHUM2007<-data.frame(terra::extract(MIROC2007 ,allcoords[allcoords$Year == 2007,]))
MIROCHUM2007$HUMAvg <- rowMeans(MIROCHUM2007[,2:366])
#Add PlotCN
MIROCHUM2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
rm(MIROC2007)
#2008
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2008  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2008.nc", subds="hurs")
MIROC2008  <- terra::rotate(MIROC2008)
MIROCHUM2008<-data.frame(terra::extract(MIROC2008 ,allcoords[allcoords$Year == 2008,]))
MIROCHUM2008$HUMAvg <- rowMeans(MIROCHUM2008[,2:367])
#Add PlotCN
MIROCHUM2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
rm(MIROC2008)
#2009
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2009  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2009.nc", subds="hurs")
MIROC2009  <- terra::rotate(MIROC2009)
MIROCHUM2009<-data.frame(terra::extract(MIROC2009 ,allcoords[allcoords$Year == 2009,]))
MIROCHUM2009$HUMAvg <- rowMeans(MIROCHUM2009[,2:366])
#Add PlotCN
MIROCHUM2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
rm(MIROC2009)
#2010
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2010  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2010.nc", subds="hurs")
MIROC2010  <- terra::rotate(MIROC2010)
MIROCHUM2010<-data.frame(terra::extract(MIROC2010 ,allcoords[allcoords$Year == 2010,]))
MIROCHUM2010$HUMAvg <- rowMeans(MIROCHUM2010[,2:366])
#Add PlotCN
MIROCHUM2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
rm(MIROC2010)
#2011
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2011  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2011.nc", subds="hurs")
MIROC2011  <- terra::rotate(MIROC2011)
#Repeat for 
MIROCHUM2011<-data.frame(terra::extract(MIROC2011 ,allcoords[allcoords$Year == 2011,]))
MIROCHUM2011$HUMAvg <- rowMeans(MIROCHUM2011[,2:366])
#Add PlotCN
MIROCHUM2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
rm(MIROC2011)
#2012
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2012  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2012.nc", subds="hurs")
MIROC2012  <- terra::rotate(MIROC2012)
MIROCHUM2012<-data.frame(terra::extract(MIROC2012 ,allcoords[allcoords$Year == 2012,]))
MIROCHUM2012$HUMAvg <- rowMeans(MIROCHUM2012[,2:367])
#Add PlotCN
MIROCHUM2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
rm(MIROC2012)
#2013
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2013  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2013.nc", subds="hurs")
MIROC2013  <- terra::rotate(MIROC2013)
MIROCHUM2013<-data.frame(terra::extract(MIROC2013 ,allcoords[allcoords$Year == 2013,]))
MIROCHUM2013$HUMAvg <- rowMeans(MIROCHUM2013[,2:366])
#Add PlotCN
MIROCHUM2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
rm(MIROC2013)
#2014
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2014  <- rast("~/Desktop/NASAClim/MIROCH/MIROCHurs2014.nc", subds="hurs")
MIROC2014  <- terra::rotate(MIROC2014)
MIROCHUM2014<-data.frame(terra::extract(MIROC2014 ,allcoords[allcoords$Year == 2014,]))
MIROCHUM2014$HUMAvg <- rowMeans(MIROCHUM2014[,2:366])
#Add PlotCN
MIROCHUM2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
rm(MIROC2014)

MIROCHUMFull<- as.data.frame(rbind(MIROCHUM2000[,368:369],MIROCHUM2001[,367:368],MIROCHUM2002[,367:368],MIROCHUM2003[,367:368],MIROCHUM2004[,368:369],MIROCHUM2005[,367:368], MIROCHUM2006[,367:368],
                                   MIROCHUM2007[,367:368], MIROCHUM2008[,368:369], MIROCHUM2009[,367:368], MIROCHUM2010[,367:368], MIROCHUM2011[,367:368],
                                   MIROCHUM2012[,368:369], MIROCHUM2013[,367:368], MIROCHUM2014[,367:368]))
MIROCHUMFull <- MIROCHUMFull[,2:1]

rm(MIROCHUM2000,MIROCHUM2001,MIROCHUM2002,MIROCHUM2003,MIROCHUM2004,MIROCHUM2005, 
   MIROCHUM2006,MIROCHUM2007, MIROCHUM2008, MIROCHUM2009, 
   MIROCHUM2010, MIROCHUM2011, MIROCHUM2012, MIROCHUM2013, MIROCHUM2014)


#NORESM
#2000
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2000  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2000.nc", subds="hurs")
NORESM2000  <- terra::rotate(NORESM2000)
NORESMHUM2000<-data.frame(terra::extract(NORESM2000 ,allcoords[allcoords$Year == 2000,]))
NORESMHUM2000$HUMAvg <- rowMeans(NORESMHUM2000[,2:366])
#Add PlotCN
NORESMHUM2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
rm(NORESM2000)
#2001
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2001  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2001.nc", subds="hurs")
NORESM2001  <- terra::rotate(NORESM2001)
NORESMHUM2001<-data.frame(terra::extract(NORESM2001 ,allcoords[allcoords$Year == 2001,]))
NORESMHUM2001$HUMAvg <- rowMeans(NORESMHUM2001[,2:366])
#Add PlotCN
NORESMHUM2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
rm(NORESM2001)
#2002
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2002  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2002.nc", subds="hurs")
NORESM2002  <- terra::rotate(NORESM2002)
NORESMHUM2002<-data.frame(terra::extract(NORESM2002 ,allcoords[allcoords$Year == 2002,]))
NORESMHUM2002$HUMAvg <- rowMeans(NORESMHUM2002[,2:366])
#Add PlotCN
NORESMHUM2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
rm(NORESM2002)
#2003
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2003  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2003.nc", subds="hurs")
NORESM2003  <- terra::rotate(NORESM2003)
NORESMHUM2003<-data.frame(terra::extract(NORESM2003 ,allcoords[allcoords$Year == 2003,]))
NORESMHUM2003$HUMAvg <- rowMeans(NORESMHUM2003[,2:366])
#Add PlotCN
NORESMHUM2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
rm(NORESM2003)
#2004
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2004  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2004.nc", subds="hurs")
NORESM2004  <- terra::rotate(NORESM2004)
NORESMHUM2004<-data.frame(terra::extract(NORESM2004 ,allcoords[allcoords$Year == 2004,]))
NORESMHUM2004$HUMAvg <- rowMeans(NORESMHUM2004[,2:366])
#Add PlotCN
NORESMHUM2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
rm(NORESM2004)
#2005
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2005  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2005.nc", subds="hurs")
NORESM2005  <- terra::rotate(NORESM2005)
#Repeat for 
NORESMHUM2005<-data.frame(terra::extract(NORESM2005 ,allcoords[allcoords$Year == 2005,]))
NORESMHUM2005$HUMAvg <- rowMeans(NORESMHUM2005[,2:366])
#Add PlotCN
NORESMHUM2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
rm(NORESM2005)
#2006
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2006  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2006.nc", subds="hurs")
NORESM2006  <- terra::rotate(NORESM2006)
NORESMHUM2006<-data.frame(terra::extract(NORESM2006 ,allcoords[allcoords$Year == 2006,]))
NORESMHUM2006$HUMAvg <- rowMeans(NORESMHUM2006[,2:366])
#Add PlotCN
NORESMHUM2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
rm(NORESM2006)
#2007
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2007  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2007.nc", subds="hurs")
NORESM2007  <- terra::rotate(NORESM2007)
NORESMHUM2007<-data.frame(terra::extract(NORESM2007 ,allcoords[allcoords$Year == 2007,]))
NORESMHUM2007$HUMAvg <- rowMeans(NORESMHUM2007[,2:366])
#Add PlotCN
NORESMHUM2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
rm(NORESM2007)
#2008
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2008  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2008.nc", subds="hurs")
NORESM2008  <- terra::rotate(NORESM2008)
NORESMHUM2008<-data.frame(terra::extract(NORESM2008 ,allcoords[allcoords$Year == 2008,]))
NORESMHUM2008$HUMAvg <- rowMeans(NORESMHUM2008[,2:366])
#Add PlotCN
NORESMHUM2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
rm(NORESM2008)
#2009
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2009  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2009.nc", subds="hurs")
NORESM2009  <- terra::rotate(NORESM2009)
NORESMHUM2009<-data.frame(terra::extract(NORESM2009 ,allcoords[allcoords$Year == 2009,]))
NORESMHUM2009$HUMAvg <- rowMeans(NORESMHUM2009[,2:366])
#Add PlotCN
NORESMHUM2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
rm(NORESM2009)
#2010
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2010  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2010.nc", subds="hurs")
NORESM2010  <- terra::rotate(NORESM2010)
NORESMHUM2010<-data.frame(terra::extract(NORESM2010 ,allcoords[allcoords$Year == 2010,]))
NORESMHUM2010$HUMAvg <- rowMeans(NORESMHUM2010[,2:366])
#Add PlotCN
NORESMHUM2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
rm(NORESM2010)
#2011
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2011  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2011.nc", subds="hurs")
NORESM2011  <- terra::rotate(NORESM2011)
#Repeat for 
NORESMHUM2011<-data.frame(terra::extract(NORESM2011 ,allcoords[allcoords$Year == 2011,]))
NORESMHUM2011$HUMAvg <- rowMeans(NORESMHUM2011[,2:366])
#Add PlotCN
NORESMHUM2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
rm(NORESM2011)
#2012
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2012  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2012.nc", subds="hurs")
NORESM2012  <- terra::rotate(NORESM2012)
NORESMHUM2012<-data.frame(terra::extract(NORESM2012 ,allcoords[allcoords$Year == 2012,]))
NORESMHUM2012$HUMAvg <- rowMeans(NORESMHUM2012[,2:366])
#Add PlotCN
NORESMHUM2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
rm(NORESM2012)
#2013
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2013  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2013.nc", subds="hurs")
NORESM2013  <- terra::rotate(NORESM2013)
NORESMHUM2013<-data.frame(terra::extract(NORESM2013 ,allcoords[allcoords$Year == 2013,]))
NORESMHUM2013$HUMAvg <- rowMeans(NORESMHUM2013[,2:366])
#Add PlotCN
NORESMHUM2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
rm(NORESM2013)
#2014
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2014  <- rast("~/Desktop/NASAClim/NORESMH/NORESMHurs2014.nc", subds="hurs")
NORESM2014  <- terra::rotate(NORESM2014)
NORESMHUM2014<-data.frame(terra::extract(NORESM2014 ,allcoords[allcoords$Year == 2014,]))
NORESMHUM2014$HUMAvg <- rowMeans(NORESMHUM2014[,2:366])
#Add PlotCN
NORESMHUM2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
rm(NORESM2014)

NORESMHUMFull<- as.data.frame(rbind(NORESMHUM2000[,367:368],NORESMHUM2001[,367:368],NORESMHUM2002[,367:368],NORESMHUM2003[,367:368],NORESMHUM2004[,367:368],NORESMHUM2005[,367:368], NORESMHUM2006[,367:368],
                                    NORESMHUM2007[,367:368], NORESMHUM2008[,367:368], NORESMHUM2009[,367:368], NORESMHUM2010[,367:368], NORESMHUM2011[,367:368],
                                    NORESMHUM2012[,367:368], NORESMHUM2013[,367:368], NORESMHUM2014[,367:368]))
NORESMHUMFull <- NORESMHUMFull[,2:1]

rm(NORESMHUM2000,NORESMHUM2001,NORESMHUM2002,NORESMHUM2003,NORESMHUM2004,NORESMHUM2005, 
   NORESMHUM2006,NORESMHUM2007, NORESMHUM2008, NORESMHUM2009, 
   NORESMHUM2010, NORESMHUM2011, NORESMHUM2012, NORESMHUM2013, NORESMHUM2014)


#radiation
#GFDL
#2000
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2000  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2000.nc", subds="rsds")
GFDL2000  <- terra::rotate(GFDL2000)
GFDLRAD2000<-data.frame(terra::extract(GFDL2000 ,allcoords[allcoords$Year == 2000,]))
GFDLRAD2000$RADAvg <- rowMeans(GFDLRAD2000[,2:366])
#Add PlotCN
GFDLRAD2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
rm(GFDL2000)
#2001
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2001  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2001.nc", subds="rsds")
GFDL2001  <- terra::rotate(GFDL2001)
GFDLRAD2001<-data.frame(terra::extract(GFDL2001 ,allcoords[allcoords$Year == 2001,]))
GFDLRAD2001$RADAvg <- rowMeans(GFDLRAD2001[,2:366])
#Add PlotCN
GFDLRAD2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
rm(GFDL2001)
#2002
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2002  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2002.nc", subds="rsds")
GFDL2002  <- terra::rotate(GFDL2002)
GFDLRAD2002<-data.frame(terra::extract(GFDL2002 ,allcoords[allcoords$Year == 2002,]))
GFDLRAD2002$RADAvg <- rowMeans(GFDLRAD2002[,2:366])
#Add PlotCN
GFDLRAD2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
rm(GFDL2002)
#2003
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2003  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2003.nc", subds="rsds")
GFDL2003  <- terra::rotate(GFDL2003)
GFDLRAD2003<-data.frame(terra::extract(GFDL2003 ,allcoords[allcoords$Year == 2003,]))
GFDLRAD2003$RADAvg <- rowMeans(GFDLRAD2003[,2:366])
#Add PlotCN
GFDLRAD2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
rm(GFDL2003)
#2004
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2004  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2004.nc", subds="rsds")
GFDL2004  <- terra::rotate(GFDL2004)
GFDLRAD2004<-data.frame(terra::extract(GFDL2004 ,allcoords[allcoords$Year == 2004,]))
GFDLRAD2004$RADAvg <- rowMeans(GFDLRAD2004[,2:366])
#Add PlotCN
GFDLRAD2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
rm(GFDL2004)
#2005
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2005  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2005.nc", subds="rsds")
GFDL2005  <- terra::rotate(GFDL2005)
GFDLRAD2005<-data.frame(terra::extract(GFDL2005 ,allcoords[allcoords$Year == 2005,]))
GFDLRAD2005$RADAvg <- rowMeans(GFDLRAD2005[,2:366])
#Add PlotCN
GFDLRAD2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
rm(GFDL2005)
#2006
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2006  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2006.nc", subds="rsds")
GFDL2006  <- terra::rotate(GFDL2006)
GFDLRAD2006<-data.frame(terra::extract(GFDL2006 ,allcoords[allcoords$Year == 2006,]))
GFDLRAD2006$RADAvg <- rowMeans(GFDLRAD2006[,2:366])
#Add PlotCN
GFDLRAD2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
rm(GFDL2006)
#2007
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2007  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2007.nc", subds="rsds")
GFDL2007  <- terra::rotate(GFDL2007)
GFDLRAD2007<-data.frame(terra::extract(GFDL2007 ,allcoords[allcoords$Year == 2007,]))
GFDLRAD2007$RADAvg <- rowMeans(GFDLRAD2007[,2:366])
#Add PlotCN
GFDLRAD2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
rm(GFDL2007)
#2008
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2008  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2008.nc", subds="rsds")
GFDL2008  <- terra::rotate(GFDL2008)
GFDLRAD2008<-data.frame(terra::extract(GFDL2008 ,allcoords[allcoords$Year == 2008,]))
GFDLRAD2008$RADAvg <- rowMeans(GFDLRAD2008[,2:366])
#Add PlotCN
GFDLRAD2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
rm(GFDL2008)
#2009
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2009  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2009.nc", subds="rsds")
GFDL2009  <- terra::rotate(GFDL2009)
GFDLRAD2009<-data.frame(terra::extract(GFDL2009 ,allcoords[allcoords$Year == 2009,]))
GFDLRAD2009$RADAvg <- rowMeans(GFDLRAD2009[,2:366])
#Add PlotCN
GFDLRAD2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
rm(GFDL2009)
#2010
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2010  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2010.nc", subds="rsds")
GFDL2010  <- terra::rotate(GFDL2010)
GFDLRAD2010<-data.frame(terra::extract(GFDL2010 ,allcoords[allcoords$Year == 2010,]))
GFDLRAD2010$RADAvg <- rowMeans(GFDLRAD2010[,2:366])
#Add PlotCN
GFDLRAD2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
rm(GFDL2010)
#2011
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2011  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2011.nc", subds="rsds")
GFDL2011  <- terra::rotate(GFDL2011)
GFDLRAD2011<-data.frame(terra::extract(GFDL2011 ,allcoords[allcoords$Year == 2011,]))
GFDLRAD2011$RADAvg <- rowMeans(GFDLRAD2011[,2:366])
#Add PlotCN
GFDLRAD2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
rm(GFDL2011)
#2012
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2012  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2012.nc", subds="rsds")
GFDL2012  <- terra::rotate(GFDL2012)
GFDLRAD2012<-data.frame(terra::extract(GFDL2012 ,allcoords[allcoords$Year == 2012,]))
GFDLRAD2012$RADAvg <- rowMeans(GFDLRAD2012[,2:366])
#Add PlotCN
GFDLRAD2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
rm(GFDL2012)
#2013
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2013  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2013.nc", subds="rsds")
GFDL2013  <- terra::rotate(GFDL2013)
GFDLRAD2013<-data.frame(terra::extract(GFDL2013 ,allcoords[allcoords$Year == 2013,]))
GFDLRAD2013$RADAvg <- rowMeans(GFDLRAD2013[,2:366])
#Add PlotCN
GFDLRAD2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
rm(GFDL2013)
#2014
setwd("~/Desktop/NASAClim/GFDLH")
GFDL2014  <- rast("~/Desktop/NASAClim/GFDLH/GFDLRsds2014.nc", subds="rsds")
GFDL2014  <- terra::rotate(GFDL2014)
GFDLRAD2014<-data.frame(terra::extract(GFDL2014 ,allcoords[allcoords$Year == 2014,]))
GFDLRAD2014$RADAvg <- rowMeans(GFDLRAD2014[,2:366])
#Add PlotCN
GFDLRAD2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
rm(GFDL2014)

GFDLRADFull<- as.data.frame(rbind(GFDLRAD2000[,367:368],GFDLRAD2001[,367:368],GFDLRAD2002[,367:368],GFDLRAD2003[,367:368],GFDLRAD2004[,367:368],GFDLRAD2005[,367:368], GFDLRAD2006[,367:368],
                                  GFDLRAD2007[,367:368], GFDLRAD2008[,367:368], GFDLRAD2009[,367:368], GFDLRAD2010[,367:368], GFDLRAD2011[,367:368],
                                  GFDLRAD2012[,367:368], GFDLRAD2013[,367:368], GFDLRAD2014[,367:368]))
GFDLRADFull <- GFDLRADFull[,2:1]

rm(GFDLRAD2000,GFDLRAD2001,GFDLRAD2002,GFDLRAD2003,GFDLRAD2004,GFDLRAD2005, 
   GFDLRAD2006,GFDLRAD2007, GFDLRAD2008, GFDLRAD2009, 
   GFDLRAD2010, GFDLRAD2011, GFDLRAD2012, GFDLRAD2013, GFDLRAD2014)


#MIROC
#2000
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2000  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2000.nc", subds="rsds")
MIROC2000  <- terra::rotate(MIROC2000)
MIROCRAD2000<-data.frame(terra::extract(MIROC2000 ,allcoords[allcoords$Year == 2000,]))
MIROCRAD2000$RADAvg <- rowMeans(MIROCRAD2000[,2:367])
#Add PlotCN
MIROCRAD2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
rm(MIROC2000)
#2001
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2001  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2001.nc", subds="rsds")
MIROC2001  <- terra::rotate(MIROC2001)
MIROCRAD2001<-data.frame(terra::extract(MIROC2001 ,allcoords[allcoords$Year == 2001,]))
MIROCRAD2001$RADAvg <- rowMeans(MIROCRAD2001[,2:366])
#Add PlotCN
MIROCRAD2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
rm(MIROC2001)
#2002
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2002  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2002.nc", subds="rsds")
MIROC2002  <- terra::rotate(MIROC2002)
MIROCRAD2002<-data.frame(terra::extract(MIROC2002 ,allcoords[allcoords$Year == 2002,]))
MIROCRAD2002$RADAvg <- rowMeans(MIROCRAD2002[,2:366])
#Add PlotCN
MIROCRAD2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
rm(MIROC2002)
#2003
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2003  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2003.nc", subds="rsds")
MIROC2003  <- terra::rotate(MIROC2003)
MIROCRAD2003<-data.frame(terra::extract(MIROC2003 ,allcoords[allcoords$Year == 2003,]))
MIROCRAD2003$RADAvg <- rowMeans(MIROCRAD2003[,2:366])
#Add PlotCN
MIROCRAD2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
rm(MIROC2003)
#2004
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2004  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2004.nc", subds="rsds")
MIROC2004  <- terra::rotate(MIROC2004)
MIROCRAD2004<-data.frame(terra::extract(MIROC2004 ,allcoords[allcoords$Year == 2004,]))
MIROCRAD2004$RADAvg <- rowMeans(MIROCRAD2004[,2:367])
#Add PlotCN
MIROCRAD2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
rm(MIROC2004)
#2005
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2005  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2005.nc", subds="rsds")
MIROC2005  <- terra::rotate(MIROC2005)
#Repeat for 
MIROCRAD2005<-data.frame(terra::extract(MIROC2005 ,allcoords[allcoords$Year == 2005,]))
MIROCRAD2005$RADAvg <- rowMeans(MIROCRAD2005[,2:366])
#Add PlotCN
MIROCRAD2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
rm(MIROC2005)
#2006
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2006  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2006.nc", subds="rsds")
MIROC2006  <- terra::rotate(MIROC2006)
MIROCRAD2006<-data.frame(terra::extract(MIROC2006 ,allcoords[allcoords$Year == 2006,]))
MIROCRAD2006$RADAvg <- rowMeans(MIROCRAD2006[,2:366])
#Add PlotCN
MIROCRAD2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
rm(MIROC2006)
#2007
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2007  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2007.nc", subds="rsds")
MIROC2007  <- terra::rotate(MIROC2007)
MIROCRAD2007<-data.frame(terra::extract(MIROC2007 ,allcoords[allcoords$Year == 2007,]))
MIROCRAD2007$RADAvg <- rowMeans(MIROCRAD2007[,2:366])
#Add PlotCN
MIROCRAD2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
rm(MIROC2007)
#2008
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2008  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2008.nc", subds="rsds")
MIROC2008  <- terra::rotate(MIROC2008)
MIROCRAD2008<-data.frame(terra::extract(MIROC2008 ,allcoords[allcoords$Year == 2008,]))
MIROCRAD2008$RADAvg <- rowMeans(MIROCRAD2008[,2:367])
#Add PlotCN
MIROCRAD2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
rm(MIROC2008)
#2009
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2009  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2009.nc", subds="rsds")
MIROC2009  <- terra::rotate(MIROC2009)
MIROCRAD2009<-data.frame(terra::extract(MIROC2009 ,allcoords[allcoords$Year == 2009,]))
MIROCRAD2009$RADAvg <- rowMeans(MIROCRAD2009[,2:366])
#Add PlotCN
MIROCRAD2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
rm(MIROC2009)
#2010
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2010  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2010.nc", subds="rsds")
MIROC2010  <- terra::rotate(MIROC2010)
MIROCRAD2010<-data.frame(terra::extract(MIROC2010 ,allcoords[allcoords$Year == 2010,]))
MIROCRAD2010$RADAvg <- rowMeans(MIROCRAD2010[,2:366])
#Add PlotCN
MIROCRAD2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
rm(MIROC2010)
#2011
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2011  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2011.nc", subds="rsds")
MIROC2011  <- terra::rotate(MIROC2011)
#Repeat for 
MIROCRAD2011<-data.frame(terra::extract(MIROC2011 ,allcoords[allcoords$Year == 2011,]))
MIROCRAD2011$RADAvg <- rowMeans(MIROCRAD2011[,2:366])
#Add PlotCN
MIROCRAD2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
rm(MIROC2011)
#2012
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2012  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2012.nc", subds="rsds")
MIROC2012  <- terra::rotate(MIROC2012)
MIROCRAD2012<-data.frame(terra::extract(MIROC2012 ,allcoords[allcoords$Year == 2012,]))
MIROCRAD2012$RADAvg <- rowMeans(MIROCRAD2012[,2:367])
#Add PlotCN
MIROCRAD2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
rm(MIROC2012)
#2013
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2013  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2013.nc", subds="rsds")
MIROC2013  <- terra::rotate(MIROC2013)
MIROCRAD2013<-data.frame(terra::extract(MIROC2013 ,allcoords[allcoords$Year == 2013,]))
MIROCRAD2013$RADAvg <- rowMeans(MIROCRAD2013[,2:366])
#Add PlotCN
MIROCRAD2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
rm(MIROC2013)
#2014
setwd("~/Desktop/NASAClim/MIROCH")
MIROC2014  <- rast("~/Desktop/NASAClim/MIROCH/MIROCRsds2014.nc", subds="rsds")
MIROC2014  <- terra::rotate(MIROC2014)
MIROCRAD2014<-data.frame(terra::extract(MIROC2014 ,allcoords[allcoords$Year == 2014,]))
MIROCRAD2014$RADAvg <- rowMeans(MIROCRAD2014[,2:366])
#Add PlotCN
MIROCRAD2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
rm(MIROC2014)

MIROCRADFull<- as.data.frame(rbind(MIROCRAD2000[,368:369],MIROCRAD2001[,367:368],MIROCRAD2002[,367:368],MIROCRAD2003[,367:368],MIROCRAD2004[,368:369],MIROCRAD2005[,367:368], MIROCRAD2006[,367:368],
                                   MIROCRAD2007[,367:368], MIROCRAD2008[,368:369], MIROCRAD2009[,367:368], MIROCRAD2010[,367:368], MIROCRAD2011[,367:368],
                                   MIROCRAD2012[,368:369], MIROCRAD2013[,367:368], MIROCRAD2014[,367:368]))
MIROCRADFull <- MIROCRADFull[,2:1]

rm(MIROCRAD2000,MIROCRAD2001,MIROCRAD2002,MIROCRAD2003,MIROCRAD2004,MIROCRAD2005, 
   MIROCRAD2006,MIROCRAD2007, MIROCRAD2008, MIROCRAD2009, 
   MIROCRAD2010, MIROCRAD2011, MIROCRAD2012, MIROCRAD2013, MIROCRAD2014)


#NORESM
#2000
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2000  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2000.nc", subds="rsds")
NORESM2000  <- terra::rotate(NORESM2000)
NORESMRAD2000<-data.frame(terra::extract(NORESM2000 ,allcoords[allcoords$Year == 2000,]))
NORESMRAD2000$RADAvg <- rowMeans(NORESMRAD2000[,2:366])
#Add PlotCN
NORESMRAD2000$PlotCN <- allcoords[allcoords$Year == 2000,]$PlotCN
rm(NORESM2000)
#2001
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2001  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2001.nc", subds="rsds")
NORESM2001  <- terra::rotate(NORESM2001)
NORESMRAD2001<-data.frame(terra::extract(NORESM2001 ,allcoords[allcoords$Year == 2001,]))
NORESMRAD2001$RADAvg <- rowMeans(NORESMRAD2001[,2:366])
#Add PlotCN
NORESMRAD2001$PlotCN <- allcoords[allcoords$Year == 2001,]$PlotCN
rm(NORESM2001)
#2002
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2002  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2002.nc", subds="rsds")
NORESM2002  <- terra::rotate(NORESM2002)
NORESMRAD2002<-data.frame(terra::extract(NORESM2002 ,allcoords[allcoords$Year == 2002,]))
NORESMRAD2002$RADAvg <- rowMeans(NORESMRAD2002[,2:366])
#Add PlotCN
NORESMRAD2002$PlotCN <- allcoords[allcoords$Year == 2002,]$PlotCN
rm(NORESM2002)
#2003
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2003  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2003.nc", subds="rsds")
NORESM2003  <- terra::rotate(NORESM2003)
NORESMRAD2003<-data.frame(terra::extract(NORESM2003 ,allcoords[allcoords$Year == 2003,]))
NORESMRAD2003$RADAvg <- rowMeans(NORESMRAD2003[,2:366])
#Add PlotCN
NORESMRAD2003$PlotCN <- allcoords[allcoords$Year == 2003,]$PlotCN
rm(NORESM2003)
#2004
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2004  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2004.nc", subds="rsds")
NORESM2004  <- terra::rotate(NORESM2004)
NORESMRAD2004<-data.frame(terra::extract(NORESM2004 ,allcoords[allcoords$Year == 2004,]))
NORESMRAD2004$RADAvg <- rowMeans(NORESMRAD2004[,2:366])
#Add PlotCN
NORESMRAD2004$PlotCN <- allcoords[allcoords$Year == 2004,]$PlotCN
rm(NORESM2004)
#2005
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2005  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2005.nc", subds="rsds")
NORESM2005  <- terra::rotate(NORESM2005)
#Repeat for 
NORESMRAD2005<-data.frame(terra::extract(NORESM2005 ,allcoords[allcoords$Year == 2005,]))
NORESMRAD2005$RADAvg <- rowMeans(NORESMRAD2005[,2:366])
#Add PlotCN
NORESMRAD2005$PlotCN <- allcoords[allcoords$Year == 2005,]$PlotCN
rm(NORESM2005)
#2006
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2006  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2006.nc", subds="rsds")
NORESM2006  <- terra::rotate(NORESM2006)
NORESMRAD2006<-data.frame(terra::extract(NORESM2006 ,allcoords[allcoords$Year == 2006,]))
NORESMRAD2006$RADAvg <- rowMeans(NORESMRAD2006[,2:366])
#Add PlotCN
NORESMRAD2006$PlotCN <- allcoords[allcoords$Year == 2006,]$PlotCN
rm(NORESM2006)
#2007
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2007  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2007.nc", subds="rsds")
NORESM2007  <- terra::rotate(NORESM2007)
NORESMRAD2007<-data.frame(terra::extract(NORESM2007 ,allcoords[allcoords$Year == 2007,]))
NORESMRAD2007$RADAvg <- rowMeans(NORESMRAD2007[,2:366])
#Add PlotCN
NORESMRAD2007$PlotCN <- allcoords[allcoords$Year == 2007,]$PlotCN
rm(NORESM2007)
#2008
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2008  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2008.nc", subds="rsds")
NORESM2008  <- terra::rotate(NORESM2008)
NORESMRAD2008<-data.frame(terra::extract(NORESM2008 ,allcoords[allcoords$Year == 2008,]))
NORESMRAD2008$RADAvg <- rowMeans(NORESMRAD2008[,2:366])
#Add PlotCN
NORESMRAD2008$PlotCN <- allcoords[allcoords$Year == 2008,]$PlotCN
rm(NORESM2008)
#2009
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2009  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2009.nc", subds="rsds")
NORESM2009  <- terra::rotate(NORESM2009)
NORESMRAD2009<-data.frame(terra::extract(NORESM2009 ,allcoords[allcoords$Year == 2009,]))
NORESMRAD2009$RADAvg <- rowMeans(NORESMRAD2009[,2:366])
#Add PlotCN
NORESMRAD2009$PlotCN <- allcoords[allcoords$Year == 2009,]$PlotCN
rm(NORESM2009)
#2010
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2010  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2010.nc", subds="rsds")
NORESM2010  <- terra::rotate(NORESM2010)
NORESMRAD2010<-data.frame(terra::extract(NORESM2010 ,allcoords[allcoords$Year == 2010,]))
NORESMRAD2010$RADAvg <- rowMeans(NORESMRAD2010[,2:366])
#Add PlotCN
NORESMRAD2010$PlotCN <- allcoords[allcoords$Year == 2010,]$PlotCN
rm(NORESM2010)
#2011
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2011  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2011.nc", subds="rsds")
NORESM2011  <- terra::rotate(NORESM2011)
#Repeat for 
NORESMRAD2011<-data.frame(terra::extract(NORESM2011 ,allcoords[allcoords$Year == 2011,]))
NORESMRAD2011$RADAvg <- rowMeans(NORESMRAD2011[,2:366])
#Add PlotCN
NORESMRAD2011$PlotCN <- allcoords[allcoords$Year == 2011,]$PlotCN
rm(NORESM2011)
#2012
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2012  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2012.nc", subds="rsds")
NORESM2012  <- terra::rotate(NORESM2012)
NORESMRAD2012<-data.frame(terra::extract(NORESM2012 ,allcoords[allcoords$Year == 2012,]))
NORESMRAD2012$RADAvg <- rowMeans(NORESMRAD2012[,2:366])
#Add PlotCN
NORESMRAD2012$PlotCN <- allcoords[allcoords$Year == 2012,]$PlotCN
rm(NORESM2012)
#2013
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2013  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2013.nc", subds="rsds")
NORESM2013  <- terra::rotate(NORESM2013)
NORESMRAD2013<-data.frame(terra::extract(NORESM2013 ,allcoords[allcoords$Year == 2013,]))
NORESMRAD2013$RADAvg <- rowMeans(NORESMRAD2013[,2:366])
#Add PlotCN
NORESMRAD2013$PlotCN <- allcoords[allcoords$Year == 2013,]$PlotCN
rm(NORESM2013)
#2014
setwd("~/Desktop/NASAClim/NORESMH")
NORESM2014  <- rast("~/Desktop/NASAClim/NORESMH/NORESMRsds2014.nc", subds="rsds")
NORESM2014  <- terra::rotate(NORESM2014)
NORESMRAD2014<-data.frame(terra::extract(NORESM2014 ,allcoords[allcoords$Year == 2014,]))
NORESMRAD2014$RADAvg <- rowMeans(NORESMRAD2014[,2:366])
#Add PlotCN
NORESMRAD2014$PlotCN <- allcoords[allcoords$Year == 2014,]$PlotCN
rm(NORESM2014)

NORESMRADFull<- as.data.frame(rbind(NORESMRAD2000[,367:368],NORESMRAD2001[,367:368],NORESMRAD2002[,367:368],NORESMRAD2003[,367:368],NORESMRAD2004[,367:368],NORESMRAD2005[,367:368], NORESMRAD2006[,367:368],
                                    NORESMRAD2007[,367:368], NORESMRAD2008[,367:368], NORESMRAD2009[,367:368], NORESMRAD2010[,367:368], NORESMRAD2011[,367:368],
                                    NORESMRAD2012[,367:368], NORESMRAD2013[,367:368], NORESMRAD2014[,367:368]))
NORESMRADFull <- NORESMRADFull[,2:1]

rm(NORESMRAD2000,NORESMRAD2001,NORESMRAD2002,NORESMRAD2003,NORESMRAD2004,NORESMRAD2005, 
   NORESMRAD2006,NORESMRAD2007, NORESMRAD2008, NORESMRAD2009, 
   NORESMRAD2010, NORESMRAD2011, NORESMRAD2012, NORESMRAD2013, NORESMRAD2014)

TempAll <- merge(GFDLTempFull,MIROCTempFull, by="PlotCN")%>%merge(NORESMTempFull,by="PlotCN")
TempAll$MAT <- rowMeans(TempAll[,2:4])
TempAll <- TempAll[,c(1,5)]
PRECAll <- merge(GFDLPRECFull,MIROCPRECFull, by="PlotCN")%>%merge(NORESMPRECFull,by="PlotCN")
PRECAll$PPT <- rowMeans(PRECAll[,2:4])
PRECAll <- PRECAll[,c(1,5)]
HUMAll <- merge(GFDLHUMFull,MIROCHUMFull, by="PlotCN")%>%merge(NORESMHUMFull,by="PlotCN")
HUMAll$RHUM <- rowMeans(HUMAll[,2:4])
HUMAll <- HUMAll[,c(1,5)]
RADAll <- merge(GFDLRADFull,MIROCRADFull, by="PlotCN")%>%merge(NORESMRADFull,by="PlotCN")
RADAll$RAD <- rowMeans(RADAll[,2:4])
RADAll <- RADAll[,c(1,5)]

setwd("~/Desktop/FIA/")
write.csv(TempAll,file="midAtlTemp0014.csv")
write.csv(PRECAll,file="midAtlPrec0014.csv")
write.csv(HUMAll,file="midAtlHum0014.csv")
write.csv(RADAll,file="midAtlRad0014.csv")

#future data projections from 2015-2080
#MIROC26
#Temp
#2015
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2015.nc", subds="tas")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCTEMP2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCTEMP2015$AvgTemp <- rowMeans(MIROCTEMP2015[,2:366])
#Add PlotCN
MIROCTEMP2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert K  to C
MIROCTEMP2015$AvgTemp <- MIROCTEMP2015$AvgTemp-273.15
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2016.nc", subds="tas")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCTEMP2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCTEMP2016$AvgTemp <- rowMeans(MIROCTEMP2016[,2:367])
#Add PlotCN
MIROCTEMP2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert K  to C
MIROCTEMP2016$AvgTemp <- MIROCTEMP2016$AvgTemp-273.15
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2017.nc", subds="tas")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCTEMP2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCTEMP2017$AvgTemp <- rowMeans(MIROCTEMP2017[,2:366])
#Add PlotCN
MIROCTEMP2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert K  to C
MIROCTEMP2017$AvgTemp <- MIROCTEMP2017$AvgTemp-273.15
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2018.nc", subds="tas")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCTEMP2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCTEMP2018$AvgTemp <- rowMeans(MIROCTEMP2018[,2:366])
#Add PlotCN
MIROCTEMP2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert K  to C
MIROCTEMP2018$AvgTemp <- MIROCTEMP2018$AvgTemp-273.15
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2019.nc", subds="tas")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCTEMP2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCTEMP2019$AvgTemp <- rowMeans(MIROCTEMP2019[,2:366])
#Add PlotCN
MIROCTEMP2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert K  to C
MIROCTEMP2019$AvgTemp <- MIROCTEMP2019$AvgTemp-273.15
rm(MIROC2019)

#Temp25
MIROC26b25 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2025.nc", subds="tas")
MIROC26b25 <- terra::rotate(MIROC26b25)
#Pull  vals
MIROC26TEMP25<-data.frame(terra::extract(MIROC26b25,allcoords1))
MIROC26TEMP25$Temp25 <- rowMeans(MIROC26TEMP25[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP25$Temp25 <- MIROC26TEMP25$Temp25-273.15
rownames(MIROC26TEMP25) <- allcoords1$PlotCN
rm(MIROC26b25)
#Temp30
MIROC26b30 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2030.nc", subds="tas")
MIROC26b30 <- terra::rotate(MIROC26b30)
#Pull  vals
MIROC26TEMP30<-data.frame(terra::extract(MIROC26b30,allcoords1))
MIROC26TEMP30$Temp30 <- rowMeans(MIROC26TEMP30[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP30$Temp30 <- MIROC26TEMP30$Temp30-273.15
rownames(MIROC26TEMP30) <- allcoords1$PlotCN
rm(MIROC26b30)
#Temp35
MIROC26b35 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2035.nc", subds="tas")
MIROC26b35 <- terra::rotate(MIROC26b35)
#Pull  vals
MIROC26TEMP35<-data.frame(terra::extract(MIROC26b35,allcoords1))
MIROC26TEMP35$Temp35 <- rowMeans(MIROC26TEMP35[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP35$Temp35 <- MIROC26TEMP35$Temp35-273.15
rownames(MIROC26TEMP35) <- allcoords1$PlotCN
rm(MIROC26b35)
#Temp40
MIROC26b40 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2040.nc", subds="tas")
MIROC26b40 <- terra::rotate(MIROC26b40)
#Pull  vals
MIROC26TEMP40<-data.frame(terra::extract(MIROC26b40,allcoords1))
MIROC26TEMP40$Temp40 <- rowMeans(MIROC26TEMP40[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP40$Temp40 <- MIROC26TEMP40$Temp40-273.15
rownames(MIROC26TEMP40) <- allcoords1$PlotCN
rm(MIROC26b40)
#Temp45
MIROC26b45 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2045.nc", subds="tas")
MIROC26b45 <- terra::rotate(MIROC26b45)
#Pull  vals
MIROC26TEMP45<-data.frame(terra::extract(MIROC26b45,allcoords1))
MIROC26TEMP45$Temp45 <- rowMeans(MIROC26TEMP45[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP45$Temp45 <- MIROC26TEMP45$Temp45-273.15
rownames(MIROC26TEMP45) <- allcoords1$PlotCN
rm(MIROC26b45)

#Temp50
MIROC26b50 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2050.nc", subds="tas")
MIROC26b50 <- terra::rotate(MIROC26b50)
#Pull  vals
MIROC26TEMP50<-data.frame(terra::extract(MIROC26b50,allcoords1))
MIROC26TEMP50$Temp50 <- rowMeans(MIROC26TEMP50[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP50$Temp50 <- MIROC26TEMP50$Temp50-273.15
rownames(MIROC26TEMP50) <- allcoords1$PlotCN
rm(MIROC26b50)
#Temp55
MIROC26b55 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2055.nc", subds="tas")
MIROC26b55 <- terra::rotate(MIROC26b55)
#Pull  vals
MIROC26TEMP55<-data.frame(terra::extract(MIROC26b55,allcoords1))
MIROC26TEMP55$Temp55 <- rowMeans(MIROC26TEMP55[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP55$Temp55 <- MIROC26TEMP55$Temp55-273.15
rownames(MIROC26TEMP55) <- allcoords1$PlotCN
rm(MIROC26b55)
#Temp60
MIROC26b60 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2060.nc", subds="tas")
MIROC26b60 <- terra::rotate(MIROC26b60)
#Pull  vals
MIROC26TEMP60<-data.frame(terra::extract(MIROC26b60,allcoords1))
MIROC26TEMP60$Temp60 <- rowMeans(MIROC26TEMP60[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP60$Temp60 <- MIROC26TEMP60$Temp60-273.15
rownames(MIROC26TEMP60) <- allcoords1$PlotCN
rm(MIROC26b60)
#Temp65
MIROC26b65 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2065.nc", subds="tas")
MIROC26b65 <- terra::rotate(MIROC26b65)
#Pull  vals
MIROC26TEMP65<-data.frame(terra::extract(MIROC26b65,allcoords1))
MIROC26TEMP65$Temp65 <- rowMeans(MIROC26TEMP65[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP65$Temp65 <- MIROC26TEMP65$Temp65-273.15
rownames(MIROC26TEMP65) <- allcoords1$PlotCN
rm(MIROC26b65)
#Temp70
MIROC26b70 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2070.nc", subds="tas")
MIROC26b70 <- terra::rotate(MIROC26b70)
#Pull  vals
MIROC26TEMP70<-data.frame(terra::extract(MIROC26b70,allcoords1))
MIROC26TEMP70$Temp70 <- rowMeans(MIROC26TEMP70[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP70$Temp70 <- MIROC26TEMP70$Temp70-273.15
rownames(MIROC26TEMP70) <- allcoords1$PlotCN
rm(MIROC26b70)
#Temp75
MIROC26b75 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2075.nc", subds="tas")
MIROC26b75 <- terra::rotate(MIROC26b75)
#Pull  vals
MIROC26TEMP75<-data.frame(terra::extract(MIROC26b75,allcoords1))
MIROC26TEMP75$Temp75 <- rowMeans(MIROC26TEMP75[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP75$Temp75 <- MIROC26TEMP75$Temp75-273.15
rownames(MIROC26TEMP75) <- allcoords1$PlotCN
rm(MIROC26b75)
#Temp80
MIROC26b80 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Ta2080.nc", subds="tas")
MIROC26b80 <- terra::rotate(MIROC26b80)
#Pull  vals
MIROC26TEMP80<-data.frame(terra::extract(MIROC26b80,allcoords1))
MIROC26TEMP80$Temp80 <- rowMeans(MIROC26TEMP80[,2:366])
#convert kg/m2/s -> to mm
MIROC26TEMP80$Temp80 <- MIROC26TEMP80$Temp80-273.15
rownames(MIROC26TEMP80) <- allcoords1$PlotCN
rm(MIROC26b80)

MIROC26TempFull<- data.frame(cbind(MIROC26TEMP25$Temp25,MIROC26TEMP30$Temp30,MIROC26TEMP35$Temp35,MIROC26TEMP40$Temp40, MIROC26TEMP45$Temp45,
                                   MIROC26TEMP50$Temp50, MIROC26TEMP55$Temp55, MIROC26TEMP60$Temp60, MIROC26TEMP65$Temp65, MIROC26TEMP70$Temp70, MIROC26TEMP75$Temp75, MIROC26TEMP80$Temp80))

colnames(MIROC26TempFull)<- c("Temp25","Temp30","Temp35","Temp40", "Temp45","Temp50", "Temp55", "Temp60", "Temp65", "Temp70", "Temp75", "Temp80")

MIROC26TEMP1519 <- as.data.frame(rbind(MIROCTEMP2015[,c(368,367)], MIROCTEMP2016[,c(369,368)], MIROCTEMP2017[,c(368,367)], MIROCTEMP2018[,c(368,367)],
                                       MIROCTEMP2019[,c(368,367)]))

rm(MIROC26TEMP25,MIROC26TEMP30,MIROC26TEMP35,MIROC26TEMP40, MIROC26TEMP45,
   MIROC26TEMP50, MIROC26TEMP55, MIROC26TEMP60, MIROC26TEMP65, MIROC26TEMP70, MIROC26TEMP75, MIROC26TEMP80)
rm(MIROCTEMP2015,MIROCTEMP2016,MIROCTEMP2017,MIROCTEMP2018,MIROCTEMP2019)
rownames(MIROC26TempFull) <- allcoords1$PlotCN

#Prec
#2015
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2015.nc", subds="pr")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCPREC2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCPREC2015$PrecSum <- rowSums(MIROCPREC2015[,2:366])
#Add PlotCN
MIROCPREC2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert to mm
MIROCPREC2015$PrecSum <- MIROCPREC2015$PrecSum*86400
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2016.nc", subds="pr")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCPREC2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCPREC2016$PrecSum <- rowSums(MIROCPREC2016[,2:367])
#Add PlotCN
MIROCPREC2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert to mm
MIROCPREC2016$PrecSum <- MIROCPREC2016$PrecSum*86400
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2017.nc", subds="pr")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCPREC2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCPREC2017$PrecSum <- rowSums(MIROCPREC2017[,2:366])
#Add PlotCN
MIROCPREC2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert to mm
MIROCPREC2017$PrecSum <- MIROCPREC2017$PrecSum*86400
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2018.nc", subds="pr")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCPREC2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCPREC2018$PrecSum <- rowSums(MIROCPREC2018[,2:366])
#Add PlotCN
MIROCPREC2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert to mm
MIROCPREC2018$PrecSum <- MIROCPREC2018$PrecSum*86400
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2019.nc", subds="pr")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCPREC2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCPREC2019$PrecSum <- rowSums(MIROCPREC2019[,2:366])
#Add PlotCN
MIROCPREC2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert to mm
MIROCPREC2019$PrecSum <- MIROCPREC2019$PrecSum*86400
rm(MIROC2019)

#Prec25
MIROC26b25 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2025.nc", subds="pr")
MIROC26b25 <- terra::rotate(MIROC26b25)
#Pull  vals
MIROC26PREC25<-data.frame(terra::extract(MIROC26b25,allcoords1))
MIROC26PREC25$Prec25 <- rowSums(MIROC26PREC25[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC25$Prec25 <- MIROC26PREC25$Prec25*86400
rownames(MIROC26PREC25) <- allcoords1$PlotCN
rm(MIROC26b25)
#Prec30
MIROC26b30 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2030.nc", subds="pr")
MIROC26b30 <- terra::rotate(MIROC26b30)
#Pull  vals
MIROC26PREC30<-data.frame(terra::extract(MIROC26b30,allcoords1))
MIROC26PREC30$Prec30 <- rowSums(MIROC26PREC30[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC30$Prec30 <- MIROC26PREC30$Prec30*86400
rownames(MIROC26PREC30) <- allcoords1$PlotCN
rm(MIROC26b30)
#Prec35
MIROC26b35 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2035.nc", subds="pr")
MIROC26b35 <- terra::rotate(MIROC26b35)
#Pull  vals
MIROC26PREC35<-data.frame(terra::extract(MIROC26b35,allcoords1))
MIROC26PREC35$Prec35 <- rowSums(MIROC26PREC35[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC35$Prec35 <- MIROC26PREC35$Prec35*86400
rownames(MIROC26PREC35) <- allcoords1$PlotCN
rm(MIROC26b35)
#Prec40
MIROC26b40 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2040.nc", subds="pr")
MIROC26b40 <- terra::rotate(MIROC26b40)
#Pull  vals
MIROC26PREC40<-data.frame(terra::extract(MIROC26b40,allcoords1))
MIROC26PREC40$Prec40 <- rowSums(MIROC26PREC40[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC40$Prec40 <- MIROC26PREC40$Prec40*86400
rownames(MIROC26PREC40) <- allcoords1$PlotCN
rm(MIROC26b40)
#Prec45
MIROC26b45 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2045.nc", subds="pr")
MIROC26b45 <- terra::rotate(MIROC26b45)
#Pull  vals
MIROC26PREC45<-data.frame(terra::extract(MIROC26b45,allcoords1))
MIROC26PREC45$Prec45 <- rowSums(MIROC26PREC45[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC45$Prec45 <- MIROC26PREC45$Prec45*86400
rownames(MIROC26PREC45) <- allcoords1$PlotCN
rm(MIROC26b45)

#Prec50
MIROC26b50 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2050.nc", subds="pr")
MIROC26b50 <- terra::rotate(MIROC26b50)
#Pull  vals
MIROC26PREC50<-data.frame(terra::extract(MIROC26b50,allcoords1))
MIROC26PREC50$Prec50 <- rowSums(MIROC26PREC50[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC50$Prec50 <- MIROC26PREC50$Prec50*86400
rownames(MIROC26PREC50) <- allcoords1$PlotCN
rm(MIROC26b50)
#Prec55
MIROC26b55 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2055.nc", subds="pr")
MIROC26b55 <- terra::rotate(MIROC26b55)
#Pull  vals
MIROC26PREC55<-data.frame(terra::extract(MIROC26b55,allcoords1))
MIROC26PREC55$Prec55 <- rowSums(MIROC26PREC55[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC55$Prec55 <- MIROC26PREC55$Prec55*86400
rownames(MIROC26PREC55) <- allcoords1$PlotCN
rm(MIROC26b55)
#Prec60
MIROC26b60 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2060.nc", subds="pr")
MIROC26b60 <- terra::rotate(MIROC26b60)
#Pull  vals
MIROC26PREC60<-data.frame(terra::extract(MIROC26b60,allcoords1))
MIROC26PREC60$Prec60 <- rowSums(MIROC26PREC60[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC60$Prec60 <- MIROC26PREC60$Prec60*86400
rownames(MIROC26PREC60) <- allcoords1$PlotCN
rm(MIROC26b60)
#Prec65
MIROC26b65 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2065.nc", subds="pr")
MIROC26b65 <- terra::rotate(MIROC26b65)
#Pull  vals
MIROC26PREC65<-data.frame(terra::extract(MIROC26b65,allcoords1))
MIROC26PREC65$Prec65 <- rowSums(MIROC26PREC65[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC65$Prec65 <- MIROC26PREC65$Prec65*86400
rownames(MIROC26PREC65) <- allcoords1$PlotCN
rm(MIROC26b65)
#Prec70
MIROC26b70 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2070.nc", subds="pr")
MIROC26b70 <- terra::rotate(MIROC26b70)
#Pull  vals
MIROC26PREC70<-data.frame(terra::extract(MIROC26b70,allcoords1))
MIROC26PREC70$Prec70 <- rowSums(MIROC26PREC70[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC70$Prec70 <- MIROC26PREC70$Prec70*86400
rownames(MIROC26PREC70) <- allcoords1$PlotCN
rm(MIROC26b70)
#Prec75
MIROC26b75 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2075.nc", subds="pr")
MIROC26b75 <- terra::rotate(MIROC26b75)
#Pull  vals
MIROC26PREC75<-data.frame(terra::extract(MIROC26b75,allcoords1))
MIROC26PREC75$Prec75 <- rowSums(MIROC26PREC75[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC75$Prec75 <- MIROC26PREC75$Prec75*86400
rownames(MIROC26PREC75) <- allcoords1$PlotCN
rm(MIROC26b75)
#Prec80
MIROC26b80 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Pr2080.nc", subds="pr")
MIROC26b80 <- terra::rotate(MIROC26b80)
#Pull  vals
MIROC26PREC80<-data.frame(terra::extract(MIROC26b80,allcoords1))
MIROC26PREC80$Prec80 <- rowSums(MIROC26PREC80[,2:366])
#convert kg/m2/s -> to mm
MIROC26PREC80$Prec80 <- MIROC26PREC80$Prec80*86400
rownames(MIROC26PREC80) <- allcoords1$PlotCN
rm(MIROC26b80)

MIROC26PrecFull<- data.frame(cbind(MIROC26PREC25$Prec25,MIROC26PREC30$Prec30,MIROC26PREC35$Prec35,MIROC26PREC40$Prec40, MIROC26PREC45$Prec45,
                                   MIROC26PREC50$Prec50, MIROC26PREC55$Prec55, MIROC26PREC60$Prec60, MIROC26PREC65$Prec65, MIROC26PREC70$Prec70, MIROC26PREC75$Prec75, MIROC26PREC80$Prec80))

colnames(MIROC26PrecFull)<- c("Prec25","Prec30","Prec35","Prec40", "Prec45","Prec50", "Prec55", "Prec60", "Prec65", "Prec70", "Prec75", "Prec80")

MIROC26PREC1519 <- as.data.frame(rbind(MIROCPREC2015[,c(368,367)], MIROCPREC2016[,c(369,368)], MIROCPREC2017[,c(368,367)], MIROCPREC2018[,c(368,367)],
                                       MIROCPREC2019[,c(368,367)]))

rm(MIROCPREC2015,MIROCPREC2016,MIROCPREC2017,MIROCPREC2018,MIROCPREC2019)

rm(MIROC26PREC25,MIROC26PREC30,MIROC26PREC35,MIROC26PREC40, MIROC26PREC45,
   MIROC26PREC50, MIROC26PREC55, MIROC26PREC60, MIROC26PREC65, MIROC26PREC70, MIROC26PREC75, MIROC26PREC80)
rownames(MIROC26PrecFull) <- allcoords1$PlotCN

#HUM
#2015
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2015.nc", subds="hurs")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCHUM2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCHUM2015$AvgHUM <- rowMeans(MIROCHUM2015[,2:366])
#Add PlotCN
MIROCHUM2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2016.nc", subds="hurs")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCHUM2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCHUM2016$AvgHUM <- rowMeans(MIROCHUM2016[,2:367])
#Add PlotCN
MIROCHUM2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2017.nc", subds="hurs")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCHUM2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCHUM2017$AvgHUM <- rowMeans(MIROCHUM2017[,2:366])
#Add PlotCN
MIROCHUM2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2018.nc", subds="hurs")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCHUM2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCHUM2018$AvgHUM <- rowMeans(MIROCHUM2018[,2:366])
#Add PlotCN
MIROCHUM2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2019.nc", subds="hurs")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCHUM2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCHUM2019$AvgHUM <- rowMeans(MIROCHUM2019[,2:366])
#Add PlotCN
MIROCHUM2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(MIROC2019)

#HUM25
MIROC26b25 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2025.nc", subds="hurs")
MIROC26b25 <- terra::rotate(MIROC26b25)
#Pull  vals
MIROC26HUM25<-data.frame(terra::extract(MIROC26b25,allcoords1))
MIROC26HUM25$HUM25 <- rowMeans(MIROC26HUM25[,2:366])
rownames(MIROC26HUM25) <- allcoords1$PlotCN
rm(MIROC26b25)
#HUM30
MIROC26b30 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2030.nc", subds="hurs")
MIROC26b30 <- terra::rotate(MIROC26b30)
#Pull  vals
MIROC26HUM30<-data.frame(terra::extract(MIROC26b30,allcoords1))
MIROC26HUM30$HUM30 <- rowMeans(MIROC26HUM30[,2:366])
rownames(MIROC26HUM30) <- allcoords1$PlotCN
rm(MIROC26b30)
#HUM35
MIROC26b35 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2035.nc", subds="hurs")
MIROC26b35 <- terra::rotate(MIROC26b35)
#Pull  vals
MIROC26HUM35<-data.frame(terra::extract(MIROC26b35,allcoords1))
MIROC26HUM35$HUM35 <- rowMeans(MIROC26HUM35[,2:366])
rownames(MIROC26HUM35) <- allcoords1$PlotCN
rm(MIROC26b35)
#HUM40
MIROC26b40 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2040.nc", subds="hurs")
MIROC26b40 <- terra::rotate(MIROC26b40)
#Pull  vals
MIROC26HUM40<-data.frame(terra::extract(MIROC26b40,allcoords1))
MIROC26HUM40$HUM40 <- rowMeans(MIROC26HUM40[,2:366])
rownames(MIROC26HUM40) <- allcoords1$PlotCN
rm(MIROC26b40)
#HUM45
MIROC26b45 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2045.nc", subds="hurs")
MIROC26b45 <- terra::rotate(MIROC26b45)
#Pull  vals
MIROC26HUM45<-data.frame(terra::extract(MIROC26b45,allcoords1))
MIROC26HUM45$HUM45 <- rowMeans(MIROC26HUM45[,2:366])
rownames(MIROC26HUM45) <- allcoords1$PlotCN
rm(MIROC26b45)

#HUM50
MIROC26b50 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2050.nc", subds="hurs")
MIROC26b50 <- terra::rotate(MIROC26b50)
#Pull  vals
MIROC26HUM50<-data.frame(terra::extract(MIROC26b50,allcoords1))
MIROC26HUM50$HUM50 <- rowMeans(MIROC26HUM50[,2:366])
rownames(MIROC26HUM50) <- allcoords1$PlotCN
rm(MIROC26b50)
#HUM55
MIROC26b55 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2055.nc", subds="hurs")
MIROC26b55 <- terra::rotate(MIROC26b55)
#Pull  vals
MIROC26HUM55<-data.frame(terra::extract(MIROC26b55,allcoords1))
MIROC26HUM55$HUM55 <- rowMeans(MIROC26HUM55[,2:366])
rownames(MIROC26HUM55) <- allcoords1$PlotCN
rm(MIROC26b55)
#HUM60
MIROC26b60 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2060.nc", subds="hurs")
MIROC26b60 <- terra::rotate(MIROC26b60)
#Pull  vals
MIROC26HUM60<-data.frame(terra::extract(MIROC26b60,allcoords1))
MIROC26HUM60$HUM60 <- rowMeans(MIROC26HUM60[,2:366])
rownames(MIROC26HUM60) <- allcoords1$PlotCN
rm(MIROC26b60)
#HUM65
MIROC26b65 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2065.nc", subds="hurs")
MIROC26b65 <- terra::rotate(MIROC26b65)
#Pull  vals
MIROC26HUM65<-data.frame(terra::extract(MIROC26b65,allcoords1))
MIROC26HUM65$HUM65 <- rowMeans(MIROC26HUM65[,2:366])
rownames(MIROC26HUM65) <- allcoords1$PlotCN
rm(MIROC26b65)
#HUM70
MIROC26b70 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2070.nc", subds="hurs")
MIROC26b70 <- terra::rotate(MIROC26b70)
#Pull  vals
MIROC26HUM70<-data.frame(terra::extract(MIROC26b70,allcoords1))
MIROC26HUM70$HUM70 <- rowMeans(MIROC26HUM70[,2:366])
rownames(MIROC26HUM70) <- allcoords1$PlotCN
rm(MIROC26b70)
#HUM75
MIROC26b75 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2075.nc", subds="hurs")
MIROC26b75 <- terra::rotate(MIROC26b75)
#Pull  vals
MIROC26HUM75<-data.frame(terra::extract(MIROC26b75,allcoords1))
MIROC26HUM75$HUM75 <- rowMeans(MIROC26HUM75[,2:366])
rownames(MIROC26HUM75) <- allcoords1$PlotCN
rm(MIROC26b75)
#HUM80
MIROC26b80 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26Hurs2080.nc", subds="hurs")
MIROC26b80 <- terra::rotate(MIROC26b80)
#Pull  vals
MIROC26HUM80<-data.frame(terra::extract(MIROC26b80,allcoords1))
MIROC26HUM80$HUM80 <- rowMeans(MIROC26HUM80[,2:366])
#convert kg/m2/s -> to mm
MIROC26HUM80$HUM80 <- MIROC26HUM80$HUM80
rownames(MIROC26HUM80) <- allcoords1$PlotCN
rm(MIROC26b80)

MIROC26HUMFull<- data.frame(cbind(MIROC26HUM25$HUM25,MIROC26HUM30$HUM30,MIROC26HUM35$HUM35,MIROC26HUM40$HUM40, MIROC26HUM45$HUM45,
                                  MIROC26HUM50$HUM50, MIROC26HUM55$HUM55, MIROC26HUM60$HUM60, MIROC26HUM65$HUM65, MIROC26HUM70$HUM70, MIROC26HUM75$HUM75, MIROC26HUM80$HUM80))

colnames(MIROC26HUMFull)<- c("HUM25","HUM30","HUM35","HUM40", "HUM45","HUM50", "HUM55", "HUM60", "HUM65", "HUM70", "HUM75", "HUM80")

MIROC26HUM1519 <- as.data.frame(rbind(MIROCHUM2015[,c(368,367)], MIROCHUM2016[,c(369,368)], MIROCHUM2017[,c(368,367)], MIROCHUM2018[,c(368,367)],
                                      MIROCHUM2019[,c(368,367)]))


rm(MIROC26HUM25,MIROC26HUM30,MIROC26HUM35,MIROC26HUM40, MIROC26HUM45,
   MIROC26HUM50, MIROC26HUM55, MIROC26HUM60, MIROC26HUM65, MIROC26HUM70, MIROC26HUM75, MIROC26HUM80)
rm(MIROCHUM2015,MIROCHUM2016,MIROCHUM2017,MIROCHUM2018,MIROCHUM2019)
rownames(MIROC26HUMFull) <- allcoords1$PlotCN
#RAD
#2015
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2015.nc", subds="rsds")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCRAD2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCRAD2015$AvgRAD <- rowMeans(MIROCRAD2015[,2:366])
#Add PlotCN
MIROCRAD2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2016.nc", subds="rsds")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCRAD2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCRAD2016$AvgRAD <- rowMeans(MIROCRAD2016[,2:367])
#Add PlotCN
MIROCRAD2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2017.nc", subds="rsds")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCRAD2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCRAD2017$AvgRAD <- rowMeans(MIROCRAD2017[,2:366])
#Add PlotCN
MIROCRAD2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2018.nc", subds="rsds")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCRAD2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCRAD2018$AvgRAD <- rowMeans(MIROCRAD2018[,2:366])
#Add PlotCN
MIROCRAD2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC26")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2019.nc", subds="rsds")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCRAD2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCRAD2019$AvgRAD <- rowMeans(MIROCRAD2019[,2:366])
#Add PlotCN
MIROCRAD2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(MIROC2019)

#RAD25
MIROC26b25 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2025.nc", subds="rsds")
MIROC26b25 <- terra::rotate(MIROC26b25)
#Pull  vals
MIROC26RAD25<-data.frame(terra::extract(MIROC26b25,allcoords1))
MIROC26RAD25$RAD25 <- rowMeans(MIROC26RAD25[,2:366])
rownames(MIROC26RAD25) <- allcoords1$PlotCN
rm(MIROC26b25)
#RAD30
MIROC26b30 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2030.nc", subds="rsds")
MIROC26b30 <- terra::rotate(MIROC26b30)
#Pull  vals
MIROC26RAD30<-data.frame(terra::extract(MIROC26b30,allcoords1))
MIROC26RAD30$RAD30 <- rowMeans(MIROC26RAD30[,2:366])
rownames(MIROC26RAD30) <- allcoords1$PlotCN
rm(MIROC26b30)
#RAD35
MIROC26b35 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2035.nc", subds="rsds")
MIROC26b35 <- terra::rotate(MIROC26b35)
#Pull  vals
MIROC26RAD35<-data.frame(terra::extract(MIROC26b35,allcoords1))
MIROC26RAD35$RAD35 <- rowMeans(MIROC26RAD35[,2:366])
rownames(MIROC26RAD35) <- allcoords1$PlotCN
rm(MIROC26b35)
#RAD40
MIROC26b40 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2040.nc", subds="rsds")
MIROC26b40 <- terra::rotate(MIROC26b40)
#Pull  vals
MIROC26RAD40<-data.frame(terra::extract(MIROC26b40,allcoords1))
MIROC26RAD40$RAD40 <- rowMeans(MIROC26RAD40[,2:366])
rownames(MIROC26RAD40) <- allcoords1$PlotCN
rm(MIROC26b40)
#RAD45
MIROC26b45 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2045.nc", subds="rsds")
MIROC26b45 <- terra::rotate(MIROC26b45)
#Pull  vals
MIROC26RAD45<-data.frame(terra::extract(MIROC26b45,allcoords1))
MIROC26RAD45$RAD45 <- rowMeans(MIROC26RAD45[,2:366])
rownames(MIROC26RAD45) <- allcoords1$PlotCN
rm(MIROC26b45)

#RAD50
MIROC26b50 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2050.nc", subds="rsds")
MIROC26b50 <- terra::rotate(MIROC26b50)
#Pull  vals
MIROC26RAD50<-data.frame(terra::extract(MIROC26b50,allcoords1))
MIROC26RAD50$RAD50 <- rowMeans(MIROC26RAD50[,2:366])
rownames(MIROC26RAD50) <- allcoords1$PlotCN
rm(MIROC26b50)
#RAD55
MIROC26b55 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2055.nc", subds="rsds")
MIROC26b55 <- terra::rotate(MIROC26b55)
#Pull  vals
MIROC26RAD55<-data.frame(terra::extract(MIROC26b55,allcoords1))
MIROC26RAD55$RAD55 <- rowMeans(MIROC26RAD55[,2:366])
rownames(MIROC26RAD55) <- allcoords1$PlotCN
rm(MIROC26b55)
#RAD60
MIROC26b60 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2060.nc", subds="rsds")
MIROC26b60 <- terra::rotate(MIROC26b60)
#Pull  vals
MIROC26RAD60<-data.frame(terra::extract(MIROC26b60,allcoords1))
MIROC26RAD60$RAD60 <- rowMeans(MIROC26RAD60[,2:366])
rownames(MIROC26RAD60) <- allcoords1$PlotCN
rm(MIROC26b60)
#RAD65
MIROC26b65 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2065.nc", subds="rsds")
MIROC26b65 <- terra::rotate(MIROC26b65)
#Pull  vals
MIROC26RAD65<-data.frame(terra::extract(MIROC26b65,allcoords1))
MIROC26RAD65$RAD65 <- rowMeans(MIROC26RAD65[,2:366])
rownames(MIROC26RAD65) <- allcoords1$PlotCN
rm(MIROC26b65)
#RAD70
MIROC26b70 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2070.nc", subds="rsds")
MIROC26b70 <- terra::rotate(MIROC26b70)
#Pull  vals
MIROC26RAD70<-data.frame(terra::extract(MIROC26b70,allcoords1))
MIROC26RAD70$RAD70 <- rowMeans(MIROC26RAD70[,2:366])
rownames(MIROC26RAD70) <- allcoords1$PlotCN
rm(MIROC26b70)
#RAD75
MIROC26b75 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2075.nc", subds="rsds")
MIROC26b75 <- terra::rotate(MIROC26b75)
#Pull  vals
MIROC26RAD75<-data.frame(terra::extract(MIROC26b75,allcoords1))
MIROC26RAD75$RAD75 <- rowMeans(MIROC26RAD75[,2:366])
rownames(MIROC26RAD75) <- allcoords1$PlotCN
rm(MIROC26b75)
#RAD80
MIROC26b80 <- rast("~/Desktop/NASAClim/MIROC26/MIROC26RAD2080.nc", subds="rsds")
MIROC26b80 <- terra::rotate(MIROC26b80)
#Pull  vals
MIROC26RAD80<-data.frame(terra::extract(MIROC26b80,allcoords1))
MIROC26RAD80$RAD80 <- rowMeans(MIROC26RAD80[,2:366])
#convert kg/m2/s -> to mm
MIROC26RAD80$RAD80 <- MIROC26RAD80$RAD80
rownames(MIROC26RAD80) <- allcoords1$PlotCN
rm(MIROC26b80)

MIROC26RADFull<- data.frame(cbind(MIROC26RAD25$RAD25,MIROC26RAD30$RAD30,MIROC26RAD35$RAD35,MIROC26RAD40$RAD40, MIROC26RAD45$RAD45,
                                  MIROC26RAD50$RAD50, MIROC26RAD55$RAD55, MIROC26RAD60$RAD60, MIROC26RAD65$RAD65, MIROC26RAD70$RAD70, MIROC26RAD75$RAD75, MIROC26RAD80$RAD80))

colnames(MIROC26RADFull)<- c("RAD25","RAD30","RAD35","RAD40", "RAD45","RAD50", "RAD55", "RAD60", "RAD65", "RAD70", "RAD75", "RAD80")

MIROC26RAD1519 <- as.data.frame(rbind(MIROCRAD2015[,c(368,367)], MIROCRAD2016[,c(369,368)], MIROCRAD2017[,c(368,367)], MIROCRAD2018[,c(368,367)],
                                      MIROCRAD2019[,c(368,367)]))

rm(MIROCRAD2015,MIROCRAD2016,MIROCRAD2017,MIROCRAD2018,MIROCRAD2019)

rm(MIROC26RAD25,MIROC26RAD30,MIROC26RAD35,MIROC26RAD40, MIROC26RAD45,
   MIROC26RAD50, MIROC26RAD55, MIROC26RAD60, MIROC26RAD65, MIROC26RAD70, MIROC26RAD75, MIROC26RAD80)
rownames(MIROC26RADFull) <- allcoords1$PlotCN

#future data projections from 2015-2080
#MIROC45
#Temp
#2015
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2015.nc", subds="tas")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCTEMP2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCTEMP2015$AvgTemp <- rowMeans(MIROCTEMP2015[,2:366])
#Add PlotCN
MIROCTEMP2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert K  to C
MIROCTEMP2015$AvgTemp <- MIROCTEMP2015$AvgTemp-273.15
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2016.nc", subds="tas")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCTEMP2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCTEMP2016$AvgTemp <- rowMeans(MIROCTEMP2016[,2:367])
#Add PlotCN
MIROCTEMP2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert K  to C
MIROCTEMP2016$AvgTemp <- MIROCTEMP2016$AvgTemp-273.15
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2017.nc", subds="tas")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCTEMP2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCTEMP2017$AvgTemp <- rowMeans(MIROCTEMP2017[,2:366])
#Add PlotCN
MIROCTEMP2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert K  to C
MIROCTEMP2017$AvgTemp <- MIROCTEMP2017$AvgTemp-273.15
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2018.nc", subds="tas")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCTEMP2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCTEMP2018$AvgTemp <- rowMeans(MIROCTEMP2018[,2:366])
#Add PlotCN
MIROCTEMP2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert K  to C
MIROCTEMP2018$AvgTemp <- MIROCTEMP2018$AvgTemp-273.15
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2019.nc", subds="tas")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCTEMP2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCTEMP2019$AvgTemp <- rowMeans(MIROCTEMP2019[,2:366])
#Add PlotCN
MIROCTEMP2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert K  to C
MIROCTEMP2019$AvgTemp <- MIROCTEMP2019$AvgTemp-273.15
rm(MIROC2019)

#Temp25
MIROC45b25 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2025.nc", subds="tas")
MIROC45b25 <- terra::rotate(MIROC45b25)
#Pull  vals
MIROC45TEMP25<-data.frame(terra::extract(MIROC45b25,allcoords1))
MIROC45TEMP25$Temp25 <- rowMeans(MIROC45TEMP25[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP25$Temp25 <- MIROC45TEMP25$Temp25-273.15
rownames(MIROC45TEMP25) <- allcoords1$PlotCN
rm(MIROC45b25)
#Temp30
MIROC45b30 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2030.nc", subds="tas")
MIROC45b30 <- terra::rotate(MIROC45b30)
#Pull  vals
MIROC45TEMP30<-data.frame(terra::extract(MIROC45b30,allcoords1))
MIROC45TEMP30$Temp30 <- rowMeans(MIROC45TEMP30[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP30$Temp30 <- MIROC45TEMP30$Temp30-273.15
rownames(MIROC45TEMP30) <- allcoords1$PlotCN
rm(MIROC45b30)
#Temp35
MIROC45b35 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2035.nc", subds="tas")
MIROC45b35 <- terra::rotate(MIROC45b35)
#Pull  vals
MIROC45TEMP35<-data.frame(terra::extract(MIROC45b35,allcoords1))
MIROC45TEMP35$Temp35 <- rowMeans(MIROC45TEMP35[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP35$Temp35 <- MIROC45TEMP35$Temp35-273.15
rownames(MIROC45TEMP35) <- allcoords1$PlotCN
rm(MIROC45b35)
#Temp40
MIROC45b40 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2040.nc", subds="tas")
MIROC45b40 <- terra::rotate(MIROC45b40)
#Pull  vals
MIROC45TEMP40<-data.frame(terra::extract(MIROC45b40,allcoords1))
MIROC45TEMP40$Temp40 <- rowMeans(MIROC45TEMP40[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP40$Temp40 <- MIROC45TEMP40$Temp40-273.15
rownames(MIROC45TEMP40) <- allcoords1$PlotCN
rm(MIROC45b40)
#Temp45
MIROC45b45 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2045.nc", subds="tas")
MIROC45b45 <- terra::rotate(MIROC45b45)
#Pull  vals
MIROC45TEMP45<-data.frame(terra::extract(MIROC45b45,allcoords1))
MIROC45TEMP45$Temp45 <- rowMeans(MIROC45TEMP45[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP45$Temp45 <- MIROC45TEMP45$Temp45-273.15
rownames(MIROC45TEMP45) <- allcoords1$PlotCN
rm(MIROC45b45)

#Temp50
MIROC45b50 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2050.nc", subds="tas")
MIROC45b50 <- terra::rotate(MIROC45b50)
#Pull  vals
MIROC45TEMP50<-data.frame(terra::extract(MIROC45b50,allcoords1))
MIROC45TEMP50$Temp50 <- rowMeans(MIROC45TEMP50[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP50$Temp50 <- MIROC45TEMP50$Temp50-273.15
rownames(MIROC45TEMP50) <- allcoords1$PlotCN
rm(MIROC45b50)
#Temp55
MIROC45b55 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2055.nc", subds="tas")
MIROC45b55 <- terra::rotate(MIROC45b55)
#Pull  vals
MIROC45TEMP55<-data.frame(terra::extract(MIROC45b55,allcoords1))
MIROC45TEMP55$Temp55 <- rowMeans(MIROC45TEMP55[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP55$Temp55 <- MIROC45TEMP55$Temp55-273.15
rownames(MIROC45TEMP55) <- allcoords1$PlotCN
rm(MIROC45b55)
#Temp60
MIROC45b60 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2060.nc", subds="tas")
MIROC45b60 <- terra::rotate(MIROC45b60)
#Pull  vals
MIROC45TEMP60<-data.frame(terra::extract(MIROC45b60,allcoords1))
MIROC45TEMP60$Temp60 <- rowMeans(MIROC45TEMP60[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP60$Temp60 <- MIROC45TEMP60$Temp60-273.15
rownames(MIROC45TEMP60) <- allcoords1$PlotCN
rm(MIROC45b60)
#Temp65
MIROC45b65 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2065.nc", subds="tas")
MIROC45b65 <- terra::rotate(MIROC45b65)
#Pull  vals
MIROC45TEMP65<-data.frame(terra::extract(MIROC45b65,allcoords1))
MIROC45TEMP65$Temp65 <- rowMeans(MIROC45TEMP65[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP65$Temp65 <- MIROC45TEMP65$Temp65-273.15
rownames(MIROC45TEMP65) <- allcoords1$PlotCN
rm(MIROC45b65)
#Temp70
MIROC45b70 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2070.nc", subds="tas")
MIROC45b70 <- terra::rotate(MIROC45b70)
#Pull  vals
MIROC45TEMP70<-data.frame(terra::extract(MIROC45b70,allcoords1))
MIROC45TEMP70$Temp70 <- rowMeans(MIROC45TEMP70[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP70$Temp70 <- MIROC45TEMP70$Temp70-273.15
rownames(MIROC45TEMP70) <- allcoords1$PlotCN
rm(MIROC45b70)
#Temp75
MIROC45b75 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2075.nc", subds="tas")
MIROC45b75 <- terra::rotate(MIROC45b75)
#Pull  vals
MIROC45TEMP75<-data.frame(terra::extract(MIROC45b75,allcoords1))
MIROC45TEMP75$Temp75 <- rowMeans(MIROC45TEMP75[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP75$Temp75 <- MIROC45TEMP75$Temp75-273.15
rownames(MIROC45TEMP75) <- allcoords1$PlotCN
rm(MIROC45b75)
#Temp80
MIROC45b80 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Ta2080.nc", subds="tas")
MIROC45b80 <- terra::rotate(MIROC45b80)
#Pull  vals
MIROC45TEMP80<-data.frame(terra::extract(MIROC45b80,allcoords1))
MIROC45TEMP80$Temp80 <- rowMeans(MIROC45TEMP80[,2:366])
#convert kg/m2/s -> to mm
MIROC45TEMP80$Temp80 <- MIROC45TEMP80$Temp80-273.15
rownames(MIROC45TEMP80) <- allcoords1$PlotCN
rm(MIROC45b80)

MIROC45TempFull<- data.frame(cbind(MIROC45TEMP25$Temp25,MIROC45TEMP30$Temp30,MIROC45TEMP35$Temp35,MIROC45TEMP40$Temp40, MIROC45TEMP45$Temp45,
                                   MIROC45TEMP50$Temp50, MIROC45TEMP55$Temp55, MIROC45TEMP60$Temp60, MIROC45TEMP65$Temp65, MIROC45TEMP70$Temp70, MIROC45TEMP75$Temp75, MIROC45TEMP80$Temp80))

colnames(MIROC45TempFull)<- c("Temp25","Temp30","Temp35","Temp40", "Temp45","Temp50", "Temp55", "Temp60", "Temp65", "Temp70", "Temp75", "Temp80")

MIROC45TEMP1519 <- as.data.frame(rbind(MIROCTEMP2015[,c(368,367)], MIROCTEMP2016[,c(369,368)], MIROCTEMP2017[,c(368,367)], MIROCTEMP2018[,c(368,367)],
                                       MIROCTEMP2019[,c(368,367)]))


rm(MIROC45TEMP25,MIROC45TEMP30,MIROC45TEMP35,MIROC45TEMP40, MIROC45TEMP45,
   MIROC45TEMP50, MIROC45TEMP55, MIROC45TEMP60, MIROC45TEMP65, MIROC45TEMP70, MIROC45TEMP75, MIROC45TEMP80)
rownames(MIROC45TempFull) <- allcoords1$PlotCN

#Prec
#2015
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2015.nc", subds="pr")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCPREC2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCPREC2015$PrecSum <- rowSums(MIROCPrec2015[,2:366])
#Add PlotCN
MIROCPREC2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert to mm
MIROCPREC2015$PrecSum <- MIROCPREC2015$PrecSum*86400
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2016.nc", subds="pr")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCPREC2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCPREC2016$PrecSum <- rowSums(MIROCPrec2016[,2:367])
#Add PlotCN
MIROCPREC2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert to mm
MIROCPREC2016$PrecSum <- MIROCPREC2016$PrecSum*86400
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2017.nc", subds="pr")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCPREC2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCPREC2017$PrecSum <- rowSums(MIROCPrec2017[,2:366])
#Add PlotCN
MIROCPREC2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert to mm
MIROCPREC2017$PrecSum <- MIROCPREC2017$PrecSum*86400
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2018.nc", subds="pr")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCPREC2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCPREC2018$PrecSum <- rowSums(MIROCPrec2018[,2:366])
#Add PlotCN
MIROCPREC2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert to mm
MIROCPREC2018$PrecSum <- MIROCPREC2018$PrecSum*86400
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2019.nc", subds="pr")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCPREC2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCPREC2019$PrecSum <- rowSums(MIROCPrec2019[,2:366])
#Add PlotCN
MIROCPREC2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert to mm
MIROCPREC2019$PrecSum <- MIROCPREC2019$PrecSum*86400
rm(MIROC2019)

#Prec25
MIROC45b25 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2025.nc", subds="pr")
MIROC45b25 <- terra::rotate(MIROC45b25)
#Pull  vals
MIROC45PREC25<-data.frame(terra::extract(MIROC45b25,allcoords1))
MIROC45PREC25$Prec25 <- rowSums(MIROC45Prec25[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC25$Prec25 <- MIROC45PREC25$Prec25*86400
rownames(MIROC45PREC25) <- allcoords1$PlotCN
rm(MIROC45b25)
#Prec30
MIROC45b30 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2030.nc", subds="pr")
MIROC45b30 <- terra::rotate(MIROC45b30)
#Pull  vals
MIROC45PREC30<-data.frame(terra::extract(MIROC45b30,allcoords1))
MIROC45PREC30$Prec30 <- rowSums(MIROC45Prec30[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC30$Prec30 <- MIROC45PREC30$Prec30*86400
rownames(MIROC45PREC30) <- allcoords1$PlotCN
rm(MIROC45b30)
#Prec35
MIROC45b35 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2035.nc", subds="pr")
MIROC45b35 <- terra::rotate(MIROC45b35)
#Pull  vals
MIROC45PREC35<-data.frame(terra::extract(MIROC45b35,allcoords1))
MIROC45PREC35$Prec35 <- rowSums(MIROC45Prec35[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC35$Prec35 <- MIROC45PREC35$Prec35*86400
rownames(MIROC45PREC35) <- allcoords1$PlotCN
rm(MIROC45b35)
#Prec40
MIROC45b40 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2040.nc", subds="pr")
MIROC45b40 <- terra::rotate(MIROC45b40)
#Pull  vals
MIROC45PREC40<-data.frame(terra::extract(MIROC45b40,allcoords1))
MIROC45PREC40$Prec40 <- rowSums(MIROC45Prec40[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC40$Prec40 <- MIROC45PREC40$Prec40*86400
rownames(MIROC45PREC40) <- allcoords1$PlotCN
rm(MIROC45b40)
#Prec45
MIROC45b45 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2045.nc", subds="pr")
MIROC45b45 <- terra::rotate(MIROC45b45)
#Pull  vals
MIROC45PREC45<-data.frame(terra::extract(MIROC45b45,allcoords1))
MIROC45PREC45$Prec45 <- rowSums(MIROC45Prec45[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC45$Prec45 <- MIROC45PREC45$Prec45*86400
rownames(MIROC45PREC45) <- allcoords1$PlotCN
rm(MIROC45b45)

#Prec50
MIROC45b50 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2050.nc", subds="pr")
MIROC45b50 <- terra::rotate(MIROC45b50)
#Pull  vals
MIROC45PREC50<-data.frame(terra::extract(MIROC45b50,allcoords1))
MIROC45PREC50$Prec50 <- rowSums(MIROC45Prec50[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC50$Prec50 <- MIROC45PREC50$Prec50*86400
rownames(MIROC45PREC50) <- allcoords1$PlotCN
rm(MIROC45b50)
#Prec55
MIROC45b55 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2055.nc", subds="pr")
MIROC45b55 <- terra::rotate(MIROC45b55)
#Pull  vals
MIROC45PREC55<-data.frame(terra::extract(MIROC45b55,allcoords1))
MIROC45PREC55$Prec55 <- rowSums(MIROC45Prec55[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC55$Prec55 <- MIROC45PREC55$Prec55*86400
rownames(MIROC45PREC55) <- allcoords1$PlotCN
rm(MIROC45b55)
#Prec60
MIROC45b60 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2060.nc", subds="pr")
MIROC45b60 <- terra::rotate(MIROC45b60)
#Pull  vals
MIROC45PREC60<-data.frame(terra::extract(MIROC45b60,allcoords1))
MIROC45PREC60$Prec60 <- rowSums(MIROC45Prec60[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC60$Prec60 <- MIROC45PREC60$Prec60*86400
rownames(MIROC45PREC60) <- allcoords1$PlotCN
rm(MIROC45b60)
#Prec65
MIROC45b65 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2065.nc", subds="pr")
MIROC45b65 <- terra::rotate(MIROC45b65)
#Pull  vals
MIROC45PREC65<-data.frame(terra::extract(MIROC45b65,allcoords1))
MIROC45PREC65$Prec65 <- rowSums(MIROC45Prec65[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC65$Prec65 <- MIROC45PREC65$Prec65*86400
rownames(MIROC45PREC65) <- allcoords1$PlotCN
rm(MIROC45b65)
#Prec70
MIROC45b70 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2070.nc", subds="pr")
MIROC45b70 <- terra::rotate(MIROC45b70)
#Pull  vals
MIROC45PREC70<-data.frame(terra::extract(MIROC45b70,allcoords1))
MIROC45PREC70$Prec70 <- rowSums(MIROC45Prec70[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC70$Prec70 <- MIROC45PREC70$Prec70*86400
rownames(MIROC45PREC70) <- allcoords1$PlotCN
rm(MIROC45b70)
#Prec75
MIROC45b75 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2075.nc", subds="pr")
MIROC45b75 <- terra::rotate(MIROC45b75)
#Pull  vals
MIROC45PREC75<-data.frame(terra::extract(MIROC45b75,allcoords1))
MIROC45PREC75$Prec75 <- rowSums(MIROC45Prec75[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC75$Prec75 <- MIROC45PREC75$Prec75*86400
rownames(MIROC45PREC75) <- allcoords1$PlotCN
rm(MIROC45b75)
#Prec80
MIROC45b80 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Pr2080.nc", subds="pr")
MIROC45b80 <- terra::rotate(MIROC45b80)
#Pull  vals
MIROC45PREC80<-data.frame(terra::extract(MIROC45b80,allcoords1))
MIROC45PREC80$Prec80 <- rowSums(MIROC45Prec80[,2:366])
#convert kg/m2/s -> to mm
MIROC45PREC80$Prec80 <- MIROC45PREC80$Prec80*86400
rownames(MIROC45PREC80) <- allcoords1$PlotCN
rm(MIROC45b80)

MIROC45PrecFull<- data.frame(cbind(MIROC45PREC25$Prec25,MIROC45PREC30$Prec30,MIROC45PREC35$Prec35,MIROC45PREC40$Prec40, MIROC45PREC45$Prec45,
                                   MIROC45PREC50$Prec50, MIROC45PREC55$Prec55, MIROC45PREC60$Prec60, MIROC45PREC65$Prec65, MIROC45PREC70$Prec70, MIROC45PREC75$Prec75, MIROC45PREC80$Prec80))

colnames(MIROC45PrecFull)<- c("Prec25","Prec30","Prec35","Prec40", "Prec45","Prec50", "Prec55", "Prec60", "Prec65", "Prec70", "Prec75", "Prec80")

MIROC45PREC1519 <- as.data.frame(rbind(MIROCPREC2015[,c(368,367)], MIROCPREC2016[,c(369,368)], MIROCPREC2017[,c(368,367)], MIROCPREC2018[,c(368,367)],
                                       MIROCPREC2019[,c(368,367)]))


rm(MIROC45PREC25,MIROC45PREC30,MIROC45PREC35,MIROC45PREC40, MIROC45PREC45,
   MIROC45PREC50, MIROC45PREC55, MIROC45PREC60, MIROC45PREC65, MIROC45PREC70, MIROC45PREC75, MIROC45PREC80)
rownames(MIROC45PrecFull) <- allcoords1$PlotCN

#HUM
#2015
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2015.nc", subds="hurs")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCHUM2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCHUM2015$AvgHUM <- rowMeans(MIROCHUM2015[,2:366])
#Add PlotCN
MIROCHUM2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2016.nc", subds="hurs")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCHUM2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCHUM2016$AvgHUM <- rowMeans(MIROCHUM2016[,2:367])
#Add PlotCN
MIROCHUM2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2017.nc", subds="hurs")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCHUM2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCHUM2017$AvgHUM <- rowMeans(MIROCHUM2017[,2:366])
#Add PlotCN
MIROCHUM2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2018.nc", subds="hurs")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCHUM2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCHUM2018$AvgHUM <- rowMeans(MIROCHUM2018[,2:366])
#Add PlotCN
MIROCHUM2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2019.nc", subds="hurs")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCHUM2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCHUM2019$AvgHUM <- rowMeans(MIROCHUM2019[,2:366])
#Add PlotCN
MIROCHUM2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(MIROC2019)

#HUM25
MIROC45b25 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2025.nc", subds="hurs")
MIROC45b25 <- terra::rotate(MIROC45b25)
#Pull  vals
MIROC45HUM25<-data.frame(terra::extract(MIROC45b25,allcoords1))
MIROC45HUM25$HUM25 <- rowMeans(MIROC45HUM25[,2:366])
rownames(MIROC45HUM25) <- allcoords1$PlotCN
rm(MIROC45b25)
#HUM30
MIROC45b30 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2030.nc", subds="hurs")
MIROC45b30 <- terra::rotate(MIROC45b30)
#Pull  vals
MIROC45HUM30<-data.frame(terra::extract(MIROC45b30,allcoords1))
MIROC45HUM30$HUM30 <- rowMeans(MIROC45HUM30[,2:366])
rownames(MIROC45HUM30) <- allcoords1$PlotCN
rm(MIROC45b30)
#HUM35
MIROC45b35 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2035.nc", subds="hurs")
MIROC45b35 <- terra::rotate(MIROC45b35)
#Pull  vals
MIROC45HUM35<-data.frame(terra::extract(MIROC45b35,allcoords1))
MIROC45HUM35$HUM35 <- rowMeans(MIROC45HUM35[,2:366])
rownames(MIROC45HUM35) <- allcoords1$PlotCN
rm(MIROC45b35)
#HUM40
MIROC45b40 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2040.nc", subds="hurs")
MIROC45b40 <- terra::rotate(MIROC45b40)
#Pull  vals
MIROC45HUM40<-data.frame(terra::extract(MIROC45b40,allcoords1))
MIROC45HUM40$HUM40 <- rowMeans(MIROC45HUM40[,2:366])
rownames(MIROC45HUM40) <- allcoords1$PlotCN
rm(MIROC45b40)
#HUM45
MIROC45b45 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2045.nc", subds="hurs")
MIROC45b45 <- terra::rotate(MIROC45b45)
#Pull  vals
MIROC45HUM45<-data.frame(terra::extract(MIROC45b45,allcoords1))
MIROC45HUM45$HUM45 <- rowMeans(MIROC45HUM45[,2:366])
rownames(MIROC45HUM45) <- allcoords1$PlotCN
rm(MIROC45b45)

#HUM50
MIROC45b50 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2050.nc", subds="hurs")
MIROC45b50 <- terra::rotate(MIROC45b50)
#Pull  vals
MIROC45HUM50<-data.frame(terra::extract(MIROC45b50,allcoords1))
MIROC45HUM50$HUM50 <- rowMeans(MIROC45HUM50[,2:366])
rownames(MIROC45HUM50) <- allcoords1$PlotCN
rm(MIROC45b50)
#HUM55
MIROC45b55 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2055.nc", subds="hurs")
MIROC45b55 <- terra::rotate(MIROC45b55)
#Pull  vals
MIROC45HUM55<-data.frame(terra::extract(MIROC45b55,allcoords1))
MIROC45HUM55$HUM55 <- rowMeans(MIROC45HUM55[,2:366])
rownames(MIROC45HUM55) <- allcoords1$PlotCN
rm(MIROC45b55)
#HUM60
MIROC45b60 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2060.nc", subds="hurs")
MIROC45b60 <- terra::rotate(MIROC45b60)
#Pull  vals
MIROC45HUM60<-data.frame(terra::extract(MIROC45b60,allcoords1))
MIROC45HUM60$HUM60 <- rowMeans(MIROC45HUM60[,2:366])
rownames(MIROC45HUM60) <- allcoords1$PlotCN
rm(MIROC45b60)
#HUM65
MIROC45b65 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2065.nc", subds="hurs")
MIROC45b65 <- terra::rotate(MIROC45b65)
#Pull  vals
MIROC45HUM65<-data.frame(terra::extract(MIROC45b65,allcoords1))
MIROC45HUM65$HUM65 <- rowMeans(MIROC45HUM65[,2:366])
rownames(MIROC45HUM65) <- allcoords1$PlotCN
rm(MIROC45b65)
#HUM70
MIROC45b70 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2070.nc", subds="hurs")
MIROC45b70 <- terra::rotate(MIROC45b70)
#Pull  vals
MIROC45HUM70<-data.frame(terra::extract(MIROC45b70,allcoords1))
MIROC45HUM70$HUM70 <- rowMeans(MIROC45HUM70[,2:366])
rownames(MIROC45HUM70) <- allcoords1$PlotCN
rm(MIROC45b70)
#HUM75
MIROC45b75 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2075.nc", subds="hurs")
MIROC45b75 <- terra::rotate(MIROC45b75)
#Pull  vals
MIROC45HUM75<-data.frame(terra::extract(MIROC45b75,allcoords1))
MIROC45HUM75$HUM75 <- rowMeans(MIROC45HUM75[,2:366])
rownames(MIROC45HUM75) <- allcoords1$PlotCN
rm(MIROC45b75)
#HUM80
MIROC45b80 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45Hurs2080.nc", subds="hurs")
MIROC45b80 <- terra::rotate(MIROC45b80)
#Pull  vals
MIROC45HUM80<-data.frame(terra::extract(MIROC45b80,allcoords1))
MIROC45HUM80$HUM80 <- rowMeans(MIROC45HUM80[,2:366])
#convert kg/m2/s -> to mm
MIROC45HUM80$HUM80 <- MIROC45HUM80$HUM80
rownames(MIROC45HUM80) <- allcoords1$PlotCN
rm(MIROC45b80)

MIROC45HUMFull<- data.frame(cbind(MIROC45HUM25$HUM25,MIROC45HUM30$HUM30,MIROC45HUM35$HUM35,MIROC45HUM40$HUM40, MIROC45HUM45$HUM45,
                                  MIROC45HUM50$HUM50, MIROC45HUM55$HUM55, MIROC45HUM60$HUM60, MIROC45HUM65$HUM65, MIROC45HUM70$HUM70, MIROC45HUM75$HUM75, MIROC45HUM80$HUM80))

colnames(MIROC45HUMFull)<- c("HUM25","HUM30","HUM35","HUM40", "HUM45","HUM50", "HUM55", "HUM60", "HUM65", "HUM70", "HUM75", "HUM80")

MIROC45HUM1519 <- as.data.frame(rbind(MIROCHUM2015[,c(368,367)], MIROCHUM2016[,c(369,368)], MIROCHUM2017[,c(368,367)], MIROCHUM2018[,c(368,367)],
                                      MIROCHUM2019[,c(368,367)]))


rm(MIROC45HUM25,MIROC45HUM30,MIROC45HUM35,MIROC45HUM40, MIROC45HUM45,
   MIROC45HUM50, MIROC45HUM55, MIROC45HUM60, MIROC45HUM65, MIROC45HUM70, MIROC45HUM75, MIROC45HUM80)
rownames(MIROC45HUMFull) <- allcoords1$PlotCN
#RAD
#2015
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2015.nc", subds="rsds")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCRAD2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCRAD2015$AvgRAD <- rowMeans(MIROCRAD2015[,2:366])
#Add PlotCN
MIROCRAD2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2016.nc", subds="rsds")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCRAD2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCRAD2016$AvgRAD <- rowMeans(MIROCRAD2016[,2:367])
#Add PlotCN
MIROCRAD2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2017.nc", subds="rsds")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCRAD2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCRAD2017$AvgRAD <- rowMeans(MIROCRAD2017[,2:366])
#Add PlotCN
MIROCRAD2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2018.nc", subds="rsds")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCRAD2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCRAD2018$AvgRAD <- rowMeans(MIROCRAD2018[,2:366])
#Add PlotCN
MIROCRAD2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC45")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2019.nc", subds="rsds")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCRAD2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCRAD2019$AvgRAD <- rowMeans(MIROCRAD2019[,2:366])
#Add PlotCN
MIROCRAD2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(MIROC2019)

#RAD25
MIROC45b25 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2025.nc", subds="rsds")
MIROC45b25 <- terra::rotate(MIROC45b25)
#Pull  vals
MIROC45RAD25<-data.frame(terra::extract(MIROC45b25,allcoords1))
MIROC45RAD25$RAD25 <- rowMeans(MIROC45RAD25[,2:366])
rownames(MIROC45RAD25) <- allcoords1$PlotCN
rm(MIROC45b25)
#RAD30
MIROC45b30 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2030.nc", subds="rsds")
MIROC45b30 <- terra::rotate(MIROC45b30)
#Pull  vals
MIROC45RAD30<-data.frame(terra::extract(MIROC45b30,allcoords1))
MIROC45RAD30$RAD30 <- rowMeans(MIROC45RAD30[,2:366])
rownames(MIROC45RAD30) <- allcoords1$PlotCN
rm(MIROC45b30)
#RAD35
MIROC45b35 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2035.nc", subds="rsds")
MIROC45b35 <- terra::rotate(MIROC45b35)
#Pull  vals
MIROC45RAD35<-data.frame(terra::extract(MIROC45b35,allcoords1))
MIROC45RAD35$RAD35 <- rowMeans(MIROC45RAD35[,2:366])
rownames(MIROC45RAD35) <- allcoords1$PlotCN
rm(MIROC45b35)
#RAD40
MIROC45b40 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2040.nc", subds="rsds")
MIROC45b40 <- terra::rotate(MIROC45b40)
#Pull  vals
MIROC45RAD40<-data.frame(terra::extract(MIROC45b40,allcoords1))
MIROC45RAD40$RAD40 <- rowMeans(MIROC45RAD40[,2:366])
rownames(MIROC45RAD40) <- allcoords1$PlotCN
rm(MIROC45b40)
#RAD45
MIROC45b45 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2045.nc", subds="rsds")
MIROC45b45 <- terra::rotate(MIROC45b45)
#Pull  vals
MIROC45RAD45<-data.frame(terra::extract(MIROC45b45,allcoords1))
MIROC45RAD45$RAD45 <- rowMeans(MIROC45RAD45[,2:366])
rownames(MIROC45RAD45) <- allcoords1$PlotCN
rm(MIROC45b45)

#RAD50
MIROC45b50 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2050.nc", subds="rsds")
MIROC45b50 <- terra::rotate(MIROC45b50)
#Pull  vals
MIROC45RAD50<-data.frame(terra::extract(MIROC45b50,allcoords1))
MIROC45RAD50$RAD50 <- rowMeans(MIROC45RAD50[,2:366])
rownames(MIROC45RAD50) <- allcoords1$PlotCN
rm(MIROC45b50)
#RAD55
MIROC45b55 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2055.nc", subds="rsds")
MIROC45b55 <- terra::rotate(MIROC45b55)
#Pull  vals
MIROC45RAD55<-data.frame(terra::extract(MIROC45b55,allcoords1))
MIROC45RAD55$RAD55 <- rowMeans(MIROC45RAD55[,2:366])
rownames(MIROC45RAD55) <- allcoords1$PlotCN
rm(MIROC45b55)
#RAD60
MIROC45b60 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2060.nc", subds="rsds")
MIROC45b60 <- terra::rotate(MIROC45b60)
#Pull  vals
MIROC45RAD60<-data.frame(terra::extract(MIROC45b60,allcoords1))
MIROC45RAD60$RAD60 <- rowMeans(MIROC45RAD60[,2:366])
rownames(MIROC45RAD60) <- allcoords1$PlotCN
rm(MIROC45b60)
#RAD65
MIROC45b65 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2065.nc", subds="rsds")
MIROC45b65 <- terra::rotate(MIROC45b65)
#Pull  vals
MIROC45RAD65<-data.frame(terra::extract(MIROC45b65,allcoords1))
MIROC45RAD65$RAD65 <- rowMeans(MIROC45RAD65[,2:366])
rownames(MIROC45RAD65) <- allcoords1$PlotCN
rm(MIROC45b65)
#RAD70
MIROC45b70 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2070.nc", subds="rsds")
MIROC45b70 <- terra::rotate(MIROC45b70)
#Pull  vals
MIROC45RAD70<-data.frame(terra::extract(MIROC45b70,allcoords1))
MIROC45RAD70$RAD70 <- rowMeans(MIROC45RAD70[,2:366])
rownames(MIROC45RAD70) <- allcoords1$PlotCN
rm(MIROC45b70)
#RAD75
MIROC45b75 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2075.nc", subds="rsds")
MIROC45b75 <- terra::rotate(MIROC45b75)
#Pull  vals
MIROC45RAD75<-data.frame(terra::extract(MIROC45b75,allcoords1))
MIROC45RAD75$RAD75 <- rowMeans(MIROC45RAD75[,2:366])
rownames(MIROC45RAD75) <- allcoords1$PlotCN
rm(MIROC45b75)
#RAD80
MIROC45b80 <- rast("~/Desktop/NASAClim/MIROC45/MIROC45RAD2080.nc", subds="rsds")
MIROC45b80 <- terra::rotate(MIROC45b80)
#Pull  vals
MIROC45RAD80<-data.frame(terra::extract(MIROC45b80,allcoords1))
MIROC45RAD80$RAD80 <- rowMeans(MIROC45RAD80[,2:366])
#convert kg/m2/s -> to mm
MIROC45RAD80$RAD80 <- MIROC45RAD80$RAD80
rownames(MIROC45RAD80) <- allcoords1$PlotCN
rm(MIROC45b80)

MIROC45RADFull<- data.frame(cbind(MIROC45RAD25$RAD25,MIROC45RAD30$RAD30,MIROC45RAD35$RAD35,MIROC45RAD40$RAD40, MIROC45RAD45$RAD45,
                                  MIROC45RAD50$RAD50, MIROC45RAD55$RAD55, MIROC45RAD60$RAD60, MIROC45RAD65$RAD65, MIROC45RAD70$RAD70, MIROC45RAD75$RAD75, MIROC45RAD80$RAD80))

colnames(MIROC45RADFull)<- c("RAD25","RAD30","RAD35","RAD40", "RAD45","RAD50", "RAD55", "RAD60", "RAD65", "RAD70", "RAD75", "RAD80")

MIROC45RAD1519 <- as.data.frame(rbind(MIROCRAD2015[,c(368,367)], MIROCRAD2016[,c(369,368)], MIROCRAD2017[,c(368,367)], MIROCRAD2018[,c(368,367)],
                                      MIROCRAD2019[,c(368,367)]))


rm(MIROC45RAD25,MIROC45RAD30,MIROC45RAD35,MIROC45RAD40, MIROC45RAD45,
   MIROC45RAD50, MIROC45RAD55, MIROC45RAD60, MIROC45RAD65, MIROC45RAD70, MIROC45RAD75, MIROC45RAD80)
rownames(MIROC45RADFull) <- allcoords1$PlotCN

#MIROC85
#Temp
#2015
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2015.nc", subds="tas")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCTEMP2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCTEMP2015$AvgTemp <- rowMeans(MIROCTEMP2015[,2:366])
#Add PlotCN
MIROCTEMP2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert K  to C
MIROCTEMP2015$AvgTemp <- MIROCTEMP2015$AvgTemp-273.15
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2016.nc", subds="tas")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCTEMP2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCTEMP2016$AvgTemp <- rowMeans(MIROCTEMP2016[,2:367])
#Add PlotCN
MIROCTEMP2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert K  to C
MIROCTEMP2016$AvgTemp <- MIROCTEMP2016$AvgTemp-273.15
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2017.nc", subds="tas")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCTEMP2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCTEMP2017$AvgTemp <- rowMeans(MIROCTEMP2017[,2:366])
#Add PlotCN
MIROCTEMP2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert K  to C
MIROCTEMP2017$AvgTemp <- MIROCTEMP2017$AvgTemp-273.15
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2018.nc", subds="tas")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCTEMP2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCTEMP2018$AvgTemp <- rowMeans(MIROCTEMP2018[,2:366])
#Add PlotCN
MIROCTEMP2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert K  to C
MIROCTEMP2018$AvgTemp <- MIROCTEMP2018$AvgTemp-273.15
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2019.nc", subds="tas")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCTEMP2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCTEMP2019$AvgTemp <- rowMeans(MIROCTEMP2019[,2:366])
#Add PlotCN
MIROCTEMP2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert K  to C
MIROCTEMP2019$AvgTemp <- MIROCTEMP2019$AvgTemp-273.15
rm(MIROC2019)

#Temp25
MIROC85b25 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2025.nc", subds="tas")
MIROC85b25 <- terra::rotate(MIROC85b25)
#Pull  vals
MIROC85TEMP25<-data.frame(terra::extract(MIROC85b25,allcoords1))
MIROC85TEMP25$Temp25 <- rowMeans(MIROC85TEMP25[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP25$Temp25 <- MIROC85TEMP25$Temp25-273.15
rownames(MIROC85TEMP25) <- allcoords1$PlotCN
rm(MIROC85b25)
#Temp30
MIROC85b30 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2030.nc", subds="tas")
MIROC85b30 <- terra::rotate(MIROC85b30)
#Pull  vals
MIROC85TEMP30<-data.frame(terra::extract(MIROC85b30,allcoords1))
MIROC85TEMP30$Temp30 <- rowMeans(MIROC85TEMP30[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP30$Temp30 <- MIROC85TEMP30$Temp30-273.15
rownames(MIROC85TEMP30) <- allcoords1$PlotCN
rm(MIROC85b30)
#Temp35
MIROC85b35 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2035.nc", subds="tas")
MIROC85b35 <- terra::rotate(MIROC85b35)
#Pull  vals
MIROC85TEMP35<-data.frame(terra::extract(MIROC85b35,allcoords1))
MIROC85TEMP35$Temp35 <- rowMeans(MIROC85TEMP35[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP35$Temp35 <- MIROC85TEMP35$Temp35-273.15
rownames(MIROC85TEMP35) <- allcoords1$PlotCN
rm(MIROC85b35)
#Temp40
MIROC85b40 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2040.nc", subds="tas")
MIROC85b40 <- terra::rotate(MIROC85b40)
#Pull  vals
MIROC85TEMP40<-data.frame(terra::extract(MIROC85b40,allcoords1))
MIROC85TEMP40$Temp40 <- rowMeans(MIROC85TEMP40[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP40$Temp40 <- MIROC85TEMP40$Temp40-273.15
rownames(MIROC85TEMP40) <- allcoords1$PlotCN
rm(MIROC85b40)
#Temp45
MIROC85b45 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2045.nc", subds="tas")
MIROC85b45 <- terra::rotate(MIROC85b45)
#Pull  vals
MIROC85TEMP45<-data.frame(terra::extract(MIROC85b45,allcoords1))
MIROC85TEMP45$Temp45 <- rowMeans(MIROC85TEMP45[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP45$Temp45 <- MIROC85TEMP45$Temp45-273.15
rownames(MIROC85TEMP45) <- allcoords1$PlotCN
rm(MIROC85b45)

#Temp50
MIROC85b50 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2050.nc", subds="tas")
MIROC85b50 <- terra::rotate(MIROC85b50)
#Pull  vals
MIROC85TEMP50<-data.frame(terra::extract(MIROC85b50,allcoords1))
MIROC85TEMP50$Temp50 <- rowMeans(MIROC85TEMP50[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP50$Temp50 <- MIROC85TEMP50$Temp50-273.15
rownames(MIROC85TEMP50) <- allcoords1$PlotCN
rm(MIROC85b50)
#Temp55
MIROC85b55 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2055.nc", subds="tas")
MIROC85b55 <- terra::rotate(MIROC85b55)
#Pull  vals
MIROC85TEMP55<-data.frame(terra::extract(MIROC85b55,allcoords1))
MIROC85TEMP55$Temp55 <- rowMeans(MIROC85TEMP55[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP55$Temp55 <- MIROC85TEMP55$Temp55-273.15
rownames(MIROC85TEMP55) <- allcoords1$PlotCN
rm(MIROC85b55)
#Temp60
MIROC85b60 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2060.nc", subds="tas")
MIROC85b60 <- terra::rotate(MIROC85b60)
#Pull  vals
MIROC85TEMP60<-data.frame(terra::extract(MIROC85b60,allcoords1))
MIROC85TEMP60$Temp60 <- rowMeans(MIROC85TEMP60[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP60$Temp60 <- MIROC85TEMP60$Temp60-273.15
rownames(MIROC85TEMP60) <- allcoords1$PlotCN
rm(MIROC85b60)
#Temp65
MIROC85b65 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2065.nc", subds="tas")
MIROC85b65 <- terra::rotate(MIROC85b65)
#Pull  vals
MIROC85TEMP65<-data.frame(terra::extract(MIROC85b65,allcoords1))
MIROC85TEMP65$Temp65 <- rowMeans(MIROC85TEMP65[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP65$Temp65 <- MIROC85TEMP65$Temp65-273.15
rownames(MIROC85TEMP65) <- allcoords1$PlotCN
rm(MIROC85b65)
#Temp70
MIROC85b70 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2070.nc", subds="tas")
MIROC85b70 <- terra::rotate(MIROC85b70)
#Pull  vals
MIROC85TEMP70<-data.frame(terra::extract(MIROC85b70,allcoords1))
MIROC85TEMP70$Temp70 <- rowMeans(MIROC85TEMP70[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP70$Temp70 <- MIROC85TEMP70$Temp70-273.15
rownames(MIROC85TEMP70) <- allcoords1$PlotCN
rm(MIROC85b70)
#Temp75
MIROC85b75 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2075.nc", subds="tas")
MIROC85b75 <- terra::rotate(MIROC85b75)
#Pull  vals
MIROC85TEMP75<-data.frame(terra::extract(MIROC85b75,allcoords1))
MIROC85TEMP75$Temp75 <- rowMeans(MIROC85TEMP75[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP75$Temp75 <- MIROC85TEMP75$Temp75-273.15
rownames(MIROC85TEMP75) <- allcoords1$PlotCN
rm(MIROC85b75)
#Temp80
MIROC85b80 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Ta2080.nc", subds="tas")
MIROC85b80 <- terra::rotate(MIROC85b80)
#Pull  vals
MIROC85TEMP80<-data.frame(terra::extract(MIROC85b80,allcoords1))
MIROC85TEMP80$Temp80 <- rowMeans(MIROC85TEMP80[,2:366])
#convert kg/m2/s -> to mm
MIROC85TEMP80$Temp80 <- MIROC85TEMP80$Temp80-273.15
rownames(MIROC85TEMP80) <- allcoords1$PlotCN
rm(MIROC85b80)

MIROC85TempFull<- data.frame(cbind(MIROC85TEMP25$Temp25,MIROC85TEMP30$Temp30,MIROC85TEMP35$Temp35,MIROC85TEMP40$Temp40, MIROC85TEMP45$Temp45,
                                   MIROC85TEMP50$Temp50, MIROC85TEMP55$Temp55, MIROC85TEMP60$Temp60, MIROC85TEMP65$Temp65, MIROC85TEMP70$Temp70, MIROC85TEMP75$Temp75, MIROC85TEMP80$Temp80))

colnames(MIROC85TempFull)<- c("Temp25","Temp30","Temp35","Temp40", "Temp45","Temp50", "Temp55", "Temp60", "Temp65", "Temp70", "Temp75", "Temp80")


MIROC85TEMP1519 <- as.data.frame(rbind(MIROCTEMP2015[,c(368,367)], MIROCTEMP2016[,c(369,368)], MIROCTEMP2017[,c(368,367)], MIROCTEMP2018[,c(368,367)],
                                       MIROCTEMP2019[,c(368,367)]))


rm(MIROC85TEMP25,MIROC85TEMP30,MIROC85TEMP35,MIROC85TEMP40, MIROC85TEMP45,
   MIROC85TEMP50, MIROC85TEMP55, MIROC85TEMP60, MIROC85TEMP65, MIROC85TEMP70, MIROC85TEMP75, MIROC85TEMP80)
rownames(MIROC85TempFull) <- allcoords1$PlotCN

#Prec
#2015
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2015.nc", subds="pr")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCPREC2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCPREC2015$PrecSum <- rowSums(MIROCPrec2015[,2:366])
#Add PlotCN
MIROCPREC2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert to mm
MIROCPREC2015$PrecSum <- MIROCPREC2015$PrecSum*86400
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2016.nc", subds="pr")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCPREC2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCPREC2016$PrecSum <- rowSums(MIROCPrec2016[,2:367])
#Add PlotCN
MIROCPREC2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert to mm
MIROCPREC2016$PrecSum <- MIROCPREC2016$PrecSum*86400
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2017.nc", subds="pr")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCPREC2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCPREC2017$PrecSum <- rowSums(MIROCPrec2017[,2:366])
#Add PlotCN
MIROCPREC2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert to mm
MIROCPREC2017$PrecSum <- MIROCPREC2017$PrecSum*86400
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2018.nc", subds="pr")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCPREC2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCPREC2018$PrecSum <- rowSums(MIROCPrec2018[,2:366])
#Add PlotCN
MIROCPREC2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert to mm
MIROCPREC2018$PrecSum <- MIROCPREC2018$PrecSum*86400
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2019.nc", subds="pr")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCPREC2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCPREC2019$PrecSum <- rowSums(MIROCPrec2019[,2:366])
#Add PlotCN
MIROCPREC2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert to mm
MIROCPREC2019$PrecSum <- MIROCPREC2019$PrecSum*86400
rm(MIROC2019)

#Prec25
MIROC85b25 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2025.nc", subds="pr")
MIROC85b25 <- terra::rotate(MIROC85b25)
#Pull  vals
MIROC85PREC25<-data.frame(terra::extract(MIROC85b25,allcoords1))
MIROC85PREC25$Prec25 <- rowSums(MIROC85Prec25[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC25$Prec25 <- MIROC85PREC25$Prec25*86400
rownames(MIROC85PREC25) <- allcoords1$PlotCN
rm(MIROC85b25)
#Prec30
MIROC85b30 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2030.nc", subds="pr")
MIROC85b30 <- terra::rotate(MIROC85b30)
#Pull  vals
MIROC85PREC30<-data.frame(terra::extract(MIROC85b30,allcoords1))
MIROC85PREC30$Prec30 <- rowSums(MIROC85Prec30[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC30$Prec30 <- MIROC85PREC30$Prec30*86400
rownames(MIROC85PREC30) <- allcoords1$PlotCN
rm(MIROC85b30)
#Prec35
MIROC85b35 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2035.nc", subds="pr")
MIROC85b35 <- terra::rotate(MIROC85b35)
#Pull  vals
MIROC85PREC35<-data.frame(terra::extract(MIROC85b35,allcoords1))
MIROC85PREC35$Prec35 <- rowSums(MIROC85Prec35[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC35$Prec35 <- MIROC85PREC35$Prec35*86400
rownames(MIROC85PREC35) <- allcoords1$PlotCN
rm(MIROC85b35)
#Prec40
MIROC85b40 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2040.nc", subds="pr")
MIROC85b40 <- terra::rotate(MIROC85b40)
#Pull  vals
MIROC85PREC40<-data.frame(terra::extract(MIROC85b40,allcoords1))
MIROC85PREC40$Prec40 <- rowSums(MIROC85Prec40[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC40$Prec40 <- MIROC85PREC40$Prec40*86400
rownames(MIROC85PREC40) <- allcoords1$PlotCN
rm(MIROC85b40)
#Prec45
MIROC85b45 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2045.nc", subds="pr")
MIROC85b45 <- terra::rotate(MIROC85b45)
#Pull  vals
MIROC85PREC45<-data.frame(terra::extract(MIROC85b45,allcoords1))
MIROC85PREC45$Prec45 <- rowSums(MIROC85Prec45[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC45$Prec45 <- MIROC85PREC45$Prec45*86400
rownames(MIROC85PREC45) <- allcoords1$PlotCN
rm(MIROC85b45)

#Prec50
MIROC85b50 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2050.nc", subds="pr")
MIROC85b50 <- terra::rotate(MIROC85b50)
#Pull  vals
MIROC85PREC50<-data.frame(terra::extract(MIROC85b50,allcoords1))
MIROC85PREC50$Prec50 <- rowSums(MIROC85Prec50[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC50$Prec50 <- MIROC85PREC50$Prec50*86400
rownames(MIROC85PREC50) <- allcoords1$PlotCN
rm(MIROC85b50)
#Prec55
MIROC85b55 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2055.nc", subds="pr")
MIROC85b55 <- terra::rotate(MIROC85b55)
#Pull  vals
MIROC85PREC55<-data.frame(terra::extract(MIROC85b55,allcoords1))
MIROC85PREC55$Prec55 <- rowSums(MIROC85Prec55[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC55$Prec55 <- MIROC85PREC55$Prec55*86400
rownames(MIROC85PREC55) <- allcoords1$PlotCN
rm(MIROC85b55)
#Prec60
MIROC85b60 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2060.nc", subds="pr")
MIROC85b60 <- terra::rotate(MIROC85b60)
#Pull  vals
MIROC85PREC60<-data.frame(terra::extract(MIROC85b60,allcoords1))
MIROC85PREC60$Prec60 <- rowSums(MIROC85Prec60[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC60$Prec60 <- MIROC85PREC60$Prec60*86400
rownames(MIROC85PREC60) <- allcoords1$PlotCN
rm(MIROC85b60)
#Prec65
MIROC85b65 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2065.nc", subds="pr")
MIROC85b65 <- terra::rotate(MIROC85b65)
#Pull  vals
MIROC85PREC65<-data.frame(terra::extract(MIROC85b65,allcoords1))
MIROC85PREC65$Prec65 <- rowSums(MIROC85Prec65[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC65$Prec65 <- MIROC85PREC65$Prec65*86400
rownames(MIROC85PREC65) <- allcoords1$PlotCN
rm(MIROC85b65)
#Prec70
MIROC85b70 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2070.nc", subds="pr")
MIROC85b70 <- terra::rotate(MIROC85b70)
#Pull  vals
MIROC85PREC70<-data.frame(terra::extract(MIROC85b70,allcoords1))
MIROC85PREC70$Prec70 <- rowSums(MIROC85Prec70[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC70$Prec70 <- MIROC85PREC70$Prec70*86400
rownames(MIROC85PREC70) <- allcoords1$PlotCN
rm(MIROC85b70)
#Prec75
MIROC85b75 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2075.nc", subds="pr")
MIROC85b75 <- terra::rotate(MIROC85b75)
#Pull  vals
MIROC85PREC75<-data.frame(terra::extract(MIROC85b75,allcoords1))
MIROC85PREC75$Prec75 <- rowSums(MIROC85Prec75[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC75$Prec75 <- MIROC85PREC75$Prec75*86400
rownames(MIROC85PREC75) <- allcoords1$PlotCN
rm(MIROC85b75)
#Prec80
MIROC85b80 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Pr2080.nc", subds="pr")
MIROC85b80 <- terra::rotate(MIROC85b80)
#Pull  vals
MIROC85PREC80<-data.frame(terra::extract(MIROC85b80,allcoords1))
MIROC85PREC80$Prec80 <- rowSums(MIROC85Prec80[,2:366])
#convert kg/m2/s -> to mm
MIROC85PREC80$Prec80 <- MIROC85PREC80$Prec80*86400
rownames(MIROC85PREC80) <- allcoords1$PlotCN
rm(MIROC85b80)

MIROC85PrecFull<- data.frame(cbind(MIROC85PREC25$Prec25,MIROC85PREC30$Prec30,MIROC85PREC35$Prec35,MIROC85PREC40$Prec40, MIROC85PREC45$Prec45,
                                   MIROC85PREC50$Prec50, MIROC85PREC55$Prec55, MIROC85PREC60$Prec60, MIROC85PREC65$Prec65, MIROC85PREC70$Prec70, MIROC85PREC75$Prec75, MIROC85PREC80$Prec80))

colnames(MIROC85PrecFull)<- c("Prec25","Prec30","Prec35","Prec40", "Prec45","Prec50", "Prec55", "Prec60", "Prec65", "Prec70", "Prec75", "Prec80")

MIROC85PREC1519 <- as.data.frame(rbind(MIROCPREC2015[,c(368,367)], MIROCPREC2016[,c(369,368)], MIROCPREC2017[,c(368,367)], MIROCPREC2018[,c(368,367)],
                                       MIROCPREC2019[,c(368,367)]))


rm(MIROC85PREC25,MIROC85PREC30,MIROC85PREC35,MIROC85PREC40, MIROC85PREC45,
   MIROC85PREC50, MIROC85PREC55, MIROC85PREC60, MIROC85PREC65, MIROC85PREC70, MIROC85PREC75, MIROC85PREC80)
rownames(MIROC85PrecFull) <- allcoords1$PlotCN

#HUM
#2015
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2015.nc", subds="hurs")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCHUM2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCHUM2015$AvgHUM <- rowMeans(MIROCHUM2015[,2:366])
#Add PlotCN
MIROCHUM2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2016.nc", subds="hurs")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCHUM2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCHUM2016$AvgHUM <- rowMeans(MIROCHUM2016[,2:367])
#Add PlotCN
MIROCHUM2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2017.nc", subds="hurs")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCHUM2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCHUM2017$AvgHUM <- rowMeans(MIROCHUM2017[,2:366])
#Add PlotCN
MIROCHUM2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2018.nc", subds="hurs")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCHUM2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCHUM2018$AvgHUM <- rowMeans(MIROCHUM2018[,2:366])
#Add PlotCN
MIROCHUM2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2019.nc", subds="hurs")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCHUM2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCHUM2019$AvgHUM <- rowMeans(MIROCHUM2019[,2:366])
#Add PlotCN
MIROCHUM2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(MIROC2019)

#HUM25
MIROC85b25 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2025.nc", subds="hurs")
MIROC85b25 <- terra::rotate(MIROC85b25)
#Pull  vals
MIROC85HUM25<-data.frame(terra::extract(MIROC85b25,allcoords1))
MIROC85HUM25$HUM25 <- rowMeans(MIROC85HUM25[,2:366])
rownames(MIROC85HUM25) <- allcoords1$PlotCN
rm(MIROC85b25)
#HUM30
MIROC85b30 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2030.nc", subds="hurs")
MIROC85b30 <- terra::rotate(MIROC85b30)
#Pull  vals
MIROC85HUM30<-data.frame(terra::extract(MIROC85b30,allcoords1))
MIROC85HUM30$HUM30 <- rowMeans(MIROC85HUM30[,2:366])
rownames(MIROC85HUM30) <- allcoords1$PlotCN
rm(MIROC85b30)
#HUM35
MIROC85b35 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2035.nc", subds="hurs")
MIROC85b35 <- terra::rotate(MIROC85b35)
#Pull  vals
MIROC85HUM35<-data.frame(terra::extract(MIROC85b35,allcoords1))
MIROC85HUM35$HUM35 <- rowMeans(MIROC85HUM35[,2:366])
rownames(MIROC85HUM35) <- allcoords1$PlotCN
rm(MIROC85b35)
#HUM40
MIROC85b40 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2040.nc", subds="hurs")
MIROC85b40 <- terra::rotate(MIROC85b40)
#Pull  vals
MIROC85HUM40<-data.frame(terra::extract(MIROC85b40,allcoords1))
MIROC85HUM40$HUM40 <- rowMeans(MIROC85HUM40[,2:366])
rownames(MIROC85HUM40) <- allcoords1$PlotCN
rm(MIROC85b40)
#HUM45
MIROC85b45 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2045.nc", subds="hurs")
MIROC85b45 <- terra::rotate(MIROC85b45)
#Pull  vals
MIROC85HUM45<-data.frame(terra::extract(MIROC85b45,allcoords1))
MIROC85HUM45$HUM45 <- rowMeans(MIROC85HUM45[,2:366])
rownames(MIROC85HUM45) <- allcoords1$PlotCN
rm(MIROC85b45)

#HUM50
MIROC85b50 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2050.nc", subds="hurs")
MIROC85b50 <- terra::rotate(MIROC85b50)
#Pull  vals
MIROC85HUM50<-data.frame(terra::extract(MIROC85b50,allcoords1))
MIROC85HUM50$HUM50 <- rowMeans(MIROC85HUM50[,2:366])
rownames(MIROC85HUM50) <- allcoords1$PlotCN
rm(MIROC85b50)
#HUM55
MIROC85b55 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2055.nc", subds="hurs")
MIROC85b55 <- terra::rotate(MIROC85b55)
#Pull  vals
MIROC85HUM55<-data.frame(terra::extract(MIROC85b55,allcoords1))
MIROC85HUM55$HUM55 <- rowMeans(MIROC85HUM55[,2:366])
rownames(MIROC85HUM55) <- allcoords1$PlotCN
rm(MIROC85b55)
#HUM60
MIROC85b60 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2060.nc", subds="hurs")
MIROC85b60 <- terra::rotate(MIROC85b60)
#Pull  vals
MIROC85HUM60<-data.frame(terra::extract(MIROC85b60,allcoords1))
MIROC85HUM60$HUM60 <- rowMeans(MIROC85HUM60[,2:366])
rownames(MIROC85HUM60) <- allcoords1$PlotCN
rm(MIROC85b60)
#HUM65
MIROC85b65 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2065.nc", subds="hurs")
MIROC85b65 <- terra::rotate(MIROC85b65)
#Pull  vals
MIROC85HUM65<-data.frame(terra::extract(MIROC85b65,allcoords1))
MIROC85HUM65$HUM65 <- rowMeans(MIROC85HUM65[,2:366])
rownames(MIROC85HUM65) <- allcoords1$PlotCN
rm(MIROC85b65)
#HUM70
MIROC85b70 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2070.nc", subds="hurs")
MIROC85b70 <- terra::rotate(MIROC85b70)
#Pull  vals
MIROC85HUM70<-data.frame(terra::extract(MIROC85b70,allcoords1))
MIROC85HUM70$HUM70 <- rowMeans(MIROC85HUM70[,2:366])
rownames(MIROC85HUM70) <- allcoords1$PlotCN
rm(MIROC85b70)
#HUM75
MIROC85b75 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2075.nc", subds="hurs")
MIROC85b75 <- terra::rotate(MIROC85b75)
#Pull  vals
MIROC85HUM75<-data.frame(terra::extract(MIROC85b75,allcoords1))
MIROC85HUM75$HUM75 <- rowMeans(MIROC85HUM75[,2:366])
rownames(MIROC85HUM75) <- allcoords1$PlotCN
rm(MIROC85b75)
#HUM80
MIROC85b80 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85Hurs2080.nc", subds="hurs")
MIROC85b80 <- terra::rotate(MIROC85b80)
#Pull  vals
MIROC85HUM80<-data.frame(terra::extract(MIROC85b80,allcoords1))
MIROC85HUM80$HUM80 <- rowMeans(MIROC85HUM80[,2:366])
#convert kg/m2/s -> to mm
MIROC85HUM80$HUM80 <- MIROC85HUM80$HUM80
rownames(MIROC85HUM80) <- allcoords1$PlotCN
rm(MIROC85b80)

MIROC85HUMFull<- data.frame(cbind(MIROC85HUM25$HUM25,MIROC85HUM30$HUM30,MIROC85HUM35$HUM35,MIROC85HUM40$HUM40, MIROC85HUM45$HUM45,
                                  MIROC85HUM50$HUM50, MIROC85HUM55$HUM55, MIROC85HUM60$HUM60, MIROC85HUM65$HUM65, MIROC85HUM70$HUM70, MIROC85HUM75$HUM75, MIROC85HUM80$HUM80))

colnames(MIROC85HUMFull)<- c("HUM25","HUM30","HUM35","HUM40", "HUM45","HUM50", "HUM55", "HUM60", "HUM65", "HUM70", "HUM75", "HUM80")


MIROC85HUM1519 <- as.data.frame(rbind(MIROCHUM2015[,c(368,367)], MIROCHUM2016[,c(369,368)], MIROCHUM2017[,c(368,367)], MIROCHUM2018[,c(368,367)],
                                      MIROCHUM2019[,c(368,367)]))


rm(MIROC85HUM25,MIROC85HUM30,MIROC85HUM35,MIROC85HUM40, MIROC85HUM45,
   MIROC85HUM50, MIROC85HUM55, MIROC85HUM60, MIROC85HUM65, MIROC85HUM70, MIROC85HUM75, MIROC85HUM80)
rownames(MIROC85HUMFull) <- allcoords1$PlotCN
#RAD
#2015
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2015  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2015.nc", subds="rsds")
MIROC2015  <- terra::rotate(MIROC2015)
#Pull  vals
MIROCRAD2015<-data.frame(terra::extract(MIROC2015 ,allcoords[allcoords$Year == 2015,]))
MIROCRAD2015$AvgRAD <- rowMeans(MIROCRAD2015[,2:366])
#Add PlotCN
MIROCRAD2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(MIROC2015)
#2016
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2016  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2016.nc", subds="rsds")
MIROC2016  <- terra::rotate(MIROC2016)
#Pull  vals
MIROCRAD2016<-data.frame(terra::extract(MIROC2016 ,allcoords[allcoords$Year == 2016,]))
MIROCRAD2016$AvgRAD <- rowMeans(MIROCRAD2016[,2:367])
#Add PlotCN
MIROCRAD2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(MIROC2016)
#2017
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2017  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2017.nc", subds="rsds")
MIROC2017  <- terra::rotate(MIROC2017)
#Pull  vals
MIROCRAD2017<-data.frame(terra::extract(MIROC2017 ,allcoords[allcoords$Year == 2017,]))
MIROCRAD2017$AvgRAD <- rowMeans(MIROCRAD2017[,2:366])
#Add PlotCN
MIROCRAD2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(MIROC2017)
#2018
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2018  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2018.nc", subds="rsds")
MIROC2018  <- terra::rotate(MIROC2018)
#Pull  vals
MIROCRAD2018<-data.frame(terra::extract(MIROC2018 ,allcoords[allcoords$Year == 2018,]))
MIROCRAD2018$AvgRAD <- rowMeans(MIROCRAD2018[,2:366])
#Add PlotCN
MIROCRAD2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(MIROC2018)
#2019
setwd("~/Desktop/NASAClim/MIROC85")
MIROC2019  <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2019.nc", subds="rsds")
MIROC2019  <- terra::rotate(MIROC2019)
#Pull  vals
MIROCRAD2019<-data.frame(terra::extract(MIROC2019 ,allcoords[allcoords$Year == 2019,]))
MIROCRAD2019$AvgRAD <- rowMeans(MIROCRAD2019[,2:366])
#Add PlotCN
MIROCRAD2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(MIROC2019)

#RAD25
MIROC85b25 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2025.nc", subds="rsds")
MIROC85b25 <- terra::rotate(MIROC85b25)
#Pull  vals
MIROC85RAD25<-data.frame(terra::extract(MIROC85b25,allcoords1))
MIROC85RAD25$RAD25 <- rowMeans(MIROC85RAD25[,2:366])
rownames(MIROC85RAD25) <- allcoords1$PlotCN
rm(MIROC85b25)
#RAD30
MIROC85b30 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2030.nc", subds="rsds")
MIROC85b30 <- terra::rotate(MIROC85b30)
#Pull  vals
MIROC85RAD30<-data.frame(terra::extract(MIROC85b30,allcoords1))
MIROC85RAD30$RAD30 <- rowMeans(MIROC85RAD30[,2:366])
rownames(MIROC85RAD30) <- allcoords1$PlotCN
rm(MIROC85b30)
#RAD35
MIROC85b35 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2035.nc", subds="rsds")
MIROC85b35 <- terra::rotate(MIROC85b35)
#Pull  vals
MIROC85RAD35<-data.frame(terra::extract(MIROC85b35,allcoords1))
MIROC85RAD35$RAD35 <- rowMeans(MIROC85RAD35[,2:366])
rownames(MIROC85RAD35) <- allcoords1$PlotCN
rm(MIROC85b35)
#RAD40
MIROC85b40 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2040.nc", subds="rsds")
MIROC85b40 <- terra::rotate(MIROC85b40)
#Pull  vals
MIROC85RAD40<-data.frame(terra::extract(MIROC85b40,allcoords1))
MIROC85RAD40$RAD40 <- rowMeans(MIROC85RAD40[,2:366])
rownames(MIROC85RAD40) <- allcoords1$PlotCN
rm(MIROC85b40)
#RAD45
MIROC85b45 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2045.nc", subds="rsds")
MIROC85b45 <- terra::rotate(MIROC85b45)
#Pull  vals
MIROC85RAD45<-data.frame(terra::extract(MIROC85b45,allcoords1))
MIROC85RAD45$RAD45 <- rowMeans(MIROC85RAD45[,2:366])
rownames(MIROC85RAD45) <- allcoords1$PlotCN
rm(MIROC85b45)

#RAD50
MIROC85b50 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2050.nc", subds="rsds")
MIROC85b50 <- terra::rotate(MIROC85b50)
#Pull  vals
MIROC85RAD50<-data.frame(terra::extract(MIROC85b50,allcoords1))
MIROC85RAD50$RAD50 <- rowMeans(MIROC85RAD50[,2:366])
rownames(MIROC85RAD50) <- allcoords1$PlotCN
rm(MIROC85b50)
#RAD55
MIROC85b55 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2055.nc", subds="rsds")
MIROC85b55 <- terra::rotate(MIROC85b55)
#Pull  vals
MIROC85RAD55<-data.frame(terra::extract(MIROC85b55,allcoords1))
MIROC85RAD55$RAD55 <- rowMeans(MIROC85RAD55[,2:366])
rownames(MIROC85RAD55) <- allcoords1$PlotCN
rm(MIROC85b55)
#RAD60
MIROC85b60 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2060.nc", subds="rsds")
MIROC85b60 <- terra::rotate(MIROC85b60)
#Pull  vals
MIROC85RAD60<-data.frame(terra::extract(MIROC85b60,allcoords1))
MIROC85RAD60$RAD60 <- rowMeans(MIROC85RAD60[,2:366])
rownames(MIROC85RAD60) <- allcoords1$PlotCN
rm(MIROC85b60)
#RAD65
MIROC85b65 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2065.nc", subds="rsds")
MIROC85b65 <- terra::rotate(MIROC85b65)
#Pull  vals
MIROC85RAD65<-data.frame(terra::extract(MIROC85b65,allcoords1))
MIROC85RAD65$RAD65 <- rowMeans(MIROC85RAD65[,2:366])
rownames(MIROC85RAD65) <- allcoords1$PlotCN
rm(MIROC85b65)
#RAD70
MIROC85b70 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2070.nc", subds="rsds")
MIROC85b70 <- terra::rotate(MIROC85b70)
#Pull  vals
MIROC85RAD70<-data.frame(terra::extract(MIROC85b70,allcoords1))
MIROC85RAD70$RAD70 <- rowMeans(MIROC85RAD70[,2:366])
rownames(MIROC85RAD70) <- allcoords1$PlotCN
rm(MIROC85b70)
#RAD75
MIROC85b75 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2075.nc", subds="rsds")
MIROC85b75 <- terra::rotate(MIROC85b75)
#Pull  vals
MIROC85RAD75<-data.frame(terra::extract(MIROC85b75,allcoords1))
MIROC85RAD75$RAD75 <- rowMeans(MIROC85RAD75[,2:366])
rownames(MIROC85RAD75) <- allcoords1$PlotCN
rm(MIROC85b75)
#RAD80
MIROC85b80 <- rast("~/Desktop/NASAClim/MIROC85/MIROC85RAD2080.nc", subds="rsds")
MIROC85b80 <- terra::rotate(MIROC85b80)
#Pull  vals
MIROC85RAD80<-data.frame(terra::extract(MIROC85b80,allcoords1))
MIROC85RAD80$RAD80 <- rowMeans(MIROC85RAD80[,2:366])
#convert kg/m2/s -> to mm
MIROC85RAD80$RAD80 <- MIROC85RAD80$RAD80
rownames(MIROC85RAD80) <- allcoords1$PlotCN
rm(MIROC85b80)

MIROC85RADFull<- data.frame(cbind(MIROC85RAD25$RAD25,MIROC85RAD30$RAD30,MIROC85RAD35$RAD35,MIROC85RAD40$RAD40, MIROC85RAD45$RAD45,
                                  MIROC85RAD50$RAD50, MIROC85RAD55$RAD55, MIROC85RAD60$RAD60, MIROC85RAD65$RAD65, MIROC85RAD70$RAD70, MIROC85RAD75$RAD75, MIROC85RAD80$RAD80))

colnames(MIROC85RADFull)<- c("RAD25","RAD30","RAD35","RAD40", "RAD45","RAD50", "RAD55", "RAD60", "RAD65", "RAD70", "RAD75", "RAD80")


MIROC85RAD1519 <- as.data.frame(rbind(MIROCRAD2015[,c(368,367)], MIROCRAD2016[,c(369,368)], MIROCRAD2017[,c(368,367)], MIROCRAD2018[,c(368,367)],
                                      MIROCRAD2019[,c(368,367)]))


rm(MIROC85RAD25,MIROC85RAD30,MIROC85RAD35,MIROC85RAD40, MIROC85RAD45,
   MIROC85RAD50, MIROC85RAD55, MIROC85RAD60, MIROC85RAD65, MIROC85RAD70, MIROC85RAD75, MIROC85RAD80)
rownames(MIROC85RADFull) <- allcoords1$PlotCN

MIROC26ALL <- as.data.frame(cbind(MIROC26TempFull,MIROC26PrecFull,MIROC26HUMFull,MIROC26RADFull))
MIROC45ALL <- as.data.frame(cbind(MIROC45TempFull,MIROC45PrecFull,MIROC45HUMFull,MIROC45RADFull))
MIROC85ALL <- as.data.frame(cbind(MIROC85TempFull,MIROC85PrecFull,MIROC85HUMFull,MIROC85RADFull))

write.csv(MIROC26ALL, file="MIROC26ALL.csv")
write.csv(MIROC45ALL, file="MIROC45ALL.csv")
write.csv(MIROC85ALL, file="MIROC85ALL.csv")


#GFDL26
#Temp
#2015
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2015.nc", subds="tas")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLTEMP2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLTEMP2015$AvgTemp <- rowMeans(GFDLTEMP2015[,2:366])
#Add PlotCN
GFDLTEMP2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert K  to C
GFDLTEMP2015$AvgTemp <- GFDLTEMP2015$AvgTemp-273.15
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2016.nc", subds="tas")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLTEMP2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLTEMP2016$AvgTemp <- rowMeans(GFDLTEMP2016[,2:366])
#Add PlotCN
GFDLTEMP2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert K  to C
GFDLTEMP2016$AvgTemp <- GFDLTEMP2016$AvgTemp-273.15
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2017.nc", subds="tas")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLTEMP2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLTEMP2017$AvgTemp <- rowMeans(GFDLTEMP2017[,2:366])
#Add PlotCN
GFDLTEMP2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert K  to C
GFDLTEMP2017$AvgTemp <- GFDLTEMP2017$AvgTemp-273.15
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2018.nc", subds="tas")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLTEMP2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLTEMP2018$AvgTemp <- rowMeans(GFDLTEMP2018[,2:366])
#Add PlotCN
GFDLTEMP2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert K  to C
GFDLTEMP2018$AvgTemp <- GFDLTEMP2018$AvgTemp-273.15
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2019.nc", subds="tas")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLTEMP2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLTEMP2019$AvgTemp <- rowMeans(GFDLTEMP2019[,2:366])
#Add PlotCN
GFDLTEMP2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert K  to C
GFDLTEMP2019$AvgTemp <- GFDLTEMP2019$AvgTemp-273.15
rm(GFDL2019)

#Temp25
GFDL26b25 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2025.nc", subds="tas")
GFDL26b25 <- terra::rotate(GFDL26b25)
#Pull  vals
GFDL26TEMP25<-data.frame(terra::extract(GFDL26b25,allcoords1))
GFDL26TEMP25$Temp25 <- rowMeans(GFDL26TEMP25[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP25$Temp25 <- GFDL26TEMP25$Temp25-273.15
rownames(GFDL26TEMP25) <- allcoords1$PlotCN
rm(GFDL26b25)
#Temp30
GFDL26b30 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2030.nc", subds="tas")
GFDL26b30 <- terra::rotate(GFDL26b30)
#Pull  vals
GFDL26TEMP30<-data.frame(terra::extract(GFDL26b30,allcoords1))
GFDL26TEMP30$Temp30 <- rowMeans(GFDL26TEMP30[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP30$Temp30 <- GFDL26TEMP30$Temp30-273.15
rownames(GFDL26TEMP30) <- allcoords1$PlotCN
rm(GFDL26b30)
#Temp35
GFDL26b35 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2035.nc", subds="tas")
GFDL26b35 <- terra::rotate(GFDL26b35)
#Pull  vals
GFDL26TEMP35<-data.frame(terra::extract(GFDL26b35,allcoords1))
GFDL26TEMP35$Temp35 <- rowMeans(GFDL26TEMP35[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP35$Temp35 <- GFDL26TEMP35$Temp35-273.15
rownames(GFDL26TEMP35) <- allcoords1$PlotCN
rm(GFDL26b35)
#Temp40
GFDL26b40 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2040.nc", subds="tas")
GFDL26b40 <- terra::rotate(GFDL26b40)
#Pull  vals
GFDL26TEMP40<-data.frame(terra::extract(GFDL26b40,allcoords1))
GFDL26TEMP40$Temp40 <- rowMeans(GFDL26TEMP40[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP40$Temp40 <- GFDL26TEMP40$Temp40-273.15
rownames(GFDL26TEMP40) <- allcoords1$PlotCN
rm(GFDL26b40)
#Temp45
GFDL26b45 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2045.nc", subds="tas")
GFDL26b45 <- terra::rotate(GFDL26b45)
#Pull  vals
GFDL26TEMP45<-data.frame(terra::extract(GFDL26b45,allcoords1))
GFDL26TEMP45$Temp45 <- rowMeans(GFDL26TEMP45[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP45$Temp45 <- GFDL26TEMP45$Temp45-273.15
rownames(GFDL26TEMP45) <- allcoords1$PlotCN
rm(GFDL26b45)

#Temp50
GFDL26b50 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2050.nc", subds="tas")
GFDL26b50 <- terra::rotate(GFDL26b50)
#Pull  vals
GFDL26TEMP50<-data.frame(terra::extract(GFDL26b50,allcoords1))
GFDL26TEMP50$Temp50 <- rowMeans(GFDL26TEMP50[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP50$Temp50 <- GFDL26TEMP50$Temp50-273.15
rownames(GFDL26TEMP50) <- allcoords1$PlotCN
rm(GFDL26b50)
#Temp55
GFDL26b55 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2055.nc", subds="tas")
GFDL26b55 <- terra::rotate(GFDL26b55)
#Pull  vals
GFDL26TEMP55<-data.frame(terra::extract(GFDL26b55,allcoords1))
GFDL26TEMP55$Temp55 <- rowMeans(GFDL26TEMP55[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP55$Temp55 <- GFDL26TEMP55$Temp55-273.15
rownames(GFDL26TEMP55) <- allcoords1$PlotCN
rm(GFDL26b55)
#Temp60
GFDL26b60 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2060.nc", subds="tas")
GFDL26b60 <- terra::rotate(GFDL26b60)
#Pull  vals
GFDL26TEMP60<-data.frame(terra::extract(GFDL26b60,allcoords1))
GFDL26TEMP60$Temp60 <- rowMeans(GFDL26TEMP60[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP60$Temp60 <- GFDL26TEMP60$Temp60-273.15
rownames(GFDL26TEMP60) <- allcoords1$PlotCN
rm(GFDL26b60)
#Temp65
GFDL26b65 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2065.nc", subds="tas")
GFDL26b65 <- terra::rotate(GFDL26b65)
#Pull  vals
GFDL26TEMP65<-data.frame(terra::extract(GFDL26b65,allcoords1))
GFDL26TEMP65$Temp65 <- rowMeans(GFDL26TEMP65[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP65$Temp65 <- GFDL26TEMP65$Temp65-273.15
rownames(GFDL26TEMP65) <- allcoords1$PlotCN
rm(GFDL26b65)
#Temp70
GFDL26b70 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2070.nc", subds="tas")
GFDL26b70 <- terra::rotate(GFDL26b70)
#Pull  vals
GFDL26TEMP70<-data.frame(terra::extract(GFDL26b70,allcoords1))
GFDL26TEMP70$Temp70 <- rowMeans(GFDL26TEMP70[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP70$Temp70 <- GFDL26TEMP70$Temp70-273.15
rownames(GFDL26TEMP70) <- allcoords1$PlotCN
rm(GFDL26b70)
#Temp75
GFDL26b75 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2075.nc", subds="tas")
GFDL26b75 <- terra::rotate(GFDL26b75)
#Pull  vals
GFDL26TEMP75<-data.frame(terra::extract(GFDL26b75,allcoords1))
GFDL26TEMP75$Temp75 <- rowMeans(GFDL26TEMP75[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP75$Temp75 <- GFDL26TEMP75$Temp75-273.15
rownames(GFDL26TEMP75) <- allcoords1$PlotCN
rm(GFDL26b75)
#Temp80
GFDL26b80 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Ta2080.nc", subds="tas")
GFDL26b80 <- terra::rotate(GFDL26b80)
#Pull  vals
GFDL26TEMP80<-data.frame(terra::extract(GFDL26b80,allcoords1))
GFDL26TEMP80$Temp80 <- rowMeans(GFDL26TEMP80[,2:366])
#convert kg/m2/s -> to mm
GFDL26TEMP80$Temp80 <- GFDL26TEMP80$Temp80-273.15
rownames(GFDL26TEMP80) <- allcoords1$PlotCN
rm(GFDL26b80)

GFDL26TempFull<- data.frame(cbind(GFDL26TEMP25$Temp25,GFDL26TEMP30$Temp30,GFDL26TEMP35$Temp35,GFDL26TEMP40$Temp40, GFDL26TEMP45$Temp45,
                                  GFDL26TEMP50$Temp50, GFDL26TEMP55$Temp55, GFDL26TEMP60$Temp60, GFDL26TEMP65$Temp65, GFDL26TEMP70$Temp70, GFDL26TEMP75$Temp75, GFDL26TEMP80$Temp80))

colnames(GFDL26TempFull)<- c("Temp25","Temp30","Temp35","Temp40", "Temp45","Temp50", "Temp55", "Temp60", "Temp65", "Temp70", "Temp75", "Temp80")

GFDL26TEMP1519 <- as.data.frame(rbind(GFDLTEMP2015[,c(368,367)], GFDLTEMP2016[,c(368,367)], GFDLTEMP2017[,c(368,367)], GFDLTEMP2018[,c(368,367)],
                                      GFDLTEMP2019[,c(368,367)]))


rm(GFDL26TEMP25,GFDL26TEMP30,GFDL26TEMP35,GFDL26TEMP40, GFDL26TEMP45,
   GFDL26TEMP50, GFDL26TEMP55, GFDL26TEMP60, GFDL26TEMP65, GFDL26TEMP70, GFDL26TEMP75, GFDL26TEMP80)
rownames(GFDL26TempFull) <- allcoords1$PlotCN

#Prec
#2015
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2015.nc", subds="pr")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLPREC2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLPREC2015$PrecSum <- rowSums(GFDLPrec2015[,2:366])
#Add PlotCN
GFDLPREC2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert to mm
GFDLPREC2015$PrecSum <- GFDLPREC2015$PrecSum*86400
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2016.nc", subds="pr")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLPREC2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLPREC2016$PrecSum <- rowSums(GFDLPrec2016[,2:366])
#Add PlotCN
GFDLPREC2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert to mm
GFDLPREC2016$PrecSum <- GFDLPREC2016$PrecSum*86400
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2017.nc", subds="pr")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLPREC2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLPREC2017$PrecSum <- rowSums(GFDLPrec2017[,2:366])
#Add PlotCN
GFDLPREC2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert to mm
GFDLPREC2017$PrecSum <- GFDLPREC2017$PrecSum*86400
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2018.nc", subds="pr")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLPREC2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLPREC2018$PrecSum <- rowSums(GFDLPrec2018[,2:366])
#Add PlotCN
GFDLPREC2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert to mm
GFDLPREC2018$PrecSum <- GFDLPREC2018$PrecSum*86400
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2019.nc", subds="pr")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLPREC2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLPREC2019$PrecSum <- rowSums(GFDLPrec2019[,2:366])
#Add PlotCN
GFDLPREC2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert to mm
GFDLPREC2019$PrecSum <- GFDLPREC2019$PrecSum*86400
rm(GFDL2019)

#Prec25
GFDL26b25 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2025.nc", subds="pr")
GFDL26b25 <- terra::rotate(GFDL26b25)
#Pull  vals
GFDL26PREC25<-data.frame(terra::extract(GFDL26b25,allcoords1))
GFDL26PREC25$Prec25 <- rowSums(GFDL26Prec25[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC25$Prec25 <- GFDL26PREC25$Prec25*86400
rownames(GFDL26PREC25) <- allcoords1$PlotCN
rm(GFDL26b25)
#Prec30
GFDL26b30 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2030.nc", subds="pr")
GFDL26b30 <- terra::rotate(GFDL26b30)
#Pull  vals
GFDL26PREC30<-data.frame(terra::extract(GFDL26b30,allcoords1))
GFDL26PREC30$Prec30 <- rowSums(GFDL26Prec30[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC30$Prec30 <- GFDL26PREC30$Prec30*86400
rownames(GFDL26PREC30) <- allcoords1$PlotCN
rm(GFDL26b30)
#Prec35
GFDL26b35 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2035.nc", subds="pr")
GFDL26b35 <- terra::rotate(GFDL26b35)
#Pull  vals
GFDL26PREC35<-data.frame(terra::extract(GFDL26b35,allcoords1))
GFDL26PREC35$Prec35 <- rowSums(GFDL26Prec35[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC35$Prec35 <- GFDL26PREC35$Prec35*86400
rownames(GFDL26PREC35) <- allcoords1$PlotCN
rm(GFDL26b35)
#Prec40
GFDL26b40 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2040.nc", subds="pr")
GFDL26b40 <- terra::rotate(GFDL26b40)
#Pull  vals
GFDL26PREC40<-data.frame(terra::extract(GFDL26b40,allcoords1))
GFDL26PREC40$Prec40 <- rowSums(GFDL26Prec40[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC40$Prec40 <- GFDL26PREC40$Prec40*86400
rownames(GFDL26PREC40) <- allcoords1$PlotCN
rm(GFDL26b40)
#Prec45
GFDL26b45 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2045.nc", subds="pr")
GFDL26b45 <- terra::rotate(GFDL26b45)
#Pull  vals
GFDL26PREC45<-data.frame(terra::extract(GFDL26b45,allcoords1))
GFDL26PREC45$Prec45 <- rowSums(GFDL26Prec45[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC45$Prec45 <- GFDL26PREC45$Prec45*86400
rownames(GFDL26PREC45) <- allcoords1$PlotCN
rm(GFDL26b45)

#Prec50
GFDL26b50 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2050.nc", subds="pr")
GFDL26b50 <- terra::rotate(GFDL26b50)
#Pull  vals
GFDL26PREC50<-data.frame(terra::extract(GFDL26b50,allcoords1))
GFDL26PREC50$Prec50 <- rowSums(GFDL26Prec50[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC50$Prec50 <- GFDL26PREC50$Prec50*86400
rownames(GFDL26PREC50) <- allcoords1$PlotCN
rm(GFDL26b50)
#Prec55
GFDL26b55 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2055.nc", subds="pr")
GFDL26b55 <- terra::rotate(GFDL26b55)
#Pull  vals
GFDL26PREC55<-data.frame(terra::extract(GFDL26b55,allcoords1))
GFDL26PREC55$Prec55 <- rowSums(GFDL26Prec55[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC55$Prec55 <- GFDL26PREC55$Prec55*86400
rownames(GFDL26PREC55) <- allcoords1$PlotCN
rm(GFDL26b55)
#Prec60
GFDL26b60 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2060.nc", subds="pr")
GFDL26b60 <- terra::rotate(GFDL26b60)
#Pull  vals
GFDL26PREC60<-data.frame(terra::extract(GFDL26b60,allcoords1))
GFDL26PREC60$Prec60 <- rowSums(GFDL26Prec60[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC60$Prec60 <- GFDL26PREC60$Prec60*86400
rownames(GFDL26PREC60) <- allcoords1$PlotCN
rm(GFDL26b60)
#Prec65
GFDL26b65 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2065.nc", subds="pr")
GFDL26b65 <- terra::rotate(GFDL26b65)
#Pull  vals
GFDL26PREC65<-data.frame(terra::extract(GFDL26b65,allcoords1))
GFDL26PREC65$Prec65 <- rowSums(GFDL26Prec65[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC65$Prec65 <- GFDL26PREC65$Prec65*86400
rownames(GFDL26PREC65) <- allcoords1$PlotCN
rm(GFDL26b65)
#Prec70
GFDL26b70 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2070.nc", subds="pr")
GFDL26b70 <- terra::rotate(GFDL26b70)
#Pull  vals
GFDL26PREC70<-data.frame(terra::extract(GFDL26b70,allcoords1))
GFDL26PREC70$Prec70 <- rowSums(GFDL26Prec70[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC70$Prec70 <- GFDL26PREC70$Prec70*86400
rownames(GFDL26PREC70) <- allcoords1$PlotCN
rm(GFDL26b70)
#Prec75
GFDL26b75 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2075.nc", subds="pr")
GFDL26b75 <- terra::rotate(GFDL26b75)
#Pull  vals
GFDL26PREC75<-data.frame(terra::extract(GFDL26b75,allcoords1))
GFDL26PREC75$Prec75 <- rowSums(GFDL26Prec75[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC75$Prec75 <- GFDL26PREC75$Prec75*86400
rownames(GFDL26PREC75) <- allcoords1$PlotCN
rm(GFDL26b75)
#Prec80
GFDL26b80 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Pr2080.nc", subds="pr")
GFDL26b80 <- terra::rotate(GFDL26b80)
#Pull  vals
GFDL26PREC80<-data.frame(terra::extract(GFDL26b80,allcoords1))
GFDL26PREC80$Prec80 <- rowSums(GFDL26Prec80[,2:366])
#convert kg/m2/s -> to mm
GFDL26PREC80$Prec80 <- GFDL26PREC80$Prec80*86400
rownames(GFDL26PREC80) <- allcoords1$PlotCN
rm(GFDL26b80)

GFDL26PrecFull<- data.frame(cbind(GFDL26PREC25$Prec25,GFDL26PREC30$Prec30,GFDL26PREC35$Prec35,GFDL26PREC40$Prec40, GFDL26PREC45$Prec45,
                                  GFDL26PREC50$Prec50, GFDL26PREC55$Prec55, GFDL26PREC60$Prec60, GFDL26PREC65$Prec65, GFDL26PREC70$Prec70, GFDL26PREC75$Prec75, GFDL26PREC80$Prec80))

colnames(GFDL26PrecFull)<- c("Prec25","Prec30","Prec35","Prec40", "Prec45","Prec50", "Prec55", "Prec60", "Prec65", "Prec70", "Prec75", "Prec80")

GFDL26PREC1519 <- as.data.frame(rbind(GFDLPREC2015[,c(368,367)], GFDLPREC2016[,c(368,367)], GFDLPREC2017[,c(368,367)], GFDLPREC2018[,c(368,367)],
                                      GFDLPREC2019[,c(368,367)]))


rm(GFDL26PREC25,GFDL26PREC30,GFDL26PREC35,GFDL26PREC40, GFDL26PREC45,
   GFDL26PREC50, GFDL26PREC55, GFDL26PREC60, GFDL26PREC65, GFDL26PREC70, GFDL26PREC75, GFDL26PREC80)
rownames(GFDL26PrecFull) <- allcoords1$PlotCN

#HUM
#2015
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2015.nc", subds="hurs")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLHUM2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLHUM2015$AvgHUM <- rowMeans(GFDLHUM2015[,2:366])
#Add PlotCN
GFDLHUM2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2016.nc", subds="hurs")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLHUM2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLHUM2016$AvgHUM <- rowMeans(GFDLHUM2016[,2:366])
#Add PlotCN
GFDLHUM2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2017.nc", subds="hurs")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLHUM2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLHUM2017$AvgHUM <- rowMeans(GFDLHUM2017[,2:366])
#Add PlotCN
GFDLHUM2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2018.nc", subds="hurs")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLHUM2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLHUM2018$AvgHUM <- rowMeans(GFDLHUM2018[,2:366])
#Add PlotCN
GFDLHUM2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2019.nc", subds="hurs")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLHUM2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLHUM2019$AvgHUM <- rowMeans(GFDLHUM2019[,2:366])
#Add PlotCN
GFDLHUM2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(GFDL2019)

#HUM25
GFDL26b25 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2025.nc", subds="hurs")
GFDL26b25 <- terra::rotate(GFDL26b25)
#Pull  vals
GFDL26HUM25<-data.frame(terra::extract(GFDL26b25,allcoords1))
GFDL26HUM25$HUM25 <- rowMeans(GFDL26HUM25[,2:366])
rownames(GFDL26HUM25) <- allcoords1$PlotCN
rm(GFDL26b25)
#HUM30
GFDL26b30 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2030.nc", subds="hurs")
GFDL26b30 <- terra::rotate(GFDL26b30)
#Pull  vals
GFDL26HUM30<-data.frame(terra::extract(GFDL26b30,allcoords1))
GFDL26HUM30$HUM30 <- rowMeans(GFDL26HUM30[,2:366])
rownames(GFDL26HUM30) <- allcoords1$PlotCN
rm(GFDL26b30)
#HUM35
GFDL26b35 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2035.nc", subds="hurs")
GFDL26b35 <- terra::rotate(GFDL26b35)
#Pull  vals
GFDL26HUM35<-data.frame(terra::extract(GFDL26b35,allcoords1))
GFDL26HUM35$HUM35 <- rowMeans(GFDL26HUM35[,2:366])
rownames(GFDL26HUM35) <- allcoords1$PlotCN
rm(GFDL26b35)
#HUM40
GFDL26b40 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2040.nc", subds="hurs")
GFDL26b40 <- terra::rotate(GFDL26b40)
#Pull  vals
GFDL26HUM40<-data.frame(terra::extract(GFDL26b40,allcoords1))
GFDL26HUM40$HUM40 <- rowMeans(GFDL26HUM40[,2:366])
rownames(GFDL26HUM40) <- allcoords1$PlotCN
rm(GFDL26b40)
#HUM45
GFDL26b45 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2045.nc", subds="hurs")
GFDL26b45 <- terra::rotate(GFDL26b45)
#Pull  vals
GFDL26HUM45<-data.frame(terra::extract(GFDL26b45,allcoords1))
GFDL26HUM45$HUM45 <- rowMeans(GFDL26HUM45[,2:366])
rownames(GFDL26HUM45) <- allcoords1$PlotCN
rm(GFDL26b45)

#HUM50
GFDL26b50 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2050.nc", subds="hurs")
GFDL26b50 <- terra::rotate(GFDL26b50)
#Pull  vals
GFDL26HUM50<-data.frame(terra::extract(GFDL26b50,allcoords1))
GFDL26HUM50$HUM50 <- rowMeans(GFDL26HUM50[,2:366])
rownames(GFDL26HUM50) <- allcoords1$PlotCN
rm(GFDL26b50)
#HUM55
GFDL26b55 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2055.nc", subds="hurs")
GFDL26b55 <- terra::rotate(GFDL26b55)
#Pull  vals
GFDL26HUM55<-data.frame(terra::extract(GFDL26b55,allcoords1))
GFDL26HUM55$HUM55 <- rowMeans(GFDL26HUM55[,2:366])
rownames(GFDL26HUM55) <- allcoords1$PlotCN
rm(GFDL26b55)
#HUM60
GFDL26b60 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2060.nc", subds="hurs")
GFDL26b60 <- terra::rotate(GFDL26b60)
#Pull  vals
GFDL26HUM60<-data.frame(terra::extract(GFDL26b60,allcoords1))
GFDL26HUM60$HUM60 <- rowMeans(GFDL26HUM60[,2:366])
rownames(GFDL26HUM60) <- allcoords1$PlotCN
rm(GFDL26b60)
#HUM65
GFDL26b65 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2065.nc", subds="hurs")
GFDL26b65 <- terra::rotate(GFDL26b65)
#Pull  vals
GFDL26HUM65<-data.frame(terra::extract(GFDL26b65,allcoords1))
GFDL26HUM65$HUM65 <- rowMeans(GFDL26HUM65[,2:366])
rownames(GFDL26HUM65) <- allcoords1$PlotCN
rm(GFDL26b65)
#HUM70
GFDL26b70 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2070.nc", subds="hurs")
GFDL26b70 <- terra::rotate(GFDL26b70)
#Pull  vals
GFDL26HUM70<-data.frame(terra::extract(GFDL26b70,allcoords1))
GFDL26HUM70$HUM70 <- rowMeans(GFDL26HUM70[,2:366])
rownames(GFDL26HUM70) <- allcoords1$PlotCN
rm(GFDL26b70)
#HUM75
GFDL26b75 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2075.nc", subds="hurs")
GFDL26b75 <- terra::rotate(GFDL26b75)
#Pull  vals
GFDL26HUM75<-data.frame(terra::extract(GFDL26b75,allcoords1))
GFDL26HUM75$HUM75 <- rowMeans(GFDL26HUM75[,2:366])
rownames(GFDL26HUM75) <- allcoords1$PlotCN
rm(GFDL26b75)
#HUM80
GFDL26b80 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26Hurs2080.nc", subds="hurs")
GFDL26b80 <- terra::rotate(GFDL26b80)
#Pull  vals
GFDL26HUM80<-data.frame(terra::extract(GFDL26b80,allcoords1))
GFDL26HUM80$HUM80 <- rowMeans(GFDL26HUM80[,2:366])
#convert kg/m2/s -> to mm
GFDL26HUM80$HUM80 <- GFDL26HUM80$HUM80
rownames(GFDL26HUM80) <- allcoords1$PlotCN
rm(GFDL26b80)

GFDL26HUMFull<- data.frame(cbind(GFDL26HUM25$HUM25,GFDL26HUM30$HUM30,GFDL26HUM35$HUM35,GFDL26HUM40$HUM40, GFDL26HUM45$HUM45,
                                 GFDL26HUM50$HUM50, GFDL26HUM55$HUM55, GFDL26HUM60$HUM60, GFDL26HUM65$HUM65, GFDL26HUM70$HUM70, GFDL26HUM75$HUM75, GFDL26HUM80$HUM80))

colnames(GFDL26HUMFull)<- c("HUM25","HUM30","HUM35","HUM40", "HUM45","HUM50", "HUM55", "HUM60", "HUM65", "HUM70", "HUM75", "HUM80")

GFDL26HUM1519 <- as.data.frame(rbind(GFDLHUM2015[,c(368,367)], GFDLHUM2016[,c(368,367)], GFDLHUM2017[,c(368,367)], GFDLHUM2018[,c(368,367)],
                                     GFDLHUM2019[,c(368,367)]))


rm(GFDL26HUM25,GFDL26HUM30,GFDL26HUM35,GFDL26HUM40, GFDL26HUM45,
   GFDL26HUM50, GFDL26HUM55, GFDL26HUM60, GFDL26HUM65, GFDL26HUM70, GFDL26HUM75, GFDL26HUM80)
rownames(GFDL26HUMFull) <- allcoords1$PlotCN
#RAD
#2015
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2015.nc", subds="rsds")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLRAD2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLRAD2015$AvgRAD <- rowMeans(GFDLRAD2015[,2:366])
#Add PlotCN
GFDLRAD2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2016.nc", subds="rsds")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLRAD2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLRAD2016$AvgRAD <- rowMeans(GFDLRAD2016[,2:366])
#Add PlotCN
GFDLRAD2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2017.nc", subds="rsds")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLRAD2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLRAD2017$AvgRAD <- rowMeans(GFDLRAD2017[,2:366])
#Add PlotCN
GFDLRAD2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2018.nc", subds="rsds")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLRAD2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLRAD2018$AvgRAD <- rowMeans(GFDLRAD2018[,2:366])
#Add PlotCN
GFDLRAD2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL26")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2019.nc", subds="rsds")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLRAD2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLRAD2019$AvgRAD <- rowMeans(GFDLRAD2019[,2:366])
#Add PlotCN
GFDLRAD2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(GFDL2019)

#RAD25
GFDL26b25 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2025.nc", subds="rsds")
GFDL26b25 <- terra::rotate(GFDL26b25)
#Pull  vals
GFDL26RAD25<-data.frame(terra::extract(GFDL26b25,allcoords1))
GFDL26RAD25$RAD25 <- rowMeans(GFDL26RAD25[,2:366])
rownames(GFDL26RAD25) <- allcoords1$PlotCN
rm(GFDL26b25)
#RAD30
GFDL26b30 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2030.nc", subds="rsds")
GFDL26b30 <- terra::rotate(GFDL26b30)
#Pull  vals
GFDL26RAD30<-data.frame(terra::extract(GFDL26b30,allcoords1))
GFDL26RAD30$RAD30 <- rowMeans(GFDL26RAD30[,2:366])
rownames(GFDL26RAD30) <- allcoords1$PlotCN
rm(GFDL26b30)
#RAD35
GFDL26b35 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2035.nc", subds="rsds")
GFDL26b35 <- terra::rotate(GFDL26b35)
#Pull  vals
GFDL26RAD35<-data.frame(terra::extract(GFDL26b35,allcoords1))
GFDL26RAD35$RAD35 <- rowMeans(GFDL26RAD35[,2:366])
rownames(GFDL26RAD35) <- allcoords1$PlotCN
rm(GFDL26b35)
#RAD40
GFDL26b40 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2040.nc", subds="rsds")
GFDL26b40 <- terra::rotate(GFDL26b40)
#Pull  vals
GFDL26RAD40<-data.frame(terra::extract(GFDL26b40,allcoords1))
GFDL26RAD40$RAD40 <- rowMeans(GFDL26RAD40[,2:366])
rownames(GFDL26RAD40) <- allcoords1$PlotCN
rm(GFDL26b40)
#RAD45
GFDL26b45 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2045.nc", subds="rsds")
GFDL26b45 <- terra::rotate(GFDL26b45)
#Pull  vals
GFDL26RAD45<-data.frame(terra::extract(GFDL26b45,allcoords1))
GFDL26RAD45$RAD45 <- rowMeans(GFDL26RAD45[,2:366])
rownames(GFDL26RAD45) <- allcoords1$PlotCN
rm(GFDL26b45)

#RAD50
GFDL26b50 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2050.nc", subds="rsds")
GFDL26b50 <- terra::rotate(GFDL26b50)
#Pull  vals
GFDL26RAD50<-data.frame(terra::extract(GFDL26b50,allcoords1))
GFDL26RAD50$RAD50 <- rowMeans(GFDL26RAD50[,2:366])
rownames(GFDL26RAD50) <- allcoords1$PlotCN
rm(GFDL26b50)
#RAD55
GFDL26b55 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2055.nc", subds="rsds")
GFDL26b55 <- terra::rotate(GFDL26b55)
#Pull  vals
GFDL26RAD55<-data.frame(terra::extract(GFDL26b55,allcoords1))
GFDL26RAD55$RAD55 <- rowMeans(GFDL26RAD55[,2:366])
rownames(GFDL26RAD55) <- allcoords1$PlotCN
rm(GFDL26b55)
#RAD60
GFDL26b60 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2060.nc", subds="rsds")
GFDL26b60 <- terra::rotate(GFDL26b60)
#Pull  vals
GFDL26RAD60<-data.frame(terra::extract(GFDL26b60,allcoords1))
GFDL26RAD60$RAD60 <- rowMeans(GFDL26RAD60[,2:366])
rownames(GFDL26RAD60) <- allcoords1$PlotCN
rm(GFDL26b60)
#RAD65
GFDL26b65 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2065.nc", subds="rsds")
GFDL26b65 <- terra::rotate(GFDL26b65)
#Pull  vals
GFDL26RAD65<-data.frame(terra::extract(GFDL26b65,allcoords1))
GFDL26RAD65$RAD65 <- rowMeans(GFDL26RAD65[,2:366])
rownames(GFDL26RAD65) <- allcoords1$PlotCN
rm(GFDL26b65)
#RAD70
GFDL26b70 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2070.nc", subds="rsds")
GFDL26b70 <- terra::rotate(GFDL26b70)
#Pull  vals
GFDL26RAD70<-data.frame(terra::extract(GFDL26b70,allcoords1))
GFDL26RAD70$RAD70 <- rowMeans(GFDL26RAD70[,2:366])
rownames(GFDL26RAD70) <- allcoords1$PlotCN
rm(GFDL26b70)
#RAD75
GFDL26b75 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2075.nc", subds="rsds")
GFDL26b75 <- terra::rotate(GFDL26b75)
#Pull  vals
GFDL26RAD75<-data.frame(terra::extract(GFDL26b75,allcoords1))
GFDL26RAD75$RAD75 <- rowMeans(GFDL26RAD75[,2:366])
rownames(GFDL26RAD75) <- allcoords1$PlotCN
rm(GFDL26b75)
#RAD80
GFDL26b80 <- rast("~/Desktop/NASAClim/GFDL26/GFDL26RAD2080.nc", subds="rsds")
GFDL26b80 <- terra::rotate(GFDL26b80)
#Pull  vals
GFDL26RAD80<-data.frame(terra::extract(GFDL26b80,allcoords1))
GFDL26RAD80$RAD80 <- rowMeans(GFDL26RAD80[,2:366])
#convert kg/m2/s -> to mm
GFDL26RAD80$RAD80 <- GFDL26RAD80$RAD80
rownames(GFDL26RAD80) <- allcoords1$PlotCN
rm(GFDL26b80)

GFDL26RADFull<- data.frame(cbind(GFDL26RAD25$RAD25,GFDL26RAD30$RAD30,GFDL26RAD35$RAD35,GFDL26RAD40$RAD40, GFDL26RAD45$RAD45,
                                 GFDL26RAD50$RAD50, GFDL26RAD55$RAD55, GFDL26RAD60$RAD60, GFDL26RAD65$RAD65, GFDL26RAD70$RAD70, GFDL26RAD75$RAD75, GFDL26RAD80$RAD80))

colnames(GFDL26RADFull)<- c("RAD25","RAD30","RAD35","RAD40", "RAD45","RAD50", "RAD55", "RAD60", "RAD65", "RAD70", "RAD75", "RAD80")


GFDL26RAD1519 <- as.data.frame(rbind(GFDLRAD2015[,c(368,367)], GFDLRAD2016[,c(368,367)], GFDLRAD2017[,c(368,367)], GFDLRAD2018[,c(368,367)],
                                     GFDLRAD2019[,c(368,367)]))


rm(GFDL26RAD25,GFDL26RAD30,GFDL26RAD35,GFDL26RAD40, GFDL26RAD45,
   GFDL26RAD50, GFDL26RAD55, GFDL26RAD60, GFDL26RAD65, GFDL26RAD70, GFDL26RAD75, GFDL26RAD80)
rownames(GFDL26RADFull) <- allcoords1$PlotCN

#future data projections from 2015-2080
#GFDL45
#Temp
#2015
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2015.nc", subds="tas")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLTEMP2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLTEMP2015$AvgTemp <- rowMeans(GFDLTEMP2015[,2:366])
#Add PlotCN
GFDLTEMP2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert K  to C
GFDLTEMP2015$AvgTemp <- GFDLTEMP2015$AvgTemp-273.15
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2016.nc", subds="tas")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLTEMP2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLTEMP2016$AvgTemp <- rowMeans(GFDLTEMP2016[,2:366])
#Add PlotCN
GFDLTEMP2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert K  to C
GFDLTEMP2016$AvgTemp <- GFDLTEMP2016$AvgTemp-273.15
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2017.nc", subds="tas")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLTEMP2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLTEMP2017$AvgTemp <- rowMeans(GFDLTEMP2017[,2:366])
#Add PlotCN
GFDLTEMP2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert K  to C
GFDLTEMP2017$AvgTemp <- GFDLTEMP2017$AvgTemp-273.15
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2018.nc", subds="tas")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLTEMP2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLTEMP2018$AvgTemp <- rowMeans(GFDLTEMP2018[,2:366])
#Add PlotCN
GFDLTEMP2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert K  to C
GFDLTEMP2018$AvgTemp <- GFDLTEMP2018$AvgTemp-273.15
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2019.nc", subds="tas")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLTEMP2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLTEMP2019$AvgTemp <- rowMeans(GFDLTEMP2019[,2:366])
#Add PlotCN
GFDLTEMP2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert K  to C
GFDLTEMP2019$AvgTemp <- GFDLTEMP2019$AvgTemp-273.15
rm(GFDL2019)

#Temp25
GFDL45b25 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2025.nc", subds="tas")
GFDL45b25 <- terra::rotate(GFDL45b25)
#Pull  vals
GFDL45TEMP25<-data.frame(terra::extract(GFDL45b25,allcoords1))
GFDL45TEMP25$Temp25 <- rowMeans(GFDL45TEMP25[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP25$Temp25 <- GFDL45TEMP25$Temp25-273.15
rownames(GFDL45TEMP25) <- allcoords1$PlotCN
rm(GFDL45b25)
#Temp30
GFDL45b30 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2030.nc", subds="tas")
GFDL45b30 <- terra::rotate(GFDL45b30)
#Pull  vals
GFDL45TEMP30<-data.frame(terra::extract(GFDL45b30,allcoords1))
GFDL45TEMP30$Temp30 <- rowMeans(GFDL45TEMP30[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP30$Temp30 <- GFDL45TEMP30$Temp30-273.15
rownames(GFDL45TEMP30) <- allcoords1$PlotCN
rm(GFDL45b30)
#Temp35
GFDL45b35 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2035.nc", subds="tas")
GFDL45b35 <- terra::rotate(GFDL45b35)
#Pull  vals
GFDL45TEMP35<-data.frame(terra::extract(GFDL45b35,allcoords1))
GFDL45TEMP35$Temp35 <- rowMeans(GFDL45TEMP35[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP35$Temp35 <- GFDL45TEMP35$Temp35-273.15
rownames(GFDL45TEMP35) <- allcoords1$PlotCN
rm(GFDL45b35)
#Temp40
GFDL45b40 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2040.nc", subds="tas")
GFDL45b40 <- terra::rotate(GFDL45b40)
#Pull  vals
GFDL45TEMP40<-data.frame(terra::extract(GFDL45b40,allcoords1))
GFDL45TEMP40$Temp40 <- rowMeans(GFDL45TEMP40[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP40$Temp40 <- GFDL45TEMP40$Temp40-273.15
rownames(GFDL45TEMP40) <- allcoords1$PlotCN
rm(GFDL45b40)
#Temp45
GFDL45b45 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2045.nc", subds="tas")
GFDL45b45 <- terra::rotate(GFDL45b45)
#Pull  vals
GFDL45TEMP45<-data.frame(terra::extract(GFDL45b45,allcoords1))
GFDL45TEMP45$Temp45 <- rowMeans(GFDL45TEMP45[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP45$Temp45 <- GFDL45TEMP45$Temp45-273.15
rownames(GFDL45TEMP45) <- allcoords1$PlotCN
rm(GFDL45b45)

#Temp50
GFDL45b50 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2050.nc", subds="tas")
GFDL45b50 <- terra::rotate(GFDL45b50)
#Pull  vals
GFDL45TEMP50<-data.frame(terra::extract(GFDL45b50,allcoords1))
GFDL45TEMP50$Temp50 <- rowMeans(GFDL45TEMP50[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP50$Temp50 <- GFDL45TEMP50$Temp50-273.15
rownames(GFDL45TEMP50) <- allcoords1$PlotCN
rm(GFDL45b50)
#Temp55
GFDL45b55 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2055.nc", subds="tas")
GFDL45b55 <- terra::rotate(GFDL45b55)
#Pull  vals
GFDL45TEMP55<-data.frame(terra::extract(GFDL45b55,allcoords1))
GFDL45TEMP55$Temp55 <- rowMeans(GFDL45TEMP55[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP55$Temp55 <- GFDL45TEMP55$Temp55-273.15
rownames(GFDL45TEMP55) <- allcoords1$PlotCN
rm(GFDL45b55)
#Temp60
GFDL45b60 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2060.nc", subds="tas")
GFDL45b60 <- terra::rotate(GFDL45b60)
#Pull  vals
GFDL45TEMP60<-data.frame(terra::extract(GFDL45b60,allcoords1))
GFDL45TEMP60$Temp60 <- rowMeans(GFDL45TEMP60[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP60$Temp60 <- GFDL45TEMP60$Temp60-273.15
rownames(GFDL45TEMP60) <- allcoords1$PlotCN
rm(GFDL45b60)
#Temp65
GFDL45b65 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2065.nc", subds="tas")
GFDL45b65 <- terra::rotate(GFDL45b65)
#Pull  vals
GFDL45TEMP65<-data.frame(terra::extract(GFDL45b65,allcoords1))
GFDL45TEMP65$Temp65 <- rowMeans(GFDL45TEMP65[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP65$Temp65 <- GFDL45TEMP65$Temp65-273.15
rownames(GFDL45TEMP65) <- allcoords1$PlotCN
rm(GFDL45b65)
#Temp70
GFDL45b70 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2070.nc", subds="tas")
GFDL45b70 <- terra::rotate(GFDL45b70)
#Pull  vals
GFDL45TEMP70<-data.frame(terra::extract(GFDL45b70,allcoords1))
GFDL45TEMP70$Temp70 <- rowMeans(GFDL45TEMP70[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP70$Temp70 <- GFDL45TEMP70$Temp70-273.15
rownames(GFDL45TEMP70) <- allcoords1$PlotCN
rm(GFDL45b70)
#Temp75
GFDL45b75 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2075.nc", subds="tas")
GFDL45b75 <- terra::rotate(GFDL45b75)
#Pull  vals
GFDL45TEMP75<-data.frame(terra::extract(GFDL45b75,allcoords1))
GFDL45TEMP75$Temp75 <- rowMeans(GFDL45TEMP75[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP75$Temp75 <- GFDL45TEMP75$Temp75-273.15
rownames(GFDL45TEMP75) <- allcoords1$PlotCN
rm(GFDL45b75)
#Temp80
GFDL45b80 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Ta2080.nc", subds="tas")
GFDL45b80 <- terra::rotate(GFDL45b80)
#Pull  vals
GFDL45TEMP80<-data.frame(terra::extract(GFDL45b80,allcoords1))
GFDL45TEMP80$Temp80 <- rowMeans(GFDL45TEMP80[,2:366])
#convert kg/m2/s -> to mm
GFDL45TEMP80$Temp80 <- GFDL45TEMP80$Temp80-273.15
rownames(GFDL45TEMP80) <- allcoords1$PlotCN
rm(GFDL45b80)

GFDL45TempFull<- data.frame(cbind(GFDL45TEMP25$Temp25,GFDL45TEMP30$Temp30,GFDL45TEMP35$Temp35,GFDL45TEMP40$Temp40, GFDL45TEMP45$Temp45,
                                  GFDL45TEMP50$Temp50, GFDL45TEMP55$Temp55, GFDL45TEMP60$Temp60, GFDL45TEMP65$Temp65, GFDL45TEMP70$Temp70, GFDL45TEMP75$Temp75, GFDL45TEMP80$Temp80))

colnames(GFDL45TempFull)<- c("Temp25","Temp30","Temp35","Temp40", "Temp45","Temp50", "Temp55", "Temp60", "Temp65", "Temp70", "Temp75", "Temp80")

GFDL45TEMP1519 <- as.data.frame(rbind(GFDLTEMP2015[,c(368,367)], GFDLTEMP2016[,c(368,367)], GFDLTEMP2017[,c(368,367)], GFDLTEMP2018[,c(368,367)],
                                      GFDLTEMP2019[,c(368,367)]))


rm(GFDL45TEMP25,GFDL45TEMP30,GFDL45TEMP35,GFDL45TEMP40, GFDL45TEMP45,
   GFDL45TEMP50, GFDL45TEMP55, GFDL45TEMP60, GFDL45TEMP65, GFDL45TEMP70, GFDL45TEMP75, GFDL45TEMP80)
rownames(GFDL45TempFull) <- allcoords1$PlotCN

#Prec
#2015
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2015.nc", subds="pr")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLPREC2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLPREC2015$PrecSum <- rowSums(GFDLPrec2015[,2:366])
#Add PlotCN
GFDLPREC2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert to mm
GFDLPREC2015$PrecSum <- GFDLPREC2015$PrecSum*86400
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2016.nc", subds="pr")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLPREC2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLPREC2016$PrecSum <- rowSums(GFDLPrec2016[,2:366])
#Add PlotCN
GFDLPREC2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert to mm
GFDLPREC2016$PrecSum <- GFDLPREC2016$PrecSum*86400
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2017.nc", subds="pr")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLPREC2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLPREC2017$PrecSum <- rowSums(GFDLPrec2017[,2:366])
#Add PlotCN
GFDLPREC2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert to mm
GFDLPREC2017$PrecSum <- GFDLPREC2017$PrecSum*86400
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2018.nc", subds="pr")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLPREC2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLPREC2018$PrecSum <- rowSums(GFDLPrec2018[,2:366])
#Add PlotCN
GFDLPREC2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert to mm
GFDLPREC2018$PrecSum <- GFDLPREC2018$PrecSum*86400
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2019.nc", subds="pr")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLPREC2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLPREC2019$PrecSum <- rowSums(GFDLPrec2019[,2:366])
#Add PlotCN
GFDLPREC2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert to mm
GFDLPREC2019$PrecSum <- GFDLPREC2019$PrecSum*86400
rm(GFDL2019)

#Prec25
GFDL45b25 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2025.nc", subds="pr")
GFDL45b25 <- terra::rotate(GFDL45b25)
#Pull  vals
GFDL45PREC25<-data.frame(terra::extract(GFDL45b25,allcoords1))
GFDL45PREC25$Prec25 <- rowSums(GFDL45Prec25[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC25$Prec25 <- GFDL45PREC25$Prec25*86400
rownames(GFDL45PREC25) <- allcoords1$PlotCN
rm(GFDL45b25)
#Prec30
GFDL45b30 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2030.nc", subds="pr")
GFDL45b30 <- terra::rotate(GFDL45b30)
#Pull  vals
GFDL45PREC30<-data.frame(terra::extract(GFDL45b30,allcoords1))
GFDL45PREC30$Prec30 <- rowSums(GFDL45Prec30[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC30$Prec30 <- GFDL45PREC30$Prec30*86400
rownames(GFDL45PREC30) <- allcoords1$PlotCN
rm(GFDL45b30)
#Prec35
GFDL45b35 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2035.nc", subds="pr")
GFDL45b35 <- terra::rotate(GFDL45b35)
#Pull  vals
GFDL45PREC35<-data.frame(terra::extract(GFDL45b35,allcoords1))
GFDL45PREC35$Prec35 <- rowSums(GFDL45Prec35[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC35$Prec35 <- GFDL45PREC35$Prec35*86400
rownames(GFDL45PREC35) <- allcoords1$PlotCN
rm(GFDL45b35)
#Prec40
GFDL45b40 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2040.nc", subds="pr")
GFDL45b40 <- terra::rotate(GFDL45b40)
#Pull  vals
GFDL45PREC40<-data.frame(terra::extract(GFDL45b40,allcoords1))
GFDL45PREC40$Prec40 <- rowSums(GFDL45Prec40[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC40$Prec40 <- GFDL45PREC40$Prec40*86400
rownames(GFDL45PREC40) <- allcoords1$PlotCN
rm(GFDL45b40)
#Prec45
GFDL45b45 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2045.nc", subds="pr")
GFDL45b45 <- terra::rotate(GFDL45b45)
#Pull  vals
GFDL45PREC45<-data.frame(terra::extract(GFDL45b45,allcoords1))
GFDL45PREC45$Prec45 <- rowSums(GFDL45Prec45[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC45$Prec45 <- GFDL45PREC45$Prec45*86400
rownames(GFDL45PREC45) <- allcoords1$PlotCN
rm(GFDL45b45)

#Prec50
GFDL45b50 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2050.nc", subds="pr")
GFDL45b50 <- terra::rotate(GFDL45b50)
#Pull  vals
GFDL45PREC50<-data.frame(terra::extract(GFDL45b50,allcoords1))
GFDL45PREC50$Prec50 <- rowSums(GFDL45Prec50[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC50$Prec50 <- GFDL45PREC50$Prec50*86400
rownames(GFDL45PREC50) <- allcoords1$PlotCN
rm(GFDL45b50)
#Prec55
GFDL45b55 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2055.nc", subds="pr")
GFDL45b55 <- terra::rotate(GFDL45b55)
#Pull  vals
GFDL45PREC55<-data.frame(terra::extract(GFDL45b55,allcoords1))
GFDL45PREC55$Prec55 <- rowSums(GFDL45Prec55[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC55$Prec55 <- GFDL45PREC55$Prec55*86400
rownames(GFDL45PREC55) <- allcoords1$PlotCN
rm(GFDL45b55)
#Prec60
GFDL45b60 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2060.nc", subds="pr")
GFDL45b60 <- terra::rotate(GFDL45b60)
#Pull  vals
GFDL45PREC60<-data.frame(terra::extract(GFDL45b60,allcoords1))
GFDL45PREC60$Prec60 <- rowSums(GFDL45Prec60[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC60$Prec60 <- GFDL45PREC60$Prec60*86400
rownames(GFDL45PREC60) <- allcoords1$PlotCN
rm(GFDL45b60)
#Prec65
GFDL45b65 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2065.nc", subds="pr")
GFDL45b65 <- terra::rotate(GFDL45b65)
#Pull  vals
GFDL45PREC65<-data.frame(terra::extract(GFDL45b65,allcoords1))
GFDL45PREC65$Prec65 <- rowSums(GFDL45Prec65[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC65$Prec65 <- GFDL45PREC65$Prec65*86400
rownames(GFDL45PREC65) <- allcoords1$PlotCN
rm(GFDL45b65)
#Prec70
GFDL45b70 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2070.nc", subds="pr")
GFDL45b70 <- terra::rotate(GFDL45b70)
#Pull  vals
GFDL45PREC70<-data.frame(terra::extract(GFDL45b70,allcoords1))
GFDL45PREC70$Prec70 <- rowSums(GFDL45Prec70[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC70$Prec70 <- GFDL45PREC70$Prec70*86400
rownames(GFDL45PREC70) <- allcoords1$PlotCN
rm(GFDL45b70)
#Prec75
GFDL45b75 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2075.nc", subds="pr")
GFDL45b75 <- terra::rotate(GFDL45b75)
#Pull  vals
GFDL45PREC75<-data.frame(terra::extract(GFDL45b75,allcoords1))
GFDL45PREC75$Prec75 <- rowSums(GFDL45Prec75[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC75$Prec75 <- GFDL45PREC75$Prec75*86400
rownames(GFDL45PREC75) <- allcoords1$PlotCN
rm(GFDL45b75)
#Prec80
GFDL45b80 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Pr2080.nc", subds="pr")
GFDL45b80 <- terra::rotate(GFDL45b80)
#Pull  vals
GFDL45PREC80<-data.frame(terra::extract(GFDL45b80,allcoords1))
GFDL45PREC80$Prec80 <- rowSums(GFDL45Prec80[,2:366])
#convert kg/m2/s -> to mm
GFDL45PREC80$Prec80 <- GFDL45PREC80$Prec80*86400
rownames(GFDL45PREC80) <- allcoords1$PlotCN
rm(GFDL45b80)

GFDL45PrecFull<- data.frame(cbind(GFDL45PREC25$Prec25,GFDL45PREC30$Prec30,GFDL45PREC35$Prec35,GFDL45PREC40$Prec40, GFDL45PREC45$Prec45,
                                  GFDL45PREC50$Prec50, GFDL45PREC55$Prec55, GFDL45PREC60$Prec60, GFDL45PREC65$Prec65, GFDL45PREC70$Prec70, GFDL45PREC75$Prec75, GFDL45PREC80$Prec80))

colnames(GFDL45PrecFull)<- c("Prec25","Prec30","Prec35","Prec40", "Prec45","Prec50", "Prec55", "Prec60", "Prec65", "Prec70", "Prec75", "Prec80")

GFDL45PREC1519 <- as.data.frame(rbind(GFDLPREC2015[,c(368,367)], GFDLPREC2016[,c(368,367)], GFDLPREC2017[,c(368,367)], GFDLPREC2018[,c(368,367)],
                                      GFDLPREC2019[,c(368,367)]))


rm(GFDL45PREC25,GFDL45PREC30,GFDL45PREC35,GFDL45PREC40, GFDL45PREC45,
   GFDL45PREC50, GFDL45PREC55, GFDL45PREC60, GFDL45PREC65, GFDL45PREC70, GFDL45PREC75, GFDL45PREC80)
rownames(GFDL45PrecFull) <- allcoords1$PlotCN

#HUM
#2015
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2015.nc", subds="hurs")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLHUM2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLHUM2015$AvgHUM <- rowMeans(GFDLHUM2015[,2:366])
#Add PlotCN
GFDLHUM2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2016.nc", subds="hurs")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLHUM2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLHUM2016$AvgHUM <- rowMeans(GFDLHUM2016[,2:366])
#Add PlotCN
GFDLHUM2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2017.nc", subds="hurs")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLHUM2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLHUM2017$AvgHUM <- rowMeans(GFDLHUM2017[,2:366])
#Add PlotCN
GFDLHUM2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2018.nc", subds="hurs")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLHUM2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLHUM2018$AvgHUM <- rowMeans(GFDLHUM2018[,2:366])
#Add PlotCN
GFDLHUM2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2019.nc", subds="hurs")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLHUM2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLHUM2019$AvgHUM <- rowMeans(GFDLHUM2019[,2:366])
#Add PlotCN
GFDLHUM2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(GFDL2019)

#HUM25
GFDL45b25 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2025.nc", subds="hurs")
GFDL45b25 <- terra::rotate(GFDL45b25)
#Pull  vals
GFDL45HUM25<-data.frame(terra::extract(GFDL45b25,allcoords1))
GFDL45HUM25$HUM25 <- rowMeans(GFDL45HUM25[,2:366])
rownames(GFDL45HUM25) <- allcoords1$PlotCN
rm(GFDL45b25)
#HUM30
GFDL45b30 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2030.nc", subds="hurs")
GFDL45b30 <- terra::rotate(GFDL45b30)
#Pull  vals
GFDL45HUM30<-data.frame(terra::extract(GFDL45b30,allcoords1))
GFDL45HUM30$HUM30 <- rowMeans(GFDL45HUM30[,2:366])
rownames(GFDL45HUM30) <- allcoords1$PlotCN
rm(GFDL45b30)
#HUM35
GFDL45b35 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2035.nc", subds="hurs")
GFDL45b35 <- terra::rotate(GFDL45b35)
#Pull  vals
GFDL45HUM35<-data.frame(terra::extract(GFDL45b35,allcoords1))
GFDL45HUM35$HUM35 <- rowMeans(GFDL45HUM35[,2:366])
rownames(GFDL45HUM35) <- allcoords1$PlotCN
rm(GFDL45b35)
#HUM40
GFDL45b40 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2040.nc", subds="hurs")
GFDL45b40 <- terra::rotate(GFDL45b40)
#Pull  vals
GFDL45HUM40<-data.frame(terra::extract(GFDL45b40,allcoords1))
GFDL45HUM40$HUM40 <- rowMeans(GFDL45HUM40[,2:366])
rownames(GFDL45HUM40) <- allcoords1$PlotCN
rm(GFDL45b40)
#HUM45
GFDL45b45 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2045.nc", subds="hurs")
GFDL45b45 <- terra::rotate(GFDL45b45)
#Pull  vals
GFDL45HUM45<-data.frame(terra::extract(GFDL45b45,allcoords1))
GFDL45HUM45$HUM45 <- rowMeans(GFDL45HUM45[,2:366])
rownames(GFDL45HUM45) <- allcoords1$PlotCN
rm(GFDL45b45)

#HUM50
GFDL45b50 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2050.nc", subds="hurs")
GFDL45b50 <- terra::rotate(GFDL45b50)
#Pull  vals
GFDL45HUM50<-data.frame(terra::extract(GFDL45b50,allcoords1))
GFDL45HUM50$HUM50 <- rowMeans(GFDL45HUM50[,2:366])
rownames(GFDL45HUM50) <- allcoords1$PlotCN
rm(GFDL45b50)
#HUM55
GFDL45b55 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2055.nc", subds="hurs")
GFDL45b55 <- terra::rotate(GFDL45b55)
#Pull  vals
GFDL45HUM55<-data.frame(terra::extract(GFDL45b55,allcoords1))
GFDL45HUM55$HUM55 <- rowMeans(GFDL45HUM55[,2:366])
rownames(GFDL45HUM55) <- allcoords1$PlotCN
rm(GFDL45b55)
#HUM60
GFDL45b60 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2060.nc", subds="hurs")
GFDL45b60 <- terra::rotate(GFDL45b60)
#Pull  vals
GFDL45HUM60<-data.frame(terra::extract(GFDL45b60,allcoords1))
GFDL45HUM60$HUM60 <- rowMeans(GFDL45HUM60[,2:366])
rownames(GFDL45HUM60) <- allcoords1$PlotCN
rm(GFDL45b60)
#HUM65
GFDL45b65 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2065.nc", subds="hurs")
GFDL45b65 <- terra::rotate(GFDL45b65)
#Pull  vals
GFDL45HUM65<-data.frame(terra::extract(GFDL45b65,allcoords1))
GFDL45HUM65$HUM65 <- rowMeans(GFDL45HUM65[,2:366])
rownames(GFDL45HUM65) <- allcoords1$PlotCN
rm(GFDL45b65)
#HUM70
GFDL45b70 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2070.nc", subds="hurs")
GFDL45b70 <- terra::rotate(GFDL45b70)
#Pull  vals
GFDL45HUM70<-data.frame(terra::extract(GFDL45b70,allcoords1))
GFDL45HUM70$HUM70 <- rowMeans(GFDL45HUM70[,2:366])
rownames(GFDL45HUM70) <- allcoords1$PlotCN
rm(GFDL45b70)
#HUM75
GFDL45b75 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2075.nc", subds="hurs")
GFDL45b75 <- terra::rotate(GFDL45b75)
#Pull  vals
GFDL45HUM75<-data.frame(terra::extract(GFDL45b75,allcoords1))
GFDL45HUM75$HUM75 <- rowMeans(GFDL45HUM75[,2:366])
rownames(GFDL45HUM75) <- allcoords1$PlotCN
rm(GFDL45b75)
#HUM80
GFDL45b80 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45Hurs2080.nc", subds="hurs")
GFDL45b80 <- terra::rotate(GFDL45b80)
#Pull  vals
GFDL45HUM80<-data.frame(terra::extract(GFDL45b80,allcoords1))
GFDL45HUM80$HUM80 <- rowMeans(GFDL45HUM80[,2:366])
#convert kg/m2/s -> to mm
GFDL45HUM80$HUM80 <- GFDL45HUM80$HUM80
rownames(GFDL45HUM80) <- allcoords1$PlotCN
rm(GFDL45b80)

GFDL45HUMFull<- data.frame(cbind(GFDL45HUM25$HUM25,GFDL45HUM30$HUM30,GFDL45HUM35$HUM35,GFDL45HUM40$HUM40, GFDL45HUM45$HUM45,
                                 GFDL45HUM50$HUM50, GFDL45HUM55$HUM55, GFDL45HUM60$HUM60, GFDL45HUM65$HUM65, GFDL45HUM70$HUM70, GFDL45HUM75$HUM75, GFDL45HUM80$HUM80))

colnames(GFDL45HUMFull)<- c("HUM25","HUM30","HUM35","HUM40", "HUM45","HUM50", "HUM55", "HUM60", "HUM65", "HUM70", "HUM75", "HUM80")

GFDL45HUM1519 <- as.data.frame(rbind(GFDLHUM2015[,c(368,367)], GFDLHUM2016[,c(368,367)], GFDLHUM2017[,c(368,367)], GFDLHUM2018[,c(368,367)],
                                     GFDLHUM2019[,c(368,367)]))


rm(GFDL45HUM25,GFDL45HUM30,GFDL45HUM35,GFDL45HUM40, GFDL45HUM45,
   GFDL45HUM50, GFDL45HUM55, GFDL45HUM60, GFDL45HUM65, GFDL45HUM70, GFDL45HUM75, GFDL45HUM80)
rownames(GFDL45HUMFull) <- allcoords1$PlotCN
#RAD
#2015
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2015.nc", subds="rsds")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLRAD2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLRAD2015$AvgRAD <- rowMeans(GFDLRAD2015[,2:366])
#Add PlotCN
GFDLRAD2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2016.nc", subds="rsds")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLRAD2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLRAD2016$AvgRAD <- rowMeans(GFDLRAD2016[,2:366])
#Add PlotCN
GFDLRAD2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2017.nc", subds="rsds")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLRAD2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLRAD2017$AvgRAD <- rowMeans(GFDLRAD2017[,2:366])
#Add PlotCN
GFDLRAD2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2018.nc", subds="rsds")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLRAD2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLRAD2018$AvgRAD <- rowMeans(GFDLRAD2018[,2:366])
#Add PlotCN
GFDLRAD2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL45")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2019.nc", subds="rsds")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLRAD2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLRAD2019$AvgRAD <- rowMeans(GFDLRAD2019[,2:366])
#Add PlotCN
GFDLRAD2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(GFDL2019)

#RAD25
GFDL45b25 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2025.nc", subds="rsds")
GFDL45b25 <- terra::rotate(GFDL45b25)
#Pull  vals
GFDL45RAD25<-data.frame(terra::extract(GFDL45b25,allcoords1))
GFDL45RAD25$RAD25 <- rowMeans(GFDL45RAD25[,2:366])
rownames(GFDL45RAD25) <- allcoords1$PlotCN
rm(GFDL45b25)
#RAD30
GFDL45b30 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2030.nc", subds="rsds")
GFDL45b30 <- terra::rotate(GFDL45b30)
#Pull  vals
GFDL45RAD30<-data.frame(terra::extract(GFDL45b30,allcoords1))
GFDL45RAD30$RAD30 <- rowMeans(GFDL45RAD30[,2:366])
rownames(GFDL45RAD30) <- allcoords1$PlotCN
rm(GFDL45b30)
#RAD35
GFDL45b35 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2035.nc", subds="rsds")
GFDL45b35 <- terra::rotate(GFDL45b35)
#Pull  vals
GFDL45RAD35<-data.frame(terra::extract(GFDL45b35,allcoords1))
GFDL45RAD35$RAD35 <- rowMeans(GFDL45RAD35[,2:366])
rownames(GFDL45RAD35) <- allcoords1$PlotCN
rm(GFDL45b35)
#RAD40
GFDL45b40 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2040.nc", subds="rsds")
GFDL45b40 <- terra::rotate(GFDL45b40)
#Pull  vals
GFDL45RAD40<-data.frame(terra::extract(GFDL45b40,allcoords1))
GFDL45RAD40$RAD40 <- rowMeans(GFDL45RAD40[,2:366])
rownames(GFDL45RAD40) <- allcoords1$PlotCN
rm(GFDL45b40)
#RAD45
GFDL45b45 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2045.nc", subds="rsds")
GFDL45b45 <- terra::rotate(GFDL45b45)
#Pull  vals
GFDL45RAD45<-data.frame(terra::extract(GFDL45b45,allcoords1))
GFDL45RAD45$RAD45 <- rowMeans(GFDL45RAD45[,2:366])
rownames(GFDL45RAD45) <- allcoords1$PlotCN
rm(GFDL45b45)

#RAD50
GFDL45b50 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2050.nc", subds="rsds")
GFDL45b50 <- terra::rotate(GFDL45b50)
#Pull  vals
GFDL45RAD50<-data.frame(terra::extract(GFDL45b50,allcoords1))
GFDL45RAD50$RAD50 <- rowMeans(GFDL45RAD50[,2:366])
rownames(GFDL45RAD50) <- allcoords1$PlotCN
rm(GFDL45b50)
#RAD55
GFDL45b55 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2055.nc", subds="rsds")
GFDL45b55 <- terra::rotate(GFDL45b55)
#Pull  vals
GFDL45RAD55<-data.frame(terra::extract(GFDL45b55,allcoords1))
GFDL45RAD55$RAD55 <- rowMeans(GFDL45RAD55[,2:366])
rownames(GFDL45RAD55) <- allcoords1$PlotCN
rm(GFDL45b55)
#RAD60
GFDL45b60 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2060.nc", subds="rsds")
GFDL45b60 <- terra::rotate(GFDL45b60)
#Pull  vals
GFDL45RAD60<-data.frame(terra::extract(GFDL45b60,allcoords1))
GFDL45RAD60$RAD60 <- rowMeans(GFDL45RAD60[,2:366])
rownames(GFDL45RAD60) <- allcoords1$PlotCN
rm(GFDL45b60)
#RAD65
GFDL45b65 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2065.nc", subds="rsds")
GFDL45b65 <- terra::rotate(GFDL45b65)
#Pull  vals
GFDL45RAD65<-data.frame(terra::extract(GFDL45b65,allcoords1))
GFDL45RAD65$RAD65 <- rowMeans(GFDL45RAD65[,2:366])
rownames(GFDL45RAD65) <- allcoords1$PlotCN
rm(GFDL45b65)
#RAD70
GFDL45b70 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2070.nc", subds="rsds")
GFDL45b70 <- terra::rotate(GFDL45b70)
#Pull  vals
GFDL45RAD70<-data.frame(terra::extract(GFDL45b70,allcoords1))
GFDL45RAD70$RAD70 <- rowMeans(GFDL45RAD70[,2:366])
rownames(GFDL45RAD70) <- allcoords1$PlotCN
rm(GFDL45b70)
#RAD75
GFDL45b75 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2075.nc", subds="rsds")
GFDL45b75 <- terra::rotate(GFDL45b75)
#Pull  vals
GFDL45RAD75<-data.frame(terra::extract(GFDL45b75,allcoords1))
GFDL45RAD75$RAD75 <- rowMeans(GFDL45RAD75[,2:366])
rownames(GFDL45RAD75) <- allcoords1$PlotCN
rm(GFDL45b75)
#RAD80
GFDL45b80 <- rast("~/Desktop/NASAClim/GFDL45/GFDL45RAD2080.nc", subds="rsds")
GFDL45b80 <- terra::rotate(GFDL45b80)
#Pull  vals
GFDL45RAD80<-data.frame(terra::extract(GFDL45b80,allcoords1))
GFDL45RAD80$RAD80 <- rowMeans(GFDL45RAD80[,2:366])
#convert kg/m2/s -> to mm
GFDL45RAD80$RAD80 <- GFDL45RAD80$RAD80
rownames(GFDL45RAD80) <- allcoords1$PlotCN
rm(GFDL45b80)

GFDL45RADFull<- data.frame(cbind(GFDL45RAD25$RAD25,GFDL45RAD30$RAD30,GFDL45RAD35$RAD35,GFDL45RAD40$RAD40, GFDL45RAD45$RAD45,
                                 GFDL45RAD50$RAD50, GFDL45RAD55$RAD55, GFDL45RAD60$RAD60, GFDL45RAD65$RAD65, GFDL45RAD70$RAD70, GFDL45RAD75$RAD75, GFDL45RAD80$RAD80))

colnames(GFDL45RADFull)<- c("RAD25","RAD30","RAD35","RAD40", "RAD45","RAD50", "RAD55", "RAD60", "RAD65", "RAD70", "RAD75", "RAD80")


GFDL45RAD1519 <- as.data.frame(rbind(GFDLRAD2015[,c(368,367)], GFDLRAD2016[,c(368,367)], GFDLRAD2017[,c(368,367)], GFDLRAD2018[,c(368,367)],
                                     GFDLRAD2019[,c(368,367)]))


rm(GFDL45RAD25,GFDL45RAD30,GFDL45RAD35,GFDL45RAD40, GFDL45RAD45,
   GFDL45RAD50, GFDL45RAD55, GFDL45RAD60, GFDL45RAD65, GFDL45RAD70, GFDL45RAD75, GFDL45RAD80)
rownames(GFDL45RADFull) <- allcoords1$PlotCN

#GFDL85
#Temp
#2015
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2015.nc", subds="tas")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLTEMP2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLTEMP2015$AvgTemp <- rowMeans(GFDLTEMP2015[,2:366])
#Add PlotCN
GFDLTEMP2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert K  to C
GFDLTEMP2015$AvgTemp <- GFDLTEMP2015$AvgTemp-273.15
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2016.nc", subds="tas")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLTEMP2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLTEMP2016$AvgTemp <- rowMeans(GFDLTEMP2016[,2:366])
#Add PlotCN
GFDLTEMP2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert K  to C
GFDLTEMP2016$AvgTemp <- GFDLTEMP2016$AvgTemp-273.15
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2017.nc", subds="tas")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLTEMP2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLTEMP2017$AvgTemp <- rowMeans(GFDLTEMP2017[,2:366])
#Add PlotCN
GFDLTEMP2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert K  to C
GFDLTEMP2017$AvgTemp <- GFDLTEMP2017$AvgTemp-273.15
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2018.nc", subds="tas")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLTEMP2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLTEMP2018$AvgTemp <- rowMeans(GFDLTEMP2018[,2:366])
#Add PlotCN
GFDLTEMP2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert K  to C
GFDLTEMP2018$AvgTemp <- GFDLTEMP2018$AvgTemp-273.15
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2019.nc", subds="tas")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLTEMP2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLTEMP2019$AvgTemp <- rowMeans(GFDLTEMP2019[,2:366])
#Add PlotCN
GFDLTEMP2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert K  to C
GFDLTEMP2019$AvgTemp <- GFDLTEMP2019$AvgTemp-273.15
rm(GFDL2019)

#Temp25
GFDL85b25 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2025.nc", subds="tas")
GFDL85b25 <- terra::rotate(GFDL85b25)
#Pull  vals
GFDL85TEMP25<-data.frame(terra::extract(GFDL85b25,allcoords1))
GFDL85TEMP25$Temp25 <- rowMeans(GFDL85TEMP25[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP25$Temp25 <- GFDL85TEMP25$Temp25-273.15
rownames(GFDL85TEMP25) <- allcoords1$PlotCN
rm(GFDL85b25)
#Temp30
GFDL85b30 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2030.nc", subds="tas")
GFDL85b30 <- terra::rotate(GFDL85b30)
#Pull  vals
GFDL85TEMP30<-data.frame(terra::extract(GFDL85b30,allcoords1))
GFDL85TEMP30$Temp30 <- rowMeans(GFDL85TEMP30[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP30$Temp30 <- GFDL85TEMP30$Temp30-273.15
rownames(GFDL85TEMP30) <- allcoords1$PlotCN
rm(GFDL85b30)
#Temp35
GFDL85b35 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2035.nc", subds="tas")
GFDL85b35 <- terra::rotate(GFDL85b35)
#Pull  vals
GFDL85TEMP35<-data.frame(terra::extract(GFDL85b35,allcoords1))
GFDL85TEMP35$Temp35 <- rowMeans(GFDL85TEMP35[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP35$Temp35 <- GFDL85TEMP35$Temp35-273.15
rownames(GFDL85TEMP35) <- allcoords1$PlotCN
rm(GFDL85b35)
#Temp40
GFDL85b40 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2040.nc", subds="tas")
GFDL85b40 <- terra::rotate(GFDL85b40)
#Pull  vals
GFDL85TEMP40<-data.frame(terra::extract(GFDL85b40,allcoords1))
GFDL85TEMP40$Temp40 <- rowMeans(GFDL85TEMP40[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP40$Temp40 <- GFDL85TEMP40$Temp40-273.15
rownames(GFDL85TEMP40) <- allcoords1$PlotCN
rm(GFDL85b40)
#Temp45
GFDL85b45 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2045.nc", subds="tas")
GFDL85b45 <- terra::rotate(GFDL85b45)
#Pull  vals
GFDL85TEMP45<-data.frame(terra::extract(GFDL85b45,allcoords1))
GFDL85TEMP45$Temp45 <- rowMeans(GFDL85TEMP45[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP45$Temp45 <- GFDL85TEMP45$Temp45-273.15
rownames(GFDL85TEMP45) <- allcoords1$PlotCN
rm(GFDL85b45)

#Temp50
GFDL85b50 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2050.nc", subds="tas")
GFDL85b50 <- terra::rotate(GFDL85b50)
#Pull  vals
GFDL85TEMP50<-data.frame(terra::extract(GFDL85b50,allcoords1))
GFDL85TEMP50$Temp50 <- rowMeans(GFDL85TEMP50[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP50$Temp50 <- GFDL85TEMP50$Temp50-273.15
rownames(GFDL85TEMP50) <- allcoords1$PlotCN
rm(GFDL85b50)
#Temp55
GFDL85b55 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2055.nc", subds="tas")
GFDL85b55 <- terra::rotate(GFDL85b55)
#Pull  vals
GFDL85TEMP55<-data.frame(terra::extract(GFDL85b55,allcoords1))
GFDL85TEMP55$Temp55 <- rowMeans(GFDL85TEMP55[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP55$Temp55 <- GFDL85TEMP55$Temp55-273.15
rownames(GFDL85TEMP55) <- allcoords1$PlotCN
rm(GFDL85b55)
#Temp60
GFDL85b60 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2060.nc", subds="tas")
GFDL85b60 <- terra::rotate(GFDL85b60)
#Pull  vals
GFDL85TEMP60<-data.frame(terra::extract(GFDL85b60,allcoords1))
GFDL85TEMP60$Temp60 <- rowMeans(GFDL85TEMP60[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP60$Temp60 <- GFDL85TEMP60$Temp60-273.15
rownames(GFDL85TEMP60) <- allcoords1$PlotCN
rm(GFDL85b60)
#Temp65
GFDL85b65 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2065.nc", subds="tas")
GFDL85b65 <- terra::rotate(GFDL85b65)
#Pull  vals
GFDL85TEMP65<-data.frame(terra::extract(GFDL85b65,allcoords1))
GFDL85TEMP65$Temp65 <- rowMeans(GFDL85TEMP65[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP65$Temp65 <- GFDL85TEMP65$Temp65-273.15
rownames(GFDL85TEMP65) <- allcoords1$PlotCN
rm(GFDL85b65)
#Temp70
GFDL85b70 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2070.nc", subds="tas")
GFDL85b70 <- terra::rotate(GFDL85b70)
#Pull  vals
GFDL85TEMP70<-data.frame(terra::extract(GFDL85b70,allcoords1))
GFDL85TEMP70$Temp70 <- rowMeans(GFDL85TEMP70[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP70$Temp70 <- GFDL85TEMP70$Temp70-273.15
rownames(GFDL85TEMP70) <- allcoords1$PlotCN
rm(GFDL85b70)
#Temp75
GFDL85b75 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2075.nc", subds="tas")
GFDL85b75 <- terra::rotate(GFDL85b75)
#Pull  vals
GFDL85TEMP75<-data.frame(terra::extract(GFDL85b75,allcoords1))
GFDL85TEMP75$Temp75 <- rowMeans(GFDL85TEMP75[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP75$Temp75 <- GFDL85TEMP75$Temp75-273.15
rownames(GFDL85TEMP75) <- allcoords1$PlotCN
rm(GFDL85b75)
#Temp80
GFDL85b80 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Ta2080.nc", subds="tas")
GFDL85b80 <- terra::rotate(GFDL85b80)
#Pull  vals
GFDL85TEMP80<-data.frame(terra::extract(GFDL85b80,allcoords1))
GFDL85TEMP80$Temp80 <- rowMeans(GFDL85TEMP80[,2:366])
#convert kg/m2/s -> to mm
GFDL85TEMP80$Temp80 <- GFDL85TEMP80$Temp80-273.15
rownames(GFDL85TEMP80) <- allcoords1$PlotCN
rm(GFDL85b80)

GFDL85TempFull<- data.frame(cbind(GFDL85TEMP25$Temp25,GFDL85TEMP30$Temp30,GFDL85TEMP35$Temp35,GFDL85TEMP40$Temp40, GFDL85TEMP45$Temp45,
                                  GFDL85TEMP50$Temp50, GFDL85TEMP55$Temp55, GFDL85TEMP60$Temp60, GFDL85TEMP65$Temp65, GFDL85TEMP70$Temp70, GFDL85TEMP75$Temp75, GFDL85TEMP80$Temp80))

colnames(GFDL85TempFull)<- c("Temp25","Temp30","Temp35","Temp40", "Temp45","Temp50", "Temp55", "Temp60", "Temp65", "Temp70", "Temp75", "Temp80")

GFDL85TEMP1519 <- as.data.frame(rbind(GFDLTEMP2015[,c(368,367)], GFDLTEMP2016[,c(368,367)], GFDLTEMP2017[,c(368,367)], GFDLTEMP2018[,c(368,367)],
                                      GFDLTEMP2019[,c(368,367)]))


rm(GFDL85TEMP25,GFDL85TEMP30,GFDL85TEMP35,GFDL85TEMP40, GFDL85TEMP45,
   GFDL85TEMP50, GFDL85TEMP55, GFDL85TEMP60, GFDL85TEMP65, GFDL85TEMP70, GFDL85TEMP75, GFDL85TEMP80)
rownames(GFDL85TempFull) <- allcoords1$PlotCN

#Prec
#2015
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2015.nc", subds="pr")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLPREC2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLPREC2015$PrecSum <- rowSums(GFDLPrec2015[,2:366])
#Add PlotCN
GFDLPREC2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert to mm
GFDLPREC2015$PrecSum <- GFDLPREC2015$PrecSum*86400
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2016.nc", subds="pr")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLPREC2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLPREC2016$PrecSum <- rowSums(GFDLPrec2016[,2:366])
#Add PlotCN
GFDLPREC2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert to mm
GFDLPREC2016$PrecSum <- GFDLPREC2016$PrecSum*86400
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2017.nc", subds="pr")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLPREC2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLPREC2017$PrecSum <- rowSums(GFDLPrec2017[,2:366])
#Add PlotCN
GFDLPREC2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert to mm
GFDLPREC2017$PrecSum <- GFDLPREC2017$PrecSum*86400
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2018.nc", subds="pr")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLPREC2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLPREC2018$PrecSum <- rowSums(GFDLPrec2018[,2:366])
#Add PlotCN
GFDLPREC2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert to mm
GFDLPREC2018$PrecSum <- GFDLPREC2018$PrecSum*86400
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2019.nc", subds="pr")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLPREC2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLPREC2019$PrecSum <- rowSums(GFDLPrec2019[,2:366])
#Add PlotCN
GFDLPREC2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert to mm
GFDLPREC2019$PrecSum <- GFDLPREC2019$PrecSum*86400
rm(GFDL2019)

#Prec25
GFDL85b25 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2025.nc", subds="pr")
GFDL85b25 <- terra::rotate(GFDL85b25)
#Pull  vals
GFDL85PREC25<-data.frame(terra::extract(GFDL85b25,allcoords1))
GFDL85PREC25$Prec25 <- rowSums(GFDL85Prec25[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC25$Prec25 <- GFDL85PREC25$Prec25*86400
rownames(GFDL85PREC25) <- allcoords1$PlotCN
rm(GFDL85b25)
#Prec30
GFDL85b30 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2030.nc", subds="pr")
GFDL85b30 <- terra::rotate(GFDL85b30)
#Pull  vals
GFDL85PREC30<-data.frame(terra::extract(GFDL85b30,allcoords1))
GFDL85PREC30$Prec30 <- rowSums(GFDL85Prec30[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC30$Prec30 <- GFDL85PREC30$Prec30*86400
rownames(GFDL85PREC30) <- allcoords1$PlotCN
rm(GFDL85b30)
#Prec35
GFDL85b35 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2035.nc", subds="pr")
GFDL85b35 <- terra::rotate(GFDL85b35)
#Pull  vals
GFDL85PREC35<-data.frame(terra::extract(GFDL85b35,allcoords1))
GFDL85PREC35$Prec35 <- rowSums(GFDL85Prec35[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC35$Prec35 <- GFDL85PREC35$Prec35*86400
rownames(GFDL85PREC35) <- allcoords1$PlotCN
rm(GFDL85b35)
#Prec40
GFDL85b40 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2040.nc", subds="pr")
GFDL85b40 <- terra::rotate(GFDL85b40)
#Pull  vals
GFDL85PREC40<-data.frame(terra::extract(GFDL85b40,allcoords1))
GFDL85PREC40$Prec40 <- rowSums(GFDL85Prec40[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC40$Prec40 <- GFDL85PREC40$Prec40*86400
rownames(GFDL85PREC40) <- allcoords1$PlotCN
rm(GFDL85b40)
#Prec45
GFDL85b45 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2045.nc", subds="pr")
GFDL85b45 <- terra::rotate(GFDL85b45)
#Pull  vals
GFDL85PREC45<-data.frame(terra::extract(GFDL85b45,allcoords1))
GFDL85PREC45$Prec45 <- rowSums(GFDL85Prec45[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC45$Prec45 <- GFDL85PREC45$Prec45*86400
rownames(GFDL85PREC45) <- allcoords1$PlotCN
rm(GFDL85b45)

#Prec50
GFDL85b50 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2050.nc", subds="pr")
GFDL85b50 <- terra::rotate(GFDL85b50)
#Pull  vals
GFDL85PREC50<-data.frame(terra::extract(GFDL85b50,allcoords1))
GFDL85PREC50$Prec50 <- rowSums(GFDL85Prec50[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC50$Prec50 <- GFDL85PREC50$Prec50*86400
rownames(GFDL85PREC50) <- allcoords1$PlotCN
rm(GFDL85b50)
#Prec55
GFDL85b55 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2055.nc", subds="pr")
GFDL85b55 <- terra::rotate(GFDL85b55)
#Pull  vals
GFDL85PREC55<-data.frame(terra::extract(GFDL85b55,allcoords1))
GFDL85PREC55$Prec55 <- rowSums(GFDL85Prec55[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC55$Prec55 <- GFDL85PREC55$Prec55*86400
rownames(GFDL85PREC55) <- allcoords1$PlotCN
rm(GFDL85b55)
#Prec60
GFDL85b60 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2060.nc", subds="pr")
GFDL85b60 <- terra::rotate(GFDL85b60)
#Pull  vals
GFDL85PREC60<-data.frame(terra::extract(GFDL85b60,allcoords1))
GFDL85PREC60$Prec60 <- rowSums(GFDL85Prec60[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC60$Prec60 <- GFDL85PREC60$Prec60*86400
rownames(GFDL85PREC60) <- allcoords1$PlotCN
rm(GFDL85b60)
#Prec65
GFDL85b65 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2065.nc", subds="pr")
GFDL85b65 <- terra::rotate(GFDL85b65)
#Pull  vals
GFDL85PREC65<-data.frame(terra::extract(GFDL85b65,allcoords1))
GFDL85PREC65$Prec65 <- rowSums(GFDL85Prec65[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC65$Prec65 <- GFDL85PREC65$Prec65*86400
rownames(GFDL85PREC65) <- allcoords1$PlotCN
rm(GFDL85b65)
#Prec70
GFDL85b70 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2070.nc", subds="pr")
GFDL85b70 <- terra::rotate(GFDL85b70)
#Pull  vals
GFDL85PREC70<-data.frame(terra::extract(GFDL85b70,allcoords1))
GFDL85PREC70$Prec70 <- rowSums(GFDL85Prec70[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC70$Prec70 <- GFDL85PREC70$Prec70*86400
rownames(GFDL85PREC70) <- allcoords1$PlotCN
rm(GFDL85b70)
#Prec75
GFDL85b75 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2075.nc", subds="pr")
GFDL85b75 <- terra::rotate(GFDL85b75)
#Pull  vals
GFDL85PREC75<-data.frame(terra::extract(GFDL85b75,allcoords1))
GFDL85PREC75$Prec75 <- rowSums(GFDL85Prec75[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC75$Prec75 <- GFDL85PREC75$Prec75*86400
rownames(GFDL85PREC75) <- allcoords1$PlotCN
rm(GFDL85b75)
#Prec80
GFDL85b80 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Pr2080.nc", subds="pr")
GFDL85b80 <- terra::rotate(GFDL85b80)
#Pull  vals
GFDL85PREC80<-data.frame(terra::extract(GFDL85b80,allcoords1))
GFDL85PREC80$Prec80 <- rowSums(GFDL85Prec80[,2:366])
#convert kg/m2/s -> to mm
GFDL85PREC80$Prec80 <- GFDL85PREC80$Prec80*86400
rownames(GFDL85PREC80) <- allcoords1$PlotCN
rm(GFDL85b80)

GFDL85PrecFull<- data.frame(cbind(GFDL85PREC25$Prec25,GFDL85PREC30$Prec30,GFDL85PREC35$Prec35,GFDL85PREC40$Prec40, GFDL85PREC45$Prec45,
                                  GFDL85PREC50$Prec50, GFDL85PREC55$Prec55, GFDL85PREC60$Prec60, GFDL85PREC65$Prec65, GFDL85PREC70$Prec70, GFDL85PREC75$Prec75, GFDL85PREC80$Prec80))

colnames(GFDL85PrecFull)<- c("Prec25","Prec30","Prec35","Prec40", "Prec45","Prec50", "Prec55", "Prec60", "Prec65", "Prec70", "Prec75", "Prec80")

GFDL85PREC1519 <- as.data.frame(rbind(GFDLPREC2015[,c(368,367)], GFDLPREC2016[,c(368,367)], GFDLPREC2017[,c(368,367)], GFDLPREC2018[,c(368,367)],
                                      GFDLPREC2019[,c(368,367)]))


rm(GFDL85PREC25,GFDL85PREC30,GFDL85PREC35,GFDL85PREC40, GFDL85PREC45,
   GFDL85PREC50, GFDL85PREC55, GFDL85PREC60, GFDL85PREC65, GFDL85PREC70, GFDL85PREC75, GFDL85PREC80)
rownames(GFDL85PrecFull) <- allcoords1$PlotCN

#HUM
#2015
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2015.nc", subds="hurs")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLHUM2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLHUM2015$AvgHUM <- rowMeans(GFDLHUM2015[,2:366])
#Add PlotCN
GFDLHUM2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2016.nc", subds="hurs")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLHUM2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLHUM2016$AvgHUM <- rowMeans(GFDLHUM2016[,2:366])
#Add PlotCN
GFDLHUM2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2017.nc", subds="hurs")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLHUM2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLHUM2017$AvgHUM <- rowMeans(GFDLHUM2017[,2:366])
#Add PlotCN
GFDLHUM2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2018.nc", subds="hurs")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLHUM2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLHUM2018$AvgHUM <- rowMeans(GFDLHUM2018[,2:366])
#Add PlotCN
GFDLHUM2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2019.nc", subds="hurs")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLHUM2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLHUM2019$AvgHUM <- rowMeans(GFDLHUM2019[,2:366])
#Add PlotCN
GFDLHUM2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(GFDL2019)

#HUM25
GFDL85b25 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2025.nc", subds="hurs")
GFDL85b25 <- terra::rotate(GFDL85b25)
#Pull  vals
GFDL85HUM25<-data.frame(terra::extract(GFDL85b25,allcoords1))
GFDL85HUM25$HUM25 <- rowMeans(GFDL85HUM25[,2:366])
rownames(GFDL85HUM25) <- allcoords1$PlotCN
rm(GFDL85b25)
#HUM30
GFDL85b30 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2030.nc", subds="hurs")
GFDL85b30 <- terra::rotate(GFDL85b30)
#Pull  vals
GFDL85HUM30<-data.frame(terra::extract(GFDL85b30,allcoords1))
GFDL85HUM30$HUM30 <- rowMeans(GFDL85HUM30[,2:366])
rownames(GFDL85HUM30) <- allcoords1$PlotCN
rm(GFDL85b30)
#HUM35
GFDL85b35 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2035.nc", subds="hurs")
GFDL85b35 <- terra::rotate(GFDL85b35)
#Pull  vals
GFDL85HUM35<-data.frame(terra::extract(GFDL85b35,allcoords1))
GFDL85HUM35$HUM35 <- rowMeans(GFDL85HUM35[,2:366])
rownames(GFDL85HUM35) <- allcoords1$PlotCN
rm(GFDL85b35)
#HUM40
GFDL85b40 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2040.nc", subds="hurs")
GFDL85b40 <- terra::rotate(GFDL85b40)
#Pull  vals
GFDL85HUM40<-data.frame(terra::extract(GFDL85b40,allcoords1))
GFDL85HUM40$HUM40 <- rowMeans(GFDL85HUM40[,2:366])
rownames(GFDL85HUM40) <- allcoords1$PlotCN
rm(GFDL85b40)
#HUM45
GFDL85b45 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2045.nc", subds="hurs")
GFDL85b45 <- terra::rotate(GFDL85b45)
#Pull  vals
GFDL85HUM45<-data.frame(terra::extract(GFDL85b45,allcoords1))
GFDL85HUM45$HUM45 <- rowMeans(GFDL85HUM45[,2:366])
rownames(GFDL85HUM45) <- allcoords1$PlotCN
rm(GFDL85b45)

#HUM50
GFDL85b50 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2050.nc", subds="hurs")
GFDL85b50 <- terra::rotate(GFDL85b50)
#Pull  vals
GFDL85HUM50<-data.frame(terra::extract(GFDL85b50,allcoords1))
GFDL85HUM50$HUM50 <- rowMeans(GFDL85HUM50[,2:366])
rownames(GFDL85HUM50) <- allcoords1$PlotCN
rm(GFDL85b50)
#HUM55
GFDL85b55 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2055.nc", subds="hurs")
GFDL85b55 <- terra::rotate(GFDL85b55)
#Pull  vals
GFDL85HUM55<-data.frame(terra::extract(GFDL85b55,allcoords1))
GFDL85HUM55$HUM55 <- rowMeans(GFDL85HUM55[,2:366])
rownames(GFDL85HUM55) <- allcoords1$PlotCN
rm(GFDL85b55)
#HUM60
GFDL85b60 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2060.nc", subds="hurs")
GFDL85b60 <- terra::rotate(GFDL85b60)
#Pull  vals
GFDL85HUM60<-data.frame(terra::extract(GFDL85b60,allcoords1))
GFDL85HUM60$HUM60 <- rowMeans(GFDL85HUM60[,2:366])
rownames(GFDL85HUM60) <- allcoords1$PlotCN
rm(GFDL85b60)
#HUM65
GFDL85b65 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2065.nc", subds="hurs")
GFDL85b65 <- terra::rotate(GFDL85b65)
#Pull  vals
GFDL85HUM65<-data.frame(terra::extract(GFDL85b65,allcoords1))
GFDL85HUM65$HUM65 <- rowMeans(GFDL85HUM65[,2:366])
rownames(GFDL85HUM65) <- allcoords1$PlotCN
rm(GFDL85b65)
#HUM70
GFDL85b70 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2070.nc", subds="hurs")
GFDL85b70 <- terra::rotate(GFDL85b70)
#Pull  vals
GFDL85HUM70<-data.frame(terra::extract(GFDL85b70,allcoords1))
GFDL85HUM70$HUM70 <- rowMeans(GFDL85HUM70[,2:366])
rownames(GFDL85HUM70) <- allcoords1$PlotCN
rm(GFDL85b70)
#HUM75
GFDL85b75 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2075.nc", subds="hurs")
GFDL85b75 <- terra::rotate(GFDL85b75)
#Pull  vals
GFDL85HUM75<-data.frame(terra::extract(GFDL85b75,allcoords1))
GFDL85HUM75$HUM75 <- rowMeans(GFDL85HUM75[,2:366])
rownames(GFDL85HUM75) <- allcoords1$PlotCN
rm(GFDL85b75)
#HUM80
GFDL85b80 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85Hurs2080.nc", subds="hurs")
GFDL85b80 <- terra::rotate(GFDL85b80)
#Pull  vals
GFDL85HUM80<-data.frame(terra::extract(GFDL85b80,allcoords1))
GFDL85HUM80$HUM80 <- rowMeans(GFDL85HUM80[,2:366])
#convert kg/m2/s -> to mm
GFDL85HUM80$HUM80 <- GFDL85HUM80$HUM80
rownames(GFDL85HUM80) <- allcoords1$PlotCN
rm(GFDL85b80)

GFDL85HUMFull<- data.frame(cbind(GFDL85HUM25$HUM25,GFDL85HUM30$HUM30,GFDL85HUM35$HUM35,GFDL85HUM40$HUM40, GFDL85HUM45$HUM45,
                                 GFDL85HUM50$HUM50, GFDL85HUM55$HUM55, GFDL85HUM60$HUM60, GFDL85HUM65$HUM65, GFDL85HUM70$HUM70, GFDL85HUM75$HUM75, GFDL85HUM80$HUM80))

colnames(GFDL85HUMFull)<- c("HUM25","HUM30","HUM35","HUM40", "HUM45","HUM50", "HUM55", "HUM60", "HUM65", "HUM70", "HUM75", "HUM80")

GFDL85HUM1519 <- as.data.frame(rbind(GFDLHUM2015[,c(368,367)], GFDLHUM2016[,c(368,367)], GFDLHUM2017[,c(368,367)], GFDLHUM2018[,c(368,367)],
                                     GFDLHUM2019[,c(368,367)]))


rm(GFDL85HUM25,GFDL85HUM30,GFDL85HUM35,GFDL85HUM40, GFDL85HUM45,
   GFDL85HUM50, GFDL85HUM55, GFDL85HUM60, GFDL85HUM65, GFDL85HUM70, GFDL85HUM75, GFDL85HUM80)
rownames(GFDL85HUMFull) <- allcoords1$PlotCN
#RAD
#2015
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2015  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2015.nc", subds="rsds")
GFDL2015  <- terra::rotate(GFDL2015)
#Pull  vals
GFDLRAD2015<-data.frame(terra::extract(GFDL2015 ,allcoords[allcoords$Year == 2015,]))
GFDLRAD2015$AvgRAD <- rowMeans(GFDLRAD2015[,2:366])
#Add PlotCN
GFDLRAD2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(GFDL2015)
#2016
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2016  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2016.nc", subds="rsds")
GFDL2016  <- terra::rotate(GFDL2016)
#Pull  vals
GFDLRAD2016<-data.frame(terra::extract(GFDL2016 ,allcoords[allcoords$Year == 2016,]))
GFDLRAD2016$AvgRAD <- rowMeans(GFDLRAD2016[,2:366])
#Add PlotCN
GFDLRAD2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(GFDL2016)
#2017
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2017  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2017.nc", subds="rsds")
GFDL2017  <- terra::rotate(GFDL2017)
#Pull  vals
GFDLRAD2017<-data.frame(terra::extract(GFDL2017 ,allcoords[allcoords$Year == 2017,]))
GFDLRAD2017$AvgRAD <- rowMeans(GFDLRAD2017[,2:366])
#Add PlotCN
GFDLRAD2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(GFDL2017)
#2018
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2018  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2018.nc", subds="rsds")
GFDL2018  <- terra::rotate(GFDL2018)
#Pull  vals
GFDLRAD2018<-data.frame(terra::extract(GFDL2018 ,allcoords[allcoords$Year == 2018,]))
GFDLRAD2018$AvgRAD <- rowMeans(GFDLRAD2018[,2:366])
#Add PlotCN
GFDLRAD2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(GFDL2018)
#2019
setwd("~/Desktop/NASAClim/GFDL85")
GFDL2019  <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2019.nc", subds="rsds")
GFDL2019  <- terra::rotate(GFDL2019)
#Pull  vals
GFDLRAD2019<-data.frame(terra::extract(GFDL2019 ,allcoords[allcoords$Year == 2019,]))
GFDLRAD2019$AvgRAD <- rowMeans(GFDLRAD2019[,2:366])
#Add PlotCN
GFDLRAD2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(GFDL2019)

#RAD25
GFDL85b25 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2025.nc", subds="rsds")
GFDL85b25 <- terra::rotate(GFDL85b25)
#Pull  vals
GFDL85RAD25<-data.frame(terra::extract(GFDL85b25,allcoords1))
GFDL85RAD25$RAD25 <- rowMeans(GFDL85RAD25[,2:366])
rownames(GFDL85RAD25) <- allcoords1$PlotCN
rm(GFDL85b25)
#RAD30
GFDL85b30 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2030.nc", subds="rsds")
GFDL85b30 <- terra::rotate(GFDL85b30)
#Pull  vals
GFDL85RAD30<-data.frame(terra::extract(GFDL85b30,allcoords1))
GFDL85RAD30$RAD30 <- rowMeans(GFDL85RAD30[,2:366])
rownames(GFDL85RAD30) <- allcoords1$PlotCN
rm(GFDL85b30)
#RAD35
GFDL85b35 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2035.nc", subds="rsds")
GFDL85b35 <- terra::rotate(GFDL85b35)
#Pull  vals
GFDL85RAD35<-data.frame(terra::extract(GFDL85b35,allcoords1))
GFDL85RAD35$RAD35 <- rowMeans(GFDL85RAD35[,2:366])
rownames(GFDL85RAD35) <- allcoords1$PlotCN
rm(GFDL85b35)
#RAD40
GFDL85b40 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2040.nc", subds="rsds")
GFDL85b40 <- terra::rotate(GFDL85b40)
#Pull  vals
GFDL85RAD40<-data.frame(terra::extract(GFDL85b40,allcoords1))
GFDL85RAD40$RAD40 <- rowMeans(GFDL85RAD40[,2:366])
rownames(GFDL85RAD40) <- allcoords1$PlotCN
rm(GFDL85b40)
#RAD45
GFDL85b45 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2045.nc", subds="rsds")
GFDL85b45 <- terra::rotate(GFDL85b45)
#Pull  vals
GFDL85RAD45<-data.frame(terra::extract(GFDL85b45,allcoords1))
GFDL85RAD45$RAD45 <- rowMeans(GFDL85RAD45[,2:366])
rownames(GFDL85RAD45) <- allcoords1$PlotCN
rm(GFDL85b45)

#RAD50
GFDL85b50 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2050.nc", subds="rsds")
GFDL85b50 <- terra::rotate(GFDL85b50)
#Pull  vals
GFDL85RAD50<-data.frame(terra::extract(GFDL85b50,allcoords1))
GFDL85RAD50$RAD50 <- rowMeans(GFDL85RAD50[,2:366])
rownames(GFDL85RAD50) <- allcoords1$PlotCN
rm(GFDL85b50)
#RAD55
GFDL85b55 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2055.nc", subds="rsds")
GFDL85b55 <- terra::rotate(GFDL85b55)
#Pull  vals
GFDL85RAD55<-data.frame(terra::extract(GFDL85b55,allcoords1))
GFDL85RAD55$RAD55 <- rowMeans(GFDL85RAD55[,2:366])
rownames(GFDL85RAD55) <- allcoords1$PlotCN
rm(GFDL85b55)
#RAD60
GFDL85b60 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2060.nc", subds="rsds")
GFDL85b60 <- terra::rotate(GFDL85b60)
#Pull  vals
GFDL85RAD60<-data.frame(terra::extract(GFDL85b60,allcoords1))
GFDL85RAD60$RAD60 <- rowMeans(GFDL85RAD60[,2:366])
rownames(GFDL85RAD60) <- allcoords1$PlotCN
rm(GFDL85b60)
#RAD65
GFDL85b65 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2065.nc", subds="rsds")
GFDL85b65 <- terra::rotate(GFDL85b65)
#Pull  vals
GFDL85RAD65<-data.frame(terra::extract(GFDL85b65,allcoords1))
GFDL85RAD65$RAD65 <- rowMeans(GFDL85RAD65[,2:366])
rownames(GFDL85RAD65) <- allcoords1$PlotCN
rm(GFDL85b65)
#RAD70
GFDL85b70 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2070.nc", subds="rsds")
GFDL85b70 <- terra::rotate(GFDL85b70)
#Pull  vals
GFDL85RAD70<-data.frame(terra::extract(GFDL85b70,allcoords1))
GFDL85RAD70$RAD70 <- rowMeans(GFDL85RAD70[,2:366])
rownames(GFDL85RAD70) <- allcoords1$PlotCN
rm(GFDL85b70)
#RAD75
GFDL85b75 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2075.nc", subds="rsds")
GFDL85b75 <- terra::rotate(GFDL85b75)
#Pull  vals
GFDL85RAD75<-data.frame(terra::extract(GFDL85b75,allcoords1))
GFDL85RAD75$RAD75 <- rowMeans(GFDL85RAD75[,2:366])
rownames(GFDL85RAD75) <- allcoords1$PlotCN
rm(GFDL85b75)
#RAD80
GFDL85b80 <- rast("~/Desktop/NASAClim/GFDL85/GFDL85RAD2080.nc", subds="rsds")
GFDL85b80 <- terra::rotate(GFDL85b80)
#Pull  vals
GFDL85RAD80<-data.frame(terra::extract(GFDL85b80,allcoords1))
GFDL85RAD80$RAD80 <- rowMeans(GFDL85RAD80[,2:366])
#convert kg/m2/s -> to mm
GFDL85RAD80$RAD80 <- GFDL85RAD80$RAD80
rownames(GFDL85RAD80) <- allcoords1$PlotCN
rm(GFDL85b80)

GFDL85RADFull<- data.frame(cbind(GFDL85RAD25$RAD25,GFDL85RAD30$RAD30,GFDL85RAD35$RAD35,GFDL85RAD40$RAD40, GFDL85RAD45$RAD45,
                                 GFDL85RAD50$RAD50, GFDL85RAD55$RAD55, GFDL85RAD60$RAD60, GFDL85RAD65$RAD65, GFDL85RAD70$RAD70, GFDL85RAD75$RAD75, GFDL85RAD80$RAD80))

colnames(GFDL85RADFull)<- c("RAD25","RAD30","RAD35","RAD40", "RAD45","RAD50", "RAD55", "RAD60", "RAD65", "RAD70", "RAD75", "RAD80")

GFDL85RAD1519 <- as.data.frame(rbind(GFDLRAD2015[,c(368,367)], GFDLRAD2016[,c(368,367)], GFDLRAD2017[,c(368,367)], GFDLRAD2018[,c(368,367)],
                                     GFDLRAD2019[,c(368,367)]))


rm(GFDL85RAD25,GFDL85RAD30,GFDL85RAD35,GFDL85RAD40, GFDL85RAD45,
   GFDL85RAD50, GFDL85RAD55, GFDL85RAD60, GFDL85RAD65, GFDL85RAD70, GFDL85RAD75, GFDL85RAD80)
rownames(GFDL85RADFull) <- allcoords1$PlotCN

GFDL26ALL <- as.data.frame(cbind(GFDL26TempFull,GFDL26PrecFull,GFDL26HUMFull,GFDL26RADFull))
GFDL45ALL <- as.data.frame(cbind(GFDL45TempFull,GFDL45PrecFull,GFDL45HUMFull,GFDL45RADFull))
GFDL85ALL <- as.data.frame(cbind(GFDL85TempFull,GFDL85PrecFull,GFDL85HUMFull,GFDL85RADFull))

write.csv(GFDL26ALL, file="GFDL26ALL.csv")
write.csv(GFDL45ALL, file="GFDL45ALL.csv")
write.csv(GFDL85ALL, file="GFDL85ALL.csv")

#NORESM26
#Temp
#2015
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2015.nc", subds="tas")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMTEMP2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMTEMP2015$AvgTemp <- rowMeans(NORESMTEMP2015[,2:366])
#Add PlotCN
NORESMTEMP2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert K  to C
NORESMTEMP2015$AvgTemp <- NORESMTEMP2015$AvgTemp-273.15
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2016.nc", subds="tas")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMTEMP2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMTEMP2016$AvgTemp <- rowMeans(NORESMTEMP2016[,2:366])
#Add PlotCN
NORESMTEMP2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert K  to C
NORESMTEMP2016$AvgTemp <- NORESMTEMP2016$AvgTemp-273.15
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2017.nc", subds="tas")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMTEMP2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMTEMP2017$AvgTemp <- rowMeans(NORESMTEMP2017[,2:366])
#Add PlotCN
NORESMTEMP2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert K  to C
NORESMTEMP2017$AvgTemp <- NORESMTEMP2017$AvgTemp-273.15
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2018.nc", subds="tas")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMTEMP2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMTEMP2018$AvgTemp <- rowMeans(NORESMTEMP2018[,2:366])
#Add PlotCN
NORESMTEMP2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert K  to C
NORESMTEMP2018$AvgTemp <- NORESMTEMP2018$AvgTemp-273.15
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2019.nc", subds="tas")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMTEMP2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMTEMP2019$AvgTemp <- rowMeans(NORESMTEMP2019[,2:366])
#Add PlotCN
NORESMTEMP2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert K  to C
NORESMTEMP2019$AvgTemp <- NORESMTEMP2019$AvgTemp-273.15
rm(NORESM2019)

#Temp25
NORESM26b25 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2025.nc", subds="tas")
NORESM26b25 <- terra::rotate(NORESM26b25)
#Pull  vals
NORESM26TEMP25<-data.frame(terra::extract(NORESM26b25,allcoords1))
NORESM26TEMP25$Temp25 <- rowMeans(NORESM26TEMP25[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP25$Temp25 <- NORESM26TEMP25$Temp25-273.15
rownames(NORESM26TEMP25) <- allcoords1$PlotCN
rm(NORESM26b25)
#Temp30
NORESM26b30 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2030.nc", subds="tas")
NORESM26b30 <- terra::rotate(NORESM26b30)
#Pull  vals
NORESM26TEMP30<-data.frame(terra::extract(NORESM26b30,allcoords1))
NORESM26TEMP30$Temp30 <- rowMeans(NORESM26TEMP30[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP30$Temp30 <- NORESM26TEMP30$Temp30-273.15
rownames(NORESM26TEMP30) <- allcoords1$PlotCN
rm(NORESM26b30)
#Temp35
NORESM26b35 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2035.nc", subds="tas")
NORESM26b35 <- terra::rotate(NORESM26b35)
#Pull  vals
NORESM26TEMP35<-data.frame(terra::extract(NORESM26b35,allcoords1))
NORESM26TEMP35$Temp35 <- rowMeans(NORESM26TEMP35[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP35$Temp35 <- NORESM26TEMP35$Temp35-273.15
rownames(NORESM26TEMP35) <- allcoords1$PlotCN
rm(NORESM26b35)
#Temp40
NORESM26b40 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2040.nc", subds="tas")
NORESM26b40 <- terra::rotate(NORESM26b40)
#Pull  vals
NORESM26TEMP40<-data.frame(terra::extract(NORESM26b40,allcoords1))
NORESM26TEMP40$Temp40 <- rowMeans(NORESM26TEMP40[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP40$Temp40 <- NORESM26TEMP40$Temp40-273.15
rownames(NORESM26TEMP40) <- allcoords1$PlotCN
rm(NORESM26b40)
#Temp45
NORESM26b45 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2045.nc", subds="tas")
NORESM26b45 <- terra::rotate(NORESM26b45)
#Pull  vals
NORESM26TEMP45<-data.frame(terra::extract(NORESM26b45,allcoords1))
NORESM26TEMP45$Temp45 <- rowMeans(NORESM26TEMP45[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP45$Temp45 <- NORESM26TEMP45$Temp45-273.15
rownames(NORESM26TEMP45) <- allcoords1$PlotCN
rm(NORESM26b45)

#Temp50
NORESM26b50 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2050.nc", subds="tas")
NORESM26b50 <- terra::rotate(NORESM26b50)
#Pull  vals
NORESM26TEMP50<-data.frame(terra::extract(NORESM26b50,allcoords1))
NORESM26TEMP50$Temp50 <- rowMeans(NORESM26TEMP50[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP50$Temp50 <- NORESM26TEMP50$Temp50-273.15
rownames(NORESM26TEMP50) <- allcoords1$PlotCN
rm(NORESM26b50)
#Temp55
NORESM26b55 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2055.nc", subds="tas")
NORESM26b55 <- terra::rotate(NORESM26b55)
#Pull  vals
NORESM26TEMP55<-data.frame(terra::extract(NORESM26b55,allcoords1))
NORESM26TEMP55$Temp55 <- rowMeans(NORESM26TEMP55[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP55$Temp55 <- NORESM26TEMP55$Temp55-273.15
rownames(NORESM26TEMP55) <- allcoords1$PlotCN
rm(NORESM26b55)
#Temp60
NORESM26b60 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2060.nc", subds="tas")
NORESM26b60 <- terra::rotate(NORESM26b60)
#Pull  vals
NORESM26TEMP60<-data.frame(terra::extract(NORESM26b60,allcoords1))
NORESM26TEMP60$Temp60 <- rowMeans(NORESM26TEMP60[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP60$Temp60 <- NORESM26TEMP60$Temp60-273.15
rownames(NORESM26TEMP60) <- allcoords1$PlotCN
rm(NORESM26b60)
#Temp65
NORESM26b65 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2065.nc", subds="tas")
NORESM26b65 <- terra::rotate(NORESM26b65)
#Pull  vals
NORESM26TEMP65<-data.frame(terra::extract(NORESM26b65,allcoords1))
NORESM26TEMP65$Temp65 <- rowMeans(NORESM26TEMP65[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP65$Temp65 <- NORESM26TEMP65$Temp65-273.15
rownames(NORESM26TEMP65) <- allcoords1$PlotCN
rm(NORESM26b65)
#Temp70
NORESM26b70 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2070.nc", subds="tas")
NORESM26b70 <- terra::rotate(NORESM26b70)
#Pull  vals
NORESM26TEMP70<-data.frame(terra::extract(NORESM26b70,allcoords1))
NORESM26TEMP70$Temp70 <- rowMeans(NORESM26TEMP70[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP70$Temp70 <- NORESM26TEMP70$Temp70-273.15
rownames(NORESM26TEMP70) <- allcoords1$PlotCN
rm(NORESM26b70)
#Temp75
NORESM26b75 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2075.nc", subds="tas")
NORESM26b75 <- terra::rotate(NORESM26b75)
#Pull  vals
NORESM26TEMP75<-data.frame(terra::extract(NORESM26b75,allcoords1))
NORESM26TEMP75$Temp75 <- rowMeans(NORESM26TEMP75[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP75$Temp75 <- NORESM26TEMP75$Temp75-273.15
rownames(NORESM26TEMP75) <- allcoords1$PlotCN
rm(NORESM26b75)
#Temp80
NORESM26b80 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Ta2080.nc", subds="tas")
NORESM26b80 <- terra::rotate(NORESM26b80)
#Pull  vals
NORESM26TEMP80<-data.frame(terra::extract(NORESM26b80,allcoords1))
NORESM26TEMP80$Temp80 <- rowMeans(NORESM26TEMP80[,2:366])
#convert kg/m2/s -> to mm
NORESM26TEMP80$Temp80 <- NORESM26TEMP80$Temp80-273.15
rownames(NORESM26TEMP80) <- allcoords1$PlotCN
rm(NORESM26b80)

NORESM26TempFull<- data.frame(cbind(NORESM26TEMP25$Temp25,NORESM26TEMP30$Temp30,NORESM26TEMP35$Temp35,NORESM26TEMP40$Temp40, NORESM26TEMP45$Temp45,
                                    NORESM26TEMP50$Temp50, NORESM26TEMP55$Temp55, NORESM26TEMP60$Temp60, NORESM26TEMP65$Temp65, NORESM26TEMP70$Temp70, NORESM26TEMP75$Temp75, NORESM26TEMP80$Temp80))

colnames(NORESM26TempFull)<- c("Temp25","Temp30","Temp35","Temp40", "Temp45","Temp50", "Temp55", "Temp60", "Temp65", "Temp70", "Temp75", "Temp80")



NORESM26TEMP1519 <- as.data.frame(rbind(NORESMTEMP2015[,c(368,367)], NORESMTEMP2016[,c(368,367)], NORESMTEMP2017[,c(368,367)], NORESMTEMP2018[,c(368,367)],
                                        NORESMTEMP2019[,c(368,367)]))


rm(NORESM26TEMP25,NORESM26TEMP30,NORESM26TEMP35,NORESM26TEMP40, NORESM26TEMP45,
   NORESM26TEMP50, NORESM26TEMP55, NORESM26TEMP60, NORESM26TEMP65, NORESM26TEMP70, NORESM26TEMP75, NORESM26TEMP80)
rownames(NORESM26TempFull) <- allcoords1$PlotCN

#Prec
#2015
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2015.nc", subds="pr")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMPREC2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMPREC2015$PrecSum <- rowSums(NORESMPREC2015[,2:366])
#Add PlotCN
NORESMPREC2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert to mm
NORESMPREC2015$PrecSum <- NORESMPREC2015$PrecSum*86400
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2016.nc", subds="pr")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMPREC2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMPREC2016$PrecSum <- rowSums(NORESMPREC2016[,2:366])
#Add PlotCN
NORESMPREC2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert to mm
NORESMPREC2016$PrecSum <- NORESMPREC2016$PrecSum*86400
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2017.nc", subds="pr")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMPREC2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMPREC2017$PrecSum <- rowSums(NORESMPREC2017[,2:366])
#Add PlotCN
NORESMPREC2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert to mm
NORESMPREC2017$PrecSum <- NORESMPREC2017$PrecSum*86400
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2018.nc", subds="pr")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMPREC2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMPREC2018$PrecSum <- rowSums(NORESMPREC2018[,2:366])
#Add PlotCN
NORESMPREC2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert to mm
NORESMPREC2018$PrecSum <- NORESMPREC2018$PrecSum*86400
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2019.nc", subds="pr")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMPREC2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMPREC2019$PrecSum <- rowSums(NORESMPREC2019[,2:366])
#Add PlotCN
NORESMPREC2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert to mm
NORESMPREC2019$PrecSum <- NORESMPREC2019$PrecSum*86400
rm(NORESM2019)

#Prec25
NORESM26b25 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2025.nc", subds="pr")
NORESM26b25 <- terra::rotate(NORESM26b25)
#Pull  vals
NORESM26PREC25<-data.frame(terra::extract(NORESM26b25,allcoords1))
NORESM26PREC25$Prec25 <- rowSums(NORESM26PREC25[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC25$Prec25 <- NORESM26PREC25$Prec25*86400
rownames(NORESM26PREC25) <- allcoords1$PlotCN
rm(NORESM26b25)
#Prec30
NORESM26b30 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2030.nc", subds="pr")
NORESM26b30 <- terra::rotate(NORESM26b30)
#Pull  vals
NORESM26PREC30<-data.frame(terra::extract(NORESM26b30,allcoords1))
NORESM26PREC30$Prec30 <- rowSums(NORESM26PREC30[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC30$Prec30 <- NORESM26PREC30$Prec30*86400
rownames(NORESM26PREC30) <- allcoords1$PlotCN
rm(NORESM26b30)
#Prec35
NORESM26b35 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2035.nc", subds="pr")
NORESM26b35 <- terra::rotate(NORESM26b35)
#Pull  vals
NORESM26PREC35<-data.frame(terra::extract(NORESM26b35,allcoords1))
NORESM26PREC35$Prec35 <- rowSums(NORESM26PREC35[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC35$Prec35 <- NORESM26PREC35$Prec35*86400
rownames(NORESM26PREC35) <- allcoords1$PlotCN
rm(NORESM26b35)
#Prec40
NORESM26b40 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2040.nc", subds="pr")
NORESM26b40 <- terra::rotate(NORESM26b40)
#Pull  vals
NORESM26PREC40<-data.frame(terra::extract(NORESM26b40,allcoords1))
NORESM26PREC40$Prec40 <- rowSums(NORESM26PREC40[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC40$Prec40 <- NORESM26PREC40$Prec40*86400
rownames(NORESM26PREC40) <- allcoords1$PlotCN
rm(NORESM26b40)
#Prec45
NORESM26b45 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2045.nc", subds="pr")
NORESM26b45 <- terra::rotate(NORESM26b45)
#Pull  vals
NORESM26PREC45<-data.frame(terra::extract(NORESM26b45,allcoords1))
NORESM26PREC45$Prec45 <- rowSums(NORESM26PREC45[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC45$Prec45 <- NORESM26PREC45$Prec45*86400
rownames(NORESM26PREC45) <- allcoords1$PlotCN
rm(NORESM26b45)

#Prec50
NORESM26b50 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2050.nc", subds="pr")
NORESM26b50 <- terra::rotate(NORESM26b50)
#Pull  vals
NORESM26PREC50<-data.frame(terra::extract(NORESM26b50,allcoords1))
NORESM26PREC50$Prec50 <- rowSums(NORESM26PREC50[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC50$Prec50 <- NORESM26PREC50$Prec50*86400
rownames(NORESM26PREC50) <- allcoords1$PlotCN
rm(NORESM26b50)
#Prec55
NORESM26b55 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2055.nc", subds="pr")
NORESM26b55 <- terra::rotate(NORESM26b55)
#Pull  vals
NORESM26PREC55<-data.frame(terra::extract(NORESM26b55,allcoords1))
NORESM26PREC55$Prec55 <- rowSums(NORESM26PREC55[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC55$Prec55 <- NORESM26PREC55$Prec55*86400
rownames(NORESM26PREC55) <- allcoords1$PlotCN
rm(NORESM26b55)
#Prec60
NORESM26b60 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2060.nc", subds="pr")
NORESM26b60 <- terra::rotate(NORESM26b60)
#Pull  vals
NORESM26PREC60<-data.frame(terra::extract(NORESM26b60,allcoords1))
NORESM26PREC60$Prec60 <- rowSums(NORESM26PREC60[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC60$Prec60 <- NORESM26PREC60$Prec60*86400
rownames(NORESM26PREC60) <- allcoords1$PlotCN
rm(NORESM26b60)
#Prec65
NORESM26b65 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2065.nc", subds="pr")
NORESM26b65 <- terra::rotate(NORESM26b65)
#Pull  vals
NORESM26PREC65<-data.frame(terra::extract(NORESM26b65,allcoords1))
NORESM26PREC65$Prec65 <- rowSums(NORESM26PREC65[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC65$Prec65 <- NORESM26PREC65$Prec65*86400
rownames(NORESM26PREC65) <- allcoords1$PlotCN
rm(NORESM26b65)
#Prec70
NORESM26b70 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2070.nc", subds="pr")
NORESM26b70 <- terra::rotate(NORESM26b70)
#Pull  vals
NORESM26PREC70<-data.frame(terra::extract(NORESM26b70,allcoords1))
NORESM26PREC70$Prec70 <- rowSums(NORESM26PREC70[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC70$Prec70 <- NORESM26PREC70$Prec70*86400
rownames(NORESM26PREC70) <- allcoords1$PlotCN
rm(NORESM26b70)
#Prec75
NORESM26b75 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2075.nc", subds="pr")
NORESM26b75 <- terra::rotate(NORESM26b75)
#Pull  vals
NORESM26PREC75<-data.frame(terra::extract(NORESM26b75,allcoords1))
NORESM26PREC75$Prec75 <- rowSums(NORESM26PREC75[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC75$Prec75 <- NORESM26PREC75$Prec75*86400
rownames(NORESM26PREC75) <- allcoords1$PlotCN
rm(NORESM26b75)
#Prec80
NORESM26b80 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Pr2080.nc", subds="pr")
NORESM26b80 <- terra::rotate(NORESM26b80)
#Pull  vals
NORESM26PREC80<-data.frame(terra::extract(NORESM26b80,allcoords1))
NORESM26PREC80$Prec80 <- rowSums(NORESM26PREC80[,2:366])
#convert kg/m2/s -> to mm
NORESM26PREC80$Prec80 <- NORESM26PREC80$Prec80*86400
rownames(NORESM26PREC80) <- allcoords1$PlotCN
rm(NORESM26b80)

NORESM26PrecFull<- data.frame(cbind(NORESM26PREC25$Prec25,NORESM26PREC30$Prec30,NORESM26PREC35$Prec35,NORESM26PREC40$Prec40, NORESM26PREC45$Prec45,
                                    NORESM26PREC50$Prec50, NORESM26PREC55$Prec55, NORESM26PREC60$Prec60, NORESM26PREC65$Prec65, NORESM26PREC70$Prec70, NORESM26PREC75$Prec75, NORESM26PREC80$Prec80))

colnames(NORESM26PrecFull)<- c("Prec25","Prec30","Prec35","Prec40", "Prec45","Prec50", "Prec55", "Prec60", "Prec65", "Prec70", "Prec75", "Prec80")

NORESM26PREC1519 <- as.data.frame(rbind(NORESMPREC2015[,c(368,367)], NORESMPREC2016[,c(368,367)], NORESMPREC2017[,c(368,367)], NORESMPREC2018[,c(368,367)],
                                        NORESMPREC2019[,c(368,367)]))


rm(NORESM26PREC25,NORESM26PREC30,NORESM26PREC35,NORESM26PREC40, NORESM26PREC45,
   NORESM26PREC50, NORESM26PREC55, NORESM26PREC60, NORESM26PREC65, NORESM26PREC70, NORESM26PREC75, NORESM26PREC80)
rownames(NORESM26PrecFull) <- allcoords1$PlotCN

#HUM
#2015
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2015.nc", subds="hurs")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMHUM2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMHUM2015$AvgHUM <- rowMeans(NORESMHUM2015[,2:366])
#Add PlotCN
NORESMHUM2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2016.nc", subds="hurs")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMHUM2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMHUM2016$AvgHUM <- rowMeans(NORESMHUM2016[,2:366])
#Add PlotCN
NORESMHUM2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2017.nc", subds="hurs")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMHUM2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMHUM2017$AvgHUM <- rowMeans(NORESMHUM2017[,2:366])
#Add PlotCN
NORESMHUM2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2018.nc", subds="hurs")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMHUM2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMHUM2018$AvgHUM <- rowMeans(NORESMHUM2018[,2:366])
#Add PlotCN
NORESMHUM2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2019.nc", subds="hurs")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMHUM2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMHUM2019$AvgHUM <- rowMeans(NORESMHUM2019[,2:366])
#Add PlotCN
NORESMHUM2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(NORESM2019)

#HUM25
NORESM26b25 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2025.nc", subds="hurs")
NORESM26b25 <- terra::rotate(NORESM26b25)
#Pull  vals
NORESM26HUM25<-data.frame(terra::extract(NORESM26b25,allcoords1))
NORESM26HUM25$HUM25 <- rowMeans(NORESM26HUM25[,2:366])
rownames(NORESM26HUM25) <- allcoords1$PlotCN
rm(NORESM26b25)
#HUM30
NORESM26b30 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2030.nc", subds="hurs")
NORESM26b30 <- terra::rotate(NORESM26b30)
#Pull  vals
NORESM26HUM30<-data.frame(terra::extract(NORESM26b30,allcoords1))
NORESM26HUM30$HUM30 <- rowMeans(NORESM26HUM30[,2:366])
rownames(NORESM26HUM30) <- allcoords1$PlotCN
rm(NORESM26b30)
#HUM35
NORESM26b35 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2035.nc", subds="hurs")
NORESM26b35 <- terra::rotate(NORESM26b35)
#Pull  vals
NORESM26HUM35<-data.frame(terra::extract(NORESM26b35,allcoords1))
NORESM26HUM35$HUM35 <- rowMeans(NORESM26HUM35[,2:366])
rownames(NORESM26HUM35) <- allcoords1$PlotCN
rm(NORESM26b35)
#HUM40
NORESM26b40 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2040.nc", subds="hurs")
NORESM26b40 <- terra::rotate(NORESM26b40)
#Pull  vals
NORESM26HUM40<-data.frame(terra::extract(NORESM26b40,allcoords1))
NORESM26HUM40$HUM40 <- rowMeans(NORESM26HUM40[,2:366])
rownames(NORESM26HUM40) <- allcoords1$PlotCN
rm(NORESM26b40)
#HUM45
NORESM26b45 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2045.nc", subds="hurs")
NORESM26b45 <- terra::rotate(NORESM26b45)
#Pull  vals
NORESM26HUM45<-data.frame(terra::extract(NORESM26b45,allcoords1))
NORESM26HUM45$HUM45 <- rowMeans(NORESM26HUM45[,2:366])
rownames(NORESM26HUM45) <- allcoords1$PlotCN
rm(NORESM26b45)

#HUM50
NORESM26b50 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2050.nc", subds="hurs")
NORESM26b50 <- terra::rotate(NORESM26b50)
#Pull  vals
NORESM26HUM50<-data.frame(terra::extract(NORESM26b50,allcoords1))
NORESM26HUM50$HUM50 <- rowMeans(NORESM26HUM50[,2:366])
rownames(NORESM26HUM50) <- allcoords1$PlotCN
rm(NORESM26b50)
#HUM55
NORESM26b55 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2055.nc", subds="hurs")
NORESM26b55 <- terra::rotate(NORESM26b55)
#Pull  vals
NORESM26HUM55<-data.frame(terra::extract(NORESM26b55,allcoords1))
NORESM26HUM55$HUM55 <- rowMeans(NORESM26HUM55[,2:366])
rownames(NORESM26HUM55) <- allcoords1$PlotCN
rm(NORESM26b55)
#HUM60
NORESM26b60 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2060.nc", subds="hurs")
NORESM26b60 <- terra::rotate(NORESM26b60)
#Pull  vals
NORESM26HUM60<-data.frame(terra::extract(NORESM26b60,allcoords1))
NORESM26HUM60$HUM60 <- rowMeans(NORESM26HUM60[,2:366])
rownames(NORESM26HUM60) <- allcoords1$PlotCN
rm(NORESM26b60)
#HUM65
NORESM26b65 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2065.nc", subds="hurs")
NORESM26b65 <- terra::rotate(NORESM26b65)
#Pull  vals
NORESM26HUM65<-data.frame(terra::extract(NORESM26b65,allcoords1))
NORESM26HUM65$HUM65 <- rowMeans(NORESM26HUM65[,2:366])
rownames(NORESM26HUM65) <- allcoords1$PlotCN
rm(NORESM26b65)
#HUM70
NORESM26b70 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2070.nc", subds="hurs")
NORESM26b70 <- terra::rotate(NORESM26b70)
#Pull  vals
NORESM26HUM70<-data.frame(terra::extract(NORESM26b70,allcoords1))
NORESM26HUM70$HUM70 <- rowMeans(NORESM26HUM70[,2:366])
rownames(NORESM26HUM70) <- allcoords1$PlotCN
rm(NORESM26b70)
#HUM75
NORESM26b75 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2075.nc", subds="hurs")
NORESM26b75 <- terra::rotate(NORESM26b75)
#Pull  vals
NORESM26HUM75<-data.frame(terra::extract(NORESM26b75,allcoords1))
NORESM26HUM75$HUM75 <- rowMeans(NORESM26HUM75[,2:366])
rownames(NORESM26HUM75) <- allcoords1$PlotCN
rm(NORESM26b75)
#HUM80
NORESM26b80 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26Hurs2080.nc", subds="hurs")
NORESM26b80 <- terra::rotate(NORESM26b80)
#Pull  vals
NORESM26HUM80<-data.frame(terra::extract(NORESM26b80,allcoords1))
NORESM26HUM80$HUM80 <- rowMeans(NORESM26HUM80[,2:366])
#convert kg/m2/s -> to mm
NORESM26HUM80$HUM80 <- NORESM26HUM80$HUM80
rownames(NORESM26HUM80) <- allcoords1$PlotCN
rm(NORESM26b80)

NORESM26HUMFull<- data.frame(cbind(NORESM26HUM25$HUM25,NORESM26HUM30$HUM30,NORESM26HUM35$HUM35,NORESM26HUM40$HUM40, NORESM26HUM45$HUM45,
                                   NORESM26HUM50$HUM50, NORESM26HUM55$HUM55, NORESM26HUM60$HUM60, NORESM26HUM65$HUM65, NORESM26HUM70$HUM70, NORESM26HUM75$HUM75, NORESM26HUM80$HUM80))

colnames(NORESM26HUMFull)<- c("HUM25","HUM30","HUM35","HUM40", "HUM45","HUM50", "HUM55", "HUM60", "HUM65", "HUM70", "HUM75", "HUM80")

NORESM26HUM1519 <- as.data.frame(rbind(NORESMHUM2015[,c(368,367)], NORESMHUM2016[,c(368,367)], NORESMHUM2017[,c(368,367)], NORESMHUM2018[,c(368,367)],
                                       NORESMHUM2019[,c(368,367)]))


rm(NORESM26HUM25,NORESM26HUM30,NORESM26HUM35,NORESM26HUM40, NORESM26HUM45,
   NORESM26HUM50, NORESM26HUM55, NORESM26HUM60, NORESM26HUM65, NORESM26HUM70, NORESM26HUM75, NORESM26HUM80)
rownames(NORESM26HUMFull) <- allcoords1$PlotCN
#RAD
#2015
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2015.nc", subds="rsds")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMRAD2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMRAD2015$AvgRAD <- rowMeans(NORESMRAD2015[,2:366])
#Add PlotCN
NORESMRAD2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2016.nc", subds="rsds")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMRAD2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMRAD2016$AvgRAD <- rowMeans(NORESMRAD2016[,2:366])
#Add PlotCN
NORESMRAD2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2017.nc", subds="rsds")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMRAD2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMRAD2017$AvgRAD <- rowMeans(NORESMRAD2017[,2:366])
#Add PlotCN
NORESMRAD2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2018.nc", subds="rsds")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMRAD2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMRAD2018$AvgRAD <- rowMeans(NORESMRAD2018[,2:366])
#Add PlotCN
NORESMRAD2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM26")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2019.nc", subds="rsds")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMRAD2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMRAD2019$AvgRAD <- rowMeans(NORESMRAD2019[,2:366])
#Add PlotCN
NORESMRAD2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(NORESM2019)

#RAD25
NORESM26b25 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2025.nc", subds="rsds")
NORESM26b25 <- terra::rotate(NORESM26b25)
#Pull  vals
NORESM26RAD25<-data.frame(terra::extract(NORESM26b25,allcoords1))
NORESM26RAD25$RAD25 <- rowMeans(NORESM26RAD25[,2:366])
rownames(NORESM26RAD25) <- allcoords1$PlotCN
rm(NORESM26b25)
#RAD30
NORESM26b30 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2030.nc", subds="rsds")
NORESM26b30 <- terra::rotate(NORESM26b30)
#Pull  vals
NORESM26RAD30<-data.frame(terra::extract(NORESM26b30,allcoords1))
NORESM26RAD30$RAD30 <- rowMeans(NORESM26RAD30[,2:366])
rownames(NORESM26RAD30) <- allcoords1$PlotCN
rm(NORESM26b30)
#RAD35
NORESM26b35 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2035.nc", subds="rsds")
NORESM26b35 <- terra::rotate(NORESM26b35)
#Pull  vals
NORESM26RAD35<-data.frame(terra::extract(NORESM26b35,allcoords1))
NORESM26RAD35$RAD35 <- rowMeans(NORESM26RAD35[,2:366])
rownames(NORESM26RAD35) <- allcoords1$PlotCN
rm(NORESM26b35)
#RAD40
NORESM26b40 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2040.nc", subds="rsds")
NORESM26b40 <- terra::rotate(NORESM26b40)
#Pull  vals
NORESM26RAD40<-data.frame(terra::extract(NORESM26b40,allcoords1))
NORESM26RAD40$RAD40 <- rowMeans(NORESM26RAD40[,2:366])
rownames(NORESM26RAD40) <- allcoords1$PlotCN
rm(NORESM26b40)
#RAD45
NORESM26b45 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2045.nc", subds="rsds")
NORESM26b45 <- terra::rotate(NORESM26b45)
#Pull  vals
NORESM26RAD45<-data.frame(terra::extract(NORESM26b45,allcoords1))
NORESM26RAD45$RAD45 <- rowMeans(NORESM26RAD45[,2:366])
rownames(NORESM26RAD45) <- allcoords1$PlotCN
rm(NORESM26b45)

#RAD50
NORESM26b50 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2050.nc", subds="rsds")
NORESM26b50 <- terra::rotate(NORESM26b50)
#Pull  vals
NORESM26RAD50<-data.frame(terra::extract(NORESM26b50,allcoords1))
NORESM26RAD50$RAD50 <- rowMeans(NORESM26RAD50[,2:366])
rownames(NORESM26RAD50) <- allcoords1$PlotCN
rm(NORESM26b50)
#RAD55
NORESM26b55 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2055.nc", subds="rsds")
NORESM26b55 <- terra::rotate(NORESM26b55)
#Pull  vals
NORESM26RAD55<-data.frame(terra::extract(NORESM26b55,allcoords1))
NORESM26RAD55$RAD55 <- rowMeans(NORESM26RAD55[,2:366])
rownames(NORESM26RAD55) <- allcoords1$PlotCN
rm(NORESM26b55)
#RAD60
NORESM26b60 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2060.nc", subds="rsds")
NORESM26b60 <- terra::rotate(NORESM26b60)
#Pull  vals
NORESM26RAD60<-data.frame(terra::extract(NORESM26b60,allcoords1))
NORESM26RAD60$RAD60 <- rowMeans(NORESM26RAD60[,2:366])
rownames(NORESM26RAD60) <- allcoords1$PlotCN
rm(NORESM26b60)
#RAD65
NORESM26b65 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2065.nc", subds="rsds")
NORESM26b65 <- terra::rotate(NORESM26b65)
#Pull  vals
NORESM26RAD65<-data.frame(terra::extract(NORESM26b65,allcoords1))
NORESM26RAD65$RAD65 <- rowMeans(NORESM26RAD65[,2:366])
rownames(NORESM26RAD65) <- allcoords1$PlotCN
rm(NORESM26b65)
#RAD70
NORESM26b70 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2070.nc", subds="rsds")
NORESM26b70 <- terra::rotate(NORESM26b70)
#Pull  vals
NORESM26RAD70<-data.frame(terra::extract(NORESM26b70,allcoords1))
NORESM26RAD70$RAD70 <- rowMeans(NORESM26RAD70[,2:366])
rownames(NORESM26RAD70) <- allcoords1$PlotCN
rm(NORESM26b70)
#RAD75
NORESM26b75 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2075.nc", subds="rsds")
NORESM26b75 <- terra::rotate(NORESM26b75)
#Pull  vals
NORESM26RAD75<-data.frame(terra::extract(NORESM26b75,allcoords1))
NORESM26RAD75$RAD75 <- rowMeans(NORESM26RAD75[,2:366])
rownames(NORESM26RAD75) <- allcoords1$PlotCN
rm(NORESM26b75)
#RAD80
NORESM26b80 <- rast("~/Desktop/NASAClim/NORESM26/NORESM26RAD2080.nc", subds="rsds")
NORESM26b80 <- terra::rotate(NORESM26b80)
#Pull  vals
NORESM26RAD80<-data.frame(terra::extract(NORESM26b80,allcoords1))
NORESM26RAD80$RAD80 <- rowMeans(NORESM26RAD80[,2:366])
#convert kg/m2/s -> to mm
NORESM26RAD80$RAD80 <- NORESM26RAD80$RAD80
rownames(NORESM26RAD80) <- allcoords1$PlotCN
rm(NORESM26b80)

NORESM26RADFull<- data.frame(cbind(NORESM26RAD25$RAD25,NORESM26RAD30$RAD30,NORESM26RAD35$RAD35,NORESM26RAD40$RAD40, NORESM26RAD45$RAD45,
                                   NORESM26RAD50$RAD50, NORESM26RAD55$RAD55, NORESM26RAD60$RAD60, NORESM26RAD65$RAD65, NORESM26RAD70$RAD70, NORESM26RAD75$RAD75, NORESM26RAD80$RAD80))

colnames(NORESM26RADFull)<- c("RAD25","RAD30","RAD35","RAD40", "RAD45","RAD50", "RAD55", "RAD60", "RAD65", "RAD70", "RAD75", "RAD80")


NORESM26RAD1519 <- as.data.frame(rbind(NORESMRAD2015[,c(368,367)], NORESMRAD2016[,c(368,367)], NORESMRAD2017[,c(368,367)], NORESMRAD2018[,c(368,367)],
                                       NORESMRAD2019[,c(368,367)]))

rm(NORESM26RAD25,NORESM26RAD30,NORESM26RAD35,NORESM26RAD40, NORESM26RAD45,
   NORESM26RAD50, NORESM26RAD55, NORESM26RAD60, NORESM26RAD65, NORESM26RAD70, NORESM26RAD75, NORESM26RAD80)
rownames(NORESM26RADFull) <- allcoords1$PlotCN

#future data projections from 2015-2080
#NORESM45
#Temp
#2015
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2015.nc", subds="tas")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMTEMP2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMTEMP2015$AvgTemp <- rowMeans(NORESMTEMP2015[,2:366])
#Add PlotCN
NORESMTEMP2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert K  to C
NORESMTEMP2015$AvgTemp <- NORESMTEMP2015$AvgTemp-273.15
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2016.nc", subds="tas")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMTEMP2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMTEMP2016$AvgTemp <- rowMeans(NORESMTEMP2016[,2:366])
#Add PlotCN
NORESMTEMP2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert K  to C
NORESMTEMP2016$AvgTemp <- NORESMTEMP2016$AvgTemp-273.15
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2017.nc", subds="tas")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMTEMP2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMTEMP2017$AvgTemp <- rowMeans(NORESMTEMP2017[,2:366])
#Add PlotCN
NORESMTEMP2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert K  to C
NORESMTEMP2017$AvgTemp <- NORESMTEMP2017$AvgTemp-273.15
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2018.nc", subds="tas")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMTEMP2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMTEMP2018$AvgTemp <- rowMeans(NORESMTEMP2018[,2:366])
#Add PlotCN
NORESMTEMP2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert K  to C
NORESMTEMP2018$AvgTemp <- NORESMTEMP2018$AvgTemp-273.15
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2019.nc", subds="tas")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMTEMP2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMTEMP2019$AvgTemp <- rowMeans(NORESMTEMP2019[,2:366])
#Add PlotCN
NORESMTEMP2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert K  to C
NORESMTEMP2019$AvgTemp <- NORESMTEMP2019$AvgTemp-273.15
rm(NORESM2019)

#Temp25
NORESM45b25 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2025.nc", subds="tas")
NORESM45b25 <- terra::rotate(NORESM45b25)
#Pull  vals
NORESM45TEMP25<-data.frame(terra::extract(NORESM45b25,allcoords1))
NORESM45TEMP25$Temp25 <- rowMeans(NORESM45TEMP25[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP25$Temp25 <- NORESM45TEMP25$Temp25-273.15
rownames(NORESM45TEMP25) <- allcoords1$PlotCN
rm(NORESM45b25)
#Temp30
NORESM45b30 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2030.nc", subds="tas")
NORESM45b30 <- terra::rotate(NORESM45b30)
#Pull  vals
NORESM45TEMP30<-data.frame(terra::extract(NORESM45b30,allcoords1))
NORESM45TEMP30$Temp30 <- rowMeans(NORESM45TEMP30[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP30$Temp30 <- NORESM45TEMP30$Temp30-273.15
rownames(NORESM45TEMP30) <- allcoords1$PlotCN
rm(NORESM45b30)
#Temp35
NORESM45b35 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2035.nc", subds="tas")
NORESM45b35 <- terra::rotate(NORESM45b35)
#Pull  vals
NORESM45TEMP35<-data.frame(terra::extract(NORESM45b35,allcoords1))
NORESM45TEMP35$Temp35 <- rowMeans(NORESM45TEMP35[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP35$Temp35 <- NORESM45TEMP35$Temp35-273.15
rownames(NORESM45TEMP35) <- allcoords1$PlotCN
rm(NORESM45b35)
#Temp40
NORESM45b40 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2040.nc", subds="tas")
NORESM45b40 <- terra::rotate(NORESM45b40)
#Pull  vals
NORESM45TEMP40<-data.frame(terra::extract(NORESM45b40,allcoords1))
NORESM45TEMP40$Temp40 <- rowMeans(NORESM45TEMP40[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP40$Temp40 <- NORESM45TEMP40$Temp40-273.15
rownames(NORESM45TEMP40) <- allcoords1$PlotCN
rm(NORESM45b40)
#Temp45
NORESM45b45 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2045.nc", subds="tas")
NORESM45b45 <- terra::rotate(NORESM45b45)
#Pull  vals
NORESM45TEMP45<-data.frame(terra::extract(NORESM45b45,allcoords1))
NORESM45TEMP45$Temp45 <- rowMeans(NORESM45TEMP45[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP45$Temp45 <- NORESM45TEMP45$Temp45-273.15
rownames(NORESM45TEMP45) <- allcoords1$PlotCN
rm(NORESM45b45)

#Temp50
NORESM45b50 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2050.nc", subds="tas")
NORESM45b50 <- terra::rotate(NORESM45b50)
#Pull  vals
NORESM45TEMP50<-data.frame(terra::extract(NORESM45b50,allcoords1))
NORESM45TEMP50$Temp50 <- rowMeans(NORESM45TEMP50[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP50$Temp50 <- NORESM45TEMP50$Temp50-273.15
rownames(NORESM45TEMP50) <- allcoords1$PlotCN
rm(NORESM45b50)
#Temp55
NORESM45b55 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2055.nc", subds="tas")
NORESM45b55 <- terra::rotate(NORESM45b55)
#Pull  vals
NORESM45TEMP55<-data.frame(terra::extract(NORESM45b55,allcoords1))
NORESM45TEMP55$Temp55 <- rowMeans(NORESM45TEMP55[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP55$Temp55 <- NORESM45TEMP55$Temp55-273.15
rownames(NORESM45TEMP55) <- allcoords1$PlotCN
rm(NORESM45b55)
#Temp60
NORESM45b60 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2060.nc", subds="tas")
NORESM45b60 <- terra::rotate(NORESM45b60)
#Pull  vals
NORESM45TEMP60<-data.frame(terra::extract(NORESM45b60,allcoords1))
NORESM45TEMP60$Temp60 <- rowMeans(NORESM45TEMP60[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP60$Temp60 <- NORESM45TEMP60$Temp60-273.15
rownames(NORESM45TEMP60) <- allcoords1$PlotCN
rm(NORESM45b60)
#Temp65
NORESM45b65 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2065.nc", subds="tas")
NORESM45b65 <- terra::rotate(NORESM45b65)
#Pull  vals
NORESM45TEMP65<-data.frame(terra::extract(NORESM45b65,allcoords1))
NORESM45TEMP65$Temp65 <- rowMeans(NORESM45TEMP65[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP65$Temp65 <- NORESM45TEMP65$Temp65-273.15
rownames(NORESM45TEMP65) <- allcoords1$PlotCN
rm(NORESM45b65)
#Temp70
NORESM45b70 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2070.nc", subds="tas")
NORESM45b70 <- terra::rotate(NORESM45b70)
#Pull  vals
NORESM45TEMP70<-data.frame(terra::extract(NORESM45b70,allcoords1))
NORESM45TEMP70$Temp70 <- rowMeans(NORESM45TEMP70[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP70$Temp70 <- NORESM45TEMP70$Temp70-273.15
rownames(NORESM45TEMP70) <- allcoords1$PlotCN
rm(NORESM45b70)
#Temp75
NORESM45b75 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2075.nc", subds="tas")
NORESM45b75 <- terra::rotate(NORESM45b75)
#Pull  vals
NORESM45TEMP75<-data.frame(terra::extract(NORESM45b75,allcoords1))
NORESM45TEMP75$Temp75 <- rowMeans(NORESM45TEMP75[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP75$Temp75 <- NORESM45TEMP75$Temp75-273.15
rownames(NORESM45TEMP75) <- allcoords1$PlotCN
rm(NORESM45b75)
#Temp80
NORESM45b80 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Ta2080.nc", subds="tas")
NORESM45b80 <- terra::rotate(NORESM45b80)
#Pull  vals
NORESM45TEMP80<-data.frame(terra::extract(NORESM45b80,allcoords1))
NORESM45TEMP80$Temp80 <- rowMeans(NORESM45TEMP80[,2:366])
#convert kg/m2/s -> to mm
NORESM45TEMP80$Temp80 <- NORESM45TEMP80$Temp80-273.15
rownames(NORESM45TEMP80) <- allcoords1$PlotCN
rm(NORESM45b80)

NORESM45TempFull<- data.frame(cbind(NORESM45TEMP25$Temp25,NORESM45TEMP30$Temp30,NORESM45TEMP35$Temp35,NORESM45TEMP40$Temp40, NORESM45TEMP45$Temp45,
                                    NORESM45TEMP50$Temp50, NORESM45TEMP55$Temp55, NORESM45TEMP60$Temp60, NORESM45TEMP65$Temp65, NORESM45TEMP70$Temp70, NORESM45TEMP75$Temp75, NORESM45TEMP80$Temp80))

colnames(NORESM45TempFull)<- c("Temp25","Temp30","Temp35","Temp40", "Temp45","Temp50", "Temp55", "Temp60", "Temp65", "Temp70", "Temp75", "Temp80")

NORESM45TEMP1519 <- as.data.frame(rbind(NORESMTEMP2015[,c(368,367)], NORESMTEMP2016[,c(368,367)], NORESMTEMP2017[,c(368,367)], NORESMTEMP2018[,c(368,367)],
                                        NORESMTEMP2019[,c(368,367)]))


rm(NORESM45TEMP25,NORESM45TEMP30,NORESM45TEMP35,NORESM45TEMP40, NORESM45TEMP45,
   NORESM45TEMP50, NORESM45TEMP55, NORESM45TEMP60, NORESM45TEMP65, NORESM45TEMP70, NORESM45TEMP75, NORESM45TEMP80)
rownames(NORESM45TempFull) <- allcoords1$PlotCN

#Prec
#2015
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2015.nc", subds="pr")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMPREC2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMPREC2015$PrecSum <- rowSums(NORESMPREC2015[,2:366])
#Add PlotCN
NORESMPREC2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert to mm
NORESMPREC2015$PrecSum <- NORESMPREC2015$PrecSum*86400
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2016.nc", subds="pr")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMPREC2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMPREC2016$PrecSum <- rowSums(NORESMPREC2016[,2:366])
#Add PlotCN
NORESMPREC2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert to mm
NORESMPREC2016$PrecSum <- NORESMPREC2016$PrecSum*86400
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2017.nc", subds="pr")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMPREC2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMPREC2017$PrecSum <- rowSums(NORESMPREC2017[,2:366])
#Add PlotCN
NORESMPREC2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert to mm
NORESMPREC2017$PrecSum <- NORESMPREC2017$PrecSum*86400
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2018.nc", subds="pr")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMPREC2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMPREC2018$PrecSum <- rowSums(NORESMPREC2018[,2:366])
#Add PlotCN
NORESMPREC2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert to mm
NORESMPREC2018$PrecSum <- NORESMPREC2018$PrecSum*86400
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2019.nc", subds="pr")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMPREC2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMPREC2019$PrecSum <- rowSums(NORESMPREC2019[,2:366])
#Add PlotCN
NORESMPREC2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert to mm
NORESMPREC2019$PrecSum <- NORESMPREC2019$PrecSum*86400
rm(NORESM2019)

#Prec25
NORESM45b25 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2025.nc", subds="pr")
NORESM45b25 <- terra::rotate(NORESM45b25)
#Pull  vals
NORESM45PREC25<-data.frame(terra::extract(NORESM45b25,allcoords1))
NORESM45PREC25$Prec25 <- rowSums(NORESM45PREC25[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC25$Prec25 <- NORESM45PREC25$Prec25*86400
rownames(NORESM45PREC25) <- allcoords1$PlotCN
rm(NORESM45b25)
#Prec30
NORESM45b30 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2030.nc", subds="pr")
NORESM45b30 <- terra::rotate(NORESM45b30)
#Pull  vals
NORESM45PREC30<-data.frame(terra::extract(NORESM45b30,allcoords1))
NORESM45PREC30$Prec30 <- rowSums(NORESM45PREC30[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC30$Prec30 <- NORESM45PREC30$Prec30*86400
rownames(NORESM45PREC30) <- allcoords1$PlotCN
rm(NORESM45b30)
#Prec35
NORESM45b35 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2035.nc", subds="pr")
NORESM45b35 <- terra::rotate(NORESM45b35)
#Pull  vals
NORESM45PREC35<-data.frame(terra::extract(NORESM45b35,allcoords1))
NORESM45PREC35$Prec35 <- rowSums(NORESM45PREC35[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC35$Prec35 <- NORESM45PREC35$Prec35*86400
rownames(NORESM45PREC35) <- allcoords1$PlotCN
rm(NORESM45b35)
#Prec40
NORESM45b40 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2040.nc", subds="pr")
NORESM45b40 <- terra::rotate(NORESM45b40)
#Pull  vals
NORESM45PREC40<-data.frame(terra::extract(NORESM45b40,allcoords1))
NORESM45PREC40$Prec40 <- rowSums(NORESM45PREC40[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC40$Prec40 <- NORESM45PREC40$Prec40*86400
rownames(NORESM45PREC40) <- allcoords1$PlotCN
rm(NORESM45b40)
#Prec45
NORESM45b45 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2045.nc", subds="pr")
NORESM45b45 <- terra::rotate(NORESM45b45)
#Pull  vals
NORESM45PREC45<-data.frame(terra::extract(NORESM45b45,allcoords1))
NORESM45PREC45$Prec45 <- rowSums(NORESM45PREC45[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC45$Prec45 <- NORESM45PREC45$Prec45*86400
rownames(NORESM45PREC45) <- allcoords1$PlotCN
rm(NORESM45b45)

#Prec50
NORESM45b50 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2050.nc", subds="pr")
NORESM45b50 <- terra::rotate(NORESM45b50)
#Pull  vals
NORESM45PREC50<-data.frame(terra::extract(NORESM45b50,allcoords1))
NORESM45PREC50$Prec50 <- rowSums(NORESM45PREC50[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC50$Prec50 <- NORESM45PREC50$Prec50*86400
rownames(NORESM45PREC50) <- allcoords1$PlotCN
rm(NORESM45b50)
#Prec55
NORESM45b55 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2055.nc", subds="pr")
NORESM45b55 <- terra::rotate(NORESM45b55)
#Pull  vals
NORESM45PREC55<-data.frame(terra::extract(NORESM45b55,allcoords1))
NORESM45PREC55$Prec55 <- rowSums(NORESM45PREC55[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC55$Prec55 <- NORESM45PREC55$Prec55*86400
rownames(NORESM45PREC55) <- allcoords1$PlotCN
rm(NORESM45b55)
#Prec60
NORESM45b60 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2060.nc", subds="pr")
NORESM45b60 <- terra::rotate(NORESM45b60)
#Pull  vals
NORESM45PREC60<-data.frame(terra::extract(NORESM45b60,allcoords1))
NORESM45PREC60$Prec60 <- rowSums(NORESM45PREC60[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC60$Prec60 <- NORESM45PREC60$Prec60*86400
rownames(NORESM45PREC60) <- allcoords1$PlotCN
rm(NORESM45b60)
#Prec65
NORESM45b65 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2065.nc", subds="pr")
NORESM45b65 <- terra::rotate(NORESM45b65)
#Pull  vals
NORESM45PREC65<-data.frame(terra::extract(NORESM45b65,allcoords1))
NORESM45PREC65$Prec65 <- rowSums(NORESM45PREC65[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC65$Prec65 <- NORESM45PREC65$Prec65*86400
rownames(NORESM45PREC65) <- allcoords1$PlotCN
rm(NORESM45b65)
#Prec70
NORESM45b70 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2070.nc", subds="pr")
NORESM45b70 <- terra::rotate(NORESM45b70)
#Pull  vals
NORESM45PREC70<-data.frame(terra::extract(NORESM45b70,allcoords1))
NORESM45PREC70$Prec70 <- rowSums(NORESM45PREC70[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC70$Prec70 <- NORESM45PREC70$Prec70*86400
rownames(NORESM45PREC70) <- allcoords1$PlotCN
rm(NORESM45b70)
#Prec75
NORESM45b75 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2075.nc", subds="pr")
NORESM45b75 <- terra::rotate(NORESM45b75)
#Pull  vals
NORESM45PREC75<-data.frame(terra::extract(NORESM45b75,allcoords1))
NORESM45PREC75$Prec75 <- rowSums(NORESM45PREC75[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC75$Prec75 <- NORESM45PREC75$Prec75*86400
rownames(NORESM45PREC75) <- allcoords1$PlotCN
rm(NORESM45b75)
#Prec80
NORESM45b80 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Pr2080.nc", subds="pr")
NORESM45b80 <- terra::rotate(NORESM45b80)
#Pull  vals
NORESM45PREC80<-data.frame(terra::extract(NORESM45b80,allcoords1))
NORESM45PREC80$Prec80 <- rowSums(NORESM45PREC80[,2:366])
#convert kg/m2/s -> to mm
NORESM45PREC80$Prec80 <- NORESM45PREC80$Prec80*86400
rownames(NORESM45PREC80) <- allcoords1$PlotCN
rm(NORESM45b80)

NORESM45PrecFull<- data.frame(cbind(NORESM45PREC25$Prec25,NORESM45PREC30$Prec30,NORESM45PREC35$Prec35,NORESM45PREC40$Prec40, NORESM45PREC45$Prec45,
                                    NORESM45PREC50$Prec50, NORESM45PREC55$Prec55, NORESM45PREC60$Prec60, NORESM45PREC65$Prec65, NORESM45PREC70$Prec70, NORESM45PREC75$Prec75, NORESM45PREC80$Prec80))

colnames(NORESM45PrecFull)<- c("Prec25","Prec30","Prec35","Prec40", "Prec45","Prec50", "Prec55", "Prec60", "Prec65", "Prec70", "Prec75", "Prec80")

NORESM45PREC1519 <- as.data.frame(rbind(NORESMPREC2015[,c(368,367)], NORESMPREC2016[,c(368,367)], NORESMPREC2017[,c(368,367)], NORESMPREC2018[,c(368,367)],
                                        NORESMPREC2019[,c(368,367)]))

rm(NORESM45PREC25,NORESM45PREC30,NORESM45PREC35,NORESM45PREC40, NORESM45PREC45,
   NORESM45PREC50, NORESM45PREC55, NORESM45PREC60, NORESM45PREC65, NORESM45PREC70, NORESM45PREC75, NORESM45PREC80)
rownames(NORESM45PrecFull) <- allcoords1$PlotCN

#HUM
#2015
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2015.nc", subds="hurs")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMHUM2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMHUM2015$AvgHUM <- rowMeans(NORESMHUM2015[,2:366])
#Add PlotCN
NORESMHUM2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2016.nc", subds="hurs")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMHUM2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMHUM2016$AvgHUM <- rowMeans(NORESMHUM2016[,2:366])
#Add PlotCN
NORESMHUM2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2017.nc", subds="hurs")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMHUM2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMHUM2017$AvgHUM <- rowMeans(NORESMHUM2017[,2:366])
#Add PlotCN
NORESMHUM2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2018.nc", subds="hurs")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMHUM2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMHUM2018$AvgHUM <- rowMeans(NORESMHUM2018[,2:366])
#Add PlotCN
NORESMHUM2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2019.nc", subds="hurs")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMHUM2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMHUM2019$AvgHUM <- rowMeans(NORESMHUM2019[,2:366])
#Add PlotCN
NORESMHUM2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(NORESM2019)

#HUM25
NORESM45b25 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2025.nc", subds="hurs")
NORESM45b25 <- terra::rotate(NORESM45b25)
#Pull  vals
NORESM45HUM25<-data.frame(terra::extract(NORESM45b25,allcoords1))
NORESM45HUM25$HUM25 <- rowMeans(NORESM45HUM25[,2:366])
rownames(NORESM45HUM25) <- allcoords1$PlotCN
rm(NORESM45b25)
#HUM30
NORESM45b30 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2030.nc", subds="hurs")
NORESM45b30 <- terra::rotate(NORESM45b30)
#Pull  vals
NORESM45HUM30<-data.frame(terra::extract(NORESM45b30,allcoords1))
NORESM45HUM30$HUM30 <- rowMeans(NORESM45HUM30[,2:366])
rownames(NORESM45HUM30) <- allcoords1$PlotCN
rm(NORESM45b30)
#HUM35
NORESM45b35 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2035.nc", subds="hurs")
NORESM45b35 <- terra::rotate(NORESM45b35)
#Pull  vals
NORESM45HUM35<-data.frame(terra::extract(NORESM45b35,allcoords1))
NORESM45HUM35$HUM35 <- rowMeans(NORESM45HUM35[,2:366])
rownames(NORESM45HUM35) <- allcoords1$PlotCN
rm(NORESM45b35)
#HUM40
NORESM45b40 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2040.nc", subds="hurs")
NORESM45b40 <- terra::rotate(NORESM45b40)
#Pull  vals
NORESM45HUM40<-data.frame(terra::extract(NORESM45b40,allcoords1))
NORESM45HUM40$HUM40 <- rowMeans(NORESM45HUM40[,2:366])
rownames(NORESM45HUM40) <- allcoords1$PlotCN
rm(NORESM45b40)
#HUM45
NORESM45b45 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2045.nc", subds="hurs")
NORESM45b45 <- terra::rotate(NORESM45b45)
#Pull  vals
NORESM45HUM45<-data.frame(terra::extract(NORESM45b45,allcoords1))
NORESM45HUM45$HUM45 <- rowMeans(NORESM45HUM45[,2:366])
rownames(NORESM45HUM45) <- allcoords1$PlotCN
rm(NORESM45b45)

#HUM50
NORESM45b50 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2050.nc", subds="hurs")
NORESM45b50 <- terra::rotate(NORESM45b50)
#Pull  vals
NORESM45HUM50<-data.frame(terra::extract(NORESM45b50,allcoords1))
NORESM45HUM50$HUM50 <- rowMeans(NORESM45HUM50[,2:366])
rownames(NORESM45HUM50) <- allcoords1$PlotCN
rm(NORESM45b50)
#HUM55
NORESM45b55 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2055.nc", subds="hurs")
NORESM45b55 <- terra::rotate(NORESM45b55)
#Pull  vals
NORESM45HUM55<-data.frame(terra::extract(NORESM45b55,allcoords1))
NORESM45HUM55$HUM55 <- rowMeans(NORESM45HUM55[,2:366])
rownames(NORESM45HUM55) <- allcoords1$PlotCN
rm(NORESM45b55)
#HUM60
NORESM45b60 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2060.nc", subds="hurs")
NORESM45b60 <- terra::rotate(NORESM45b60)
#Pull  vals
NORESM45HUM60<-data.frame(terra::extract(NORESM45b60,allcoords1))
NORESM45HUM60$HUM60 <- rowMeans(NORESM45HUM60[,2:366])
rownames(NORESM45HUM60) <- allcoords1$PlotCN
rm(NORESM45b60)
#HUM65
NORESM45b65 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2065.nc", subds="hurs")
NORESM45b65 <- terra::rotate(NORESM45b65)
#Pull  vals
NORESM45HUM65<-data.frame(terra::extract(NORESM45b65,allcoords1))
NORESM45HUM65$HUM65 <- rowMeans(NORESM45HUM65[,2:366])
rownames(NORESM45HUM65) <- allcoords1$PlotCN
rm(NORESM45b65)
#HUM70
NORESM45b70 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2070.nc", subds="hurs")
NORESM45b70 <- terra::rotate(NORESM45b70)
#Pull  vals
NORESM45HUM70<-data.frame(terra::extract(NORESM45b70,allcoords1))
NORESM45HUM70$HUM70 <- rowMeans(NORESM45HUM70[,2:366])
rownames(NORESM45HUM70) <- allcoords1$PlotCN
rm(NORESM45b70)
#HUM75
NORESM45b75 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2075.nc", subds="hurs")
NORESM45b75 <- terra::rotate(NORESM45b75)
#Pull  vals
NORESM45HUM75<-data.frame(terra::extract(NORESM45b75,allcoords1))
NORESM45HUM75$HUM75 <- rowMeans(NORESM45HUM75[,2:366])
rownames(NORESM45HUM75) <- allcoords1$PlotCN
rm(NORESM45b75)
#HUM80
NORESM45b80 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45Hurs2080.nc", subds="hurs")
NORESM45b80 <- terra::rotate(NORESM45b80)
#Pull  vals
NORESM45HUM80<-data.frame(terra::extract(NORESM45b80,allcoords1))
NORESM45HUM80$HUM80 <- rowMeans(NORESM45HUM80[,2:366])
#convert kg/m2/s -> to mm
NORESM45HUM80$HUM80 <- NORESM45HUM80$HUM80
rownames(NORESM45HUM80) <- allcoords1$PlotCN
rm(NORESM45b80)

NORESM45HUMFull<- data.frame(cbind(NORESM45HUM25$HUM25,NORESM45HUM30$HUM30,NORESM45HUM35$HUM35,NORESM45HUM40$HUM40, NORESM45HUM45$HUM45,
                                   NORESM45HUM50$HUM50, NORESM45HUM55$HUM55, NORESM45HUM60$HUM60, NORESM45HUM65$HUM65, NORESM45HUM70$HUM70, NORESM45HUM75$HUM75, NORESM45HUM80$HUM80))

colnames(NORESM45HUMFull)<- c("HUM25","HUM30","HUM35","HUM40", "HUM45","HUM50", "HUM55", "HUM60", "HUM65", "HUM70", "HUM75", "HUM80")

NORESM45HUM1519 <- as.data.frame(rbind(NORESMHUM2015[,c(368,367)], NORESMHUM2016[,c(368,367)], NORESMHUM2017[,c(368,367)], NORESMHUM2018[,c(368,367)],
                                       NORESMHUM2019[,c(368,367)]))


rm(NORESM45HUM25,NORESM45HUM30,NORESM45HUM35,NORESM45HUM40, NORESM45HUM45,
   NORESM45HUM50, NORESM45HUM55, NORESM45HUM60, NORESM45HUM65, NORESM45HUM70, NORESM45HUM75, NORESM45HUM80)
rownames(NORESM45HUMFull) <- allcoords1$PlotCN
#RAD
#2015
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2015.nc", subds="rsds")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMRAD2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMRAD2015$AvgRAD <- rowMeans(NORESMRAD2015[,2:366])
#Add PlotCN
NORESMRAD2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2016.nc", subds="rsds")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMRAD2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMRAD2016$AvgRAD <- rowMeans(NORESMRAD2016[,2:366])
#Add PlotCN
NORESMRAD2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2017.nc", subds="rsds")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMRAD2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMRAD2017$AvgRAD <- rowMeans(NORESMRAD2017[,2:366])
#Add PlotCN
NORESMRAD2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2018.nc", subds="rsds")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMRAD2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMRAD2018$AvgRAD <- rowMeans(NORESMRAD2018[,2:366])
#Add PlotCN
NORESMRAD2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM45")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2019.nc", subds="rsds")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMRAD2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMRAD2019$AvgRAD <- rowMeans(NORESMRAD2019[,2:366])
#Add PlotCN
NORESMRAD2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(NORESM2019)

#RAD25
NORESM45b25 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2025.nc", subds="rsds")
NORESM45b25 <- terra::rotate(NORESM45b25)
#Pull  vals
NORESM45RAD25<-data.frame(terra::extract(NORESM45b25,allcoords1))
NORESM45RAD25$RAD25 <- rowMeans(NORESM45RAD25[,2:366])
rownames(NORESM45RAD25) <- allcoords1$PlotCN
rm(NORESM45b25)
#RAD30
NORESM45b30 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2030.nc", subds="rsds")
NORESM45b30 <- terra::rotate(NORESM45b30)
#Pull  vals
NORESM45RAD30<-data.frame(terra::extract(NORESM45b30,allcoords1))
NORESM45RAD30$RAD30 <- rowMeans(NORESM45RAD30[,2:366])
rownames(NORESM45RAD30) <- allcoords1$PlotCN
rm(NORESM45b30)
#RAD35
NORESM45b35 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2035.nc", subds="rsds")
NORESM45b35 <- terra::rotate(NORESM45b35)
#Pull  vals
NORESM45RAD35<-data.frame(terra::extract(NORESM45b35,allcoords1))
NORESM45RAD35$RAD35 <- rowMeans(NORESM45RAD35[,2:366])
rownames(NORESM45RAD35) <- allcoords1$PlotCN
rm(NORESM45b35)
#RAD40
NORESM45b40 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2040.nc", subds="rsds")
NORESM45b40 <- terra::rotate(NORESM45b40)
#Pull  vals
NORESM45RAD40<-data.frame(terra::extract(NORESM45b40,allcoords1))
NORESM45RAD40$RAD40 <- rowMeans(NORESM45RAD40[,2:366])
rownames(NORESM45RAD40) <- allcoords1$PlotCN
rm(NORESM45b40)
#RAD45
NORESM45b45 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2045.nc", subds="rsds")
NORESM45b45 <- terra::rotate(NORESM45b45)
#Pull  vals
NORESM45RAD45<-data.frame(terra::extract(NORESM45b45,allcoords1))
NORESM45RAD45$RAD45 <- rowMeans(NORESM45RAD45[,2:366])
rownames(NORESM45RAD45) <- allcoords1$PlotCN
rm(NORESM45b45)

#RAD50
NORESM45b50 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2050.nc", subds="rsds")
NORESM45b50 <- terra::rotate(NORESM45b50)
#Pull  vals
NORESM45RAD50<-data.frame(terra::extract(NORESM45b50,allcoords1))
NORESM45RAD50$RAD50 <- rowMeans(NORESM45RAD50[,2:366])
rownames(NORESM45RAD50) <- allcoords1$PlotCN
rm(NORESM45b50)
#RAD55
NORESM45b55 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2055.nc", subds="rsds")
NORESM45b55 <- terra::rotate(NORESM45b55)
#Pull  vals
NORESM45RAD55<-data.frame(terra::extract(NORESM45b55,allcoords1))
NORESM45RAD55$RAD55 <- rowMeans(NORESM45RAD55[,2:366])
rownames(NORESM45RAD55) <- allcoords1$PlotCN
rm(NORESM45b55)
#RAD60
NORESM45b60 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2060.nc", subds="rsds")
NORESM45b60 <- terra::rotate(NORESM45b60)
#Pull  vals
NORESM45RAD60<-data.frame(terra::extract(NORESM45b60,allcoords1))
NORESM45RAD60$RAD60 <- rowMeans(NORESM45RAD60[,2:366])
rownames(NORESM45RAD60) <- allcoords1$PlotCN
rm(NORESM45b60)
#RAD65
NORESM45b65 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2065.nc", subds="rsds")
NORESM45b65 <- terra::rotate(NORESM45b65)
#Pull  vals
NORESM45RAD65<-data.frame(terra::extract(NORESM45b65,allcoords1))
NORESM45RAD65$RAD65 <- rowMeans(NORESM45RAD65[,2:366])
rownames(NORESM45RAD65) <- allcoords1$PlotCN
rm(NORESM45b65)
#RAD70
NORESM45b70 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2070.nc", subds="rsds")
NORESM45b70 <- terra::rotate(NORESM45b70)
#Pull  vals
NORESM45RAD70<-data.frame(terra::extract(NORESM45b70,allcoords1))
NORESM45RAD70$RAD70 <- rowMeans(NORESM45RAD70[,2:366])
rownames(NORESM45RAD70) <- allcoords1$PlotCN
rm(NORESM45b70)
#RAD75
NORESM45b75 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2075.nc", subds="rsds")
NORESM45b75 <- terra::rotate(NORESM45b75)
#Pull  vals
NORESM45RAD75<-data.frame(terra::extract(NORESM45b75,allcoords1))
NORESM45RAD75$RAD75 <- rowMeans(NORESM45RAD75[,2:366])
rownames(NORESM45RAD75) <- allcoords1$PlotCN
rm(NORESM45b75)
#RAD80
NORESM45b80 <- rast("~/Desktop/NASAClim/NORESM45/NORESM45RAD2080.nc", subds="rsds")
NORESM45b80 <- terra::rotate(NORESM45b80)
#Pull  vals
NORESM45RAD80<-data.frame(terra::extract(NORESM45b80,allcoords1))
NORESM45RAD80$RAD80 <- rowMeans(NORESM45RAD80[,2:366])
#convert kg/m2/s -> to mm
NORESM45RAD80$RAD80 <- NORESM45RAD80$RAD80
rownames(NORESM45RAD80) <- allcoords1$PlotCN
rm(NORESM45b80)

NORESM45RADFull<- data.frame(cbind(NORESM45RAD25$RAD25,NORESM45RAD30$RAD30,NORESM45RAD35$RAD35,NORESM45RAD40$RAD40, NORESM45RAD45$RAD45,
                                   NORESM45RAD50$RAD50, NORESM45RAD55$RAD55, NORESM45RAD60$RAD60, NORESM45RAD65$RAD65, NORESM45RAD70$RAD70, NORESM45RAD75$RAD75, NORESM45RAD80$RAD80))

colnames(NORESM45RADFull)<- c("RAD25","RAD30","RAD35","RAD40", "RAD45","RAD50", "RAD55", "RAD60", "RAD65", "RAD70", "RAD75", "RAD80")

NORESM45RAD1519 <- as.data.frame(rbind(NORESMRAD2015[,c(368,367)], NORESMRAD2016[,c(368,367)], NORESMRAD2017[,c(368,367)], NORESMRAD2018[,c(368,367)],
                                       NORESMRAD2019[,c(368,367)]))


rm(NORESM45RAD25,NORESM45RAD30,NORESM45RAD35,NORESM45RAD40, NORESM45RAD45,
   NORESM45RAD50, NORESM45RAD55, NORESM45RAD60, NORESM45RAD65, NORESM45RAD70, NORESM45RAD75, NORESM45RAD80)
rownames(NORESM45RADFull) <- allcoords1$PlotCN

#NORESM85
#Temp
#2015
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2015.nc", subds="tas")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMTEMP2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMTEMP2015$AvgTemp <- rowMeans(NORESMTEMP2015[,2:366])
#Add PlotCN
NORESMTEMP2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert K  to C
NORESMTEMP2015$AvgTemp <- NORESMTEMP2015$AvgTemp-273.15
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2016.nc", subds="tas")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMTEMP2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMTEMP2016$AvgTemp <- rowMeans(NORESMTEMP2016[,2:366])
#Add PlotCN
NORESMTEMP2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert K  to C
NORESMTEMP2016$AvgTemp <- NORESMTEMP2016$AvgTemp-273.15
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2017.nc", subds="tas")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMTEMP2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMTEMP2017$AvgTemp <- rowMeans(NORESMTEMP2017[,2:366])
#Add PlotCN
NORESMTEMP2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert K  to C
NORESMTEMP2017$AvgTemp <- NORESMTEMP2017$AvgTemp-273.15
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2018.nc", subds="tas")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMTEMP2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMTEMP2018$AvgTemp <- rowMeans(NORESMTEMP2018[,2:366])
#Add PlotCN
NORESMTEMP2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert K  to C
NORESMTEMP2018$AvgTemp <- NORESMTEMP2018$AvgTemp-273.15
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2019.nc", subds="tas")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMTEMP2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMTEMP2019$AvgTemp <- rowMeans(NORESMTEMP2019[,2:366])
#Add PlotCN
NORESMTEMP2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert K  to C
NORESMTEMP2019$AvgTemp <- NORESMTEMP2019$AvgTemp-273.15
rm(NORESM2019)

#Temp25
NORESM85b25 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2025.nc", subds="tas")
NORESM85b25 <- terra::rotate(NORESM85b25)
#Pull  vals
NORESM85TEMP25<-data.frame(terra::extract(NORESM85b25,allcoords1))
NORESM85TEMP25$Temp25 <- rowMeans(NORESM85TEMP25[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP25$Temp25 <- NORESM85TEMP25$Temp25-273.15
rownames(NORESM85TEMP25) <- allcoords1$PlotCN
rm(NORESM85b25)
#Temp30
NORESM85b30 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2030.nc", subds="tas")
NORESM85b30 <- terra::rotate(NORESM85b30)
#Pull  vals
NORESM85TEMP30<-data.frame(terra::extract(NORESM85b30,allcoords1))
NORESM85TEMP30$Temp30 <- rowMeans(NORESM85TEMP30[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP30$Temp30 <- NORESM85TEMP30$Temp30-273.15
rownames(NORESM85TEMP30) <- allcoords1$PlotCN
rm(NORESM85b30)
#Temp35
NORESM85b35 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2035.nc", subds="tas")
NORESM85b35 <- terra::rotate(NORESM85b35)
#Pull  vals
NORESM85TEMP35<-data.frame(terra::extract(NORESM85b35,allcoords1))
NORESM85TEMP35$Temp35 <- rowMeans(NORESM85TEMP35[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP35$Temp35 <- NORESM85TEMP35$Temp35-273.15
rownames(NORESM85TEMP35) <- allcoords1$PlotCN
rm(NORESM85b35)
#Temp40
NORESM85b40 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2040.nc", subds="tas")
NORESM85b40 <- terra::rotate(NORESM85b40)
#Pull  vals
NORESM85TEMP40<-data.frame(terra::extract(NORESM85b40,allcoords1))
NORESM85TEMP40$Temp40 <- rowMeans(NORESM85TEMP40[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP40$Temp40 <- NORESM85TEMP40$Temp40-273.15
rownames(NORESM85TEMP40) <- allcoords1$PlotCN
rm(NORESM85b40)
#Temp45
NORESM85b45 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2045.nc", subds="tas")
NORESM85b45 <- terra::rotate(NORESM85b45)
#Pull  vals
NORESM85TEMP45<-data.frame(terra::extract(NORESM85b45,allcoords1))
NORESM85TEMP45$Temp45 <- rowMeans(NORESM85TEMP45[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP45$Temp45 <- NORESM85TEMP45$Temp45-273.15
rownames(NORESM85TEMP45) <- allcoords1$PlotCN
rm(NORESM85b45)

#Temp50
NORESM85b50 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2050.nc", subds="tas")
NORESM85b50 <- terra::rotate(NORESM85b50)
#Pull  vals
NORESM85TEMP50<-data.frame(terra::extract(NORESM85b50,allcoords1))
NORESM85TEMP50$Temp50 <- rowMeans(NORESM85TEMP50[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP50$Temp50 <- NORESM85TEMP50$Temp50-273.15
rownames(NORESM85TEMP50) <- allcoords1$PlotCN
rm(NORESM85b50)
#Temp55
NORESM85b55 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2055.nc", subds="tas")
NORESM85b55 <- terra::rotate(NORESM85b55)
#Pull  vals
NORESM85TEMP55<-data.frame(terra::extract(NORESM85b55,allcoords1))
NORESM85TEMP55$Temp55 <- rowMeans(NORESM85TEMP55[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP55$Temp55 <- NORESM85TEMP55$Temp55-273.15
rownames(NORESM85TEMP55) <- allcoords1$PlotCN
rm(NORESM85b55)
#Temp60
NORESM85b60 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2060.nc", subds="tas")
NORESM85b60 <- terra::rotate(NORESM85b60)
#Pull  vals
NORESM85TEMP60<-data.frame(terra::extract(NORESM85b60,allcoords1))
NORESM85TEMP60$Temp60 <- rowMeans(NORESM85TEMP60[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP60$Temp60 <- NORESM85TEMP60$Temp60-273.15
rownames(NORESM85TEMP60) <- allcoords1$PlotCN
rm(NORESM85b60)
#Temp65
NORESM85b65 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2065.nc", subds="tas")
NORESM85b65 <- terra::rotate(NORESM85b65)
#Pull  vals
NORESM85TEMP65<-data.frame(terra::extract(NORESM85b65,allcoords1))
NORESM85TEMP65$Temp65 <- rowMeans(NORESM85TEMP65[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP65$Temp65 <- NORESM85TEMP65$Temp65-273.15
rownames(NORESM85TEMP65) <- allcoords1$PlotCN
rm(NORESM85b65)
#Temp70
NORESM85b70 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2070.nc", subds="tas")
NORESM85b70 <- terra::rotate(NORESM85b70)
#Pull  vals
NORESM85TEMP70<-data.frame(terra::extract(NORESM85b70,allcoords1))
NORESM85TEMP70$Temp70 <- rowMeans(NORESM85TEMP70[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP70$Temp70 <- NORESM85TEMP70$Temp70-273.15
rownames(NORESM85TEMP70) <- allcoords1$PlotCN
rm(NORESM85b70)
#Temp75
NORESM85b75 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2075.nc", subds="tas")
NORESM85b75 <- terra::rotate(NORESM85b75)
#Pull  vals
NORESM85TEMP75<-data.frame(terra::extract(NORESM85b75,allcoords1))
NORESM85TEMP75$Temp75 <- rowMeans(NORESM85TEMP75[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP75$Temp75 <- NORESM85TEMP75$Temp75-273.15
rownames(NORESM85TEMP75) <- allcoords1$PlotCN
rm(NORESM85b75)
#Temp80
NORESM85b80 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Ta2080.nc", subds="tas")
NORESM85b80 <- terra::rotate(NORESM85b80)
#Pull  vals
NORESM85TEMP80<-data.frame(terra::extract(NORESM85b80,allcoords1))
NORESM85TEMP80$Temp80 <- rowMeans(NORESM85TEMP80[,2:366])
#convert kg/m2/s -> to mm
NORESM85TEMP80$Temp80 <- NORESM85TEMP80$Temp80-273.15
rownames(NORESM85TEMP80) <- allcoords1$PlotCN
rm(NORESM85b80)

NORESM85TempFull<- data.frame(cbind(NORESM85TEMP25$Temp25,NORESM85TEMP30$Temp30,NORESM85TEMP35$Temp35,NORESM85TEMP40$Temp40, NORESM85TEMP45$Temp45,
                                    NORESM85TEMP50$Temp50, NORESM85TEMP55$Temp55, NORESM85TEMP60$Temp60, NORESM85TEMP65$Temp65, NORESM85TEMP70$Temp70, NORESM85TEMP75$Temp75, NORESM85TEMP80$Temp80))

colnames(NORESM85TempFull)<- c("Temp25","Temp30","Temp35","Temp40", "Temp45","Temp50", "Temp55", "Temp60", "Temp65", "Temp70", "Temp75", "Temp80")

NORESM85TEMP1519 <- as.data.frame(rbind(NORESMTEMP2015[,c(368,367)], NORESMTEMP2016[,c(368,367)], NORESMTEMP2017[,c(368,367)], NORESMTEMP2018[,c(368,367)],
                                        NORESMTEMP2019[,c(368,367)]))


rm(NORESM85TEMP25,NORESM85TEMP30,NORESM85TEMP35,NORESM85TEMP40, NORESM85TEMP45,
   NORESM85TEMP50, NORESM85TEMP55, NORESM85TEMP60, NORESM85TEMP65, NORESM85TEMP70, NORESM85TEMP75, NORESM85TEMP80)
rownames(NORESM85TempFull) <- allcoords1$PlotCN

#Prec
#2015
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2015.nc", subds="pr")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMPREC2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMPREC2015$PrecSum <- rowSums(NORESMPREC2015[,2:366])
#Add PlotCN
NORESMPREC2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
#convert to mm
NORESMPREC2015$PrecSum <- NORESMPREC2015$PrecSum*86400
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2016.nc", subds="pr")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMPREC2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMPREC2016$PrecSum <- rowSums(NORESMPREC2016[,2:366])
#Add PlotCN
NORESMPREC2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
#convert to mm
NORESMPREC2016$PrecSum <- NORESMPREC2016$PrecSum*86400
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2017.nc", subds="pr")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMPREC2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMPREC2017$PrecSum <- rowSums(NORESMPREC2017[,2:366])
#Add PlotCN
NORESMPREC2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
#convert to mm
NORESMPREC2017$PrecSum <- NORESMPREC2017$PrecSum*86400
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2018.nc", subds="pr")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMPREC2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMPREC2018$PrecSum <- rowSums(NORESMPREC2018[,2:366])
#Add PlotCN
NORESMPREC2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
#convert to mm
NORESMPREC2018$PrecSum <- NORESMPREC2018$PrecSum*86400
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2019.nc", subds="pr")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMPREC2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMPREC2019$PrecSum <- rowSums(NORESMPREC2019[,2:366])
#Add PlotCN
NORESMPREC2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
#convert to mm
NORESMPREC2019$PrecSum <- NORESMPREC2019$PrecSum*86400
rm(NORESM2019)

#Prec25
NORESM85b25 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2025.nc", subds="pr")
NORESM85b25 <- terra::rotate(NORESM85b25)
#Pull  vals
NORESM85PREC25<-data.frame(terra::extract(NORESM85b25,allcoords1))
NORESM85PREC25$Prec25 <- rowSums(NORESM85PREC25[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC25$Prec25 <- NORESM85PREC25$Prec25*86400
rownames(NORESM85PREC25) <- allcoords1$PlotCN
rm(NORESM85b25)
#Prec30
NORESM85b30 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2030.nc", subds="pr")
NORESM85b30 <- terra::rotate(NORESM85b30)
#Pull  vals
NORESM85PREC30<-data.frame(terra::extract(NORESM85b30,allcoords1))
NORESM85PREC30$Prec30 <- rowSums(NORESM85PREC30[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC30$Prec30 <- NORESM85PREC30$Prec30*86400
rownames(NORESM85PREC30) <- allcoords1$PlotCN
rm(NORESM85b30)
#Prec35
NORESM85b35 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2035.nc", subds="pr")
NORESM85b35 <- terra::rotate(NORESM85b35)
#Pull  vals
NORESM85PREC35<-data.frame(terra::extract(NORESM85b35,allcoords1))
NORESM85PREC35$Prec35 <- rowSums(NORESM85PREC35[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC35$Prec35 <- NORESM85PREC35$Prec35*86400
rownames(NORESM85PREC35) <- allcoords1$PlotCN
rm(NORESM85b35)
#Prec40
NORESM85b40 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2040.nc", subds="pr")
NORESM85b40 <- terra::rotate(NORESM85b40)
#Pull  vals
NORESM85PREC40<-data.frame(terra::extract(NORESM85b40,allcoords1))
NORESM85PREC40$Prec40 <- rowSums(NORESM85PREC40[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC40$Prec40 <- NORESM85PREC40$Prec40*86400
rownames(NORESM85PREC40) <- allcoords1$PlotCN
rm(NORESM85b40)
#Prec45
NORESM85b45 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2045.nc", subds="pr")
NORESM85b45 <- terra::rotate(NORESM85b45)
#Pull  vals
NORESM85PREC45<-data.frame(terra::extract(NORESM85b45,allcoords1))
NORESM85PREC45$Prec45 <- rowSums(NORESM85PREC45[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC45$Prec45 <- NORESM85PREC45$Prec45*86400
rownames(NORESM85PREC45) <- allcoords1$PlotCN
rm(NORESM85b45)

#Prec50
NORESM85b50 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2050.nc", subds="pr")
NORESM85b50 <- terra::rotate(NORESM85b50)
#Pull  vals
NORESM85PREC50<-data.frame(terra::extract(NORESM85b50,allcoords1))
NORESM85PREC50$Prec50 <- rowSums(NORESM85PREC50[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC50$Prec50 <- NORESM85PREC50$Prec50*86400
rownames(NORESM85PREC50) <- allcoords1$PlotCN
rm(NORESM85b50)
#Prec55
NORESM85b55 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2055.nc", subds="pr")
NORESM85b55 <- terra::rotate(NORESM85b55)
#Pull  vals
NORESM85PREC55<-data.frame(terra::extract(NORESM85b55,allcoords1))
NORESM85PREC55$Prec55 <- rowSums(NORESM85PREC55[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC55$Prec55 <- NORESM85PREC55$Prec55*86400
rownames(NORESM85PREC55) <- allcoords1$PlotCN
rm(NORESM85b55)
#Prec60
NORESM85b60 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2060.nc", subds="pr")
NORESM85b60 <- terra::rotate(NORESM85b60)
#Pull  vals
NORESM85PREC60<-data.frame(terra::extract(NORESM85b60,allcoords1))
NORESM85PREC60$Prec60 <- rowSums(NORESM85PREC60[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC60$Prec60 <- NORESM85PREC60$Prec60*86400
rownames(NORESM85PREC60) <- allcoords1$PlotCN
rm(NORESM85b60)
#Prec65
NORESM85b65 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2065.nc", subds="pr")
NORESM85b65 <- terra::rotate(NORESM85b65)
#Pull  vals
NORESM85PREC65<-data.frame(terra::extract(NORESM85b65,allcoords1))
NORESM85PREC65$Prec65 <- rowSums(NORESM85PREC65[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC65$Prec65 <- NORESM85PREC65$Prec65*86400
rownames(NORESM85PREC65) <- allcoords1$PlotCN
rm(NORESM85b65)
#Prec70
NORESM85b70 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2070.nc", subds="pr")
NORESM85b70 <- terra::rotate(NORESM85b70)
#Pull  vals
NORESM85PREC70<-data.frame(terra::extract(NORESM85b70,allcoords1))
NORESM85PREC70$Prec70 <- rowSums(NORESM85PREC70[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC70$Prec70 <- NORESM85PREC70$Prec70*86400
rownames(NORESM85PREC70) <- allcoords1$PlotCN
rm(NORESM85b70)
#Prec75
NORESM85b75 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2075.nc", subds="pr")
NORESM85b75 <- terra::rotate(NORESM85b75)
#Pull  vals
NORESM85PREC75<-data.frame(terra::extract(NORESM85b75,allcoords1))
NORESM85PREC75$Prec75 <- rowSums(NORESM85PREC75[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC75$Prec75 <- NORESM85PREC75$Prec75*86400
rownames(NORESM85PREC75) <- allcoords1$PlotCN
rm(NORESM85b75)
#Prec80
NORESM85b80 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Pr2080.nc", subds="pr")
NORESM85b80 <- terra::rotate(NORESM85b80)
#Pull  vals
NORESM85PREC80<-data.frame(terra::extract(NORESM85b80,allcoords1))
NORESM85PREC80$Prec80 <- rowSums(NORESM85PREC80[,2:366])
#convert kg/m2/s -> to mm
NORESM85PREC80$Prec80 <- NORESM85PREC80$Prec80*86400
rownames(NORESM85PREC80) <- allcoords1$PlotCN
rm(NORESM85b80)

NORESM85PrecFull<- data.frame(cbind(NORESM85PREC25$Prec25,NORESM85PREC30$Prec30,NORESM85PREC35$Prec35,NORESM85PREC40$Prec40, NORESM85PREC45$Prec45,
                                    NORESM85PREC50$Prec50, NORESM85PREC55$Prec55, NORESM85PREC60$Prec60, NORESM85PREC65$Prec65, NORESM85PREC70$Prec70, NORESM85PREC75$Prec75, NORESM85PREC80$Prec80))

colnames(NORESM85PrecFull)<- c("Prec25","Prec30","Prec35","Prec40", "Prec45","Prec50", "Prec55", "Prec60", "Prec65", "Prec70", "Prec75", "Prec80")

NORESM85PREC1519 <- as.data.frame(rbind(NORESMPREC2015[,c(368,367)], NORESMPREC2016[,c(368,367)], NORESMPREC2017[,c(368,367)], NORESMPREC2018[,c(368,367)],
                                        NORESMPREC2019[,c(368,367)]))

rm(NORESM85PREC25,NORESM85PREC30,NORESM85PREC35,NORESM85PREC40, NORESM85PREC45,
   NORESM85PREC50, NORESM85PREC55, NORESM85PREC60, NORESM85PREC65, NORESM85PREC70, NORESM85PREC75, NORESM85PREC80)
rownames(NORESM85PrecFull) <- allcoords1$PlotCN

#HUM
#2015
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2015.nc", subds="hurs")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMHUM2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMHUM2015$AvgHUM <- rowMeans(NORESMHUM2015[,2:366])
#Add PlotCN
NORESMHUM2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2016.nc", subds="hurs")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMHUM2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMHUM2016$AvgHUM <- rowMeans(NORESMHUM2016[,2:366])
#Add PlotCN
NORESMHUM2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2017.nc", subds="hurs")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMHUM2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMHUM2017$AvgHUM <- rowMeans(NORESMHUM2017[,2:366])
#Add PlotCN
NORESMHUM2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2018.nc", subds="hurs")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMHUM2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMHUM2018$AvgHUM <- rowMeans(NORESMHUM2018[,2:366])
#Add PlotCN
NORESMHUM2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2019.nc", subds="hurs")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMHUM2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMHUM2019$AvgHUM <- rowMeans(NORESMHUM2019[,2:366])
#Add PlotCN
NORESMHUM2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(NORESM2019)

#HUM25
NORESM85b25 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2025.nc", subds="hurs")
NORESM85b25 <- terra::rotate(NORESM85b25)
#Pull  vals
NORESM85HUM25<-data.frame(terra::extract(NORESM85b25,allcoords1))
NORESM85HUM25$HUM25 <- rowMeans(NORESM85HUM25[,2:366])
rownames(NORESM85HUM25) <- allcoords1$PlotCN
rm(NORESM85b25)
#HUM30
NORESM85b30 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2030.nc", subds="hurs")
NORESM85b30 <- terra::rotate(NORESM85b30)
#Pull  vals
NORESM85HUM30<-data.frame(terra::extract(NORESM85b30,allcoords1))
NORESM85HUM30$HUM30 <- rowMeans(NORESM85HUM30[,2:366])
rownames(NORESM85HUM30) <- allcoords1$PlotCN
rm(NORESM85b30)
#HUM35
NORESM85b35 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2035.nc", subds="hurs")
NORESM85b35 <- terra::rotate(NORESM85b35)
#Pull  vals
NORESM85HUM35<-data.frame(terra::extract(NORESM85b35,allcoords1))
NORESM85HUM35$HUM35 <- rowMeans(NORESM85HUM35[,2:366])
rownames(NORESM85HUM35) <- allcoords1$PlotCN
rm(NORESM85b35)
#HUM40
NORESM85b40 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2040.nc", subds="hurs")
NORESM85b40 <- terra::rotate(NORESM85b40)
#Pull  vals
NORESM85HUM40<-data.frame(terra::extract(NORESM85b40,allcoords1))
NORESM85HUM40$HUM40 <- rowMeans(NORESM85HUM40[,2:366])
rownames(NORESM85HUM40) <- allcoords1$PlotCN
rm(NORESM85b40)
#HUM45
NORESM85b45 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2045.nc", subds="hurs")
NORESM85b45 <- terra::rotate(NORESM85b45)
#Pull  vals
NORESM85HUM45<-data.frame(terra::extract(NORESM85b45,allcoords1))
NORESM85HUM45$HUM45 <- rowMeans(NORESM85HUM45[,2:366])
rownames(NORESM85HUM45) <- allcoords1$PlotCN
rm(NORESM85b45)

#HUM50
NORESM85b50 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2050.nc", subds="hurs")
NORESM85b50 <- terra::rotate(NORESM85b50)
#Pull  vals
NORESM85HUM50<-data.frame(terra::extract(NORESM85b50,allcoords1))
NORESM85HUM50$HUM50 <- rowMeans(NORESM85HUM50[,2:366])
rownames(NORESM85HUM50) <- allcoords1$PlotCN
rm(NORESM85b50)
#HUM55
NORESM85b55 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2055.nc", subds="hurs")
NORESM85b55 <- terra::rotate(NORESM85b55)
#Pull  vals
NORESM85HUM55<-data.frame(terra::extract(NORESM85b55,allcoords1))
NORESM85HUM55$HUM55 <- rowMeans(NORESM85HUM55[,2:366])
rownames(NORESM85HUM55) <- allcoords1$PlotCN
rm(NORESM85b55)
#HUM60
NORESM85b60 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2060.nc", subds="hurs")
NORESM85b60 <- terra::rotate(NORESM85b60)
#Pull  vals
NORESM85HUM60<-data.frame(terra::extract(NORESM85b60,allcoords1))
NORESM85HUM60$HUM60 <- rowMeans(NORESM85HUM60[,2:366])
rownames(NORESM85HUM60) <- allcoords1$PlotCN
rm(NORESM85b60)
#HUM65
NORESM85b65 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2065.nc", subds="hurs")
NORESM85b65 <- terra::rotate(NORESM85b65)
#Pull  vals
NORESM85HUM65<-data.frame(terra::extract(NORESM85b65,allcoords1))
NORESM85HUM65$HUM65 <- rowMeans(NORESM85HUM65[,2:366])
rownames(NORESM85HUM65) <- allcoords1$PlotCN
rm(NORESM85b65)
#HUM70
NORESM85b70 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2070.nc", subds="hurs")
NORESM85b70 <- terra::rotate(NORESM85b70)
#Pull  vals
NORESM85HUM70<-data.frame(terra::extract(NORESM85b70,allcoords1))
NORESM85HUM70$HUM70 <- rowMeans(NORESM85HUM70[,2:366])
rownames(NORESM85HUM70) <- allcoords1$PlotCN
rm(NORESM85b70)
#HUM75
NORESM85b75 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2075.nc", subds="hurs")
NORESM85b75 <- terra::rotate(NORESM85b75)
#Pull  vals
NORESM85HUM75<-data.frame(terra::extract(NORESM85b75,allcoords1))
NORESM85HUM75$HUM75 <- rowMeans(NORESM85HUM75[,2:366])
rownames(NORESM85HUM75) <- allcoords1$PlotCN
rm(NORESM85b75)
#HUM80
NORESM85b80 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85Hurs2080.nc", subds="hurs")
NORESM85b80 <- terra::rotate(NORESM85b80)
#Pull  vals
NORESM85HUM80<-data.frame(terra::extract(NORESM85b80,allcoords1))
NORESM85HUM80$HUM80 <- rowMeans(NORESM85HUM80[,2:366])
#convert kg/m2/s -> to mm
NORESM85HUM80$HUM80 <- NORESM85HUM80$HUM80
rownames(NORESM85HUM80) <- allcoords1$PlotCN
rm(NORESM85b80)

NORESM85HUMFull<- data.frame(cbind(NORESM85HUM25$HUM25,NORESM85HUM30$HUM30,NORESM85HUM35$HUM35,NORESM85HUM40$HUM40, NORESM85HUM45$HUM45,
                                   NORESM85HUM50$HUM50, NORESM85HUM55$HUM55, NORESM85HUM60$HUM60, NORESM85HUM65$HUM65, NORESM85HUM70$HUM70, NORESM85HUM75$HUM75, NORESM85HUM80$HUM80))

colnames(NORESM85HUMFull)<- c("HUM25","HUM30","HUM35","HUM40", "HUM45","HUM50", "HUM55", "HUM60", "HUM65", "HUM70", "HUM75", "HUM80")

NORESM85HUM1519 <- as.data.frame(rbind(NORESMHUM2015[,c(368,367)], NORESMHUM2016[,c(368,367)], NORESMHUM2017[,c(368,367)], NORESMHUM2018[,c(368,367)],
                                       NORESMHUM2019[,c(368,367)]))


rm(NORESM85HUM25,NORESM85HUM30,NORESM85HUM35,NORESM85HUM40, NORESM85HUM45,
   NORESM85HUM50, NORESM85HUM55, NORESM85HUM60, NORESM85HUM65, NORESM85HUM70, NORESM85HUM75, NORESM85HUM80)
rownames(NORESM85HUMFull) <- allcoords1$PlotCN
#RAD
#2015
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2015  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2015.nc", subds="rsds")
NORESM2015  <- terra::rotate(NORESM2015)
#Pull  vals
NORESMRAD2015<-data.frame(terra::extract(NORESM2015 ,allcoords[allcoords$Year == 2015,]))
NORESMRAD2015$AvgRAD <- rowMeans(NORESMRAD2015[,2:366])
#Add PlotCN
NORESMRAD2015$PlotCN <- allcoords[allcoords$Year == 2015,]$PlotCN
rm(NORESM2015)
#2016
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2016  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2016.nc", subds="rsds")
NORESM2016  <- terra::rotate(NORESM2016)
#Pull  vals
NORESMRAD2016<-data.frame(terra::extract(NORESM2016 ,allcoords[allcoords$Year == 2016,]))
NORESMRAD2016$AvgRAD <- rowMeans(NORESMRAD2016[,2:366])
#Add PlotCN
NORESMRAD2016$PlotCN <- allcoords[allcoords$Year == 2016,]$PlotCN
rm(NORESM2016)
#2017
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2017  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2017.nc", subds="rsds")
NORESM2017  <- terra::rotate(NORESM2017)
#Pull  vals
NORESMRAD2017<-data.frame(terra::extract(NORESM2017 ,allcoords[allcoords$Year == 2017,]))
NORESMRAD2017$AvgRAD <- rowMeans(NORESMRAD2017[,2:366])
#Add PlotCN
NORESMRAD2017$PlotCN <- allcoords[allcoords$Year == 2017,]$PlotCN
rm(NORESM2017)
#2018
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2018  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2018.nc", subds="rsds")
NORESM2018  <- terra::rotate(NORESM2018)
#Pull  vals
NORESMRAD2018<-data.frame(terra::extract(NORESM2018 ,allcoords[allcoords$Year == 2018,]))
NORESMRAD2018$AvgRAD <- rowMeans(NORESMRAD2018[,2:366])
#Add PlotCN
NORESMRAD2018$PlotCN <- allcoords[allcoords$Year == 2018,]$PlotCN
rm(NORESM2018)
#2019
setwd("~/Desktop/NASAClim/NORESM85")
NORESM2019  <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2019.nc", subds="rsds")
NORESM2019  <- terra::rotate(NORESM2019)
#Pull  vals
NORESMRAD2019<-data.frame(terra::extract(NORESM2019 ,allcoords[allcoords$Year == 2019,]))
NORESMRAD2019$AvgRAD <- rowMeans(NORESMRAD2019[,2:366])
#Add PlotCN
NORESMRAD2019$PlotCN <- allcoords[allcoords$Year == 2019,]$PlotCN
rm(NORESM2019)

#RAD25
NORESM85b25 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2025.nc", subds="rsds")
NORESM85b25 <- terra::rotate(NORESM85b25)
#Pull  vals
NORESM85RAD25<-data.frame(terra::extract(NORESM85b25,allcoords1))
NORESM85RAD25$RAD25 <- rowMeans(NORESM85RAD25[,2:366])
rownames(NORESM85RAD25) <- allcoords1$PlotCN
rm(NORESM85b25)
#RAD30
NORESM85b30 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2030.nc", subds="rsds")
NORESM85b30 <- terra::rotate(NORESM85b30)
#Pull  vals
NORESM85RAD30<-data.frame(terra::extract(NORESM85b30,allcoords1))
NORESM85RAD30$RAD30 <- rowMeans(NORESM85RAD30[,2:366])
rownames(NORESM85RAD30) <- allcoords1$PlotCN
rm(NORESM85b30)
#RAD35
NORESM85b35 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2035.nc", subds="rsds")
NORESM85b35 <- terra::rotate(NORESM85b35)
#Pull  vals
NORESM85RAD35<-data.frame(terra::extract(NORESM85b35,allcoords1))
NORESM85RAD35$RAD35 <- rowMeans(NORESM85RAD35[,2:366])
rownames(NORESM85RAD35) <- allcoords1$PlotCN
rm(NORESM85b35)
#RAD40
NORESM85b40 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2040.nc", subds="rsds")
NORESM85b40 <- terra::rotate(NORESM85b40)
#Pull  vals
NORESM85RAD40<-data.frame(terra::extract(NORESM85b40,allcoords1))
NORESM85RAD40$RAD40 <- rowMeans(NORESM85RAD40[,2:366])
rownames(NORESM85RAD40) <- allcoords1$PlotCN
rm(NORESM85b40)
#RAD45
NORESM85b45 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2045.nc", subds="rsds")
NORESM85b45 <- terra::rotate(NORESM85b45)
#Pull  vals
NORESM85RAD45<-data.frame(terra::extract(NORESM85b45,allcoords1))
NORESM85RAD45$RAD45 <- rowMeans(NORESM85RAD45[,2:366])
rownames(NORESM85RAD45) <- allcoords1$PlotCN
rm(NORESM85b45)

#RAD50
NORESM85b50 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2050.nc", subds="rsds")
NORESM85b50 <- terra::rotate(NORESM85b50)
#Pull  vals
NORESM85RAD50<-data.frame(terra::extract(NORESM85b50,allcoords1))
NORESM85RAD50$RAD50 <- rowMeans(NORESM85RAD50[,2:366])
rownames(NORESM85RAD50) <- allcoords1$PlotCN
rm(NORESM85b50)
#RAD55
NORESM85b55 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2055.nc", subds="rsds")
NORESM85b55 <- terra::rotate(NORESM85b55)
#Pull  vals
NORESM85RAD55<-data.frame(terra::extract(NORESM85b55,allcoords1))
NORESM85RAD55$RAD55 <- rowMeans(NORESM85RAD55[,2:366])
rownames(NORESM85RAD55) <- allcoords1$PlotCN
rm(NORESM85b55)
#RAD60
NORESM85b60 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2060.nc", subds="rsds")
NORESM85b60 <- terra::rotate(NORESM85b60)
#Pull  vals
NORESM85RAD60<-data.frame(terra::extract(NORESM85b60,allcoords1))
NORESM85RAD60$RAD60 <- rowMeans(NORESM85RAD60[,2:366])
rownames(NORESM85RAD60) <- allcoords1$PlotCN
rm(NORESM85b60)
#RAD65
NORESM85b65 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2065.nc", subds="rsds")
NORESM85b65 <- terra::rotate(NORESM85b65)
#Pull  vals
NORESM85RAD65<-data.frame(terra::extract(NORESM85b65,allcoords1))
NORESM85RAD65$RAD65 <- rowMeans(NORESM85RAD65[,2:366])
rownames(NORESM85RAD65) <- allcoords1$PlotCN
rm(NORESM85b65)
#RAD70
NORESM85b70 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2070.nc", subds="rsds")
NORESM85b70 <- terra::rotate(NORESM85b70)
#Pull  vals
NORESM85RAD70<-data.frame(terra::extract(NORESM85b70,allcoords1))
NORESM85RAD70$RAD70 <- rowMeans(NORESM85RAD70[,2:366])
rownames(NORESM85RAD70) <- allcoords1$PlotCN
rm(NORESM85b70)
#RAD75
NORESM85b75 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2075.nc", subds="rsds")
NORESM85b75 <- terra::rotate(NORESM85b75)
#Pull  vals
NORESM85RAD75<-data.frame(terra::extract(NORESM85b75,allcoords1))
NORESM85RAD75$RAD75 <- rowMeans(NORESM85RAD75[,2:366])
rownames(NORESM85RAD75) <- allcoords1$PlotCN
rm(NORESM85b75)
#RAD80
NORESM85b80 <- rast("~/Desktop/NASAClim/NORESM85/NORESM85RAD2080.nc", subds="rsds")
NORESM85b80 <- terra::rotate(NORESM85b80)
#Pull  vals
NORESM85RAD80<-data.frame(terra::extract(NORESM85b80,allcoords1))
NORESM85RAD80$RAD80 <- rowMeans(NORESM85RAD80[,2:366])
#convert kg/m2/s -> to mm
NORESM85RAD80$RAD80 <- NORESM85RAD80$RAD80
rownames(NORESM85RAD80) <- allcoords1$PlotCN
rm(NORESM85b80)

NORESM85RADFull<- data.frame(cbind(NORESM85RAD25$RAD25,NORESM85RAD30$RAD30,NORESM85RAD35$RAD35,NORESM85RAD40$RAD40, NORESM85RAD45$RAD45,
                                   NORESM85RAD50$RAD50, NORESM85RAD55$RAD55, NORESM85RAD60$RAD60, NORESM85RAD65$RAD65, NORESM85RAD70$RAD70, NORESM85RAD75$RAD75, NORESM85RAD80$RAD80))

colnames(NORESM85RADFull)<- c("RAD25","RAD30","RAD35","RAD40", "RAD45","RAD50", "RAD55", "RAD60", "RAD65", "RAD70", "RAD75", "RAD80")

NORESM85RAD1519 <- as.data.frame(rbind(NORESMRAD2015[,c(368,367)], NORESMRAD2016[,c(368,367)], NORESMRAD2017[,c(368,367)], NORESMRAD2018[,c(368,367)],
                                       NORESMRAD2019[,c(368,367)]))


rm(NORESM85RAD25,NORESM85RAD30,NORESM85RAD35,NORESM85RAD40, NORESM85RAD45,
   NORESM85RAD50, NORESM85RAD55, NORESM85RAD60, NORESM85RAD65, NORESM85RAD70, NORESM85RAD75, NORESM85RAD80)
rownames(NORESM85RADFull) <- allcoords1$PlotCN

NORESM26ALL <- as.data.frame(cbind(NORESM26TempFull,NORESM26PrecFull,NORESM26HUMFull,NORESM26RADFull))
NORESM45ALL <- as.data.frame(cbind(NORESM45TempFull,NORESM45PrecFull,NORESM45HUMFull,NORESM45RADFull))
NORESM85ALL <- as.data.frame(cbind(NORESM85TempFull,NORESM85PrecFull,NORESM85HUMFull,NORESM85RADFull))

write.csv(NORESM26ALL, file="NORESM26ALL.csv")
write.csv(NORESM45ALL, file="NORESM45ALL.csv")
write.csv(NORESM85ALL, file="NORESM85ALL.csv")


avg_clim <- function(..., na.rm = TRUE) {
  df_list <- list(...)
  result <- Reduce("+", df_list) / length(df_list)
  return(result)
}

A26MIROC1519 <- merge(MIROC26TEMP1519,MIROC26PREC1519, by="PlotCN")%>%merge(MIROC26HUM1519, by="PlotCN")%>%merge(MIROC26RAD1519, by="PlotCN")
A45MIROC1519 <- merge(MIROC45TEMP1519,MIROC45PREC1519, by="PlotCN")%>%merge(MIROC45HUM1519, by="PlotCN")%>%merge(MIROC45RAD1519, by="PlotCN")
A85MIROC1519 <- merge(MIROC85TEMP1519,MIROC85PREC1519, by="PlotCN")%>%merge(MIROC85HUM1519, by="PlotCN")%>%merge(MIROC85RAD1519, by="PlotCN")
#average across multiple dataframes
MIROC1519ALL <- avg_clim(A26MIROC1519, A45MIROC1519, A85MIROC1519)
write.csv(MIROC1519ALL, file="MIROC1519ALL.csv")

A26GFDL1519 <- merge(GFDL26TEMP1519,GFDL26PREC1519, by="PlotCN")%>%merge(GFDL26HUM1519, by="PlotCN")%>%merge(GFDL26RAD1519, by="PlotCN")
A45GFDL1519 <- merge(GFDL45TEMP1519,GFDL45PREC1519, by="PlotCN")%>%merge(GFDL45HUM1519, by="PlotCN")%>%merge(GFDL45RAD1519, by="PlotCN")
A85GFDL1519 <- merge(GFDL85TEMP1519,GFDL85PREC1519, by="PlotCN")%>%merge(GFDL85HUM1519, by="PlotCN")%>%merge(GFDL85RAD1519, by="PlotCN")
#average across multiple dataframes
GFDL1519ALL <- avg_clim(A26GFDL1519, A45GFDL1519, A85GFDL1519)
write.csv(GFDL1519ALL, file="GFDL1519ALL.csv")

A26NORESM1519 <- merge(NORESM26TEMP1519,NORESM26PREC1519, by="PlotCN")%>%merge(NORESM26HUM1519, by="PlotCN")%>%merge(NORESM26RAD1519, by="PlotCN")
A45NORESM1519 <- merge(NORESM45TEMP1519,NORESM45PREC1519, by="PlotCN")%>%merge(NORESM45HUM1519, by="PlotCN")%>%merge(NORESM45RAD1519, by="PlotCN")
A85NORESM1519 <- merge(NORESM85TEMP1519,NORESM85PREC1519, by="PlotCN")%>%merge(NORESM85HUM1519, by="PlotCN")%>%merge(NORESM85RAD1519, by="PlotCN")
#average across multiple dataframes
NORESM1519ALL <- avg_clim(A26NORESM1519, A45NORESM1519, A85NORESM1519)
write.csv(NORESM1519ALL, file="NORESM1519ALL.csv")

#restart R and read in files again, holding all files can crash R
setwd("~/Desktop/FIA/")
GFDL1519ALL <- read.csv("GFDL1519ALL.csv")
MIROC1519ALL <- read.csv("MIROC1519ALL.csv")
NORESM1519ALL <- read.csv("NORESM1519ALL.csv")
GFDL1519ALL<- GFDL1519ALL[-c(1)]
MIROC1519ALL<-MIROC1519ALL[-c(1)]
NORESM1519ALL<-NORESM1519ALL[-c(1)]

#name columns
colnames(GFDL1519ALL)<- c("PlotCN","MAT","PPT","RHUM","RAD")
colnames(MIROC1519ALL) <- c("PlotCN","MAT","PPT","RHUM","RAD")
colnames(NORESM1519ALL) <- c("PlotCN","MAT","PPT","RHUM","RAD")
#create model averages
midAtl1519ALL <- avg_clim(GFDL1519ALL[,2:5],MIROC1519ALL[,2:5],NORESM1519ALL[,2:5])
midAtl1519ALL$PlotCN <- GFDL1519ALL$PlotCN
midAtl1519ALL <- midAtl1519ALL[,c(5,1:4)]
#combine 2000-2014 to 2015-2020
#read in 0014 files
midAtlTemp0014<- read.csv('midAtlTemp0014.csv')
midAtlPREC0014<-read.csv('midAtlPREC0014.csv')
midAtlHUM0014<-read.csv('midAtlHUM0014.csv')
midAtlRAD0014<-read.csv('midAtlRAD0014.csv')
midAtlTemp0014<- midAtlTemp0014[-c(1)]
midAtlPREC0014<-midAtlPREC0014[-c(1)]
midAtlHUM0014<-midAtlHUM0014[-c(1)]
midAtlRAD0014<-midAtlRAD0014[-c(1)]
midAtlTemp0014<- distinct(midAtlTemp0014)
midAtlPREC0014<-distinct(midAtlPREC0014)
midAtlHUM0014<-distinct(midAtlHUM0014)
midAtlRAD0014<-distinct(midAtlRAD0014)
#combine all 00-14 files
midAtl0014ALL <- merge(midAtlTemp0014,midAtlPREC0014, by="PlotCN")%>%merge(midAtlHUM0014,by="PlotCN")%>%merge(midAtlRAD0014,by="PlotCN")
#combine 00-14 with 15-19
midAtlAllClimH <- rbind(midAtl0014ALL,midAtl1519ALL)
#combine with FIA data
allFIA <- read.csv('allFIAinc.csv')
allFIA <- allFIA[-c(2)]
allFIA<- merge(allFIA, midAtlAllClimH, by="PlotCN", all.x=TRUE)
allFIA <- na.omit(allFIA)
write.csv(allFIA,file="allFIAdata.csv")

#read in future projection models
GFDL26ALL <- read.csv('GFDL26ALL.csv')
GFDL45ALL <- read.csv('GFDL45ALL.csv')
GFDL85ALL <- read.csv('GFDL85ALL.csv')
MIROC26ALL <- read.csv('MIROC26ALL.csv')
MIROC45ALL <- read.csv('MIROC45ALL.csv')
MIROC85ALL <- read.csv('MIROC85ALL.csv')
NORESM26ALL <- read.csv('NORESM26ALL.csv')
NORESM45ALL <- read.csv('NORESM45ALL.csv')
NORESM85ALL <- read.csv('NORESM85ALL.csv')

names(GFDL26ALL)[1] <- "PlotCN"
names(GFDL45ALL)[1] <- "PlotCN"
names(GFDL85ALL)[1] <- "PlotCN"
names(MIROC26ALL)[1] <- "PlotCN"
names(MIROC45ALL)[1] <- "PlotCN"
names(MIROC85ALL)[1] <- "PlotCN"
names(NORESM26ALL)[1] <- "PlotCN"
names(NORESM45ALL)[1] <- "PlotCN"
names(NORESM85ALL)[1] <- "PlotCN"
#combine models and avg values
midAtlClim26 <- avg_clim(GFDL26ALL[,2:49],MIROC26ALL[,2:49],NORESM26ALL[,2:49])
midAtlClim45 <- avg_clim(GFDL45ALL[,2:49],MIROC45ALL[,2:49],NORESM45ALL[,2:49])
midAtlClim85 <- avg_clim(GFDL85ALL[,2:49],MIROC85ALL[,2:49],NORESM85ALL[,2:49])
#add PlotCN
midAtlClim26 <- cbind(GFDL26ALL$PlotCN,midAtlClim26) 
midAtlClim45 <- cbind(GFDL45ALL$PlotCN,midAtlClim45) 
midAtlClim85 <- cbind(GFDL85ALL$PlotCN,midAtlClim85) 
names(midAtlClim26)[1] <- "PlotCN"
names(midAtlClim45)[1] <- "PlotCN"
names(midAtlClim85)[1] <- "PlotCN"
#save files
write.csv(midAtlClim26,file="midAtlClim26.csv")
write.csv(midAtlClim45,file="midAtlClim45.csv")
write.csv(midAtlClim85,file="midAtlClim85.csv")

#DO SQI
SQIdata <- read.csv("midAtlSQI.csv")
SQIdata <- SQIdata[-c(1)]
SQIdata <- merge(SQIdata,midAtlAllClimH, by="PlotCN", all.x=TRUE)
SQIdata <- na.omit(SQIdata)


##Lastly, subset plots within MidAtlantic Ecoregion area
#overlap with state boundaries
#read in regional shapefile
midatlmap <- vect("/Users/DamaniEubanks/Desktop/FIA/MARshape/v10/mid-atlantic_and_new_england.gdb")
setwd("~/Desktop/FIA/WI")
spUSA <- shapefile("cb_2021_us_state_500k.shp")
#Clip shapefile to only midatl
midcut <- subset(spUSA, NAME%in%c("Maryland","New Jersey","New York","Delaware","Pennsylvania"))
midcut <- vect(midcut)
midcut <- project(midcut, crs(midatlmap))
ex <- terra::ext(midcut)
#re read regional shapefile with extent
midatlmap <- vect("/Users/DamaniEubanks/Desktop/FIA/MARshape/v10/mid-atlantic_and_new_england.gdb",extent=ex)
#overlap full states with region map, plot for figure
par(mar=c(5,6,4,2)+.1)
#mgp(title=5)
plot(midatlmap, col="Grey",axes=FALSE)
plot(midcut,add=TRUE)


# Load the regional map
midatlmap <- vect("/Users/DamaniEubanks/Desktop/FIA/MARshape/v10/mid-atlantic_and_new_england.gdb")
# Load and subset US states
setwd("~/Desktop/FIA/WI")
spUSA <- vect("cb_2021_us_state_500k.shp")
midcut <- subset(spUSA ,spUSA$NAME %in% c("Maryland", "New Jersey", "New York", "Delaware", "Pennsylvania"))
# Reproject to match CRS
midcut <- project(midcut, crs(midatlmap))
#check for overlap points
allFIAcoords <- allFIA[,c(1,9,8)]
SQIcoords <- SQIdata[,c(1,5,4)]
allFIAcoords <- terra::vect(allFIAcoords, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(allFIAcoords) <- "EPSG:4326"
allFIAcoords <- terra::project(allFIAcoords,crs(midatlmap))
SQIcoords <-  terra::vect(SQIcoords, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(SQIcoords) <- "EPSG:4326"
SQIcoords <- terra::project(SQIcoords, crs(midatlmap))
#cut points that are outside of regional map
allFIAcut <- terra::intersect(allFIAcoords,midatlmap)
allSQIcut <- terra::intersect(SQIcoords,midatlmap)
#
allFIAcut <- st_as_sf(allFIAcut)
allSQIcut <- st_as_sf(allSQIcut)
#visualize

# Get combined extent of both layers
ext1 <- as.vector(ext(midatlmap)) 
ext2 <- as.vector(ext(midcut))
# Calculate union extent
combined_ext <- c(
  min(ext1[1], ext2[1]),  # xmin
  max(ext1[2], ext2[2]),  # xmax
  min(ext1[3], ext2[3]),  # ymin
  max(ext1[4], ext2[4])   # ymax
)
combined_ext <- ext(combined_ext)


midatl_sf <- st_as_sf(midatlmap)
midcut_sf <- st_as_sf(midcut)

#plot 
ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  annotation_scale(
    location = "bl",          
    width_hint = 0.25,       
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  geom_sf(data = allSQIcut, # one time with FIAcut, one time with SQIcut
          color = "black",    
          shape = 3,          
          size = 1, 
          stroke = 1) +
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ),
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  )

#inset map of Eastern US
setwd("~/Desktop/FIA/bound_p")
spUSA1 <- vect("boundaries_p_2021_v3.shp")
e <- ext(-5000,3000000, -2400000, 1700000)
spUSA1 <- crop(spUSA1,e)

UScut <- project(spUSA1, crs(midatlmap))
UScut_sf <- st_as_sf(UScut)

ggplot() +
 geom_sf(data = midatl_sf,
          fill = "grey",  
          color = NA) + 
  geom_sf(data = UScut_sf, 
          fill = NA,         
          color = "black") +
  annotation_scale(
    location = "br",          
    width_hint = 0.25,       
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) + theme_void()



#cut
allFIA1 <- allFIA %>% subset(PlotCN%in%allFIAcut$PlotCN)
SQIdata1 <- SQIdata %>% subset(PlotCN%in%allSQIcut$PlotCN)
#save final files
setwd("~/Desktop/FIA/")
write.csv(allFIA1, file="allFIAfinal.csv")
write.csv(SQIdata1, file="allSQIfinal.csv")

#add basal per plot to relate to mortality. 
allFIA <- read.csv("allFIAfinal.csv")
allFIA <- allFIA[-c(1)]

BasalDead <- tpa(db=midAtl, method="annual",byPlot = TRUE,treeType = "dead")
names(BasalDead)[3] <- "PlotCN"
names(BasalDead)[5] <- "BAdead"
allFIA <- merge(allFIA,BasalDead[,c(3,5)],by="PlotCN", all.x=TRUE)
BasalDead <- merge(BasalDead,PlotData[,c(1,4:5)], by="PlotCN", all.x=TRUE)
BasalDeadChange <- BasalDead %>%
  group_by(Latitude,Longitude) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    BAdeadChange = BAdead - lag(BAdead),
    YearsBetween = YEAR - lag(YEAR),  
    AnnualBAdeadChange = BAdeadChange / YearsBetween
  ) %>%
  ungroup()
AvgBADeadRate <- BasalDeadChange %>% group_by(Latitude,Longitude) %>% dplyr::summarise(mean(AnnualBAdeadChange,na.rm=TRUE))
names(AvgBADeadRate)[3] <- "AvgBADeadRate"
AvgBADeadRate <- merge(PlotData[,c(1,4:5)],AvgBADeadRate, by=c("Latitude","Longitude"), all.y=TRUE)
allFIA <- merge(allFIA,AvgBADeadRate[,c(3,4)],by="PlotCN", all.x=TRUE)
BasalAll <- tpa(db=midAtl, method="annual",byPlot = TRUE,treeType = "all")
names(BasalAll)[3] <- "PlotCN"
names(BasalAll)[5] <- "BAAll"
allFIA <- merge(allFIA,BasalAll[,c(3,5)],by="PlotCN", all.x=TRUE)
write.csv(allFIA,file="allFIAfinal.csv")


#Future Landuse Chen et al, files provided in github data section, different folder for each RCP/SSP scenario, 2.6,4.5,8.5
allFIAcoords <- allFIA[,c(1,9,8)]
SQIcoords <- SQIdata[,c(1,5,4)]
allFIAcoords <- allFIAcoords[!duplicated(allFIAcoords[c("Longitude", "Latitude")]), ]
SQIcoords <- SQIcoords[!duplicated(SQIcoords[c("Longitude", "Latitude")]), ]
combcoords <- rbind(allFIAcoords,SQIcoords)
combcoords <-  SpatialPointsDataFrame(combcoords[,2:3], data = combcoords, proj4string =CRS("+proj=longlat +datum=WGS84 +no_defs"))
#parallel processing
registerDoParallel(cores=detectCores()-1)
#function to open cdf
processcdf <- function(nc, varname, lon, lat, fillvalue) {
  pft <- ncvar_get(nc, varname)
  pft[pft == fillvalue$value] <- NA
  raster(pft, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
         crs=CRS("+proj=longlat +datum=WGS84 +no_defs"))
}

#SSP1RCP26
setwd("~/Desktop/ChenProjectedLU/SSP1RCP26")
#2020
system.time({
  rcp2620 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2020.nc")
  lon <- ncvar_get(rcp2620, "longitude")
  lat <- ncvar_get(rcp2620, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2620, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2620, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2620)
  rcp2620brick <- brick(rpft_list)
  rcp2620brick <- projectRaster(rcp2620brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2620LU <- data.frame(raster::extract(rcp2620brick, combcoords))
})
stopImplicitCluster()
All2620LU$ForestTotal20 <- rowSums(All2620LU, na.rm=TRUE)
rm(rcp2620,rcp2620brick)
#2025
system.time({
  rcp2625 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2025.nc")
  lon <- ncvar_get(rcp2625, "longitude")
  lat <- ncvar_get(rcp2625, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2625, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2625, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2625)
  rcp2625brick <- brick(rpft_list)
  rcp2625brick <- projectRaster(rcp2625brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2625LU <- data.frame(raster::extract(rcp2625brick, combcoords))
})
stopImplicitCluster()
All2625LU$ForestTotal25 <- rowSums(All2625LU, na.rm=TRUE)
rm(rcp2625,rcp2625brick)
#2030
system.time({
  rcp2630 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2030.nc")
  lon <- ncvar_get(rcp2630, "longitude")
  lat <- ncvar_get(rcp2630, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2630, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2630, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2630)
  rcp2630brick <- brick(rpft_list)
  rcp2630brick <- projectRaster(rcp2630brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2630LU <- data.frame(raster::extract(rcp2630brick, combcoords))
})
stopImplicitCluster()
All2630LU$ForestTotal30 <- rowSums(All2630LU, na.rm=TRUE)
rm(rcp2630,rcp2630brick)
#2035
system.time({
  rcp2635 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2035.nc")
  lon <- ncvar_get(rcp2635, "longitude")
  lat <- ncvar_get(rcp2635, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2635, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2635, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2635)
  rcp2635brick <- brick(rpft_list)
  rcp2635brick <- projectRaster(rcp2635brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2635LU <- data.frame(raster::extract(rcp2635brick, combcoords))
})
stopImplicitCluster()
All2635LU$ForestTotal35 <- rowSums(All2635LU, na.rm=TRUE)
rm(rcp2635,rcp2635brick)
#2040
system.time({
  rcp2640 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2040.nc")
  lon <- ncvar_get(rcp2640, "longitude")
  lat <- ncvar_get(rcp2640, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2640, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2640, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2640)
  rcp2640brick <- brick(rpft_list)
  rcp2640brick <- projectRaster(rcp2640brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2640LU <- data.frame(raster::extract(rcp2640brick, combcoords))
})
stopImplicitCluster()
All2640LU$ForestTotal40 <- rowSums(All2640LU, na.rm=TRUE)
rm(rcp2640,rcp2640brick)
#2045
system.time({
  rcp2645 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2045.nc")
  lon <- ncvar_get(rcp2645, "longitude")
  lat <- ncvar_get(rcp2645, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2645, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2645, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2645)
  rcp2645brick <- brick(rpft_list)
  rcp2645brick <- projectRaster(rcp2645brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2645LU <- data.frame(raster::extract(rcp2645brick, combcoords))
})
stopImplicitCluster()
All2645LU$ForestTotal45 <- rowSums(All2645LU, na.rm=TRUE)
rm(rcp2645,rcp2645brick)
#2050
system.time({
  rcp2650 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2050.nc")
  lon <- ncvar_get(rcp2650, "longitude")
  lat <- ncvar_get(rcp2650, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2650, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2650, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2650)
  rcp2650brick <- brick(rpft_list)
  rcp2650brick <- projectRaster(rcp2650brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2650LU <- data.frame(raster::extract(rcp2650brick, combcoords))
})
stopImplicitCluster()
All2650LU$ForestTotal50 <- rowSums(All2650LU, na.rm=TRUE)
rm(rcp2650,rcp2650brick)
#2055
system.time({
  rcp2655 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2055.nc")
  lon <- ncvar_get(rcp2655, "longitude")
  lat <- ncvar_get(rcp2655, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2655, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2655, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2655)
  rcp2655brick <- brick(rpft_list)
  rcp2655brick <- projectRaster(rcp2655brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2655LU <- data.frame(raster::extract(rcp2655brick, combcoords))
})
stopImplicitCluster()
All2655LU$ForestTotal55 <- rowSums(All2655LU, na.rm=TRUE)
rm(rcp2655,rcp2655brick)
#2060
system.time({
  rcp2660 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2060.nc")
  lon <- ncvar_get(rcp2660, "longitude")
  lat <- ncvar_get(rcp2660, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2660, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2660, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2660)
  rcp2660brick <- brick(rpft_list)
  rcp2660brick <- projectRaster(rcp2660brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2660LU <- data.frame(raster::extract(rcp2660brick, combcoords))
})
stopImplicitCluster()
All2660LU$ForestTotal60 <- rowSums(All2660LU, na.rm=TRUE)
rm(rcp2660,rcp2660brick)
#2065
system.time({
  rcp2665 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2065.nc")
  lon <- ncvar_get(rcp2665, "longitude")
  lat <- ncvar_get(rcp2665, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2665, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2665, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2665)
  rcp2665brick <- brick(rpft_list)
  rcp2665brick <- projectRaster(rcp2665brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2665LU <- data.frame(raster::extract(rcp2665brick, combcoords))
})
stopImplicitCluster()
All2665LU$ForestTotal65 <- rowSums(All2665LU, na.rm=TRUE)
rm(rcp2665,rcp2665brick)
#2070
system.time({
  rcp2670 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2070.nc")
  lon <- ncvar_get(rcp2670, "longitude")
  lat <- ncvar_get(rcp2670, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2670, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2670, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2670)
  rcp2670brick <- brick(rpft_list)
  rcp2670brick <- projectRaster(rcp2670brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2670LU <- data.frame(raster::extract(rcp2670brick, combcoords))
})
stopImplicitCluster()
All2670LU$ForestTotal70 <- rowSums(All2670LU, na.rm=TRUE)
rm(rcp2670,rcp2670brick)
#2075
system.time({
  rcp2675 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2075.nc")
  lon <- ncvar_get(rcp2675, "longitude")
  lat <- ncvar_get(rcp2675, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2675, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2675, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2675)
  rcp2675brick <- brick(rpft_list)
  rcp2675brick <- projectRaster(rcp2675brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2675LU <- data.frame(raster::extract(rcp2675brick, combcoords))
})
stopImplicitCluster()
All2675LU$ForestTotal75 <- rowSums(All2675LU, na.rm=TRUE)
rm(rcp2675,rcp2675brick)
#2080
system.time({
  rcp2680 <- nc_open("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2080.nc")
  lon <- ncvar_get(rcp2680, "longitude")
  lat <- ncvar_get(rcp2680, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp2680, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp2680, varname, lon, lat, fillvalue)
  }
  nc_close(rcp2680)
  rcp2680brick <- brick(rpft_list)
  rcp2680brick <- projectRaster(rcp2680brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All2680LU <- data.frame(raster::extract(rcp2680brick, combcoords))
})
stopImplicitCluster()
All2680LU$ForestTotal80 <- rowSums(All2680LU, na.rm=TRUE)
rm(rcp2680,rcp2680brick)

All26LUTotals <- data.frame(combcoords$PlotCN,All2620LU[,9], All2625LU[,9],
                            All2630LU[,9], All2635LU[,9], All2640LU[,9], All2645LU[,9], All2650LU[,9],
                            All2655LU[,9], All2660LU[,9], All2665LU[,9], All2670LU[,9], All2675LU[,9], All2680LU[,9])
colnames(All26LUTotals) <- c("PlotCN","ForestTotal20","ForestTotal25","ForestTotal30","ForestTotal35","ForestTotal40",
                             "ForestTotal45","ForestTotal50","ForestTotal55","ForestTotal60","ForestTotal65","ForestTotal70","ForestTotal75",
                             "ForestTotal80")
#estimate FAD class, repeat for each time frame, 2020-2080
All26LUTotals$FADClass20 <- ifelse(All26LUTotals$ForestTotal20 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal20 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal20 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal20>=10, 2, 1))))
All26LUTotals$FADClass25 <- ifelse(All26LUTotals$ForestTotal25 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal25 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal25 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal25>=10, 2, 1))))
All26LUTotals$FADClass30 <- ifelse(All26LUTotals$ForestTotal30 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal30 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal30 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal30>=10, 2, 1))))
All26LUTotals$FADClass35 <- ifelse(All26LUTotals$ForestTotal35 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal35 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal35 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal35>=10, 2, 1))))
All26LUTotals$FADClass40 <- ifelse(All26LUTotals$ForestTotal40 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal40 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal40 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal40>=10, 2, 1))))
All26LUTotals$FADClass45 <- ifelse(All26LUTotals$ForestTotal45 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal45 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal45 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal45>=10, 2, 1))))
All26LUTotals$FADClass50 <- ifelse(All26LUTotals$ForestTotal50 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal50 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal50 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal50>=10, 2, 1))))
All26LUTotals$FADClass55 <- ifelse(All26LUTotals$ForestTotal55 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal55 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal55 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal55>=10, 2, 1))))
All26LUTotals$FADClass60 <- ifelse(All26LUTotals$ForestTotal60 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal60 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal60 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal60>=10, 2, 1))))
All26LUTotals$FADClass65 <- ifelse(All26LUTotals$ForestTotal65 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal65 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal65 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal65>=10, 2, 1))))
All26LUTotals$FADClass70 <- ifelse(All26LUTotals$ForestTotal70 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal70 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal70 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal70>=10, 2, 1))))
All26LUTotals$FADClass75 <- ifelse(All26LUTotals$ForestTotal75 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal75 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal75 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal75>=10, 2, 1))))
All26LUTotals$FADClass80 <- ifelse(All26LUTotals$ForestTotal80 >=90, 5,
                                   ifelse(All26LUTotals$ForestTotal80 >=60, 4,
                                          ifelse(All26LUTotals$ForestTotal80 >=40, 3,
                                                 ifelse(All26LUTotals$ForestTotal80>=10, 2, 1))))


#SSP2RCP45
setwd("~/Desktop/ChenProjectedLU/SSP2RCP45")
#2020
system.time({
  rcp4520 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2020.nc")
  lon <- ncvar_get(rcp4520, "longitude")
  lat <- ncvar_get(rcp4520, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4520, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4520, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4520)
  rcp4520brick <- brick(rpft_list)
  rcp4520brick <- projectRaster(rcp4520brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4520LU <- data.frame(raster::extract(rcp4520brick, combcoords))
})
stopImplicitCluster()
All4520LU$ForestTotal20 <- rowSums(All4520LU, na.rm=TRUE)
rm(rcp4520,rcp4520brick)
#2025
system.time({
  rcp4525 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2025.nc")
  lon <- ncvar_get(rcp4525, "longitude")
  lat <- ncvar_get(rcp4525, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4525, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4525, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4525)
  rcp4525brick <- brick(rpft_list)
  rcp4525brick <- projectRaster(rcp4525brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4525LU <- data.frame(raster::extract(rcp4525brick, combcoords))
})
stopImplicitCluster()
All4525LU$ForestTotal25 <- rowSums(All4525LU, na.rm=TRUE)
rm(rcp4525,rcp4525brick)
#2030
system.time({
  rcp4530 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2030.nc")
  lon <- ncvar_get(rcp4530, "longitude")
  lat <- ncvar_get(rcp4530, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4530, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4530, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4530)
  rcp4530brick <- brick(rpft_list)
  rcp4530brick <- projectRaster(rcp4530brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4530LU <- data.frame(raster::extract(rcp4530brick, combcoords))
})
stopImplicitCluster()
All4530LU$ForestTotal30 <- rowSums(All4530LU, na.rm=TRUE)
rm(rcp4530,rcp4530brick)
#2035
system.time({
  rcp4535 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2035.nc")
  lon <- ncvar_get(rcp4535, "longitude")
  lat <- ncvar_get(rcp4535, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4535, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4535, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4535)
  rcp4535brick <- brick(rpft_list)
  rcp4535brick <- projectRaster(rcp4535brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4535LU <- data.frame(raster::extract(rcp4535brick, combcoords))
})
stopImplicitCluster()
All4535LU$ForestTotal35 <- rowSums(All4535LU, na.rm=TRUE)
rm(rcp4535,rcp4535brick)
#2040
system.time({
  rcp4540 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2040.nc")
  lon <- ncvar_get(rcp4540, "longitude")
  lat <- ncvar_get(rcp4540, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4540, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4540, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4540)
  rcp4540brick <- brick(rpft_list)
  rcp4540brick <- projectRaster(rcp4540brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4540LU <- data.frame(raster::extract(rcp4540brick, combcoords))
})
stopImplicitCluster()
All4540LU$ForestTotal40 <- rowSums(All4540LU, na.rm=TRUE)
rm(rcp4540,rcp4540brick)
#2045
system.time({
  rcp4545 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2045.nc")
  lon <- ncvar_get(rcp4545, "longitude")
  lat <- ncvar_get(rcp4545, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4545, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4545, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4545)
  rcp4545brick <- brick(rpft_list)
  rcp4545brick <- projectRaster(rcp4545brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4545LU <- data.frame(raster::extract(rcp4545brick, combcoords))
})
stopImplicitCluster()
All4545LU$ForestTotal45 <- rowSums(All4545LU, na.rm=TRUE)
rm(rcp4545,rcp4545brick)
#2050
system.time({
  rcp4550 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2050.nc")
  lon <- ncvar_get(rcp4550, "longitude")
  lat <- ncvar_get(rcp4550, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4550, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4550, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4550)
  rcp4550brick <- brick(rpft_list)
  rcp4550brick <- projectRaster(rcp4550brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4550LU <- data.frame(raster::extract(rcp4550brick, combcoords))
})
stopImplicitCluster()
All4550LU$ForestTotal50 <- rowSums(All4550LU, na.rm=TRUE)
rm(rcp4550,rcp4550brick)
#2055
system.time({
  rcp4555 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2055.nc")
  lon <- ncvar_get(rcp4555, "longitude")
  lat <- ncvar_get(rcp4555, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4555, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4555, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4555)
  rcp4555brick <- brick(rpft_list)
  rcp4555brick <- projectRaster(rcp4555brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4555LU <- data.frame(raster::extract(rcp4555brick, combcoords))
})
stopImplicitCluster()
All4555LU$ForestTotal55 <- rowSums(All4555LU, na.rm=TRUE)
rm(rcp4555,rcp4555brick)
#2060
system.time({
  rcp4560 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2060.nc")
  lon <- ncvar_get(rcp4560, "longitude")
  lat <- ncvar_get(rcp4560, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4560, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4560, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4560)
  rcp4560brick <- brick(rpft_list)
  rcp4560brick <- projectRaster(rcp4560brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4560LU <- data.frame(raster::extract(rcp4560brick, combcoords))
})
stopImplicitCluster()
All4560LU$ForestTotal60 <- rowSums(All4560LU, na.rm=TRUE)
rm(rcp4560,rcp4560brick)
#2065
system.time({
  rcp4565 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2065.nc")
  lon <- ncvar_get(rcp4565, "longitude")
  lat <- ncvar_get(rcp4565, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4565, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4565, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4565)
  rcp4565brick <- brick(rpft_list)
  rcp4565brick <- projectRaster(rcp4565brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4565LU <- data.frame(raster::extract(rcp4565brick, combcoords))
})
stopImplicitCluster()
All4565LU$ForestTotal65 <- rowSums(All4565LU, na.rm=TRUE)
rm(rcp4565,rcp4565brick)
#2070
system.time({
  rcp4570 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2070.nc")
  lon <- ncvar_get(rcp4570, "longitude")
  lat <- ncvar_get(rcp4570, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4570, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4570, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4570)
  rcp4570brick <- brick(rpft_list)
  rcp4570brick <- projectRaster(rcp4570brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4570LU <- data.frame(raster::extract(rcp4570brick, combcoords))
})
stopImplicitCluster()
All4570LU$ForestTotal70 <- rowSums(All4570LU, na.rm=TRUE)
rm(rcp4570,rcp4570brick)
#2075
system.time({
  rcp4575 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2075.nc")
  lon <- ncvar_get(rcp4575, "longitude")
  lat <- ncvar_get(rcp4575, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4575, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4575, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4575)
  rcp4575brick <- brick(rpft_list)
  rcp4575brick <- projectRaster(rcp4575brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4575LU <- data.frame(raster::extract(rcp4575brick, combcoords))
})
stopImplicitCluster()
All4575LU$ForestTotal75 <- rowSums(All4575LU, na.rm=TRUE)
rm(rcp4575,rcp4575brick)
#2080
system.time({
  rcp4580 <- nc_open("GCAM_Demeter_LU_ssp2_rcp45_modelmean_2080.nc")
  lon <- ncvar_get(rcp4580, "longitude")
  lat <- ncvar_get(rcp4580, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp4580, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp4580, varname, lon, lat, fillvalue)
  }
  nc_close(rcp4580)
  rcp4580brick <- brick(rpft_list)
  rcp4580brick <- projectRaster(rcp4580brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All4580LU <- data.frame(raster::extract(rcp4580brick, combcoords))
})
stopImplicitCluster()
All4580LU$ForestTotal80 <- rowSums(All4580LU, na.rm=TRUE)
rm(rcp4580,rcp4580brick)

All45LUTotals <- data.frame(combcoords$PlotCN,All4520LU[,9], All4525LU[,9],
                            All4530LU[,9], All4535LU[,9], All4540LU[,9], All4545LU[,9], All4550LU[,9],
                            All4555LU[,9], All4560LU[,9], All4565LU[,9], All4570LU[,9], All4575LU[,9], All4580LU[,9])
colnames(All45LUTotals) <- c("PlotCN","ForestTotal20","ForestTotal25","ForestTotal30","ForestTotal35","ForestTotal40",
                             "ForestTotal45","ForestTotal50","ForestTotal55","ForestTotal60","ForestTotal65","ForestTotal70","ForestTotal75",
                             "ForestTotal80")
#estimate FAD class, repeat for each time frame, 2020-2080
All45LUTotals$FADClass20 <- ifelse(All45LUTotals$ForestTotal20 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal20 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal20 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal20>=10, 2, 1))))
All45LUTotals$FADClass25 <- ifelse(All45LUTotals$ForestTotal25 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal25 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal25 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal25>=10, 2, 1))))
All45LUTotals$FADClass30 <- ifelse(All45LUTotals$ForestTotal30 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal30 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal30 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal30>=10, 2, 1))))
All45LUTotals$FADClass35 <- ifelse(All45LUTotals$ForestTotal35 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal35 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal35 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal35>=10, 2, 1))))
All45LUTotals$FADClass40 <- ifelse(All45LUTotals$ForestTotal40 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal40 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal40 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal40>=10, 2, 1))))
All45LUTotals$FADClass45 <- ifelse(All45LUTotals$ForestTotal45 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal45 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal45 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal45>=10, 2, 1))))
All45LUTotals$FADClass50 <- ifelse(All45LUTotals$ForestTotal50 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal50 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal50 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal50>=10, 2, 1))))
All45LUTotals$FADClass55 <- ifelse(All45LUTotals$ForestTotal55 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal55 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal55 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal55>=10, 2, 1))))
All45LUTotals$FADClass60 <- ifelse(All45LUTotals$ForestTotal60 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal60 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal60 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal60>=10, 2, 1))))
All45LUTotals$FADClass65 <- ifelse(All45LUTotals$ForestTotal65 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal65 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal65 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal65>=10, 2, 1))))
All45LUTotals$FADClass70 <- ifelse(All45LUTotals$ForestTotal70 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal70 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal70 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal70>=10, 2, 1))))
All45LUTotals$FADClass75 <- ifelse(All45LUTotals$ForestTotal75 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal75 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal75 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal75>=10, 2, 1))))
All45LUTotals$FADClass80 <- ifelse(All45LUTotals$ForestTotal80 >=90, 5,
                                   ifelse(All45LUTotals$ForestTotal80 >=60, 4,
                                          ifelse(All45LUTotals$ForestTotal80 >=40, 3,
                                                 ifelse(All45LUTotals$ForestTotal80>=10, 2, 1))))


setwd("~/Desktop/ChenProjectedLU/SSP5RCP85")
#2020
system.time({
  rcp8520 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2020.nc")
  lon <- ncvar_get(rcp8520, "longitude")
  lat <- ncvar_get(rcp8520, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8520, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8520, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8520)
  rcp8520brick <- brick(rpft_list)
  rcp8520brick <- projectRaster(rcp8520brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8520LU <- data.frame(raster::extract(rcp8520brick, combcoords))
})
stopImplicitCluster()
All8520LU$ForestTotal20 <- rowSums(All8520LU, na.rm=TRUE)
rm(rcp8520,rcp8520brick)
#2025
system.time({
  rcp8525 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2025.nc")
  lon <- ncvar_get(rcp8525, "longitude")
  lat <- ncvar_get(rcp8525, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8525, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8525, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8525)
  rcp8525brick <- brick(rpft_list)
  rcp8525brick <- projectRaster(rcp8525brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8525LU <- data.frame(raster::extract(rcp8525brick, combcoords))
})
stopImplicitCluster()
All8525LU$ForestTotal25 <- rowSums(All8525LU, na.rm=TRUE)
rm(rcp8525,rcp8525brick)
#2030
system.time({
  rcp8530 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2030.nc")
  lon <- ncvar_get(rcp8530, "longitude")
  lat <- ncvar_get(rcp8530, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8530, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8530, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8530)
  rcp8530brick <- brick(rpft_list)
  rcp8530brick <- projectRaster(rcp8530brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8530LU <- data.frame(raster::extract(rcp8530brick, combcoords))
})
stopImplicitCluster()
All8530LU$ForestTotal30 <- rowSums(All8530LU, na.rm=TRUE)
rm(rcp8530,rcp8530brick)
#2035
system.time({
  rcp8535 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2035.nc")
  lon <- ncvar_get(rcp8535, "longitude")
  lat <- ncvar_get(rcp8535, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8535, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8535, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8535)
  rcp8535brick <- brick(rpft_list)
  rcp8535brick <- projectRaster(rcp8535brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8535LU <- data.frame(raster::extract(rcp8535brick, combcoords))
})
stopImplicitCluster()
All8535LU$ForestTotal35 <- rowSums(All8535LU, na.rm=TRUE)
rm(rcp8535,rcp8535brick)
#2040
system.time({
  rcp8540 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2040.nc")
  lon <- ncvar_get(rcp8540, "longitude")
  lat <- ncvar_get(rcp8540, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8540, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8540, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8540)
  rcp8540brick <- brick(rpft_list)
  rcp8540brick <- projectRaster(rcp8540brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8540LU <- data.frame(raster::extract(rcp8540brick, combcoords))
})
stopImplicitCluster()
All8540LU$ForestTotal40 <- rowSums(All8540LU, na.rm=TRUE)
rm(rcp8540,rcp8540brick)
#2045
system.time({
  rcp8545 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2045.nc")
  lon <- ncvar_get(rcp8545, "longitude")
  lat <- ncvar_get(rcp8545, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8545, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8545, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8545)
  rcp8545brick <- brick(rpft_list)
  rcp8545brick <- projectRaster(rcp8545brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8545LU <- data.frame(raster::extract(rcp8545brick, combcoords))
})
stopImplicitCluster()
All8545LU$ForestTotal85 <- rowSums(All8545LU, na.rm=TRUE)
rm(rcp8545,rcp8545brick)
#2050
system.time({
  rcp8550 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2050.nc")
  lon <- ncvar_get(rcp8550, "longitude")
  lat <- ncvar_get(rcp8550, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8550, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8550, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8550)
  rcp8550brick <- brick(rpft_list)
  rcp8550brick <- projectRaster(rcp8550brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8550LU <- data.frame(raster::extract(rcp8550brick, combcoords))
})
stopImplicitCluster()
All8550LU$ForestTotal50 <- rowSums(All8550LU, na.rm=TRUE)
rm(rcp8550,rcp8550brick)
#2055
system.time({
  rcp8555 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2055.nc")
  lon <- ncvar_get(rcp8555, "longitude")
  lat <- ncvar_get(rcp8555, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8555, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8555, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8555)
  rcp8555brick <- brick(rpft_list)
  rcp8555brick <- projectRaster(rcp8555brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8555LU <- data.frame(raster::extract(rcp8555brick, combcoords))
})
stopImplicitCluster()
All8555LU$ForestTotal55 <- rowSums(All8555LU, na.rm=TRUE)
rm(rcp8555,rcp8555brick)
#2060
system.time({
  rcp8560 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2060.nc")
  lon <- ncvar_get(rcp8560, "longitude")
  lat <- ncvar_get(rcp8560, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8560, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8560, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8560)
  rcp8560brick <- brick(rpft_list)
  rcp8560brick <- projectRaster(rcp8560brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8560LU <- data.frame(raster::extract(rcp8560brick, combcoords))
})
stopImplicitCluster()
All8560LU$ForestTotal60 <- rowSums(All8560LU, na.rm=TRUE)
rm(rcp8560,rcp8560brick)
#2065
system.time({
  rcp8565 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2065.nc")
  lon <- ncvar_get(rcp8565, "longitude")
  lat <- ncvar_get(rcp8565, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8565, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8565, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8565)
  rcp8565brick <- brick(rpft_list)
  rcp8565brick <- projectRaster(rcp8565brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8565LU <- data.frame(raster::extract(rcp8565brick, combcoords))
})
stopImplicitCluster()
All8565LU$ForestTotal65 <- rowSums(All8565LU, na.rm=TRUE)
rm(rcp8565,rcp8565brick)
#2070
system.time({
  rcp8570 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2070.nc")
  lon <- ncvar_get(rcp8570, "longitude")
  lat <- ncvar_get(rcp8570, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8570, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8570, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8570)
  rcp8570brick <- brick(rpft_list)
  rcp8570brick <- projectRaster(rcp8570brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8570LU <- data.frame(raster::extract(rcp8570brick, combcoords))
})
stopImplicitCluster()
All8570LU$ForestTotal70 <- rowSums(All8570LU, na.rm=TRUE)
rm(rcp8570,rcp8570brick)
#2075
system.time({
  rcp8575 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2075.nc")
  lon <- ncvar_get(rcp8575, "longitude")
  lat <- ncvar_get(rcp8575, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8575, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8575, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8575)
  rcp8575brick <- brick(rpft_list)
  rcp8575brick <- projectRaster(rcp8575brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8575LU <- data.frame(raster::extract(rcp8575brick, combcoords))
})
stopImplicitCluster()
All8575LU$ForestTotal75 <- rowSums(All8575LU, na.rm=TRUE)
rm(rcp8575,rcp8575brick)
#2080
system.time({
  rcp8580 <- nc_open("GCAM_Demeter_LU_ssp5_rcp85_modelmean_2080.nc")
  lon <- ncvar_get(rcp8580, "longitude")
  lat <- ncvar_get(rcp8580, "latitude", verbose=FALSE)
  fillvalue <- ncatt_get(rcp8580, "PFT1", "_FillValue")
  pft_names <- paste0("PFT", 1:8)
  rpft_list <- foreach(varname = pft_names) %dopar% {
    processcdf(rcp8580, varname, lon, lat, fillvalue)
  }
  nc_close(rcp8580)
  rcp8580brick <- brick(rpft_list)
  rcp8580brick <- projectRaster(rcp8580brick, crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  All8580LU <- data.frame(raster::extract(rcp8580brick, combcoords))
})
stopImplicitCluster()
All8580LU$ForestTotal80 <- rowSums(All8580LU, na.rm=TRUE)
rm(rcp8580,rcp8580brick)

All85LUTotals <- data.frame(combcoords$PlotCN,All8520LU[,9], All8525LU[,9],
                            All8530LU[,9], All8535LU[,9], All8540LU[,9], All8545LU[,9], All8550LU[,9],
                            All8555LU[,9], All8560LU[,9], All8565LU[,9], All8570LU[,9], All8575LU[,9], All8580LU[,9])
colnames(All85LUTotals) <- c("PlotCN","ForestTotal20","ForestTotal25","ForestTotal30","ForestTotal35","ForestTotal40",
                             "ForestTotal85","ForestTotal50","ForestTotal55","ForestTotal60","ForestTotal65","ForestTotal70","ForestTotal75",
                             "ForestTotal80")
#estimate FAD class, repeat for each time frame, 2020-2080
All85LUTotals$FADClass20 <- ifelse(All85LUTotals$ForestTotal20 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal20 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal20 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal20>=10, 2, 1))))
All85LUTotals$FADClass25 <- ifelse(All85LUTotals$ForestTotal25 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal25 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal25 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal25>=10, 2, 1))))
All85LUTotals$FADClass30 <- ifelse(All85LUTotals$ForestTotal30 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal30 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal30 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal30>=10, 2, 1))))
All85LUTotals$FADClass35 <- ifelse(All85LUTotals$ForestTotal35 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal35 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal35 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal35>=10, 2, 1))))
All85LUTotals$FADClass40 <- ifelse(All85LUTotals$ForestTotal40 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal40 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal40 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal40>=10, 2, 1))))
All85LUTotals$FADClass85 <- ifelse(All85LUTotals$ForestTotal85 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal85 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal85 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal85>=10, 2, 1))))
All85LUTotals$FADClass50 <- ifelse(All85LUTotals$ForestTotal50 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal50 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal50 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal50>=10, 2, 1))))
All85LUTotals$FADClass55 <- ifelse(All85LUTotals$ForestTotal55 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal55 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal55 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal55>=10, 2, 1))))
All85LUTotals$FADClass60 <- ifelse(All85LUTotals$ForestTotal60 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal60 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal60 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal60>=10, 2, 1))))
All85LUTotals$FADClass65 <- ifelse(All85LUTotals$ForestTotal65 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal65 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal65 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal65>=10, 2, 1))))
All85LUTotals$FADClass70 <- ifelse(All85LUTotals$ForestTotal70 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal70 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal70 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal70>=10, 2, 1))))
All85LUTotals$FADClass75 <- ifelse(All85LUTotals$ForestTotal75 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal75 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal75 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal75>=10, 2, 1))))
All85LUTotals$FADClass80 <- ifelse(All85LUTotals$ForestTotal80 >=90, 5,
                                   ifelse(All85LUTotals$ForestTotal80 >=60, 4,
                                          ifelse(All85LUTotals$ForestTotal80 >=40, 3,
                                                 ifelse(All85LUTotals$ForestTotal80>=10, 2, 1))))


#save files
setwd("~/Desktop/FIA/")
write.csv(All26LUTotals, file="RCP26FAD.csv")
write.csv(All45LUTotals, file="RCP45FAD.csv")
write.csv(All85LUTotals, file="RCP85FAD.csv")

