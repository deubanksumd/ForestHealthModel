#Model Prediction Graphs
#set working directory
setwd("~/Desktop/FIAModelCode/")
#read in all necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(terra)
library(sf)
library(ggspatial)
library(purrr)
library(patchwork)

#read in data
allFIA <- read.csv("allFIAfinal.csv")
allFIA <- allFIA[-c(1)]
allFIA[allFIA == 0] <- 0.001
SQIdata <- read.csv("allSQIfinal.csv")
SQIdata <- SQIdata[-c(1)]
#create forest type groups
Fgroups <- c(100, 120, 140, 150, 160, 170, 180, 200, 220, 240, 260, 280, 300, 320, 
             340, 360, 370, 380, 390, 400, 500, 600, 700, 800, 900, 910, 920, 940, 
             960, 970, 980, 990, 999, Inf)
Flabels <- seq_along(Fgroups[-1])
allFIA <- allFIA %>% mutate(ForestGroup = Flabels[findInterval(allFIA$ForestType,Fgroups)])
SQIdata <- SQIdata %>% mutate(ForestGroup = Flabels[findInterval(SQIdata$ForestType,Fgroups)])
#Group repeat plots
SQIdata <- SQIdata %>% group_by(PlotCN) %>% mutate(SQIPCT = mean(SQIPCT)) %>% ungroup() %>% distinct(PlotCN, .keep_all = TRUE)
#Split data then filter remeasured plots
FgroupSplit <- split(allFIA, allFIA$ForestGroup)
F1Re <- FgroupSplit[["1"]]%>%group_by(Latitude)%>%filter(n()>2)
F5Re <- FgroupSplit[["5"]]%>%group_by(Latitude)%>%filter(n()>2)
F20Re <- FgroupSplit[["20"]]%>%group_by(Latitude)%>%filter(n()>2)
F21Re <- FgroupSplit[["21"]]%>%group_by(Latitude)%>%filter(n()>2)
F23Re <- FgroupSplit[["23"]]%>%group_by(Latitude)%>%filter(n()>2)
F24Re <- FgroupSplit[["24"]]%>%group_by(Latitude)%>%filter(n()>2)

#read in predictions
#Carbon
F1CarbPred26 <- read.csv("F1CarbPred26.csv")
F1CarbPred45 <- read.csv("F1CarbPred45.csv")
F1CarbPred85 <- read.csv("F1CarbPred85.csv")
F5CarbPred26 <- read.csv("F5CarbPred26.csv")
F5CarbPred45 <- read.csv("F5CarbPred45.csv")
F5CarbPred85 <- read.csv("F5CarbPred85.csv")
F20CarbPred26 <- read.csv("F20CarbPred26.csv")
F20CarbPred45 <- read.csv("F20CarbPred45.csv")
F20CarbPred85 <- read.csv("F20CarbPred85.csv")
F21CarbPred26 <- read.csv("F21CarbPred26.csv")
F21CarbPred45 <- read.csv("F21CarbPred45.csv")
F21CarbPred85 <- read.csv("F21CarbPred85.csv")
F23CarbPred26 <- read.csv("F23CarbPred26.csv")
F23CarbPred45 <- read.csv("F23CarbPred45.csv")
F23CarbPred85 <- read.csv("F23CarbPred85.csv")
F24CarbPred26 <- read.csv("F24CarbPred26.csv")
F24CarbPred45 <- read.csv("F24CarbPred45.csv")
F24CarbPred85 <- read.csv("F24CarbPred85.csv")
#remove empty first column
F1CarbPred26 <- F1CarbPred26[-c(1)]
F1CarbPred45 <- F1CarbPred45[-c(1)]
F1CarbPred85 <- F1CarbPred85[-c(1)]
F5CarbPred26 <- F5CarbPred26[-c(1)]
F5CarbPred45 <- F5CarbPred45[-c(1)]
F5CarbPred85 <- F5CarbPred85[-c(1)]
F20CarbPred26 <- F20CarbPred26[-c(1)]
F20CarbPred45 <- F20CarbPred45[-c(1)]
F20CarbPred85 <- F20CarbPred85[-c(1)]
F21CarbPred26 <- F21CarbPred26[-c(1)]
F21CarbPred45 <- F21CarbPred45[-c(1)]
F21CarbPred85 <- F21CarbPred85[-c(1)]
F23CarbPred26 <- F23CarbPred26[-c(1)]
F23CarbPred45 <- F23CarbPred45[-c(1)]
F23CarbPred85 <- F23CarbPred85[-c(1)]
F24CarbPred26 <- F24CarbPred26[-c(1)]
F24CarbPred45 <- F24CarbPred45[-c(1)]
F24CarbPred85 <- F24CarbPred85[-c(1)]
#remove any duplicated rows 
F1CarbPred26 <- distinct(F1CarbPred26)
F1CarbPred45 <- distinct(F1CarbPred45)
F1CarbPred85 <- distinct(F1CarbPred85)
F5CarbPred26 <- distinct(F5CarbPred26)
F5CarbPred45 <- distinct(F5CarbPred45)
F5CarbPred85 <- distinct(F5CarbPred85)
F20CarbPred26 <- distinct(F20CarbPred26)
F20CarbPred45 <- distinct(F20CarbPred45)
F20CarbPred85 <- distinct(F20CarbPred85)
F21CarbPred26 <- distinct(F21CarbPred26)
F21CarbPred45 <- distinct(F21CarbPred45)
F21CarbPred85 <- distinct(F21CarbPred85)
F23CarbPred26 <- distinct(F23CarbPred26)
F23CarbPred45 <- distinct(F23CarbPred45)
F23CarbPred85 <- distinct(F23CarbPred85)
F24CarbPred26 <- distinct(F24CarbPred26)
F24CarbPred45 <- distinct(F24CarbPred45)
F24CarbPred85 <- distinct(F24CarbPred85)
#repeat for all models/scenarios
F1HillPred26 <- read.csv("F1HillPred26.csv")
F1HillPred45 <- read.csv("F1HillPred45.csv")
F1HillPred85 <- read.csv("F1HillPred85.csv")
F5HillPred26 <- read.csv("F5HillPred26.csv")
F5HillPred45 <- read.csv("F5HillPred45.csv")
F5HillPred85 <- read.csv("F5HillPred85.csv")
F20HillPred26 <- read.csv("F20HillPred26.csv")
F20HillPred45 <- read.csv("F20HillPred45.csv")
F20HillPred85 <- read.csv("F20HillPred85.csv")
F21HillPred26 <- read.csv("F21HillPred26.csv")
F21HillPred45 <- read.csv("F21HillPred45.csv")
F21HillPred85 <- read.csv("F21HillPred85.csv")
F23HillPred26 <- read.csv("F23HillPred26.csv")
F23HillPred45 <- read.csv("F23HillPred45.csv")
F23HillPred85 <- read.csv("F23HillPred85.csv")
F24HillPred26 <- read.csv("F24HillPred26.csv")
F24HillPred45 <- read.csv("F24HillPred45.csv")
F24HillPred85 <- read.csv("F24HillPred85.csv")
F1HillPred26 <- F1HillPred26[-c(1)]
F1HillPred45 <- F1HillPred45[-c(1)]
F1HillPred85 <- F1HillPred85[-c(1)]
F5HillPred26 <- F5HillPred26[-c(1)]
F5HillPred45 <- F5HillPred45[-c(1)]
F5HillPred85 <- F5HillPred85[-c(1)]
F20HillPred26 <- F20HillPred26[-c(1)]
F20HillPred45 <- F20HillPred45[-c(1)]
F20HillPred85 <- F20HillPred85[-c(1)]
F21HillPred26 <- F21HillPred26[-c(1)]
F21HillPred45 <- F21HillPred45[-c(1)]
F21HillPred85 <- F21HillPred85[-c(1)]
F23HillPred26 <- F23HillPred26[-c(1)]
F23HillPred45 <- F23HillPred45[-c(1)]
F23HillPred85 <- F23HillPred85[-c(1)]
F24HillPred26 <- F24HillPred26[-c(1)]
F24HillPred45 <- F24HillPred45[-c(1)]
F24HillPred85 <- F24HillPred85[-c(1)]
#remove any duplicated rows
F1HillPred26 <- distinct(F1HillPred26)
F1HillPred45 <- distinct(F1HillPred45)
F1HillPred85 <- distinct(F1HillPred85)
F5HillPred26 <- distinct(F5HillPred26)
F5HillPred45 <- distinct(F5HillPred45)
F5HillPred85 <- distinct(F5HillPred85)
F20HillPred26 <- distinct(F20HillPred26)
F20HillPred45 <- distinct(F20HillPred45)
F20HillPred85 <- distinct(F20HillPred85)
F21HillPred26 <- distinct(F21HillPred26)
F21HillPred45 <- distinct(F21HillPred45)
F21HillPred85 <- distinct(F21HillPred85)
F23HillPred26 <- distinct(F23HillPred26)
F23HillPred45 <- distinct(F23HillPred45)
F23HillPred85 <- distinct(F23HillPred85)
F24HillPred26 <- distinct(F24HillPred26)
F24HillPred45 <- distinct(F24HillPred45)
F24HillPred85 <- distinct(F24HillPred85)

F1JaccPred26 <- read.csv("F1JaccPred26b.csv")
F1JaccPred45 <- read.csv("F1JaccPred45b.csv")
F1JaccPred85 <- read.csv("F1JaccPred85b.csv")
F5JaccPred26 <- read.csv("F5JaccPred26b.csv")
F5JaccPred45 <- read.csv("F5JaccPred45b.csv")
F5JaccPred85 <- read.csv("F5JaccPred85b.csv")
F20JaccPred26 <- read.csv("F20JaccPred26b.csv")
F20JaccPred45 <- read.csv("F20JaccPred45b.csv")
F20JaccPred85 <- read.csv("F20JaccPred85b.csv")
F21JaccPred26 <- read.csv("F21JaccPred26b.csv")
F21JaccPred45 <- read.csv("F21JaccPred45b.csv")
F21JaccPred85 <- read.csv("F21JaccPred85b.csv")
F23JaccPred26 <- read.csv("F23JaccPred26b.csv")
F23JaccPred45 <- read.csv("F23JaccPred45b.csv")
F23JaccPred85 <- read.csv("F23JaccPred85b.csv")
F24JaccPred26 <- read.csv("F24JaccPred26b.csv")
F24JaccPred45 <- read.csv("F24JaccPred45b.csv")
F24JaccPred85 <- read.csv("F24JaccPred85b.csv")
F1JaccPred26 <- F1JaccPred26[-c(1)]
F1JaccPred45 <- F1JaccPred45[-c(1)]
F1JaccPred85 <- F1JaccPred85[-c(1)]
F5JaccPred26 <- F5JaccPred26[-c(1)]
F5JaccPred45 <- F5JaccPred45[-c(1)]
F5JaccPred85 <- F5JaccPred85[-c(1)]
F20JaccPred26 <- F20JaccPred26[-c(1)]
F20JaccPred45 <- F20JaccPred45[-c(1)]
F20JaccPred85 <- F20JaccPred85[-c(1)]
F21JaccPred26 <- F21JaccPred26[-c(1)]
F21JaccPred45 <- F21JaccPred45[-c(1)]
F21JaccPred85 <- F21JaccPred85[-c(1)]
F23JaccPred26 <- F23JaccPred26[-c(1)]
F23JaccPred45 <- F23JaccPred45[-c(1)]
F23JaccPred85 <- F23JaccPred85[-c(1)]
F24JaccPred26 <- F24JaccPred26[-c(1)]
F24JaccPred45 <- F24JaccPred45[-c(1)]
F24JaccPred85 <- F24JaccPred85[-c(1)]
#remove any duplicated rows 
F1JaccPred26 <- distinct(F1JaccPred26)
F1JaccPred45 <- distinct(F1JaccPred45)
F1JaccPred85 <- distinct(F1JaccPred85)
F5JaccPred26 <- distinct(F5JaccPred26)
F5JaccPred45 <- distinct(F5JaccPred45)
F5JaccPred85 <- distinct(F5JaccPred85)
F20JaccPred26 <- distinct(F20JaccPred26)
F20JaccPred45 <- distinct(F20JaccPred45)
F20JaccPred85 <- distinct(F20JaccPred85)
F21JaccPred26 <- distinct(F21JaccPred26)
F21JaccPred45 <- distinct(F21JaccPred45)
F21JaccPred85 <- distinct(F21JaccPred85)
F23JaccPred26 <- distinct(F23JaccPred26)
F23JaccPred45 <- distinct(F23JaccPred45)
F23JaccPred85 <- distinct(F23JaccPred85)
F24JaccPred26 <- distinct(F24JaccPred26)
F24JaccPred45 <- distinct(F24JaccPred45)
F24JaccPred85 <- distinct(F24JaccPred85)

F1MPDPred26 <- read.csv("F1MPDPred26.csv")
F1MPDPred45 <- read.csv("F1MPDPred45.csv")
F1MPDPred85 <- read.csv("F1MPDPred85.csv")
F5MPDPred26 <- read.csv("F5MPDPred26.csv")
F5MPDPred45 <- read.csv("F5MPDPred45.csv")
F5MPDPred85 <- read.csv("F5MPDPred85.csv")
F20MPDPred26 <- read.csv("F20MPDPred26.csv")
F20MPDPred45 <- read.csv("F20MPDPred45.csv")
F20MPDPred85 <- read.csv("F20MPDPred85.csv")
F21MPDPred26 <- read.csv("F21MPDPred26.csv")
F21MPDPred45 <- read.csv("F21MPDPred45.csv")
F21MPDPred85 <- read.csv("F21MPDPred85.csv")
F23MPDPred26 <- read.csv("F23MPDPred26.csv")
F23MPDPred45 <- read.csv("F23MPDPred45.csv")
F23MPDPred85 <- read.csv("F23MPDPred85.csv")
F24MPDPred26 <- read.csv("F24MPDPred26.csv")
F24MPDPred45 <- read.csv("F24MPDPred45.csv")
F24MPDPred85 <- read.csv("F24MPDPred85.csv")
F1MPDPred26 <- F1MPDPred26[-c(1)]
F1MPDPred45 <- F1MPDPred45[-c(1)]
F1MPDPred85 <- F1MPDPred85[-c(1)]
F5MPDPred26 <- F5MPDPred26[-c(1)]
F5MPDPred45 <- F5MPDPred45[-c(1)]
F5MPDPred85 <- F5MPDPred85[-c(1)]
F20MPDPred26 <- F20MPDPred26[-c(1)]
F20MPDPred45 <- F20MPDPred45[-c(1)]
F20MPDPred85 <- F20MPDPred85[-c(1)]
F21MPDPred26 <- F21MPDPred26[-c(1)]
F21MPDPred45 <- F21MPDPred45[-c(1)]
F21MPDPred85 <- F21MPDPred85[-c(1)]
F23MPDPred26 <- F23MPDPred26[-c(1)]
F23MPDPred45 <- F23MPDPred45[-c(1)]
F23MPDPred85 <- F23MPDPred85[-c(1)]
F24MPDPred26 <- F24MPDPred26[-c(1)]
F24MPDPred45 <- F24MPDPred45[-c(1)]
F24MPDPred85 <- F24MPDPred85[-c(1)]
#remove any duplicated rows 
F1MPDPred26 <- distinct(F1MPDPred26)
F1MPDPred45 <- distinct(F1MPDPred45)
F1MPDPred85 <- distinct(F1MPDPred85)
F5MPDPred26 <- distinct(F5MPDPred26)
F5MPDPred45 <- distinct(F5MPDPred45)
F5MPDPred85 <- distinct(F5MPDPred85)
F20MPDPred26 <- distinct(F20MPDPred26)
F20MPDPred45 <- distinct(F20MPDPred45)
F20MPDPred85 <- distinct(F20MPDPred85)
F21MPDPred26 <- distinct(F21MPDPred26)
F21MPDPred45 <- distinct(F21MPDPred45)
F21MPDPred85 <- distinct(F21MPDPred85)
F23MPDPred26 <- distinct(F23MPDPred26)
F23MPDPred45 <- distinct(F23MPDPred45)
F23MPDPred85 <- distinct(F23MPDPred85)
F24MPDPred26 <- distinct(F24MPDPred26)
F24MPDPred45 <- distinct(F24MPDPred45)
F24MPDPred85 <- distinct(F24MPDPred85)

F1MortPred26 <- read.csv("F1MortPred26B.csv")
F1MortPred45 <- read.csv("F1MortPred45B.csv")
F1MortPred85 <- read.csv("F1MortPred85B.csv")
F5MortPred26 <- read.csv("F5MortPred26B.csv")
F5MortPred45 <- read.csv("F5MortPred45B.csv")
F5MortPred85 <- read.csv("F5MortPred85B.csv")
F20MortPred26 <- read.csv("F20MortPred26B.csv")
F20MortPred45 <- read.csv("F20MortPred45B.csv")
F20MortPred85 <- read.csv("F20MortPred85B.csv")
F21MortPred26 <- read.csv("F21MortPred26B.csv")
F21MortPred45 <- read.csv("F21MortPred45B.csv")
F21MortPred85 <- read.csv("F21MortPred85B.csv")
F23MortPred26 <- read.csv("F23MortPred26B.csv")
F23MortPred45 <- read.csv("F23MortPred45B.csv")
F23MortPred85 <- read.csv("F23MortPred85B.csv")
F24MortPred26 <- read.csv("F24MortPred26B.csv")
F24MortPred45 <- read.csv("F24MortPred45B.csv")
F24MortPred85 <- read.csv("F24MortPred85B.csv")
F1MortPred26 <- F1MortPred26[-c(1)]
F1MortPred45 <- F1MortPred45[-c(1)]
F1MortPred85 <- F1MortPred85[-c(1)]
F5MortPred26 <- F5MortPred26[-c(1)]
F5MortPred45 <- F5MortPred45[-c(1)]
F5MortPred85 <- F5MortPred85[-c(1)]
F20MortPred26 <- F20MortPred26[-c(1)]
F20MortPred45 <- F20MortPred45[-c(1)]
F20MortPred85 <- F20MortPred85[-c(1)]
F21MortPred26 <- F21MortPred26[-c(1)]
F21MortPred45 <- F21MortPred45[-c(1)]
F21MortPred85 <- F21MortPred85[-c(1)]
F23MortPred26 <- F23MortPred26[-c(1)]
F23MortPred45 <- F23MortPred45[-c(1)]
F23MortPred85 <- F23MortPred85[-c(1)]
F24MortPred26 <- F24MortPred26[-c(1)]
F24MortPred45 <- F24MortPred45[-c(1)]
F24MortPred85 <- F24MortPred85[-c(1)]
#remove any duplicated rows 
F1MortPred26 <- distinct(F1MortPred26)
F1MortPred45 <- distinct(F1MortPred45)
F1MortPred85 <- distinct(F1MortPred85)
F5MortPred26 <- distinct(F5MortPred26)
F5MortPred45 <- distinct(F5MortPred45)
F5MortPred85 <- distinct(F5MortPred85)
F20MortPred26 <- distinct(F20MortPred26)
F20MortPred45 <- distinct(F20MortPred45)
F20MortPred85 <- distinct(F20MortPred85)
F21MortPred26 <- distinct(F21MortPred26)
F21MortPred45 <- distinct(F21MortPred45)
F21MortPred85 <- distinct(F21MortPred85)
F23MortPred26 <- distinct(F23MortPred26)
F23MortPred45 <- distinct(F23MortPred45)
F23MortPred85 <- distinct(F23MortPred85)
F24MortPred26 <- distinct(F24MortPred26)
F24MortPred45 <- distinct(F24MortPred45)
F24MortPred85 <- distinct(F24MortPred85)

F1AQIPred26 <- read.csv("F1AQIPred26T.csv")
F1AQIPred45 <- read.csv("F1AQIPred45T.csv")
F1AQIPred85 <- read.csv("F1AQIPred85T.csv")
F5AQIPred26 <- read.csv("F5AQIPred26T.csv")
F5AQIPred45 <- read.csv("F5AQIPred45T.csv")
F5AQIPred85 <- read.csv("F5AQIPred85T.csv")
F20AQIPred26 <- read.csv("F20AQIPred26T.csv")
F20AQIPred45 <- read.csv("F20AQIPred45T.csv")
F20AQIPred85 <- read.csv("F20AQIPred85T.csv")
F21AQIPred26 <- read.csv("F21AQIPred26T.csv")
F21AQIPred45 <- read.csv("F21AQIPred45T.csv")
F21AQIPred85 <- read.csv("F21AQIPred85T.csv")
F23AQIPred26 <- read.csv("F23AQIPred26T.csv")
F23AQIPred45 <- read.csv("F23AQIPred45T.csv")
F23AQIPred85 <- read.csv("F23AQIPred85T.csv")
F24AQIPred26 <- read.csv("F24AQIPred26T.csv")
F24AQIPred45 <- read.csv("F24AQIPred45T.csv")
F24AQIPred85 <- read.csv("F24AQIPred85T.csv")
F1AQIPred26 <- F1AQIPred26[-c(1)]
F1AQIPred45 <- F1AQIPred45[-c(1)]
F1AQIPred85 <- F1AQIPred85[-c(1)]
F5AQIPred26 <- F5AQIPred26[-c(1)]
F5AQIPred45 <- F5AQIPred45[-c(1)]
F5AQIPred85 <- F5AQIPred85[-c(1)]
F20AQIPred26 <- F20AQIPred26[-c(1)]
F20AQIPred45 <- F20AQIPred45[-c(1)]
F20AQIPred85 <- F20AQIPred85[-c(1)]
F21AQIPred26 <- F21AQIPred26[-c(1)]
F21AQIPred45 <- F21AQIPred45[-c(1)]
F21AQIPred85 <- F21AQIPred85[-c(1)]
F23AQIPred26 <- F23AQIPred26[-c(1)]
F23AQIPred45 <- F23AQIPred45[-c(1)]
F23AQIPred85 <- F23AQIPred85[-c(1)]
F24AQIPred26 <- F24AQIPred26[-c(1)]
F24AQIPred45 <- F24AQIPred45[-c(1)]
F24AQIPred85 <- F24AQIPred85[-c(1)]
#remove any duplicated rows 
F1AQIPred26 <- distinct(F1AQIPred26)
F1AQIPred45 <- distinct(F1AQIPred45)
F1AQIPred85 <- distinct(F1AQIPred85)
F5AQIPred26 <- distinct(F5AQIPred26)
F5AQIPred45 <- distinct(F5AQIPred45)
F5AQIPred85 <- distinct(F5AQIPred85)
F20AQIPred26 <- distinct(F20AQIPred26)
F20AQIPred45 <- distinct(F20AQIPred45)
F20AQIPred85 <- distinct(F20AQIPred85)
F21AQIPred26 <- distinct(F21AQIPred26)
F21AQIPred45 <- distinct(F21AQIPred45)
F21AQIPred85 <- distinct(F21AQIPred85)
F23AQIPred26 <- distinct(F23AQIPred26)
F23AQIPred45 <- distinct(F23AQIPred45)
F23AQIPred85 <- distinct(F23AQIPred85)
F24AQIPred26 <- distinct(F24AQIPred26)
F24AQIPred45 <- distinct(F24AQIPred45)
F24AQIPred85 <- distinct(F24AQIPred85)

SQIPred26 <- read.csv("SQIPred26.csv")
SQIPred45 <- read.csv("SQIPred45.csv")
SQIPred85 <- read.csv("SQIPred85.csv")
SQIPred26 <- SQIPred26[-c(1)]
SQIPred45 <- SQIPred45[-c(1)]
SQIPred85 <- SQIPred85[-c(1)]

#Start with Carbon
#Borrow scale and center numbers from model data
set.seed(123)
train_data <- function(df) {df %>%mutate(split = sample(c(rep("train", floor(0.26 * nrow(.))),rep("test", nrow(.) - floor(0.26 * nrow(.))))))}
FgroupSplitAll <- map(FgroupSplit, ~ tryCatch(train_data(.x)))
FgroupSplitTrain  <- map(FgroupSplitAll, ~ filter(.x, split == "train") %>% dplyr::select(-split))
FgroupSplitTest  <- map(FgroupSplitAll, ~ filter(.x, split == "test") %>% dplyr::select(-split))

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
centerCarb1 <- attr(datCarbSplit1$CarbSum, "scaled:center")
scaleCarb1 <- attr(datCarbSplit1$CarbSum, "scaled:scale")
centerCarb5 <- attr(datCarbSplit5$CarbSum, "scaled:center")
scaleCarb5 <- attr(datCarbSplit5$CarbSum, "scaled:scale")
centerCarb20 <- attr(datCarbSplit20$CarbSum, "scaled:center")
scaleCarb20 <- attr(datCarbSplit20$CarbSum, "scaled:scale")
centerCarb21 <- attr(datCarbSplit21$CarbSum, "scaled:center")
scaleCarb21 <- attr(datCarbSplit21$CarbSum, "scaled:scale")
centerCarb23 <- attr(datCarbSplit23$CarbSum, "scaled:center")
scaleCarb23 <- attr(datCarbSplit23$CarbSum, "scaled:scale")
centerCarb24 <- attr(datCarbSplit24$CarbSum, "scaled:center")
scaleCarb24 <- attr(datCarbSplit24$CarbSum, "scaled:scale")
#Calculate and visualize annual means
#RCP26
#F1 Group
F1MeanCarb <- F1Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F1Q5Carb <- F1Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F1Q5Carb)[2] <- "Q5"
F1Q95Carb <- F1Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F1Q95Carb)[2] <- "Q95"
F1QCarb <- merge(F1Q5Carb,F1Q95Carb, by="Year")
F1MeanCarb <- merge(F1MeanCarb,F1QCarb, by="Year")
names(F1MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F1CarbPred26[,2:37] <- (F1CarbPred26[,2:37]*scaleCarb1)+centerCarb1
FutureCarbF126 <- t(data.frame(apply(F1CarbPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1CarbQ <- map(F1CarbPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1CarbQ <- map_dfr(F1CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1CarbMean26 <- data.frame(FutureCarbF126[,(seq(1,36,3))])
F1CarbMean26$Year <- seq(2025,2080,5)
F1CarbMean26 <- F1CarbMean26[,2:1]
names(F1CarbMean26)[2] <- "MeanCarbPerAcre"
#separate quantiles
F1CarbQ[,1]<-seq(2025,2080,5)
names(F1CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF126 <- merge(F1CarbMean26[1:12,],F1CarbQ[1:12,], by="Year")
CombMeanCarbF126 <- data.frame(rbind(F1MeanCarb,CombMeanCarbF126))
#Graph
highlight <- CombMeanCarbF126[19:30,]
F1Carb26Plot<- ggplot(data = CombMeanCarbF126, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F5 Group
F5MeanCarb <- F5Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F5Q5Carb <- F5Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F5Q5Carb)[2] <- "Q5"
F5Q95Carb <- F5Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F5Q95Carb)[2] <- "Q95"
F5QCarb <- merge(F5Q5Carb,F5Q95Carb, by="Year")
F5MeanCarb <- merge(F5MeanCarb,F5QCarb, by="Year")
names(F5MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F5CarbPred26[,2:37] <- (F5CarbPred26[,2:37]*scaleCarb5)+centerCarb5
FutureCarbF526 <- t(data.frame(apply(F5CarbPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5CarbQ <- map(F5CarbPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5CarbQ <- map_dfr(F5CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5CarbMean26 <- data.frame(FutureCarbF526[,(seq(1,36,3))])
F5CarbMean26$Year <- seq(2025,2080,5)
F5CarbMean26 <- F5CarbMean26[,2:1]
names(F5CarbMean26)[2] <- "MeanCarbPerAcre"
#separate quantiles
F5CarbQ[,1]<-seq(2025,2080,5)
names(F5CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF526 <- merge(F5CarbMean26[1:12,],F5CarbQ[1:12,], by="Year")
CombMeanCarbF526 <- data.frame(rbind(F5MeanCarb,CombMeanCarbF526))
#Graph
highlight <- CombMeanCarbF526[19:30,]
F5Carb26Plot<- ggplot(data = CombMeanCarbF526, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF526, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF526, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F20 Group
F20MeanCarb <- F20Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F20Q5Carb <- F20Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F20Q5Carb)[2] <- "Q5"
F20Q95Carb <- F20Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F20Q95Carb)[2] <- "Q95"
F20QCarb <- merge(F20Q5Carb,F20Q95Carb, by="Year")
F20MeanCarb <- merge(F20MeanCarb,F20QCarb, by="Year")
names(F20MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F20CarbPred26[,2:37] <- (F20CarbPred26[,2:37]*scaleCarb20)+centerCarb20
FutureCarbF2026 <- t(data.frame(apply(F20CarbPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20CarbQ <- map(F20CarbPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20CarbQ <- map_dfr(F20CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20CarbMean26 <- data.frame(FutureCarbF2026[,(seq(1,36,3))])
F20CarbMean26$Year <- seq(2025,2080,5)
F20CarbMean26 <- F20CarbMean26[,2:1]
names(F20CarbMean26)[2] <- "MeanCarbPerAcre"
#separate quantiles
F20CarbQ[,1]<-seq(2025,2080,5)
names(F20CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2026 <- merge(F20CarbMean26[1:12,],F20CarbQ[1:12,], by="Year")
CombMeanCarbF2026 <- data.frame(rbind(F20MeanCarb,CombMeanCarbF2026))
#Graph
highlight <- CombMeanCarbF2026[19:30,]
F20Carb26Plot<- ggplot(data = CombMeanCarbF2026, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2026, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2026, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F21 Group
F21MeanCarb <- F21Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F21Q5Carb <- F21Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F21Q5Carb)[2] <- "Q5"
F21Q95Carb <- F21Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F21Q95Carb)[2] <- "Q95"
F21QCarb <- merge(F21Q5Carb,F21Q95Carb, by="Year")
F21MeanCarb <- merge(F21MeanCarb,F21QCarb, by="Year")
names(F21MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F21CarbPred26[,2:37] <- (F21CarbPred26[,2:37]*scaleCarb21)+centerCarb21
FutureCarbF2126 <- t(data.frame(apply(F21CarbPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21CarbQ <- map(F21CarbPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21CarbQ <- map_dfr(F21CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21CarbMean26 <- data.frame(FutureCarbF2126[,(seq(1,36,3))])
F21CarbMean26$Year <- seq(2025,2080,5)
F21CarbMean26 <- F21CarbMean26[,2:1]
names(F21CarbMean26)[2] <- "MeanCarbPerAcre"
#separate quantiles
F21CarbQ[,1]<-seq(2025,2080,5)
names(F21CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2126 <- merge(F21CarbMean26[1:12,],F21CarbQ[1:12,], by="Year")
CombMeanCarbF2126 <- data.frame(rbind(F21MeanCarb,CombMeanCarbF2126))
#Graph
highlight <- CombMeanCarbF2126[19:30,]
F21Carb26Plot<- ggplot(data = CombMeanCarbF2126, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F23 Group
F23MeanCarb <- F23Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F23Q5Carb <- F23Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F23Q5Carb)[2] <- "Q5"
F23Q95Carb <- F23Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F23Q95Carb)[2] <- "Q95"
F23QCarb <- merge(F23Q5Carb,F23Q95Carb, by="Year")
F23MeanCarb <- merge(F23MeanCarb,F23QCarb, by="Year")
names(F23MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F23CarbPred26[,2:37] <- (F23CarbPred26[,2:37]*scaleCarb23)+centerCarb23
FutureCarbF2326 <- t(data.frame(apply(F23CarbPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23CarbQ <- map(F23CarbPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23CarbQ <- map_dfr(F23CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23CarbMean26 <- data.frame(FutureCarbF2326[,(seq(1,36,3))])
F23CarbMean26$Year <- seq(2025,2080,5)
F23CarbMean26 <- F23CarbMean26[,2:1]
names(F23CarbMean26)[2] <- "MeanCarbPerAcre"
#separate quantiles
F23CarbQ[,1]<-seq(2025,2080,5)
names(F23CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2326 <- merge(F23CarbMean26[1:12,],F23CarbQ[1:12,], by="Year")
CombMeanCarbF2326 <- data.frame(rbind(F23MeanCarb,CombMeanCarbF2326))
#Graph
highlight <- CombMeanCarbF2326[19:30,]
F23Carb26Plot<- ggplot(data = CombMeanCarbF2326, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2326, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2326, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F24 Group
F24MeanCarb <- F24Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F24Q5Carb <- F24Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F24Q5Carb)[2] <- "Q5"
F24Q95Carb <- F24Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F24Q95Carb)[2] <- "Q95"
F24QCarb <- merge(F24Q5Carb,F24Q95Carb, by="Year")
F24MeanCarb <- merge(F24MeanCarb,F24QCarb, by="Year")
names(F24MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F24CarbPred26[,2:37] <- (F24CarbPred26[,2:37]*scaleCarb24)+centerCarb24
FutureCarbF2426 <- t(data.frame(apply(F24CarbPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24CarbQ <- map(F24CarbPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24CarbQ <- map_dfr(F24CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24CarbMean26 <- data.frame(FutureCarbF2426[,(seq(1,36,3))])
F24CarbMean26$Year <- seq(2025,2080,5)
F24CarbMean26 <- F24CarbMean26[,2:1]
names(F24CarbMean26)[2] <- "MeanCarbPerAcre"
#separate quantiles
F24CarbQ[,1]<-seq(2025,2080,5)
names(F24CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2426 <- merge(F24CarbMean26[1:12,],F24CarbQ[1:12,], by="Year")
CombMeanCarbF2426 <- data.frame(rbind(F24MeanCarb,CombMeanCarbF2426))
#Graph
highlight <- CombMeanCarbF2426[19:30,]
F24Carb26Plot<- ggplot(data = CombMeanCarbF2426, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2426, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2426, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F1 Group RCP45
#F1 Group
F1MeanCarb <- F1Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F1Q5Carb <- F1Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F1Q5Carb)[2] <- "Q5"
F1Q95Carb <- F1Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F1Q95Carb)[2] <- "Q95"
F1QCarb <- merge(F1Q5Carb,F1Q95Carb, by="Year")
F1MeanCarb <- merge(F1MeanCarb,F1QCarb, by="Year")
names(F1MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F1CarbPred45[,2:37] <- (F1CarbPred45[,2:37]*scaleCarb1)+centerCarb1
FutureCarbF145 <- t(data.frame(apply(F1CarbPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1CarbQ <- map(F1CarbPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1CarbQ <- map_dfr(F1CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1CarbMean45 <- data.frame(FutureCarbF145[,(seq(1,36,3))])
F1CarbMean45$Year <- seq(2025,2080,5)
F1CarbMean45 <- F1CarbMean45[,2:1]
names(F1CarbMean45)[2] <- "MeanCarbPerAcre"
#separate quantiles
F1CarbQ[,1]<-seq(2025,2080,5)
names(F1CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF145 <- merge(F1CarbMean45[1:12,],F1CarbQ[1:12,], by="Year")
CombMeanCarbF145 <- data.frame(rbind(F1MeanCarb,CombMeanCarbF145))
#Graph
highlight <- CombMeanCarbF145[19:30,]
F1Carb45Plot<- ggplot(data = CombMeanCarbF145, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F5 Group
F5MeanCarb <- F5Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F5Q5Carb <- F5Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F5Q5Carb)[2] <- "Q5"
F5Q95Carb <- F5Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F5Q95Carb)[2] <- "Q95"
F5QCarb <- merge(F5Q5Carb,F5Q95Carb, by="Year")
F5MeanCarb <- merge(F5MeanCarb,F5QCarb, by="Year")
names(F5MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F5CarbPred45[,2:37] <- (F5CarbPred45[,2:37]*scaleCarb5)+centerCarb5
FutureCarbF545 <- t(data.frame(apply(F5CarbPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5CarbQ <- map(F5CarbPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5CarbQ <- map_dfr(F5CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5CarbMean45 <- data.frame(FutureCarbF545[,(seq(1,36,3))])
F5CarbMean45$Year <- seq(2025,2080,5)
F5CarbMean45 <- F5CarbMean45[,2:1]
names(F5CarbMean45)[2] <- "MeanCarbPerAcre"
#separate quantiles
F5CarbQ[,1]<-seq(2025,2080,5)
names(F5CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF545 <- merge(F5CarbMean45[1:12,],F5CarbQ[1:12,], by="Year")
CombMeanCarbF545 <- data.frame(rbind(F5MeanCarb,CombMeanCarbF545))
#Graph
highlight <- CombMeanCarbF545[19:30,]
F5Carb45Plot<- ggplot(data = CombMeanCarbF545, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF545, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF545, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F20 Group
F20MeanCarb <- F20Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F20Q5Carb <- F20Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F20Q5Carb)[2] <- "Q5"
F20Q95Carb <- F20Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F20Q95Carb)[2] <- "Q95"
F20QCarb <- merge(F20Q5Carb,F20Q95Carb, by="Year")
F20MeanCarb <- merge(F20MeanCarb,F20QCarb, by="Year")
names(F20MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F20CarbPred45[,2:37] <- (F20CarbPred45[,2:37]*scaleCarb20)+centerCarb20
FutureCarbF2045 <- t(data.frame(apply(F20CarbPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20CarbQ <- map(F20CarbPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20CarbQ <- map_dfr(F20CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20CarbMean45 <- data.frame(FutureCarbF2045[,(seq(1,36,3))])
F20CarbMean45$Year <- seq(2025,2080,5)
F20CarbMean45 <- F20CarbMean45[,2:1]
names(F20CarbMean45)[2] <- "MeanCarbPerAcre"
#separate quantiles
F20CarbQ[,1]<-seq(2025,2080,5)
names(F20CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2045 <- merge(F20CarbMean45[1:12,],F20CarbQ[1:12,], by="Year")
CombMeanCarbF2045 <- data.frame(rbind(F20MeanCarb,CombMeanCarbF2045))
#Graph
highlight <- CombMeanCarbF2045[19:30,]
F20Carb45Plot<- ggplot(data = CombMeanCarbF2045, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2045, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2045, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F21 Group
F21MeanCarb <- F21Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F21Q5Carb <- F21Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F21Q5Carb)[2] <- "Q5"
F21Q95Carb <- F21Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F21Q95Carb)[2] <- "Q95"
F21QCarb <- merge(F21Q5Carb,F21Q95Carb, by="Year")
F21MeanCarb <- merge(F21MeanCarb,F21QCarb, by="Year")
names(F21MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F21CarbPred45[,2:37] <- (F21CarbPred45[,2:37]*scaleCarb21)+centerCarb21
FutureCarbF2145 <- t(data.frame(apply(F21CarbPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21CarbQ <- map(F21CarbPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21CarbQ <- map_dfr(F21CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21CarbMean45 <- data.frame(FutureCarbF2145[,(seq(1,36,3))])
F21CarbMean45$Year <- seq(2025,2080,5)
F21CarbMean45 <- F21CarbMean45[,2:1]
names(F21CarbMean45)[2] <- "MeanCarbPerAcre"
#separate quantiles
F21CarbQ[,1]<-seq(2025,2080,5)
names(F21CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2145 <- merge(F21CarbMean45[1:12,],F21CarbQ[1:12,], by="Year")
CombMeanCarbF2145 <- data.frame(rbind(F21MeanCarb,CombMeanCarbF2145))
#Graph
highlight <- CombMeanCarbF2145[19:30,]
F21Carb45Plot<- ggplot(data = CombMeanCarbF2145, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F23 Group
F23MeanCarb <- F23Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F23Q5Carb <- F23Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F23Q5Carb)[2] <- "Q5"
F23Q95Carb <- F23Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F23Q95Carb)[2] <- "Q95"
F23QCarb <- merge(F23Q5Carb,F23Q95Carb, by="Year")
F23MeanCarb <- merge(F23MeanCarb,F23QCarb, by="Year")
names(F23MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F23CarbPred45[,2:37] <- (F23CarbPred45[,2:37]*scaleCarb23)+centerCarb23
FutureCarbF2345 <- t(data.frame(apply(F23CarbPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23CarbQ <- map(F23CarbPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23CarbQ <- map_dfr(F23CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23CarbMean45 <- data.frame(FutureCarbF2345[,(seq(1,36,3))])
F23CarbMean45$Year <- seq(2025,2080,5)
F23CarbMean45 <- F23CarbMean45[,2:1]
names(F23CarbMean45)[2] <- "MeanCarbPerAcre"
#separate quantiles
F23CarbQ[,1]<-seq(2025,2080,5)
names(F23CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2345 <- merge(F23CarbMean45[1:12,],F23CarbQ[1:12,], by="Year")
CombMeanCarbF2345 <- data.frame(rbind(F23MeanCarb,CombMeanCarbF2345))
#Graph
highlight <- CombMeanCarbF2345[19:30,]
F23Carb45Plot<- ggplot(data = CombMeanCarbF2345, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2345, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2345, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F24 Group
F24MeanCarb <- F24Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F24Q5Carb <- F24Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F24Q5Carb)[2] <- "Q5"
F24Q95Carb <- F24Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F24Q95Carb)[2] <- "Q95"
F24QCarb <- merge(F24Q5Carb,F24Q95Carb, by="Year")
F24MeanCarb <- merge(F24MeanCarb,F24QCarb, by="Year")
names(F24MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F24CarbPred45[,2:37] <- (F24CarbPred45[,2:37]*scaleCarb24)+centerCarb24
FutureCarbF2445 <- t(data.frame(apply(F24CarbPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24CarbQ <- map(F24CarbPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24CarbQ <- map_dfr(F24CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24CarbMean45 <- data.frame(FutureCarbF2445[,(seq(1,36,3))])
F24CarbMean45$Year <- seq(2025,2080,5)
F24CarbMean45 <- F24CarbMean45[,2:1]
names(F24CarbMean45)[2] <- "MeanCarbPerAcre"
#separate quantiles
F24CarbQ[,1]<-seq(2025,2080,5)
names(F24CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2445 <- merge(F24CarbMean45[1:12,],F24CarbQ[1:12,], by="Year")
CombMeanCarbF2445 <- data.frame(rbind(F24MeanCarb,CombMeanCarbF2445))
#Graph
highlight <- CombMeanCarbF2445[19:30,]
F24Carb45Plot<- ggplot(data = CombMeanCarbF2445, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2445, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2445, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#RCP85
#F1 Group
F1MeanCarb <- F1Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F1Q5Carb <- F1Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F1Q5Carb)[2] <- "Q5"
F1Q95Carb <- F1Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F1Q95Carb)[2] <- "Q95"
F1QCarb <- merge(F1Q5Carb,F1Q95Carb, by="Year")
F1MeanCarb <- merge(F1MeanCarb,F1QCarb, by="Year")
names(F1MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F1CarbPred85[,2:37] <- (F1CarbPred85[,2:37]*scaleCarb1)+centerCarb1
FutureCarbF185 <- t(data.frame(apply(F1CarbPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1CarbQ <- map(F1CarbPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1CarbQ <- map_dfr(F1CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1CarbMean85 <- data.frame(FutureCarbF185[,(seq(1,36,3))])
F1CarbMean85$Year <- seq(2025,2080,5)
F1CarbMean85 <- F1CarbMean85[,2:1]
names(F1CarbMean85)[2] <- "MeanCarbPerAcre"
#separate quantiles
F1CarbQ[,1]<-seq(2025,2080,5)
names(F1CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF185 <- merge(F1CarbMean85[1:12,],F1CarbQ[1:12,], by="Year")
CombMeanCarbF185 <- data.frame(rbind(F1MeanCarb,CombMeanCarbF185))
#Graph
highlight <- CombMeanCarbF185[19:30,]
F1Carb85Plot<- ggplot(data = CombMeanCarbF185, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F5 Group
F5MeanCarb <- F5Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F5Q5Carb <- F5Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F5Q5Carb)[2] <- "Q5"
F5Q95Carb <- F5Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F5Q95Carb)[2] <- "Q95"
F5QCarb <- merge(F5Q5Carb,F5Q95Carb, by="Year")
F5MeanCarb <- merge(F5MeanCarb,F5QCarb, by="Year")
names(F5MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F5CarbPred85[,2:37] <- (F5CarbPred85[,2:37]*scaleCarb5)+centerCarb5
FutureCarbF585 <- t(data.frame(apply(F5CarbPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5CarbQ <- map(F5CarbPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5CarbQ <- map_dfr(F5CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5CarbMean85 <- data.frame(FutureCarbF585[,(seq(1,36,3))])
F5CarbMean85$Year <- seq(2025,2080,5)
F5CarbMean85 <- F5CarbMean85[,2:1]
names(F5CarbMean85)[2] <- "MeanCarbPerAcre"
#separate quantiles
F5CarbQ[,1]<-seq(2025,2080,5)
names(F5CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF585 <- merge(F5CarbMean85[1:12,],F5CarbQ[1:12,], by="Year")
CombMeanCarbF585 <- data.frame(rbind(F5MeanCarb,CombMeanCarbF585))
#Graph
highlight <- CombMeanCarbF585[19:30,]
F5Carb85Plot<- ggplot(data = CombMeanCarbF585, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF585, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF585, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F20 Group
F20MeanCarb <- F20Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F20Q5Carb <- F20Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F20Q5Carb)[2] <- "Q5"
F20Q95Carb <- F20Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F20Q95Carb)[2] <- "Q95"
F20QCarb <- merge(F20Q5Carb,F20Q95Carb, by="Year")
F20MeanCarb <- merge(F20MeanCarb,F20QCarb, by="Year")
names(F20MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F20CarbPred85[,2:37] <- (F20CarbPred85[,2:37]*scaleCarb20)+centerCarb20
FutureCarbF2085 <- t(data.frame(apply(F20CarbPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20CarbQ <- map(F20CarbPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20CarbQ <- map_dfr(F20CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20CarbMean85 <- data.frame(FutureCarbF2085[,(seq(1,36,3))])
F20CarbMean85$Year <- seq(2025,2080,5)
F20CarbMean85 <- F20CarbMean85[,2:1]
names(F20CarbMean85)[2] <- "MeanCarbPerAcre"
#separate quantiles
F20CarbQ[,1]<-seq(2025,2080,5)
names(F20CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2085 <- merge(F20CarbMean85[1:12,],F20CarbQ[1:12,], by="Year")
CombMeanCarbF2085 <- data.frame(rbind(F20MeanCarb,CombMeanCarbF2085))
#Graph
highlight <- CombMeanCarbF2085[19:30,]
F20Carb85Plot<- ggplot(data = CombMeanCarbF2085, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2085, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2085, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F21 Group
F21MeanCarb <- F21Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F21Q5Carb <- F21Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F21Q5Carb)[2] <- "Q5"
F21Q95Carb <- F21Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F21Q95Carb)[2] <- "Q95"
F21QCarb <- merge(F21Q5Carb,F21Q95Carb, by="Year")
F21MeanCarb <- merge(F21MeanCarb,F21QCarb, by="Year")
names(F21MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F21CarbPred85[,2:37] <- (F21CarbPred85[,2:37]*scaleCarb21)+centerCarb21
FutureCarbF2185 <- t(data.frame(apply(F21CarbPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21CarbQ <- map(F21CarbPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21CarbQ <- map_dfr(F21CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21CarbMean85 <- data.frame(FutureCarbF2185[,(seq(1,36,3))])
F21CarbMean85$Year <- seq(2025,2080,5)
F21CarbMean85 <- F21CarbMean85[,2:1]
names(F21CarbMean85)[2] <- "MeanCarbPerAcre"
#separate quantiles
F21CarbQ[,1]<-seq(2025,2080,5)
names(F21CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2185 <- merge(F21CarbMean85[1:12,],F21CarbQ[1:12,], by="Year")
CombMeanCarbF2185 <- data.frame(rbind(F21MeanCarb,CombMeanCarbF2185))
#Graph
highlight <- CombMeanCarbF2185[19:30,]
F21Carb85Plot<- ggplot(data = CombMeanCarbF2185, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F23 Group
F23MeanCarb <- F23Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F23Q5Carb <- F23Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F23Q5Carb)[2] <- "Q5"
F23Q95Carb <- F23Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F23Q95Carb)[2] <- "Q95"
F23QCarb <- merge(F23Q5Carb,F23Q95Carb, by="Year")
F23MeanCarb <- merge(F23MeanCarb,F23QCarb, by="Year")
names(F23MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F23CarbPred85[,2:37] <- (F23CarbPred85[,2:37]*scaleCarb23)+centerCarb23
FutureCarbF2385 <- t(data.frame(apply(F23CarbPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23CarbQ <- map(F23CarbPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23CarbQ <- map_dfr(F23CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23CarbMean85 <- data.frame(FutureCarbF2385[,(seq(1,36,3))])
F23CarbMean85$Year <- seq(2025,2080,5)
F23CarbMean85 <- F23CarbMean85[,2:1]
names(F23CarbMean85)[2] <- "MeanCarbPerAcre"
#separate quantiles
F23CarbQ[,1]<-seq(2025,2080,5)
names(F23CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2385 <- merge(F23CarbMean85[1:12,],F23CarbQ[1:12,], by="Year")
CombMeanCarbF2385 <- data.frame(rbind(F23MeanCarb,CombMeanCarbF2385))
#Graph
highlight <- CombMeanCarbF2385[19:30,]
F23Carb85Plot<- ggplot(data = CombMeanCarbF2385, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2385, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2385, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F24 Group
F24MeanCarb <- F24Re %>% group_by(Year)%>%summarise(mean(CarbonPerAcre))
F24Q5Carb <- F24Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.05))
names(F24Q5Carb)[2] <- "Q5"
F24Q95Carb <- F24Re %>% group_by(Year)%>%reframe(quantile(CarbonPerAcre, prob=0.95))                                                                                               
names(F24Q95Carb)[2] <- "Q95"
F24QCarb <- merge(F24Q5Carb,F24Q95Carb, by="Year")
F24MeanCarb <- merge(F24MeanCarb,F24QCarb, by="Year")
names(F24MeanCarb)<- c("Year","MeanCarbPerAcre","Q5","Q95")
#future values
#unscale future values
F24CarbPred85[,2:37] <- (F24CarbPred85[,2:37]*scaleCarb24)+centerCarb24
FutureCarbF2485 <- t(data.frame(apply(F24CarbPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24CarbQ <- map(F24CarbPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24CarbQ <- map_dfr(F24CarbQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24CarbMean85 <- data.frame(FutureCarbF2485[,(seq(1,36,3))])
F24CarbMean85$Year <- seq(2025,2080,5)
F24CarbMean85 <- F24CarbMean85[,2:1]
names(F24CarbMean85)[2] <- "MeanCarbPerAcre"
#separate quantiles
F24CarbQ[,1]<-seq(2025,2080,5)
names(F24CarbQ)<-c("Year","Q5","Q95")
#combine data
CombMeanCarbF2485 <- merge(F24CarbMean85[1:12,],F24CarbQ[1:12,], by="Year")
CombMeanCarbF2485 <- data.frame(rbind(F24MeanCarb,CombMeanCarbF2485))
#Graph
highlight <- CombMeanCarbF2485[19:30,]
F24Carb85Plot<- ggplot(data = CombMeanCarbF2485, aes(x=Year,y=MeanCarbPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Carbon Storage(Tonnes Per Acre)") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanCarbF2485, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanCarbF2485, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanCarbPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

F1CombCarb <- F1Carb26Plot/F1Carb45Plot/F1Carb85Plot
F1CombCarb + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F5CombCarb <- F5Carb26Plot/F5Carb45Plot/F5Carb85Plot
F5CombCarb + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F20CombCarb <- F20Carb26Plot/F20Carb45Plot/F20Carb85Plot
F20CombCarb + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F21CombCarb <- F21Carb26Plot/F21Carb45Plot/F21Carb85Plot
F21CombCarb + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F23CombCarb <- F23Carb26Plot/F23Carb45Plot/F23Carb85Plot
F23CombCarb + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F24CombCarb <- F24Carb26Plot/F24Carb45Plot/F24Carb85Plot
F24CombCarb + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))


#Hill Shannon
#dont need to scale
#Calculate and visualize annual means
#F1 Group RCP26
F1MeanHill <- F1Re %>% group_by(Year)%>%summarise(mean(HillShannonIndex))
F1Q5Hill <- F1Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.05))
names(F1Q5Hill)[2] <- "Q5"
F1Q95Hill <- F1Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.95))                                                                                               
names(F1Q95Hill)[2] <- "Q95"
F1QHill <- merge(F1Q5Hill,F1Q95Hill, by="Year")
F1MeanHill <- merge(F1MeanHill,F1QHill, by="Year")
names(F1MeanHill)<- c("Year","HillShannonIndex","Q5","Q95")
FutureHillF126 <- t(data.frame(apply(F1HillPred26B[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1HillQ <- map(F1HillPred26B[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1HillQ <- map_dfr(F1HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1HillMean26 <- data.frame(FutureHillF126[,(seq(1,36,3))])
F1HillMean26$Year <- seq(2025,2080,5)
F1HillMean26 <- F1HillMean26[,2:1]
names(F1HillMean26)[2] <- "HillShannonIndex"
#separate quantiles
F1HillQ[,1]<-seq(2025,2080,5)
names(F1HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF126 <- merge(F1HillMean26[1:12,],F1HillQ[1:12,], by="Year")
CombMeanHillF126 <- data.frame(rbind(F1MeanHill,CombMeanHillF126))
#Graph
highlight <- CombMeanHillF126[19:30,]
F1Hill26Plot<- ggplot(data = CombMeanHillF126, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F5 Group
F5MeanHill <- F5Re %>% group_by(Year)%>%summarise(mean(HillShannonIndex))
F5Q5Hill <- F5Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.05))
names(F5Q5Hill)[2] <- "Q5"
F5Q95Hill <- F5Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.95))                                                                                               
names(F5Q95Hill)[2] <- "Q95"
F5QHill <- merge(F5Q5Hill,F5Q95Hill, by="Year")
F5MeanHill <- merge(F5MeanHill,F5QHill, by="Year")
names(F5MeanHill)<- c("Year","HillShannonIndex","Q5","Q95")
FutureHillF526 <- t(data.frame(apply(F5HillPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5HillQ <- map(F5HillPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5HillQ <- map_dfr(F5HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5HillMean26 <- data.frame(FutureHillF526[,(seq(1,36,3))])
F5HillMean26$Year <- seq(2025,2080,5)
F5HillMean26 <- F5HillMean26[,2:1]
names(F5HillMean26)[2] <- "HillShannonIndex"
#separate quantiles
F5HillQ[,1]<-seq(2025,2080,5)
names(F5HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF526 <- merge(F5HillMean26[1:12,],F5HillQ[1:12,], by="Year")
CombMeanHillF526 <- data.frame(rbind(F5MeanHill,CombMeanHillF526))
#Graph
highlight <- CombMeanHillF526[18:29,]
F5Hill26Plot<- ggplot(data = CombMeanHillF526, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF526, row_number() %in% 2:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF526, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F20 Group
F20MeanHill <- F20Re %>% group_by(Year)%>%summarise(mean(HillShannonIndex))
F20Q5Hill <- F20Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.05))
names(F20Q5Hill)[2] <- "Q5"
F20Q95Hill <- F20Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.95))                                                                                               
names(F20Q95Hill)[2] <- "Q95"
F20QHill <- merge(F20Q5Hill,F20Q95Hill, by="Year")
F20MeanHill <- merge(F20MeanHill,F20QHill, by="Year")
names(F20MeanHill)<- c("Year","HillShannonIndex","Q5","Q95")
FutureHillF2026 <- t(data.frame(apply(F20HillPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20HillQ <- map(F20HillPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20HillQ <- map_dfr(F20HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20HillMean26 <- data.frame(FutureHillF2026[,(seq(1,36,3))])
F20HillMean26$Year <- seq(2025,2080,5)
F20HillMean26 <- F20HillMean26[,2:1]
names(F20HillMean26)[2] <- "HillShannonIndex"
#separate quantiles
F20HillQ[,1]<-seq(2025,2080,5)
names(F20HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2026 <- merge(F20HillMean26[1:12,],F20HillQ[1:12,], by="Year")
CombMeanHillF2026 <- data.frame(rbind(F20MeanHill,CombMeanHillF2026))
#Graph
highlight <- CombMeanHillF2026[19:30,]
F20Hill26Plot<- ggplot(data = CombMeanHillF2026, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2026, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2026, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 


#F21 Group
F21MeanHill <- F21Re %>% group_by(Year)%>%summarise(mean(HillShannonIndex))
F21Q5Hill <- F21Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.05))
names(F21Q5Hill)[2] <- "Q5"
F21Q95Hill <- F21Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.95))                                                                                               
names(F21Q95Hill)[2] <- "Q95"
F21QHill <- merge(F21Q5Hill,F21Q95Hill, by="Year")
F21MeanHill <- merge(F21MeanHill,F21QHill, by="Year")
names(F21MeanHill)<- c("Year","HillShannonIndex","Q5","Q95")
FutureHillF2126 <- t(data.frame(apply(F21HillPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21HillQ <- map(F21HillPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21HillQ <- map_dfr(F21HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21HillMean26 <- data.frame(FutureHillF2126[,(seq(1,36,3))])
F21HillMean26$Year <- seq(2025,2080,5)
F21HillMean26 <- F21HillMean26[,2:1]
names(F21HillMean26)[2] <- "HillShannonIndex"
#separate quantiles
F21HillQ[,1]<-seq(2025,2080,5)
names(F21HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2126 <- merge(F21HillMean26[1:12,],F21HillQ[1:12,], by="Year")
CombMeanHillF2126 <- data.frame(rbind(F21MeanHill,CombMeanHillF2126))
#Graph
highlight <- CombMeanHillF2126[19:30,]
F21Hill26Plot<- ggplot(data = CombMeanHillF2126, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F23 Group
F23MeanHill <- F23Re %>% group_by(Year)%>%summarise(mean(HillShannonIndex))
F23Q5Hill <- F23Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.05))
names(F23Q5Hill)[2] <- "Q5"
F23Q95Hill <- F23Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.95))                                                                                               
names(F23Q95Hill)[2] <- "Q95"
F23QHill <- merge(F23Q5Hill,F23Q95Hill, by="Year")
F23MeanHill <- merge(F23MeanHill,F23QHill, by="Year")
names(F23MeanHill)<- c("Year","HillShannonIndex","Q5","Q95")
FutureHillF2326 <- t(data.frame(apply(F23HillPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23HillQ <- map(F23HillPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23HillQ <- map_dfr(F23HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23HillMean26 <- data.frame(FutureHillF2326[,(seq(1,36,3))])
F23HillMean26$Year <- seq(2025,2080,5)
F23HillMean26 <- F23HillMean26[,2:1]
names(F23HillMean26)[2] <- "HillShannonIndex"
#separate quantiles
F23HillQ[,1]<-seq(2025,2080,5)
names(F23HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2326 <- merge(F23HillMean26[1:12,],F23HillQ[1:12,], by="Year")
CombMeanHillF2326 <- data.frame(rbind(F23MeanHill,CombMeanHillF2326))
#Graph
highlight <- CombMeanHillF2326[19:30,]
F23Hill26Plot<- ggplot(data = CombMeanHillF2326, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2326, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2326, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F24 Group
F24MeanHill <- F24Re %>% group_by(Year)%>%summarise(mean(HillShannonIndex))
F24Q5Hill <- F24Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.05))
names(F24Q5Hill)[2] <- "Q5"
F24Q95Hill <- F24Re %>% group_by(Year)%>%reframe(quantile(HillShannonIndex, prob=0.95))                                                                                               
names(F24Q95Hill)[2] <- "Q95"
F24QHill <- merge(F24Q5Hill,F24Q95Hill, by="Year")
F24MeanHill <- merge(F24MeanHill,F24QHill, by="Year")
names(F24MeanHill)<- c("Year","HillShannonIndex","Q5","Q95")
FutureHillF2426 <- t(data.frame(apply(F24HillPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24HillQ <- map(F24HillPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24HillQ <- map_dfr(F24HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24HillMean26 <- data.frame(FutureHillF2426[,(seq(1,36,3))])
F24HillMean26$Year <- seq(2025,2080,5)
F24HillMean26 <- F24HillMean26[,2:1]
names(F24HillMean26)[2] <- "HillShannonIndex"
#separate quantiles
F24HillQ[,1]<-seq(2025,2080,5)
names(F24HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2426 <- merge(F24HillMean26[1:12,],F24HillQ[1:12,], by="Year")
CombMeanHillF2426 <- data.frame(rbind(F24MeanHill,CombMeanHillF2426))
#Graph
highlight <- CombMeanHillF2426[19:30,]
F24Hill26Plot<- ggplot(data = CombMeanHillF2426, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2426, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2426, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F1 Group RCP45
#F1 Group
FutureHillF145 <- t(data.frame(apply(F1HillPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1HillQ <- map(F1HillPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1HillQ <- map_dfr(F1HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1HillMean45 <- data.frame(FutureHillF145[,(seq(1,36,3))])
F1HillMean45$Year <- seq(2025,2080,5)
F1HillMean45 <- F1HillMean45[,2:1]
names(F1HillMean45)[2] <- "HillShannonIndex"
#separate quantiles
F1HillQ[,1]<-seq(2025,2080,5)
names(F1HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF145 <- merge(F1HillMean45[1:12,],F1HillQ[1:12,], by="Year")
CombMeanHillF145 <- data.frame(rbind(F1MeanHill,CombMeanHillF145))
#Graph
highlight <- CombMeanHillF145[19:30,]
F1Hill45Plot<- ggplot(data = CombMeanHillF145, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F5 Group
FutureHillF545 <- t(data.frame(apply(F5HillPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5HillQ <- map(F5HillPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5HillQ <- map_dfr(F5HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5HillMean45 <- data.frame(FutureHillF545[,(seq(1,36,3))])
F5HillMean45$Year <- seq(2025,2080,5)
F5HillMean45 <- F5HillMean45[,2:1]
names(F5HillMean45)[2] <- "HillShannonIndex"
#separate quantiles
F5HillQ[,1]<-seq(2025,2080,5)
names(F5HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF545 <- merge(F5HillMean45[1:12,],F5HillQ[1:12,], by="Year")
CombMeanHillF545 <- data.frame(rbind(F5MeanHill,CombMeanHillF545))
#Graph
highlight <- CombMeanHillF545[18:29,]
F5Hill45Plot<- ggplot(data = CombMeanHillF545, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF545, row_number() %in% 2:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF545, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F20 Group
FutureHillF2045 <- t(data.frame(apply(F20HillPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20HillQ <- map(F20HillPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20HillQ <- map_dfr(F20HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20HillMean45 <- data.frame(FutureHillF2045[,(seq(1,36,3))])
F20HillMean45$Year <- seq(2025,2080,5)
F20HillMean45 <- F20HillMean45[,2:1]
names(F20HillMean45)[2] <- "HillShannonIndex"
#separate quantiles
F20HillQ[,1]<-seq(2025,2080,5)
names(F20HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2045 <- merge(F20HillMean45[1:12,],F20HillQ[1:12,], by="Year")
CombMeanHillF2045 <- data.frame(rbind(F20MeanHill,CombMeanHillF2045))
#Graph
highlight <- CombMeanHillF2045[19:30,]
F20Hill45Plot<- ggplot(data = CombMeanHillF2045, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2045, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2045, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 


#F21 Group
FutureHillF2145 <- t(data.frame(apply(F21HillPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21HillQ <- map(F21HillPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21HillQ <- map_dfr(F21HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21HillMean45 <- data.frame(FutureHillF2145[,(seq(1,36,3))])
F21HillMean45$Year <- seq(2025,2080,5)
F21HillMean45 <- F21HillMean45[,2:1]
names(F21HillMean45)[2] <- "HillShannonIndex"
#separate quantiles
F21HillQ[,1]<-seq(2025,2080,5)
names(F21HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2145 <- merge(F21HillMean45[1:12,],F21HillQ[1:12,], by="Year")
CombMeanHillF2145 <- data.frame(rbind(F21MeanHill,CombMeanHillF2145))
#Graph
highlight <- CombMeanHillF2145[19:30,]
F21Hill45Plot<- ggplot(data = CombMeanHillF2145, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F23 Group
FutureHillF2345 <- t(data.frame(apply(F23HillPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23HillQ <- map(F23HillPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23HillQ <- map_dfr(F23HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23HillMean45 <- data.frame(FutureHillF2345[,(seq(1,36,3))])
F23HillMean45$Year <- seq(2025,2080,5)
F23HillMean45 <- F23HillMean45[,2:1]
names(F23HillMean45)[2] <- "HillShannonIndex"
#separate quantiles
F23HillQ[,1]<-seq(2025,2080,5)
names(F23HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2345 <- merge(F23HillMean45[1:12,],F23HillQ[1:12,], by="Year")
CombMeanHillF2345 <- data.frame(rbind(F23MeanHill,CombMeanHillF2345))
#Graph
highlight <- CombMeanHillF2345[19:30,]
F23Hill45Plot<- ggplot(data = CombMeanHillF2345, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2345, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2345, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F24 Group
FutureHillF2445 <- t(data.frame(apply(F24HillPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24HillQ <- map(F24HillPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24HillQ <- map_dfr(F24HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24HillMean45 <- data.frame(FutureHillF2445[,(seq(1,36,3))])
F24HillMean45$Year <- seq(2025,2080,5)
F24HillMean45 <- F24HillMean45[,2:1]
names(F24HillMean45)[2] <- "HillShannonIndex"
#separate quantiles
F24HillQ[,1]<-seq(2025,2080,5)
names(F24HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2445 <- merge(F24HillMean45[1:12,],F24HillQ[1:12,], by="Year")
CombMeanHillF2445 <- data.frame(rbind(F24MeanHill,CombMeanHillF2445))
#Graph
highlight <- CombMeanHillF2445[19:30,]
F24Hill45Plot<- ggplot(data = CombMeanHillF2445, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2445, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2445, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#RCP85
#F1 Group
FutureHillF185 <- t(data.frame(apply(F1HillPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1HillQ <- map(F1HillPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1HillQ <- map_dfr(F1HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1HillMean85 <- data.frame(FutureHillF185[,(seq(1,36,3))])
F1HillMean85$Year <- seq(2025,2080,5)
F1HillMean85 <- F1HillMean85[,2:1]
names(F1HillMean85)[2] <- "HillShannonIndex"
#separate quantiles
F1HillQ[,1]<-seq(2025,2080,5)
names(F1HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF185 <- merge(F1HillMean85[1:12,],F1HillQ[1:12,], by="Year")
CombMeanHillF185 <- data.frame(rbind(F1MeanHill,CombMeanHillF185))
#Graph
highlight <- CombMeanHillF185[19:30,]
F1Hill85Plot<- ggplot(data = CombMeanHillF185, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F5 Group
FutureHillF585 <- t(data.frame(apply(F5HillPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5HillQ <- map(F5HillPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5HillQ <- map_dfr(F5HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5HillMean85 <- data.frame(FutureHillF585[,(seq(1,36,3))])
F5HillMean85$Year <- seq(2025,2080,5)
F5HillMean85 <- F5HillMean85[,2:1]
names(F5HillMean85)[2] <- "HillShannonIndex"
#separate quantiles
F5HillQ[,1]<-seq(2025,2080,5)
names(F5HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF585 <- merge(F5HillMean85[1:12,],F5HillQ[1:12,], by="Year")
CombMeanHillF585 <- data.frame(rbind(F5MeanHill,CombMeanHillF585))
#Graph
highlight <- CombMeanHillF585[18:29,]
F5Hill85Plot<- ggplot(data = CombMeanHillF585, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF585, row_number() %in% 2:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF585, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F20 Group
FutureHillF2085 <- t(data.frame(apply(F20HillPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20HillQ <- map(F20HillPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20HillQ <- map_dfr(F20HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20HillMean85 <- data.frame(FutureHillF2085[,(seq(1,36,3))])
F20HillMean85$Year <- seq(2025,2080,5)
F20HillMean85 <- F20HillMean85[,2:1]
names(F20HillMean85)[2] <- "HillShannonIndex"
#separate quantiles
F20HillQ[,1]<-seq(2025,2080,5)
names(F20HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2085 <- merge(F20HillMean85[1:12,],F20HillQ[1:12,], by="Year")
CombMeanHillF2085 <- data.frame(rbind(F20MeanHill,CombMeanHillF2085))
#Graph
highlight <- CombMeanHillF2085[19:30,]
F20Hill85Plot<- ggplot(data = CombMeanHillF2085, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2085, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2085, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 


#F21 Group
FutureHillF2185 <- t(data.frame(apply(F21HillPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21HillQ <- map(F21HillPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21HillQ <- map_dfr(F21HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21HillMean85 <- data.frame(FutureHillF2185[,(seq(1,36,3))])
F21HillMean85$Year <- seq(2025,2080,5)
F21HillMean85 <- F21HillMean85[,2:1]
names(F21HillMean85)[2] <- "HillShannonIndex"
#separate quantiles
F21HillQ[,1]<-seq(2025,2080,5)
names(F21HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2185 <- merge(F21HillMean85[1:12,],F21HillQ[1:12,], by="Year")
CombMeanHillF2185 <- data.frame(rbind(F21MeanHill,CombMeanHillF2185))
#Graph
highlight <- CombMeanHillF2185[19:30,]
F21Hill85Plot<- ggplot(data = CombMeanHillF2185, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F23 Group
FutureHillF2385 <- t(data.frame(apply(F23HillPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23HillQ <- map(F23HillPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23HillQ <- map_dfr(F23HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23HillMean85 <- data.frame(FutureHillF2385[,(seq(1,36,3))])
F23HillMean85$Year <- seq(2025,2080,5)
F23HillMean85 <- F23HillMean85[,2:1]
names(F23HillMean85)[2] <- "HillShannonIndex"
#separate quantiles
F23HillQ[,1]<-seq(2025,2080,5)
names(F23HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2385 <- merge(F23HillMean85[1:12,],F23HillQ[1:12,], by="Year")
CombMeanHillF2385 <- data.frame(rbind(F23MeanHill,CombMeanHillF2385))
#Graph
highlight <- CombMeanHillF2385[19:30,]
F23Hill85Plot<- ggplot(data = CombMeanHillF2385, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2385, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2385, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F24 Group
FutureHillF2485 <- t(data.frame(apply(F24HillPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24HillQ <- map(F24HillPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24HillQ <- map_dfr(F24HillQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24HillMean85 <- data.frame(FutureHillF2485[,(seq(1,36,3))])
F24HillMean85$Year <- seq(2025,2080,5)
F24HillMean85 <- F24HillMean85[,2:1]
names(F24HillMean85)[2] <- "HillShannonIndex"
#separate quantiles
F24HillQ[,1]<-seq(2025,2080,5)
names(F24HillQ)<-c("Year","Q5","Q95")
#combine data
CombMeanHillF2485 <- merge(F24HillMean85[1:12,],F24HillQ[1:12,], by="Year")
CombMeanHillF2485 <- data.frame(rbind(F24MeanHill,CombMeanHillF2485))
#Graph
highlight <- CombMeanHillF2485[19:30,]
F24Hill85Plot<- ggplot(data = CombMeanHillF2485, aes(x=Year,y=HillShannonIndex))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Hill Shannon Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanHillF2485, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanHillF2485, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=HillShannonIndex), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#create multiplots
F1CombHill <- F1Hill26Plot/F1Hill45Plot/F1Hill85Plot
F1CombHill + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F5CombHill <- F5Hill26Plot/F5Hill45Plot/F5Hill85Plot
F5CombHill + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F20CombHill <- F20Hill26Plot/F20Hill45Plot/F20Hill85Plot
F20CombHill + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F21CombHill <- F21Hill26Plot/F21Hill45Plot/F21Hill85Plot
F21CombHill + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F23CombHill <- F23Hill26Plot/F23Hill45Plot/F23Hill85Plot
F23CombHill + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F24CombHill <- F24Hill26Plot/F24Hill45Plot/F24Hill85Plot
F24CombHill + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))

#Jaccards
#no need to scale
#Calculate and visualize annual means
#Jaccard’s Similarity Index, don't scale
#F1 Group RCP26
F1MeanJacc <- F1Re %>% group_by(Year)%>%summarise(mean(JaccardMean))
F1Q5Jacc <- F1Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.05))
names(F1Q5Jacc)[2] <- "Q5"
F1Q95Jacc <- F1Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.95))                                                                                               
names(F1Q95Jacc)[2] <- "Q95"
F1QJacc <- merge(F1Q5Jacc,F1Q95Jacc, by="Year")
F1MeanJacc <- merge(F1MeanJacc,F1QJacc, by="Year")
names(F1MeanJacc)<- c("Year","JaccardMean","Q5","Q95")
FutureJaccF126 <- t(data.frame(apply(F1JaccPred26B[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1JaccQ <- map(F1JaccPred26B[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1JaccQ <- map_dfr(F1JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1JaccMean26 <- data.frame(FutureJaccF126[,(seq(1,36,3))])
F1JaccMean26$Year <- seq(2025,2080,5)
F1JaccMean26 <- F1JaccMean26[,2:1]
names(F1JaccMean26)[2] <- "JaccardMean"
#separate quantiles
F1JaccQ[,1]<-seq(2025,2080,5)
names(F1JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF126 <- merge(F1JaccMean26[1:12,],F1JaccQ[1:12,], by="Year")
CombMeanJaccF126 <- data.frame(rbind(F1MeanJacc,CombMeanJaccF126))
#Graph
highlight <- CombMeanJaccF126[19:30,]
F1Jacc26Plot<- ggplot(data = CombMeanJaccF126, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F5 Group
F5MeanJacc <- F5Re %>% group_by(Year)%>%summarise(mean(JaccardMean))
F5Q5Jacc <- F5Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.05))
names(F5Q5Jacc)[2] <- "Q5"
F5Q95Jacc <- F5Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.95))                                                                                               
names(F5Q95Jacc)[2] <- "Q95"
F5QJacc <- merge(F5Q5Jacc,F5Q95Jacc, by="Year")
F5MeanJacc <- merge(F5MeanJacc,F5QJacc, by="Year")
names(F5MeanJacc)<- c("Year","JaccardMean","Q5","Q95")
FutureJaccF526 <- t(data.frame(apply(F5JaccPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5JaccQ <- map(F5JaccPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5JaccQ <- map_dfr(F5JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5JaccMean26 <- data.frame(FutureJaccF526[,(seq(1,36,3))])
F5JaccMean26$Year <- seq(2025,2080,5)
F5JaccMean26 <- F5JaccMean26[,2:1]
names(F5JaccMean26)[2] <- "JaccardMean"
#separate quantiles
F5JaccQ[,1]<-seq(2025,2080,5)
names(F5JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF526 <- merge(F5JaccMean26[1:12,],F5JaccQ[1:12,], by="Year")
CombMeanJaccF526 <- data.frame(rbind(F5MeanJacc,CombMeanJaccF526))
#Graph
highlight <- CombMeanJaccF526[18:29,]
F5Jacc26Plot<- ggplot(data = CombMeanJaccF526, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF526, row_number() %in% 2:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF526, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F20 Group
F20MeanJacc <- F20Re %>% group_by(Year)%>%summarise(mean(JaccardMean))
F20Q5Jacc <- F20Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.05))
names(F20Q5Jacc)[2] <- "Q5"
F20Q95Jacc <- F20Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.95))                                                                                               
names(F20Q95Jacc)[2] <- "Q95"
F20QJacc <- merge(F20Q5Jacc,F20Q95Jacc, by="Year")
F20MeanJacc <- merge(F20MeanJacc,F20QJacc, by="Year")
names(F20MeanJacc)<- c("Year","JaccardMean","Q5","Q95")
FutureJaccF2026 <- t(data.frame(apply(F20JaccPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20JaccQ <- map(F20JaccPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20JaccQ <- map_dfr(F20JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20JaccMean26 <- data.frame(FutureJaccF2026[,(seq(1,36,3))])
F20JaccMean26$Year <- seq(2025,2080,5)
F20JaccMean26 <- F20JaccMean26[,2:1]
names(F20JaccMean26)[2] <- "JaccardMean"
#separate quantiles
F20JaccQ[,1]<-seq(2025,2080,5)
names(F20JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2026 <- merge(F20JaccMean26[1:12,],F20JaccQ[1:12,], by="Year")
CombMeanJaccF2026 <- data.frame(rbind(F20MeanJacc,CombMeanJaccF2026))
#Graph
highlight <- CombMeanJaccF2026[19:30,]
F20Jacc26Plot<- ggplot(data = CombMeanJaccF2026, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2026, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2026, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 


#F21 Group
F21MeanJacc <- F21Re %>% group_by(Year)%>%summarise(mean(JaccardMean))
F21Q5Jacc <- F21Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.05))
names(F21Q5Jacc)[2] <- "Q5"
F21Q95Jacc <- F21Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.95))                                                                                               
names(F21Q95Jacc)[2] <- "Q95"
F21QJacc <- merge(F21Q5Jacc,F21Q95Jacc, by="Year")
F21MeanJacc <- merge(F21MeanJacc,F21QJacc, by="Year")
names(F21MeanJacc)<- c("Year","JaccardMean","Q5","Q95")
FutureJaccF2126 <- t(data.frame(apply(F21JaccPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21JaccQ <- map(F21JaccPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21JaccQ <- map_dfr(F21JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21JaccMean26 <- data.frame(FutureJaccF2126[,(seq(1,36,3))])
F21JaccMean26$Year <- seq(2025,2080,5)
F21JaccMean26 <- F21JaccMean26[,2:1]
names(F21JaccMean26)[2] <- "JaccardMean"
#separate quantiles
F21JaccQ[,1]<-seq(2025,2080,5)
names(F21JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2126 <- merge(F21JaccMean26[1:12,],F21JaccQ[1:12,], by="Year")
CombMeanJaccF2126 <- data.frame(rbind(F21MeanJacc,CombMeanJaccF2126))
#Graph
highlight <- CombMeanJaccF2126[19:30,]
F21Jacc26Plot<- ggplot(data = CombMeanJaccF2126, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F23 Group
F23MeanJacc <- F23Re %>% group_by(Year)%>%summarise(mean(JaccardMean))
F23Q5Jacc <- F23Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.05))
names(F23Q5Jacc)[2] <- "Q5"
F23Q95Jacc <- F23Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.95))                                                                                               
names(F23Q95Jacc)[2] <- "Q95"
F23QJacc <- merge(F23Q5Jacc,F23Q95Jacc, by="Year")
F23MeanJacc <- merge(F23MeanJacc,F23QJacc, by="Year")
names(F23MeanJacc)<- c("Year","JaccardMean","Q5","Q95")
FutureJaccF2326 <- t(data.frame(apply(F23JaccPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23JaccQ <- map(F23JaccPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23JaccQ <- map_dfr(F23JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23JaccMean26 <- data.frame(FutureJaccF2326[,(seq(1,36,3))])
F23JaccMean26$Year <- seq(2025,2080,5)
F23JaccMean26 <- F23JaccMean26[,2:1]
names(F23JaccMean26)[2] <- "JaccardMean"
#separate quantiles
F23JaccQ[,1]<-seq(2025,2080,5)
names(F23JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2326 <- merge(F23JaccMean26[1:12,],F23JaccQ[1:12,], by="Year")
CombMeanJaccF2326 <- data.frame(rbind(F23MeanJacc,CombMeanJaccF2326))
#Graph
highlight <- CombMeanJaccF2326[19:30,]
F23Jacc26Plot<- ggplot(data = CombMeanJaccF2326, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2326, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2326, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F24 Group
F24MeanJacc <- F24Re %>% group_by(Year)%>%summarise(mean(JaccardMean))
F24Q5Jacc <- F24Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.05))
names(F24Q5Jacc)[2] <- "Q5"
F24Q95Jacc <- F24Re %>% group_by(Year)%>%reframe(quantile(JaccardMean, prob=0.95))                                                                                               
names(F24Q95Jacc)[2] <- "Q95"
F24QJacc <- merge(F24Q5Jacc,F24Q95Jacc, by="Year")
F24MeanJacc <- merge(F24MeanJacc,F24QJacc, by="Year")
names(F24MeanJacc)<- c("Year","JaccardMean","Q5","Q95")
FutureJaccF2426 <- t(data.frame(apply(F24JaccPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24JaccQ <- map(F24JaccPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24JaccQ <- map_dfr(F24JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24JaccMean26 <- data.frame(FutureJaccF2426[,(seq(1,36,3))])
F24JaccMean26$Year <- seq(2025,2080,5)
F24JaccMean26 <- F24JaccMean26[,2:1]
names(F24JaccMean26)[2] <- "JaccardMean"
#separate quantiles
F24JaccQ[,1]<-seq(2025,2080,5)
names(F24JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2426 <- merge(F24JaccMean26[1:12,],F24JaccQ[1:12,], by="Year")
CombMeanJaccF2426 <- data.frame(rbind(F24MeanJacc,CombMeanJaccF2426))
#Graph
highlight <- CombMeanJaccF2426[19:30,]
F24Jacc26Plot<- ggplot(data = CombMeanJaccF2426, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2426, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2426, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F1 Group RCP45
#F1 Group
FutureJaccF145 <- t(data.frame(apply(F1JaccPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1JaccQ <- map(F1JaccPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1JaccQ <- map_dfr(F1JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1JaccMean45 <- data.frame(FutureJaccF145[,(seq(1,36,3))])
F1JaccMean45$Year <- seq(2025,2080,5)
F1JaccMean45 <- F1JaccMean45[,2:1]
names(F1JaccMean45)[2] <- "JaccardMean"
#separate quantiles
F1JaccQ[,1]<-seq(2025,2080,5)
names(F1JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF145 <- merge(F1JaccMean45[1:12,],F1JaccQ[1:12,], by="Year")
CombMeanJaccF145 <- data.frame(rbind(F1MeanJacc,CombMeanJaccF145))
#Graph
highlight <- CombMeanJaccF145[19:30,]
F1Jacc45Plot<- ggplot(data = CombMeanJaccF145, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F5 Group
FutureJaccF545 <- t(data.frame(apply(F5JaccPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5JaccQ <- map(F5JaccPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5JaccQ <- map_dfr(F5JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5JaccMean45 <- data.frame(FutureJaccF545[,(seq(1,36,3))])
F5JaccMean45$Year <- seq(2025,2080,5)
F5JaccMean45 <- F5JaccMean45[,2:1]
names(F5JaccMean45)[2] <- "JaccardMean"
#separate quantiles
F5JaccQ[,1]<-seq(2025,2080,5)
names(F5JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF545 <- merge(F5JaccMean45[1:12,],F5JaccQ[1:12,], by="Year")
CombMeanJaccF545 <- data.frame(rbind(F5MeanJacc,CombMeanJaccF545))
#Graph
highlight <- CombMeanJaccF545[18:29,]
F5Jacc45Plot<- ggplot(data = CombMeanJaccF545, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF545, row_number() %in% 2:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF545, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F20 Group
FutureJaccF2045 <- t(data.frame(apply(F20JaccPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20JaccQ <- map(F20JaccPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20JaccQ <- map_dfr(F20JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20JaccMean45 <- data.frame(FutureJaccF2045[,(seq(1,36,3))])
F20JaccMean45$Year <- seq(2025,2080,5)
F20JaccMean45 <- F20JaccMean45[,2:1]
names(F20JaccMean45)[2] <- "JaccardMean"
#separate quantiles
F20JaccQ[,1]<-seq(2025,2080,5)
names(F20JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2045 <- merge(F20JaccMean45[1:12,],F20JaccQ[1:12,], by="Year")
CombMeanJaccF2045 <- data.frame(rbind(F20MeanJacc,CombMeanJaccF2045))
#Graph
highlight <- CombMeanJaccF2045[19:30,]
F20Jacc45Plot<- ggplot(data = CombMeanJaccF2045, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2045, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2045, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 


#F21 Group
FutureJaccF2145 <- t(data.frame(apply(F21JaccPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21JaccQ <- map(F21JaccPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21JaccQ <- map_dfr(F21JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21JaccMean45 <- data.frame(FutureJaccF2145[,(seq(1,36,3))])
F21JaccMean45$Year <- seq(2025,2080,5)
F21JaccMean45 <- F21JaccMean45[,2:1]
names(F21JaccMean45)[2] <- "JaccardMean"
#separate quantiles
F21JaccQ[,1]<-seq(2025,2080,5)
names(F21JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2145 <- merge(F21JaccMean45[1:12,],F21JaccQ[1:12,], by="Year")
CombMeanJaccF2145 <- data.frame(rbind(F21MeanJacc,CombMeanJaccF2145))
#Graph
highlight <- CombMeanJaccF2145[19:30,]
F21Jacc45Plot<- ggplot(data = CombMeanJaccF2145, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F23 Group
FutureJaccF2345 <- t(data.frame(apply(F23JaccPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23JaccQ <- map(F23JaccPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23JaccQ <- map_dfr(F23JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23JaccMean45 <- data.frame(FutureJaccF2345[,(seq(1,36,3))])
F23JaccMean45$Year <- seq(2025,2080,5)
F23JaccMean45 <- F23JaccMean45[,2:1]
names(F23JaccMean45)[2] <- "JaccardMean"
#separate quantiles
F23JaccQ[,1]<-seq(2025,2080,5)
names(F23JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2345 <- merge(F23JaccMean45[1:12,],F23JaccQ[1:12,], by="Year")
CombMeanJaccF2345 <- data.frame(rbind(F23MeanJacc,CombMeanJaccF2345))
#Graph
highlight <- CombMeanJaccF2345[19:30,]
F23Jacc45Plot<- ggplot(data = CombMeanJaccF2345, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2345, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2345, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F24 Group
FutureJaccF2445 <- t(data.frame(apply(F24JaccPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24JaccQ <- map(F24JaccPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24JaccQ <- map_dfr(F24JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24JaccMean45 <- data.frame(FutureJaccF2445[,(seq(1,36,3))])
F24JaccMean45$Year <- seq(2025,2080,5)
F24JaccMean45 <- F24JaccMean45[,2:1]
names(F24JaccMean45)[2] <- "JaccardMean"
#separate quantiles
F24JaccQ[,1]<-seq(2025,2080,5)
names(F24JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2445 <- merge(F24JaccMean45[1:12,],F24JaccQ[1:12,], by="Year")
CombMeanJaccF2445 <- data.frame(rbind(F24MeanJacc,CombMeanJaccF2445))
#Graph
highlight <- CombMeanJaccF2445[19:30,]
F24Jacc45Plot<- ggplot(data = CombMeanJaccF2445, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2445, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2445, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#RCP85
#F1 Group
FutureJaccF185 <- t(data.frame(apply(F1JaccPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1JaccQ <- map(F1JaccPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1JaccQ <- map_dfr(F1JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1JaccMean85 <- data.frame(FutureJaccF185[,(seq(1,36,3))])
F1JaccMean85$Year <- seq(2025,2080,5)
F1JaccMean85 <- F1JaccMean85[,2:1]
names(F1JaccMean85)[2] <- "JaccardMean"
#separate quantiles
F1JaccQ[,1]<-seq(2025,2080,5)
names(F1JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF185 <- merge(F1JaccMean85[1:12,],F1JaccQ[1:12,], by="Year")
CombMeanJaccF185 <- data.frame(rbind(F1MeanJacc,CombMeanJaccF185))
#Graph
highlight <- CombMeanJaccF185[19:30,]
F1Jacc85Plot<- ggplot(data = CombMeanJaccF185, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F5 Group
FutureJaccF585 <- t(data.frame(apply(F5JaccPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5JaccQ <- map(F5JaccPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5JaccQ <- map_dfr(F5JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5JaccMean85 <- data.frame(FutureJaccF585[,(seq(1,36,3))])
F5JaccMean85$Year <- seq(2025,2080,5)
F5JaccMean85 <- F5JaccMean85[,2:1]
names(F5JaccMean85)[2] <- "JaccardMean"
#separate quantiles
F5JaccQ[,1]<-seq(2025,2080,5)
names(F5JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF585 <- merge(F5JaccMean85[1:12,],F5JaccQ[1:12,], by="Year")
CombMeanJaccF585 <- data.frame(rbind(F5MeanJacc,CombMeanJaccF585))
#Graph
highlight <- CombMeanJaccF585[18:29,]
F5Jacc85Plot<- ggplot(data = CombMeanJaccF585, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF585, row_number() %in% 2:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF585, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F20 Group
FutureJaccF2085 <- t(data.frame(apply(F20JaccPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20JaccQ <- map(F20JaccPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20JaccQ <- map_dfr(F20JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20JaccMean85 <- data.frame(FutureJaccF2085[,(seq(1,36,3))])
F20JaccMean85$Year <- seq(2025,2080,5)
F20JaccMean85 <- F20JaccMean85[,2:1]
names(F20JaccMean85)[2] <- "JaccardMean"
#separate quantiles
F20JaccQ[,1]<-seq(2025,2080,5)
names(F20JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2085 <- merge(F20JaccMean85[1:12,],F20JaccQ[1:12,], by="Year")
CombMeanJaccF2085 <- data.frame(rbind(F20MeanJacc,CombMeanJaccF2085))
#Graph
highlight <- CombMeanJaccF2085[19:30,]
F20Jacc85Plot<- ggplot(data = CombMeanJaccF2085, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2085, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2085, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 


#F21 Group
FutureJaccF2185 <- t(data.frame(apply(F21JaccPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21JaccQ <- map(F21JaccPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21JaccQ <- map_dfr(F21JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21JaccMean85 <- data.frame(FutureJaccF2185[,(seq(1,36,3))])
F21JaccMean85$Year <- seq(2025,2080,5)
F21JaccMean85 <- F21JaccMean85[,2:1]
names(F21JaccMean85)[2] <- "JaccardMean"
#separate quantiles
F21JaccQ[,1]<-seq(2025,2080,5)
names(F21JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2185 <- merge(F21JaccMean85[1:12,],F21JaccQ[1:12,], by="Year")
CombMeanJaccF2185 <- data.frame(rbind(F21MeanJacc,CombMeanJaccF2185))
#Graph
highlight <- CombMeanJaccF2185[19:30,]
F21Jacc85Plot<- ggplot(data = CombMeanJaccF2185, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F23 Group
FutureJaccF2385 <- t(data.frame(apply(F23JaccPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23JaccQ <- map(F23JaccPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23JaccQ <- map_dfr(F23JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23JaccMean85 <- data.frame(FutureJaccF2385[,(seq(1,36,3))])
F23JaccMean85$Year <- seq(2025,2080,5)
F23JaccMean85 <- F23JaccMean85[,2:1]
names(F23JaccMean85)[2] <- "JaccardMean"
#separate quantiles
F23JaccQ[,1]<-seq(2025,2080,5)
names(F23JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2385 <- merge(F23JaccMean85[1:12,],F23JaccQ[1:12,], by="Year")
CombMeanJaccF2385 <- data.frame(rbind(F23MeanJacc,CombMeanJaccF2385))
#Graph
highlight <- CombMeanJaccF2385[19:30,]
F23Jacc85Plot<- ggplot(data = CombMeanJaccF2385, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2385, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2385, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F24 Group
FutureJaccF2485 <- t(data.frame(apply(F24JaccPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24JaccQ <- map(F24JaccPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24JaccQ <- map_dfr(F24JaccQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24JaccMean85 <- data.frame(FutureJaccF2485[,(seq(1,36,3))])
F24JaccMean85$Year <- seq(2025,2080,5)
F24JaccMean85 <- F24JaccMean85[,2:1]
names(F24JaccMean85)[2] <- "JaccardMean"
#separate quantiles
F24JaccQ[,1]<-seq(2025,2080,5)
names(F24JaccQ)<-c("Year","Q5","Q95")
#combine data
CombMeanJaccF2485 <- merge(F24JaccMean85[1:12,],F24JaccQ[1:12,], by="Year")
CombMeanJaccF2485 <- data.frame(rbind(F24MeanJacc,CombMeanJaccF2485))
#Graph
highlight <- CombMeanJaccF2485[19:30,]
F24Jacc85Plot<- ggplot(data = CombMeanJaccF2485, aes(x=Year,y=JaccardMean))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Jaccard’s Similarity Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanJaccF2485, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanJaccF2485, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=JaccardMean), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#create multiplots
F1CombJacc <- F1Jacc26Plot/F1Jacc45Plot/F1Jacc85Plot
F1CombJacc + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F5CombJacc <- F5Jacc26Plot/F5Jacc45Plot/F5Jacc85Plot
F5CombJacc + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F20CombJacc <- F20Jacc26Plot/F20Jacc45Plot/F20Jacc85Plot
F20CombJacc + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F21CombJacc <- F21Jacc26Plot/F21Jacc45Plot/F21Jacc85Plot
F21CombJacc + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F23CombJacc <- F23Jacc26Plot/F23Jacc45Plot/F23Jacc85Plot
F23CombJacc + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F24CombJacc <- F24Jacc26Plot/F24Jacc45Plot/F24Jacc85Plot
F24CombJacc + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))



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

#Calculate and visualize annual means
#RCP26
#F1 Group
F1MeanMPD <- F1Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F1Q5MPD <- F1Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F1Q5MPD)[2] <- "Q5"
F1Q95MPD <- F1Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F1Q95MPD)[2] <- "Q95"
F1QMPD <- merge(F1Q5MPD,F1Q95MPD, by="Year")
F1MeanMPD <- merge(F1MeanMPD,F1QMPD, by="Year")
names(F1MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F1MPDPred26[,2:37] <- (F1MPDPred26[,2:37]*scaleMPD1)+centerMPD1
FutureMPDF126 <- t(data.frame(apply(F1MPDPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1MPDQ <- map(F1MPDPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1MPDQ <- map_dfr(F1MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1MPDMean26 <- data.frame(FutureMPDF126[,(seq(1,36,3))])
F1MPDMean26$Year <- seq(2025,2080,5)
F1MPDMean26 <- F1MPDMean26[,2:1]
names(F1MPDMean26)[2] <- "MeanMPDPerAcre"
#separate quantiles
F1MPDQ[,1]<-seq(2025,2080,5)
names(F1MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF126 <- merge(F1MPDMean26[1:12,],F1MPDQ[1:12,], by="Year")
CombMeanMPDF126 <- data.frame(rbind(F1MeanMPD,CombMeanMPDF126))
#Graph
highlight <- CombMeanMPDF126[19:30,]
F1MPD26Plot<- ggplot(data = CombMeanMPDF126, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F5 Group
F5MeanMPD <- F5Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F5Q5MPD <- F5Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F5Q5MPD)[2] <- "Q5"
F5Q95MPD <- F5Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F5Q95MPD)[2] <- "Q95"
F5QMPD <- merge(F5Q5MPD,F5Q95MPD, by="Year")
F5MeanMPD <- merge(F5MeanMPD,F5QMPD, by="Year")
names(F5MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F5MPDPred26[,2:37] <- (F5MPDPred26[,2:37]*scaleMPD5)+centerMPD5
FutureMPDF526 <- t(data.frame(apply(F5MPDPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5MPDQ <- map(F5MPDPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5MPDQ <- map_dfr(F5MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5MPDMean26 <- data.frame(FutureMPDF526[,(seq(1,36,3))])
F5MPDMean26$Year <- seq(2025,2080,5)
F5MPDMean26 <- F5MPDMean26[,2:1]
names(F5MPDMean26)[2] <- "MeanMPDPerAcre"
#separate quantiles
F5MPDQ[,1]<-seq(2025,2080,5)
names(F5MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF526 <- merge(F5MPDMean26[1:12,],F5MPDQ[1:12,], by="Year")
CombMeanMPDF526 <- data.frame(rbind(F5MeanMPD,CombMeanMPDF526))
#Graph
highlight <- CombMeanMPDF526[19:30,]
F5MPD26Plot<- ggplot(data = CombMeanMPDF526, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF526, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF526, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F20 Group
F20MeanMPD <- F20Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F20Q5MPD <- F20Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F20Q5MPD)[2] <- "Q5"
F20Q95MPD <- F20Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F20Q95MPD)[2] <- "Q95"
F20QMPD <- merge(F20Q5MPD,F20Q95MPD, by="Year")
F20MeanMPD <- merge(F20MeanMPD,F20QMPD, by="Year")
names(F20MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F20MPDPred26[,2:37] <- (F20MPDPred26[,2:37]*scaleMPD20)+centerMPD20
FutureMPDF2026 <- t(data.frame(apply(F20MPDPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20MPDQ <- map(F20MPDPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20MPDQ <- map_dfr(F20MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20MPDMean26 <- data.frame(FutureMPDF2026[,(seq(1,36,3))])
F20MPDMean26$Year <- seq(2025,2080,5)
F20MPDMean26 <- F20MPDMean26[,2:1]
names(F20MPDMean26)[2] <- "MeanMPDPerAcre"
#separate quantiles
F20MPDQ[,1]<-seq(2025,2080,5)
names(F20MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2026 <- merge(F20MPDMean26[1:12,],F20MPDQ[1:12,], by="Year")
CombMeanMPDF2026 <- data.frame(rbind(F20MeanMPD,CombMeanMPDF2026))
#Graph
highlight <- CombMeanMPDF2026[19:30,]
F20MPD26Plot<- ggplot(data = CombMeanMPDF2026, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2026, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2026, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F21 Group
F21MeanMPD <- F21Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F21Q5MPD <- F21Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F21Q5MPD)[2] <- "Q5"
F21Q95MPD <- F21Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F21Q95MPD)[2] <- "Q95"
F21QMPD <- merge(F21Q5MPD,F21Q95MPD, by="Year")
F21MeanMPD <- merge(F21MeanMPD,F21QMPD, by="Year")
names(F21MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F21MPDPred26[,2:37] <- (F21MPDPred26[,2:37]*scaleMPD21)+centerMPD21
FutureMPDF2126 <- t(data.frame(apply(F21MPDPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21MPDQ <- map(F21MPDPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21MPDQ <- map_dfr(F21MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21MPDMean26 <- data.frame(FutureMPDF2126[,(seq(1,36,3))])
F21MPDMean26$Year <- seq(2025,2080,5)
F21MPDMean26 <- F21MPDMean26[,2:1]
names(F21MPDMean26)[2] <- "MeanMPDPerAcre"
#separate quantiles
F21MPDQ[,1]<-seq(2025,2080,5)
names(F21MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2126 <- merge(F21MPDMean26[1:12,],F21MPDQ[1:12,], by="Year")
CombMeanMPDF2126 <- data.frame(rbind(F21MeanMPD,CombMeanMPDF2126))
#Graph
highlight <- CombMeanMPDF2126[19:30,]
F21MPD26Plot<- ggplot(data = CombMeanMPDF2126, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F23 Group
F23MeanMPD <- F23Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F23Q5MPD <- F23Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F23Q5MPD)[2] <- "Q5"
F23Q95MPD <- F23Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F23Q95MPD)[2] <- "Q95"
F23QMPD <- merge(F23Q5MPD,F23Q95MPD, by="Year")
F23MeanMPD <- merge(F23MeanMPD,F23QMPD, by="Year")
names(F23MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F23MPDPred26[,2:37] <- (F23MPDPred26[,2:37]*scaleMPD23)+centerMPD23
FutureMPDF2326 <- t(data.frame(apply(F23MPDPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23MPDQ <- map(F23MPDPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23MPDQ <- map_dfr(F23MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23MPDMean26 <- data.frame(FutureMPDF2326[,(seq(1,36,3))])
F23MPDMean26$Year <- seq(2025,2080,5)
F23MPDMean26 <- F23MPDMean26[,2:1]
names(F23MPDMean26)[2] <- "MeanMPDPerAcre"
#separate quantiles
F23MPDQ[,1]<-seq(2025,2080,5)
names(F23MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2326 <- merge(F23MPDMean26[1:12,],F23MPDQ[1:12,], by="Year")
CombMeanMPDF2326 <- data.frame(rbind(F23MeanMPD,CombMeanMPDF2326))
#Graph
highlight <- CombMeanMPDF2326[19:30,]
F23MPD26Plot<- ggplot(data = CombMeanMPDF2326, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2326, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2326, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F24 Group
F24MeanMPD <- F24Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F24Q5MPD <- F24Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F24Q5MPD)[2] <- "Q5"
F24Q95MPD <- F24Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F24Q95MPD)[2] <- "Q95"
F24QMPD <- merge(F24Q5MPD,F24Q95MPD, by="Year")
F24MeanMPD <- merge(F24MeanMPD,F24QMPD, by="Year")
names(F24MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F24MPDPred26[,2:37] <- (F24MPDPred26[,2:37]*scaleMPD24)+centerMPD24
FutureMPDF2426 <- t(data.frame(apply(F24MPDPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24MPDQ <- map(F24MPDPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24MPDQ <- map_dfr(F24MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24MPDMean26 <- data.frame(FutureMPDF2426[,(seq(1,36,3))])
F24MPDMean26$Year <- seq(2025,2080,5)
F24MPDMean26 <- F24MPDMean26[,2:1]
names(F24MPDMean26)[2] <- "MeanMPDPerAcre"
#separate quantiles
F24MPDQ[,1]<-seq(2025,2080,5)
names(F24MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2426 <- merge(F24MPDMean26[1:12,],F24MPDQ[1:12,], by="Year")
CombMeanMPDF2426 <- data.frame(rbind(F24MeanMPD,CombMeanMPDF2426))
#Graph
highlight <- CombMeanMPDF2426[19:30,]
F24MPD26Plot<- ggplot(data = CombMeanMPDF2426, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2426, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2426, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F1 Group RCP45
#F1 Group
F1MeanMPD <- F1Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F1Q5MPD <- F1Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F1Q5MPD)[2] <- "Q5"
F1Q95MPD <- F1Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F1Q95MPD)[2] <- "Q95"
F1QMPD <- merge(F1Q5MPD,F1Q95MPD, by="Year")
F1MeanMPD <- merge(F1MeanMPD,F1QMPD, by="Year")
names(F1MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F1MPDPred45[,2:37] <- (F1MPDPred45[,2:37]*scaleMPD1)+centerMPD1
FutureMPDF145 <- t(data.frame(apply(F1MPDPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1MPDQ <- map(F1MPDPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1MPDQ <- map_dfr(F1MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1MPDMean45 <- data.frame(FutureMPDF145[,(seq(1,36,3))])
F1MPDMean45$Year <- seq(2025,2080,5)
F1MPDMean45 <- F1MPDMean45[,2:1]
names(F1MPDMean45)[2] <- "MeanMPDPerAcre"
#separate quantiles
F1MPDQ[,1]<-seq(2025,2080,5)
names(F1MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF145 <- merge(F1MPDMean45[1:12,],F1MPDQ[1:12,], by="Year")
CombMeanMPDF145 <- data.frame(rbind(F1MeanMPD,CombMeanMPDF145))
#Graph
highlight <- CombMeanMPDF145[19:30,]
F1MPD45Plot<- ggplot(data = CombMeanMPDF145, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F5 Group
F5MeanMPD <- F5Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F5Q5MPD <- F5Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F5Q5MPD)[2] <- "Q5"
F5Q95MPD <- F5Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F5Q95MPD)[2] <- "Q95"
F5QMPD <- merge(F5Q5MPD,F5Q95MPD, by="Year")
F5MeanMPD <- merge(F5MeanMPD,F5QMPD, by="Year")
names(F5MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F5MPDPred45[,2:37] <- (F5MPDPred45[,2:37]*scaleMPD5)+centerMPD5
FutureMPDF545 <- t(data.frame(apply(F5MPDPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5MPDQ <- map(F5MPDPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5MPDQ <- map_dfr(F5MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5MPDMean45 <- data.frame(FutureMPDF545[,(seq(1,36,3))])
F5MPDMean45$Year <- seq(2025,2080,5)
F5MPDMean45 <- F5MPDMean45[,2:1]
names(F5MPDMean45)[2] <- "MeanMPDPerAcre"
#separate quantiles
F5MPDQ[,1]<-seq(2025,2080,5)
names(F5MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF545 <- merge(F5MPDMean45[1:12,],F5MPDQ[1:12,], by="Year")
CombMeanMPDF545 <- data.frame(rbind(F5MeanMPD,CombMeanMPDF545))
#Graph
highlight <- CombMeanMPDF545[19:30,]
F5MPD45Plot<- ggplot(data = CombMeanMPDF545, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF545, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF545, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F20 Group
F20MeanMPD <- F20Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F20Q5MPD <- F20Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F20Q5MPD)[2] <- "Q5"
F20Q95MPD <- F20Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F20Q95MPD)[2] <- "Q95"
F20QMPD <- merge(F20Q5MPD,F20Q95MPD, by="Year")
F20MeanMPD <- merge(F20MeanMPD,F20QMPD, by="Year")
names(F20MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F20MPDPred45[,2:37] <- (F20MPDPred45[,2:37]*scaleMPD20)+centerMPD20
FutureMPDF2045 <- t(data.frame(apply(F20MPDPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20MPDQ <- map(F20MPDPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20MPDQ <- map_dfr(F20MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20MPDMean45 <- data.frame(FutureMPDF2045[,(seq(1,36,3))])
F20MPDMean45$Year <- seq(2025,2080,5)
F20MPDMean45 <- F20MPDMean45[,2:1]
names(F20MPDMean45)[2] <- "MeanMPDPerAcre"
#separate quantiles
F20MPDQ[,1]<-seq(2025,2080,5)
names(F20MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2045 <- merge(F20MPDMean45[1:12,],F20MPDQ[1:12,], by="Year")
CombMeanMPDF2045 <- data.frame(rbind(F20MeanMPD,CombMeanMPDF2045))
#Graph
highlight <- CombMeanMPDF2045[19:30,]
F20MPD45Plot<- ggplot(data = CombMeanMPDF2045, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2045, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2045, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F21 Group
F21MeanMPD <- F21Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F21Q5MPD <- F21Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F21Q5MPD)[2] <- "Q5"
F21Q95MPD <- F21Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F21Q95MPD)[2] <- "Q95"
F21QMPD <- merge(F21Q5MPD,F21Q95MPD, by="Year")
F21MeanMPD <- merge(F21MeanMPD,F21QMPD, by="Year")
names(F21MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F21MPDPred45[,2:37] <- (F21MPDPred45[,2:37]*scaleMPD21)+centerMPD21
FutureMPDF2145 <- t(data.frame(apply(F21MPDPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21MPDQ <- map(F21MPDPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21MPDQ <- map_dfr(F21MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21MPDMean45 <- data.frame(FutureMPDF2145[,(seq(1,36,3))])
F21MPDMean45$Year <- seq(2025,2080,5)
F21MPDMean45 <- F21MPDMean45[,2:1]
names(F21MPDMean45)[2] <- "MeanMPDPerAcre"
#separate quantiles
F21MPDQ[,1]<-seq(2025,2080,5)
names(F21MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2145 <- merge(F21MPDMean45[1:12,],F21MPDQ[1:12,], by="Year")
CombMeanMPDF2145 <- data.frame(rbind(F21MeanMPD,CombMeanMPDF2145))
#Graph
highlight <- CombMeanMPDF2145[19:30,]
F21MPD45Plot<- ggplot(data = CombMeanMPDF2145, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F23 Group
F23MeanMPD <- F23Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F23Q5MPD <- F23Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F23Q5MPD)[2] <- "Q5"
F23Q95MPD <- F23Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F23Q95MPD)[2] <- "Q95"
F23QMPD <- merge(F23Q5MPD,F23Q95MPD, by="Year")
F23MeanMPD <- merge(F23MeanMPD,F23QMPD, by="Year")
names(F23MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F23MPDPred45[,2:37] <- (F23MPDPred45[,2:37]*scaleMPD23)+centerMPD23
FutureMPDF2345 <- t(data.frame(apply(F23MPDPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23MPDQ <- map(F23MPDPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23MPDQ <- map_dfr(F23MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23MPDMean45 <- data.frame(FutureMPDF2345[,(seq(1,36,3))])
F23MPDMean45$Year <- seq(2025,2080,5)
F23MPDMean45 <- F23MPDMean45[,2:1]
names(F23MPDMean45)[2] <- "MeanMPDPerAcre"
#separate quantiles
F23MPDQ[,1]<-seq(2025,2080,5)
names(F23MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2345 <- merge(F23MPDMean45[1:12,],F23MPDQ[1:12,], by="Year")
CombMeanMPDF2345 <- data.frame(rbind(F23MeanMPD,CombMeanMPDF2345))
#Graph
highlight <- CombMeanMPDF2345[19:30,]
F23MPD45Plot<- ggplot(data = CombMeanMPDF2345, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2345, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2345, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F24 Group
F24MeanMPD <- F24Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F24Q5MPD <- F24Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F24Q5MPD)[2] <- "Q5"
F24Q95MPD <- F24Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F24Q95MPD)[2] <- "Q95"
F24QMPD <- merge(F24Q5MPD,F24Q95MPD, by="Year")
F24MeanMPD <- merge(F24MeanMPD,F24QMPD, by="Year")
names(F24MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F24MPDPred45[,2:37] <- (F24MPDPred45[,2:37]*scaleMPD24)+centerMPD24
FutureMPDF2445 <- t(data.frame(apply(F24MPDPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24MPDQ <- map(F24MPDPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24MPDQ <- map_dfr(F24MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24MPDMean45 <- data.frame(FutureMPDF2445[,(seq(1,36,3))])
F24MPDMean45$Year <- seq(2025,2080,5)
F24MPDMean45 <- F24MPDMean45[,2:1]
names(F24MPDMean45)[2] <- "MeanMPDPerAcre"
#separate quantiles
F24MPDQ[,1]<-seq(2025,2080,5)
names(F24MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2445 <- merge(F24MPDMean45[1:12,],F24MPDQ[1:12,], by="Year")
CombMeanMPDF2445 <- data.frame(rbind(F24MeanMPD,CombMeanMPDF2445))
#Graph
highlight <- CombMeanMPDF2445[19:30,]
F24MPD45Plot<- ggplot(data = CombMeanMPDF2445, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2445, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2445, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#RCP85
#F1 Group
F1MeanMPD <- F1Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F1Q5MPD <- F1Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F1Q5MPD)[2] <- "Q5"
F1Q95MPD <- F1Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F1Q95MPD)[2] <- "Q95"
F1QMPD <- merge(F1Q5MPD,F1Q95MPD, by="Year")
F1MeanMPD <- merge(F1MeanMPD,F1QMPD, by="Year")
names(F1MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F1MPDPred85[,2:37] <- (F1MPDPred85[,2:37]*scaleMPD1)+centerMPD1
FutureMPDF185 <- t(data.frame(apply(F1MPDPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1MPDQ <- map(F1MPDPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1MPDQ <- map_dfr(F1MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1MPDMean85 <- data.frame(FutureMPDF185[,(seq(1,36,3))])
F1MPDMean85$Year <- seq(2025,2080,5)
F1MPDMean85 <- F1MPDMean85[,2:1]
names(F1MPDMean85)[2] <- "MeanMPDPerAcre"
#separate quantiles
F1MPDQ[,1]<-seq(2025,2080,5)
names(F1MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF185 <- merge(F1MPDMean85[1:12,],F1MPDQ[1:12,], by="Year")
CombMeanMPDF185 <- data.frame(rbind(F1MeanMPD,CombMeanMPDF185))
#Graph
highlight <- CombMeanMPDF185[19:30,]
F1MPD85Plot<- ggplot(data = CombMeanMPDF185, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F5 Group
F5MeanMPD <- F5Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F5Q5MPD <- F5Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F5Q5MPD)[2] <- "Q5"
F5Q95MPD <- F5Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F5Q95MPD)[2] <- "Q95"
F5QMPD <- merge(F5Q5MPD,F5Q95MPD, by="Year")
F5MeanMPD <- merge(F5MeanMPD,F5QMPD, by="Year")
names(F5MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F5MPDPred85[,2:37] <- (F5MPDPred85[,2:37]*scaleMPD5)+centerMPD5
FutureMPDF585 <- t(data.frame(apply(F5MPDPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5MPDQ <- map(F5MPDPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5MPDQ <- map_dfr(F5MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5MPDMean85 <- data.frame(FutureMPDF585[,(seq(1,36,3))])
F5MPDMean85$Year <- seq(2025,2080,5)
F5MPDMean85 <- F5MPDMean85[,2:1]
names(F5MPDMean85)[2] <- "MeanMPDPerAcre"
#separate quantiles
F5MPDQ[,1]<-seq(2025,2080,5)
names(F5MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF585 <- merge(F5MPDMean85[1:12,],F5MPDQ[1:12,], by="Year")
CombMeanMPDF585 <- data.frame(rbind(F5MeanMPD,CombMeanMPDF585))
#Graph
highlight <- CombMeanMPDF585[19:30,]
F5MPD85Plot<- ggplot(data = CombMeanMPDF585, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF585, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF585, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F20 Group
F20MeanMPD <- F20Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F20Q5MPD <- F20Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F20Q5MPD)[2] <- "Q5"
F20Q95MPD <- F20Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F20Q95MPD)[2] <- "Q95"
F20QMPD <- merge(F20Q5MPD,F20Q95MPD, by="Year")
F20MeanMPD <- merge(F20MeanMPD,F20QMPD, by="Year")
names(F20MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F20MPDPred85[,2:37] <- (F20MPDPred85[,2:37]*scaleMPD20)+centerMPD20
FutureMPDF2085 <- t(data.frame(apply(F20MPDPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20MPDQ <- map(F20MPDPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20MPDQ <- map_dfr(F20MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20MPDMean85 <- data.frame(FutureMPDF2085[,(seq(1,36,3))])
F20MPDMean85$Year <- seq(2025,2080,5)
F20MPDMean85 <- F20MPDMean85[,2:1]
names(F20MPDMean85)[2] <- "MeanMPDPerAcre"
#separate quantiles
F20MPDQ[,1]<-seq(2025,2080,5)
names(F20MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2085 <- merge(F20MPDMean85[1:12,],F20MPDQ[1:12,], by="Year")
CombMeanMPDF2085 <- data.frame(rbind(F20MeanMPD,CombMeanMPDF2085))
#Graph
highlight <- CombMeanMPDF2085[19:30,]
F20MPD85Plot<- ggplot(data = CombMeanMPDF2085, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2085, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2085, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F21 Group
F21MeanMPD <- F21Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F21Q5MPD <- F21Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F21Q5MPD)[2] <- "Q5"
F21Q95MPD <- F21Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F21Q95MPD)[2] <- "Q95"
F21QMPD <- merge(F21Q5MPD,F21Q95MPD, by="Year")
F21MeanMPD <- merge(F21MeanMPD,F21QMPD, by="Year")
names(F21MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F21MPDPred85[,2:37] <- (F21MPDPred85[,2:37]*scaleMPD21)+centerMPD21
FutureMPDF2185 <- t(data.frame(apply(F21MPDPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21MPDQ <- map(F21MPDPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21MPDQ <- map_dfr(F21MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21MPDMean85 <- data.frame(FutureMPDF2185[,(seq(1,36,3))])
F21MPDMean85$Year <- seq(2025,2080,5)
F21MPDMean85 <- F21MPDMean85[,2:1]
names(F21MPDMean85)[2] <- "MeanMPDPerAcre"
#separate quantiles
F21MPDQ[,1]<-seq(2025,2080,5)
names(F21MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2185 <- merge(F21MPDMean85[1:12,],F21MPDQ[1:12,], by="Year")
CombMeanMPDF2185 <- data.frame(rbind(F21MeanMPD,CombMeanMPDF2185))
#Graph
highlight <- CombMeanMPDF2185[19:30,]
F21MPD85Plot<- ggplot(data = CombMeanMPDF2185, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F23 Group
F23MeanMPD <- F23Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F23Q5MPD <- F23Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F23Q5MPD)[2] <- "Q5"
F23Q95MPD <- F23Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F23Q95MPD)[2] <- "Q95"
F23QMPD <- merge(F23Q5MPD,F23Q95MPD, by="Year")
F23MeanMPD <- merge(F23MeanMPD,F23QMPD, by="Year")
names(F23MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F23MPDPred85[,2:37] <- (F23MPDPred85[,2:37]*scaleMPD23)+centerMPD23
FutureMPDF2385 <- t(data.frame(apply(F23MPDPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23MPDQ <- map(F23MPDPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23MPDQ <- map_dfr(F23MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23MPDMean85 <- data.frame(FutureMPDF2385[,(seq(1,36,3))])
F23MPDMean85$Year <- seq(2025,2080,5)
F23MPDMean85 <- F23MPDMean85[,2:1]
names(F23MPDMean85)[2] <- "MeanMPDPerAcre"
#separate quantiles
F23MPDQ[,1]<-seq(2025,2080,5)
names(F23MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2385 <- merge(F23MPDMean85[1:12,],F23MPDQ[1:12,], by="Year")
CombMeanMPDF2385 <- data.frame(rbind(F23MeanMPD,CombMeanMPDF2385))
#Graph
highlight <- CombMeanMPDF2385[19:30,]
F23MPD85Plot<- ggplot(data = CombMeanMPDF2385, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2385, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2385, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F24 Group
F24MeanMPD <- F24Re %>% group_by(Year)%>%summarise(mean(MeanPairwiseDistance))
F24Q5MPD <- F24Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.05))
names(F24Q5MPD)[2] <- "Q5"
F24Q95MPD <- F24Re %>% group_by(Year)%>%reframe(quantile(MeanPairwiseDistance, prob=0.95))                                                                                               
names(F24Q95MPD)[2] <- "Q95"
F24QMPD <- merge(F24Q5MPD,F24Q95MPD, by="Year")
F24MeanMPD <- merge(F24MeanMPD,F24QMPD, by="Year")
names(F24MeanMPD)<- c("Year","MeanMPDPerAcre","Q5","Q95")
#future values
#unscale future values
F24MPDPred85[,2:37] <- (F24MPDPred85[,2:37]*scaleMPD24)+centerMPD24
FutureMPDF2485 <- t(data.frame(apply(F24MPDPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24MPDQ <- map(F24MPDPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24MPDQ <- map_dfr(F24MPDQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24MPDMean85 <- data.frame(FutureMPDF2485[,(seq(1,36,3))])
F24MPDMean85$Year <- seq(2025,2080,5)
F24MPDMean85 <- F24MPDMean85[,2:1]
names(F24MPDMean85)[2] <- "MeanMPDPerAcre"
#separate quantiles
F24MPDQ[,1]<-seq(2025,2080,5)
names(F24MPDQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMPDF2485 <- merge(F24MPDMean85[1:12,],F24MPDQ[1:12,], by="Year")
CombMeanMPDF2485 <- data.frame(rbind(F24MeanMPD,CombMeanMPDF2485))
#Graph
highlight <- CombMeanMPDF2485[19:30,]
F24MPD85Plot<- ggplot(data = CombMeanMPDF2485, aes(x=Year,y=MeanMPDPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("AverageMean Pairwise Distance") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMPDF2485, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMPDF2485, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanMPDPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

F1CombMPD <- F1MPD26Plot/F1MPD45Plot/F1MPD85Plot
F1CombMPD + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F5CombMPD <- F5MPD26Plot/F5MPD45Plot/F5MPD85Plot
F5CombMPD + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F20CombMPD <- F20MPD26Plot/F20MPD45Plot/F20MPD85Plot
F20CombMPD + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F21CombMPD <- F21MPD26Plot/F21MPD45Plot/F21MPD85Plot
F21CombMPD + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F23CombMPD <- F23MPD26Plot/F23MPD45Plot/F23MPD85Plot
F23CombMPD + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F24CombMPD <- F24MPD26Plot/F24MPD45Plot/F24MPD85Plot
F24CombMPD + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))


#Mortality, don't scale
#F1 Group RCP26
F1MeanMort <- F1Re %>% group_by(Year)%>%summarise(mean(MortRate))
F1Q5Mort <- F1Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.05))
names(F1Q5Mort)[2] <- "Q5"
F1Q95Mort <- F1Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.95))                                                                                               
names(F1Q95Mort)[2] <- "Q95"
F1QMort <- merge(F1Q5Mort,F1Q95Mort, by="Year")
F1MeanMort <- merge(F1MeanMort,F1QMort, by="Year")
names(F1MeanMort)<- c("Year","MortRate","Q5","Q95")
FutureMortF126 <- t(data.frame(apply(F1MortPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1MortQ <- map(F1MortPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1MortQ <- map_dfr(F1MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1MortMean26 <- data.frame(FutureMortF126[,(seq(1,36,3))])
F1MortMean26$Year <- seq(2025,2080,5)
F1MortMean26 <- F1MortMean26[,2:1]
names(F1MortMean26)[2] <- "MortRate"
#separate quantiles
F1MortQ[,1]<-seq(2025,2080,5)
names(F1MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF126 <- merge(F1MortMean26[1:12,],F1MortQ[1:12,], by="Year")
CombMeanMortF126 <- data.frame(rbind(F1MeanMort,CombMeanMortF126))
#Graph
highlight <- CombMeanMortF126[19:30,]
F1Mort26Plot<- ggplot(data = CombMeanMortF126, aes(x=Year,y=MortRate))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F5 Group
F5MeanMort <- F5Re %>% group_by(Year)%>%summarise(mean(MortRate))
F5Q5Mort <- F5Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.05))
names(F5Q5Mort)[2] <- "Q5"
F5Q95Mort <- F5Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.95))                                                                                               
names(F5Q95Mort)[2] <- "Q95"
F5QMort <- merge(F5Q5Mort,F5Q95Mort, by="Year")
F5MeanMort <- merge(F5MeanMort,F5QMort, by="Year")
names(F5MeanMort)<- c("Year","MortRate","Q5","Q95")
FutureMortF526 <- t(data.frame(apply(F5MortPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5MortQ <- map(F5MortPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5MortQ <- map_dfr(F5MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5MortMean26 <- data.frame(FutureMortF526[,(seq(1,36,3))])
F5MortMean26$Year <- seq(2025,2080,5)
F5MortMean26 <- F5MortMean26[,2:1]
names(F5MortMean26)[2] <- "MortRate"
#separate quantiles
F5MortQ[,1]<-seq(2025,2080,5)
names(F5MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF526 <- merge(F5MortMean26[1:12,],F5MortQ[1:12,], by="Year")
CombMeanMortF526 <- data.frame(rbind(F5MeanMort,CombMeanMortF526))
#Graph
highlight <- CombMeanMortF526[18:29,]
F5Mort26Plot<- ggplot(data = CombMeanMortF526[2:29,], aes(x=Year,y=MortRate))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF526, row_number() %in% 2:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF526, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F20 Group
F20MeanMort <- F20Re %>% group_by(Year)%>%summarise(mean(MortRate))
F20Q5Mort <- F20Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.05))
names(F20Q5Mort)[2] <- "Q5"
F20Q95Mort <- F20Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.95))                                                                                               
names(F20Q95Mort)[2] <- "Q95"
F20QMort <- merge(F20Q5Mort,F20Q95Mort, by="Year")
F20MeanMort <- merge(F20MeanMort,F20QMort, by="Year")
names(F20MeanMort)<- c("Year","MortRate","Q5","Q95")
FutureMortF2026 <- t(data.frame(apply(F20MortPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20MortQ <- map(F20MortPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20MortQ <- map_dfr(F20MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20MortMean26 <- data.frame(FutureMortF2026[,(seq(1,36,3))])
F20MortMean26$Year <- seq(2025,2080,5)
F20MortMean26 <- F20MortMean26[,2:1]
names(F20MortMean26)[2] <- "MortRate"
#separate quantiles
F20MortQ[,1]<-seq(2025,2080,5)
names(F20MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2026 <- merge(F20MortMean26[1:12,],F20MortQ[1:12,], by="Year")
CombMeanMortF2026 <- data.frame(rbind(F20MeanMort,CombMeanMortF2026))
#Graph
highlight <- CombMeanMortF2026[19:30,]
F20Mort26Plot<- ggplot(data = CombMeanMortF2026, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2026, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2026, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 


#F21 Group
F21MeanMort <- F21Re %>% group_by(Year)%>%summarise(mean(MortRate))
F21Q5Mort <- F21Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.05))
names(F21Q5Mort)[2] <- "Q5"
F21Q95Mort <- F21Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.95))                                                                                               
names(F21Q95Mort)[2] <- "Q95"
F21QMort <- merge(F21Q5Mort,F21Q95Mort, by="Year")
F21MeanMort <- merge(F21MeanMort,F21QMort, by="Year")
names(F21MeanMort)<- c("Year","MortRate","Q5","Q95")
FutureMortF2126 <- t(data.frame(apply(F21MortPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21MortQ <- map(F21MortPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21MortQ <- map_dfr(F21MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21MortMean26 <- data.frame(FutureMortF2126[,(seq(1,36,3))])
F21MortMean26$Year <- seq(2025,2080,5)
F21MortMean26 <- F21MortMean26[,2:1]
names(F21MortMean26)[2] <- "MortRate"
#separate quantiles
F21MortQ[,1]<-seq(2025,2080,5)
names(F21MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2126 <- merge(F21MortMean26[1:12,],F21MortQ[1:12,], by="Year")
CombMeanMortF2126 <- data.frame(rbind(F21MeanMort,CombMeanMortF2126))
#Graph
highlight <- CombMeanMortF2126[19:30,]
F21Mort26Plot<- ggplot(data = CombMeanMortF2126, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F23 Group
F23MeanMort <- F23Re %>% group_by(Year)%>%summarise(mean(MortRate))
F23Q5Mort <- F23Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.05))
names(F23Q5Mort)[2] <- "Q5"
F23Q95Mort <- F23Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.95))                                                                                               
names(F23Q95Mort)[2] <- "Q95"
F23QMort <- merge(F23Q5Mort,F23Q95Mort, by="Year")
F23MeanMort <- merge(F23MeanMort,F23QMort, by="Year")
names(F23MeanMort)<- c("Year","MortRate","Q5","Q95")
FutureMortF2326 <- t(data.frame(apply(F23MortPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23MortQ <- map(F23MortPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23MortQ <- map_dfr(F23MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23MortMean26 <- data.frame(FutureMortF2326[,(seq(1,36,3))])
F23MortMean26$Year <- seq(2025,2080,5)
F23MortMean26 <- F23MortMean26[,2:1]
names(F23MortMean26)[2] <- "MortRate"
#separate quantiles
F23MortQ[,1]<-seq(2025,2080,5)
names(F23MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2326 <- merge(F23MortMean26[1:12,],F23MortQ[1:12,], by="Year")
CombMeanMortF2326 <- data.frame(rbind(F23MeanMort,CombMeanMortF2326))
#Graph
highlight <- CombMeanMortF2326[19:30,]
F23Mort26Plot<- ggplot(data = CombMeanMortF2326, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2326, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2326, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F24 Group
F24MeanMort <- F24Re %>% group_by(Year)%>%summarise(mean(MortRate))
F24Q5Mort <- F24Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.05))
names(F24Q5Mort)[2] <- "Q5"
F24Q95Mort <- F24Re %>% group_by(Year)%>%reframe(quantile(MortRate, prob=0.95))                                                                                               
names(F24Q95Mort)[2] <- "Q95"
F24QMort <- merge(F24Q5Mort,F24Q95Mort, by="Year")
F24MeanMort <- merge(F24MeanMort,F24QMort, by="Year")
names(F24MeanMort)<- c("Year","MortRate","Q5","Q95")
FutureMortF2426 <- t(data.frame(apply(F24MortPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24MortQ <- map(F24MortPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24MortQ <- map_dfr(F24MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24MortMean26 <- data.frame(FutureMortF2426[,(seq(1,36,3))])
F24MortMean26$Year <- seq(2025,2080,5)
F24MortMean26 <- F24MortMean26[,2:1]
names(F24MortMean26)[2] <- "MortRate"
#separate quantiles
F24MortQ[,1]<-seq(2025,2080,5)
names(F24MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2426 <- merge(F24MortMean26[1:12,],F24MortQ[1:12,], by="Year")
CombMeanMortF2426 <- data.frame(rbind(F24MeanMort,CombMeanMortF2426))
#Graph
highlight <- CombMeanMortF2426[19:30,]
F24Mort26Plot<- ggplot(data = CombMeanMortF2426, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2426, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2426, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F1 Group RCP45
#F1 Group
FutureMortF145 <- t(data.frame(apply(F1MortPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1MortQ <- map(F1MortPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1MortQ <- map_dfr(F1MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1MortMean45 <- data.frame(FutureMortF145[,(seq(1,36,3))])
F1MortMean45$Year <- seq(2025,2080,5)
F1MortMean45 <- F1MortMean45[,2:1]
names(F1MortMean45)[2] <- "MortRate"
#separate quantiles
F1MortQ[,1]<-seq(2025,2080,5)
names(F1MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF145 <- merge(F1MortMean45[1:12,],F1MortQ[1:12,], by="Year")
CombMeanMortF145 <- data.frame(rbind(F1MeanMort,CombMeanMortF145))
#Graph
highlight <- CombMeanMortF145[19:30,]
F1Mort45Plot<- ggplot(data = CombMeanMortF145, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F5 Group
FutureMortF545 <- t(data.frame(apply(F5MortPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5MortQ <- map(F5MortPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5MortQ <- map_dfr(F5MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5MortMean45 <- data.frame(FutureMortF545[,(seq(1,36,3))])
F5MortMean45$Year <- seq(2025,2080,5)
F5MortMean45 <- F5MortMean45[,2:1]
names(F5MortMean45)[2] <- "MortRate"
#separate quantiles
F5MortQ[,1]<-seq(2025,2080,5)
names(F5MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF545 <- merge(F5MortMean45[1:12,],F5MortQ[1:12,], by="Year")
CombMeanMortF545 <- data.frame(rbind(F5MeanMort,CombMeanMortF545))
#Graph
highlight <- CombMeanMortF545[18:29,]
F5Mort45Plot<- ggplot(data = CombMeanMortF545[2:29,], aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF545, row_number() %in% 2:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF545, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F20 Group
FutureMortF2045 <- t(data.frame(apply(F20MortPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20MortQ <- map(F20MortPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20MortQ <- map_dfr(F20MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20MortMean45 <- data.frame(FutureMortF2045[,(seq(1,36,3))])
F20MortMean45$Year <- seq(2025,2080,5)
F20MortMean45 <- F20MortMean45[,2:1]
names(F20MortMean45)[2] <- "MortRate"
#separate quantiles
F20MortQ[,1]<-seq(2025,2080,5)
names(F20MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2045 <- merge(F20MortMean45[1:12,],F20MortQ[1:12,], by="Year")
CombMeanMortF2045 <- data.frame(rbind(F20MeanMort,CombMeanMortF2045))
#Graph
highlight <- CombMeanMortF2045[19:30,]
F20Mort45Plot<- ggplot(data = CombMeanMortF2045, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2045, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2045, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 


#F21 Group
FutureMortF2145 <- t(data.frame(apply(F21MortPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21MortQ <- map(F21MortPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21MortQ <- map_dfr(F21MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21MortMean45 <- data.frame(FutureMortF2145[,(seq(1,36,3))])
F21MortMean45$Year <- seq(2025,2080,5)
F21MortMean45 <- F21MortMean45[,2:1]
names(F21MortMean45)[2] <- "MortRate"
#separate quantiles
F21MortQ[,1]<-seq(2025,2080,5)
names(F21MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2145 <- merge(F21MortMean45[1:12,],F21MortQ[1:12,], by="Year")
CombMeanMortF2145 <- data.frame(rbind(F21MeanMort,CombMeanMortF2145))
#Graph
highlight <- CombMeanMortF2145[19:30,]
F21Mort45Plot<- ggplot(data = CombMeanMortF2145, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F23 Group
FutureMortF2345 <- t(data.frame(apply(F23MortPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23MortQ <- map(F23MortPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23MortQ <- map_dfr(F23MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23MortMean45 <- data.frame(FutureMortF2345[,(seq(1,36,3))])
F23MortMean45$Year <- seq(2025,2080,5)
F23MortMean45 <- F23MortMean45[,2:1]
names(F23MortMean45)[2] <- "MortRate"
#separate quantiles
F23MortQ[,1]<-seq(2025,2080,5)
names(F23MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2345 <- merge(F23MortMean45[1:12,],F23MortQ[1:12,], by="Year")
CombMeanMortF2345 <- data.frame(rbind(F23MeanMort,CombMeanMortF2345))
#Graph
highlight <- CombMeanMortF2345[19:30,]
F23Mort45Plot<- ggplot(data = CombMeanMortF2345, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2345, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2345, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F24 Group
FutureMortF2445 <- t(data.frame(apply(F24MortPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24MortQ <- map(F24MortPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24MortQ <- map_dfr(F24MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24MortMean45 <- data.frame(FutureMortF2445[,(seq(1,36,3))])
F24MortMean45$Year <- seq(2025,2080,5)
F24MortMean45 <- F24MortMean45[,2:1]
names(F24MortMean45)[2] <- "MortRate"
#separate quantiles
F24MortQ[,1]<-seq(2025,2080,5)
names(F24MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2445 <- merge(F24MortMean45[1:12,],F24MortQ[1:12,], by="Year")
CombMeanMortF2445 <- data.frame(rbind(F24MeanMort,CombMeanMortF2445))
#Graph
highlight <- CombMeanMortF2445[19:30,]
F24Mort45Plot<- ggplot(data = CombMeanMortF2445, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2445, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2445, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#RCP85
#F1 Group
FutureMortF185 <- t(data.frame(apply(F1MortPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1MortQ <- map(F1MortPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1MortQ <- map_dfr(F1MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1MortMean85 <- data.frame(FutureMortF185[,(seq(1,36,3))])
F1MortMean85$Year <- seq(2025,2080,5)
F1MortMean85 <- F1MortMean85[,2:1]
names(F1MortMean85)[2] <- "MortRate"
#separate quantiles
F1MortQ[,1]<-seq(2025,2080,5)
names(F1MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF185 <- merge(F1MortMean85[1:12,],F1MortQ[1:12,], by="Year")
CombMeanMortF185 <- data.frame(rbind(F1MeanMort,CombMeanMortF185))
#Graph
highlight <- CombMeanMortF185[19:30,]
F1Mort85Plot<- ggplot(data = CombMeanMortF185, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F5 Group
FutureMortF585 <- t(data.frame(apply(F5MortPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5MortQ <- map(F5MortPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5MortQ <- map_dfr(F5MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5MortMean85 <- data.frame(FutureMortF585[,(seq(1,36,3))])
F5MortMean85$Year <- seq(2025,2080,5)
F5MortMean85 <- F5MortMean85[,2:1]
names(F5MortMean85)[2] <- "MortRate"
#separate quantiles
F5MortQ[,1]<-seq(2025,2080,5)
names(F5MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF585 <- merge(F5MortMean85[1:12,],F5MortQ[1:12,], by="Year")
CombMeanMortF585 <- data.frame(rbind(F5MeanMort,CombMeanMortF585))
#Graph
highlight <- CombMeanMortF585[18:29,]
F5Mort85Plot<- ggplot(data = CombMeanMortF585[2:29,], aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF585, row_number() %in% 2:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF585, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F20 Group
FutureMortF2085 <- t(data.frame(apply(F20MortPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20MortQ <- map(F20MortPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20MortQ <- map_dfr(F20MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20MortMean85 <- data.frame(FutureMortF2085[,(seq(1,36,3))])
F20MortMean85$Year <- seq(2025,2080,5)
F20MortMean85 <- F20MortMean85[,2:1]
names(F20MortMean85)[2] <- "MortRate"
#separate quantiles
F20MortQ[,1]<-seq(2025,2080,5)
names(F20MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2085 <- merge(F20MortMean85[1:12,],F20MortQ[1:12,], by="Year")
CombMeanMortF2085 <- data.frame(rbind(F20MeanMort,CombMeanMortF2085))
#Graph
highlight <- CombMeanMortF2085[19:30,]
F20Mort85Plot<- ggplot(data = CombMeanMortF2085, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2085, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2085, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 


#F21 Group
FutureMortF2185 <- t(data.frame(apply(F21MortPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21MortQ <- map(F21MortPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21MortQ <- map_dfr(F21MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21MortMean85 <- data.frame(FutureMortF2185[,(seq(1,36,3))])
F21MortMean85$Year <- seq(2025,2080,5)
F21MortMean85 <- F21MortMean85[,2:1]
names(F21MortMean85)[2] <- "MortRate"
#separate quantiles
F21MortQ[,1]<-seq(2025,2080,5)
names(F21MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2185 <- merge(F21MortMean85[1:12,],F21MortQ[1:12,], by="Year")
CombMeanMortF2185 <- data.frame(rbind(F21MeanMort,CombMeanMortF2185))
#Graph
highlight <- CombMeanMortF2185[19:30,]
F21Mort85Plot<- ggplot(data = CombMeanMortF2185, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F23 Group
FutureMortF2385 <- t(data.frame(apply(F23MortPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23MortQ <- map(F23MortPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23MortQ <- map_dfr(F23MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23MortMean85 <- data.frame(FutureMortF2385[,(seq(1,36,3))])
F23MortMean85$Year <- seq(2025,2080,5)
F23MortMean85 <- F23MortMean85[,2:1]
names(F23MortMean85)[2] <- "MortRate"
#separate quantiles
F23MortQ[,1]<-seq(2025,2080,5)
names(F23MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2385 <- merge(F23MortMean85[1:12,],F23MortQ[1:12,], by="Year")
CombMeanMortF2385 <- data.frame(rbind(F23MeanMort,CombMeanMortF2385))
#Graph
highlight <- CombMeanMortF2385[19:30,]
F23Mort85Plot<- ggplot(data = CombMeanMortF2385, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2385, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2385, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#F24 Group
FutureMortF2485 <- t(data.frame(apply(F24MortPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24MortQ <- map(F24MortPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24MortQ <- map_dfr(F24MortQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24MortMean85 <- data.frame(FutureMortF2485[,(seq(1,36,3))])
F24MortMean85$Year <- seq(2025,2080,5)
F24MortMean85 <- F24MortMean85[,2:1]
names(F24MortMean85)[2] <- "MortRate"
#separate quantiles
F24MortQ[,1]<-seq(2025,2080,5)
names(F24MortQ)<-c("Year","Q5","Q95")
#combine data
CombMeanMortF2485 <- merge(F24MortMean85[1:12,],F24MortQ[1:12,], by="Year")
CombMeanMortF2485 <- data.frame(rbind(F24MeanMort,CombMeanMortF2485))
#Graph
highlight <- CombMeanMortF2485[19:30,]
F24Mort85Plot<- ggplot(data = CombMeanMortF2485, aes(x=Year,y=MortRate))+ 
   annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Mortality Rate") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanMortF2485, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanMortF2485, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MortRate), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic() 

#create multiplots
F1CombMort <- F1Mort26Plot/F1Mort45Plot/F1Mort85Plot
F1CombMort + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F5CombMort <- F5Mort26Plot/F5Mort45Plot/F5Mort85Plot
F5CombMort + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F20CombMort <- F20Mort26Plot/F20Mort45Plot/F20Mort85Plot
F20CombMort + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F21CombMort <- F21Mort26Plot/F21Mort45Plot/F21Mort85Plot
F21CombMort + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F23CombMort <- F23Mort26Plot/F23Mort45Plot/F23Mort85Plot
F23CombMort + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F24CombMort <- F24Mort26Plot/F24Mort45Plot/F24Mort85Plot
F24CombMort + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))

#AQI
datAQISplit1 <- list(
  AQI = scale(as.numeric(FgroupSplitTrain[["1"]]$MaxAQI)),
  PPT = scale(as.numeric(FgroupSplitTrain[["1"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplitTrain[["1"]]$MAT)),
  FAD = as.integer(FgroupSplitTrain[["1"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplitTrain[["1"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplitTrain[["1"]]$ForestID)),
  Elevation = scale(FgroupSplitTrain[["1"]]$Elevation),
  RHUM = scale(FgroupSplitTrain[["1"]]$RHUM),
  RAD = scale(FgroupSplitTrain[["1"]]$RAD)
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
  RAD = scale(FgroupSplitTrain[["5"]]$RAD)
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
  RAD = scale(FgroupSplitTrain[["20"]]$RAD)
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
  RAD = scale(FgroupSplitTrain[["21"]]$RAD)
)
datAQISplit23 <- list(
  AQI = scale(as.numeric(FgroupSplit[["23"]]$MaxAQI)),
  PPT = scale(as.numeric(FgroupSplit[["23"]]$PPT)),
  MAT = scale(as.numeric(FgroupSplit[["23"]]$MAT)),
  FAD = as.integer(FgroupSplit[["23"]]$FADClass),
  Soil = as.integer(as.factor(FgroupSplit[["23"]]$SoilID)),
  FType = as.integer(as.factor(FgroupSplit[["23"]]$ForestID)),
  Elevation = scale(FgroupSplit[["23"]]$Elevation),
  RHUM = scale(FgroupSplit[["23"]]$RHUM),
  RAD = scale(FgroupSplit[["23"]]$RAD)
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
  RAD = scale(FgroupSplitTrain[["24"]]$RAD)
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

#Calculate and visualize annual means
#F1 Group RCP26
F1MeanAQI <- F1Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F1Q5AQI <- F1Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F1Q5AQI)[2] <- "Q5"
F1Q95AQI <- F1Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F1Q95AQI)[2] <- "Q95"
F1QAQI <- merge(F1Q5AQI,F1Q95AQI, by="Year")
F1MeanAQI <- merge(F1MeanAQI,F1QAQI, by="Year")
names(F1MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F1AQIPred26[,2:37] <- (F1AQIPred26[,2:37]*scaleAQI1)+centerAQI1
FutureAQIF126 <- t(data.frame(apply(F1AQIPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1AQIQ <- map(F1AQIPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1AQIQ <- map_dfr(F1AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1AQIMean26 <- data.frame(FutureAQIF126[,(seq(1,36,3))])
F1AQIMean26$Year <- seq(2025,2080,5)
F1AQIMean26 <- F1AQIMean26[,2:1]
names(F1AQIMean26)[2] <- "MeanAQIPerAcre"
#separate quantiles
F1AQIQ[,1]<-seq(2025,2080,5)
names(F1AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF126 <- merge(F1AQIMean26[1:12,],F1AQIQ[1:12,], by="Year")
CombMeanAQIF126 <- data.frame(rbind(F1MeanAQI,CombMeanAQIF126))
#Graph
highlight <- CombMeanAQIF126[19:30,]
F1AQI26Plot<- ggplot(data = CombMeanAQIF126, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F5 Group
F5MeanAQI <- F5Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F5Q5AQI <- F5Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F5Q5AQI)[2] <- "Q5"
F5Q95AQI <- F5Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F5Q95AQI)[2] <- "Q95"
F5QAQI <- merge(F5Q5AQI,F5Q95AQI, by="Year")
F5MeanAQI <- merge(F5MeanAQI,F5QAQI, by="Year")
names(F5MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F5AQIPred26[,2:37] <- (F5AQIPred26[,2:37]*scaleAQI5)+centerAQI5
FutureAQIF526 <- t(data.frame(apply(F5AQIPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5AQIQ <- map(F5AQIPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5AQIQ <- map_dfr(F5AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5AQIMean26 <- data.frame(FutureAQIF526[,(seq(1,36,3))])
F5AQIMean26$Year <- seq(2025,2080,5)
F5AQIMean26 <- F5AQIMean26[,2:1]
names(F5AQIMean26)[2] <- "MeanAQIPerAcre"
#separate quantiles
F5AQIQ[,1]<-seq(2025,2080,5)
names(F5AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF526 <- merge(F5AQIMean26[1:12,],F5AQIQ[1:12,], by="Year")
CombMeanAQIF526 <- data.frame(rbind(F5MeanAQI,CombMeanAQIF526))
#Graph
highlight <- CombMeanAQIF526[18:29,]
F5AQI26Plot<- ggplot(data = CombMeanAQIF526, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF526, row_number() %in% 1:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF526, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F20 Group
F20MeanAQI <- F20Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F20Q5AQI <- F20Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F20Q5AQI)[2] <- "Q5"
F20Q95AQI <- F20Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F20Q95AQI)[2] <- "Q95"
F20QAQI <- merge(F20Q5AQI,F20Q95AQI, by="Year")
F20MeanAQI <- merge(F20MeanAQI,F20QAQI, by="Year")
names(F20MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F20AQIPred26[,2:37] <- (F20AQIPred26[,2:37]*scaleAQI20)+centerAQI20
FutureAQIF2026 <- t(data.frame(apply(F20AQIPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20AQIQ <- map(F20AQIPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20AQIQ <- map_dfr(F20AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20AQIMean26 <- data.frame(FutureAQIF2026[,(seq(1,36,3))])
F20AQIMean26$Year <- seq(2025,2080,5)
F20AQIMean26 <- F20AQIMean26[,2:1]
names(F20AQIMean26)[2] <- "MeanAQIPerAcre"
#separate quantiles
F20AQIQ[,1]<-seq(2025,2080,5)
names(F20AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2026 <- merge(F20AQIMean26[1:12,],F20AQIQ[1:12,], by="Year")
CombMeanAQIF2026 <- data.frame(rbind(F20MeanAQI,CombMeanAQIF2026))
#Graph
highlight <- CombMeanAQIF2026[19:30,]
F20AQI26Plot<- ggplot(data = CombMeanAQIF2026, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2026, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2026, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F21 Group
F21MeanAQI <- F21Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F21Q5AQI <- F21Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F21Q5AQI)[2] <- "Q5"
F21Q95AQI <- F21Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F21Q95AQI)[2] <- "Q95"
F21QAQI <- merge(F21Q5AQI,F21Q95AQI, by="Year")
F21MeanAQI <- merge(F21MeanAQI,F21QAQI, by="Year")
names(F21MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F21AQIPred26[,2:37] <- (F21AQIPred26[,2:37]*scaleAQI21)+centerAQI21
FutureAQIF2126 <- t(data.frame(apply(F21AQIPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21AQIQ <- map(F21AQIPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21AQIQ <- map_dfr(F21AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21AQIMean26 <- data.frame(FutureAQIF2126[,(seq(1,36,3))])
F21AQIMean26$Year <- seq(2025,2080,5)
F21AQIMean26 <- F21AQIMean26[,2:1]
names(F21AQIMean26)[2] <- "MeanAQIPerAcre"
#separate quantiles
F21AQIQ[,1]<-seq(2025,2080,5)
names(F21AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2126 <- merge(F21AQIMean26[1:12,],F21AQIQ[1:12,], by="Year")
CombMeanAQIF2126 <- data.frame(rbind(F21MeanAQI,CombMeanAQIF2126))
#Graph
highlight <- CombMeanAQIF2126[19:30,]
F21AQI26Plot<- ggplot(data = CombMeanAQIF2126, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2126, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2126, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F23 Group
F23MeanAQI <- F23Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F23Q5AQI <- F23Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F23Q5AQI)[2] <- "Q5"
F23Q95AQI <- F23Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F23Q95AQI)[2] <- "Q95"
F23QAQI <- merge(F23Q5AQI,F23Q95AQI, by="Year")
F23MeanAQI <- merge(F23MeanAQI,F23QAQI, by="Year")
names(F23MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F23AQIPred26[,2:37] <- (F23AQIPred26[,2:37]*scaleAQI23)+centerAQI23
FutureAQIF2326 <- t(data.frame(apply(F23AQIPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23AQIQ <- map(F23AQIPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23AQIQ <- map_dfr(F23AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23AQIMean26 <- data.frame(FutureAQIF2326[,(seq(1,36,3))])
F23AQIMean26$Year <- seq(2025,2080,5)
F23AQIMean26 <- F23AQIMean26[,2:1]
names(F23AQIMean26)[2] <- "MeanAQIPerAcre"
#separate quantiles
F23AQIQ[,1]<-seq(2025,2080,5)
names(F23AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2326 <- merge(F23AQIMean26[1:12,],F23AQIQ[1:12,], by="Year")
CombMeanAQIF2326 <- data.frame(rbind(F23MeanAQI,CombMeanAQIF2326))
#Graph
highlight <- CombMeanAQIF2326[19:30,]
F23AQI26Plot<- ggplot(data = CombMeanAQIF2326, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2326, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2326, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F24 Group
F24MeanAQI <- F24Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F24Q5AQI <- F24Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F24Q5AQI)[2] <- "Q5"
F24Q95AQI <- F24Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F24Q95AQI)[2] <- "Q95"
F24QAQI <- merge(F24Q5AQI,F24Q95AQI, by="Year")
F24MeanAQI <- merge(F24MeanAQI,F24QAQI, by="Year")
names(F24MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F24AQIPred26[,2:37] <- (F24AQIPred26[,2:37]*scaleAQI24)+centerAQI24
FutureAQIF2426 <- t(data.frame(apply(F24AQIPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24AQIQ <- map(F24AQIPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24AQIQ <- map_dfr(F24AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24AQIMean26 <- data.frame(FutureAQIF2426[,(seq(1,36,3))])
F24AQIMean26$Year <- seq(2025,2080,5)
F24AQIMean26 <- F24AQIMean26[,2:1]
names(F24AQIMean26)[2] <- "MeanAQIPerAcre"
#separate quantiles
F24AQIQ[,1]<-seq(2025,2080,5)
names(F24AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2426 <- merge(F24AQIMean26[1:12,],F24AQIQ[1:12,], by="Year")
CombMeanAQIF2426 <- data.frame(rbind(F24MeanAQI,CombMeanAQIF2426))
#Graph
highlight <- CombMeanAQIF2426[19:30,]
F24AQI26Plot<- ggplot(data = CombMeanAQIF2426, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2426, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2426, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F1 Group RCP45
#F1 Group
F1MeanAQI <- F1Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F1Q5AQI <- F1Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F1Q5AQI)[2] <- "Q5"
F1Q95AQI <- F1Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F1Q95AQI)[2] <- "Q95"
F1QAQI <- merge(F1Q5AQI,F1Q95AQI, by="Year")
F1MeanAQI <- merge(F1MeanAQI,F1QAQI, by="Year")
names(F1MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F1AQIPred45[,2:37] <- (F1AQIPred45[,2:37]*scaleAQI1)+centerAQI1
FutureAQIF145 <- t(data.frame(apply(F1AQIPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1AQIQ <- map(F1AQIPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1AQIQ <- map_dfr(F1AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1AQIMean45 <- data.frame(FutureAQIF145[,(seq(1,36,3))])
F1AQIMean45$Year <- seq(2025,2080,5)
F1AQIMean45 <- F1AQIMean45[,2:1]
names(F1AQIMean45)[2] <- "MeanAQIPerAcre"
#separate quantiles
F1AQIQ[,1]<-seq(2025,2080,5)
names(F1AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF145 <- merge(F1AQIMean45[1:12,],F1AQIQ[1:12,], by="Year")
CombMeanAQIF145 <- data.frame(rbind(F1MeanAQI,CombMeanAQIF145))
#Graph
highlight <- CombMeanAQIF145[19:30,]
F1AQI45Plot<- ggplot(data = CombMeanAQIF145, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F5 Group
F5MeanAQI <- F5Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F5Q5AQI <- F5Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F5Q5AQI)[2] <- "Q5"
F5Q95AQI <- F5Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F5Q95AQI)[2] <- "Q95"
F5QAQI <- merge(F5Q5AQI,F5Q95AQI, by="Year")
F5MeanAQI <- merge(F5MeanAQI,F5QAQI, by="Year")
names(F5MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F5AQIPred45[,2:37] <- (F5AQIPred45[,2:37]*scaleAQI5)+centerAQI5
FutureAQIF545 <- t(data.frame(apply(F5AQIPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5AQIQ <- map(F5AQIPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5AQIQ <- map_dfr(F5AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5AQIMean45 <- data.frame(FutureAQIF545[,(seq(1,36,3))])
F5AQIMean45$Year <- seq(2025,2080,5)
F5AQIMean45 <- F5AQIMean45[,2:1]
names(F5AQIMean45)[2] <- "MeanAQIPerAcre"
#separate quantiles
F5AQIQ[,1]<-seq(2025,2080,5)
names(F5AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF545 <- merge(F5AQIMean45[1:12,],F5AQIQ[1:12,], by="Year")
CombMeanAQIF545 <- data.frame(rbind(F5MeanAQI,CombMeanAQIF545))
#Graph
highlight <- CombMeanAQIF545[18:29,]
F5AQI45Plot<- ggplot(data = CombMeanAQIF545, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF545, row_number() %in% 1:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF545, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F20 Group
F20MeanAQI <- F20Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F20Q5AQI <- F20Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F20Q5AQI)[2] <- "Q5"
F20Q95AQI <- F20Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F20Q95AQI)[2] <- "Q95"
F20QAQI <- merge(F20Q5AQI,F20Q95AQI, by="Year")
F20MeanAQI <- merge(F20MeanAQI,F20QAQI, by="Year")
names(F20MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F20AQIPred45[,2:37] <- (F20AQIPred45[,2:37]*scaleAQI20)+centerAQI20
FutureAQIF2045 <- t(data.frame(apply(F20AQIPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20AQIQ <- map(F20AQIPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20AQIQ <- map_dfr(F20AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20AQIMean45 <- data.frame(FutureAQIF2045[,(seq(1,36,3))])
F20AQIMean45$Year <- seq(2025,2080,5)
F20AQIMean45 <- F20AQIMean45[,2:1]
names(F20AQIMean45)[2] <- "MeanAQIPerAcre"
#separate quantiles
F20AQIQ[,1]<-seq(2025,2080,5)
names(F20AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2045 <- merge(F20AQIMean45[1:12,],F20AQIQ[1:12,], by="Year")
CombMeanAQIF2045 <- data.frame(rbind(F20MeanAQI,CombMeanAQIF2045))
#Graph
highlight <- CombMeanAQIF2045[19:30,]
F20AQI45Plot<- ggplot(data = CombMeanAQIF2045, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2045, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2045, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F21 Group
F21MeanAQI <- F21Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F21Q5AQI <- F21Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F21Q5AQI)[2] <- "Q5"
F21Q95AQI <- F21Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F21Q95AQI)[2] <- "Q95"
F21QAQI <- merge(F21Q5AQI,F21Q95AQI, by="Year")
F21MeanAQI <- merge(F21MeanAQI,F21QAQI, by="Year")
names(F21MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F21AQIPred45[,2:37] <- (F21AQIPred45[,2:37]*scaleAQI21)+centerAQI21
FutureAQIF2145 <- t(data.frame(apply(F21AQIPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21AQIQ <- map(F21AQIPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21AQIQ <- map_dfr(F21AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21AQIMean45 <- data.frame(FutureAQIF2145[,(seq(1,36,3))])
F21AQIMean45$Year <- seq(2025,2080,5)
F21AQIMean45 <- F21AQIMean45[,2:1]
names(F21AQIMean45)[2] <- "MeanAQIPerAcre"
#separate quantiles
F21AQIQ[,1]<-seq(2025,2080,5)
names(F21AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2145 <- merge(F21AQIMean45[1:12,],F21AQIQ[1:12,], by="Year")
CombMeanAQIF2145 <- data.frame(rbind(F21MeanAQI,CombMeanAQIF2145))
#Graph
highlight <- CombMeanAQIF2145[19:30,]
F21AQI45Plot<- ggplot(data = CombMeanAQIF2145, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2145, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2145, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F23 Group
F23MeanAQI <- F23Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F23Q5AQI <- F23Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F23Q5AQI)[2] <- "Q5"
F23Q95AQI <- F23Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F23Q95AQI)[2] <- "Q95"
F23QAQI <- merge(F23Q5AQI,F23Q95AQI, by="Year")
F23MeanAQI <- merge(F23MeanAQI,F23QAQI, by="Year")
names(F23MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F23AQIPred45[,2:37] <- (F23AQIPred45[,2:37]*scaleAQI23)+centerAQI23
FutureAQIF2345 <- t(data.frame(apply(F23AQIPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23AQIQ <- map(F23AQIPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23AQIQ <- map_dfr(F23AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23AQIMean45 <- data.frame(FutureAQIF2345[,(seq(1,36,3))])
F23AQIMean45$Year <- seq(2025,2080,5)
F23AQIMean45 <- F23AQIMean45[,2:1]
names(F23AQIMean45)[2] <- "MeanAQIPerAcre"
#separate quantiles
F23AQIQ[,1]<-seq(2025,2080,5)
names(F23AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2345 <- merge(F23AQIMean45[1:12,],F23AQIQ[1:12,], by="Year")
CombMeanAQIF2345 <- data.frame(rbind(F23MeanAQI,CombMeanAQIF2345))
#Graph
highlight <- CombMeanAQIF2345[19:30,]
F23AQI45Plot<- ggplot(data = CombMeanAQIF2345, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2345, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2345, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F24 Group
F24MeanAQI <- F24Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F24Q5AQI <- F24Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F24Q5AQI)[2] <- "Q5"
F24Q95AQI <- F24Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F24Q95AQI)[2] <- "Q95"
F24QAQI <- merge(F24Q5AQI,F24Q95AQI, by="Year")
F24MeanAQI <- merge(F24MeanAQI,F24QAQI, by="Year")
names(F24MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F24AQIPred45[,2:37] <- (F24AQIPred45[,2:37]*scaleAQI24)+centerAQI24
FutureAQIF2445 <- t(data.frame(apply(F24AQIPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24AQIQ <- map(F24AQIPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24AQIQ <- map_dfr(F24AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24AQIMean45 <- data.frame(FutureAQIF2445[,(seq(1,36,3))])
F24AQIMean45$Year <- seq(2025,2080,5)
F24AQIMean45 <- F24AQIMean45[,2:1]
names(F24AQIMean45)[2] <- "MeanAQIPerAcre"
#separate quantiles
F24AQIQ[,1]<-seq(2025,2080,5)
names(F24AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2445 <- merge(F24AQIMean45[1:12,],F24AQIQ[1:12,], by="Year")
CombMeanAQIF2445 <- data.frame(rbind(F24MeanAQI,CombMeanAQIF2445))
#Graph
highlight <- CombMeanAQIF2445[19:30,]
F24AQI45Plot<- ggplot(data = CombMeanAQIF2445, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2445, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2445, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#RCP85
#F1 Group
F1MeanAQI <- F1Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F1Q5AQI <- F1Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F1Q5AQI)[2] <- "Q5"
F1Q95AQI <- F1Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F1Q95AQI)[2] <- "Q95"
F1QAQI <- merge(F1Q5AQI,F1Q95AQI, by="Year")
F1MeanAQI <- merge(F1MeanAQI,F1QAQI, by="Year")
names(F1MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F1AQIPred85[,2:37] <- (F1AQIPred85[,2:37]*scaleAQI1)+centerAQI1
FutureAQIF185 <- t(data.frame(apply(F1AQIPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F1AQIQ <- map(F1AQIPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F1AQIQ <- map_dfr(F1AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F1AQIMean85 <- data.frame(FutureAQIF185[,(seq(1,36,3))])
F1AQIMean85$Year <- seq(2025,2080,5)
F1AQIMean85 <- F1AQIMean85[,2:1]
names(F1AQIMean85)[2] <- "MeanAQIPerAcre"
#separate quantiles
F1AQIQ[,1]<-seq(2025,2080,5)
names(F1AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF185 <- merge(F1AQIMean85[1:12,],F1AQIQ[1:12,], by="Year")
CombMeanAQIF185 <- data.frame(rbind(F1MeanAQI,CombMeanAQIF185))
#Graph
highlight <- CombMeanAQIF185[19:30,]
F1AQI85Plot<- ggplot(data = CombMeanAQIF185, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F5 Group
F5MeanAQI <- F5Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F5Q5AQI <- F5Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F5Q5AQI)[2] <- "Q5"
F5Q95AQI <- F5Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F5Q95AQI)[2] <- "Q95"
F5QAQI <- merge(F5Q5AQI,F5Q95AQI, by="Year")
F5MeanAQI <- merge(F5MeanAQI,F5QAQI, by="Year")
names(F5MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F5AQIPred85[,2:37] <- (F5AQIPred85[,2:37]*scaleAQI5)+centerAQI5
FutureAQIF585 <- t(data.frame(apply(F5AQIPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F5AQIQ <- map(F5AQIPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F5AQIQ <- map_dfr(F5AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F5AQIMean85 <- data.frame(FutureAQIF585[,(seq(1,36,3))])
F5AQIMean85$Year <- seq(2025,2080,5)
F5AQIMean85 <- F5AQIMean85[,2:1]
names(F5AQIMean85)[2] <- "MeanAQIPerAcre"
#separate quantiles
F5AQIQ[,1]<-seq(2025,2080,5)
names(F5AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF585 <- merge(F5AQIMean85[1:12,],F5AQIQ[1:12,], by="Year")
CombMeanAQIF585 <- data.frame(rbind(F5MeanAQI,CombMeanAQIF585))
#Graph
highlight <- CombMeanAQIF585[18:29,]
F5AQI85Plot<- ggplot(data = CombMeanAQIF585, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF585, row_number() %in% 1:17), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF585, row_number() %in% 18:29), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F20 Group
F20MeanAQI <- F20Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F20Q5AQI <- F20Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F20Q5AQI)[2] <- "Q5"
F20Q95AQI <- F20Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F20Q95AQI)[2] <- "Q95"
F20QAQI <- merge(F20Q5AQI,F20Q95AQI, by="Year")
F20MeanAQI <- merge(F20MeanAQI,F20QAQI, by="Year")
names(F20MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F20AQIPred85[,2:37] <- (F20AQIPred85[,2:37]*scaleAQI20)+centerAQI20
FutureAQIF2085 <- t(data.frame(apply(F20AQIPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F20AQIQ <- map(F20AQIPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F20AQIQ <- map_dfr(F20AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F20AQIMean85 <- data.frame(FutureAQIF2085[,(seq(1,36,3))])
F20AQIMean85$Year <- seq(2025,2080,5)
F20AQIMean85 <- F20AQIMean85[,2:1]
names(F20AQIMean85)[2] <- "MeanAQIPerAcre"
#separate quantiles
F20AQIQ[,1]<-seq(2025,2080,5)
names(F20AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2085 <- merge(F20AQIMean85[1:12,],F20AQIQ[1:12,], by="Year")
CombMeanAQIF2085 <- data.frame(rbind(F20MeanAQI,CombMeanAQIF2085))
#Graph
highlight <- CombMeanAQIF2085[19:30,]
F20AQI85Plot<- ggplot(data = CombMeanAQIF2085, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2085, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2085, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#F21 Group
F21MeanAQI <- F21Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F21Q5AQI <- F21Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F21Q5AQI)[2] <- "Q5"
F21Q95AQI <- F21Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F21Q95AQI)[2] <- "Q95"
F21QAQI <- merge(F21Q5AQI,F21Q95AQI, by="Year")
F21MeanAQI <- merge(F21MeanAQI,F21QAQI, by="Year")
names(F21MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F21AQIPred85[,2:37] <- (F21AQIPred85[,2:37]*scaleAQI21)+centerAQI21
FutureAQIF2185 <- t(data.frame(apply(F21AQIPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F21AQIQ <- map(F21AQIPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F21AQIQ <- map_dfr(F21AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F21AQIMean85 <- data.frame(FutureAQIF2185[,(seq(1,36,3))])
F21AQIMean85$Year <- seq(2025,2080,5)
F21AQIMean85 <- F21AQIMean85[,2:1]
names(F21AQIMean85)[2] <- "MeanAQIPerAcre"
#separate quantiles
F21AQIQ[,1]<-seq(2025,2080,5)
names(F21AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2185 <- merge(F21AQIMean85[1:12,],F21AQIQ[1:12,], by="Year")
CombMeanAQIF2185 <- data.frame(rbind(F21MeanAQI,CombMeanAQIF2185))
#Graph
highlight <- CombMeanAQIF2185[19:30,]
F21AQI85Plot<- ggplot(data = CombMeanAQIF2185, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2185, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2185, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F23 Group
F23MeanAQI <- F23Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F23Q5AQI <- F23Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F23Q5AQI)[2] <- "Q5"
F23Q95AQI <- F23Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F23Q95AQI)[2] <- "Q95"
F23QAQI <- merge(F23Q5AQI,F23Q95AQI, by="Year")
F23MeanAQI <- merge(F23MeanAQI,F23QAQI, by="Year")
names(F23MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F23AQIPred85[,2:37] <- (F23AQIPred85[,2:37]*scaleAQI23)+centerAQI23
FutureAQIF2385 <- t(data.frame(apply(F23AQIPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F23AQIQ <- map(F23AQIPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F23AQIQ <- map_dfr(F23AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F23AQIMean85 <- data.frame(FutureAQIF2385[,(seq(1,36,3))])
F23AQIMean85$Year <- seq(2025,2080,5)
F23AQIMean85 <- F23AQIMean85[,2:1]
names(F23AQIMean85)[2] <- "MeanAQIPerAcre"
#separate quantiles
F23AQIQ[,1]<-seq(2025,2080,5)
names(F23AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2385 <- merge(F23AQIMean85[1:12,],F23AQIQ[1:12,], by="Year")
CombMeanAQIF2385 <- data.frame(rbind(F23MeanAQI,CombMeanAQIF2385))
#Graph
highlight <- CombMeanAQIF2385[19:30,]
F23AQI85Plot<- ggplot(data = CombMeanAQIF2385, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2385, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2385, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

#F24 Group
F24MeanAQI <- F24Re %>% group_by(Year)%>%summarise(mean(MaxAQI))
F24Q5AQI <- F24Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.05))
names(F24Q5AQI)[2] <- "Q5"
F24Q95AQI <- F24Re %>% group_by(Year)%>%reframe(quantile(MaxAQI, prob=0.95))                                                                                               
names(F24Q95AQI)[2] <- "Q95"
F24QAQI <- merge(F24Q5AQI,F24Q95AQI, by="Year")
F24MeanAQI <- merge(F24MeanAQI,F24QAQI, by="Year")
names(F24MeanAQI)<- c("Year","MeanAQIPerAcre","Q5","Q95")
#future values
#unscale future values
F24AQIPred85[,2:37] <- (F24AQIPred85[,2:37]*scaleAQI24)+centerAQI24
FutureAQIF2485 <- t(data.frame(apply(F24AQIPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
F24AQIQ <- map(F24AQIPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
F24AQIQ <- map_dfr(F24AQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
F24AQIMean85 <- data.frame(FutureAQIF2485[,(seq(1,36,3))])
F24AQIMean85$Year <- seq(2025,2080,5)
F24AQIMean85 <- F24AQIMean85[,2:1]
names(F24AQIMean85)[2] <- "MeanAQIPerAcre"
#separate quantiles
F24AQIQ[,1]<-seq(2025,2080,5)
names(F24AQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanAQIF2485 <- merge(F24AQIMean85[1:12,],F24AQIQ[1:12,], by="Year")
CombMeanAQIF2485 <- data.frame(rbind(F24MeanAQI,CombMeanAQIF2485))
#Graph
highlight <- CombMeanAQIF2485[19:30,]
F24AQI85Plot<- ggplot(data = CombMeanAQIF2485, aes(x=Year,y=MeanAQIPerAcre))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Air Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanAQIF2485, row_number() %in% 1:18), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanAQIF2485, row_number() %in% 19:30), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=MeanAQIPerAcre), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

F1CombAQI <- F1AQI26Plot/F1AQI45Plot/F1AQI85Plot
F1CombAQI + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F5CombAQI <- F5AQI26Plot/F5AQI45Plot/F5AQI85Plot
F5CombAQI + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F20CombAQI <- F20AQI26Plot/F20AQI45Plot/F20AQI85Plot
F20CombAQI + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F21CombAQI <- F21AQI26Plot/F21AQI45Plot/F21AQI85Plot
F21CombAQI + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F23CombAQI <- F23AQI26Plot/F23AQI45Plot/F23AQI85Plot
F23CombAQI + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))
F24CombAQI <- F24AQI26Plot/F24AQI45Plot/F24AQI85Plot
F24CombAQI + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))

#SQI
#no scale 
#calculate means and visualize
MeanSQI <- SQIdata %>% group_by(Year)%>%summarise(mean(SQIPCT))
Q5SQI <- SQIdata %>% group_by(Year)%>%reframe(quantile(SQIPCT, prob=0.05))
names(Q5SQI)[2] <- "Q5"
Q95SQI <- SQIdata %>% group_by(Year)%>%reframe(quantile(SQIPCT, prob=0.95))                                                                                               
names(Q95SQI)[2] <- "Q95"
QSQI <- merge(Q5SQI,Q95SQI, by="Year")
MeanSQI <- merge(MeanSQI,QSQI, by="Year")
names(MeanSQI)<- c("Year","SQIPCT","Q5","Q95")
MeanSQI[,2:4] <- MeanSQI[,2:4]/100
FutureSQI26 <- t(data.frame(apply(SQIPred26[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
SQIQ <- map(SQIPred26[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
SQIQ <- map_dfr(SQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
SQIMean26 <- data.frame(FutureSQI26[,(seq(1,36,3))])
SQIMean26$Year <- seq(2025,2080,5)
SQIMean26 <- SQIMean26[,2:1]
names(SQIMean26)[2] <- "SQIPCT"
#separate quantiles
SQIQ[,1]<-seq(2025,2080,5)
names(SQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanSQI26 <- merge(SQIMean26[1:12,],SQIQ[1:12,], by="Year")
CombMeanSQI26 <- data.frame(rbind(MeanSQI,CombMeanSQI26))
#Graph
highlight <- CombMeanSQI26[7:18,]
SQI26Plot<- ggplot(data = CombMeanSQI26, aes(x=Year,y=SQIPCT))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 2.6",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Soil Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanSQI26, row_number() %in% 1:6), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanSQI26, row_number() %in% 7:18), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=SQIPCT), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()


#45
FutureSQI45 <- t(data.frame(apply(SQIPred45[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
SQIQ <- map(SQIPred45[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
SQIQ <- map_dfr(SQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
SQIMean45 <- data.frame(FutureSQI45[,(seq(1,36,3))])
SQIMean45$Year <- seq(2025,2080,5)
SQIMean45 <- SQIMean45[,2:1]
names(SQIMean45)[2] <- "SQIPCT"
#separate quantiles
SQIQ[,1]<-seq(2025,2080,5)
names(SQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanSQI45 <- merge(SQIMean45[1:12,],SQIQ[1:12,], by="Year")
CombMeanSQI45 <- data.frame(rbind(MeanSQI,CombMeanSQI45))
#Graph
highlight <- CombMeanSQI45[7:18,]
SQI45Plot<- ggplot(data = CombMeanSQI45, aes(x=Year,y=SQIPCT))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 4.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Soil Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanSQI45, row_number() %in% 1:6), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanSQI45, row_number() %in% 7:18), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=SQIPCT), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()



#85
FutureSQI85 <- t(data.frame(apply(SQIPred85[,2:37],2,mean)))
qcols <- seq(2, 37, by = 3)
SQIQ <- map(SQIPred85[, qcols], ~ quantile(.x, probs = c(0.05, 0.95), na.rm = TRUE))
SQIQ <- map_dfr(SQIQ, ~ as.data.frame(t(.x)), .id = "column")
#separate means from CIs
SQIMean85 <- data.frame(FutureSQI85[,(seq(1,36,3))])
SQIMean85$Year <- seq(2025,2080,5)
SQIMean85 <- SQIMean85[,2:1]
names(SQIMean85)[2] <- "SQIPCT"
#separate quantiles
SQIQ[,1]<-seq(2025,2080,5)
names(SQIQ)<-c("Year","Q5","Q95")
#combine data
CombMeanSQI85 <- merge(SQIMean85[1:12,],SQIQ[1:12,], by="Year")
CombMeanSQI85 <- data.frame(rbind(MeanSQI,CombMeanSQI85))
#Graph
highlight <- CombMeanSQI85[7:18,]
SQI85Plot<- ggplot(data = CombMeanSQI85, aes(x=Year,y=SQIPCT))+ 
  annotate("text",x=-Inf,y=Inf,label="RCP Scenario 8.5",hjust=-0.1, vjust = 1.5, size = 4) +
  geom_point(alpha=2)+ xlab("Year") + ylab("Average Soil Quality Index") + 
  scale_x_continuous(breaks=seq(2000,2080,5)) +
  geom_smooth(data = filter(CombMeanSQI85, row_number() %in% 1:6), 
              method = "lm", formula = y ~ x, color = "blue",se=FALSE) +
  geom_smooth(data = filter(CombMeanSQI85, row_number() %in% 7:18), 
              method = "lm", formula = y ~ x, color = "red",se=FALSE) +  
  geom_point(data = highlight, aes(x=Year,y=SQIPCT), color = 'red', size = 2.5) +
  theme(plot.title = element_text(hjust=0.5),text=element_text(size=25))+
  geom_errorbar(aes(ymin=Q5, ymax=Q95)) + theme_classic()

CombSQI <- SQI26Plot/SQI45Plot/SQI85Plot
CombSQI + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))



#Percent Change/Cumulative Change Calc
# Load the regional map
midatlmap <- vect("/Users/DamaniEubanks/Desktop/FIA/MARshape/v10/mid-atlantic_and_new_england.gdb")

# Load and subset US states
setwd("~/Desktop/FIA/WI")
spUSA <- vect("cb_2021_us_state_500k.shp")
midcut <- subset(spUSA ,spUSA$NAME %in% c("Maryland", "New Jersey", "New York", "Delaware", "Pennsylvania"))

# Reproject to match CRS
midcut <- project(midcut, crs(midatlmap))

#base plot
midatl_sf <- st_as_sf(midatlmap)
midcut_sf <- st_as_sf(midcut)

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

#carbon
#F1,rcp26
#combine data
F1CarbPred26add <- F1CarbPred26 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1CarbPred26add <- rbind(F1CarbPred26add, F1Re[,c(1,4,2,9,8)])
F1CarbPred26add <- subset(F1CarbPred26add,F1CarbPred26add$Latitude%in%F1Re$Latitude)

#cumulative change
#earliest year to 2080
F1Carb26CC <- F1CarbPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F1Carb26CC <- na.omit(F1Carb26CC)
F1Carb26CC <- terra::vect(F1Carb26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Carb26CC) <- "EPSG:4326"
F1Carb26CCsf <- st_as_sf(F1Carb26CC)
F1Carb26CCsf <- st_transform(F1Carb26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Carb26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, White/Red/Jack Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
    ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F1 rcp 45
#combine data
F1CarbPred45add <- F1CarbPred45 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1CarbPred45add <- rbind(F1CarbPred45add, F1Re[,c(1,4,2,9,8)])
F1CarbPred45add <- subset(F1CarbPred45add,F1CarbPred45add$Latitude%in%F1Re$Latitude)

#Cumulative Change
F1Carb45CC <- F1CarbPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F1Carb45CC <- na.omit(F1Carb45CC)
F1Carb45CC <- terra::vect(F1Carb45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Carb45CC) <- "EPSG:4326"
F1Carb45CCsf <- st_as_sf(F1Carb45CC)
F1Carb45CCsf <- st_transform(F1Carb45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Carb45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, White/Red/Jack Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))


#F1 rcp 85
#combine data
F1CarbPred85add <- F1CarbPred85 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1CarbPred85add <- rbind(F1CarbPred85add, F1Re[,c(1,4,2,9,8)])
F1CarbPred85add <- subset(F1CarbPred85add,F1CarbPred85add$Latitude%in%F1Re$Latitude)

#cumulative change
F1Carb85CC <- F1CarbPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F1Carb85CC <- na.omit(F1Carb85CC)
F1Carb85CC <- terra::vect(F1Carb85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Carb85CC) <- "EPSG:4326"
F1Carb85CCsf <- st_as_sf(F1Carb85CC)
F1Carb85CCsf <- st_transform(F1Carb85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Carb85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, White/Red/Jack Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5,rcp26

#combine data
F5CarbPred26add <- F5CarbPred26 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5CarbPred26add <- rbind(F5CarbPred26add, F5Re[,c(1,4,2,9,8)])
F5CarbPred26add <- subset(F5CarbPred26add,F5CarbPred26add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Carb26CC <- F5CarbPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F5Carb26CC <- na.omit(F5Carb26CC)
F5Carb26CC <- terra::vect(F5Carb26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Carb26CC) <- "EPSG:4326"
F5Carb26CCsf <- st_as_sf(F5Carb26CC)
F5Carb26CCsf <- st_transform(F5Carb26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Carb26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 45
#combine data
F5CarbPred45add <- F5CarbPred45 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5CarbPred45add <- rbind(F5CarbPred45add, F5Re[,c(1,4,2,9,8)])
F5CarbPred45add <- subset(F5CarbPred45add,F5CarbPred45add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Carb45CC <- F5CarbPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F5Carb45CC <- na.omit(F5Carb45CC)
F5Carb45CC <- terra::vect(F5Carb45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Carb45CC) <- "EPSG:4326"
F5Carb45CCsf <- st_as_sf(F5Carb45CC)
F5Carb45CCsf <- st_transform(F5Carb45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Carb45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 85
#combine data
F5CarbPred85add <- F5CarbPred85 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5CarbPred85add <- rbind(F5CarbPred85add, F5Re[,c(1,4,2,9,8)])
F5CarbPred85add <- subset(F5CarbPred85add,F5CarbPred85add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Carb85CC <- F5CarbPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F5Carb85CC <- na.omit(F5Carb85CC)
F5Carb85CC <- terra::vect(F5Carb85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Carb85CC) <- "EPSG:4326"
F5Carb85CCsf <- st_as_sf(F5Carb85CC)
F5Carb85CCsf <- st_transform(F5Carb85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Carb85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20,rcp26

#combine data
F20CarbPred26add <- F20CarbPred26 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20CarbPred26add <- rbind(F20CarbPred26add, F20Re[,c(1,4,2,9,8)])
F20CarbPred26add <- subset(F20CarbPred26add,F20CarbPred26add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Carb26CC <- F20CarbPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F20Carb26CC <- na.omit(F20Carb26CC)
F20Carb26CC <- terra::vect(F20Carb26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Carb26CC) <- "EPSG:4326"
F20Carb26CCsf <- st_as_sf(F20Carb26CC)
F20Carb26CCsf <- st_transform(F20Carb26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Carb26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Oak/Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 45
#combine data
F20CarbPred45add <- F20CarbPred45 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20CarbPred45add <- rbind(F20CarbPred45add, F20Re[,c(1,4,2,9,8)])
F20CarbPred45add <- subset(F20CarbPred45add,F20CarbPred45add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Carb45CC <- F20CarbPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F20Carb45CC <- na.omit(F20Carb45CC)
F20Carb45CC <- terra::vect(F20Carb45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Carb45CC) <- "EPSG:4326"
F20Carb45CCsf <- st_as_sf(F20Carb45CC)
F20Carb45CCsf <- st_transform(F20Carb45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Carb45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Oak/Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 85
#combine data
F20CarbPred85add <- F20CarbPred85 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20CarbPred85add <- rbind(F20CarbPred85add, F20Re[,c(1,4,2,9,8)])
F20CarbPred85add <- subset(F20CarbPred85add,F20CarbPred85add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Carb85CC <- F20CarbPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F20Carb85CC <- na.omit(F20Carb85CC)
F20Carb85CC <- terra::vect(F20Carb85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Carb85CC) <- "EPSG:4326"
F20Carb85CCsf <- st_as_sf(F20Carb85CC)
F20Carb85CCsf <- st_transform(F20Carb85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Carb85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Oak/Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21,rcp26

#combine data
F21CarbPred26add <- F21CarbPred26 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21CarbPred26add <- rbind(F21CarbPred26add, F21Re[,c(1,4,2,9,8)])
F21CarbPred26add <- subset(F21CarbPred26add,F21CarbPred26add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Carb26CC <- F21CarbPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F21Carb26CC <- na.omit(F21Carb26CC)
F21Carb26CC <- terra::vect(F21Carb26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Carb26CC) <- "EPSG:4326"
F21Carb26CCsf <- st_as_sf(F21Carb26CC)
F21Carb26CCsf <- st_transform(F21Carb26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Carb26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Oak/Hickory RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 45
#combine data
F21CarbPred45add <- F21CarbPred45 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21CarbPred45add <- rbind(F21CarbPred45add, F21Re[,c(1,4,2,9,8)])
F21CarbPred45add <- subset(F21CarbPred45add,F21CarbPred45add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Carb45CC <- F21CarbPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F21Carb45CC <- na.omit(F21Carb45CC)
F21Carb45CC <- terra::vect(F21Carb45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Carb45CC) <- "EPSG:4326"
F21Carb45CCsf <- st_as_sf(F21Carb45CC)
F21Carb45CCsf <- st_transform(F21Carb45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Carb45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Oak/Hickory RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 85
#combine data
F21CarbPred85add <- F21CarbPred85 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21CarbPred85add <- rbind(F21CarbPred85add, F21Re[,c(1,4,2,9,8)])
F21CarbPred85add <- subset(F21CarbPred85add,F21CarbPred85add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Carb85CC <- F21CarbPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F21Carb85CC <- na.omit(F21Carb85CC)
F21Carb85CC <- terra::vect(F21Carb85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Carb85CC) <- "EPSG:4326"
F21Carb85CCsf <- st_as_sf(F21Carb85CC)
F21Carb85CCsf <- st_transform(F21Carb85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Carb85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Oak/Hickory RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23,rcp26

#combine data
F23CarbPred26add <- F23CarbPred26 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23CarbPred26add <- rbind(F23CarbPred26add, F23Re[,c(1,4,2,9,8)])
F23CarbPred26add <- subset(F23CarbPred26add,F23CarbPred26add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Carb26CC <- F23CarbPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F23Carb26CC <- na.omit(F23Carb26CC)
F23Carb26CC <- terra::vect(F23Carb26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Carb26CC) <- "EPSG:4326"
F23Carb26CCsf <- st_as_sf(F23Carb26CC)
F23Carb26CCsf <- st_transform(F23Carb26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Carb26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 45
#combine data
F23CarbPred45add <- F23CarbPred45 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23CarbPred45add <- rbind(F23CarbPred45add, F23Re[,c(1,4,2,9,8)])
F23CarbPred45add <- subset(F23CarbPred45add,F23CarbPred45add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Carb45CC <- F23CarbPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F23Carb45CC <- na.omit(F23Carb45CC)
F23Carb45CC <- terra::vect(F23Carb45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Carb45CC) <- "EPSG:4326"
F23Carb45CCsf <- st_as_sf(F23Carb45CC)
F23Carb45CCsf <- st_transform(F23Carb45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Carb45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 85
#combine data
F23CarbPred85add <- F23CarbPred85 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23CarbPred85add <- rbind(F23CarbPred85add, F23Re[,c(1,4,2,9,8)])
F23CarbPred85add <- subset(F23CarbPred85add,F23CarbPred85add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Carb85CC <- F23CarbPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F23Carb85CC <- na.omit(F23Carb85CC)
F23Carb85CC <- terra::vect(F23Carb85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Carb85CC) <- "EPSG:4326"
F23Carb85CCsf <- st_as_sf(F23Carb85CC)
F23Carb85CCsf <- st_transform(F23Carb85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Carb85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Carbon Storage Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24,rcp26

#combine data
F24CarbPred26add <- F24CarbPred26 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24CarbPred26add <- rbind(F24CarbPred26add, F24Re[,c(1,4,2,9,8)])
F24CarbPred26add <- subset(F24CarbPred26add,F24CarbPred26add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Carb26CC <- F24CarbPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F24Carb26CC <- na.omit(F24Carb26CC)
F24Carb26CC <- terra::vect(F24Carb26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Carb26CC) <- "EPSG:4326"
F24Carb26CCsf <- st_as_sf(F24Carb26CC)
F24Carb26CCsf <- st_transform(F24Carb26CCsf, st_crs(midcut_sf))

F24CarbCCPlot26 <- ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Carb26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  #labs(title = "Carbon Storage Cumulative Change 2002-2080, Maple/Beech/Birch RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 45
#combine data
F24CarbPred45add <- F24CarbPred45 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24CarbPred45add <- rbind(F24CarbPred45add, F24Re[,c(1,4,2,9,8)])
F24CarbPred45add <- subset(F24CarbPred45add,F24CarbPred45add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Carb45CC <- F24CarbPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F24Carb45CC <- na.omit(F24Carb45CC)
F24Carb45CC <- terra::vect(F24Carb45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Carb45CC) <- "EPSG:4326"
F24Carb45CCsf <- st_as_sf(F24Carb45CC)
F24Carb45CCsf <- st_transform(F24Carb45CCsf, st_crs(midcut_sf))

F24CarbCCPlot45<- ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Carb45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  #labs(title = "Carbon Storage Cumulative Change 2002-2080, Maple/Beech/Birch RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 85
#combine data
F24CarbPred85add <- F24CarbPred85 %>%
  pivot_longer(
    cols = starts_with("Carb"),
    names_to = "CarbColumn",
    values_to = "CarbonPerAcre"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Carb", "", CarbColumn)) + 2000
  ) %>%
  select(PlotCN, CarbonPerAcre, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24CarbPred85add <- rbind(F24CarbPred85add, F24Re[,c(1,4,2,9,8)])
F24CarbPred85add <- subset(F24CarbPred85add,F24CarbPred85add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Carb85CC <- F24CarbPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Carbon_Initial = CarbonPerAcre[Year == min(Year)][1],  
    Carbon_2080 = CarbonPerAcre[Year == 2080][1],         
    CumulativeChange = (Carbon_2080 - Carbon_Initial)
  ) %>%
  ungroup()

F24Carb85CC <- na.omit(F24Carb85CC)
F24Carb85CC <- terra::vect(F24Carb85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Carb85CC) <- "EPSG:4326"
F24Carb85CCsf <- st_as_sf(F24Carb85CC)
F24Carb85CCsf <- st_transform(F24Carb85CCsf, st_crs(midcut_sf))

F24CarbCCPlot85<-ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Carb85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Tonnes Per Acre"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  #labs(title = "Carbon Storage Cumulative Change 2002-2080, Maple/Beech/Birch RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

F24CarbComboPlot <- F24CarbCCPlot26/F24CarbCCPlot45/F24CarbCCPlot85
F24CarbComboPlot + plot_layout(axis_titles = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag.position = c(1, 1))

#Hill
#F1,rcp26
#combine data
F1HillPred26add <- F1HillPred26 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1HillPred26add <- rbind(F1HillPred26add, F1Re[,c(1,5,2,9,8)])
F1HillPred26add <- subset(F1HillPred26add,F1HillPred26add$Latitude%in%F1Re$Latitude)

#cumulative change
#earliest year to 2080
F1Hill26CC <- F1HillPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F1Hill26CC <- na.omit(F1Hill26CC)
F1Hill26CC <- terra::vect(F1Hill26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Hill26CC) <- "EPSG:4326"
F1Hill26CCsf <- st_as_sf(F1Hill26CC)
F1Hill26CCsf <- st_transform(F1Hill26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Hill26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, White/Red/Jack Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F1 rcp 45
#combine data
F1HillPred45add <- F1HillPred45 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1HillPred45add <- rbind(F1HillPred45add, F1Re[,c(1,5,2,9,8)])
F1HillPred45add <- subset(F1HillPred45add,F1HillPred45add$Latitude%in%F1Re$Latitude)

#Cumulative Change
F1Hill45CC <- F1HillPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F1Hill45CC <- na.omit(F1Hill45CC)
F1Hill45CC <- terra::vect(F1Hill45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Hill45CC) <- "EPSG:4326"
F1Hill45CCsf <- st_as_sf(F1Hill45CC)
F1Hill45CCsf <- st_transform(F1Hill45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Hill45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, White/Red/Jack Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F1 rcp 85
#combine data
F1HillPred85add <- F1HillPred85 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1HillPred85add <- rbind(F1HillPred85add, F1Re[,c(1,5,2,9,8)])
F1HillPred85add <- subset(F1HillPred85add,F1HillPred85add$Latitude%in%F1Re$Latitude)

#cumulative change
F1Hill85CC <- F1HillPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F1Hill85CC <- na.omit(F1Hill85CC)
F1Hill85CC <- terra::vect(F1Hill85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Hill85CC) <- "EPSG:4326"
F1Hill85CCsf <- st_as_sf(F1Hill85CC)
F1Hill85CCsf <- st_transform(F1Hill85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Hill85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, White/Red/Jack Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5,rcp26

#combine data
F5HillPred26add <- F5HillPred26 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5HillPred26add <- rbind(F5HillPred26add, F5Re[,c(1,5,2,9,8)])
F5HillPred26add <- subset(F5HillPred26add,F5HillPred26add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Hill26CC <- F5HillPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F5Hill26CC <- na.omit(F5Hill26CC)
F5Hill26CC <- terra::vect(F5Hill26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Hill26CC) <- "EPSG:4326"
F5Hill26CCsf <- st_as_sf(F5Hill26CC)
F5Hill26CCsf <- st_transform(F5Hill26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Hill26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 45
#combine data
F5HillPred45add <- F5HillPred45 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5HillPred45add <- rbind(F5HillPred45add, F5Re[,c(1,5,2,9,8)])
F5HillPred45add <- subset(F5HillPred45add,F5HillPred45add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Hill45CC <- F5HillPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F5Hill45CC <- na.omit(F5Hill45CC)
F5Hill45CC <- terra::vect(F5Hill45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Hill45CC) <- "EPSG:4326"
F5Hill45CCsf <- st_as_sf(F5Hill45CC)
F5Hill45CCsf <- st_transform(F5Hill45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Hill45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 85
#combine data
F5HillPred85add <- F5HillPred85 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5HillPred85add <- rbind(F5HillPred85add, F5Re[,c(1,5,2,9,8)])
F5HillPred85add <- subset(F5HillPred85add,F5HillPred85add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Hill85CC <- F5HillPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F5Hill85CC <- na.omit(F5Hill85CC)
F5Hill85CC <- terra::vect(F5Hill85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Hill85CC) <- "EPSG:4326"
F5Hill85CCsf <- st_as_sf(F5Hill85CC)
F5Hill85CCsf <- st_transform(F5Hill85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Hill85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20,rcp26

#combine data
F20HillPred26add <- F20HillPred26 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20HillPred26add <- rbind(F20HillPred26add, F20Re[,c(1,5,2,9,8)])
F20HillPred26add <- subset(F20HillPred26add,F20HillPred26add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Hill26CC <- F20HillPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F20Hill26CC <- na.omit(F20Hill26CC)
F20Hill26CC <- terra::vect(F20Hill26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Hill26CC) <- "EPSG:4326"
F20Hill26CCsf <- st_as_sf(F20Hill26CC)
F20Hill26CCsf <- st_transform(F20Hill26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Hill26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Oak/Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 45
#combine data
F20HillPred45add <- F20HillPred45 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20HillPred45add <- rbind(F20HillPred45add, F20Re[,c(1,5,2,9,8)])
F20HillPred45add <- subset(F20HillPred45add,F20HillPred45add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Hill45CC <- F20HillPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F20Hill45CC <- na.omit(F20Hill45CC)
F20Hill45CC <- terra::vect(F20Hill45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Hill45CC) <- "EPSG:4326"
F20Hill45CCsf <- st_as_sf(F20Hill45CC)
F20Hill45CCsf <- st_transform(F20Hill45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Hill45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Oak/Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 85
#combine data
F20HillPred85add <- F20HillPred85 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20HillPred85add <- rbind(F20HillPred85add, F20Re[,c(1,5,2,9,8)])
F20HillPred85add <- subset(F20HillPred85add,F20HillPred85add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Hill85CC <- F20HillPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F20Hill85CC <- na.omit(F20Hill85CC)
F20Hill85CC <- terra::vect(F20Hill85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Hill85CC) <- "EPSG:4326"
F20Hill85CCsf <- st_as_sf(F20Hill85CC)
F20Hill85CCsf <- st_transform(F20Hill85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Hill85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Oak/Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21,rcp26

#combine data
F21HillPred26add <- F21HillPred26 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21HillPred26add <- rbind(F21HillPred26add, F21Re[,c(1,5,2,9,8)])
F21HillPred26add <- subset(F21HillPred26add,F21HillPred26add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Hill26CC <- F21HillPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F21Hill26CC <- na.omit(F21Hill26CC)
F21Hill26CC <- terra::vect(F21Hill26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Hill26CC) <- "EPSG:4326"
F21Hill26CCsf <- st_as_sf(F21Hill26CC)
F21Hill26CCsf <- st_transform(F21Hill26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Hill26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Oak/Hickory RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 45
#combine data
F21HillPred45add <- F21HillPred45 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21HillPred45add <- rbind(F21HillPred45add, F21Re[,c(1,5,2,9,8)])
F21HillPred45add <- subset(F21HillPred45add,F21HillPred45add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Hill45CC <- F21HillPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F21Hill45CC <- na.omit(F21Hill45CC)
F21Hill45CC <- terra::vect(F21Hill45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Hill45CC) <- "EPSG:4326"
F21Hill45CCsf <- st_as_sf(F21Hill45CC)
F21Hill45CCsf <- st_transform(F21Hill45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Hill45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Oak/Hickory RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 85
#combine data
F21HillPred85add <- F21HillPred85 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21HillPred85add <- rbind(F21HillPred85add, F21Re[,c(1,5,2,9,8)])
F21HillPred85add <- subset(F21HillPred85add,F21HillPred85add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Hill85CC <- F21HillPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F21Hill85CC <- na.omit(F21Hill85CC)
F21Hill85CC <- terra::vect(F21Hill85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Hill85CC) <- "EPSG:4326"
F21Hill85CCsf <- st_as_sf(F21Hill85CC)
F21Hill85CCsf <- st_transform(F21Hill85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Hill85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Oak/Hickory RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23,rcp26

#combine data
F23HillPred26add <- F23HillPred26 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23HillPred26add <- rbind(F23HillPred26add, F23Re[,c(1,5,2,9,8)])
F23HillPred26add <- subset(F23HillPred26add,F23HillPred26add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Hill26CC <- F23HillPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F23Hill26CC <- na.omit(F23Hill26CC)
F23Hill26CC <- terra::vect(F23Hill26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Hill26CC) <- "EPSG:4326"
F23Hill26CCsf <- st_as_sf(F23Hill26CC)
F23Hill26CCsf <- st_transform(F23Hill26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Hill26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 45
#combine data
F23HillPred45add <- F23HillPred45 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23HillPred45add <- rbind(F23HillPred45add, F23Re[,c(1,5,2,9,8)])
F23HillPred45add <- subset(F23HillPred45add,F23HillPred45add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Hill45CC <- F23HillPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F23Hill45CC <- na.omit(F23Hill45CC)
F23Hill45CC <- terra::vect(F23Hill45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Hill45CC) <- "EPSG:4326"
F23Hill45CCsf <- st_as_sf(F23Hill45CC)
F23Hill45CCsf <- st_transform(F23Hill45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Hill45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 85
#combine data
F23HillPred85add <- F23HillPred85 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23HillPred85add <- rbind(F23HillPred85add, F23Re[,c(1,5,2,9,8)])
F23HillPred85add <- subset(F23HillPred85add,F23HillPred85add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Hill85CC <- F23HillPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F23Hill85CC <- na.omit(F23Hill85CC)
F23Hill85CC <- terra::vect(F23Hill85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Hill85CC) <- "EPSG:4326"
F23Hill85CCsf <- st_as_sf(F23Hill85CC)
F23Hill85CCsf <- st_transform(F23Hill85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Hill85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24,rcp26

#combine data
F24HillPred26add <- F24HillPred26 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24HillPred26add <- rbind(F24HillPred26add, F24Re[,c(1,5,2,9,8)])
F24HillPred26add <- subset(F24HillPred26add,F24HillPred26add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Hill26CC <- F24HillPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F24Hill26CC <- na.omit(F24Hill26CC)
F24Hill26CC <- terra::vect(F24Hill26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Hill26CC) <- "EPSG:4326"
F24Hill26CCsf <- st_as_sf(F24Hill26CC)
F24Hill26CCsf <- st_transform(F24Hill26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Hill26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Maple/Beech/Birch RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 45
#combine data
F24HillPred45add <- F24HillPred45 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24HillPred45add <- rbind(F24HillPred45add, F24Re[,c(1,5,2,9,8)])
F24HillPred45add <- subset(F24HillPred45add,F24HillPred45add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Hill45CC <- F24HillPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F24Hill45CC <- na.omit(F24Hill45CC)
F24Hill45CC <- terra::vect(F24Hill45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Hill45CC) <- "EPSG:4326"
F24Hill45CCsf <- st_as_sf(F24Hill45CC)
F24Hill45CCsf <- st_transform(F24Hill45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Hill45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Maple/Beech/Birch RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 85
#combine data
F24HillPred85add <- F24HillPred85 %>%
  pivot_longer(
    cols = starts_with("Hill"),
    names_to = "HillColumn",
    values_to = "HillShannonIndex"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Hill", "", HillColumn)) + 2000
  ) %>%
  select(PlotCN, HillShannonIndex, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24HillPred85add <- rbind(F24HillPred85add, F24Re[,c(1,5,2,9,8)])
F24HillPred85add <- subset(F24HillPred85add,F24HillPred85add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Hill85CC <- F24HillPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Hill_Initial = HillShannonIndex[Year == min(Year)][1],  
    Hill_2080 = HillShannonIndex[Year == 2080][1],         
    CumulativeChange = (Hill_2080 - Hill_Initial)
  ) %>%
  ungroup()

F24Hill85CC <- na.omit(F24Hill85CC)
F24Hill85CC <- terra::vect(F24Hill85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Hill85CC) <- "EPSG:4326"
F24Hill85CCsf <- st_as_sf(F24Hill85CC)
F24Hill85CCsf <- st_transform(F24Hill85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Hill85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Effective Species Number"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Hill Shannon Index Cumulative Change 2002-2080, Maple/Beech/Birch RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#Jaccards
#F1,rcp26

#combine data
F1JaccPred26add <- F1JaccPred26 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1JaccPred26add <- rbind(F1JaccPred26add, F1Re[,c(1,6,2,9,8)])
F1JaccPred26add <- subset(F1JaccPred26add,F1JaccPred26add$Latitude%in%F1Re$Latitude)

#cumulative change
#earliest year to 2080
F1Jacc26CC <- F1JaccPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F1Jacc26CC <- na.omit(F1Jacc26CC)
F1Jacc26CC <- terra::vect(F1Jacc26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Jacc26CC) <- "EPSG:4326"
F1Jacc26CCsf <- st_as_sf(F1Jacc26CC)
F1Jacc26CCsf <- st_transform(F1Jacc26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Jacc26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, White/Red/Jack Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F1 rcp 45
#combine data
F1JaccPred45add <- F1JaccPred45 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1JaccPred45add <- rbind(F1JaccPred45add, F1Re[,c(1,6,2,9,8)])
F1JaccPred45add <- subset(F1JaccPred45add,F1JaccPred45add$Latitude%in%F1Re$Latitude)

#Cumulative Change
F1Jacc45CC <- F1JaccPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F1Jacc45CC <- na.omit(F1Jacc45CC)
F1Jacc45CC <- terra::vect(F1Jacc45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Jacc45CC) <- "EPSG:4326"
F1Jacc45CCsf <- st_as_sf(F1Jacc45CC)
F1Jacc45CCsf <- st_transform(F1Jacc45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Jacc45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, White/Red/Jack Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F1 rcp 85
#combine data
F1JaccPred85add <- F1JaccPred85 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1JaccPred85add <- rbind(F1JaccPred85add, F1Re[,c(1,6,2,9,8)])
F1JaccPred85add <- subset(F1JaccPred85add,F1JaccPred85add$Latitude%in%F1Re$Latitude)

#cumulative change
F1Jacc85CC <- F1JaccPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F1Jacc85CC <- na.omit(F1Jacc85CC)
F1Jacc85CC <- terra::vect(F1Jacc85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Jacc85CC) <- "EPSG:4326"
F1Jacc85CCsf <- st_as_sf(F1Jacc85CC)
F1Jacc85CCsf <- st_transform(F1Jacc85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Jacc85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, White/Red/Jack Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5,rcp26

#combine data
F5JaccPred26add <- F5JaccPred26 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5JaccPred26add <- rbind(F5JaccPred26add, F5Re[,c(1,6,2,9,8)])
F5JaccPred26add <- subset(F5JaccPred26add,F5JaccPred26add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Jacc26CC <- F5JaccPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F5Jacc26CC <- na.omit(F5Jacc26CC)
F5Jacc26CC <- terra::vect(F5Jacc26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Jacc26CC) <- "EPSG:4326"
F5Jacc26CCsf <- st_as_sf(F5Jacc26CC)
F5Jacc26CCsf <- st_transform(F5Jacc26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Jacc26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 45
#combine data
F5JaccPred45add <- F5JaccPred45 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5JaccPred45add <- rbind(F5JaccPred45add, F5Re[,c(1,6,2,9,8)])
F5JaccPred45add <- subset(F5JaccPred45add,F5JaccPred45add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Jacc45CC <- F5JaccPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F5Jacc45CC <- na.omit(F5Jacc45CC)
F5Jacc45CC <- terra::vect(F5Jacc45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Jacc45CC) <- "EPSG:4326"
F5Jacc45CCsf <- st_as_sf(F5Jacc45CC)
F5Jacc45CCsf <- st_transform(F5Jacc45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Jacc45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 85
#combine data
F5JaccPred85add <- F5JaccPred85 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5JaccPred85add <- rbind(F5JaccPred85add, F5Re[,c(1,6,2,9,8)])
F5JaccPred85add <- subset(F5JaccPred85add,F5JaccPred85add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Jacc85CC <- F5JaccPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F5Jacc85CC <- na.omit(F5Jacc85CC)
F5Jacc85CC <- terra::vect(F5Jacc85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Jacc85CC) <- "EPSG:4326"
F5Jacc85CCsf <- st_as_sf(F5Jacc85CC)
F5Jacc85CCsf <- st_transform(F5Jacc85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Jacc85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20,rcp26

#combine data
F20JaccPred26add <- F20JaccPred26 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20JaccPred26add <- rbind(F20JaccPred26add, F20Re[,c(1,6,2,9,8)])
F20JaccPred26add <- subset(F20JaccPred26add,F20JaccPred26add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Jacc26CC <- F20JaccPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F20Jacc26CC <- na.omit(F20Jacc26CC)
F20Jacc26CC <- terra::vect(F20Jacc26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Jacc26CC) <- "EPSG:4326"
F20Jacc26CCsf <- st_as_sf(F20Jacc26CC)
F20Jacc26CCsf <- st_transform(F20Jacc26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Jacc26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Oak/Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 45
#combine data
F20JaccPred45add <- F20JaccPred45 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20JaccPred45add <- rbind(F20JaccPred45add, F20Re[,c(1,6,2,9,8)])
F20JaccPred45add <- subset(F20JaccPred45add,F20JaccPred45add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Jacc45CC <- F20JaccPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F20Jacc45CC <- na.omit(F20Jacc45CC)
F20Jacc45CC <- terra::vect(F20Jacc45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Jacc45CC) <- "EPSG:4326"
F20Jacc45CCsf <- st_as_sf(F20Jacc45CC)
F20Jacc45CCsf <- st_transform(F20Jacc45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Jacc45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Oak/Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 85
#combine data
F20JaccPred85add <- F20JaccPred85 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20JaccPred85add <- rbind(F20JaccPred85add, F20Re[,c(1,6,2,9,8)])
F20JaccPred85add <- subset(F20JaccPred85add,F20JaccPred85add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Jacc85CC <- F20JaccPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F20Jacc85CC <- na.omit(F20Jacc85CC)
F20Jacc85CC <- terra::vect(F20Jacc85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Jacc85CC) <- "EPSG:4326"
F20Jacc85CCsf <- st_as_sf(F20Jacc85CC)
F20Jacc85CCsf <- st_transform(F20Jacc85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Jacc85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Oak/Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21,rcp26

#combine data
F21JaccPred26add <- F21JaccPred26 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21JaccPred26add <- rbind(F21JaccPred26add, F21Re[,c(1,6,2,9,8)])
F21JaccPred26add <- subset(F21JaccPred26add,F21JaccPred26add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Jacc26CC <- F21JaccPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F21Jacc26CC <- na.omit(F21Jacc26CC)
F21Jacc26CC <- terra::vect(F21Jacc26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Jacc26CC) <- "EPSG:4326"
F21Jacc26CCsf <- st_as_sf(F21Jacc26CC)
F21Jacc26CCsf <- st_transform(F21Jacc26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Jacc26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Oak/Hickory RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 45
#combine data
F21JaccPred45add <- F21JaccPred45 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21JaccPred45add <- rbind(F21JaccPred45add, F21Re[,c(1,6,2,9,8)])
F21JaccPred45add <- subset(F21JaccPred45add,F21JaccPred45add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Jacc45CC <- F21JaccPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F21Jacc45CC <- na.omit(F21Jacc45CC)
F21Jacc45CC <- terra::vect(F21Jacc45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Jacc45CC) <- "EPSG:4326"
F21Jacc45CCsf <- st_as_sf(F21Jacc45CC)
F21Jacc45CCsf <- st_transform(F21Jacc45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Jacc45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Oak/Hickory RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 85
#combine data
F21JaccPred85add <- F21JaccPred85 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21JaccPred85add <- rbind(F21JaccPred85add, F21Re[,c(1,6,2,9,8)])
F21JaccPred85add <- subset(F21JaccPred85add,F21JaccPred85add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Jacc85CC <- F21JaccPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F21Jacc85CC <- na.omit(F21Jacc85CC)
F21Jacc85CC <- terra::vect(F21Jacc85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Jacc85CC) <- "EPSG:4326"
F21Jacc85CCsf <- st_as_sf(F21Jacc85CC)
F21Jacc85CCsf <- st_transform(F21Jacc85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Jacc85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Oak/Hickory RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23,rcp26

#combine data
F23JaccPred26add <- F23JaccPred26 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23JaccPred26add <- rbind(F23JaccPred26add, F23Re[,c(1,6,2,9,8)])
F23JaccPred26add <- subset(F23JaccPred26add,F23JaccPred26add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Jacc26CC <- F23JaccPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F23Jacc26CC <- na.omit(F23Jacc26CC)
F23Jacc26CC <- terra::vect(F23Jacc26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Jacc26CC) <- "EPSG:4326"
F23Jacc26CCsf <- st_as_sf(F23Jacc26CC)
F23Jacc26CCsf <- st_transform(F23Jacc26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Jacc26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 45
#combine data
F23JaccPred45add <- F23JaccPred45 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23JaccPred45add <- rbind(F23JaccPred45add, F23Re[,c(1,6,2,9,8)])
F23JaccPred45add <- subset(F23JaccPred45add,F23JaccPred45add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Jacc45CC <- F23JaccPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F23Jacc45CC <- na.omit(F23Jacc45CC)
F23Jacc45CC <- terra::vect(F23Jacc45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Jacc45CC) <- "EPSG:4326"
F23Jacc45CCsf <- st_as_sf(F23Jacc45CC)
F23Jacc45CCsf <- st_transform(F23Jacc45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Jacc45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 85
#combine data
F23JaccPred85add <- F23JaccPred85 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23JaccPred85add <- rbind(F23JaccPred85add, F23Re[,c(1,6,2,9,8)])
F23JaccPred85add <- subset(F23JaccPred85add,F23JaccPred85add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Jacc85CC <- F23JaccPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F23Jacc85CC <- na.omit(F23Jacc85CC)
F23Jacc85CC <- terra::vect(F23Jacc85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Jacc85CC) <- "EPSG:4326"
F23Jacc85CCsf <- st_as_sf(F23Jacc85CC)
F23Jacc85CCsf <- st_transform(F23Jacc85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Jacc85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24,rcp26

#combine data
F24JaccPred26add <- F24JaccPred26 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24JaccPred26add <- rbind(F24JaccPred26add, F24Re[,c(1,6,2,9,8)])
F24JaccPred26add <- subset(F24JaccPred26add,F24JaccPred26add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Jacc26CC <- F24JaccPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F24Jacc26CC <- na.omit(F24Jacc26CC)
F24Jacc26CC <- terra::vect(F24Jacc26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Jacc26CC) <- "EPSG:4326"
F24Jacc26CCsf <- st_as_sf(F24Jacc26CC)
F24Jacc26CCsf <- st_transform(F24Jacc26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Jacc26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Maple/Beech/Birch RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 45
#combine data
F24JaccPred45add <- F24JaccPred45 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24JaccPred45add <- rbind(F24JaccPred45add, F24Re[,c(1,6,2,9,8)])
F24JaccPred45add <- subset(F24JaccPred45add,F24JaccPred45add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Jacc45CC <- F24JaccPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F24Jacc45CC <- na.omit(F24Jacc45CC)
F24Jacc45CC <- terra::vect(F24Jacc45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Jacc45CC) <- "EPSG:4326"
F24Jacc45CCsf <- st_as_sf(F24Jacc45CC)
F24Jacc45CCsf <- st_transform(F24Jacc45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Jacc45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Maple/Beech/Birch RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 85
#combine data
F24JaccPred85add <- F24JaccPred85 %>%
  pivot_longer(
    cols = starts_with("Jacc"),
    names_to = "JaccColumn",
    values_to = "JaccardMean"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Jacc", "", JaccColumn)) + 2000
  ) %>%
  select(PlotCN, JaccardMean, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24JaccPred85add <- rbind(F24JaccPred85add, F24Re[,c(1,6,2,9,8)])
F24JaccPred85add <- subset(F24JaccPred85add,F24JaccPred85add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Jacc85CC <- F24JaccPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Jacc_Initial = JaccardMean[Year == min(Year)][1],  
    Jacc_2080 = JaccardMean[Year == 2080][1],         
    CumulativeChange = (Jacc_2080 - Jacc_Initial)
  ) %>%
  ungroup()

F24Jacc85CC <- na.omit(F24Jacc85CC)
F24Jacc85CC <- terra::vect(F24Jacc85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Jacc85CC) <- "EPSG:4326"
F24Jacc85CCsf <- st_as_sf(F24Jacc85CC)
F24Jacc85CCsf <- st_transform(F24Jacc85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Jacc85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Similarity Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Jaccards Similarity Cumulative Change 2002-2080, Maple/Beech/Birch RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))


#MPD
#F1,rcp26
#combine data
F1MPDPred26add <- F1MPDPred26 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1MPDPred26add <- rbind(F1MPDPred26add, F1Re[,c(1,7,2,9,8)])
F1MPDPred26add <- subset(F1MPDPred26add,F1MPDPred26add$Latitude%in%F1Re$Latitude)

#cumulative change
#earliest year to 2080
F1MPD26CC <- F1MPDPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F1MPD26CC <- na.omit(F1MPD26CC)
F1MPD26CC <- terra::vect(F1MPD26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1MPD26CC) <- "EPSG:4326"
F1MPD26CCsf <- st_as_sf(F1MPD26CC)
F1MPD26CCsf <- st_transform(F1MPD26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1MPD26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, White/Red/Jack Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F1 rcp 45
#combine data
F1MPDPred45add <- F1MPDPred45 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1MPDPred45add <- rbind(F1MPDPred45add, F1Re[,c(1,7,2,9,8)])
F1MPDPred45add <- subset(F1MPDPred45add,F1MPDPred45add$Latitude%in%F1Re$Latitude)

#Cumulative Change
F1MPD45CC <- F1MPDPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F1MPD45CC <- na.omit(F1MPD45CC)
F1MPD45CC <- terra::vect(F1MPD45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1MPD45CC) <- "EPSG:4326"
F1MPD45CCsf <- st_as_sf(F1MPD45CC)
F1MPD45CCsf <- st_transform(F1MPD45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1MPD45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, White/Red/Jack Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))


#F1 rcp 85
#combine data
F1MPDPred85add <- F1MPDPred85 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1MPDPred85add <- rbind(F1MPDPred85add, F1Re[,c(1,7,2,9,8)])
F1MPDPred85add <- subset(F1MPDPred85add,F1MPDPred85add$Latitude%in%F1Re$Latitude)

#cumulative change
F1MPD85CC <- F1MPDPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F1MPD85CC <- na.omit(F1MPD85CC)
F1MPD85CC <- terra::vect(F1MPD85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1MPD85CC) <- "EPSG:4326"
F1MPD85CCsf <- st_as_sf(F1MPD85CC)
F1MPD85CCsf <- st_transform(F1MPD85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1MPD85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, White/Red/Jack Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5,rcp26

#combine data
F5MPDPred26add <- F5MPDPred26 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5MPDPred26add <- rbind(F5MPDPred26add, F5Re[,c(1,7,2,9,8)])
F5MPDPred26add <- subset(F5MPDPred26add,F5MPDPred26add$Latitude%in%F5Re$Latitude)

#cumulative change
F5MPD26CC <- F5MPDPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F5MPD26CC <- na.omit(F5MPD26CC)
F5MPD26CC <- terra::vect(F5MPD26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5MPD26CC) <- "EPSG:4326"
F5MPD26CCsf <- st_as_sf(F5MPD26CC)
F5MPD26CCsf <- st_transform(F5MPD26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5MPD26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 45
#combine data
F5MPDPred45add <- F5MPDPred45 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5MPDPred45add <- rbind(F5MPDPred45add, F5Re[,c(1,7,2,9,8)])
F5MPDPred45add <- subset(F5MPDPred45add,F5MPDPred45add$Latitude%in%F5Re$Latitude)

#cumulative change
F5MPD45CC <- F5MPDPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F5MPD45CC <- na.omit(F5MPD45CC)
F5MPD45CC <- terra::vect(F5MPD45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5MPD45CC) <- "EPSG:4326"
F5MPD45CCsf <- st_as_sf(F5MPD45CC)
F5MPD45CCsf <- st_transform(F5MPD45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5MPD45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 85
#combine data
F5MPDPred85add <- F5MPDPred85 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5MPDPred85add <- rbind(F5MPDPred85add, F5Re[,c(1,7,2,9,8)])
F5MPDPred85add <- subset(F5MPDPred85add,F5MPDPred85add$Latitude%in%F5Re$Latitude)

#cumulative change
F5MPD85CC <- F5MPDPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F5MPD85CC <- na.omit(F5MPD85CC)
F5MPD85CC <- terra::vect(F5MPD85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5MPD85CC) <- "EPSG:4326"
F5MPD85CCsf <- st_as_sf(F5MPD85CC)
F5MPD85CCsf <- st_transform(F5MPD85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5MPD85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20,rcp26

#combine data
F20MPDPred26add <- F20MPDPred26 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20MPDPred26add <- rbind(F20MPDPred26add, F20Re[,c(1,7,2,9,8)])
F20MPDPred26add <- subset(F20MPDPred26add,F20MPDPred26add$Latitude%in%F20Re$Latitude)

#cumulative change
F20MPD26CC <- F20MPDPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F20MPD26CC <- na.omit(F20MPD26CC)
F20MPD26CC <- terra::vect(F20MPD26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20MPD26CC) <- "EPSG:4326"
F20MPD26CCsf <- st_as_sf(F20MPD26CC)
F20MPD26CCsf <- st_transform(F20MPD26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20MPD26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Oak/Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 45
#combine data
F20MPDPred45add <- F20MPDPred45 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20MPDPred45add <- rbind(F20MPDPred45add, F20Re[,c(1,7,2,9,8)])
F20MPDPred45add <- subset(F20MPDPred45add,F20MPDPred45add$Latitude%in%F20Re$Latitude)

#cumulative change
F20MPD45CC <- F20MPDPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F20MPD45CC <- na.omit(F20MPD45CC)
F20MPD45CC <- terra::vect(F20MPD45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20MPD45CC) <- "EPSG:4326"
F20MPD45CCsf <- st_as_sf(F20MPD45CC)
F20MPD45CCsf <- st_transform(F20MPD45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20MPD45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Oak/Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 85
#combine data
F20MPDPred85add <- F20MPDPred85 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20MPDPred85add <- rbind(F20MPDPred85add, F20Re[,c(1,7,2,9,8)])
F20MPDPred85add <- subset(F20MPDPred85add,F20MPDPred85add$Latitude%in%F20Re$Latitude)

#cumulative change
F20MPD85CC <- F20MPDPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F20MPD85CC <- na.omit(F20MPD85CC)
F20MPD85CC <- terra::vect(F20MPD85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20MPD85CC) <- "EPSG:4326"
F20MPD85CCsf <- st_as_sf(F20MPD85CC)
F20MPD85CCsf <- st_transform(F20MPD85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20MPD85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Oak/Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21,rcp26

#combine data
F21MPDPred26add <- F21MPDPred26 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21MPDPred26add <- rbind(F21MPDPred26add, F21Re[,c(1,7,2,9,8)])
F21MPDPred26add <- subset(F21MPDPred26add,F21MPDPred26add$Latitude%in%F21Re$Latitude)

#cumulative change
F21MPD26CC <- F21MPDPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F21MPD26CC <- na.omit(F21MPD26CC)
F21MPD26CC <- terra::vect(F21MPD26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21MPD26CC) <- "EPSG:4326"
F21MPD26CCsf <- st_as_sf(F21MPD26CC)
F21MPD26CCsf <- st_transform(F21MPD26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21MPD26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Oak/Hickory RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 45
#combine data
F21MPDPred45add <- F21MPDPred45 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21MPDPred45add <- rbind(F21MPDPred45add, F21Re[,c(1,7,2,9,8)])
F21MPDPred45add <- subset(F21MPDPred45add,F21MPDPred45add$Latitude%in%F21Re$Latitude)

#cumulative change
F21MPD45CC <- F21MPDPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F21MPD45CC <- na.omit(F21MPD45CC)
F21MPD45CC <- terra::vect(F21MPD45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21MPD45CC) <- "EPSG:4326"
F21MPD45CCsf <- st_as_sf(F21MPD45CC)
F21MPD45CCsf <- st_transform(F21MPD45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21MPD45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Oak/Hickory RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 85
#combine data
F21MPDPred85add <- F21MPDPred85 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21MPDPred85add <- rbind(F21MPDPred85add, F21Re[,c(1,7,2,9,8)])
F21MPDPred85add <- subset(F21MPDPred85add,F21MPDPred85add$Latitude%in%F21Re$Latitude)

#cumulative change
F21MPD85CC <- F21MPDPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F21MPD85CC <- na.omit(F21MPD85CC)
F21MPD85CC <- terra::vect(F21MPD85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21MPD85CC) <- "EPSG:4326"
F21MPD85CCsf <- st_as_sf(F21MPD85CC)
F21MPD85CCsf <- st_transform(F21MPD85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21MPD85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Oak/Hickory RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23,rcp26

#combine data
F23MPDPred26add <- F23MPDPred26 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23MPDPred26add <- rbind(F23MPDPred26add, F23Re[,c(1,7,2,9,8)])
F23MPDPred26add <- subset(F23MPDPred26add,F23MPDPred26add$Latitude%in%F23Re$Latitude)

#cumulative change
F23MPD26CC <- F23MPDPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F23MPD26CC <- na.omit(F23MPD26CC)
F23MPD26CC <- terra::vect(F23MPD26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23MPD26CC) <- "EPSG:4326"
F23MPD26CCsf <- st_as_sf(F23MPD26CC)
F23MPD26CCsf <- st_transform(F23MPD26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23MPD26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 45
#combine data
F23MPDPred45add <- F23MPDPred45 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23MPDPred45add <- rbind(F23MPDPred45add, F23Re[,c(1,7,2,9,8)])
F23MPDPred45add <- subset(F23MPDPred45add,F23MPDPred45add$Latitude%in%F23Re$Latitude)

#cumulative change
F23MPD45CC <- F23MPDPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F23MPD45CC <- na.omit(F23MPD45CC)
F23MPD45CC <- terra::vect(F23MPD45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23MPD45CC) <- "EPSG:4326"
F23MPD45CCsf <- st_as_sf(F23MPD45CC)
F23MPD45CCsf <- st_transform(F23MPD45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23MPD45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 85
#combine data
F23MPDPred85add <- F23MPDPred85 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23MPDPred85add <- rbind(F23MPDPred85add, F23Re[,c(1,7,2,9,8)])
F23MPDPred85add <- subset(F23MPDPred85add,F23MPDPred85add$Latitude%in%F23Re$Latitude)

#cumulative change
F23MPD85CC <- F23MPDPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F23MPD85CC <- na.omit(F23MPD85CC)
F23MPD85CC <- terra::vect(F23MPD85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23MPD85CC) <- "EPSG:4326"
F23MPD85CCsf <- st_as_sf(F23MPD85CC)
F23MPD85CCsf <- st_transform(F23MPD85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23MPD85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24,rcp26

#combine data
F24MPDPred26add <- F24MPDPred26 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24MPDPred26add <- rbind(F24MPDPred26add, F24Re[,c(1,7,2,9,8)])
F24MPDPred26add <- subset(F24MPDPred26add,F24MPDPred26add$Latitude%in%F24Re$Latitude)

#cumulative change
F24MPD26CC <- F24MPDPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F24MPD26CC <- na.omit(F24MPD26CC)
F24MPD26CC <- terra::vect(F24MPD26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24MPD26CC) <- "EPSG:4326"
F24MPD26CCsf <- st_as_sf(F24MPD26CC)
F24MPD26CCsf <- st_transform(F24MPD26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24MPD26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Maple/Beech/Birch RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 45
#combine data
F24MPDPred45add <- F24MPDPred45 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24MPDPred45add <- rbind(F24MPDPred45add, F24Re[,c(1,7,2,9,8)])
F24MPDPred45add <- subset(F24MPDPred45add,F24MPDPred45add$Latitude%in%F24Re$Latitude)

#cumulative change
F24MPD45CC <- F24MPDPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F24MPD45CC <- na.omit(F24MPD45CC)
F24MPD45CC <- terra::vect(F24MPD45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24MPD45CC) <- "EPSG:4326"
F24MPD45CCsf <- st_as_sf(F24MPD45CC)
F24MPD45CCsf <- st_transform(F24MPD45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24MPD45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Maple/Beech/Birch RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 85
#combine data
F24MPDPred85add <- F24MPDPred85 %>%
  pivot_longer(
    cols = starts_with("MPD"),
    names_to = "MPDColumn",
    values_to = "MeanPairwiseDistance"
  ) %>%
  mutate(
    Year = as.numeric(gsub("MPD", "", MPDColumn)) + 2000
  ) %>%
  select(PlotCN, MeanPairwiseDistance, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24MPDPred85add <- rbind(F24MPDPred85add, F24Re[,c(1,7,2,9,8)])
F24MPDPred85add <- subset(F24MPDPred85add,F24MPDPred85add$Latitude%in%F24Re$Latitude)

#cumulative change
F24MPD85CC <- F24MPDPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    MPDon_Initial = MeanPairwiseDistance[Year == min(Year)][1],  
    MPDon_2080 = MeanPairwiseDistance[Year == 2080][1],         
    CumulativeChange = (MPDon_2080 - MPDon_Initial)
  ) %>%
  ungroup()

F24MPD85CC <- na.omit(F24MPD85CC)
F24MPD85CC <- terra::vect(F24MPD85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24MPD85CC) <- "EPSG:4326"
F24MPD85CCsf <- st_as_sf(F24MPD85CC)
F24MPD85CCsf <- st_transform(F24MPD85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24MPD85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mean Pairwise Distance"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Mean Pairwise Distance Cumulative Change 2002-2080, Maple/Beech/Birch RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))


#Mort
#F1,rcp26
#combine data
F1MortPred26add <- F1MortPred26 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1MortPred26add <- rbind(F1MortPred26add, F1Re[,c(1,26,2,9,8)])
F1MortPred26add <- subset(F1MortPred26add,F1MortPred26add$Latitude%in%F1Re$Latitude)

#cumulative change
#earliest year to 2080
F1Mort26CC <- F1MortPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F1Mort26CC <- na.omit(F1Mort26CC)
F1Mort26CC <- terra::vect(F1Mort26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Mort26CC) <- "EPSG:4326"
F1Mort26CCsf <- st_as_sf(F1Mort26CC)
F1Mort26CCsf <- st_transform(F1Mort26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Mort26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, White/Red/Jack Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F1 rcp 45
#combine data
F1MortPred45add <- F1MortPred45 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1MortPred45add <- rbind(F1MortPred45add, F1Re[,c(1,26,2,9,8)])
F1MortPred45add <- subset(F1MortPred45add,F1MortPred45add$Latitude%in%F1Re$Latitude)

#Cumulative Change
F1Mort45CC <- F1MortPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F1Mort45CC <- na.omit(F1Mort45CC)
F1Mort45CC <- terra::vect(F1Mort45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Mort45CC) <- "EPSG:4326"
F1Mort45CCsf <- st_as_sf(F1Mort45CC)
F1Mort45CCsf <- st_transform(F1Mort45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Mort45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, White/Red/Jack Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))


#F1 rcp 85
#combine data
F1MortPred85add <- F1MortPred85 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1MortPred85add <- rbind(F1MortPred85add, F1Re[,c(1,26,2,9,8)])
F1MortPred85add <- subset(F1MortPred85add,F1MortPred85add$Latitude%in%F1Re$Latitude)

#cumulative change
F1Mort85CC <- F1MortPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F1Mort85CC <- na.omit(F1Mort85CC)
F1Mort85CC <- terra::vect(F1Mort85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1Mort85CC) <- "EPSG:4326"
F1Mort85CCsf <- st_as_sf(F1Mort85CC)
F1Mort85CCsf <- st_transform(F1Mort85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1Mort85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, White/Red/Jack Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5,rcp26

#combine data
F5MortPred26add <- F5MortPred26 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5MortPred26add <- rbind(F5MortPred26add, F5Re[,c(1,26,2,9,8)])
F5MortPred26add <- subset(F5MortPred26add,F5MortPred26add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Mort26CC <- F5MortPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F5Mort26CC <- na.omit(F5Mort26CC)
F5Mort26CC <- terra::vect(F5Mort26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Mort26CC) <- "EPSG:4326"
F5Mort26CCsf <- st_as_sf(F5Mort26CC)
F5Mort26CCsf <- st_transform(F5Mort26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Mort26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 45
#combine data
F5MortPred45add <- F5MortPred45 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5MortPred45add <- rbind(F5MortPred45add, F5Re[,c(1,26,2,9,8)])
F5MortPred45add <- subset(F5MortPred45add,F5MortPred45add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Mort45CC <- F5MortPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F5Mort45CC <- na.omit(F5Mort45CC)
F5Mort45CC <- terra::vect(F5Mort45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Mort45CC) <- "EPSG:4326"
F5Mort45CCsf <- st_as_sf(F5Mort45CC)
F5Mort45CCsf <- st_transform(F5Mort45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Mort45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 85
#combine data
F5MortPred85add <- F5MortPred85 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5MortPred85add <- rbind(F5MortPred85add, F5Re[,c(1,26,2,9,8)])
F5MortPred85add <- subset(F5MortPred85add,F5MortPred85add$Latitude%in%F5Re$Latitude)

#cumulative change
F5Mort85CC <- F5MortPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F5Mort85CC <- na.omit(F5Mort85CC)
F5Mort85CC <- terra::vect(F5Mort85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5Mort85CC) <- "EPSG:4326"
F5Mort85CCsf <- st_as_sf(F5Mort85CC)
F5Mort85CCsf <- st_transform(F5Mort85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5Mort85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20,rcp26

#combine data
F20MortPred26add <- F20MortPred26 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20MortPred26add <- rbind(F20MortPred26add, F20Re[,c(1,26,2,9,8)])
F20MortPred26add <- subset(F20MortPred26add,F20MortPred26add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Mort26CC <- F20MortPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F20Mort26CC <- na.omit(F20Mort26CC)
F20Mort26CC <- terra::vect(F20Mort26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Mort26CC) <- "EPSG:4326"
F20Mort26CCsf <- st_as_sf(F20Mort26CC)
F20Mort26CCsf <- st_transform(F20Mort26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Mort26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Oak/Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 45
#combine data
F20MortPred45add <- F20MortPred45 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20MortPred45add <- rbind(F20MortPred45add, F20Re[,c(1,26,2,9,8)])
F20MortPred45add <- subset(F20MortPred45add,F20MortPred45add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Mort45CC <- F20MortPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F20Mort45CC <- na.omit(F20Mort45CC)
F20Mort45CC <- terra::vect(F20Mort45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Mort45CC) <- "EPSG:4326"
F20Mort45CCsf <- st_as_sf(F20Mort45CC)
F20Mort45CCsf <- st_transform(F20Mort45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Mort45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Oak/Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 85
#combine data
F20MortPred85add <- F20MortPred85 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20MortPred85add <- rbind(F20MortPred85add, F20Re[,c(1,26,2,9,8)])
F20MortPred85add <- subset(F20MortPred85add,F20MortPred85add$Latitude%in%F20Re$Latitude)

#cumulative change
F20Mort85CC <- F20MortPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F20Mort85CC <- na.omit(F20Mort85CC)
F20Mort85CC <- terra::vect(F20Mort85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20Mort85CC) <- "EPSG:4326"
F20Mort85CCsf <- st_as_sf(F20Mort85CC)
F20Mort85CCsf <- st_transform(F20Mort85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20Mort85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Oak/Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21,rcp26

#combine data
F21MortPred26add <- F21MortPred26 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21MortPred26add <- rbind(F21MortPred26add, F21Re[,c(1,26,2,9,8)])
F21MortPred26add <- subset(F21MortPred26add,F21MortPred26add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Mort26CC <- F21MortPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F21Mort26CC <- na.omit(F21Mort26CC)
F21Mort26CC <- terra::vect(F21Mort26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Mort26CC) <- "EPSG:4326"
F21Mort26CCsf <- st_as_sf(F21Mort26CC)
F21Mort26CCsf <- st_transform(F21Mort26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Mort26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Oak/Hickory RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 45
#combine data
F21MortPred45add <- F21MortPred45 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21MortPred45add <- rbind(F21MortPred45add, F21Re[,c(1,26,2,9,8)])
F21MortPred45add <- subset(F21MortPred45add,F21MortPred45add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Mort45CC <- F21MortPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F21Mort45CC <- na.omit(F21Mort45CC)
F21Mort45CC <- terra::vect(F21Mort45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Mort45CC) <- "EPSG:4326"
F21Mort45CCsf <- st_as_sf(F21Mort45CC)
F21Mort45CCsf <- st_transform(F21Mort45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Mort45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Oak/Hickory RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 85
#combine data
F21MortPred85add <- F21MortPred85 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21MortPred85add <- rbind(F21MortPred85add, F21Re[,c(1,26,2,9,8)])
F21MortPred85add <- subset(F21MortPred85add,F21MortPred85add$Latitude%in%F21Re$Latitude)

#cumulative change
F21Mort85CC <- F21MortPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F21Mort85CC <- na.omit(F21Mort85CC)
F21Mort85CC <- terra::vect(F21Mort85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21Mort85CC) <- "EPSG:4326"
F21Mort85CCsf <- st_as_sf(F21Mort85CC)
F21Mort85CCsf <- st_transform(F21Mort85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21Mort85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Oak/Hickory RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23,rcp26

#combine data
F23MortPred26add <- F23MortPred26 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23MortPred26add <- rbind(F23MortPred26add, F23Re[,c(1,26,2,9,8)])
F23MortPred26add <- subset(F23MortPred26add,F23MortPred26add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Mort26CC <- F23MortPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F23Mort26CC <- na.omit(F23Mort26CC)
F23Mort26CC <- terra::vect(F23Mort26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Mort26CC) <- "EPSG:4326"
F23Mort26CCsf <- st_as_sf(F23Mort26CC)
F23Mort26CCsf <- st_transform(F23Mort26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Mort26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 45
#combine data
F23MortPred45add <- F23MortPred45 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23MortPred45add <- rbind(F23MortPred45add, F23Re[,c(1,26,2,9,8)])
F23MortPred45add <- subset(F23MortPred45add,F23MortPred45add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Mort45CC <- F23MortPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F23Mort45CC <- na.omit(F23Mort45CC)
F23Mort45CC <- terra::vect(F23Mort45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Mort45CC) <- "EPSG:4326"
F23Mort45CCsf <- st_as_sf(F23Mort45CC)
F23Mort45CCsf <- st_transform(F23Mort45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Mort45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 85
#combine data
F23MortPred85add <- F23MortPred85 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23MortPred85add <- rbind(F23MortPred85add, F23Re[,c(1,26,2,9,8)])
F23MortPred85add <- subset(F23MortPred85add,F23MortPred85add$Latitude%in%F23Re$Latitude)

#cumulative change
F23Mort85CC <- F23MortPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F23Mort85CC <- na.omit(F23Mort85CC)
F23Mort85CC <- terra::vect(F23Mort85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23Mort85CC) <- "EPSG:4326"
F23Mort85CCsf <- st_as_sf(F23Mort85CC)
F23Mort85CCsf <- st_transform(F23Mort85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23Mort85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24,rcp26
#combine data
F24MortPred26add <- F24MortPred26 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24MortPred26add <- rbind(F24MortPred26add, F24Re[,c(1,26,2,9,8)])
F24MortPred26add <- subset(F24MortPred26add,F24MortPred26add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Mort26CC <- F24MortPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F24Mort26CC <- na.omit(F24Mort26CC)
F24Mort26CC <- terra::vect(F24Mort26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Mort26CC) <- "EPSG:4326"
F24Mort26CCsf <- st_as_sf(F24Mort26CC)
F24Mort26CCsf <- st_transform(F24Mort26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Mort26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Maple/Beech/Birch RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 45
#combine data
F24MortPred45add <- F24MortPred45 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24MortPred45add <- rbind(F24MortPred45add, F24Re[,c(1,26,2,9,8)])
F24MortPred45add <- subset(F24MortPred45add,F24MortPred45add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Mort45CC <- F24MortPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F24Mort45CC <- na.omit(F24Mort45CC)
F24Mort45CC <- terra::vect(F24Mort45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Mort45CC) <- "EPSG:4326"
F24Mort45CCsf <- st_as_sf(F24Mort45CC)
F24Mort45CCsf <- st_transform(F24Mort45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Mort45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Maple/Beech/Birch RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 85
#combine data
F24MortPred85add <- F24MortPred85 %>%
  pivot_longer(
    cols = starts_with("Mort"),
    names_to = "MortColumn",
    values_to = "MortRate"
  ) %>%
  mutate(
    Year = as.numeric(gsub("Mort", "", MortColumn)) + 2000
  ) %>%
  select(PlotCN, MortRate, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24MortPred85add <- rbind(F24MortPred85add, F24Re[,c(1,26,2,9,8)])
F24MortPred85add <- subset(F24MortPred85add,F24MortPred85add$Latitude%in%F24Re$Latitude)

#cumulative change
F24Mort85CC <- F24MortPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    Mort_Initial = MortRate[Year == min(Year)][1],  
    Mort_2080 = MortRate[Year == 2080][1],         
    CumulativeChange = (Mort_2080 - Mort_Initial)
  ) %>%
  ungroup()

F24Mort85CC <- na.omit(F24Mort85CC)
F24Mort85CC <- terra::vect(F24Mort85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24Mort85CC) <- "EPSG:4326"
F24Mort85CCsf <- st_as_sf(F24Mort85CC)
F24Mort85CCsf <- st_transform(F24Mort85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24Mort85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Mortality Rate"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Tree Mortality Rate Cumulative Change 2002-2080, Maple/Beech/Birch RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))


#AQI
#F1,rcp26
#combine data
F1AQIPred26add <- F1AQIPred26 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1AQIPred26add <- rbind(F1AQIPred26add, F1Re[,c(1,14,2,9,8)])
F1AQIPred26add <- subset(F1AQIPred26add,F1AQIPred26add$Latitude%in%F1Re$Latitude)

#cumulative change
#earliest year to 2080
F1AQI26CC <- F1AQIPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F1AQI26CC <- na.omit(F1AQI26CC)
F1AQI26CC <- terra::vect(F1AQI26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1AQI26CC) <- "EPSG:4326"
F1AQI26CCsf <- st_as_sf(F1AQI26CC)
F1AQI26CCsf <- st_transform(F1AQI26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1AQI26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, White/Red/Jack Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F1 rcp 45
#combine data
F1AQIPred45add <- F1AQIPred45 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1AQIPred45add <- rbind(F1AQIPred45add, F1Re[,c(1,14,2,9,8)])
F1AQIPred45add <- subset(F1AQIPred45add,F1AQIPred45add$Latitude%in%F1Re$Latitude)

#Cumulative Change
F1AQI45CC <- F1AQIPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F1AQI45CC <- na.omit(F1AQI45CC)
F1AQI45CC <- terra::vect(F1AQI45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1AQI45CC) <- "EPSG:4326"
F1AQI45CCsf <- st_as_sf(F1AQI45CC)
F1AQI45CCsf <- st_transform(F1AQI45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1AQI45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, White/Red/Jack Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))


#F1 rcp 85
#combine data
F1AQIPred85add <- F1AQIPred85 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F1AQIPred85add <- rbind(F1AQIPred85add, F1Re[,c(1,14,2,9,8)])
F1AQIPred85add <- subset(F1AQIPred85add,F1AQIPred85add$Latitude%in%F1Re$Latitude)

#cumulative change
F1AQI85CC <- F1AQIPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F1AQI85CC <- na.omit(F1AQI85CC)
F1AQI85CC <- terra::vect(F1AQI85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F1AQI85CC) <- "EPSG:4326"
F1AQI85CCsf <- st_as_sf(F1AQI85CC)
F1AQI85CCsf <- st_transform(F1AQI85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F1AQI85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, White/Red/Jack Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5,rcp26

#combine data
F5AQIPred26add <- F5AQIPred26 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5AQIPred26add <- rbind(F5AQIPred26add, F5Re[,c(1,14,2,9,8)])
F5AQIPred26add <- subset(F5AQIPred26add,F5AQIPred26add$Latitude%in%F5Re$Latitude)

#cumulative change
F5AQI26CC <- F5AQIPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F5AQI26CC <- na.omit(F5AQI26CC)
F5AQI26CC <- terra::vect(F5AQI26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5AQI26CC) <- "EPSG:4326"
F5AQI26CCsf <- st_as_sf(F5AQI26CC)
F5AQI26CCsf <- st_transform(F5AQI26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5AQI26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 45
#combine data
F5AQIPred45add <- F5AQIPred45 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5AQIPred45add <- rbind(F5AQIPred45add, F5Re[,c(1,14,2,9,8)])
F5AQIPred45add <- subset(F5AQIPred45add,F5AQIPred45add$Latitude%in%F5Re$Latitude)

#cumulative change
F5AQI45CC <- F5AQIPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F5AQI45CC <- na.omit(F5AQI45CC)
F5AQI45CC <- terra::vect(F5AQI45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5AQI45CC) <- "EPSG:4326"
F5AQI45CCsf <- st_as_sf(F5AQI45CC)
F5AQI45CCsf <- st_transform(F5AQI45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5AQI45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F5 rcp 85
#combine data
F5AQIPred85add <- F5AQIPred85 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F5AQIPred85add <- rbind(F5AQIPred85add, F5Re[,c(1,14,2,9,8)])
F5AQIPred85add <- subset(F5AQIPred85add,F5AQIPred85add$Latitude%in%F5Re$Latitude)

#cumulative change
F5AQI85CC <- F5AQIPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F5AQI85CC <- na.omit(F5AQI85CC)
F5AQI85CC <- terra::vect(F5AQI85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F5AQI85CC) <- "EPSG:4326"
F5AQI85CCsf <- st_as_sf(F5AQI85CC)
F5AQI85CCsf <- st_transform(F5AQI85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F5AQI85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Loblolly/Short Leaf Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20,rcp26

#combine data
F20AQIPred26add <- F20AQIPred26 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20AQIPred26add <- rbind(F20AQIPred26add, F20Re[,c(1,14,2,9,8)])
F20AQIPred26add <- subset(F20AQIPred26add,F20AQIPred26add$Latitude%in%F20Re$Latitude)

#cumulative change
F20AQI26CC <- F20AQIPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F20AQI26CC <- na.omit(F20AQI26CC)
F20AQI26CC <- terra::vect(F20AQI26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20AQI26CC) <- "EPSG:4326"
F20AQI26CCsf <- st_as_sf(F20AQI26CC)
F20AQI26CCsf <- st_transform(F20AQI26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20AQI26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Oak/Pine RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 45
#combine data
F20AQIPred45add <- F20AQIPred45 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20AQIPred45add <- rbind(F20AQIPred45add, F20Re[,c(1,14,2,9,8)])
F20AQIPred45add <- subset(F20AQIPred45add,F20AQIPred45add$Latitude%in%F20Re$Latitude)

#cumulative change
F20AQI45CC <- F20AQIPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F20AQI45CC <- na.omit(F20AQI45CC)
F20AQI45CC <- terra::vect(F20AQI45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20AQI45CC) <- "EPSG:4326"
F20AQI45CCsf <- st_as_sf(F20AQI45CC)
F20AQI45CCsf <- st_transform(F20AQI45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20AQI45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Oak/Pine RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F20 rcp 85
#combine data
F20AQIPred85add <- F20AQIPred85 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F20AQIPred85add <- rbind(F20AQIPred85add, F20Re[,c(1,14,2,9,8)])
F20AQIPred85add <- subset(F20AQIPred85add,F20AQIPred85add$Latitude%in%F20Re$Latitude)

#cumulative change
F20AQI85CC <- F20AQIPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F20AQI85CC <- na.omit(F20AQI85CC)
F20AQI85CC <- terra::vect(F20AQI85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F20AQI85CC) <- "EPSG:4326"
F20AQI85CCsf <- st_as_sf(F20AQI85CC)
F20AQI85CCsf <- st_transform(F20AQI85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F20AQI85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Oak/Pine RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21,rcp26

#combine data
F21AQIPred26add <- F21AQIPred26 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21AQIPred26add <- rbind(F21AQIPred26add, F21Re[,c(1,14,2,9,8)])
F21AQIPred26add <- subset(F21AQIPred26add,F21AQIPred26add$Latitude%in%F21Re$Latitude)

#cumulative change
F21AQI26CC <- F21AQIPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F21AQI26CC <- na.omit(F21AQI26CC)
F21AQI26CC <- terra::vect(F21AQI26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21AQI26CC) <- "EPSG:4326"
F21AQI26CCsf <- st_as_sf(F21AQI26CC)
F21AQI26CCsf <- st_transform(F21AQI26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21AQI26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Oak/Hickory RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 45
#combine data
F21AQIPred45add <- F21AQIPred45 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21AQIPred45add <- rbind(F21AQIPred45add, F21Re[,c(1,14,2,9,8)])
F21AQIPred45add <- subset(F21AQIPred45add,F21AQIPred45add$Latitude%in%F21Re$Latitude)

#cumulative change
F21AQI45CC <- F21AQIPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F21AQI45CC <- na.omit(F21AQI45CC)
F21AQI45CC <- terra::vect(F21AQI45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21AQI45CC) <- "EPSG:4326"
F21AQI45CCsf <- st_as_sf(F21AQI45CC)
F21AQI45CCsf <- st_transform(F21AQI45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21AQI45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Oak/Hickory RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F21 rcp 85
#combine data
F21AQIPred85add <- F21AQIPred85 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F21AQIPred85add <- rbind(F21AQIPred85add, F21Re[,c(1,14,2,9,8)])
F21AQIPred85add <- subset(F21AQIPred85add,F21AQIPred85add$Latitude%in%F21Re$Latitude)

#cumulative change
F21AQI85CC <- F21AQIPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F21AQI85CC <- na.omit(F21AQI85CC)
F21AQI85CC <- terra::vect(F21AQI85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F21AQI85CC) <- "EPSG:4326"
F21AQI85CCsf <- st_as_sf(F21AQI85CC)
F21AQI85CCsf <- st_transform(F21AQI85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F21AQI85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Oak/Hickory RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23,rcp26

#combine data
F23AQIPred26add <- F23AQIPred26 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23AQIPred26add <- rbind(F23AQIPred26add, F23Re[,c(1,14,2,9,8)])
F23AQIPred26add <- subset(F23AQIPred26add,F23AQIPred26add$Latitude%in%F23Re$Latitude)

#cumulative change
F23AQI26CC <- F23AQIPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F23AQI26CC <- na.omit(F23AQI26CC)
F23AQI26CC <- terra::vect(F23AQI26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23AQI26CC) <- "EPSG:4326"
F23AQI26CCsf <- st_as_sf(F23AQI26CC)
F23AQI26CCsf <- st_transform(F23AQI26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23AQI26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 45
#combine data
F23AQIPred45add <- F23AQIPred45 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23AQIPred45add <- rbind(F23AQIPred45add, F23Re[,c(1,14,2,9,8)])
F23AQIPred45add <- subset(F23AQIPred45add,F23AQIPred45add$Latitude%in%F23Re$Latitude)

#cumulative change
F23AQI45CC <- F23AQIPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F23AQI45CC <- na.omit(F23AQI45CC)
F23AQI45CC <- terra::vect(F23AQI45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23AQI45CC) <- "EPSG:4326"
F23AQI45CCsf <- st_as_sf(F23AQI45CC)
F23AQI45CCsf <- st_transform(F23AQI45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23AQI45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F23 rcp 85
#combine data
F23AQIPred85add <- F23AQIPred85 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F23AQIPred85add <- rbind(F23AQIPred85add, F23Re[,c(1,14,2,9,8)])
F23AQIPred85add <- subset(F23AQIPred85add,F23AQIPred85add$Latitude%in%F23Re$Latitude)

#cumulative change
F23AQI85CC <- F23AQIPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F23AQI85CC <- na.omit(F23AQI85CC)
F23AQI85CC <- terra::vect(F23AQI85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F23AQI85CC) <- "EPSG:4326"
F23AQI85CCsf <- st_as_sf(F23AQI85CC)
F23AQI85CCsf <- st_transform(F23AQI85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F23AQI85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Elm/Ash/Cottonwood RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24,rcp26

#combine data
F24AQIPred26add <- F24AQIPred26 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24AQIPred26add <- rbind(F24AQIPred26add, F24Re[,c(1,14,2,9,8)])
F24AQIPred26add <- subset(F24AQIPred26add,F24AQIPred26add$Latitude%in%F24Re$Latitude)

#cumulative change
F24AQI26CC <- F24AQIPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F24AQI26CC <- na.omit(F24AQI26CC)
F24AQI26CC <- terra::vect(F24AQI26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24AQI26CC) <- "EPSG:4326"
F24AQI26CCsf <- st_as_sf(F24AQI26CC)
F24AQI26CCsf <- st_transform(F24AQI26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24AQI26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Maple/Beech/Birch RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 45
#combine data
F24AQIPred45add <- F24AQIPred45 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24AQIPred45add <- rbind(F24AQIPred45add, F24Re[,c(1,14,2,9,8)])
F24AQIPred45add <- subset(F24AQIPred45add,F24AQIPred45add$Latitude%in%F24Re$Latitude)

#cumulative change
F24AQI45CC <- F24AQIPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F24AQI45CC <- na.omit(F24AQI45CC)
F24AQI45CC <- terra::vect(F24AQI45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24AQI45CC) <- "EPSG:4326"
F24AQI45CCsf <- st_as_sf(F24AQI45CC)
F24AQI45CCsf <- st_transform(F24AQI45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24AQI45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Maple/Beech/Birch RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#F24 rcp 85
#combine data
F24AQIPred85add <- F24AQIPred85 %>%
  pivot_longer(
    cols = starts_with("AQI"),
    names_to = "AQIColumn",
    values_to = "MaxAQI"
  ) %>%
  mutate(
    Year = as.numeric(gsub("AQI", "", AQIColumn)) + 2000
  ) %>%
  select(PlotCN, MaxAQI, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

F24AQIPred85add <- rbind(F24AQIPred85add, F24Re[,c(1,14,2,9,8)])
F24AQIPred85add <- subset(F24AQIPred85add,F24AQIPred85add$Latitude%in%F24Re$Latitude)

#cumulative change
F24AQI85CC <- F24AQIPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    AQI_Initial = MaxAQI[Year == min(Year)][1],  
    AQI_2080 = MaxAQI[Year == 2080][1],         
    CumulativeChange = (AQI_2080 - AQI_Initial)
  ) %>%
  ungroup()

F24AQI85CC <- na.omit(F24AQI85CC)
F24AQI85CC <- terra::vect(F24AQI85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(F24AQI85CC) <- "EPSG:4326"
F24AQI85CCsf <- st_as_sf(F24AQI85CC)
F24AQI85CCsf <- st_transform(F24AQI85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = F24AQI85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Air Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Air Quality Index Cumulative Change 2002-2080, Maple/Beech/Birch RCP85")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

#Soil Quality Index
#combine data
#SQIdata$SQIPCT <- SQIdata$SQIPCT/100
SQIPred26add <- SQIPred26 %>%
  pivot_longer(
    cols = starts_with("SQI"),
    names_to = "SQIColumn",
    values_to = "SQIPCT"
  ) %>%
  mutate(
    Year = as.numeric(gsub("SQI", "", SQIColumn)) + 2000
  ) %>%
  select(PlotCN, SQIPCT, Year) %>%
  left_join(
    SQIdata %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )
SQIPred26add$SQIPCT <- SQIPred26add$SQIPCT*100
SQIPred26add <- rbind(SQIPred26add, SQIdata[,c(1,3,2,5,4)])
SQIPred26add <- distinct(SQIPred26add)



#Cumulative change Earliest year to 2080
SQI26CC <- SQIPred26add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    SQI_Initial = SQIPCT[Year == min(Year)][1],  
    SQI_2080 = SQIPCT[Year == 2080][1],         
    CumulativeChange = (SQI_2080 - SQI_Initial)
  ) %>%
  ungroup()
SQI26CC <- na.omit(SQI26CC)


SQI26CC <- terra::vect(SQI26CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(SQI26CC) <- "EPSG:4326"
SQI26CCsf <- st_as_sf(SQI26CC)
SQI26CCsf <- st_transform(SQI26CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = SQI26CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Soil Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Soil Quality Index Cumulative Change 2001-2080, RCP26")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

# rcp 45
#combine data
SQIPred45add <- SQIPred45 %>%
  pivot_longer(
    cols = starts_with("SQI"),
    names_to = "SQIColumn",
    values_to = "SQIPCT"
  ) %>%
  mutate(
    Year = as.numeric(gsub("SQI", "", SQIColumn)) + 2000
  ) %>%
  select(PlotCN, SQIPCT, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

SQIPred45add$SQIPCT <- SQIPred45add$SQIPCT*100
SQIPred45add <- rbind(SQIPred45add, SQIdata[,c(1,3,2,5,4)])
SQIPred45add <- distinct(SQIPred45add)

#cumulative change
#Earliest year to 2080
SQI45CC <- SQIPred45add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    SQI_Initial = SQIPCT[Year == min(Year)][1],  
    SQI_2080 = SQIPCT[Year == 2080][1],         
    CumulativeChange = (SQI_2080 - SQI_Initial)
  ) %>%
  ungroup()
SQI45CC <- na.omit(SQI45CC)

SQI45CC <- terra::vect(SQI45CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(SQI45CC) <- "EPSG:4326"
SQI45CCsf <- st_as_sf(SQI45CC)
SQI45CCsf <- st_transform(SQI45CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = SQI45CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Soil Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  theme_minimal() +
  theme_void()+  
  labs(title = "Soil Quality Index Cumulative Change 2001-2080, RCP45")+
  theme(plot.title = element_text(size=25))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  theme(plot.title = element_text(hjust=0.5))+
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))

# rcp 85
#combine data
SQIPred85add <- SQIPred85 %>%
  pivot_longer(
    cols = starts_with("SQI"),
    names_to = "SQIColumn",
    values_to = "SQIPCT"
  ) %>%
  mutate(
    Year = as.numeric(gsub("SQI", "", SQIColumn)) + 2000
  ) %>%
  select(PlotCN, SQIPCT, Year) %>%
  left_join(
    allFIA %>% select(PlotCN, Longitude, Latitude),
    by = "PlotCN"
  )

SQIPred85add$SQIPCT <- SQIPred85add$SQIPCT*100
SQIPred85add <- rbind(SQIPred85add, SQIdata[,c(1,3,2,5,4)])
SQIPred85add <- distinct(SQIPred85add)

#cumulative change
#Earliest year to 2080
SQI85CC <- SQIPred85add %>%
  group_by(Latitude, Longitude) %>%
  arrange(Year, .by_group = TRUE) %>%
  reframe(
    Initial_Year = min(Year),
    SQI_Initial = SQIPCT[Year == min(Year)][1],  
    SQI_2080 = SQIPCT[Year == 2080][1],         
    CumulativeChange = (SQI_2080 - SQI_Initial)
  ) %>%
  ungroup()
SQI85CC <- na.omit(SQI85CC)

SQI85CC <- terra::vect(SQI85CC, geom=c("Longitude","Latitude"),crs=crs(midatlmap))
crs(SQI85CC) <- "EPSG:4326"
SQI85CCsf <- st_as_sf(SQI85CC)
SQI85CCsf <- st_transform(SQI85CCsf, st_crs(midcut_sf))

ggplot() +
  geom_sf(data = midatl_sf, 
          fill = "grey80", 
          color = "grey50",
          linewidth = 0.3) +
  geom_sf(data = midcut_sf, 
          fill = NA, 
          color = "black",
          linewidth = 0.5) +
  geom_sf(data = SQI85CCsf,
          aes(color = CumulativeChange),  
          shape = 15,          
          size = 3,
          alpha = 1,
          stroke = 0) +
  scale_color_gradient2(
    low = "blue",      
    mid = "white",     
    high = "red",      
    midpoint = 0,      
    name = "Cumulative Change - Soil Quality Index"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_col = "black",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  theme_minimal() +
  theme_void()+  
  labs(title ="Soil Quality Index Cumulative Change 2001-2080, RCP85")+
  theme(plot.title = element_text(size=25))+
  theme(plot.title = element_text(hjust=0.5))+
  coord_sf(
    xlim = c(combined_ext[1], combined_ext[2]),
    ylim = c(combined_ext[3], combined_ext[4]),
    expand = TRUE
  ) +
  annotation_north_arrow(
    location = "tr",        
    which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    ))
