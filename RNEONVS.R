setwd("~/Desktop/NEON/NEON_struct-woody-plant/VS_data")
library(dplyr)
vs2016.09 <-read.csv(file = 'NEON.UNDE.VS.2016.09.csv')
sp2016.09<-read.csv(file = 'Species2016.09.csv')
df.vs2016.09<- as.data.frame(vs2016.09)
df.sp2016.09<- as.data.frame(sp2016.09)
#need to match up individual ids for both, then delete rows from species that don't match with VS
#left outer join
spvs2016.09<- merge(x=df.vs2016.09, y=df.sp2016.09, by = "individualID", all.x= TRUE, header = TRUE)
se2016.09 <- subset(spvs2016.09, select = c("plotID.x", "individualID", "plantStatus", "stemDiameter","measurementHeight","height","maxCrownDiameter","basalStemDiameter","scientificName"))
#repeat for other datasets
vs2016.11 <-read.csv(file = 'NEON.UNDE.VS.2016.11.csv')
sp2016.11<-read.csv(file = 'Species2016.11.csv')
df.vs2016.11<- as.data.frame(vs2016.11)
df.sp2016.11<- as.data.frame(sp2016.11)
spvs2016.11<- merge(x=df.vs2016.11, y=df.sp2016.11, by = "individualID", all.x= TRUE, header = TRUE)
se2016.11 <- subset(spvs2016.11, select = c("plotID.x", "individualID", "plantStatus", "stemDiameter","measurementHeight","height","maxCrownDiameter","basalStemDiameter","scientificName"))
#combine all data sets for the year
vs2016.t <- rbind(se2016.09,se2016.11)
vs2016.t <- with(vs2016.t, vs2016.t[order(vs2016.t[,1], vs2016.t[,2]), ])
write.csv(vs2016.t,"VS2016ALL.csv")
#repeat for other years





