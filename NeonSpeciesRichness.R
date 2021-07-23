#Annual Species Richness data per plot + per site(maybe)
#Tree species first
setwd("~/Desktop/NEON/NEON_struct-woody-plant/VS_data")
VS2016ALL <- read.csv(file = "VS2016ALL.csv")
VS2017ALL <- read.csv(file = "VS2017ALL.csv")
VS2018ALL <- read.csv(file = "VS2018ALL.csv")
VS2019ALL <- read.csv(file = "VS2019ALL.csv")
VS2020ALL <- read.csv(file = "VS2020ALL.csv")
#specieslist(repeated)
specieslist16 <- as.data.frame(unique(VS2016ALL$scientificName))
names(specieslist16) <- "Species16"
specieslist16 <- specieslist16 %>% arrange(Species)
specieslist16 <- as.data.frame(specieslist16[complete.cases(specieslist16$Species),])
write.csv(specieslist16, "specieslist16.csv")
#specieslist annual?

#speciesrichness(repeated)
SpecRich16 <- VS2016ALL %>% group_by(plotID.x) %>% summarise(length(unique(scientificName)))
SpecRich16 <- SpecRich16 %>% rename( "2016" = `length(unique(scientificName))`)
#combine into one doc
SpecRichAll <- merge(SpecRich16, SpecRich17, by = "plotID.x", all.x = TRUE, all.y = TRUE)
SpecRichAll <- merge(SpecRichAll, SpecRich18, by = "plotID.x", all.x = TRUE, all.y = TRUE)
SpecRichAll <- merge(SpecRichAll, SpecRich19, by = "plotID.x", all.x = TRUE, all.y = TRUE)
SpecRichAll <- merge(SpecRichAll, SpecRich20, by = "plotID.x", all.x = TRUE, all.y = TRUE)
write.csv(SpecRichAll, "AnnualSpeciesRichness.csv")
#Non-herbal perennial veg(ferns :/)
setwd("~/Desktop/NEON/NEON_struct-non-herb-perennial-veg/PVData")
Pveg16.1 <- read.csv(file = "NEONPV2016-08.csv")
Pveg16.2 <- read.csv(file = "NEONPV2016-09.csv")
Pveg17 <- read.csv(file = "NEONPV2017-08.csv")
Pveg18 <- read.csv(file = "NEONPV2018-08.csv")
Pveg19.1 <- read.csv(file = "NEONPV2019-07.csv")
Pveg19.2 <- read.csv(file = "NEONPV2019-08.csv")

Pveg16 <- rbind(Pveg16.1, Pveg16.2)
Pveg19 <- rbind(Pveg19.1, Pveg19.2)

SpeciesPV16 <- as.data.frame(unique(Pveg16$scientificName))
SpeciesPV17 <- as.data.frame(unique(Pveg17$scientificName))
SpeciesPV18 <- as.data.frame(unique(Pveg18$scientificName))
SpeciesPV19 <- as.data.frame(unique(Pveg19$scientificName))

write.csv(SpeciesPV16, "PVSpecies16.csv")
write.csv(SpeciesPV17, "PVSpecies17.csv")
write.csv(SpeciesPV18, "PVSpecies18.csv")
write.csv(SpeciesPV19, "PVSpecies19.csv")


