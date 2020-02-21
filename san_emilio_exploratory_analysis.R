#This script will become an analysis for Chapter 3 of the thesis, and/or the ForestGEO publication.
#Needs to make use of data pulled out of the census_validation.R script

library(ggplot2)

#Basic geometry of San Emilio - stated as 16ha plot, 160,000m^2 
#Reality - A set of 20x20m plots, from 0 - 240 and 140 - 780,
#so the dimensions are 12x32 = 384 * 400 = 153,600m^2
stakes <- read.csv('data/topo2.csv', header = FALSE)
#ggplot(stakes, aes(x=V2, y=V3)) + geom_point()

#Precise stem information is in meters between the bounds: 
#0 < X < 240, 140 < Y < 780
stems_1976 <- read.csv("data/xtot1976.txt", header=FALSE, sep="\t")

#There is an extra block of stems closer to the road after enquist showed up
#Approx: 0 < X < 25, 140 < Y < 350
stems_1996 <- read.csv("data/xxtot1996.txt", header=FALSE, sep="\t")
a_stems_1996 <- read.csv("data/SanEmilio_1996.txt", header=FALSE, sep="\t")

#The extra block of stems closer to the road extends to Y ~ 450
#There is not clearly any tag information in this dataset
stems_2006 <- read.csv("data/xtot2006.raw.data.csv")[,c("sp", "dbh", "gx", "gx")]
#gx, gy are the exact stem locations in meters
#ggplot(stems_2006, aes(x=gx, y=gy,size=dbh, color=sp)) + geom_point()

#The tag ID information is a complete mess - analyses based on the time series 
#might be extremely dubious. What did we get in to?
census_names_76 <- c("SPP", "X", "Y", "DBH", "ID")
census_names_96 <- c("SPP", "X", "Y", "DBH_76", "DBH", "ID")
a_census_names_96 <- c("SPP", "X", "Y", "DBH")
census_names_06 <- c("SPP", "DBH", "X", "Y")

colnames(stems_1976) <- census_names_76
colnames(stems_1996) <- census_names_96
colnames(a_stems_1996) <- a_census_names_96
colnames(stems_2006) <- census_names_06

#stems_1996 <- a_stems_1996

stems_1976$DBH <- as.numeric(stems_1976$DBH)
stems_1996$DBH <- as.numeric(stems_1996$DBH)/10 #centimeters multiplied by 10? This data is a mess

#These stakes do not depict the stems with negative X values
topo <- read.csv("data/topo2.csv")
stake_names <- c("STAKE", "X", "Y", "ELEVATION")
colnames(topo) <- stake_names
#ggplot(topo, aes(x=X, y=Y, color=ELEVATION)) + geom_point(size=5)

species_table <- read.csv("data/Species_number_translation.csv")

summary_76_96 <- read.csv("SanEm_Sum_data.csv")
dominant_76 <- which(summary_76_96$Abundance_1976 > 100)
dominant_96 <- which(summary_76_96$Abundance_1996 > 100)

#Check species with more than 100 individuals in each census
#summary_76_96$Genus_Species[dominant_76]
#summary_76_96$Genus_Species[dominant_96]

#Predictions of the GTFSD
#I. Density for each size class
#II. Nearest neighbor distance for each size class
#III. Basal diameter for each size class
#IV. Height and canopy radius scaling for each size class (in addition to canopy spacing - new questions related to canopy overlap)
#V. Mortality rate for each size class
#VI. Sap flux for each size class (corresponds to resource use, growth rates, etc.)

#Evaluation methods applicable to each:
#Census:              TLS:                  Aerial:
#I, II, III, V, VI    I, II, III, IV        I, II, III, IV
#
#We can easily write regressions for Aerial data, and maybe dream up new predictions with Erica. I want to write Math with her

#Stem scaling regressions for 2006 data - many of these predictions fail for the largest size classes
bin = 1
size_classes <- seq(1, 201, by=bin)
densities <- data.frame(density=integer(), size=integer(), year=factor())
for(i in size_classes)
{
  class_density_76 <- length(which(stems_1976$DBH >= i & stems_1976$DBH < i+bin))
  class_density_96 <- length(which(stems_1996$DBH >= i & stems_1996$DBH < i+bin))
  class_density_06 <- length(which(stems_2006$DBH >= i & stems_2006$DBH < i+bin))
  #We could add nearest neighbor distances, canopy and height information for remote sensing analyses
  #Novel component is LiDAR/remote sensing methods that get towards the true distribution, so we don't assume anything allometrically
  
  new_class_76 <- data.frame(density=class_density_76, size=i, year=1976)
  new_class_96 <- data.frame(density=class_density_96, size=i, year=1996)
  new_class_06 <- data.frame(density=class_density_06, size=i, year=2006)
  densities <- rbind(densities, new_class_76)
  densities <- rbind(densities, new_class_96)
  densities <- rbind(densities, new_class_06)
}
densities <- densities[which(densities$density != 0),]
#ggplot(densities, aes(x=size, y=log(density), color=year)) + geom_point()


