library(ggplot2)

stems_1976 <- read.csv("data/xtot1976.txt", sep="\t")
stems_1996 <- read.csv("data/xxtot1996.txt", sep="\t")
stems_2006 <- read.csv("data/xtot2006.raw.data.csv")
census_names <- c("SPP", "X", "Y", "DBH", "ID")

colnames(stems_1976) <- census_names
colnames(stems_1996) <- census_names

topo <- read.csv("data/topo2.csv")
stake_names <- c("STAKE", "X", "Y", "ELEVATION")

species_table <- read.csv("data/Species_number_translation.csv")

summary_76_96 <- read.csv("SanEm_Sum_data.csv")

summary_76_96 <- read.csv("SanEm_Sum_data.csv")
dominant_76 <- which(summary_76_96$Abundance_1976 > 100)
dominant_96 <- which(summary_76_96$Abundance_1996 > 100)

#Check species with more than 100 individuals in each census
summary_76_96$Genus_Species[dominant_76]
summary_76_96$Genus_Species[dominant_966]
