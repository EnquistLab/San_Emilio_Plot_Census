#This script matches 2019 census data with stems from previous inventories based on spatial location and species IDs


species_table <- read.csv("data/Species_number_translation.csv")
colnames(species_table)[1] <- "SPP"

stems_1996 <- read.csv("data/xxtot1996.txt", header=FALSE, sep="\t")
census_names_96 <- c("SPP", "X", "Y", "DBH_76", "DBH", "ID")
colnames(stems_1996) <- census_names_96

#This is a top-level stem matching function assuming full data coverage (SPP, DBH, XY, IDs)
#Demographic parameters:
#-Growth
#-Mortality
#-Birth is a separate function using potentially only the 2006-2019 data.

#General idea:
#Starting in an early census year, attempt to match older trees (>10cm DBH) to their counterparts in later years
#Exclude lianas
#Loop through 1976, and match to 1996, 2006, and 2019.
#Matching 1976 to 1996 was already done by Brian, but discrepancies between DBH in his columns and the raw '76 data
#Loop through the remainders in 1996, and match to 2006, 2019.
#Loop through the remainders in 2006, and match to 2019.
#
#Loop:
#	Subset trees by plot, then 
#	Subset trees by species, then
#	Subset trees by a relatively generous distance threshold (around 5 meters), then
#	Check for older (larger) trees. If found, Compute delta-DBHs
#	If a tree disappears, register mortality

#	Use old nails/tags as a ground-truth
match_stems <- function(stems)
{
	
}

#Use stems with IDs from the '70s to report on the error in spatial positions and plot assignments
test_stem_matching <- function()
{
	trees_70s <- which(!is.na(stems_2019$NAIL) & stems_2019$NAIL > 0) #Trees occupied by leafcutters in 1976
	trees_90s <- which(!is.na(stems_2019$NAIL) & stems_2019$NAIL == 0) #trees > 10cm DBH at the time
	matched_76 <- which(!stems_1996$DBH_76 == 0)
}