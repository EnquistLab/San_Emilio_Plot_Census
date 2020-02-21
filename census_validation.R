###
#Top-level script for cleaning and standardizing data across all census years
###

library(ggplot2)
library(dplyr)

source('sheets_integration.R')

species_table <- read.csv("data/Species_number_translation.csv")
colnames(species_table)[1] <- "SPP"
species_key <- read.csv("data/species_code_key.csv")

#Collect all data from all the censuses together and standardize them:
stems_1976 <- read.csv("data/xtot1976.txt", header=FALSE, sep="\t", stringsAsFactors=FALSE)
stems_1996 <- read.csv("data/xxtot1996.txt", header=FALSE, sep="\t", stringsAsFactors=FALSE)
stems_2006 <- read.csv("data/xtot2006.raw.data.csv")[,c("sp", "dbh", "gx", "gx")]
stems_2019 <- read_raw_data()

census_names_76 <- c("SPP", "X", "Y", "DBH", "ID")
census_names_96 <- c("SPP", "X", "Y", "DBH_76", "DBH", "ID")
census_names_06 <- c("SPP", "DBH", "X", "Y")

colnames(stems_1976) <- census_names_76
colnames(stems_1996) <- census_names_96
colnames(stems_2006) <- census_names_06

stems_1976$DBH <- as.numeric(stems_1976$DBH)
stems_1996$DBH <- as.numeric(stems_1996$DBH)

munge_digits <- function(x){
    if(!is.na(x))
    {
        len = nchar(x)
        if(len == 1)
            return(paste("0",x,sep=""))
        else
            return(x)
    }
}

stems_1976$PX <- floor(stems_1976$X / 20)
stems_1976$PY <- floor(stems_1976$Y / 20)
plots_76_x <- sapply(stems_1976$PX, munge_digits)
plots_76_y <- sapply(stems_1976$PY, munge_digits)
plots_76 <- paste(plots_76_x, plots_76_y, sep="")
stems_1976$PLOT <- as.factor(unlist(unname(plots_76)))

stems_1996[1,]$X = 0
stems_1996$PX <- floor(stems_1996$X / 20)
stems_1996$PY <- floor(stems_1996$Y / 20)
plots_96_x <- sapply(stems_1996$PX, munge_digits)
plots_96_y <- sapply(stems_1996$PY, munge_digits)
plots_96 <- paste(plots_96_x, plots_96_y, sep="")
stems_1996$PLOT <- as.factor(unlist(unname(plots_96)))

#Get rid of the leftmost 'flange', dirties the data
stems_1976 <- stems_1976[which(stems_1976$PX > 0),]
stems_1996 <- stems_1996[which(stems_1996$PX > 0),]

#Standardize plot information (coordinates, offsets, factored plot IDs, etc)
plots <- as.character(stems_2019$PLOT)
plots <- sapply(plots, FUN=munge_plots)
plots <- unlist(unname(plots))
stems_2019$PLOT <- as.factor(plots)
plots_s <- substring(plots, 1, 2)
plots_e <- substring(plots, 3, 4)
stems_2019$PX <- as.integer(plots_s)
stems_2019$PY <- as.integer(plots_e)
stems_2019$CX <- (stems_2019$PX * 20) + 10
stems_2019$CY <- ((stems_2019$PY - 7) * 20) + 10

main_stems <- which(is.na(stems_2019$MAIN))
branch_stems <- which(!is.na(stems_2019$MAIN))

stems_2019$PLOT <- as.factor(stems_2019$PLOT)
stems_2019$NOTES <- as.character(stems_2019$NOTES)

#Copy over species codes and notes from main stems to branches
for (x in seq(0, length(branch_stems)))
{
	ind <- branch_stems[x]
	main_ind <- stems_2019[ind,]$MAIN
	stems_2019[ind,]$SPP <- stems_2019[main_ind,]$SPP
	stems_2019[ind,]$NOTES <- stems_2019[main_ind,]$NOTES
	stems_2019[ind,]$DEG <- stems_2019[main_ind,]$DEG
	stems_2019[ind,]$DIST <- stems_2019[main_ind,]$DIST
}
#Parse the Notes column and store the comma-separated field as a list
notes_list <- strsplit(stems_2019$NOTES, c(','))

#Merge species information with codes in the raw data
#Use a left-join to ensure rows without species IDs are preserved
print("Merge tables by species data")
stems_2019 <- left_join(stems_2019, species_key, by="SPP")
stems_1976 <- left_join(stems_1976, species_table)
stems_1996 <- left_join(stems_1996, species_table)

#Compute x/y offsets from polar coordinates using transponder data
#These offsets are relative to the putative 'center' of each plot.
#Rotate 90 degrees to account for the northern bearing when using polar coordinates, then convert to radians
rads = (pi*(stems_2019$DEG+90)/180)
stems_2019$OX <- stems_2019$DIST*cos(rads)
stems_2019$OY <- stems_2019$DIST*sin(rads)

#Reflect across the x axis for unknown reasons
stems_2019$OY <- -stems_2019$OY

#Make global X-Y positions assuming stems are centered on each plot (error emerges from transponder placement).
stems_2019$X <- stems_2019$CX + stems_2019$OX
stems_2019$Y <- stems_2019$CY + stems_2019$OY

#Conditions for flagging potential errors in data
validation <- is.na(stems_2019$DBH) | 
((is.na(stems_2019$SPP) | stems_2019$SPP == "") & is.na(stems_2019$MAIN))
stems_2019$INVALID <- validation

#Ready for stem mapping after this
#Begin matching stems across census years
#'76 -> '96
matched_76_96 <- match_76_96()
#The result of this vector should be all 0's, but many are not - indicating the matching isn't perfect
difference <- as.numeric(matched_76_96$DBH_76) - stems_1976[matched_76_96$MATCH,]$DBH


#Create a per-plot report on missing species ID, DBH measurements, pole pruners/Nails
generate_plot_report <- function(stems, plot_id)
{
	plot <- which(stems$PLOT == plot_id)
	missing_dbh <- which(is.na(stems[plot,]$DBH))
	missing_species <- which(stems_2019$SPP == "")
	nails <- which(!is.na(stems[plot,]$NAIL))
	macrosystems <- which(!is.na(stems[plot,]$MS))
}

#Count the number of lianas using ad hoc morphospecies, provisional names, and the notes column.
generate_liana_report <- function()
{
	form_vec <- stems_2019$FORM %in% as.factor("Liana")
	note_vec <- grepl("liana", notes_list)
	cap_note_vec <- grepl("Liana", notes_list)
	strych_vec <- grepl("Strychnos", notes_list)
	salacia_vec <- grepl("Salacia", notes_list)
	paullinia_vec <- grepl("Paullinia", notes_list)
	liana_vec <- form_vec | note_vec | cap_note_vec | strych_vec | salacia_vec | paullinia_vec

}

#Generates a list of missing DBHs, SPPs, etc.
generate_validation_report <- function(stems)
{
	plots <- unique(stems$PLOT)
	for(plot in plots)
	{

	}
}

#Takes raw census data with MS column and raw macrosystems data to swap ids when tag replacement is done
swap_macrosystems_ids <- function(stems, ms)
{
    macrosystems <- which(!is.na(stems$MS))
    ms_ids <- stems[macrosystems,]$MS
    new_ids <- stems[macrosystems,]$ID
    replaced <- match(ms_ids, ms$enq)
    nas <- which(!is.na(replaced))
    replaced_na <- replaced[nas]
    new_ids_na <- new_ids[nas]
    ms_ids_na <- ms_ids[nas]
    ms[replaced_na,]$enq <- new_ids_na
    ms[replaced_na,]$y2_notes <- paste("Old tag#: ", ms_ids_na)
    return(macrosystems)
    #write.csv(ms, file="macrosystems_y2_replaced.csv", na="")

}

match_76_96 <- function()
{   
    stems_1996_ids <- stems_1996[which(stems_1996$ID != 0),]
    stems_1996_ids$MATCH <- rep(-1, nrow(stems_1996_ids))
    #match_76 = stems_1976[which(as.character(stems_1976$PLOT) == as.character(stems_1996_ids[0,]$PLOT) & stems_1976$DBH_76 == stems_1996_ids[0,]$DBH & stems_1976$SPP == stems_1996_ids[0,]$SPP),]
    for(x in seq(1, nrow(stems_1996_ids)))
    {
        stem_match = which(as.character(stems_1976$PLOT) == as.character(stems_1996_ids[x,]$PLOT) & stems_1976$ID == stems_1996_ids[x,]$ID)# & stems_1976$DBH_76 == stems_1996_ids[x,]$DBH)# & stems_1976$SPP == stems_1996_ids[x,]$SPP)
        if(length(stem_match > 0))
        {
            #A fair amount of ambiguity between plot and ID matching
            if(length(stem_match) > 1)
            {
                print(stem_match)
                stem_match = stem_match[1]
                print(stem_match)
            }
            stems_1996_ids[x,]$MATCH = stem_match
        }       
        #match_76 = rbind(match_76, stems_1976[stem_match,])
    }
    return(stems_1996_ids[which(stems_1996_ids$MATCH > 0),])
}

#How many tags do plots take?
#Average: 223.75
#plot_distribution <- count(stems_2019, PLOT)
#ggplot(plot_distribution, aes(x=PLOT, y=n, fill=PLOT)) + geom_bar(stat="identity")
#ggsave("plot_distribution.png")

#stems_2006 <- read.csv("data/xtot2006.raw.data.csv")
#plot_007 <- which(stems_2006$col == 0 & stems_2006$row == 7)
#s007 <- stems_2006[plot_007,]
#s007$ID <- as.integer(row.names(s007))
#ggplot(s007, aes(x=x.displace, y=y.displace)) + geom_text(aes(label=sp))

