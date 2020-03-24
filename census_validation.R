###
#Top-level script for cleaning and standardizing data across all census years
###

library(ggplot2)
library(dplyr)
library(readr)

source('sheets_integration.R')

species_table <- read.csv("data/Species_number_translation.csv")
colnames(species_table)[1] <- "SPP"
species_key <- read.csv("data/species_code_key.csv")
traits <- read.csv("data/san_emilio_trait_means.csv")

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
stems_1996$DBH_76 <- as.numeric(stems_1996$DBH_76)

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

munge_plots <- function(x)
{
    if(!is.na(x))
    {
        len = nchar(x)
        if(len == 1)
            return(paste("000",x,sep=""))
        else if(len == 2)
            return(paste("00",x,sep=""))
        else if(len == 3)
            return(paste("0",x,sep=""))
    }
    return(x)
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

print("Process 2019 census")
#Standardize plot information (coordinates, offsets, factored plot IDs, etc)
plots <- as.character(stems_2019$PLOT)
plots <- sapply(plots, FUN=munge_plots)
plots <- unlist(unname(plots))
stems_2019$PLOT <- as.factor(plots)
plots_s <- substring(plots, 1, 2)
plots_e <- substring(plots, 3, 4)
stems_2019$PX <- as.integer(plots_s)
stems_2019$PY <- as.integer(plots_e)
stems_2019$PLOT <- as.factor(stems_2019$PLOT)

print(paste("Flagged invalid DBH data for IDs: ", which(stems_2019$DBH > 200)))

#Parse the Notes column and store the comma-separated field as a list
stems_2019$NOTES <- as.character(stems_2019$NOTES)
notes_list <- strsplit(stems_2019$NOTES, c(','))

print("Process branch stems")
branch_stems <- which(!is.na(stems_2019$MAIN))
copy_branch_information <- function(branch)
{
    main_ind = stems_2019[branch,]$MAIN
    plot = stems_2019[branch,]$PLOT
    main = which(stems_2019$ID == main_ind & stems_2019$PLOT == plot)
    if(length(main) == 0)
        print(paste("BranchID error:", branch))
    else if(length(main) == 1)
    {
	stems_2019[branch,]$SPP = stems_2019[main,]$SPP
	stems_2019[branch,]$DEG = stems_2019[main,]$DEG
	stems_2019[branch,]$DIST = stems_2019[main,]$DIST
    }    
    else
        print(paste("BranchID uniqueness error:", branch))
}
sapply(branch_stems, FUN=copy_branch_information)
main_stems <- which(is.na(stems_2019$MAIN))
copy_main_information <- function(main)
{
    branches = which(stems_2019$MAIN == stems_2019[main,]$ID & stems_2019$PLOT == stems_2019[main,]$PLOT)
    if(length(branches) > 0)
    {
        #print(paste("Copying main stem:", main))
        fields <- c("SPP", "DEG", "DIST")
        stems_2019[branches,fields] <<- stems_2019[main,fields]
    }
}
#sapply(main_stems, copy_main_information)

#Merge species information with codes in the raw data
#Use a left-join to ensure rows without species IDs are preserved
print("Merge tables by species data")
stems_2019 <- left_join(stems_2019, species_key, by="SPP")
stems_1976 <- left_join(stems_1976, species_table)
stems_1996 <- left_join(stems_1996, species_table)

#Compute x/y offsets from polar coordinates using transponder data
#These offsets are relative to the putative 'center' of each plot.
#Rotate 90 degrees to account for the northern bearing when using polar coordinates, then convert to radians
stems_2019$DEG <- as.numeric(stems_2019$DEG)
stems_2019$DIST <- as.numeric(stems_2019$DIST)

rads = (pi*(stems_2019$DEG+90)/180)
stems_2019$OX <- stems_2019$DIST*cos(rads)
stems_2019$OY <- stems_2019$DIST*sin(rads)

#Reflect across the x axis for unknown reasons
stems_2019$OY <- -stems_2019$OY

#Extract rebar positions from notes list
grep_boundary <- function(x, pat)
{
    ind <- grep(pat, x)
    deg <- parse_number(x[ind])
    dist <- x[ind+1]
    return(c(deg, dist))
}

sw <- grepl("SW:", notes_list)
bar_list <- lapply(notes_list[sw], FUN=grep_boundary, pat="SW")
bar_frame_sw <- data.frame(matrix(unlist(bar_list), nrow=length(bar_list), byrow=TRUE))
bar_frame_sw$PLOT <- stems_2019[sw,]$PLOT
bar_frame_sw$CORNER <- rep("SW", length(bar_list))

nw <- grepl("NW:", notes_list)
bar_list <- lapply(notes_list[nw], FUN=grep_boundary, pat="NW")
bar_frame_nw <- data.frame(matrix(unlist(bar_list), nrow=length(bar_list), byrow=TRUE))
bar_frame_nw$PLOT <- stems_2019[nw,]$PLOT
bar_frame_nw$CORNER <- rep("NW", length(bar_list))

ne <- grepl("NE:", notes_list)
bar_list <- lapply(notes_list[ne], FUN=grep_boundary, pat="NE")
bar_frame_ne <- data.frame(matrix(unlist(bar_list), nrow=length(bar_list), byrow=TRUE))
bar_frame_ne$PLOT <- stems_2019[ne,]$PLOT
bar_frame_ne$CORNER <- rep("NE", length(bar_list))

se <- grepl("SE:", notes_list)
bar_list <- lapply(notes_list[se], FUN=grep_boundary, pat="SE")
bar_frame_se <- data.frame(matrix(unlist(bar_list), nrow=length(bar_list), byrow=TRUE))
bar_frame_se$PLOT <- stems_2019[se,]$PLOT
bar_frame_se$CORNER <- rep("SE", length(bar_list))

bar_frame <- rbind(bar_frame_sw, bar_frame_nw)
bar_frame <- rbind(bar_frame_ne, bar_frame)
bar_frame <- rbind(bar_frame, bar_frame_se)
colnames(bar_frame) <- c("DEG", "DIST", "PLOT", "CORNER")

print(paste("Flagged invalid position data for row: ", which(stems_2019$DEG >= 360 | stems_2019$DIST > 30)))

bar_frame$DEG <- as.numeric(as.character(bar_frame$DEG))
bar_frame$DIST <- as.numeric(as.character(bar_frame$DIST))
rads = (pi*(bar_frame$DEG+90)/180)
bar_frame$BX <- bar_frame$DIST*cos(rads)
bar_frame$BY <- bar_frame$DIST*sin(rads)
stems_2019$CX <- rep(10, nrow(stems_2019))
stems_2019$CY <- rep(10, nrow(stems_2019))

#Compute plot centers based on offsets from the rebar positions
plots <- unique(bar_frame$PLOT)
find_and_set_plot_center <- function(x)
{
    bars <- which(bar_frame$PLOT == x)
    stems <- which(stems_2019$PLOT == x)
    stems_2019[stems,]$CX <<- rep(mean(bar_frame[bars,]$BX)+10, length(stems))
    stems_2019[stems,]$CY <<- rep(mean(bar_frame[bars,]$BY)+10, length(stems))
}
centers <- sapply(plots, find_and_set_plot_center)

#Make global X-Y positions using measurements of plot boundaries
stems_2019$X <- (stems_2019$PX * 20) + stems_2019$CX + stems_2019$OX
stems_2019$Y <- ((stems_2019$PY - 7) * 20) + stems_2019$CY + stems_2019$OY

#Conditions for flagging potential errors in data
validation <- is.na(stems_2019$DBH) | 
((is.na(stems_2019$SPP) | stems_2019$SPP == "") & is.na(stems_2019$MAIN))
print(paste("Flagged missing fields for IDs: ", which(validation)))

#Ready for stem mapping after this

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

#Takes raw census data with MS column and raw macrosystems data to swap ids when tag replacement is done
#Add species info from our list of species
clean_macrosystems_data <- function(stems, species, ms)
{
    #Add taxonomic information
    col_num <- length(colnames(ms))
    gg <- left_join(ms, species, by="SPP")
    gg1 <- gg[which(!is.na(gg$GENUS) | gg$SPP == ""),]    
    nas <- which(gg1$SPP == "")    
    gg1[nas,]$GENUS <- as.character(gg1[nas,]$GENUS)
    gg1[nas,]$GENUS <- "" 
    gg1$species_binomial <- paste(gg1$GENUS, gg1$SPECIES)
        
    print(nrow(gg1))
    col_new_num <- length(colnames(gg1))

    not <- which(is.na(gg$GENUS) & gg$SPP != "")
    gg2 <- left_join(ms[not, c(seq(1, col_num))], species, by=c("SPP" = "ALIAS"))
    #nas <- which(is.na(gg2$GENUS))    
    #gg2[nas,]$GENUS <- ""
    #gg2[nas,]$SPECIES <- ""    
    gg2$species_binomial <- paste(gg2$GENUS, gg2$SPECIES)
    print(nrow(gg2))
    ms <- rbind(gg1[,c(seq(1, col_num), col_new_num)], gg2[,c(seq(1, col_num), col_new_num)])
    print(nrow(ms))
    #Swap old tag numbers
    macrosystems <- which(!is.na(stems$MS))
    
    ms_ids <- stems[macrosystems,]$MS
    new_ids <- stems[macrosystems,]$ID
    replaced <- match(ms_ids, ms$enq)
    
    nas <- which(!is.na(replaced))
    replaced_na <- replaced[nas]
    new_ids_na <- new_ids[nas]
    ms_ids_na <- ms_ids[nas]
    ms[replaced_na,]$enq <- new_ids_na
    #ms[replaced_na,]$y2_notes <- paste("Old tag#:", as.character(ms_ids_na))
    
    return(ms)
    #write.csv(ms, file="macrosystems_y2_cleaned.csv", na="")
}

#stems_2006 <- read.csv("data/xtot2006.raw.data.csv")
#plot_007 <- which(stems_2006$col == 0 & stems_2006$row == 7)
#s007 <- stems_2006[plot_007,]
#s007$ID <- as.integer(row.names(s007))
#ggplot(s007, aes(x=x.displace, y=y.displace)) + geom_text(aes(label=sp))

