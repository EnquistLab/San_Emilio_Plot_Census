library(googledrive)

download_species_list <- function()
{
	drive_download(file="~/Costa_Rica/2019/species_code_key", path="data/species_code_key.csv", type="csv", overwrite=TRUE)
	species_key <- read.csv("data/species_code_key.csv")
	write.table(species_key$SPP, file="data/spp_codes.csv", quote=FALSE, row.names=FALSE, col.names=FALSE, sep=',', eol=",")
}

#Download all the raw data sheets as .csv from drive.google.com/Costa_Rica/2019/raw_data
#This function is outrageously slow, and I'm not sure if its Google or Tidyverse's fault.
#ow: overwrite - FALSE is useful for updating a partial set of sheets
download_raw_data <- function(ow=FALSE)
{
	plot_rows <- seq(7, 38)
	for(plot in plot_rows)
	{
		file_id = paste("San_Emilio_", plot, sep="")
		path_id = paste("~/Costa_Rica/2019/raw_data/", file_id, sep="")
		local_id = paste("data/",file_id,sep="")
		if(ow)
		{drive_download(file=path_id, path=local_id, type="csv", overwrite=TRUE)}
		else if(!file.exists(paste(local_id, '.csv',sep="")))
		{drive_download(file=path_id, path=local_id, type="csv", overwrite=FALSE)}
		else
		{print(paste("Skipping file, overwrite not enabled:", file_id))}
	}
}

#Reads the collected sheets in the data folder
read_raw_data <- function()
{
	san_emilio_2019 <- data.frame(ID=integer(), DBH=numeric(), SPP=factor(), MAIN=integer(), NOTES=character(),
					NAIL=integer(), MS=integer(), DEG=numeric(), DIST=numeric())
	plot_rows <- seq(7, 38)
	for(plot in plot_rows)
	{
		file_id = paste("San_Emilio_", plot, sep="")
		local_id = paste("data/",file_id,sep="")
		print(paste("Reading file: ", file_id))
		san_emilio_2019 <- rbind(san_emilio_2019, read.csv(paste(local_id, ".csv", sep="")))
	}
	#Reading in with colTypes just causes errors - R seems to be unable to cast to these types (as it usually would)	
	#colTypes = c("integer", "numeric", "factor", "integer", "character", "factor", "factor", "integer", "integer", "numeric", "numeric")
	#Just set the columns to the types you want
	san_emilio_2019$MAIN <- as.integer(san_emilio_2019$MAIN)
	san_emilio_2019$NOTES <- as.character(san_emilio_2019$NOTES)
	san_emilio_2019$MS <- as.integer(san_emilio_2019$MS)
	san_emilio_2019$DEG <- as.numeric(san_emilio_2019$DEG)
			
	return(san_emilio_2019)
}