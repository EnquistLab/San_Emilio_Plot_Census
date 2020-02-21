library(ggrepel)

#Determines if stems are overlapping in a given plot.
#Used for rendering plot maps, overlap if trees are within DBH distance
#Adds a vector-valued column OVERLAP with the index of overlapping trees

#This is much harder than simply having a single overlap distance and
#applying it to the distance matrix
determine_overlap <- function(stems, plot_id)
{
	stems <- stems[which(stems$PLOT == plot_id),]
	dist_mat <- dist(stems[, c("OX", "OY")])
	dist_mat <- as.matrix(dist_mat)
	stems$OVERLAP = list(NULL) #Lists only way to store multiple values
	for(x in seq(1, nrow(stems)))
	{
		#Distance matrix in meters, DBH in centimeters make sure to convert.
		condition <- function(y) 
		{   if(dist_mat[x,y] < (stems[x,]$DBH/100 && x != y)) return(y) 
			else return(NA) }
		overlaps <- sapply(seq(1, nrow(stems)), FUN=condition)
		stems[x,]$OVERLAP[[1]] <- overlaps
		#stems[overlaps,]$OVERLAP[1] <- c(stems[overlaps,]$OVERLAP, x)
	}
	return(stems)
}

#distance should be given in meters
#Returns a boolean matrix of row IDs overlapping
simple_distance_overlap <- function(stems, distance=1)
{
	dist_mat <- dist(stems[, c("OX", "OY")])
	dist_mat <- as.matrix(dist_mat)
	overlap_mat <- dist_mat < distance
	return(overlap_mat)
}

#Returns a distance matrix of size n,m
#where n is the number of candidate trees from a future census,
#and m is the candidate matches from previous years. We directly
#compare their computed OX, OY values and look for matches where dist = 0
distance_between_decades <- function(stems)
{

}


pretty_print_plot_map <- function(stems, plot_id)
{
	plot <- which(stems$PLOT == plot_id & is.na(stems$MAIN))
	scale_text_size = mean(stems[plot,]$DBH, na.rm=TRUE)
	max_dbh = max(stems[plot,]$DBH, na.rm=TRUE)
	pp <- ggplot(stems[plot,], aes(x=OX, y=OY)) +  
	geom_point(aes(size=DBH,shape=INVALID),alpha=0.25) +
	scale_shape_manual(values=c("TRUE"=16, "FALSE"=1)) +
	geom_text_repel(aes(size=DBH+(scale_text_size*(1-(DBH/max_dbh))), 
		label=ID,color=TRI), segment.size=0.25, segment.alpha=0.5) +
		scale_color_manual(values = c("W" = "green", "N" = "blue", 
										"E" = "red", "S" = "purple"))	
		
	width_i <- 8
	height_i <- 8
	ggsave(paste("plot_map_",paste(plot_id,".png",sep=""), sep=""), 
		plot=pp, path="maps/", device=png(), width=width_i, height=height_i, 
		units="in", dpi=300)
	return(pp)
}