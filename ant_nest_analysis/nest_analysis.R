library(gdata)
library(ggplot2)
library(plyr)

#Plot corners of the plot
corners <- read.csv("SanEmilioCorners.csv")
ll <- strsplit(as.character(corners$name), split=",")
corners$x <- as.numeric(unlist(lapply(ll, '[[', 1)))
corners$y <- as.numeric(unlist(lapply(ll, '[[', 2)))
corners$y[9] = 39

species_key <- read.csv("Species_number_translation.txt", sep="\t")
traits <- read.csv("traits.csv", header = TRUE)
traits <- traits[c(1, 2, 3, 16, 17, 18, 19)]

#Read and clean ant data
ant_data <- read.csv("antAll.csv")
cc <- colnames(ant_data)
cc[4] <- "vX"
cc[5] <- "vY"
colnames(ant_data) <- cc

#Remove nests without data required to generate hull or otherwise dirty
hull_data <- which(ant_data$PRECISE == "Y" & ant_data$YEAR == 1995 & ant_data$ID != 7)

#X-Y at the meter scale, need to scale ants down or trees up
tree_cols <- c("SPECIES_ID", "Xm", "Ym", "DBH")
#tree_data76 <- read.csv("xtot76.csv")
tree_data96 <- read.csv("SanEmilio_1996.txt", sep="\t")
colnames(tree_data96) <- tree_cols

#Re-scale to plot level range
tree_data96$X <- (tree_data96$Xm - (tree_data96$Xm %% 20)) / 20
tree_data96$Xq <- tree_data96$Xm %% 20
tree_data96$Y <- (tree_data96$Ym - (tree_data96$Ym %% 20)) / 20
tree_data96$Yq <- tree_data96$Ym %% 20
test_points <- data.frame(X=tree_data96$X + tree_data96$Xq/20, Y=tree_data96$Y + tree_data96$Yq/20)

#Outputs a dataframe with convex hulls as observations and columns as individuals trees absent(-1) or present(1)
#Need to replace generated variables for trees with some kind of observation ID
source("inhull.R")

#Get area from convex hulls for generating random samples
areas <- ddply(ant_data[hull_data,], .(ID), function(x) convhulln(data.frame(x$vX, x$vY), options="FA")$area)
areas$X <- runif(nrow(areas), range(tree_data96$X)[1] + 1, range(tree_data96$X)[2] - 1)
areas$Y <- runif(nrow(areas), range(tree_data96$Y)[1] + 1, range(tree_data96$Y)[2] - 1)
rand_plots <- data.frame(ID=numeric(nrow(areas)*4), X=numeric(nrow(areas)*4), Y=numeric(nrow(areas)*4))

#Draw square plot around random points
for(i in seq(1, nrow(areas)))
{
  dim = sqrt(areas[i,]$V1)/2
  #print(paste("Plot area: ", areas[i,]$V1))
  #print(paste("Plot dimension: ", dim))
  rand_plots[((i-1)*4)+1,] = (c(areas[i,]$ID, areas[i,]$X+dim, areas[i,]$Y+dim))
  rand_plots[((i-1)*4)+2,] = (c(areas[i,]$ID, areas[i,]$X+dim, areas[i,]$Y-dim))
  rand_plots[((i-1)*4)+3,] = (c(areas[i,]$ID, areas[i,]$X-dim, areas[i,]$Y+dim))
  rand_plots[((i-1)*4)+4,] = (c(areas[i,]$ID, areas[i,]$X-dim, areas[i,]$Y-dim))
}

get_stats <- function(hulls)
{
  hull_stats <- data.frame(ID=hulls$ID, ABUNDANCE=numeric(nrow(hulls)), RICHNESS=numeric(nrow(hulls)), 
                            SLA=numeric(nrow(hulls)), LEAF_AREA=numeric(nrow(hulls))) 
  hulls <- hulls[,-1]
  colnames(hulls) <- sub("V",'',colnames(hulls))
  for(i in seq(1, nrow(hulls))) 
  { 
    present = which(hulls[i,] == 1)
    amt = length(present)
    ids = tree_data96[present,]$SPECIES_ID
    rich <- length(unique(ids))
    spp <- species_key[ids,]$species
    spp <- spp[!is.na(spp)]
    trows <- match(spp, traits$Species)
    trows <- trows[!is.na(trows)]
    sla <- traits[trows,]$SLA
    area <- traits[trows,]$area
    hull_stats[i,2] = amt
    hull_stats[i,3] = rich
    hull_stats[i,4] = mean(sla, na.rm=TRUE)
    hull_stats[i,5] = mean(area, na.rm=TRUE)
  }
  print(hull_stats)
  return(hull_stats)
}
#Do collision testing on random plots
rhulls <- ddply(rand_plots, .(ID), function(x) inhull(test_points, data.frame(x$X, x$Y)))
rhull_stats <- get_stats(rhulls)

#Do collision testing on nest plots
chulls <- ddply(ant_data[hull_data,], .(ID), function(x) inhull(test_points, data.frame(x$vX, x$vY)))
chull_stats <- get_stats(chulls)

#Combine random and nest results for plotting
rhull_stats$PLOT <- rep("R", nrow(rhull_stats))
chull_stats$PLOT <- rep("N", nrow(chull_stats))
hull_stats <- rbind(rhull_stats, chull_stats)

#plot abundance, richness, traits
ap <- ggplot(data=hull_stats, aes(PLOT, ABUNDANCE)) + geom_boxplot()
rp <- ggplot(data=hull_stats, aes(PLOT, RICHNESS)) + geom_boxplot()
sp <- ggplot(data=hull_stats, aes(PLOT, SLA)) + geom_boxplot()
arp <- ggplot(data=hull_stats, aes(PLOT, LEAF_AREA)) + geom_boxplot()
#TODO:
#Plot traits inside hulls

nest_plot <- ggplot(ant_data[hull_data,], aes(group=ID, color=ID, x=vX, y=vY)) + 
  geom_point(size=1, shape=1) + geom_path(aes(group=ID), size = 1) + 
    geom_point(data=corners, aes(x=x, y=y), size=2, shape = 5) + 
      geom_point(data=tree_data96, aes( x=(X+(Xq/20)),y=(Y+(Yq/20)) ), size=0.05, alpha = 0.25, inherit.aes=FALSE) + 
        geom_point(data=rand_plots, aes(x=X, y=Y))

get_species_list <- function(chulls)
{
  for(i in seq(1, nrow(chulls))) { 
    present = which(chulls[i,] == 1)
    ids = tree_data96[present,]$SPECIES_ID
    id = chulls[i,]$ID
    spp <- species_key[ids,]$species
    spp <- spp[!is.na(spp)]
    ss <- paste(id, spp, sep = ",")
    write(ss, append=TRUE)
  }
}
