#This script matches 2019 census data with stems from previous inventories based on spatial location and species IDs

#General idea:
#Starting in an early census year, attempt to match older trees (>10cm DBH) to their counterparts in later years
#Exclude lianas
#Loop through 1976, and match to 1996, 2006, and 2019.
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

#Begin matching stems across census years
#'76 -> '96
#matched_76_96 <- match_76_96()
#The result of this vector should be all 0's, but many are not - indicating the matching isn't perfect
#difference <- as.numeric(matched_76_96$DBH_76) - stems_1976[matched_76_96$MATCH,]$DBH

#Test a generalized matching algorithm against the test set - 96 stems matched to 76 stems
test_matching_function <- function(FUN)
{
    test_set <- stems_1996[which(stems_1996$ID != 0 & stems_1996$DBH_76 > 0),]
    rejection_set <- stems_1996[which(stems_1996$ID == 0 | stems_1996$DBH_76 == 0),]
    result <- FUN()
    
    accuracy_match <- match(rownames(test_set), rownames(result))
    print(paste("Testing accuracy: ", length(which(!is.na(accuracy_match)))/nrow(test_set)))
    
    validity_match <- match(rownames(rejection_set), rownames(result))
    print(paste("Testing validity: ", length(which(!is.na(validity_match)))/nrow(rejection_set)))  
}

match_76_96 <- function()
{   
    stems_1996_ids <- stems_1996[which(stems_1996$ID != 0 & stems_1996$DBH_76 > 0),]
    stems_1996_ids$MATCH <- rep(-1, nrow(stems_1996_ids))
    #match_76 = stems_1976[which(as.character(stems_1976$PLOT) == as.character(stems_1996_ids[0,]$PLOT) & stems_1976$DBH_76 == stems_1996_ids[0,]$DBH & stems_1976$SPP == stems_1996_ids[0,]$SPP),]
    for(x in seq(1, nrow(stems_1996_ids)))
    {
        stem <- stems_1996_ids[x,]
        stem_match = which(as.character(stems_1976$PLOT) == as.character(stems_1996_ids[x,]$PLOT) & 
                            as.character(stems_1976$ID) == as.character(stems_1996_ids[x,]$ID) &
                                stems_1976$Genus == stem$Genus)# & stems_1976$DBH_76 == stems_1996_ids[x,]$DBH)# & stems_1976$SPP == stems_1996_ids[x,]$SPP)        
        #stem_match = which(abs(stems_1976$X - stem$X) < 10 & abs(stems_1976$Y - stem$Y) < 10 & stems_1976$Genus == stem$Genus)        
        if(length(stem_match > 0))
        {
            #A fair amount of ambiguity between plot and ID matching
            if(length(stem_match) > 1)
            {
                #This line is cheating
                discrep <- as.numeric(stems_1976[stem_match,]$DBH) - as.numeric(stems_1996_ids[x,]$DBH_76)
                stem_match = stem_match[which(discrep == 0)]
                if(length(stem_match) == 0)
                {
                    print("No match for ID, setting NA")
                    stem_match = NA
                }
                else if(length(stem_match) > 1)
                {  
                    print("Multiple correct matches")
                    print(stems_1976[stem_match,])
                    print(stems_1996_ids[x,])
                    stem_match = stem_match[1]
                }
            }
            else
            {
                diff = stems_1996_ids[x,]$DBH_76 - stems_1976[stem_match,]$DBH
                if(diff > 1)
                {
                    print("Single matching incorrect DBH")
                    print(stems_1976[stem_match,])
                    print(stem)
                    stem_match = NA
                }
            }
            stems_1996_ids[x,]$MATCH = stem_match
        }       
        #match_76 = rbind(match_76, stems_1976[stem_match,])
    }
    return(stems_1996_ids[which(stems_1996_ids$MATCH > 0),])
}

match_nail_stems <- function(stems)
{
    nails <- stems_2019[which(!is.na(stems_2019$NAIL)),]
    nails$MATCH <- rep(-1, nrow(nails))
    for(x in seq(1, nrow(nails)))
    {
        nail <- nails[x,]
        #print(paste("Matching for 2019 stem ID:", x$ID))
        if(nail$NAIL != 0)
        {
            match_76 <- which(as.character(stems_1976$PLOT) == as.character(nail$PLOT) & stems_1976$ID == nail$NAIL)
            if(length(match_76) > 0)            
                nails[x,]$MATCH <- match_76[1]
            #print("76 match:")
            #print(stems_1976[match_76,])
            #print(paste("DBH difference: ", x$DBH - stems_1976[match_76,]$DBH))  
        }
        else
        {
            match_96 <- which(abs(stems_1996$X - nail$X) < 10 & abs(stems_1996$Y - nail$Y) < 10 & as.character(stems_1996$Genus) == as.character(nail$GENUS))
            if(length(match_96) > 0)            
                nails[x,]$MATCH <- match_96[1]
            #print("96 match:")
            #print(stems_1996[match_96,])
        }
    }
    return(nails)    
}