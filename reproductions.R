reproduce_enquist_et_al_1999 <- function()
{
    #In methods, Enquist et al state that only species with 7 individuals > 10cm DBH were included, 
    #individuals exhibiting negative growth were excluded, and only species with individuals up to 30cm were included 
    test_set <- stems_1996[which(stems_1996$DBH_76 > 0 & stems_1996$DBH > 10 & stems_1996$DBH > stems_1996$DBH_76),]    
    test_set$Density <- sapply(match(test_set$Species, traits$Species), FUN=function(x) traits[x,c("Wood.Density")])

    #Figure 2b from the paper showing a universal growth curve reflecting metabolic constraints on growth at all sizes    
    ggplot(test_set, aes(x = Density*DBH_76^(2/3), y = Density*DBH^(2/3))) + geom_point() + 
        geom_line(stat='smooth', method='lm', se=FALSE, aes(group=Species), alpha=0.5, size = 2) + geom_abline(slope=1, intercept=0, linetype=2)
    #A look at individual changes in diameter growth using expression 4 from the paper's Box 1
    #ggplot(test_set, aes(x=(1/2*Density)*DBH^(1/3), y=DBH - DBH_76)) + geom_point(alpha=0.1) + 
    #    geom_line(stat='smooth', method='lm', se=FALSE, aes(group=Species), alpha=0.5, size = 2)
}