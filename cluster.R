#Analyses of Canberra Clusters
setwd(".")

#Read in your Map File, which presumably has your DMMs/Clusters as a variable.
myDat <- read.table("/data/Users/dfadrosh/Metcalfe_paper/Metcalfe_sputum_mapping_expanded.txt", header=TRUE, check.names=F, comment="", sep="\t", row.names=1)

#Define your cluster variable:
clustVar <- "L6_DMM"

#### Input Complete ####


#Determine the names and number of clusters you have:
clusterNames <- levels(factor(myDat[,clustVar]))
totLevels <- length(clusterNames)
#Get list of variables
anly_var <- names(myDat)[!names(myDat) %in% c(clustVar, "#SampleID")] # For now, presuming this is the name of your sampleID

clust.mod <- list()
var.clust.list <- list()
for(i in anly_var) {
	myDat[,i][myDat[,i] %in% c(""," ")] <- NA
      for(j in 1:totLevels) {
      	    clust.mod[[j]] <- anova(glm(myDat[,clustVar] %in% clusterNames[j] ~ myDat[,i], family="binomial"), test="LRT")[2,5]
	    }
	    var.clust.list[[i]] <- c(sum(!is.na(myDat[,i])), unlist(clust.mod))
}
var.clust.df <- t(data.frame(var.clust.list))
var.clust.fdr <- apply(var.clust.df[, 2:ncol(var.clust.df)], 2, p.adjust, method="fdr")
colnames(var.clust.fdr) <- paste0(clusterNames, ".fdr")

colnames(var.clust.df) <- c("sampleSize", clusterNames)

full.data <- cbind(var.clust.df, var.clust.fdr)

write.table(full.data, "Cluster_Associations.txt", sep="\t", quote=F)
