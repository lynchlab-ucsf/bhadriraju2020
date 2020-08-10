
#Dirichlet Multinomial Mixtures

#set dir
setwd(".")

#load packages
library(DirichletMultinomial)
library(lattice) #for visualization
library(xtable)
library(parallel) #for use of multiple cores during cross-validation

#.qualitative during visualization
#dev.off redefined to return w/o displaying results - closes device/file
options(width=70, digits=2)
full <- TRUE
.qualitative <- DirichletMultinomial:::.qualitative
dev.off <- function(...) invisible(grDevices::dev.off(...))

#reading data into a matrix called counts, containing sammples x data


####OTU table MUST use absolute abundances (summarize_taxa.py -a) aka whole numbers
####any rows with all 0s must be removed from OTU table
count <- t(as.matrix(read.delim("taxa_summary_for_DMM/otu_table_uparse_w_tax_NTC_filtered_filtered_reprare73265_L6.txt", row.names=1)))
#creates object of transposed matrix of table file that was read in as a data frame
#file => table format => data frame

count[1:5, 1:3]
#returns the values in rows 1-5, columns 1-3
#make sure no decimals

cnts <- log10(colSums(count)) #object cnts contains the log10 values of the total reads per taxa
pdf("taxon-counts_filt100_rare10k_L6.pdf") #creates and opens a pdf in working dir called taxon-counts.pdf
densityplot(cnts, xlim = range(cnts),
            xlab="Taxon representation (log 10 count)")
#makes density plot of log10(taxa totals) within the pdf
dev.off() #closes the pdf
###will bring error about xlim if zero rows not removed from OTU table

#Clustering

#dmn function fits a Dirichlet-Multinomial model, taking as input the count
#data and a parameter k representing the number of Dirichlet components to model

if (full){
  fit <- mclapply(1:10, dmn, count=count, verbose=TRUE)
  save(fit, file=file.path(tempdir(), "fit.rda"))
} else data(fit)

#uncomment below lines to display measure of fit for k=
#fit[[1]]
#
#fit[[2]]
#
#fit[[3]]
#
#fit[[4]]
#
#fit[[5]]
#
#fit[[6]]
#
#fit[[7]]
#
#fit[[8]]
#
#fit[[9]]
#
#fit[[10]]

#measures of fit for different values of k are plotted
#uses laplace goodness of fit
lplc <- sapply(fit, laplace)
pdf("min-laplace_filt100_10k_L6.pdf")
plot(lplc, type="b", xlab="Number of Dirichlet Components",
     ylab="Model Fit")
dev.off()
(best <- fit[[which.min(lplc)]])
#the k value with the lowest y-axis fit value is the best
#fit value of k

##Choose model that best fits each sample and write to table
mix_matrix <- mixture(best)
predictions <- mixture(best, assign=TRUE)
pred_dir <- data.frame(predictions)
write.table(pred_dir, "L6_DMM.txt", sep="\t")