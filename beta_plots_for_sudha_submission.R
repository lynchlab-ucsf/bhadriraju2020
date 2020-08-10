# code for 2X PCoA plots with legend

# load packages
library(ggplot2)
library(grid)
library(gridExtra)

setwd('.')

# read in data and subset out the data you need
MYmeta=read.table("Metcalfe_sputum_mapping_expanded.txt",header=T,sep='\t')
# read in the pc data.  the pc data here is from all time points, however the pc1 and pc2 data is only for the time specific analysis since we are breaking down by week.
PC=read.table("pc.txt",header=T,sep='\t')
#merge data from the meta and pc files
MYdata=merge(MYmeta,PC,by="SampleID",all=T)

# set colors -- line up order w/ factor above
#for dom taxa
col1 <- c("darkgreen","darkblue","firebrick1","gold1","darkorange")
#for DMM
col2 <- c("darkgreen","darkblue","firebrick1","gold1")
#for heirarchical clustering
col3 <- c("darkgreen","darkblue","firebrick1","gold1")


# set legend labels -- same order as above
#for dom taxa
#leg1 <-c("Haemophilus (26)","Moraxella (6)","Neisseria (3)","Prevotella (13)","Streptococcus (103)")
legtest <-c("Streptococcus","Prevotella","Haemophilus","Moraxella","Neisseria")
leg1 <-c("Streptococcus (103)", "Prevotella (13)", "Haemophilus (26)","Moraxella (6)","Neisseria (3)")
#for DMM
leg2 <-c("DMM1 (78)","DMM2 (45)","DMM3 (22)","DMM4 (6)")
#for heirarchical clustering
leg3 <-c("C1 (97)","C2 (28)","C3 (20)","C4 (6)")

MYdata$dom_sputum_genus <- factor(MYdata$dom_sputum_genus,levels=legtest)

# If aligning multiple plots, run each one as an object (px=) then array them with grid.arrange at the end
# If you have only one plot, # out the p11= ; to keep the legend, #legend.position="none"
# For multiple plots with a shared legend you will create the legend later (code below)


######################################################################################
 P1 =  

   ggplot(MYdata, aes(PC1, PC2)) +
     theme_bw() +
     geom_point(aes(colour=MYdata$dom_sputum_genus),size=6) +
     scale_colour_manual(name = "Dominant Sputum\n Genus", labels = legtest, values = col1) +   
     ylab("PC2  (22%)")  +
     xlab("PC1  (45%)") +   
     theme(
       axis.ticks=element_blank(),
       text = element_text(size=25, family="Helvetica"),
       legend.position="none",
       plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill = "transparent",colour = NA),
       panel.grid.minor = element_blank(), 
       panel.grid.major = element_blank(),
       plot.background = element_rect(fill = "transparent",colour = NA)) +
     labs(tag="B")
  
    # code below for adding R2 and pvals to plot -- this is for one line at upper right (x & y = "infinity"), right justified (hjust L=0/R=1/C=0.5)
    #annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.2, size=5,label=paste("list(italic(R^{2}) ==", 0.492, ", italic(P)<.001)"), parse=TRUE)
######################################################################################
######################################################################################
 P2 =  

   ggplot(MYdata, aes(PC1, PC2)) +
     theme_bw() +
     geom_point(aes(colour=MYdata$L6_DMM),size=6) +
     scale_colour_manual(name = "DMM Cluster                   ", labels = leg2, values = col2) +   
     ylab("PC2  (22%)")  +
     xlab("PC1  (45%)") +  
     theme(
       axis.ticks=element_blank(),
       text = element_text(size=25, family="Helvetica"),
       legend.position="none",
       plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill = "transparent",colour = NA),
       panel.grid.minor = element_blank(), 
       panel.grid.major = element_blank(),
       plot.background = element_rect(fill = "transparent",colour = NA)) +
     labs(tag="A")
  
    # code below for adding R2 and pvals to plot -- this is for one line at upper right (x & y = "infinity"), right justified (hjust L=0/R=1/C=0.5)
    #annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.2, size=5,label=paste("list(italic(R^{2}) ==", 0.207, ", italic(P)<.001)"), parse=TRUE)
######################################################################################
######################################################################################
  P3 =  

   ggplot(MYdata, aes(PC1, PC2)) +
     theme_bw() +
     geom_point(aes(colour=MYdata$WardCluster),size=5) +
     scale_colour_manual(name = "Heirarchical Cluster         ", labels = leg3, values = col3) +   
     ylab("PC2  (22%)")  +
     xlab("PC1  (45%)") +
     theme(
       axis.ticks=element_blank(),
       text = element_text(size=20, family="Helvetica"),
       #legend.position="none",
       plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill = "transparent",colour = NA),
       panel.grid.minor = element_blank(), 
       panel.grid.major = element_blank(),
       plot.background = element_rect(fill = "transparent",colour = NA))
  
    # code below for adding R2 and pvals to plot -- this is for one line at upper right (x & y = "infinity"), right justified (hjust L=0/R=1/C=0.5)
    #annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.2, size=5,label=paste("list(italic(R^{2}) ==", 0.299, ", italic(P)<.001)"), parse=TRUE)
######################################################################################

# LEGEND
  
# I like to isolate the legend and use it as a separate object to have better control over proportions
# Below is the code for a function I found that keeps just the legend; you will create the legend plot as an object then run this function
# on your legend plot to isolate the legend as another object
  
# g_legend function: run this code each time to initialize the function
  g_legend<-function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    return(legend)} 
  
# Copy/paste the code from any of your plots and # out the legend.position="none" line in the theme
# Can modify the legend title and text size, face, etc by adding to the theme() -- will override the global settings in text=element_text():
        legend.title = element_text(size=,face=)
        legend.title = element_text(size=,face=)
  
pleg1= 
  
  ggplot(MYdata, aes(PC1, PC2)) +
     theme_bw() +
     geom_point(aes(colour=MYdata$dom_sputum_genus),size=5) +
     scale_colour_manual(name = "Dominant Sputum\nGenus", labels = leg1, values = col1) +   
     ylab("PC2  (X%)")  +
     xlab("PC1  (X%)") +
     theme(
       axis.ticks=element_blank(),
       text = element_text(size=20, family="Helvetica"),
       plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill = "transparent",colour = NA),
       panel.grid.minor = element_blank(), 
       panel.grid.major = element_blank(),
       plot.background = element_rect(fill = "transparent",colour = NA))

leg1=g_legend(pleg1)  # your legend is now an object called "leg1"



pleg2=

  ggplot(MYdata, aes(PC1, PC2)) +
     theme_bw() +
     geom_point(aes(colour=MYdata$L6_DMM),size=5) +
     scale_colour_manual(name = "DMM", labels = leg2, values = col2) +
     ylab("PC2  (X%)")  +
     xlab("PC1  (X%)") +
     theme(
       axis.ticks=element_blank(),
       text = element_text(size=20, family="Helvetica"),
       plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill = "transparent",colour = NA),
       panel.grid.minor = element_blank(),
       panel.grid.major = element_blank(),
       plot.background = element_rect(fill = "transparent",colour = NA))

leg2=g_legend(pleg2)  # your legend is now an object called "leg2"



pleg3=

  ggplot(MYdata, aes(PC1, PC2)) +
     theme_bw() +
     geom_point(aes(colour=MYdata$WardCluster),size=5) +
     scale_colour_manual(name = "WardCluster", labels = leg3, values = col3) +
     ylab("PC2  (X%)")  +
     xlab("PC1  (X%)") +
     theme(
       axis.ticks=element_blank(),
       text = element_text(size=20, family="Helvetica"),
       plot.title = element_text(hjust = 0.5),
       panel.background = element_rect(fill = "transparent",colour = NA),
       panel.grid.minor = element_blank(),
       panel.grid.major = element_blank(),
       plot.background = element_rect(fill = "transparent",colour = NA))

leg3=g_legend(pleg3)  # your legend is now an object called "leg3"




# array your plots and legend
pdf("domtaxa_beta_plot.pdf", width = 12, height = 8)
P1
#grid.arrange(P1,leg1, ncol=2,widths=c(5,5))
dev.off()

pdf("DMM_beta_plot.pdf", width = 12, height = 8)
P2
#grid.arrange(P2,leg2, ncol=2,widths=c(5,5))
dev.off()

pdf("Ward_beta_plot.pdf", width = 12, height = 8)
P3
#grid.arrange(P3,leg3, ncol=2,widths=c(5,5))
dev.off()

pdf("Combined.pdf", width = 18, height = 6)
grid.arrange(P2,leg2,P1,leg1, ncol=4, widths=c(30,12,30,15))
dev.off()



# NOTE you will want to play around with the widths (and heights if more than 1 row of plots) to get the proportions right, and you can also
# do nested arrangements using arrangeGrob, which uses the same notation -- I can show you how to use these features if you need help
# ALSO you have another level of control over proportions when exporting the arrangement -- I save as PDF and play around with the dimensions and
# then preview the plot until it looks right before saving. takes some practice to get used to this but you will get the hang of it eventually











































