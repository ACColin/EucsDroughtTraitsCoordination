#Doing the PCA
eucstraits <- read.csv2()

eucstraits.pca <- prcomp(eucstraits[,c(1:7)],center = TRUE, scale. = TRUE) ## taking all rows of column 1 to 7

summary(eucstraits.pca)

str(eucstraits.pca)
# Plotting the PCA
library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
ggbiplot(eucstraits.pca,
         ellipse = FALSE,#look at other PCs (here PC1 and PC2 specified in choices) 
         choices = c(1,2),
         circle = FALSE, # put circle around center to see variability of samples
         obs.scale = 1, #scaling samples
         var.scale = 1, #scaling variables
         labels = species(eucstraits), #sample lables
         var.axes= TRUE, # showing variables arrows on plot
         groups = series) + # series for grouping samples
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "dark blue")) +
  ggtitle("PCA of ...") +
  theme_minimal()+
  theme(legend.position = "bottom")


