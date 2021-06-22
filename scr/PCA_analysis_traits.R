# Loading dataset and libraries
eucstraits <- read.csv(...)
## the data consist of xx environmental and physiological variables related to drought tolerance in xx closely-related species of Eucalyptus with contrasting distributions and climate zones.

# Comparing variables with pairwise interactions:
pairs(eucstraits[,-1], col = wineClasses, upper.panel = NULL, pch = 16, cex = 0.5) # [-1] takes all variables
legend("topright", bty = "n", legend = c("Exsertaria","Adnataria"), pch = 16, col = c("black","red"),xpd = T, cex = 2, y.intersp = 0.5) # legend can also regroup samples by AI, or series, or else

#Perfom PCA
eucstraits <- read.csv2()

eucstraits.pca <- prcomp(eucstraits[,-1],center = TRUE, scale. = TRUE) ## taking all rows of all columns

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


# Adding other samples in
additionalsample <- c(#string of data listed according to variables in dataset for this sample)

eucstraitsplus <- rbind(eucstraits, additionalsample)

eucstraitsplus.pca <- prcomp(eucstraitsplus[,c(1:7)], center = TRUE, scale. = TRUE)
# then plot like above but using eucstraitsplus.pca for dataset 