# Doing a PCA using FactoMineR

# set working directory

library(FactoMineR)
library(factoextra)
library(corrplot)

# Read in your data
all.traits <- read.csv("Trait means.csv", header = TRUE)

#subset wet and dry data
wet <- subset(all.traits, Treatment == "wet")
dry <- subset(all.traits, Treatment == "dry")

#Remove non data info e.g. "Species", "Distribution" etc.
wet1 <- subset(wet[, c(1:2, 6:18)]) # edit column no's based on df
dry1 <- subset(dry[, c(1:2, 6:21)])
all1 <- subset(all.traits[, c(1:2, 6:21)])

# Check
wet1
dry1
all1

# Run the PCA, use graph = TRUE to get a quick look at the data

wet.pca <- PCA(wet1[, -1:-2], scale.unit = TRUE, 
               ncp = 13, graph = TRUE) # ncp = no of PCs to retain
dry.pca <- PCA(dry1[, -1:-2], 
               scale.unit = TRUE, ncp = 13, graph = TRUE) # graph = plots graph or not
all.pca <- PCA(all1[, -1:-2], scale.unit = TRUE, 
               ncp = 13, graph = TRUE) #scale.unit = scales the data

# Summary of functions. Can work with prcomp() etc.
#
#get_eigenvalue(wet.pca)                      # Extract the eigenvalues/variances of principal components
#fviz_eig(wet.pca)                            # Visualize the eigenvalues
#get_pca_ind(wet.pca), get_pca_var(wet.pca)   # Extract the results for individuals and variables, respectively.
#fviz_pca_ind(wet.pca), fviz_pca_var(wet.pca) # Visualize the results individuals and variables, respectively.
#fviz_pca_biplot(wet.pca)                     # Make a biplot of individuals and variables.

wet.eig <- get_eigenvalue(wet.pca) 
wet.eig
#      eigenvalue variance.percent cumulative.variance.percent
#Dim.1  3.3591149        25.839345                    25.83935
#Dim.2  3.0237922        23.259940                    49.09929
#Dim.3  2.6860499        20.661923                    69.76121
#Dim.4  1.3911968        10.701514                    80.46272
#Dim.5  1.1273413         8.671856                    89.13458
#Dim.6  0.8439110         6.491623                    95.62620
#Dim.7  0.5685939         4.373799                   100.00000

dry.eig <- get_eigenvalue(dry.pca) 
dry.eig
#      eigenvalue variance.percent cumulative.variance.percent
#Dim.1  3.9148188        30.113991                    30.11399
#Dim.2  3.1611033        24.316179                    54.43017
#Dim.3  2.4123483        18.556526                    72.98670
#Dim.4  1.4509207        11.160928                    84.14762
#Dim.5  1.2074179         9.287830                    93.43545
#Dim.6  0.6172359         4.747969                    98.18342
#Dim.7  0.2361551         1.816578                   100.00000

all.eig <- get_eigenvalue(all.pca) 
all.eig
#       eigenvalue variance.percent cumulative.variance.percent
#Dim.1  4.51936198       34.7643230                    34.76432
#Dim.2  2.21415754       17.0319810                    51.79630
#Dim.3  1.88679616       14.5138166                    66.31012
#Dim.4  1.66602462       12.8155740                    79.12569
#Dim.5  0.75634624        5.8180480                    84.94374
#Dim.6  0.71843910        5.5264546                    90.47020
#Dim.7  0.46148657        3.5498967                    94.02009
#Dim.8  0.30099133        2.3153179                    96.33541
#Dim.9  0.18058910        1.3891469                    97.72456
#Dim.10 0.14887437        1.1451875                    98.86975
#Dim.11 0.11058414        0.8506472                    99.72039
#Dim.12 0.02257932        0.1736871                    99.89408
#Dim.13 0.01376954        0.1059195                   100.00000

########

# Based on the cumulative variance, choose how many PCs to retain

# access the variance components
wet.var <- get_pca_var(wet.pca)
head(wet.var$coord) #coords of vars to make scatter plot
head(wet.var$cos2) #reps quality of representation for vars on factor map
head(wet.var$contrib) #% contributions of vars to PCs

dry.var <- get_pca_var(dry.pca)
head(dry.var$coord) #coords of vars to make scatter plot
head(dry.var$cos2) #reps quality of representation for vars on factor map
head(dry.var$contrib) #% contributions of vars to PCs


all.var <- get_pca_var(all.pca)
head(all.var$coord) #coords of vars to make scatter plot
head(all.var$cos2) #reps quality of representation for vars on factor map
head(all.var$contrib) #% contributions of vars to PCs


# Plot variables
fviz_pca_var(wet.pca, col.var = "black")
fviz_pca_var(dry.pca, col.var = "black")
fviz_pca_var(all.pca, col.var = "black")

# Quality of representation
# NB: High cos2 = good rep of var in the PC, 
#     low cos2 = var not perfectly represented by PCs


corrplot(wet.var$cos2, is.corr = FALSE)
fviz_pca_var(wet.pca, col.var = "cos2",
             gradient.cols = c("white", "blue", "red"),
             repel = TRUE)

corrplot(wet.var$contrib, is.corr = FALSE)
# Check the contributions to PC1 & PC2,
#     dashed line indicates expected average contribution
fviz_contrib(wet.pca, choice = "var", axes = 1, top = 10)   # individually
fviz_contrib(wet.pca, choice = "var", axes = 2, top = 10)   # individually 
fviz_contrib(wet.pca, choice = "var", axes = 1:2, top = 10) # combined

fviz_pca_var(wet.pca, col.var = "contrib",
             gradient.cols = c("white", "blue", "red"),
             repel = TRUE)


corrplot(dry.var$cos2, is.corr = FALSE)
fviz_pca_var(dry.pca, col.var = "cos2",
             gradient.cols = c("white", "blue", "red"),
             repel = TRUE)

corrplot(dry.var$contrib, is.corr = FALSE)
# Check the contributions to PC1 & PC2,
#     dashed line indicates expected average contribution
fviz_contrib(dry.pca, choice = "var", axes = 1, top = 10)   # individually
fviz_contrib(dry.pca, choice = "var", axes = 2, top = 10)   # individually 
fviz_contrib(dry.pca, choice = "var", axes = 1:2, top = 10) # combined

fviz_pca_var(dry.pca, col.var = "contrib",
             gradient.cols = c("white", "blue", "red"),
             repel = TRUE)



corrplot(all.var$cos2, is.corr = FALSE)
fviz_pca_var(all.pca, col.var = "cos2",
             gradient.cols = c("white", "blue", "red"),
             repel = TRUE)

corrplot(all.var$contrib, is.corr = FALSE)
# Check the contributions to PC1 & PC2,
#     dashed line indicates expected average contribution
fviz_contrib(all.pca, choice = "var", axes = 1, top = 10)   # individually
fviz_contrib(all.pca, choice = "var", axes = 2, top = 10)   # individually 
fviz_contrib(all.pca, choice = "var", axes = 1:2, top = 10) # combined

fviz_pca_var(all.pca, col.var = "contrib",
             gradient.cols = c("white", "blue", "red"),
             repel = TRUE)


wet.desc <- dimdesc(wet.pca, axes = c(1, 2), proba = 0.05)
wet.desc1 <- dimdesc(wet.pca, axes = c(1, 3), proba = 0.05)
wet.desc$Dim.1
#       correlation   p.value
#WD      0.8008266 0.01691988
#Cond   -0.7095826 0.04867285
#Photo  -0.7807842 0.02219619
wet.desc$Dim.2
#        correlation     p.value
#P50leaf   0.8513734 0.007320119
#P50stem   0.7598958 0.028672664
wet.desc1$Dim.3
#                 correlation     p.value
#Stomata           -0.8419729 0.008733511
#LeafMassFraction  -0.8939173 0.002752100



dry.desc <- dimdesc(dry.pca, axes = c(1, 2), proba = 0.05)
dry.desc1 <- dimdesc(dry.pca, axes = c(1, 3), proba = 0.05)
dry.desc$Dim.1
#           correlation      p.value
#Cond         0.9779741 2.627482e-05
#Photo        0.9706252 6.197963e-05
#gmin         0.7831898 2.151542e-02
#RGR_Height   0.7460908 3.352631e-02
#SLA         -0.7861553 2.069427e-02
dry.desc$Dim.2
#             correlation     p.value
#P50stem        0.8011162 0.01685011
#P50leaf        0.7444484 0.03413497
#Biomass       -0.7411159 0.03539059
#est_Leafarea  -0.7681952 0.02597650
dry.desc1$Dim.3
#                 correlation      p.value
#LeafMassFraction   0.9531541 0.0002480675


all.desc <- dimdesc(all.pca, axes = c(1, 2), proba = 0.05)
all.desc1 <- dimdesc(all.pca, axes = c(1, 3), proba = 0.05)
all.desc$Dim.1
#             correlation      p.value
#Cond           0.8793711 7.203460e-06
#RGR_Height     0.8583827 2.089032e-05
#Photo          0.8490267 3.184959e-05
#gmin           0.8149743 1.201760e-04
#Biomass        0.6167046 1.094360e-02
#est_Leafarea   0.5507811 2.703116e-02
#WD            -0.5035805 4.673810e-02
#Stomata       -0.5229312 3.767002e-02
all.desc$Dim.2
#        correlation      p.value
#P50leaf   0.9267557 2.494607e-07
#P50stem   0.8498848 3.067723e-05
all.desc1$Dim.3
#             correlation     p.value
#SLA            0.7024413 0.002411621
#Biomass        0.6297461 0.008940662
#est_Leafarea   0.6056882 0.012896937



################################
# Check where each species/provenance is

fviz_pca_biplot(wet.pca, repel = TRUE,
                col.var = "blue",
                col.ind = "grey46",
                xlab = "PC1 (25.84%)", ylab = "PC2 (23.26%)")
fviz_pca_biplot(dry.pca, repel = TRUE,
                col.var = "red",
                col.ind = "grey46",
                xlab = "PC1 (30.11%)", ylab = "PC2 (24.32%)")
fviz_pca_biplot(all.pca, repel = TRUE,
                col.var = "black",
                col.ind = "grey46",
                xlab = "PC1 (34.76%)", ylab = "PC2 (17.03%)")


###########################################################
###########################################################

# BIVARIATE PLOTS


# WD      x P50stem
# SLA     x P50leaf
# Biomass x P50leaf
# SLA     x gmin

plot(SLA ~ gmin,
     data = subset(wet, ID == "BCAN_WET"),
     type = 'p',
     pch = 16,
     col = 'blue',
     #xlim = c(4.0, 6.0), # P50 stem
     #xlim = c(3.5, 6.0), # P50 leaf
     xlim = c(4.0, 24), # gmin
     #ylim = c(0.45, 0.65), # WD
     ylim = c(7.0, 10.5), # SLA
     #ylim = c(100, 400), # Biomass
     #xlab = "Stem P50 (-MPa)",
     #xlab = "Leaf P50 (-MPa)",
     xlab = expression("g"[min]*" (mmol m"^-2*" s"^-1*")"),
     #ylab = expression("Wood Density (g cm"^3*")"),
     ylab = expression("SLA (m"^2*" Kg)"))
     #ylab = "Aboveground Biomass (g)")
points(SLA ~ gmin,
       data = subset(wet, ID == "BTEX_WET"),
       type = 'p',
       pch = 15,
       col = 'blue')
points(SLA ~ gmin,
       data = subset(wet, ID == "GMAC_WET"),
       type = 'p',
       pch = 16,
       col = 'black')
points(SLA ~ gmin,
       data = subset(wet, ID == "GCUL_WET"),
       type = 'p',
       pch = 15,
       col = 'black')
points(SLA ~ gmin,
       data = subset(wet, ID == "PHEA_WET"),
       type = 'p',
       pch = 16,
       col = 'seagreen4')
points(SLA ~ gmin,
       data = subset(wet, ID == "PVOY_WET"),
       type = 'p',
       pch = 15,
       col = 'seagreen4')
points(SLA ~ gmin,
       data = subset(wet, ID == "TYUR_WET"),
       type = 'p',
       pch = 16,
       col = 'red')
points(SLA ~ gmin,
       data = subset(wet, ID == "TACC_WET"),
       type = 'p',
       pch = 15,
       col = 'red')
w1 <- lm(SLA ~ gmin, data = wet)
abline(w1)
w1.sum <- summary(w1)
legend("bottomt",
       legend = c("BCAN_WET", "BTEX_WET", "GMAC_WET", "GCUL_WET",
                  "PHEA_WET", "PVOY_WET", "TYUR_WET", "TACC_WET"),
       pch = c(16, 15, 16, 15, 16, 15, 16, 15),
       col = c("blue", "blue", "black", "black", "seagreen4", "seagreen4",
               "red", "red"),
       cex = 1,
       bty = 'n')
legend('topleft',
       legend = "a)", cex = 1.3, bty = 'n')
legend('topright', legend = c(expression("R"^2*" = -0.037")),
       col = "black", bty = 'n')
       

plot(SLA ~ gmin,
     data = subset(dry, ID == "BCAN_DRY"),
     type = 'p',
     pch = 1,
     col = 'blue',
     #xlim = c(4.0, 6.0), # P50 stem
     #xlim = c(3.5, 6.0), # P50 leaf
     xlim = c(4.0, 24), # gmin
     #ylim = c(0.45, 0.65), # WD
     ylim = c(7.0, 10.5), # SLA
     #ylim = c(100, 400), # Biomass
     #xlab = "Stem P50 (-MPa)",
     #xlab = "Leaf P50 (-MPa)",
     xlab = expression("g"[min]*" (mmol m"^-2*" s"^-1*")"),
     #ylab = expression("Wood Density (g cm"^3*")"),
     ylab = expression("SLA (m"^2*" Kg)"))
#ylab = "Aboveground Biomass (g)")
points(SLA ~ gmin,
       data = subset(dry, ID == "BTEX_DRY"),
       type = 'p',
       pch = 0,
       col = 'blue')
points(SLA ~ gmin,
       data = subset(dry, ID == "GMAC_DRY"),
       type = 'p',
       pch = 1,
       col = 'black')
points(SLA ~ gmin,
       data = subset(dry, ID == "GCUL_DRY"),
       type = 'p',
       pch = 0,
       col = 'black')
points(SLA ~ gmin,
       data = subset(dry, ID == "PHEA_DRY"),
       type = 'p',
       pch = 1,
       col = 'seagreen4')
points(SLA ~ gmin,
       data = subset(dry, ID == "PVOY_DRY"),
       type = 'p',
       pch = 0,
       col = 'seagreen4')
points(SLA ~ gmin,
       data = subset(dry, ID == "TYUR_DRY"),
       type = 'p',
       pch = 1,
       col = 'red')
points(SLA ~ gmin,
       data = subset(dry, ID == "TACC_DRY"),
       type = 'p',
       pch = 0,
       col = 'red')
d1 <- lm(SLA ~ gmin, data = dry)
abline(d1)
d1.sum <- summary(d1)
legend("bottomright",
       legend = c("BCAN_DRY", "BTEX_DRY", "GMAC_DRY", "GCUL_DRY",
                  "PHEA_DRY", "PVOY_DRY", "TYUR_DRY", "TACC_DRY"),
       pch = c(1, 0, 1, 0, 1, 0, 1, 0),
       col = c("blue", "blue", "black", "black", "seagreen4", "seagreen4",
               "red", "red"),
       cex = 1,
       bty = 'n')
legend('topleft',
       legend = "b)", cex = 1.3, bty = 'n')
legend('topright', legend = c(expression("R"^2*" = 0.349")),
       col = "black", bty = 'n')


###########################################################
###########################################################

# PLOT Traits vs Treatment Factor

library(ggplot2)
library(tidyverse)



blake <- subset(all.traits, Species == "blake")
glauc <- subset(all.traits, Species == "glauc")
parra <- subset(all.traits, Species == "parra")
teret <- subset(all.traits, Species == "teret")


             
# Reorder the TREATMENT FACTORS
  
blake$Treatment <- factor(blake$Treatment, c("wet", "dry"))
glauc$Treatment <- factor(glauc$Treatment, c("wet", "dry"))
parra$Treatment <- factor(parra$Treatment, c("wet", "dry"))
teret$Treatment <- factor(teret$Treatment, c("wet", "dry"))

# PLOT

gmin <- ggplot(blake, aes(x = Treatment, y = gmin, color = Provenance)) +
  geom_point(position = position_dodge(width = 0)) +
  geom_point(aes(shape = Provenance, size = 2)) +
  geom_line(aes(group = Provenance)) +
  labs(x = "Treatment", y = expression(g[min]~(mmol~m^-2~s^-1))) +
  ylim(4, 24) +
  theme(axis.text = element_text(size = 25)) +
  theme_bw()
plot(gmin)


wood <- ggplot(teret, aes(x = Treatment, y = WD, color = Provenance)) +
  geom_point(position = position_dodge(width = 0)) +
  geom_point(aes(shape = Provenance, size = 2)) +
  geom_line(aes(group = Provenance)) +
  labs(x = "Treatment", y = expression(Wood~Density~(g~cm^3))) +
  ylim(0.45, 0.65) +
  theme(axis.text = element_text(size = 25)) +
  theme_bw()
plot(wood)


biomass <- ggplot(teret, aes(x = Treatment, y = Biomass, color = Provenance)) +
  geom_point(position = position_dodge(width = 0)) +
  geom_point(aes(shape = Provenance, size = 2)) +
  geom_line(aes(group = Provenance)) +
  labs(x = "Treatment", y = expression(Aboveground~Biomass~(g))) +
  ylim(100, 400) +
  theme(axis.text = element_text(size = 25)) +
  theme_bw()
plot(biomass)


sla <- ggplot(teret, aes(x = Treatment, y = SLA, color = Provenance)) +
  geom_point(position = position_dodge(width = 0)) +
  geom_point(aes(shape = Provenance, size = 2)) +
  geom_line(aes(group = Provenance)) +
  labs(x = "Treatment", y = expression(SLA~(m^2~Kg))) +
  ylim(7, 11) +
  theme(axis.text = element_text(size = 25)) +
  theme_bw()
plot(sla)


p50leaf <- ggplot(teret, aes(x = Treatment, y = P50leaf, color = Provenance)) +
  geom_point(position = position_dodge(width = 0)) +
  geom_point(aes(shape = Provenance, size = 2)) +
  geom_line(aes(group = Provenance)) +
  labs(x = "Treatment", y = expression(P[50~leaf]~(-MPa))) +
  ylim(3.5, 6.0) +
  theme(axis.text = element_text(size = 25)) +
  theme_bw()
plot(p50leaf)


p50stem <- ggplot(teret, aes(x = Treatment, y = P50stem, color = Provenance)) +
  geom_point(position = position_dodge(width = 0)) +
  geom_point(aes(shape = Provenance, size = 2)) +
  geom_line(aes(group = Provenance)) +
  labs(x = "Treatment", y = expression(P[50~stem]~(-MPa))) +
  ylim(4.0, 6.0) +
  theme(axis.text = element_text(size = 25)) +
  theme_bw()
plot(p50stem)


ldmc <- ggplot(teret, aes(x = Treatment, y = LDMC, color = Provenance)) +
  geom_point(position = position_dodge(width = 0)) +
  geom_point(aes(shape = Provenance, size = 2)) +
  geom_line(aes(group = Provenance)) +
  labs(x = "Treatment", y = LDMC~(g)) +
  ylim(0.38, 0.5) +
  theme(axis.text = element_text(size = 25)) +
  theme_bw()
plot(ldmc)


lmf <- ggplot(teret, aes(x = Treatment, y = LeafMassFraction, color = Provenance)) +
  geom_point(position = position_dodge(width = 0)) +
  geom_point(aes(shape = Provenance, size = 2)) +
  geom_line(aes(group = Provenance)) +
  labs(x = "Treatment", y = expression(Leaf~Mass~Fraction~(g~g^-1))) +
  ylim(0.3, 0.51) +
  theme(axis.text = element_text(size = 25)) +
  theme_bw()
plot(lmf)









