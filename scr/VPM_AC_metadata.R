## Combine functional trait data, species descriptions and CCA number for Victoria and Anne-Cecile's whole CCA dataset

library(tidyverse)
library(devtools)
library(readr)
library(sp)
library(raster)
library(stats)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)


d13C_data <- read.csv("../data/d13C_data.csv", header = TRUE) #+
colnames(d13C_data)[1]<-"RowTree"
nrow(d13C_data)
view(d13C_data)

CCAdata <- read.csv("../data/CCATreesCleaned.csv", header = TRUE)

species_dataset <- read.csv("../data/species_dataset.csv", header = TRUE)
view(species_dataset)
nrow(species_dataset)

full.df <- left_join(species_dataset, d13C_data, by = c("RowTree"))
nrow(full.df)
view(full.df)

u_full.df<-
  full.df %>% distinct(RowTree, .keep_all = T)
nrow(u_full.df)
view(u_full.df)

write.csv(u_full.df, "../outputs/species_functional_traits.csv")