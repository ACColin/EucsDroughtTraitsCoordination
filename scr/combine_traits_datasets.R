## script to combine the previous dataset with species functional traits to the d13C dataset

# the two datasets are combined according to the unique RowTree ID

library(tidyverse)

u_all.df.clean <- read.csv("output/u_all.df.clean.csv", header = TRUE)

euc_trait_table <- 
  read.csv("data/euc_trait_table.csv", header = T) %>% 
  mutate(species_name = 
           paste(binomen, "subsp.", subsp, "var.", variety) %>% 
           str_remove_all(" (var|subsp)\\. NA"))


view(taxa_raw)
full.df <- left_join(u_all.df.clean, taxa_raw.clean, by = c("species" = "Binomial"))
view(full.df)
nrow(full.df)
# filter unique_ID again
u_full.df<-
  full.df %>% distinct(unique_ID,.keep_all = T)
#view(u_full.df)
colnames(u_full.df)[4]<-"Lat"
u_full.df[[24]]<-NULL
nrow(u_full.df)
min(u_full.df$Long)

write.csv(u_full.df, "output/CCA.species.info.full.dataframe.final.csv")

cca_filtered <- 
  u_full.df %>% 
  filter(Section %in% c("Maidenaria", "Eucalyptus", "Exsertaria", "Adnataria"),
         between(year, 1994, 2001))
view(cca_filtered)

Plotting climate means:
  ```{r}

plot.mean.section<-ggplot(data = cca_filtered, mapping = aes(x = BIO1, y = BIO12)) + 
  geom_point(mapping = aes(color = Section))+
  facet_grid(Section ~ .)

print(plot.mean.section)
dev.print(pdf, 'figs/plot_bioclim_mean_CCA_sections.pdf')
