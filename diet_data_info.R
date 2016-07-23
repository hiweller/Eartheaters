setwd('/Users/hannah/Dropbox/Westneat_Lab/Eartheater Project/Data/Diet/')
library('rfishbase')
library('dplyr')

genera <- c('Acarichthys', 'Apistogramma', 'Biotodoma', 'Biotoecus',
            'Crenicichla', 'Crenicara', 'Dicrossus', 'Geophagus', 'Guianacara',
            'Gymnogeophagus', 'Mazarunia', 'Mikrogeophagus',
            'Satanoperca', 'Taeniacara', 'Teleocichla')

geophagines_diet_info <- vector("list", length(genera))
names(geophagines_diet_info) <- genera

species <- vector()  
for (i in 1:length(genera)) {
  species <- append(species, species_list(Genus = genera[i]))
}
write.csv((as.data.frame(species)), file = './all_geophagine_species.csv')
for (i in 1:length(genera)) {
  species_names <- species_list(Genus=genera[i])
  geophagines_diet_info[[i]] <- fooditems(species_list = species_names)
}

save(geophagines_diet_info, file = './geophagines_diet_info.RData')
geophagines2 <- bind_rows(geophagines_diet_info)
write.csv(geophagines2, file = './geophagines_diet_info.csv')
