library("tidyverse")

#import datasets from kaggle and pokemondb.net
kag1 <- read.csv("C:/Users/Ethan/Downloads/pokemon.csv")
plat_names <- read.csv("C:/Users/Ethan/Downloads/Pokemon_Platinum_Names.csv")
evo_lvls <- read.csv("C:/Users/Ethan/Downloads/Pokemon_Evolution.csv")

#create tibbles
pkmn <- as_tibble(kag1)
nrow(pkmn)

plat_names1 <- as_tibble(plat_names)
old_names <- c("X", "X0")
new_names <- c("index", "name")
plat_names2 <- rename(plat_names1, !!!setNames(old_names, new_names))

#filter out non-gen 4s
#pokemon in game may be introduced in different gens
pkmn1 <- pkmn %>% filter(name %in% plat_names2$name) 


#add evolution levels
pkmn2 <- left_join(pkmn1, evo_lvls, by=c("name" = "Pokemon.Name"))

#share
write_csv(pkmn2, "main_pkmn_dataset.csv")
  
current_directory <- getwd()
current_directory
setwd("C:/RFiles")

