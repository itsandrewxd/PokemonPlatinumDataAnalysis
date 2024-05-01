library(tidyverse)
library(dplyr)
library(tidyr)


#Import data on pokemon typing, availability, and gym battles
routes_pkmn <- as_tibble(
  read.csv("C:/Users/Ethan/Downloads/Pokemon_Platinum_Names_Locations.csv"))
routes_ByGym <- as_tibble(
  read.csv("C:/Users/Ethan/Downloads/Updated_Locations_with_Gym_Sections.csv"))
main1 <- as_tibble(
  read.csv("C:/Users/Ethan/Downloads/main_pkmn_dataset.csv"))
gym_info <- as_tibble(
  read.csv("C:/Users/Ethan/Downloads/gym_pkmn_moves.csv"))

#Combine location data
old_names <- c("X", "X0", "Location.1")
new_names <- c("index", "name", "Location")
routes_pkmn1 <- rename(routes_pkmn, !!!setNames(old_names, new_names))

routes_alldata <- left_join(routes_pkmn1, routes_ByGym, by="Location")

#add location info to main dataset
main2 <- left_join(main1, routes_alldata, by="name")

#fill "Gym.Section" NAs for evolved pokemon 
main2 <- main2 %>% arrange(index)
fill_na_gym_section <- function(df) {
  last_gym_section <- NULL
  for (i in 1:nrow(df)) {
    if (is.na(df$Gym.Section[i])) {
      df$Gym.Section[i] <- last_gym_section
    } else {
      last_gym_section <- df$Gym.Section[i]
    }
  }
  df
}

main3 <- fill_na_gym_section(main2)

#Write updated dataset for github
setwd("C:/RFiles")
write_csv(main3, "main_pkmn_dataset_Apr30.csv")

#-----------------------------------------------------------------------------
#Typing Match-up Analysis:
  #Filter pokemon available for gym battle by "Gym.Section" and evo "level"
  #Also remember to filter out legendary 
  #and later on pokemon with special evo criteria like stones that arent available
pkmn_data <- read.csv("C:/RFiles/main_pkmn_dataset_Apr30.csv")

#first need to add a lag column for Level
sorted <- pkmn_data %>% arrange(index)
shift1 <- sorted %>% mutate(EVOd_at = lag(Level))

#now filter by gym section and level to find available pokemon
gym <- 1
gym_max_lvl=14
gym_filtered <- shift1 %>% 
  filter(Gym.Section < gym & (EVOd_at <= gym_max_lvl | is.na(EVOd_at)))
#note that Gallade and Gardevoir dont have EVOd_at values bc require dusk stone


#to score type advantage find avg of pkmn type advantage against all gym_pkmn
#assume STAB will always make attack moves aligning w/ type superior
gym_pkmn <- left_join(gym_info, pkmn_data, by="name")
gym_pkmn1 <- gym_pkmn %>% filter(gym_num == 1) %>% 
  distinct(name, .keep_all=TRUE)

#get unique types faced in gym
types_gym1 <- gym_pkmn1 %>% select(type1, type2) %>%
  pivot_longer(cols=everything(), values_to="type") %>% 
  filter(!is.na(type)) 
types_gym1

#count occurrences of each type in gym pokemon
type_counts <- gym_pkmn1 %>% 
  filter(type1 %in% types_gym1$type | type2 %in% types_gym1$type) %>%
  count(type1, type2) %>%
  arrange(type1, type2)
type_counts

type_counts_ttl <- types_gym1 %>% count(type) %>% filter(n > 1)
type_counts_ttl

#take the weighted average of type advantage for each available pkmn




