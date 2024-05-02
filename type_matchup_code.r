library(tidyverse)
library(dplyr)
library(tidyr)
#------------------------------------------------------------------------------
#Organize a main dataset for typing advantage scoring
#DO NOT REPEAT THIS SECTION FOR EACH GYM

#Import data on pokemon typing, availability, and gym battles
routes_pkmn <- as_tibble(
  read.csv("C:/Users/Ethan/Downloads/Pokemon_Platinum_Names_Locations.csv"))
routes_ByGym <- as_tibble(
  read.csv("C:/Users/Ethan/Downloads/Updated_Locations_with_Gym_Sections.csv"))
main1 <- as_tibble(
  read.csv("C:/Users/Ethan/Downloads/main_pkmn_dataset.csv"))


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


#UPDATE: NEED TO REMOVE FAIRY TYPES
# Create a data frame with the Pokémon names and their original typings
pkmn_data <- read.csv("C:/RFiles/main_pkmn_dataset_Apr30.csv")
pokemon_changes <- data.frame(
  name = c("Clefairy", "Clefable", "Snubbull", "Granbull", 
           "Togepi", "Togetic", "Togekiss", "Marill", "Azumarill", 
           "Mime Jr.", "Mr. Mime", "Jigglypuff", "Wigglytuff", 
           "Chansey", "Blissey", "Gardevoir", "Mawile", 
           "Floette", "Florges"),
  type1 = c("Normal", "Normal", "Normal", "Normal", 
            "Normal", "Normal", "Normal", "Water", "Water", 
            "Psychic", "Psychic", "Normal", "Normal", 
            "Normal", "Normal", "Psychic", "Steel", 
            "Grass", "Grass"),
  type2 = c(NA, NA, NA, NA, 
            "Flying", "Flying", "Flying", NA, NA, 
            NA, NA, NA, NA, 
            NA, NA, NA, NA, 
            NA, NA)
)

# Replace the typings in your main dataset with the original typings
pkmn_data <- data.frame(
  name = c("Clefairy", "Clefable", "Togepi", "Togetic", "Togekiss", "Marill", 
           "Azumarill"),
  type1 = c("Normal", "Normal", "Normal", "Normal", "Normal", "Water", "Water"),
  type2 = c(NA, NA, "Flying", "Flying", "Flying", NA, NA)
)

pokemon_changes <- data.frame(
  name = c("Clefairy", "Clefable", "Togepi", "Togetic", "Togekiss", "Marill", 
           "Azumarill"),
  type1 = c("Normal", "Normal", "Normal", "Normal", "Normal", "Water", "Water"),
  type2 = c(NA, NA, "Flying", "Flying", "Flying", NA, NA)
)

# Replace the typings in pkmn_data with the original typings from pokemon_changes
pkmn_data1 <- pkmn_data %>%
  left_join(pokemon_changes, by = "name") %>%
  mutate(
    type1 = case_when(
      !is.na(type1.y) ~ type1.y,
      TRUE ~ type1.x
    ),
    type2 = case_when(
      !is.na(type2.y) ~ type2.y,
      TRUE ~ type2.x
    )
  ) %>%
  select(-type1.x,-type1.y, -type1.y, -type2.y)

cat(paste(colnames(pkmn_data1), collapse=", "), "/n")
pkmn_data2 <- pkmn_data1 %>% select(index, pokedex_number, name, type1, type2,
                Gym.Section, attack, base_total, defense, experience_growth, 
                hp, sp_attack, sp_defense, speed, 
                generation, is_legendary, Level, Additional.Criteria, 
                Location, Average.Level, against_bug, against_dark, 
                against_dragon, against_electric, against_fight,
                against_fire, against_flying, against_ghost, against_grass, 
                against_ground, against_ice, against_normal, against_poison, 
                against_psychic, against_rock, against_steel, against_water)

#Write updated dataset for github
setwd("C:/RFiles")
write_csv(pkmn_data2, "main_pkmn_dataset_May1.csv")

#-----------------------------------------------------------------------------
#Typing Match-up Analysis:
  #Filter pokemon available for gym battle by "Gym.Section" and evo "level"
  #Also remember to filter out legendary 
  #and later on pokemon with special evo criteria like stones that arent avail

#Load data
pkmn_data <- read.csv("C:/RFiles/main_pkmn_dataset_May1.csv")
gym_info <- as_tibble(
  read.csv("C:/Users/Ethan/Downloads/gym_pkmn_moves.csv"))

#for filtering, need to add a lag column for Level
sorted <- pkmn_data %>% arrange(index)
shift1 <- sorted %>% mutate(EVOd_at = lag(Level))

#now filter by gym section and level to find available pokemon
gym <- 1
gym_max_lvl=14
gym_filtered <- shift1 %>% 
  filter(Gym.Section < gym & (EVOd_at <= gym_max_lvl | is.na(EVOd_at)))
#note that Gallade and Gardevoir dont have EVOd_at values bc require dusk stone



#organize data for scoring type_advantage and resistance
gym_pkmn <- left_join(gym_info, pkmn_data, by="name")
gym_pkmn1 <- gym_pkmn %>% filter(gym_num == 1) %>% 
  distinct(name, .keep_all=TRUE)

#get unique types faced in gym
types_gym1 <- gym_pkmn1 %>% select(type1, type2) %>%
  pivot_longer(cols=everything(), values_to="type") %>% 
  filter(!is.na(type)) 
types_gym1

#occurrences of each type for all gym pkmn
type_counts <- gym_pkmn1 %>% 
  filter(type1 %in% types_gym1$type | type2 %in% types_gym1$type) %>%
  count(type1, type2) %>%
  arrange(type1, type2)
type_counts

type_counts_ttl <- types_gym1 %>% count(type) %>% filter(n > 1)
type_counts_ttl

#take the weighted average type_resist for each available pkmn; lower=better
#this is against gym pokemon typing, NOT moves faced
calculate_weighted_sum <- function(gym_filtered, type_counts_ttl) {
  relevant_columns <- paste0("against_", type_counts_ttl$type)
  relevant_resists <- as.numeric(gym_filtered[relevant_columns])  
  weighted_sum <- sum(type_counts_ttl$n * relevant_resists, na.rm = TRUE)  
  return(weighted_sum)
}

# Add a new column to gym_filtered called type_weakness_gym1
gym_filtered$type_resist_gym1 <- apply(gym_filtered, 1, 
                                        calculate_weighted_sum, 
                                        type_counts_ttl)


#Expand on this score factoring all move types that can be faced and STAB

#STILL NEED TO MANUALLY UPDATE FAIRY POKEMON TYPE RESISTANCES!!!!!!!
