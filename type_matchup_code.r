library(tidyverse)
library(dplyr)
library(tidyr)
#------------------------------------------------------------------------------
#DATA PREP: Organize a main dataset for typing advantage scoring
#DO NOT REPEAT THIS SECTION FOR EACH GYM

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
current_directory <- getwd()
current_directory
setwd("C:/RFiles")
write_csv(pkmn2, "main_pkmn_dataset.csv")

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


#Rearrange dataset columns
pkmn_data1 <- read.csv("C:/RFiles/main_pkmn_dataset_Apr30.csv")

cat(paste(colnames(pkmn_data1), collapse=", "), "/n")
pkmn_data1 <- pkmn_data1 %>% 
  select(index, pokedex_number, name, type1, type2,
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


#UPDATE: NEED TO REMOVE FAIRY TYPES
#Start with this dataset that has the correct typings
pkmn_data2 <- read.csv("C:/RFiles/main_pkmn_dataset_May1.csv")

# Define the list of Pokémon names (START with NORMAL)
pokemon_names <- c("Cleffa", "Clefairy", "Clefable", "Snubbull", "Granbull", 
                   "Jigglypuff", "Wigglytuff", "Chansey", "Blissey", "Togepi",
                   "Azurill")

# Update the resistance values in pkmn_data2
for (pokemon_name in pokemon_names) {
  pokemon_index <- which(pkmn_data2$name == pokemon_name)
  if (length(pokemon_index) > 0) {
    # Set all against_... columns to 1 except against_fight and against_ghost
    against_columns <- grep("^against_", colnames(pkmn_data2), value = TRUE)
    against_columns <- setdiff(against_columns, c("against_fight", 
                                                  "against_ghost"))
    pkmn_data2[pokemon_index, against_columns] <- 1.00
    
    # Set against_fighting to 2.00 and against_ghost to 0.00
    pkmn_data2[pokemon_index, "against_fight"] <- 2.00
    pkmn_data2[pokemon_index, "against_ghost"] <- 0.00
  }
}

#Now for NORMAL/FLYING
pokemon_names <- c("Togetic", "Togekiss")

for (pokemon_name in pokemon_names) {
  pokemon_index <- which(pkmn_data2$name == pokemon_name)
  if (length(pokemon_index) > 0) {
    against_columns <- grep("^against_", colnames(pkmn_data2), value = TRUE)
    against_columns <- setdiff(against_columns, 
                               c("against_electric", "against_grass",
                                 "against_ice", "against_ground", "against_bug",
                                 "against_ghost"))
    pkmn_data2[pokemon_index, against_columns] <- 1.00
    
    pkmn_data2[pokemon_index, "against_electric"] <- 2.00
    pkmn_data2[pokemon_index, "against_grass"] <- 0.50
    pkmn_data2[pokemon_index, "against_ice"] <- 2.00
    pkmn_data2[pokemon_index, "against_ground"] <- 0.00
    pkmn_data2[pokemon_index, "against_bug"] <- 0.50
    pkmn_data2[pokemon_index, "against_ghost"] <- 0.00
  }
}

#Now for WATER
pokemon_names <- c("Marill", "Azumarill")

for (pokemon_name in pokemon_names) {
  pokemon_index <- which(pkmn_data2$name == pokemon_name)
  if (length(pokemon_index) > 0) {
    against_columns <- grep("^against_", colnames(pkmn_data2), value = TRUE)
    against_columns <- setdiff(against_columns, 
                               c("against_fire", "against_water", 
                                 "against_electric", "against_grass", 
                                 "against_ice", "against_steel"))
    pkmn_data2[pokemon_index, against_columns] <- 1.00
    
    pkmn_data2[pokemon_index, "against_fire"] <- 0.50
    pkmn_data2[pokemon_index, "against_water"] <- 0.50
    pkmn_data2[pokemon_index, "against_electric"] <- 2.00
    pkmn_data2[pokemon_index, "against_ice"] <- 0.50
    pkmn_data2[pokemon_index, "against_steel"] <- 0.50
  }
}

#Now for PSYCHIC
pokemon_names <- c("Mr. Mime", "Mime Jr.", "Ralts", "Kirlia", "Gardevoir")

for (pokemon_name in pokemon_names) {
  pokemon_index <- which(pkmn_data2$name == pokemon_name)
  if (length(pokemon_index) > 0) {
    against_columns <- grep("^against_", colnames(pkmn_data2), value = TRUE)
    against_columns <- setdiff(against_columns, 
                               c("against_fight", "against_psychic", 
                                 "against_bug", "against_ghost", 
                                 "against_dark"))
    pkmn_data2[pokemon_index, against_columns] <- 1.00
    
    pkmn_data2[pokemon_index, "against_fight"] <- 0.50
    pkmn_data2[pokemon_index, "against_psychic"] <- 0.50
    pkmn_data2[pokemon_index, "against_bug"] <- 2.00
    pkmn_data2[pokemon_index, "against_ghost"] <- 2.00
    pkmn_data2[pokemon_index, "against_dark"] <- 2.00
  }
}

# Change fairy typings that were missed before
pokemon_index <- which(pkmn_data2$name == "Kirlia")
pkmn_data2[pokemon_index, c("type1", "type2")] <- c("Psychic", "")
pkmn_data2[pokemon_index, ]
pokemon_index <- which(pkmn_data2$name == "Ralts")
pkmn_data2[pokemon_index, c("type1", "type2")] <- c("Psychic", "")
pkmn_data2[pokemon_index, ]
pokemon_index <- which(pkmn_data2$name == "Gardevoir")
pkmn_data2[pokemon_index, c("type1", "type2")] <- c("Psychic", "")
pkmn_data2[pokemon_index, ]
pokemon_index <- which(pkmn_data2$name == "Mr. Mime")
pkmn_data2[pokemon_index, c("type1", "type2")] <- c("Psychic", "")
pkmn_data2[pokemon_index, ]
pokemon_index <- which(pkmn_data2$name == "Mime Jr.")
pkmn_data2[pokemon_index, c("type1", "type2")] <- c("Psychic", "")
pkmn_data2[pokemon_index, ]
pokemon_index <- which(pkmn_data2$name == "Cleffa")
pkmn_data2[pokemon_index, c("type1", "type2")] <- c("Normal", "")
pkmn_data2[pokemon_index, ]
pokemon_index <- which(pkmn_data2$name == "Azurill")
pkmn_data2[pokemon_index, c("type1", "type2")] <- c("Normal", "")
pkmn_data2[pokemon_index, ]
pokemon_index <- which(pkmn_data2$name == "Azumarill")
pkmn_data2[pokemon_index, c("type1", "type2")] <- c("Water", "")
pkmn_data2[pokemon_index, ]
pokemon_index <- which(pkmn_data2$name == "Kirlia")
pkmn_data2[pokemon_index, c("type1", "type2")] <- c("Water", "")
pkmn_data2[pokemon_index, ]


#Write updated dataset for github
setwd("C:/GitHub/STAT_527_Final_Project/csv")
write_csv(pkmn_data2, "main_pkmn_dataset_May2.csv")
#(later realized I could setwd to github repository & push changes 
#instead of reuploading versions manually)


#-------------------------------------------------------------------------------------------------------------
#TYPE MATCH-UP ANALYSIS:
  #Filter pokemon available for gym battle by "Gym.Section" and evo "level"
  #Also remember to filter out legendary 
  #and later on pokemon with special evo criteria like stones that arent avail

#Load data
setwd("C:/GitHub/STAT_527_Final_Project/csv")
pkmn_data <- read.csv("C:/GitHub/STAT_527_Final_Project/csv/main_pkmn_dataset_May2.csv")
gym_info <- as_tibble(
  read.csv("C:/GitHub/STAT_527_Final_Project/csv/gym_pkmn_moves.csv"))

#for filtering, need to add a lag column for Level
sorted <- pkmn_data %>% arrange(index)
shift1 <- sorted %>% mutate(EVOd_at = lag(Level))



#GYM 1!
#now filter by gym section and level to find available pokemon (MANUAL STEP REQ)
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

# Add a new column to gym_filtered called type_resist_gym1
gym_filtered$type_resist_gym1 <- apply(gym_filtered, 1, 
                                        calculate_weighted_sum, 
                                        type_counts_ttl)
gym_filtered <- gym_filtered %>% arrange(type_resist_gym1)
view(gym_filtered)


#Expand on this score factoring all move types that can be faced (NO STAB)
#STAB=same type attack bonus=1.5 dmg multiplier when pokemon type common w move

# Count the occurrences of each attack move_type for moves 1-4
gym_pkmn1 <- gym_pkmn1 %>%
  mutate(move2_stat = ifelse(name == "Onix", "phy", move2_stat)) #fix typo

move_type_counts <- bind_rows(
  gym_pkmn1 %>%
    filter(move1_stat == "phy" | move1_stat == "spec") %>%
    count(move1_type) %>%
    rename(type = move1_type, n = n),
  gym_pkmn1 %>%
    filter(move2_stat == "phy" | move2_stat == "spec") %>%
    count(move2_type) %>%
    rename(type = move2_type, n = n),
  gym_pkmn1 %>%
    filter(move3_stat == "phy" | move3_stat == "spec") %>%
    count(move3_type) %>%
    rename(type = move3_type, n = n),
  gym_pkmn1 %>%
    filter(move4_stat == "phy" | move4_stat == "spec") %>%
    count(move4_type) %>%
    rename(type = move4_type, n = n)
) %>%
  mutate(type = str_trim(type)) %>%
  group_by(type) %>%
  summarise(n = sum(n))
print(move_type_counts)


#Create a new column in gym_filtered movetype_resist_gym1
calculate_weighted_sum <- function(gym_filtered, move_type_counts) {
  relevant_columns <- paste0("against_", move_type_counts$type)
  relevant_resists <- as.numeric(gym_filtered[relevant_columns])  
  weighted_sum <- sum(move_type_counts$n * relevant_resists, na.rm = TRUE)  
  return(weighted_sum)
}

# Add a new column to gym_filtered called movetype_resist_gym1
gym_filtered$movetype_resist_gym1 <- apply(gym_filtered, 1, 
                                       calculate_weighted_sum, 
                                       move_type_counts)
gym_filtered <- gym_filtered %>% arrange(movetype_resist_gym1)
view(gym_filtered)


#calculate typing effectiveness score against gym pokemon (by STAB types)
#(Attempted but failed; too much )

# calculate_weighted_sum <- function(gym_filtered, gym_pkmn1) {
#   # Get the types of each Pokémon
#   types <- cbind(gym_filtered$type1, gym_filtered$type2)
#   
#   # Initialize an empty vector to store the results
#   type_dmg_gym1 <- numeric(nrow(gym_filtered))
#   
#   # Loop through each row of gym_filtered
#   for (i in 1:nrow(gym_filtered)) {
#     # Get the types of the current Pokémon
#     current_types <- types[i, ]
#     
#     # Get the corresponding columns in gym_pkmn1
#     relevant_columns <- paste0("against_", current_types)
#     
#     # Special case: if "fighting" corresponds to "against_fight"
#     relevant_columns[relevant_columns == "against_fighting"] <- "against_fight"
#     
#     # Calculate the score
#     score <- as.numeric(gym_pkmn1[i, relevant_columns[1]]) + 
#       as.numeric(gym_pkmn1[i, relevant_columns[2]])
#     
#     type_dmg_gym1[i] <- sum(score, na.rm = TRUE)
# }
# return(type_dmg_gym1)
# }
# type_dmg_gym1 <- calculate_weighted_sum(gym_filtered, gym_pkmn1)

#Finally combine type_resist and movetype_resist to make final recommendations
#0.5 weighting for type_resist to incorporate idea of STAB damage
#MANUALLY ENTER 1,2,3 at end of gym_filtered to create diff tables for each gym
gym_filtered1 <- gym_filtered %>%
  mutate(resist_score = 0.5*type_resist_gym1 + movetype_resist_gym1) %>%
  arrange(resist_score) %>%
  select(name, resist_score, type_resist_gym1, movetype_resist_gym1, Location)
gym_filtered1

#takeaways: gallade isnt available yet; start the game with Turtwig or Piplup 
#piplup bad against the next gym, but Empoleon is good later on
#from there Onix, Heracross, and Cherubi are good pick ups
#Bidoof, Budew, Aipom/Ambipom, or Munchlax/Snorlax are good options as well



################################################################
#Gym2!
#Load data
setwd("C:/GitHub/STAT_527_Final_Project/csv")
pkmn_data <- read.csv("C:/GitHub/STAT_527_Final_Project/csv/main_pkmn_dataset_May2.csv")
gym_info <- as_tibble(
  read.csv("C:/GitHub/STAT_527_Final_Project/csv/gym_pkmn_moves.csv"))

#for filtering, need to add a lag column for Level
sorted <- pkmn_data %>% arrange(index)
shift1 <- sorted %>% mutate(EVOd_at = lag(Level))

#Manually set gym info
gym <- 2
gym_max_lvl=22
gym_filtered <- shift1 %>% 
  filter(Gym.Section < gym & (EVOd_at <= gym_max_lvl | is.na(EVOd_at)))
#note that Gallade and Gardevoir dont have EVOd_at values bc require dusk stone

#organize data for scoring type_advantage and resistance (MANUALLY SET FILTER)
gym_pkmn <- left_join(gym_info, pkmn_data, by="name")
gym_pkmn1 <- gym_pkmn %>% filter(gym_num == 2) %>% 
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

# Add a new column to gym_filtered called type_resist_gym2
gym_filtered$type_resist_gym2 <- apply(gym_filtered, 1, 
                                       calculate_weighted_sum, 
                                       type_counts_ttl)
gym_filtered <- gym_filtered %>% arrange(type_resist_gym2)
view(gym_filtered)


#Expand on this score factoring all move types that can be faced (NO STAB)
#STAB=same type attack bonus=1.5 dmg multiplier when pokemon type common w move

# Count the occurrences of each attack move_type for moves 1-4

move_type_counts <- bind_rows(
  gym_pkmn1 %>%
    filter(move1_stat == "phy" | move1_stat == "spec") %>%
    count(move1_type) %>%
    rename(type = move1_type, n = n),
  gym_pkmn1 %>%
    filter(move2_stat == "phy" | move2_stat == "spec") %>%
    count(move2_type) %>%
    rename(type = move2_type, n = n),
  gym_pkmn1 %>%
    filter(move3_stat == "phy" | move3_stat == "spec") %>%
    count(move3_type) %>%
    rename(type = move3_type, n = n),
  gym_pkmn1 %>%
    filter(move4_stat == "phy" | move4_stat == "spec") %>%
    count(move4_type) %>%
    rename(type = move4_type, n = n)
) %>%
  mutate(type = str_trim(type)) %>%
  group_by(type) %>%
  summarise(n = sum(n))
print(move_type_counts)


#Create a new column in gym_filtered movetype_resist_gym2
calculate_weighted_sum <- function(gym_filtered, move_type_counts) {
  relevant_columns <- paste0("against_", move_type_counts$type)
  relevant_resists <- as.numeric(gym_filtered[relevant_columns])  
  weighted_sum <- sum(move_type_counts$n * relevant_resists, na.rm = TRUE)  
  return(weighted_sum)
}

# Add a new column to gym_filtered called movetype_resist_gym2
gym_filtered$movetype_resist_gym2 <- apply(gym_filtered, 1, 
                                           calculate_weighted_sum, 
                                           move_type_counts)
gym_filtered <- gym_filtered %>% arrange(movetype_resist_gym2)
view(gym_filtered)

#Finally combine type_resist and movetype_resist to make final recommendations
#0.5 weighting for type_resist to incorporate idea of STAB damage
#MANUALLY ENTER 1,2,3 at end of gym_filtered to create diff tables for each gym
gym_filtered2 <- gym_filtered %>%
  mutate(resist_score2 = 0.5*type_resist_gym2 + movetype_resist_gym2) %>%
  arrange(resist_score2) %>%
  select(name, resist_score2, type_resist_gym2, movetype_resist_gym2, Location)
gym_filtered2 
move_type_counts
#takeaways: Zubat, Dustox, and Skorupi come out ahead
#then choose from Budew, Roselia/Roserade, Mothim, Beautifly, Combee, Vespiquen



################################################################
#Gym3!
#Load data
setwd("C:/GitHub/STAT_527_Final_Project/csv")
pkmn_data <- read.csv("C:/GitHub/STAT_527_Final_Project/csv/main_pkmn_dataset_May2.csv")
gym_info <- as_tibble(
  read.csv("C:/GitHub/STAT_527_Final_Project/csv/gym_pkmn_moves.csv"))

#for filtering, need to add a lag column for Level
sorted <- pkmn_data %>% arrange(index)
shift1 <- sorted %>% mutate(EVOd_at = lag(Level))

#Manually set gym info
gym <- 3
gym_max_lvl=26
gym_filtered <- shift1 %>% 
  filter(Gym.Section < gym & (EVOd_at <= gym_max_lvl | is.na(EVOd_at)))
#note that Gallade and Gardevoir dont have EVOd_at values bc require dusk stone

#organize data for scoring type_advantage and resistance (MANUALLY SET FILTER)
gym_pkmn <- left_join(gym_info, pkmn_data, by="name")
gym_pkmn1 <- gym_pkmn %>% filter(gym_num == 3) %>% 
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

# Add a new column to gym_filtered called type_resist_gym3
gym_filtered$type_resist_gym3 <- apply(gym_filtered, 1, 
                                       calculate_weighted_sum, 
                                       type_counts_ttl)
gym_filtered <- gym_filtered %>% arrange(type_resist_gym3)
view(gym_filtered)


#Expand on this score factoring all move types that can be faced (NO STAB)
#STAB=same type attack bonus=1.5 dmg multiplier when pokemon type common w move

# Count the occurrences of each attack move_type for moves 1-4

move_type_counts <- bind_rows(
  gym_pkmn1 %>%
    filter(move1_stat == "phy" | move1_stat == "spec") %>%
    count(move1_type) %>%
    rename(type = move1_type, n = n),
  gym_pkmn1 %>%
    filter(move2_stat == "phy" | move2_stat == "spec") %>%
    count(move2_type) %>%
    rename(type = move2_type, n = n),
  gym_pkmn1 %>%
    filter(move3_stat == "phy" | move3_stat == "spec") %>%
    count(move3_type) %>%
    rename(type = move3_type, n = n),
  gym_pkmn1 %>%
    filter(move4_stat == "phy" | move4_stat == "spec") %>%
    count(move4_type) %>%
    rename(type = move4_type, n = n)
) %>%
  mutate(type = str_trim(type)) %>%
  group_by(type) %>%
  summarise(n = sum(n))
print(move_type_counts)


#Create a new column in gym_filtered movetype_resist_gym3
calculate_weighted_sum <- function(gym_filtered, move_type_counts) {
  relevant_columns <- paste0("against_", move_type_counts$type)
  relevant_resists <- as.numeric(gym_filtered[relevant_columns])  
  weighted_sum <- sum(move_type_counts$n * relevant_resists, na.rm = TRUE)  
  return(weighted_sum)
}

# Add a new column to gym_filtered called movetype_resist_gym3
gym_filtered$movetype_resist_gym3 <- apply(gym_filtered, 1, 
                                           calculate_weighted_sum, 
                                           move_type_counts)
gym_filtered <- gym_filtered %>% arrange(movetype_resist_gym3)
view(gym_filtered)

#Finally combine type_resist and movetype_resist to make final recommendations
#0.5 weighting for type_resist to incorporate idea of STAB damage
#MANUALLY ENTER 1,2,3 at end of gym_filtered to create diff tables for each gym
gym_filtered3 <- gym_filtered %>%
  mutate(resist_score2 = 0.5*type_resist_gym3 + movetype_resist_gym3) %>%
  arrange(resist_score2) %>%
  select(name, resist_score2, type_resist_gym3, movetype_resist_gym3, Location)
gym_filtered3 

#takeaways: Starly/Staravia, Chatot, Hoothoot, Togetic/Togekiss come out ahead
#then choose from Murkrow, Honchkrow, Stunky




################################################################
#Gym4!
#Load data
setwd("C:/GitHub/STAT_527_Final_Project/csv")
pkmn_data <- read.csv("C:/GitHub/STAT_527_Final_Project/csv/main_pkmn_dataset_May2.csv")
gym_info <- as_tibble(
  read.csv("C:/GitHub/STAT_527_Final_Project/csv/gym_pkmn_moves.csv"))

#for filtering, need to add a lag column for Level
sorted <- pkmn_data %>% arrange(index)
shift1 <- sorted %>% mutate(EVOd_at = lag(Level))

#Manually set gym info
gym <- 4
gym_max_lvl=32
gym_filtered <- shift1 %>% 
  filter(Gym.Section < gym & (EVOd_at <= gym_max_lvl | is.na(EVOd_at)))
#note that Gallade and Gardevoir dont have EVOd_at values bc require dusk stone

#organize data for scoring type_advantage and resistance (MANUALLY SET FILTER)
gym_pkmn <- left_join(gym_info, pkmn_data, by="name")
gym_pkmn1 <- gym_pkmn %>% filter(gym_num == 4) %>% 
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

# Add a new column to gym_filtered called type_resist_gym4
gym_filtered$type_resist_gym4 <- apply(gym_filtered, 1, 
                                       calculate_weighted_sum, 
                                       type_counts_ttl)
gym_filtered <- gym_filtered %>% arrange(type_resist_gym4)
view(gym_filtered)


#Expand on this score factoring all move types that can be faced (NO STAB)
#STAB=same type attack bonus=1.5 dmg multiplier when pokemon type common w move

# Count the occurrences of each attack move_type for moves 1-4

move_type_counts <- bind_rows(
  gym_pkmn1 %>%
    filter(move1_stat == "phy" | move1_stat == "spec") %>%
    count(move1_type) %>%
    rename(type = move1_type, n = n),
  gym_pkmn1 %>%
    filter(move2_stat == "phy" | move2_stat == "spec") %>%
    count(move2_type) %>%
    rename(type = move2_type, n = n),
  gym_pkmn1 %>%
    filter(move3_stat == "phy" | move3_stat == "spec") %>%
    count(move3_type) %>%
    rename(type = move3_type, n = n),
  gym_pkmn1 %>%
    filter(move4_stat == "phy" | move4_stat == "spec") %>%
    count(move4_type) %>%
    rename(type = move4_type, n = n)
) %>%
  mutate(type = str_trim(type)) %>%
  group_by(type) %>%
  summarise(n = sum(n))
print(move_type_counts)


#Create a new column in gym_filtered movetype_resist_gym4
calculate_weighted_sum <- function(gym_filtered, move_type_counts) {
  relevant_columns <- paste0("against_", move_type_counts$type)
  relevant_resists <- as.numeric(gym_filtered[relevant_columns])  
  weighted_sum <- sum(move_type_counts$n * relevant_resists, na.rm = TRUE)  
  return(weighted_sum)
}

# Add a new column to gym_filtered called movetype_resist_gym4
gym_filtered$movetype_resist_gym4 <- apply(gym_filtered, 1, 
                                           calculate_weighted_sum, 
                                           move_type_counts)
gym_filtered <- gym_filtered %>% arrange(movetype_resist_gym4)
view(gym_filtered)

#Finally combine type_resist and movetype_resist to make final recommendations
#0.5 weighting for type_resist to incorporate idea of STAB damage
#MANUALLY ENTER 1,2,3 at end of gym_filtered to create diff tables for each gym
gym_filtered4 <- gym_filtered %>%
  mutate(resist_score2 = 0.5*type_resist_gym4 + movetype_resist_gym4) %>%
  arrange(resist_score2) %>%
  select(name, resist_score2, type_resist_gym4, movetype_resist_gym4, Location)
gym_filtered4

#takeaways: Spiritomb and Bronzor are best, also use Torterra if you have it
#then choose from Misdreavus/Mismagius, Duskull, Scizor



################################################################
#Gym5!
#Load data
setwd("C:/GitHub/STAT_527_Final_Project/csv")
pkmn_data <- read.csv("C:/GitHub/STAT_527_Final_Project/csv/main_pkmn_dataset_May2.csv")
gym_info <- as_tibble(
  read.csv("C:/GitHub/STAT_527_Final_Project/csv/gym_pkmn_moves.csv"))

#for filtering, need to add a lag column for Level
sorted <- pkmn_data %>% arrange(index)
shift1 <- sorted %>% mutate(EVOd_at = lag(Level))

#Manually set gym info
gym <- 5
gym_max_lvl=37
gym_filtered <- shift1 %>% 
  filter(Gym.Section < gym & (EVOd_at <= gym_max_lvl | is.na(EVOd_at)))
#note that Gallade and Gardevoir dont have EVOd_at values bc require dusk stone

#organize data for scoring type_advantage and resistance (MANUALLY SET FILTER)
gym_pkmn <- left_join(gym_info, pkmn_data, by="name")
gym_pkmn1 <- gym_pkmn %>% filter(gym_num == 5) %>% 
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

# Add a new column to gym_filtered called type_resist_gym5
gym_filtered$type_resist_gym5 <- apply(gym_filtered, 1, 
                                       calculate_weighted_sum, 
                                       type_counts_ttl)
gym_filtered <- gym_filtered %>% arrange(type_resist_gym5)
view(gym_filtered)


#Expand on this score factoring all move types that can be faced (NO STAB)
#STAB=same type attack bonus=1.5 dmg multiplier when pokemon type common w move

# Count the occurrences of each attack move_type for moves 1-4

move_type_counts <- bind_rows(
  gym_pkmn1 %>%
    filter(move1_stat == "phy" | move1_stat == "spec") %>%
    count(move1_type) %>%
    rename(type = move1_type, n = n),
  gym_pkmn1 %>%
    filter(move2_stat == "phy" | move2_stat == "spec") %>%
    count(move2_type) %>%
    rename(type = move2_type, n = n),
  gym_pkmn1 %>%
    filter(move3_stat == "phy" | move3_stat == "spec") %>%
    count(move3_type) %>%
    rename(type = move3_type, n = n),
  gym_pkmn1 %>%
    filter(move4_stat == "phy" | move4_stat == "spec") %>%
    count(move4_type) %>%
    rename(type = move4_type, n = n)
) %>%
  mutate(type = str_trim(type)) %>%
  group_by(type) %>%
  summarise(n = sum(n))
print(move_type_counts)


#Create a new column in gym_filtered movetype_resist_gym5
calculate_weighted_sum <- function(gym_filtered, move_type_counts) {
  relevant_columns <- paste0("against_", move_type_counts$type)
  relevant_resists <- as.numeric(gym_filtered[relevant_columns])  
  weighted_sum <- sum(move_type_counts$n * relevant_resists, na.rm = TRUE)  
  return(weighted_sum)
}

# Add a new column to gym_filtered called movetype_resist_gym5
gym_filtered$movetype_resist_gym5 <- apply(gym_filtered, 1, 
                                           calculate_weighted_sum, 
                                           move_type_counts)
gym_filtered <- gym_filtered %>% arrange(movetype_resist_gym5)
view(gym_filtered)

#Finally combine type_resist and movetype_resist to make final recommendations
#0.5 weighting for type_resist to incorporate idea of STAB damage
#MANUALLY ENTER 1,2,3 at end of gym_filtered to create diff tables for each gym
gym_filtered5 <- gym_filtered %>%
  mutate(resist_score2 = 0.5*type_resist_gym5 + movetype_resist_gym5) %>%
  arrange(resist_score2) %>%
  select(name, resist_score2, type_resist_gym5, movetype_resist_gym5, Location)
gym_filtered5

#takeaways: Empoleon is best-- but cant use if isnt your starter
#then choose from Bibarel, Golduck, Foatzel, Seaking, Azumarill,
#Octillery, Lumineon, Vaporeon

#NOTE-- you'll want to teach these pokemon a move super effective against water
# if they don't naturally learn one themselves




################################################################
#Gym6!
#Load data
setwd("C:/GitHub/STAT_527_Final_Project/csv")
pkmn_data <- read.csv("C:/GitHub/STAT_527_Final_Project/csv/main_pkmn_dataset_May2.csv")
gym_info <- as_tibble(
  read.csv("C:/GitHub/STAT_527_Final_Project/csv/gym_pkmn_moves.csv"))

#for filtering, need to add a lag column for Level
sorted <- pkmn_data %>% arrange(index)
shift1 <- sorted %>% mutate(EVOd_at = lag(Level))

#Manually set gym info
gym <- 6
gym_max_lvl=41
gym_filtered <- shift1 %>% 
  filter(Gym.Section < gym & (EVOd_at <= gym_max_lvl | is.na(EVOd_at)))
#note that Gallade and Gardevoir dont have EVOd_at values bc require dusk stone

#organize data for scoring type_advantage and resistance (MANUALLY SET FILTER)
gym_pkmn <- left_join(gym_info, pkmn_data, by="name")
gym_pkmn1 <- gym_pkmn %>% filter(gym_num == 6) %>% 
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

# Add a new column to gym_filtered called type_resist_gym6
gym_filtered$type_resist_gym6 <- apply(gym_filtered, 1, 
                                       calculate_weighted_sum, 
                                       type_counts_ttl)
gym_filtered <- gym_filtered %>% arrange(type_resist_gym6)
view(gym_filtered)


#Expand on this score factoring all move types that can be faced (NO STAB)
#STAB=same type attack bonus=1.5 dmg multiplier when pokemon type common w move

# Count the occurrences of each attack move_type for moves 1-4

move_type_counts <- bind_rows(
  gym_pkmn1 %>%
    filter(move1_stat == "phy" | move1_stat == "spec") %>%
    count(move1_type) %>%
    rename(type = move1_type, n = n),
  gym_pkmn1 %>%
    filter(move2_stat == "phy" | move2_stat == "spec") %>%
    count(move2_type) %>%
    rename(type = move2_type, n = n),
  gym_pkmn1 %>%
    filter(move3_stat == "phy" | move3_stat == "spec") %>%
    count(move3_type) %>%
    rename(type = move3_type, n = n),
  gym_pkmn1 %>%
    filter(move4_stat == "phy" | move4_stat == "spec") %>%
    count(move4_type) %>%
    rename(type = move4_type, n = n)
) %>%
  mutate(type = str_trim(type)) %>%
  group_by(type) %>%
  summarise(n = sum(n))
print(move_type_counts)


#Create a new column in gym_filtered movetype_resist_gym6
calculate_weighted_sum <- function(gym_filtered, move_type_counts) {
  relevant_columns <- paste0("against_", move_type_counts$type)
  relevant_resists <- as.numeric(gym_filtered[relevant_columns])  
  weighted_sum <- sum(move_type_counts$n * relevant_resists, na.rm = TRUE)  
  return(weighted_sum)
}

# Add a new column to gym_filtered called movetype_resist_gym6
gym_filtered$movetype_resist_gym6 <- apply(gym_filtered, 1, 
                                           calculate_weighted_sum, 
                                           move_type_counts)
gym_filtered <- gym_filtered %>% arrange(movetype_resist_gym6)
view(gym_filtered)

#Finally combine type_resist and movetype_resist to make final recommendations
#0.5 weighting for type_resist to incorporate idea of STAB damage
#MANUALLY ENTER 1,2,3 at end of gym_filtered to create diff tables for each gym
gym_filtered6 <- gym_filtered %>%
  mutate(resist_score2 = 0.5*type_resist_gym6 + movetype_resist_gym6) %>%
  arrange(resist_score2) %>%
  select(name, resist_score2, type_resist_gym6, movetype_resist_gym6, Location)
gym_filtered6

#takeaways: Gastrodon, Barboach, Whiscash, Quagsire are best
#then choose from Gliscor or Empoleon




################################################################
#Gym7!
#Load data
setwd("C:/GitHub/STAT_527_Final_Project/csv")
pkmn_data <- read.csv("C:/GitHub/STAT_527_Final_Project/csv/main_pkmn_dataset_May2.csv")
gym_info <- as_tibble(
  read.csv("C:/GitHub/STAT_527_Final_Project/csv/gym_pkmn_moves.csv"))

#for filtering, need to add a lag column for Level
sorted <- pkmn_data %>% arrange(index)
shift1 <- sorted %>% mutate(EVOd_at = lag(Level))

#Manually set gym info
gym <- 7
gym_max_lvl=44
gym_filtered <- shift1 %>% 
  filter(Gym.Section < gym & (EVOd_at <= gym_max_lvl | is.na(EVOd_at)))
#note that Gallade and Gardevoir dont have EVOd_at values bc require dusk stone

#organize data for scoring type_advantage and resistance (MANUALLY SET FILTER)
gym_pkmn <- left_join(gym_info, pkmn_data, by="name")
gym_pkmn1 <- gym_pkmn %>% filter(gym_num == 7) %>% 
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

# Add a new column to gym_filtered called type_resist_gym7
gym_filtered$type_resist_gym7 <- apply(gym_filtered, 1, 
                                       calculate_weighted_sum, 
                                       type_counts_ttl)
gym_filtered <- gym_filtered %>% arrange(type_resist_gym7)
view(gym_filtered)


#Expand on this score factoring all move types that can be faced (NO STAB)
#STAB=same type attack bonus=1.5 dmg multiplier when pokemon type common w move

# Count the occurrences of each attack move_type for moves 1-4

move_type_counts <- bind_rows(
  gym_pkmn1 %>%
    filter(move1_stat == "phy" | move1_stat == "spec") %>%
    count(move1_type) %>%
    rename(type = move1_type, n = n),
  gym_pkmn1 %>%
    filter(move2_stat == "phy" | move2_stat == "spec") %>%
    count(move2_type) %>%
    rename(type = move2_type, n = n),
  gym_pkmn1 %>%
    filter(move3_stat == "phy" | move3_stat == "spec") %>%
    count(move3_type) %>%
    rename(type = move3_type, n = n),
  gym_pkmn1 %>%
    filter(move4_stat == "phy" | move4_stat == "spec") %>%
    count(move4_type) %>%
    rename(type = move4_type, n = n)
) %>%
  mutate(type = str_trim(type)) %>%
  group_by(type) %>%
  summarise(n = sum(n))
print(move_type_counts)


#Create a new column in gym_filtered movetype_resist_gym7
calculate_weighted_sum <- function(gym_filtered, move_type_counts) {
  relevant_columns <- paste0("against_", move_type_counts$type)
  relevant_resists <- as.numeric(gym_filtered[relevant_columns])  
  weighted_sum <- sum(move_type_counts$n * relevant_resists, na.rm = TRUE)  
  return(weighted_sum)
}

# Add a new column to gym_filtered called movetype_resist_gym7
gym_filtered$movetype_resist_gym7 <- apply(gym_filtered, 1, 
                                           calculate_weighted_sum, 
                                           move_type_counts)
gym_filtered <- gym_filtered %>% arrange(movetype_resist_gym7)
view(gym_filtered)

#Finally combine type_resist and movetype_resist to make final recommendations
#0.5 weighting for type_resist to incorporate idea of STAB damage
#MANUALLY ENTER 1,2,3 at end of gym_filtered to create diff tables for each gym
gym_filtered7 <- gym_filtered %>%
  mutate(resist_score2 = 0.5*type_resist_gym7 + movetype_resist_gym7) %>%
  arrange(resist_score2) %>%
  select(name, resist_score2, type_resist_gym7, movetype_resist_gym7, Location)
gym_filtered7

#takeaways: Empoleon is best, then Scizor and Lucario
#then choose from Sneasel, Weavile, Bibarel




################################################################
#Gym8!
#Load data
setwd("C:/GitHub/STAT_527_Final_Project/csv")
pkmn_data <- read.csv("C:/GitHub/STAT_527_Final_Project/csv/main_pkmn_dataset_May2.csv")
gym_info <- as_tibble(
  read.csv("C:/GitHub/STAT_527_Final_Project/csv/gym_pkmn_moves.csv"))

#for filtering, need to add a lag column for Level
sorted <- pkmn_data %>% arrange(index)
shift1 <- sorted %>% mutate(EVOd_at = lag(Level))

#Manually set gym info
gym <- 8
gym_max_lvl=50
gym_filtered <- shift1 %>% 
  filter(Gym.Section < gym & (EVOd_at <= gym_max_lvl | is.na(EVOd_at)))
#note that Gallade and Gardevoir dont have EVOd_at values bc require dusk stone

#organize data for scoring type_advantage and resistance (MANUALLY SET FILTER)
gym_pkmn <- left_join(gym_info, pkmn_data, by="name")
gym_pkmn1 <- gym_pkmn %>% filter(gym_num == 8) %>% 
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

# Add a new column to gym_filtered called type_resist_gym8
gym_filtered$type_resist_gym8 <- apply(gym_filtered, 1, 
                                       calculate_weighted_sum, 
                                       type_counts_ttl)
gym_filtered <- gym_filtered %>% arrange(type_resist_gym8)
view(gym_filtered)


#Expand on this score factoring all move types that can be faced (NO STAB)
#STAB=same type attack bonus=1.5 dmg multiplier when pokemon type common w move

# Count the occurrences of each attack move_type for moves 1-4

move_type_counts <- bind_rows(
  gym_pkmn1 %>%
    filter(move1_stat == "phy" | move1_stat == "spec") %>%
    count(move1_type) %>%
    rename(type = move1_type, n = n),
  gym_pkmn1 %>%
    filter(move2_stat == "phy" | move2_stat == "spec") %>%
    count(move2_type) %>%
    rename(type = move2_type, n = n),
  gym_pkmn1 %>%
    filter(move3_stat == "phy" | move3_stat == "spec") %>%
    count(move3_type) %>%
    rename(type = move3_type, n = n),
  gym_pkmn1 %>%
    filter(move4_stat == "phy" | move4_stat == "spec") %>%
    count(move4_type) %>%
    rename(type = move4_type, n = n)
) %>%
  mutate(type = str_trim(type)) %>%
  group_by(type) %>%
  summarise(n = sum(n))
print(move_type_counts)


#Create a new column in gym_filtered movetype_resist_gym8
calculate_weighted_sum <- function(gym_filtered, move_type_counts) {
  relevant_columns <- paste0("against_", move_type_counts$type)
  relevant_resists <- as.numeric(gym_filtered[relevant_columns])  
  weighted_sum <- sum(move_type_counts$n * relevant_resists, na.rm = TRUE)  
  return(weighted_sum)
}

# Add a new column to gym_filtered called movetype_resist_gym8
gym_filtered$movetype_resist_gym8 <- apply(gym_filtered, 1, 
                                           calculate_weighted_sum, 
                                           move_type_counts)
gym_filtered <- gym_filtered %>% arrange(movetype_resist_gym8)
view(gym_filtered)

#Finally combine type_resist and movetype_resist to make final recommendations
#0.5 weighting for type_resist to incorporate idea of STAB damage
#MANUALLY ENTER 1,2,3 at end of gym_filtered to create diff tables for each gym
gym_filtered8 <- gym_filtered %>%
  mutate(resist_score2 = 0.5*type_resist_gym8 + movetype_resist_gym8) %>%
  arrange(resist_score2) %>%
  select(name, resist_score2, type_resist_gym8, movetype_resist_gym8, Location)
gym_filtered8

#takeaways: Gastrodon, Barboach, Wiscash, and Quagsire are best
#then choose from Golem, Steelix, Rhyperior, Rotom

########################################################

#Here's an easier way to look at it
type_results <- list(head(gym_filtered1,20), head(gym_filtered2,20),
                     head(gym_filtered3,20), head(gym_filtered4,20),
                     head(gym_filtered5,20), head(gym_filtered6,20), 
                      head(gym_filtered7,20), head(gym_filtered8,20))
print(type_results)






