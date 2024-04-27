install.packages("rvest")
library(rvest)
#read html page
moves <- read_html("https://pokemondb.net/move/all")

#scrape moves table
move_info <- moves %>% html_table(fill=TRUE)
move_info

#import gym leader dataset
gym_pkmn <- read.csv("C:/Users/Ethan/Downloads/Gym_Pkmn.csv")

#add new scraped columns to this dataset with suffix pertaining to move1,2,etc
merge1 <- merge(gym_pkmn, move_info, by.x="move1", by.y="Name", all.x=TRUE)
col_range <- ncol(gym_pkmn)+ 1:7
names(merge1)[col_range] <- paste(names(merge1)[col_range], "move1", sep="_" )

merge1 <- merge(merge1, move_info, by.x="move2", by.y="Name", all.x=TRUE)
col_range <- ncol(gym_pkmn)+ 8:14
names(merge1)[col_range] <- paste(names(merge1)[col_range], "move2", sep="_" )

merge1 <- merge(merge1, move_info, by.x="move3", by.y="Name", all.x=TRUE)
col_range <- ncol(gym_pkmn)+ 15:21
names(merge1)[col_range] <- paste(names(merge1)[col_range], "move3", sep="_" )

merge1 <- merge(merge1, move_info, by.x="move4", by.y="Name", all.x=TRUE)
col_range <- ncol(gym_pkmn)+ 22:28
names(merge1)[col_range] <- paste(names(merge1)[col_range], "move4", sep="_" )


#name csv and share
write_csv(merge1, "gym_pkmn_moves.csv")

