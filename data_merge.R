# Merge Files
nfl_games1 <- read.csv("nfl_game_history.csv")
nfl_games2 <- read.csv("nfl_game_history_2.csv")
nfl_games3 <- read.csv("nfl_game_history_3.csv")
nfl_games4 <- read.csv("nfl_game_history_4.csv")
nfl_games5 <- read.csv("nfl_game_history_5.csv")
nfl_games6 <- read.csv("nfl_game_history_6.csv")
nfl_games7 <- read.csv("nfl_game_history_7.csv")

all_nfl_games <- {}

all_nfl_games <- bind_rows(all_nfl_games, nfl_games1)
all_nfl_games <- bind_rows(all_nfl_games, nfl_games2)
all_nfl_games <- bind_rows(all_nfl_games, nfl_games3)
all_nfl_games <- bind_rows(all_nfl_games, nfl_games4)
all_nfl_games <- bind_rows(all_nfl_games, nfl_games5)
all_nfl_games <- bind_rows(all_nfl_games, nfl_games6)
all_nfl_games <- bind_rows(all_nfl_games, nfl_games7)

# Clean Up
all_nfl_games <- all_nfl_games[,-1]
summary(all_nfl_games)
all_nfl_games$OT.Status <- as.character(all_nfl_games$OT.Status)
all_nfl_games$OT.Status[all_nfl_games$OT.Status != "OT"] <- ""
all_nfl_games$OT.Status <- as.factor(all_nfl_games$OT.Status)

# Sort
all_nfl_games <- all_nfl_games %>%
  arrange(Season.Year, Week)

# Write to CSV
write.csv(all_nfl_games, "all_nfl_game_history.csv")
