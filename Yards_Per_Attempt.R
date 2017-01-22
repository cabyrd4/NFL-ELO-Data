library("rvest")
library("tidyr")
library("dplyr")
library("stringr")

# Data Import
all_games <- read.csv("all_nfl_game_history.csv")
team_code <- read.csv("team_name_ref.csv")
month_ref <- read.csv("month_ref.csv")

# Easy Variables
url1 <- "http://www.pro-football-reference.com/boxscores/"
cal_year <- str_replace(substr(all_games$Game.Date,8,12)," ","")
url2 <- ".htm"

# Day Number
day <- str_replace(substr(substr(all_games$Game.Date,5,7),1,2),",","")
day <- str_pad(day, 2, pad = "0")

# Month Number
all_games$cal_month <- str_replace(substr(all_games$Game.Date,1,4)," ","")
all_games <- left_join(all_games, month_ref, by = c("cal_month" = "month_abrv"))
all_games <- rename(all_games, cal_month_num = month_num)
all_games$cal_month_num<- str_pad(all_games$cal_month_num, 2, pad = "0")

# Add Team Codes
all_games <- left_join(all_games, team_code, by = c("Away.Team" = "team_name_long"))
all_games <- left_join(all_games, team_code, by = c("Home.Team" = "team_name_long"))
all_games <- rename(all_games, Away.Team.Code = team_name_short.x, Home.Team.Code = team_name_short.y)

# Team Code Name Check
#unique(all_games$Away.Team[is.na(all_games$team_name_short) == TRUE])

# Create Variable
all_games$boxscore_helper <- paste0(url1, cal_year, all_games$cal_month_num, day, 0, all_games$Home.Team.Code,url2)
head(all_games$boxscore_helper)

# Fix Mistakes
all_games$boxscore_helper[9589] <- "http://www.pro-football-reference.com/boxscores/201102060pit.htm"
all_games$boxscore_helper[9589+535-1] <- "http://www.pro-football-reference.com/boxscores/201302030sfo.htm"

# For Loop
all_nfl_stats = {}
for(link in second_try) { #print(link)}
url <- link

# Scrape Team Stats
nfl <- url %>%
  read_html() %>%
  html_nodes(xpath ='//comment()') %>%
  html_text() %>%
  paste(collapse = '') %>%
  read_html() %>%
  html_node('table#team_stats') %>%
  html_table() %>%
  .[colSums(is.na(.)) < nrow(.)]

# Rename Columns
names(nfl)[1] <- "Stat"
names(nfl)[2] <- "Away"
names(nfl)[3] <- "Home"

# Grab Yards & Attempts
nfl_pass <- nfl %>%
  filter(Stat == "Cmp-Att-Yd-TD-INT") %>%
  separate(Home, c("H-Cmp","H-Att","H-Yd","H-TD","H-INT"),'-') %>%
  separate(Away, c("A-Cmp","A-Att","A-Yd","A-TD","A-INT"),'-') %>%
  gather("Stats", "Values", 2:11) %>%
  filter(Stats == "A-Att" | Stats == "H-Att" | Stats == "A-Yd" | Stats == "H-Yd") %>%
  select(-Stat)

# Grab Sacks & Yard Loss
nfl_sack <- nfl %>%
  filter(Stat == "Sacked-Yards") %>%
  separate(Home, c("H-Sacks","H-Sack_Yd"),'-') %>%
  separate(Away, c("A-Sacks","A-Sack_Yd"),'-') %>%
  gather("Stats", "Values", 2:5) %>%
  select(-Stat)

# Combine & Clean Up
nfl_stats <- bind_rows(nfl_pass, nfl_sack) %>%
  spread(Stats, Values)

nfl_stats$A_adj_pass_yds <- as.numeric(nfl_stats$`A-Yd`) - as.numeric(nfl_stats$`A-Sack_Yd`)
nfl_stats$H_adj_pass_yds <- as.numeric(nfl_stats$`H-Yd`) - as.numeric(nfl_stats$`H-Sack_Yd`)

nfl_stats$A_adj_pass_att <- as.numeric(nfl_stats$`A-Att`) + as.numeric(nfl_stats$`A-Sacks`)
nfl_stats$H_adj_pass_att <- as.numeric(nfl_stats$`H-Att`) + as.numeric(nfl_stats$`H-Sacks`)

nfl_stats <- select(nfl_stats, A_adj_pass_yds, A_adj_pass_att, H_adj_pass_yds, H_adj_pass_att)

# Add Key
nfl_stats$url <- link

# Combine data
all_nfl_stats <- bind_rows(all_nfl_stats, nfl_stats)

# end loop
}

# Write CSV
write.csv(all_nfl_stats, "all_nfl_stats2.csv")

# Merge
nfl_stats1 <- read.csv("all_nfl_stats.csv")
nfl_stats2 <- read.csv("all_nfl_stats2.csv")

all_nfl_stats <- {}

all_nfl_stats <- bind_rows(all_nfl_stats, nfl_stats1)
all_nfl_stats <- bind_rows(all_nfl_stats, nfl_stats2)

write.csv(all_nfl_stats, "nfl_stat_history.csv")

# Combine and Consolidate
combo_data <- left_join(all_games, all_nfl_stats, by = c("boxscore_helper" = "url"))
combo_data <- select(combo_data, Away.Team:Week, Away.Team.Code, Home.Team.Code, A_adj_pass_yds:H_adj_pass_att)

# Write to CSV
write.csv(combo_data, "master_combo_data.csv")
