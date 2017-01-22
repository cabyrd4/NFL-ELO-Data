library("rvest")
library("tidyr")
library("dplyr")
library("stringr")

# Data Import
data <- read.csv("master_combo_data.csv")
team_code <- read.csv("team_name_ref.csv")

team_list_short <- unique(team_code$team_name_short)

# Parameters
year <- 2016

# Home Offense
home_team_yearly_offense <- data %>%
  select(Season.Year:H_adj_pass_att) %>%
  select(-Away.Team.Code, -A_adj_pass_yds, -A_adj_pass_yds) %>%
  group_by(Home.Team.Code, Season.Year) %>%
  summarise(yds_sum = sum(H_adj_pass_yds), att_sum = sum(H_adj_pass_att)) %>%
  filter(Season.Year == year) %>%
  ungroup()
  
names(home_team_yearly_offense)[1] <- "Team.Code"

# Away Offense
away_team_yearly_offense <- data %>%
  select(Season.Year:H_adj_pass_att) %>%
  select(-Home.Team.Code, -H_adj_pass_yds, -H_adj_pass_yds) %>%
  group_by(Away.Team.Code, Season.Year) %>%
  summarise(yds_sum = sum(A_adj_pass_yds), att_sum = sum(A_adj_pass_att)) %>%
  filter(Season.Year == year) %>%
  ungroup()

names(away_team_yearly_offense)[1] <- "Team.Code"

# Combine Offense
team_yearly_offense <- bind_rows(home_team_yearly_offense, away_team_yearly_offense) %>%
  group_by(Team.Code, Season.Year) %>%
  summarise(yds_sum = sum(yds_sum), att_sum = sum(att_sum))

# Home Defense
home_team_yearly_defense <- data %>%
  select(Season.Year:H_adj_pass_att) %>%
  select(-Away.Team.Code, -H_adj_pass_yds, -H_adj_pass_yds) %>%
  group_by(Home.Team.Code, Season.Year) %>%
  summarise(yds_sum_def = sum(A_adj_pass_yds), att_sum_def = sum(A_adj_pass_att)) %>%
  filter(Season.Year == year) %>%
  ungroup()

names(home_team_yearly_defense)[1] <- "Team.Code"

# Away Defense
away_team_yearly_defense <- data %>%
  select(Season.Year:H_adj_pass_att) %>%
  select(-Home.Team.Code, -A_adj_pass_yds, -A_adj_pass_yds) %>%
  group_by(Away.Team.Code, Season.Year) %>%
  summarise(yds_sum_def = sum(H_adj_pass_yds), att_sum_def = sum(H_adj_pass_att)) %>%
  filter(Season.Year == year) %>%
  ungroup()

names(away_team_yearly_defense)[1] <- "Team.Code"

# Combine Defense
team_yearly_defense <- bind_rows(home_team_yearly_defense, away_team_yearly_defense) %>%
  group_by(Team.Code, Season.Year) %>%
  summarise(yds_sum_def = sum(yds_sum_def), att_sum_def = sum(att_sum_def))

# Combine Offense & Defense
team_yearly_stats <- inner_join(team_yearly_offense, team_yearly_defense, by = "Team.Code")
team_yearly_stats <- team_yearly_stats %>%
  select(-Season.Year.y) %>%
  rename(Season.Year = Season.Year.x)

# Add Eff Fields
team_yearly_stats$off_eff <- team_yearly_stats$yds_sum / team_yearly_stats$att_sum
team_yearly_stats$def_eff <- team_yearly_stats$yds_sum_def / team_yearly_stats$att_sum_def
team_yearly_stats$eff_var <- team_yearly_stats$off_eff - team_yearly_stats$def_eff
