library("rvest")
library("tidyr")
library("dplyr")
library("stringr")

games_df <- {}

# Build URL Part 1
url1 <- "http://www.pro-football-reference.com/years/"
year <- 2006
url2 <- "/week_"

# year loop
while(year <= 2006) {
  
# Build URL Part 2
week <- 1
url3 <- ".htm"
url_string <- c(url1, year, url2, week, url3)

# week loop
while(week < 2) {
  url3 <- ".htm"
  url_string <- c(url1, year, url2, week, url3)

# Build XPath
xpath1 <- '//*[@id="content"]/div[4]/div['
div <- 1
xpath2 <- ']/table[1]'
xpath_string <- c(xpath1, div, xpath2)

# div loop
while(div < 17) {
  xpath2 <- ']/table[1]'
  xpath_string <- c(xpath1, div, xpath2)

#Scrape
url <- str_c(url_string, collapse = "")
games <- url %>%
read_html() %>%
html_nodes(xpath=str_c(xpath_string, collapse = "")) %>%
html_table()
games <- try(games[[1]])

# Organize
game_date <- try(games[1,1])
away_team <- try(games[2,1])
away_team_score <- try(games[2,2])
home_team <- try(games[3,1])
home_team_score <- try(games[3,2])
ot_status <- try(games[3,3])

# Make Data Table
headers <- c("Game Date", "Season Year", "Away Team", "Away Team Score", "Home Team", "Home Team Score", "OT Status", "Week")
row_data <- c(game_date, year, away_team, away_team_score, home_team, home_team_score, ot_status, week)
single_game_df <- data_frame(headers, row_data)
single_game_df <- spread(single_game_df, headers, row_data)

#Add more variables
games_df <- bind_rows(games_df, single_game_df)

div <- div + 1
}

week <- week + 1
}

year <- year + 1
}

# Clean Up
games_df$`Away Team Score` <- as.numeric(games_df$`Away Team Score`)
games_df$`Home Team Score` <- as.numeric(games_df$`Home Team Score`)
games_df <- filter(games_df, is.na(`Away Team Score`) == FALSE)

write.csv(games_df, "nfl_game_history_7.csv")
