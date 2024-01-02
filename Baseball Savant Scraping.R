# ------------------------------------------------------------------------------
# Baseball Savant Scraping - 2016 to present
# ------------------------------------------------------------------------------

# Load necessary libraries
library(baseballr)
library(tidyverse)
library(dplyr)
library(readr)


# ------------------------------------------------------------------------------
#   Creates necessary functions to complete data scraping & analysis process. Each function's purpose is noted below.
# ------------------------------------------------------------------------------
dates <- function(season) {
  dates_string <- seq.Date(as.Date(paste0(season, '-03-01')),                   # Creates string of start dates for each week of season
                           as.Date(paste0(season, '-10-31')), by = 'week')
  
  date_table <- data.frame(start_date = dates_string,                           # Takes string of dates & puts them into a table denoting weekly blocks of the season
                           end_date = dates_string+6)
  return(date_table)}                                                 # Creates table of dates to pass through StatCast Scraping Function - start date and end date of scraping period produced
scraper_function_batter <- scrape_statcast_savant_batter_all                    # Assigns StatCast batter data scraping function from baseballr package to new function name
scraper_function_pitcher <- scrape_statcast_savant_pitcher_all                  # Assigns StatCast pitcher data scraping function from baseballr package to new function name. Sole purpose is to gather player ID for pitchers. Can also serve as a check for scraped data as it should match the batter data scraped for the same period
format_data <- function(df) {
  # function for appending new variables to the data set
  additional_info <- function(df) {
    # apply additional coding for custom variables
    df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
                                   ifelse(type == "X" & events == "double", 2,
                                          ifelse(type == "X" & events == "triple", 3, 
                                                 ifelse(type == "X" & events == "home_run", 4, 
                                                        ifelse(type == "X" & events == "sac_fly", 5, NA))))))
    df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
                              ifelse(type == "X" & events == "double", 1,
                                     ifelse(type == "X" & events == "triple", 1, 
                                            ifelse(type == "X" & events == "home_run", 1, NA)))))
    df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
    df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
    df$on_1b[is.na(df$on_1b)] <- 0
    df$on_2b[is.na(df$on_2b)] <- 0
    df$on_3b[is.na(df$on_3b)] <- 0
    df <- df %>% 
      mutate(neutral_dates =                                                    # Manually add in games with game_date & home_team variables for games played at neutral sites during the season
               ifelse(game_date == "2017-08-29" & home_team == "HOU", 1,
                      ifelse(game_date == "2017-08-30" & home_team == "HOU", 1,
                             ifelse(game_date == "2017-08-31" & home_team == "HOU", 1,
                                    ifelse(game_date == "2017-09-11" & home_team == "TB", 1,
                                           ifelse(game_date == "2017-09-12" & home_team == "TB", 1, 
                                                  ifelse(game_date == "2017-09-13" & home_team == "TB", 1, 
                                                         ifelse(game_date == "2018-04-17" & home_team == "MIN", 1, 
                                                                ifelse(game_date == "2018-04-18" & home_team == "MIN", 1, 
                                                                       ifelse(game_date == "2018-05-04" & home_team == "SD", 1, 
                                                                              ifelse(game_date == "2018-05-05" & home_team == "SD", 1,
                                                                                     ifelse(game_date == "2018-05-06" & home_team == "SD", 1,
                                                                                            ifelse(game_date == "2018-05-04" & home_team == "SD", 1,
                                                                                                   ifelse(game_date == "2019-04-13" & home_team == "CIN", 1,
                                                                                                          ifelse(game_date == "2019-04-14" & home_team == "CIN", 1,
                                                                                                                 ifelse(game_date == "2019-05-04" & home_team == "LAA", 1,
                                                                                                                        ifelse(game_date == "2019-05-05" & home_team == "LAA", 1,
                                                                                                                               ifelse(game_date == "2019-06-13" & home_team == "KC", 1, 
                                                                                                                                      ifelse(game_date == "2021-08-12" & home_team == "CWS", 1, 
                                                                                                                                             ifelse(game_date == "2021-08-22" & home_team == "CLE", 1,
                                                                                                                                                    ifelse(game_date == "2022-08-11" & home_team == "CIN", 1,
                                                                                                                                                           ifelse(game_date == "2022-08-21" & home_team == "BAL", 1,
                                                                                                                                                                  ifelse(game_date == "2023-08-20" & away_team == "PHI", 1,0))))))))))))))))))))))) %>%
      mutate(runner_on_1b = ifelse(on_1b != 0, 1, 0)) %>%
      mutate(runner_on_2b = ifelse(on_2b != 0, 1, 0)) %>%
      mutate(runner_on_3b = ifelse(on_3b != 0, 1, 0)) %>%
      mutate(runners_on_base = runner_on_1b + runner_on_2b + runner_on_3b) %>%
      mutate(runs_scored_on_play =
               ifelse(hit_type == 5, str_count(des, "scores"),
                      ifelse(hit_type == 3, str_count(des, "scores"),
                             ifelse(hit_type == 2, str_count(des, "scores"),
                                    ifelse(hit_type == 1, str_count(des, "scores"), 
                                           ifelse(hit_type == 4, runners_on_base+1, NA)))))) %>%
      mutate(runs_Road = ifelse(inning_topbot == "Top" & runs_scored_on_play > 0, runs_scored_on_play, 0)) %>%
      mutate(runs_Home = ifelse(inning_topbot == "Bot" & runs_scored_on_play > 0, runs_scored_on_play, 0))
    
    df <- df %>%
      mutate(spray_angle = round((atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75),1))

    return(df)
  }
  df$game_date <- as.character(df$game_date)
  df <- df %>%
    arrange(game_date)
  df <- df %>%
    filter(!is.na(game_date))
  df <- df %>%
    ungroup()
  df <- df %>%
    additional_info() %>%
  return(df)
}                                               # Adds additional variables to data set for additional abilities to slice data during analysis. Base function credited to Bill Petti. Modifications were made to include the ability filter out neutral site games (input manually) during a season.


# ------------------------------------------------------------------------------
#   Scraping 2016 Data
# ------------------------------------------------------------------------------

date_table <- dates(2016)             # Creates date table for 2016 season

# Section is the functional scraping of StatCast data by week - prints message to denote the week being scraped

message(paste0("Scraping Statcast Data for ", date_table$start_date[[1]], " to ", date_table$end_date[[1]]))
march_w1b <- scraper_function_batter(date_table$start_date[[1]], date_table$end_date[[1]])
march_w1p <- scraper_function_pitcher(date_table$start_date[[1]], date_table$end_date[[1]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[2]], " to ", date_table$end_date[[2]]))
march_w2b <- scraper_function_batter(date_table$start_date[[2]], date_table$end_date[[2]])
march_w2p <- scraper_function_pitcher(date_table$start_date[[2]], date_table$end_date[[2]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[3]], " to ", date_table$end_date[[3]]))
march_w3b <- scraper_function_batter(date_table$start_date[[3]], date_table$end_date[[3]])
march_w3p <- scraper_function_pitcher(date_table$start_date[[3]], date_table$end_date[[3]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[4]], " to ", date_table$end_date[[4]]))
march_w4b <- scraper_function_batter(date_table$start_date[[4]], date_table$end_date[[4]])
march_w4p <- scraper_function_pitcher(date_table$start_date[[4]], date_table$end_date[[4]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[5]], " to ", date_table$end_date[[5]]))
march_w5b <- scraper_function_batter(date_table$start_date[[5]], date_table$end_date[[5]])
march_w5p <- scraper_function_pitcher(date_table$start_date[[5]], date_table$end_date[[5]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[6]], " to ", date_table$end_date[[6]]))
april_w1b <- scraper_function_batter(date_table$start_date[[6]], date_table$end_date[[6]])
april_w1p <- scraper_function_pitcher(date_table$start_date[[6]], date_table$end_date[[6]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[7]], " to ", date_table$end_date[[7]]))
april_w2b <- scraper_function_batter(date_table$start_date[[7]], date_table$end_date[[7]])
april_w2p <- scraper_function_pitcher(date_table$start_date[[7]], date_table$end_date[[7]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[8]], " to ", date_table$end_date[[8]]))
april_w3b <- scraper_function_batter(date_table$start_date[[8]], date_table$end_date[[8]])
april_w3p <- scraper_function_pitcher(date_table$start_date[[8]], date_table$end_date[[8]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[9]], " to ", date_table$end_date[[9]]))
april_w4b <- scraper_function_batter(date_table$start_date[[9]], date_table$end_date[[9]])
april_w4p <- scraper_function_pitcher(date_table$start_date[[9]], date_table$end_date[[9]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[10]], " to ", date_table$end_date[[10]]))
may_w1b <- scraper_function_batter(date_table$start_date[[10]], date_table$end_date[[10]])
may_w1p <- scraper_function_pitcher(date_table$start_date[[10]], date_table$end_date[[10]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[11]], " to ", date_table$end_date[[11]]))
may_w2b <- scraper_function_batter(date_table$start_date[[11]], date_table$end_date[[11]])
may_w2p <- scraper_function_pitcher(date_table$start_date[[11]], date_table$end_date[[11]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[12]], " to ", date_table$end_date[[12]]))
may_w3b <- scraper_function_batter(date_table$start_date[[12]], date_table$end_date[[12]])
may_w3p <- scraper_function_pitcher(date_table$start_date[[12]], date_table$end_date[[12]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[13]], " to ", date_table$end_date[[13]]))
may_w4b <- scraper_function_batter(date_table$start_date[[13]], date_table$end_date[[13]])
may_w4p <- scraper_function_pitcher(date_table$start_date[[13]], date_table$end_date[[13]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[14]], " to ", date_table$end_date[[14]]))
june_w1b <- scraper_function_batter(date_table$start_date[[14]], date_table$end_date[[14]])
june_w1p <- scraper_function_pitcher(date_table$start_date[[14]], date_table$end_date[[14]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[15]], " to ", date_table$end_date[[15]]))
june_w2b <- scraper_function_batter(date_table$start_date[[15]], date_table$end_date[[15]])
june_w2p <- scraper_function_pitcher(date_table$start_date[[15]], date_table$end_date[[15]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[16]], " to ", date_table$end_date[[16]]))
june_w3b <- scraper_function_batter(date_table$start_date[[16]], date_table$end_date[[16]])
june_w3p <- scraper_function_pitcher(date_table$start_date[[16]], date_table$end_date[[16]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[17]], " to ", date_table$end_date[[17]]))
june_w4b <- scraper_function_batter(date_table$start_date[[17]], date_table$end_date[[17]])
june_w4p <- scraper_function_pitcher(date_table$start_date[[17]], date_table$end_date[[17]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[18]], " to ", date_table$end_date[[18]]))
june_w5b <- scraper_function_batter(date_table$start_date[[18]], date_table$end_date[[18]])
june_w5p <- scraper_function_pitcher(date_table$start_date[[18]], date_table$end_date[[18]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[19]], " to ", date_table$end_date[[19]]))
july_w1b <- scraper_function_batter(date_table$start_date[[19]], date_table$end_date[[19]])
july_w1p <- scraper_function_pitcher(date_table$start_date[[19]], date_table$end_date[[19]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[20]], " to ", date_table$end_date[[20]]))
july_w2b <- scraper_function_batter(date_table$start_date[[20]], date_table$end_date[[20]])
july_w2p <- scraper_function_pitcher(date_table$start_date[[20]], date_table$end_date[[20]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[21]], " to ", date_table$end_date[[21]]))
july_w3b <- scraper_function_batter(date_table$start_date[[21]], date_table$end_date[[21]])
july_w3p <- scraper_function_pitcher(date_table$start_date[[21]], date_table$end_date[[21]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[22]], " to ", date_table$end_date[[22]]))
july_w4b <- scraper_function_batter(date_table$start_date[[22]], date_table$end_date[[22]])
july_w4p <- scraper_function_pitcher(date_table$start_date[[22]], date_table$end_date[[22]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[23]], " to ", date_table$end_date[[23]]))
august_w1b <- scraper_function_batter(date_table$start_date[[23]], date_table$end_date[[23]])
august_w1p <- scraper_function_pitcher(date_table$start_date[[23]], date_table$end_date[[23]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[24]], " to ", date_table$end_date[[24]]))
august_w2b <- scraper_function_batter(date_table$start_date[[24]], date_table$end_date[[24]])
august_w2p <- scraper_function_pitcher(date_table$start_date[[24]], date_table$end_date[[24]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[25]], " to ", date_table$end_date[[25]]))
august_w3b <- scraper_function_batter(date_table$start_date[[25]], date_table$end_date[[25]])
august_w3p <- scraper_function_pitcher(date_table$start_date[[25]], date_table$end_date[[25]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[26]], " to ", date_table$end_date[[26]]))
august_w4b <- scraper_function_batter(date_table$start_date[[26]], date_table$end_date[[26]])
august_w4p <- scraper_function_pitcher(date_table$start_date[[26]], date_table$end_date[[26]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[27]], " to ", date_table$end_date[[27]]))
august_w5b <- scraper_function_batter(date_table$start_date[[27]], date_table$end_date[[27]])
august_w5p <- scraper_function_pitcher(date_table$start_date[[27]], date_table$end_date[[27]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[28]], " to ", date_table$end_date[[28]]))
september_w1b <- scraper_function_batter(date_table$start_date[[28]], date_table$end_date[[28]])
september_w1p <- scraper_function_pitcher(date_table$start_date[[28]], date_table$end_date[[28]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[29]], " to ", date_table$end_date[[29]]))
september_w2b <- scraper_function_batter(date_table$start_date[[29]], date_table$end_date[[29]])
september_w2p <- scraper_function_pitcher(date_table$start_date[[29]], date_table$end_date[[29]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[30]], " to ", date_table$end_date[[30]]))
september_w3b <- scraper_function_batter(date_table$start_date[[30]], date_table$end_date[[30]])
september_w3p <- scraper_function_pitcher(date_table$start_date[[30]], date_table$end_date[[30]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[31]], " to ", date_table$end_date[[31]]))
september_w4b <- scraper_function_batter(date_table$start_date[[31]], date_table$end_date[[31]])
september_w4p <- scraper_function_pitcher(date_table$start_date[[31]], date_table$end_date[[31]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[32]], " to ", date_table$end_date[[32]]))
october_w1b <- scraper_function_batter(date_table$start_date[[32]], date_table$end_date[[32]])
october_w1p <- scraper_function_pitcher(date_table$start_date[[32]], date_table$end_date[[32]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[33]], " to ", date_table$end_date[[33]]))
october_w2b <- scraper_function_batter(date_table$start_date[[33]], date_table$end_date[[33]])
october_w2p <- scraper_function_pitcher(date_table$start_date[[33]], date_table$end_date[[33]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[34]], " to ", date_table$end_date[[34]]))
october_w3b <- scraper_function_batter(date_table$start_date[[34]], date_table$end_date[[34]])
october_w3p <- scraper_function_pitcher(date_table$start_date[[34]], date_table$end_date[[34]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[35]], " to ", date_table$end_date[[35]]))
october_w4b <- scraper_function_batter(date_table$start_date[[35]], date_table$end_date[[35]])
october_w4p <- scraper_function_pitcher(date_table$start_date[[35]], date_table$end_date[[35]])


# Binds all weeks of data pulled into single dataset for year
data2016b <- rbind(march_w1b,  march_w2b,  march_w3b,  march_w4b,  march_w5b,  april_w1b,  april_w2b,  april_w3b,  april_w4b, may_w1b, may_w2b, may_w3b, may_w4b, 
                  june_w1b, june_w2b, june_w3b, june_w4b, june_w5b, july_w1b, july_w2b, july_w3b, july_w4b, august_w1b, august_w2b, august_w3b, august_w4b, august_w5b, 
                  september_w1b, september_w2b, september_w3b, september_w4b, october_w1b, october_w2b, october_w3b, october_w4b)
data2016p <- rbind(march_w1p,  march_w2p,  march_w3p,  march_w4p,  march_w5p,  april_w1p,  april_w2p,  april_w3p,  april_w4p, may_w1p, may_w2p, may_w3p, may_w4p, 
                   june_w1p, june_w2p, june_w3p, june_w4p, june_w5p, july_w1p, july_w2p, july_w3p, july_w4p, august_w1p, august_w2p, august_w3p, august_w4p, august_w5p, 
                   september_w1p, september_w2p, september_w3p, september_w4p, october_w1p, october_w2p, october_w3p, october_w4p)

# Creates data frames for player ID numbers for all players who appeared in the season
batter_ids <- data2016b %>% 
  select(player_name, batter) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, batter)
batter_ids$name <- paste(batter_ids$first_name, batter_ids$last_name) 
colnames(batter_ids)[3] <- "player_id"
batter_ids <- batter_ids %>% select(name, player_id)


pitcher_ids <- data2016p %>% 
  select(player_name, pitcher) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, pitcher)
pitcher_ids$name <- paste(pitcher_ids$first_name, pitcher_ids$last_name) 
colnames(pitcher_ids)[3] <- "player_id"
pitcher_ids <- pitcher_ids %>% select(name, player_id)

all_player_ids <- rbind(batter_ids, pitcher_ids)
all_player_ids <- all_player_ids %>%
  unique()
data2016 <- data2016b

# Adds batter and pitcher names to data frame
data2016 <- data2016 %>%
  left_join(all_player_ids, by = c('batter' = 'player_id')) %>%
  left_join(all_player_ids, by = c('pitcher' = 'player_id'))

colnames(data2016)[93] <- "batter_name"
colnames(data2016)[94] <- "pitcher_name"

# Applies format_data() function to dataset to add additional variables
data2016 <- format_data(data2016)


# Move data into CSV file for permanent data storage
setwd('ENTER FILE PATH')
write_csv(data2016, "Statcast Scrape 2016")



# ------------------------------------------------------------------------------
#   Scraping 2017 Data
# ------------------------------------------------------------------------------

date_table <- dates(2017)             # Creates date table for 2017 season

# Section is the functional scraping of StatCast data by week - prints message to denote the week being scraped

message(paste0("Scraping Statcast Data for ", date_table$start_date[[1]], " to ", date_table$end_date[[1]]))
march_w1b <- scraper_function_batter(date_table$start_date[[1]], date_table$end_date[[1]])
march_w1p <- scraper_function_pitcher(date_table$start_date[[1]], date_table$end_date[[1]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[2]], " to ", date_table$end_date[[2]]))
march_w2b <- scraper_function_batter(date_table$start_date[[2]], date_table$end_date[[2]])
march_w2p <- scraper_function_pitcher(date_table$start_date[[2]], date_table$end_date[[2]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[3]], " to ", date_table$end_date[[3]]))
march_w3b <- scraper_function_batter(date_table$start_date[[3]], date_table$end_date[[3]])
march_w3p <- scraper_function_pitcher(date_table$start_date[[3]], date_table$end_date[[3]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[4]], " to ", date_table$end_date[[4]]))
march_w4b <- scraper_function_batter(date_table$start_date[[4]], date_table$end_date[[4]])
march_w4p <- scraper_function_pitcher(date_table$start_date[[4]], date_table$end_date[[4]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[5]], " to ", date_table$end_date[[5]]))
march_w5b <- scraper_function_batter(date_table$start_date[[5]], date_table$end_date[[5]])
march_w5p <- scraper_function_pitcher(date_table$start_date[[5]], date_table$end_date[[5]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[6]], " to ", date_table$end_date[[6]]))
april_w1b <- scraper_function_batter(date_table$start_date[[6]], date_table$end_date[[6]])
april_w1p <- scraper_function_pitcher(date_table$start_date[[6]], date_table$end_date[[6]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[7]], " to ", date_table$end_date[[7]]))
april_w2b <- scraper_function_batter(date_table$start_date[[7]], date_table$end_date[[7]])
april_w2p <- scraper_function_pitcher(date_table$start_date[[7]], date_table$end_date[[7]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[8]], " to ", date_table$end_date[[8]]))
april_w3b <- scraper_function_batter(date_table$start_date[[8]], date_table$end_date[[8]])
april_w3p <- scraper_function_pitcher(date_table$start_date[[8]], date_table$end_date[[8]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[9]], " to ", date_table$end_date[[9]]))
april_w4b <- scraper_function_batter(date_table$start_date[[9]], date_table$end_date[[9]])
april_w4p <- scraper_function_pitcher(date_table$start_date[[9]], date_table$end_date[[9]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[10]], " to ", date_table$end_date[[10]]))
may_w1b <- scraper_function_batter(date_table$start_date[[10]], date_table$end_date[[10]])
may_w1p <- scraper_function_pitcher(date_table$start_date[[10]], date_table$end_date[[10]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[11]], " to ", date_table$end_date[[11]]))
may_w2b <- scraper_function_batter(date_table$start_date[[11]], date_table$end_date[[11]])
may_w2p <- scraper_function_pitcher(date_table$start_date[[11]], date_table$end_date[[11]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[12]], " to ", date_table$end_date[[12]]))
may_w3b <- scraper_function_batter(date_table$start_date[[12]], date_table$end_date[[12]])
may_w3p <- scraper_function_pitcher(date_table$start_date[[12]], date_table$end_date[[12]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[13]], " to ", date_table$end_date[[13]]))
may_w4b <- scraper_function_batter(date_table$start_date[[13]], date_table$end_date[[13]])
may_w4p <- scraper_function_pitcher(date_table$start_date[[13]], date_table$end_date[[13]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[14]], " to ", date_table$end_date[[14]]))
june_w1b <- scraper_function_batter(date_table$start_date[[14]], date_table$end_date[[14]])
june_w1p <- scraper_function_pitcher(date_table$start_date[[14]], date_table$end_date[[14]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[15]], " to ", date_table$end_date[[15]]))
june_w2b <- scraper_function_batter(date_table$start_date[[15]], date_table$end_date[[15]])
june_w2p <- scraper_function_pitcher(date_table$start_date[[15]], date_table$end_date[[15]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[16]], " to ", date_table$end_date[[16]]))
june_w3b <- scraper_function_batter(date_table$start_date[[16]], date_table$end_date[[16]])
june_w3p <- scraper_function_pitcher(date_table$start_date[[16]], date_table$end_date[[16]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[17]], " to ", date_table$end_date[[17]]))
june_w4b <- scraper_function_batter(date_table$start_date[[17]], date_table$end_date[[17]])
june_w4p <- scraper_function_pitcher(date_table$start_date[[17]], date_table$end_date[[17]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[18]], " to ", date_table$end_date[[18]]))
june_w5b <- scraper_function_batter(date_table$start_date[[18]], date_table$end_date[[18]])
june_w5p <- scraper_function_pitcher(date_table$start_date[[18]], date_table$end_date[[18]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[19]], " to ", date_table$end_date[[19]]))
july_w1b <- scraper_function_batter(date_table$start_date[[19]], date_table$end_date[[19]])
july_w1p <- scraper_function_pitcher(date_table$start_date[[19]], date_table$end_date[[19]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[20]], " to ", date_table$end_date[[20]]))
july_w2b <- scraper_function_batter(date_table$start_date[[20]], date_table$end_date[[20]])
july_w2p <- scraper_function_pitcher(date_table$start_date[[20]], date_table$end_date[[20]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[21]], " to ", date_table$end_date[[21]]))
july_w3b <- scraper_function_batter(date_table$start_date[[21]], date_table$end_date[[21]])
july_w3p <- scraper_function_pitcher(date_table$start_date[[21]], date_table$end_date[[21]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[22]], " to ", date_table$end_date[[22]]))
july_w4b <- scraper_function_batter(date_table$start_date[[22]], date_table$end_date[[22]])
july_w4p <- scraper_function_pitcher(date_table$start_date[[22]], date_table$end_date[[22]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[23]], " to ", date_table$end_date[[23]]))
august_w1b <- scraper_function_batter(date_table$start_date[[23]], date_table$end_date[[23]])
august_w1p <- scraper_function_pitcher(date_table$start_date[[23]], date_table$end_date[[23]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[24]], " to ", date_table$end_date[[24]]))
august_w2b <- scraper_function_batter(date_table$start_date[[24]], date_table$end_date[[24]])
august_w2p <- scraper_function_pitcher(date_table$start_date[[24]], date_table$end_date[[24]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[25]], " to ", date_table$end_date[[25]]))
august_w3b <- scraper_function_batter(date_table$start_date[[25]], date_table$end_date[[25]])
august_w3p <- scraper_function_pitcher(date_table$start_date[[25]], date_table$end_date[[25]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[26]], " to ", date_table$end_date[[26]]))
august_w4b <- scraper_function_batter(date_table$start_date[[26]], date_table$end_date[[26]])
august_w4p <- scraper_function_pitcher(date_table$start_date[[26]], date_table$end_date[[26]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[27]], " to ", date_table$end_date[[27]]))
august_w5b <- scraper_function_batter(date_table$start_date[[27]], date_table$end_date[[27]])
august_w5p <- scraper_function_pitcher(date_table$start_date[[27]], date_table$end_date[[27]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[28]], " to ", date_table$end_date[[28]]))
september_w1b <- scraper_function_batter(date_table$start_date[[28]], date_table$end_date[[28]])
september_w1p <- scraper_function_pitcher(date_table$start_date[[28]], date_table$end_date[[28]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[29]], " to ", date_table$end_date[[29]]))
september_w2b <- scraper_function_batter(date_table$start_date[[29]], date_table$end_date[[29]])
september_w2p <- scraper_function_pitcher(date_table$start_date[[29]], date_table$end_date[[29]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[30]], " to ", date_table$end_date[[30]]))
september_w3b <- scraper_function_batter(date_table$start_date[[30]], date_table$end_date[[30]])
september_w3p <- scraper_function_pitcher(date_table$start_date[[30]], date_table$end_date[[30]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[31]], " to ", date_table$end_date[[31]]))
september_w4b <- scraper_function_batter(date_table$start_date[[31]], date_table$end_date[[31]])
september_w4p <- scraper_function_pitcher(date_table$start_date[[31]], date_table$end_date[[31]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[32]], " to ", date_table$end_date[[32]]))
october_w1b <- scraper_function_batter(date_table$start_date[[32]], date_table$end_date[[32]])
october_w1p <- scraper_function_pitcher(date_table$start_date[[32]], date_table$end_date[[32]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[33]], " to ", date_table$end_date[[33]]))
october_w2b <- scraper_function_batter(date_table$start_date[[33]], date_table$end_date[[33]])
october_w2p <- scraper_function_pitcher(date_table$start_date[[33]], date_table$end_date[[33]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[34]], " to ", date_table$end_date[[34]]))
october_w3b <- scraper_function_batter(date_table$start_date[[34]], date_table$end_date[[34]])
october_w3p <- scraper_function_pitcher(date_table$start_date[[34]], date_table$end_date[[34]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[35]], " to ", date_table$end_date[[35]]))
october_w4b <- scraper_function_batter(date_table$start_date[[35]], date_table$end_date[[35]])
october_w4p <- scraper_function_pitcher(date_table$start_date[[35]], date_table$end_date[[35]])


# Binds all weeks of data pulled into single dataset for year
data2017b <- rbind(march_w1b,  march_w2b,  march_w3b,  march_w4b,  march_w5b,  april_w1b,  april_w2b,  april_w3b,  april_w4b, may_w1b, may_w2b, may_w3b, may_w4b, 
                   june_w1b, june_w2b, june_w3b, june_w4b, june_w5b, july_w1b, july_w2b, july_w3b, july_w4b, august_w1b, august_w2b, august_w3b, august_w4b, august_w5b, 
                   september_w1b, september_w2b, september_w3b, september_w4b, october_w1b, october_w2b, october_w3b, october_w4b)
data2017p <- rbind(march_w1p,  march_w2p,  march_w3p,  march_w4p,  march_w5p,  april_w1p,  april_w2p,  april_w3p,  april_w4p, may_w1p, may_w2p, may_w3p, may_w4p, 
                   june_w1p, june_w2p, june_w3p, june_w4p, june_w5p, july_w1p, july_w2p, july_w3p, july_w4p, august_w1p, august_w2p, august_w3p, august_w4p, august_w5p, 
                   september_w1p, september_w2p, september_w3p, september_w4p, october_w1p, october_w2p, october_w3p, october_w4p)

# Creates data frames for player ID numbers for all players who appeared in the season
batter_ids <- data2017b %>% 
  select(player_name, batter) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, batter)
batter_ids$name <- paste(batter_ids$first_name, batter_ids$last_name) 
colnames(batter_ids)[3] <- "player_id"
batter_ids <- batter_ids %>% select(name, player_id)


pitcher_ids <- data2017p %>% 
  select(player_name, pitcher) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, pitcher)
pitcher_ids$name <- paste(pitcher_ids$first_name, pitcher_ids$last_name) 
colnames(pitcher_ids)[3] <- "player_id"
pitcher_ids <- pitcher_ids %>% select(name, player_id)

all_player_ids <- rbind(batter_ids, pitcher_ids)
all_player_ids <- all_player_ids %>%
  unique()
data2017 <- data2017b

# Adds batter and pitcher names to data frame
data2017 <- data2017 %>%
  left_join(all_player_ids, by = c('batter' = 'player_id')) %>%
  left_join(all_player_ids, by = c('pitcher' = 'player_id'))

colnames(data2017)[93] <- "batter_name"
colnames(data2017)[94] <- "pitcher_name"

# Applies format_data() function to dataset to add additional variables
data2017 <- format_data(data2017)

# Move data into CSV file for permanent data storage
setwd('ENTER FILE PATH')
write_csv(data2017, "Statcast Scrape 2017")

# Update Player ID Database
player_id_database <- read_csv("Player ID Database")

colnames(player_id_database)[1] <- "name"
colnames(player_id_database)[2] <- "player_id"

player_id_database <- rbind(player_id_database, all_player_ids)
player_id_database <- player_id_database %>%
  rbind(all_player_ids) %>%
  unique() %>%
  write_csv("Player ID Database")

# ------------------------------------------------------------------------------
#   Scraping 2018 Data
# ------------------------------------------------------------------------------

date_table <- dates(2018)             # Creates date table for 2018 season

# Section is the functional scraping of StatCast data by week - prints message to denote the week being scraped

message(paste0("Scraping Statcast Data for ", date_table$start_date[[1]], " to ", date_table$end_date[[1]]))
march_w1b <- scraper_function_batter(date_table$start_date[[1]], date_table$end_date[[1]])
march_w1p <- scraper_function_pitcher(date_table$start_date[[1]], date_table$end_date[[1]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[2]], " to ", date_table$end_date[[2]]))
march_w2b <- scraper_function_batter(date_table$start_date[[2]], date_table$end_date[[2]])
march_w2p <- scraper_function_pitcher(date_table$start_date[[2]], date_table$end_date[[2]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[3]], " to ", date_table$end_date[[3]]))
march_w3b <- scraper_function_batter(date_table$start_date[[3]], date_table$end_date[[3]])
march_w3p <- scraper_function_pitcher(date_table$start_date[[3]], date_table$end_date[[3]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[4]], " to ", date_table$end_date[[4]]))
march_w4b <- scraper_function_batter(date_table$start_date[[4]], date_table$end_date[[4]])
march_w4p <- scraper_function_pitcher(date_table$start_date[[4]], date_table$end_date[[4]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[5]], " to ", date_table$end_date[[5]]))
march_w5b <- scraper_function_batter(date_table$start_date[[5]], date_table$end_date[[5]])
march_w5p <- scraper_function_pitcher(date_table$start_date[[5]], date_table$end_date[[5]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[6]], " to ", date_table$end_date[[6]]))
april_w1b <- scraper_function_batter(date_table$start_date[[6]], date_table$end_date[[6]])
april_w1p <- scraper_function_pitcher(date_table$start_date[[6]], date_table$end_date[[6]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[7]], " to ", date_table$end_date[[7]]))
april_w2b <- scraper_function_batter(date_table$start_date[[7]], date_table$end_date[[7]])
april_w2p <- scraper_function_pitcher(date_table$start_date[[7]], date_table$end_date[[7]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[8]], " to ", date_table$end_date[[8]]))
april_w3b <- scraper_function_batter(date_table$start_date[[8]], date_table$end_date[[8]])
april_w3p <- scraper_function_pitcher(date_table$start_date[[8]], date_table$end_date[[8]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[9]], " to ", date_table$end_date[[9]]))
april_w4b <- scraper_function_batter(date_table$start_date[[9]], date_table$end_date[[9]])
april_w4p <- scraper_function_pitcher(date_table$start_date[[9]], date_table$end_date[[9]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[10]], " to ", date_table$end_date[[10]]))
may_w1b <- scraper_function_batter(date_table$start_date[[10]], date_table$end_date[[10]])
may_w1p <- scraper_function_pitcher(date_table$start_date[[10]], date_table$end_date[[10]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[11]], " to ", date_table$end_date[[11]]))
may_w2b <- scraper_function_batter(date_table$start_date[[11]], date_table$end_date[[11]])
may_w2p <- scraper_function_pitcher(date_table$start_date[[11]], date_table$end_date[[11]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[12]], " to ", date_table$end_date[[12]]))
may_w3b <- scraper_function_batter(date_table$start_date[[12]], date_table$end_date[[12]])
may_w3p <- scraper_function_pitcher(date_table$start_date[[12]], date_table$end_date[[12]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[13]], " to ", date_table$end_date[[13]]))
may_w4b <- scraper_function_batter(date_table$start_date[[13]], date_table$end_date[[13]])
may_w4p <- scraper_function_pitcher(date_table$start_date[[13]], date_table$end_date[[13]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[14]], " to ", date_table$end_date[[14]]))
june_w1b <- scraper_function_batter(date_table$start_date[[14]], date_table$end_date[[14]])
june_w1p <- scraper_function_pitcher(date_table$start_date[[14]], date_table$end_date[[14]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[15]], " to ", date_table$end_date[[15]]))
june_w2b <- scraper_function_batter(date_table$start_date[[15]], date_table$end_date[[15]])
june_w2p <- scraper_function_pitcher(date_table$start_date[[15]], date_table$end_date[[15]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[16]], " to ", date_table$end_date[[16]]))
june_w3b <- scraper_function_batter(date_table$start_date[[16]], date_table$end_date[[16]])
june_w3p <- scraper_function_pitcher(date_table$start_date[[16]], date_table$end_date[[16]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[17]], " to ", date_table$end_date[[17]]))
june_w4b <- scraper_function_batter(date_table$start_date[[17]], date_table$end_date[[17]])
june_w4p <- scraper_function_pitcher(date_table$start_date[[17]], date_table$end_date[[17]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[18]], " to ", date_table$end_date[[18]]))
june_w5b <- scraper_function_batter(date_table$start_date[[18]], date_table$end_date[[18]])
june_w5p <- scraper_function_pitcher(date_table$start_date[[18]], date_table$end_date[[18]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[19]], " to ", date_table$end_date[[19]]))
july_w1b <- scraper_function_batter(date_table$start_date[[19]], date_table$end_date[[19]])
july_w1p <- scraper_function_pitcher(date_table$start_date[[19]], date_table$end_date[[19]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[20]], " to ", date_table$end_date[[20]]))
july_w2b <- scraper_function_batter(date_table$start_date[[20]], date_table$end_date[[20]])
july_w2p <- scraper_function_pitcher(date_table$start_date[[20]], date_table$end_date[[20]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[21]], " to ", date_table$end_date[[21]]))
july_w3b <- scraper_function_batter(date_table$start_date[[21]], date_table$end_date[[21]])
july_w3p <- scraper_function_pitcher(date_table$start_date[[21]], date_table$end_date[[21]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[22]], " to ", date_table$end_date[[22]]))
july_w4b <- scraper_function_batter(date_table$start_date[[22]], date_table$end_date[[22]])
july_w4p <- scraper_function_pitcher(date_table$start_date[[22]], date_table$end_date[[22]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[23]], " to ", date_table$end_date[[23]]))
august_w1b <- scraper_function_batter(date_table$start_date[[23]], date_table$end_date[[23]])
august_w1p <- scraper_function_pitcher(date_table$start_date[[23]], date_table$end_date[[23]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[24]], " to ", date_table$end_date[[24]]))
august_w2b <- scraper_function_batter(date_table$start_date[[24]], date_table$end_date[[24]])
august_w2p <- scraper_function_pitcher(date_table$start_date[[24]], date_table$end_date[[24]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[25]], " to ", date_table$end_date[[25]]))
august_w3b <- scraper_function_batter(date_table$start_date[[25]], date_table$end_date[[25]])
august_w3p <- scraper_function_pitcher(date_table$start_date[[25]], date_table$end_date[[25]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[26]], " to ", date_table$end_date[[26]]))
august_w4b <- scraper_function_batter(date_table$start_date[[26]], date_table$end_date[[26]])
august_w4p <- scraper_function_pitcher(date_table$start_date[[26]], date_table$end_date[[26]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[27]], " to ", date_table$end_date[[27]]))
august_w5b <- scraper_function_batter(date_table$start_date[[27]], date_table$end_date[[27]])
august_w5p <- scraper_function_pitcher(date_table$start_date[[27]], date_table$end_date[[27]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[28]], " to ", date_table$end_date[[28]]))
september_w1b <- scraper_function_batter(date_table$start_date[[28]], date_table$end_date[[28]])
september_w1p <- scraper_function_pitcher(date_table$start_date[[28]], date_table$end_date[[28]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[29]], " to ", date_table$end_date[[29]]))
september_w2b <- scraper_function_batter(date_table$start_date[[29]], date_table$end_date[[29]])
september_w2p <- scraper_function_pitcher(date_table$start_date[[29]], date_table$end_date[[29]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[30]], " to ", date_table$end_date[[30]]))
september_w3b <- scraper_function_batter(date_table$start_date[[30]], date_table$end_date[[30]])
september_w3p <- scraper_function_pitcher(date_table$start_date[[30]], date_table$end_date[[30]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[31]], " to ", date_table$end_date[[31]]))
september_w4b <- scraper_function_batter(date_table$start_date[[31]], date_table$end_date[[31]])
september_w4p <- scraper_function_pitcher(date_table$start_date[[31]], date_table$end_date[[31]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[32]], " to ", date_table$end_date[[32]]))
october_w1b <- scraper_function_batter(date_table$start_date[[32]], date_table$end_date[[32]])
october_w1p <- scraper_function_pitcher(date_table$start_date[[32]], date_table$end_date[[32]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[33]], " to ", date_table$end_date[[33]]))
october_w2b <- scraper_function_batter(date_table$start_date[[33]], date_table$end_date[[33]])
october_w2p <- scraper_function_pitcher(date_table$start_date[[33]], date_table$end_date[[33]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[34]], " to ", date_table$end_date[[34]]))
october_w3b <- scraper_function_batter(date_table$start_date[[34]], date_table$end_date[[34]])
october_w3p <- scraper_function_pitcher(date_table$start_date[[34]], date_table$end_date[[34]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[35]], " to ", date_table$end_date[[35]]))
october_w4b <- scraper_function_batter(date_table$start_date[[35]], date_table$end_date[[35]])
october_w4p <- scraper_function_pitcher(date_table$start_date[[35]], date_table$end_date[[35]])


# Binds all weeks of data pulled into single dataset for year
data2018b <- rbind(march_w1b,  march_w2b,  march_w3b,  march_w4b,  march_w5b,  april_w1b,  april_w2b,  april_w3b,  april_w4b, may_w1b, may_w2b, may_w3b, may_w4b, 
                   june_w1b, june_w2b, june_w3b, june_w4b, june_w5b, july_w1b, july_w2b, july_w3b, july_w4b, august_w1b, august_w2b, august_w3b, august_w4b, august_w5b, 
                   september_w1b, september_w2b, september_w3b, september_w4b, october_w1b, october_w2b, october_w3b, october_w4b)
data2018p <- rbind(march_w1p,  march_w2p,  march_w3p,  march_w4p,  march_w5p,  april_w1p,  april_w2p,  april_w3p,  april_w4p, may_w1p, may_w2p, may_w3p, may_w4p, 
                   june_w1p, june_w2p, june_w3p, june_w4p, june_w5p, july_w1p, july_w2p, july_w3p, july_w4p, august_w1p, august_w2p, august_w3p, august_w4p, august_w5p, 
                   september_w1p, september_w2p, september_w3p, september_w4p, october_w1p, october_w2p, october_w3p, october_w4p)

# Creates data frames for player ID numbers for all players who appeared in the season
batter_ids <- data2018b %>% 
  select(player_name, batter) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, batter)
batter_ids$name <- paste(batter_ids$first_name, batter_ids$last_name) 
colnames(batter_ids)[3] <- "player_id"
batter_ids <- batter_ids %>% select(name, player_id)


pitcher_ids <- data2018p %>% 
  select(player_name, pitcher) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, pitcher)
pitcher_ids$name <- paste(pitcher_ids$first_name, pitcher_ids$last_name) 
colnames(pitcher_ids)[3] <- "player_id"
pitcher_ids <- pitcher_ids %>% select(name, player_id)

all_player_ids <- rbind(batter_ids, pitcher_ids)
all_player_ids <- all_player_ids %>%
  unique()
data2018 <- data2018b

# Adds batter and pitcher names to data frame
data2018 <- data2018 %>%
  left_join(all_player_ids, by = c('batter' = 'player_id')) %>%
  left_join(all_player_ids, by = c('pitcher' = 'player_id'))

colnames(data2018)[93] <- "batter_name"
colnames(data2018)[94] <- "pitcher_name"

# Applies format_data() function to dataset to add additional variables
data2018 <- format_data(data2018)

# Move data into CSV file for permanent data storage
setwd('ENTER FILE PATH')
write_csv(data2018, "Statcast Scrape 2018")

# Update Player ID Database
player_id_database <- read_csv("Player ID Database")

player_id_database <- rbind(player_id_database, all_player_ids)
player_id_database <- player_id_database %>%
  rbind(all_player_ids) %>%
  unique() %>%
  write_csv("Player ID Database")

# ------------------------------------------------------------------------------
#   Scraping 2019 Data
# ------------------------------------------------------------------------------

date_table <- dates(2019)             # Creates date table for 2019 season

# Section is the functional scraping of StatCast data by week - prints message to denote the week being scraped

message(paste0("Scraping Statcast Data for ", date_table$start_date[[1]], " to ", date_table$end_date[[1]]))
march_w1b <- scraper_function_batter(date_table$start_date[[1]], date_table$end_date[[1]])
march_w1p <- scraper_function_pitcher(date_table$start_date[[1]], date_table$end_date[[1]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[2]], " to ", date_table$end_date[[2]]))
march_w2b <- scraper_function_batter(date_table$start_date[[2]], date_table$end_date[[2]])
march_w2p <- scraper_function_pitcher(date_table$start_date[[2]], date_table$end_date[[2]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[3]], " to ", date_table$end_date[[3]]))
march_w3b <- scraper_function_batter(date_table$start_date[[3]], date_table$end_date[[3]])
march_w3p <- scraper_function_pitcher(date_table$start_date[[3]], date_table$end_date[[3]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[4]], " to ", date_table$end_date[[4]]))
march_w4b <- scraper_function_batter(date_table$start_date[[4]], date_table$end_date[[4]])
march_w4p <- scraper_function_pitcher(date_table$start_date[[4]], date_table$end_date[[4]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[5]], " to ", date_table$end_date[[5]]))
march_w5b <- scraper_function_batter(date_table$start_date[[5]], date_table$end_date[[5]])
march_w5p <- scraper_function_pitcher(date_table$start_date[[5]], date_table$end_date[[5]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[6]], " to ", date_table$end_date[[6]]))
april_w1b <- scraper_function_batter(date_table$start_date[[6]], date_table$end_date[[6]])
april_w1p <- scraper_function_pitcher(date_table$start_date[[6]], date_table$end_date[[6]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[7]], " to ", date_table$end_date[[7]]))
april_w2b <- scraper_function_batter(date_table$start_date[[7]], date_table$end_date[[7]])
april_w2p <- scraper_function_pitcher(date_table$start_date[[7]], date_table$end_date[[7]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[8]], " to ", date_table$end_date[[8]]))
april_w3b <- scraper_function_batter(date_table$start_date[[8]], date_table$end_date[[8]])
april_w3p <- scraper_function_pitcher(date_table$start_date[[8]], date_table$end_date[[8]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[9]], " to ", date_table$end_date[[9]]))
april_w4b <- scraper_function_batter(date_table$start_date[[9]], date_table$end_date[[9]])
april_w4p <- scraper_function_pitcher(date_table$start_date[[9]], date_table$end_date[[9]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[10]], " to ", date_table$end_date[[10]]))
may_w1b <- scraper_function_batter(date_table$start_date[[10]], date_table$end_date[[10]])
may_w1p <- scraper_function_pitcher(date_table$start_date[[10]], date_table$end_date[[10]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[11]], " to ", date_table$end_date[[11]]))
may_w2b <- scraper_function_batter(date_table$start_date[[11]], date_table$end_date[[11]])
may_w2p <- scraper_function_pitcher(date_table$start_date[[11]], date_table$end_date[[11]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[12]], " to ", date_table$end_date[[12]]))
may_w3b <- scraper_function_batter(date_table$start_date[[12]], date_table$end_date[[12]])
may_w3p <- scraper_function_pitcher(date_table$start_date[[12]], date_table$end_date[[12]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[13]], " to ", date_table$end_date[[13]]))
may_w4b <- scraper_function_batter(date_table$start_date[[13]], date_table$end_date[[13]])
may_w4p <- scraper_function_pitcher(date_table$start_date[[13]], date_table$end_date[[13]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[14]], " to ", date_table$end_date[[14]]))
june_w1b <- scraper_function_batter(date_table$start_date[[14]], date_table$end_date[[14]])
june_w1p <- scraper_function_pitcher(date_table$start_date[[14]], date_table$end_date[[14]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[15]], " to ", date_table$end_date[[15]]))
june_w2b <- scraper_function_batter(date_table$start_date[[15]], date_table$end_date[[15]])
june_w2p <- scraper_function_pitcher(date_table$start_date[[15]], date_table$end_date[[15]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[16]], " to ", date_table$end_date[[16]]))
june_w3b <- scraper_function_batter(date_table$start_date[[16]], date_table$end_date[[16]])
june_w3p <- scraper_function_pitcher(date_table$start_date[[16]], date_table$end_date[[16]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[17]], " to ", date_table$end_date[[17]]))
june_w4b <- scraper_function_batter(date_table$start_date[[17]], date_table$end_date[[17]])
june_w4p <- scraper_function_pitcher(date_table$start_date[[17]], date_table$end_date[[17]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[18]], " to ", date_table$end_date[[18]]))
june_w5b <- scraper_function_batter(date_table$start_date[[18]], date_table$end_date[[18]])
june_w5p <- scraper_function_pitcher(date_table$start_date[[18]], date_table$end_date[[18]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[19]], " to ", date_table$end_date[[19]]))
july_w1b <- scraper_function_batter(date_table$start_date[[19]], date_table$end_date[[19]])
july_w1p <- scraper_function_pitcher(date_table$start_date[[19]], date_table$end_date[[19]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[20]], " to ", date_table$end_date[[20]]))
july_w2b <- scraper_function_batter(date_table$start_date[[20]], date_table$end_date[[20]])
july_w2p <- scraper_function_pitcher(date_table$start_date[[20]], date_table$end_date[[20]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[21]], " to ", date_table$end_date[[21]]))
july_w3b <- scraper_function_batter(date_table$start_date[[21]], date_table$end_date[[21]])
july_w3p <- scraper_function_pitcher(date_table$start_date[[21]], date_table$end_date[[21]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[22]], " to ", date_table$end_date[[22]]))
july_w4b <- scraper_function_batter(date_table$start_date[[22]], date_table$end_date[[22]])
july_w4p <- scraper_function_pitcher(date_table$start_date[[22]], date_table$end_date[[22]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[23]], " to ", date_table$end_date[[23]]))
august_w1b <- scraper_function_batter(date_table$start_date[[23]], date_table$end_date[[23]])
august_w1p <- scraper_function_pitcher(date_table$start_date[[23]], date_table$end_date[[23]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[24]], " to ", date_table$end_date[[24]]))
august_w2b <- scraper_function_batter(date_table$start_date[[24]], date_table$end_date[[24]])
august_w2p <- scraper_function_pitcher(date_table$start_date[[24]], date_table$end_date[[24]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[25]], " to ", date_table$end_date[[25]]))
august_w3b <- scraper_function_batter(date_table$start_date[[25]], date_table$end_date[[25]])
august_w3p <- scraper_function_pitcher(date_table$start_date[[25]], date_table$end_date[[25]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[26]], " to ", date_table$end_date[[26]]))
august_w4b <- scraper_function_batter(date_table$start_date[[26]], date_table$end_date[[26]])
august_w4p <- scraper_function_pitcher(date_table$start_date[[26]], date_table$end_date[[26]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[27]], " to ", date_table$end_date[[27]]))
august_w5b <- scraper_function_batter(date_table$start_date[[27]], date_table$end_date[[27]])
august_w5p <- scraper_function_pitcher(date_table$start_date[[27]], date_table$end_date[[27]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[28]], " to ", date_table$end_date[[28]]))
september_w1b <- scraper_function_batter(date_table$start_date[[28]], date_table$end_date[[28]])
september_w1p <- scraper_function_pitcher(date_table$start_date[[28]], date_table$end_date[[28]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[29]], " to ", date_table$end_date[[29]]))
september_w2b <- scraper_function_batter(date_table$start_date[[29]], date_table$end_date[[29]])
september_w2p <- scraper_function_pitcher(date_table$start_date[[29]], date_table$end_date[[29]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[30]], " to ", date_table$end_date[[30]]))
september_w3b <- scraper_function_batter(date_table$start_date[[30]], date_table$end_date[[30]])
september_w3p <- scraper_function_pitcher(date_table$start_date[[30]], date_table$end_date[[30]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[31]], " to ", date_table$end_date[[31]]))
september_w4b <- scraper_function_batter(date_table$start_date[[31]], date_table$end_date[[31]])
september_w4p <- scraper_function_pitcher(date_table$start_date[[31]], date_table$end_date[[31]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[32]], " to ", date_table$end_date[[32]]))
october_w1b <- scraper_function_batter(date_table$start_date[[32]], date_table$end_date[[32]])
october_w1p <- scraper_function_pitcher(date_table$start_date[[32]], date_table$end_date[[32]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[33]], " to ", date_table$end_date[[33]]))
october_w2b <- scraper_function_batter(date_table$start_date[[33]], date_table$end_date[[33]])
october_w2p <- scraper_function_pitcher(date_table$start_date[[33]], date_table$end_date[[33]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[34]], " to ", date_table$end_date[[34]]))
october_w3b <- scraper_function_batter(date_table$start_date[[34]], date_table$end_date[[34]])
october_w3p <- scraper_function_pitcher(date_table$start_date[[34]], date_table$end_date[[34]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[35]], " to ", date_table$end_date[[35]]))
october_w4b <- scraper_function_batter(date_table$start_date[[35]], date_table$end_date[[35]])
october_w4p <- scraper_function_pitcher(date_table$start_date[[35]], date_table$end_date[[35]])


# Binds all weeks of data pulled into single dataset for year
data2019b <- rbind(march_w1b,  march_w2b,  march_w3b,  march_w4b,  march_w5b,  april_w1b,  april_w2b,  april_w3b,  april_w4b, may_w1b, may_w2b, may_w3b, may_w4b, 
                   june_w1b, june_w2b, june_w3b, june_w4b, june_w5b, july_w1b, july_w2b, july_w3b, july_w4b, august_w1b, august_w2b, august_w3b, august_w4b, august_w5b, 
                   september_w1b, september_w2b, september_w3b, september_w4b, october_w1b, october_w2b, october_w3b, october_w4b)
data2019p <- rbind(march_w1p,  march_w2p,  march_w3p,  march_w4p,  march_w5p,  april_w1p,  april_w2p,  april_w3p,  april_w4p, may_w1p, may_w2p, may_w3p, may_w4p, 
                   june_w1p, june_w2p, june_w3p, june_w4p, june_w5p, july_w1p, july_w2p, july_w3p, july_w4p, august_w1p, august_w2p, august_w3p, august_w4p, august_w5p, 
                   september_w1p, september_w2p, september_w3p, september_w4p, october_w1p, october_w2p, october_w3p, october_w4p)

# Creates data frames for player ID numbers for all players who appeared in the season
batter_ids <- data2019b %>% 
  select(player_name, batter) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, batter)
batter_ids$name <- paste(batter_ids$first_name, batter_ids$last_name) 
colnames(batter_ids)[3] <- "player_id"
batter_ids <- batter_ids %>% select(name, player_id)


pitcher_ids <- data2019p %>% 
  select(player_name, pitcher) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, pitcher)
pitcher_ids$name <- paste(pitcher_ids$first_name, pitcher_ids$last_name) 
colnames(pitcher_ids)[3] <- "player_id"
pitcher_ids <- pitcher_ids %>% select(name, player_id)

all_player_ids <- rbind(batter_ids, pitcher_ids)
all_player_ids <- all_player_ids %>%
  unique()
data2019 <- data2019b

# Adds batter and pitcher names to data frame
data2019 <- data2019 %>%
  left_join(all_player_ids, by = c('batter' = 'player_id')) %>%
  left_join(all_player_ids, by = c('pitcher' = 'player_id'))

colnames(data2019)[93] <- "batter_name"
colnames(data2019)[94] <- "pitcher_name"

# Applies format_data() function to dataset to add additional variables
data2019 <- format_data(data2019)

# Move data into CSV file for permanent data storage
setwd('ENTER FILE PATH')
write_csv(data2019, "Statcast Scrape 2019")

# Update Player ID Database
player_id_database <- read_csv("Player ID Database")

player_id_database <- rbind(player_id_database, all_player_ids)
player_id_database <- player_id_database %>%
  rbind(all_player_ids) %>%
  unique() %>%
  write_csv("Player ID Database")


# ------------------------------------------------------------------------------
#   Scraping 2020 Data
# ------------------------------------------------------------------------------

date_table <- dates(2020)             # Creates date table for 2020 season

# Section is the functional scraping of StatCast data by week - prints message to denote the week being scraped

message(paste0("Scraping Statcast Data for ", date_table$start_date[[1]], " to ", date_table$end_date[[1]]))
march_w1b <- scraper_function_batter(date_table$start_date[[1]], date_table$end_date[[1]])
march_w1p <- scraper_function_pitcher(date_table$start_date[[1]], date_table$end_date[[1]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[2]], " to ", date_table$end_date[[2]]))
march_w2b <- scraper_function_batter(date_table$start_date[[2]], date_table$end_date[[2]])
march_w2p <- scraper_function_pitcher(date_table$start_date[[2]], date_table$end_date[[2]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[3]], " to ", date_table$end_date[[3]]))
march_w3b <- scraper_function_batter(date_table$start_date[[3]], date_table$end_date[[3]])
march_w3p <- scraper_function_pitcher(date_table$start_date[[3]], date_table$end_date[[3]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[4]], " to ", date_table$end_date[[4]]))
march_w4b <- scraper_function_batter(date_table$start_date[[4]], date_table$end_date[[4]])
march_w4p <- scraper_function_pitcher(date_table$start_date[[4]], date_table$end_date[[4]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[5]], " to ", date_table$end_date[[5]]))
march_w5b <- scraper_function_batter(date_table$start_date[[5]], date_table$end_date[[5]])
march_w5p <- scraper_function_pitcher(date_table$start_date[[5]], date_table$end_date[[5]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[6]], " to ", date_table$end_date[[6]]))
april_w1b <- scraper_function_batter(date_table$start_date[[6]], date_table$end_date[[6]])
april_w1p <- scraper_function_pitcher(date_table$start_date[[6]], date_table$end_date[[6]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[7]], " to ", date_table$end_date[[7]]))
april_w2b <- scraper_function_batter(date_table$start_date[[7]], date_table$end_date[[7]])
april_w2p <- scraper_function_pitcher(date_table$start_date[[7]], date_table$end_date[[7]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[8]], " to ", date_table$end_date[[8]]))
april_w3b <- scraper_function_batter(date_table$start_date[[8]], date_table$end_date[[8]])
april_w3p <- scraper_function_pitcher(date_table$start_date[[8]], date_table$end_date[[8]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[9]], " to ", date_table$end_date[[9]]))
april_w4b <- scraper_function_batter(date_table$start_date[[9]], date_table$end_date[[9]])
april_w4p <- scraper_function_pitcher(date_table$start_date[[9]], date_table$end_date[[9]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[10]], " to ", date_table$end_date[[10]]))
may_w1b <- scraper_function_batter(date_table$start_date[[10]], date_table$end_date[[10]])
may_w1p <- scraper_function_pitcher(date_table$start_date[[10]], date_table$end_date[[10]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[11]], " to ", date_table$end_date[[11]]))
may_w2b <- scraper_function_batter(date_table$start_date[[11]], date_table$end_date[[11]])
may_w2p <- scraper_function_pitcher(date_table$start_date[[11]], date_table$end_date[[11]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[12]], " to ", date_table$end_date[[12]]))
may_w3b <- scraper_function_batter(date_table$start_date[[12]], date_table$end_date[[12]])
may_w3p <- scraper_function_pitcher(date_table$start_date[[12]], date_table$end_date[[12]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[13]], " to ", date_table$end_date[[13]]))
may_w4b <- scraper_function_batter(date_table$start_date[[13]], date_table$end_date[[13]])
may_w4p <- scraper_function_pitcher(date_table$start_date[[13]], date_table$end_date[[13]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[14]], " to ", date_table$end_date[[14]]))
june_w1b <- scraper_function_batter(date_table$start_date[[14]], date_table$end_date[[14]])
june_w1p <- scraper_function_pitcher(date_table$start_date[[14]], date_table$end_date[[14]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[15]], " to ", date_table$end_date[[15]]))
june_w2b <- scraper_function_batter(date_table$start_date[[15]], date_table$end_date[[15]])
june_w2p <- scraper_function_pitcher(date_table$start_date[[15]], date_table$end_date[[15]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[16]], " to ", date_table$end_date[[16]]))
june_w3b <- scraper_function_batter(date_table$start_date[[16]], date_table$end_date[[16]])
june_w3p <- scraper_function_pitcher(date_table$start_date[[16]], date_table$end_date[[16]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[17]], " to ", date_table$end_date[[17]]))
june_w4b <- scraper_function_batter(date_table$start_date[[17]], date_table$end_date[[17]])
june_w4p <- scraper_function_pitcher(date_table$start_date[[17]], date_table$end_date[[17]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[18]], " to ", date_table$end_date[[18]]))
june_w5b <- scraper_function_batter(date_table$start_date[[18]], date_table$end_date[[18]])
june_w5p <- scraper_function_pitcher(date_table$start_date[[18]], date_table$end_date[[18]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[19]], " to ", date_table$end_date[[19]]))
july_w1b <- scraper_function_batter(date_table$start_date[[19]], date_table$end_date[[19]])
july_w1p <- scraper_function_pitcher(date_table$start_date[[19]], date_table$end_date[[19]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[20]], " to ", date_table$end_date[[20]]))
july_w2b <- scraper_function_batter(date_table$start_date[[20]], date_table$end_date[[20]])
july_w2p <- scraper_function_pitcher(date_table$start_date[[20]], date_table$end_date[[20]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[21]], " to ", date_table$end_date[[21]]))
july_w3b <- scraper_function_batter(date_table$start_date[[21]], date_table$end_date[[21]])
july_w3p <- scraper_function_pitcher(date_table$start_date[[21]], date_table$end_date[[21]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[22]], " to ", date_table$end_date[[22]]))
july_w4b <- scraper_function_batter(date_table$start_date[[22]], date_table$end_date[[22]])
july_w4p <- scraper_function_pitcher(date_table$start_date[[22]], date_table$end_date[[22]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[23]], " to ", date_table$end_date[[23]]))
august_w1b <- scraper_function_batter(date_table$start_date[[23]], date_table$end_date[[23]])
august_w1p <- scraper_function_pitcher(date_table$start_date[[23]], date_table$end_date[[23]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[24]], " to ", date_table$end_date[[24]]))
august_w2b <- scraper_function_batter(date_table$start_date[[24]], date_table$end_date[[24]])
august_w2p <- scraper_function_pitcher(date_table$start_date[[24]], date_table$end_date[[24]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[25]], " to ", date_table$end_date[[25]]))
august_w3b <- scraper_function_batter(date_table$start_date[[25]], date_table$end_date[[25]])
august_w3p <- scraper_function_pitcher(date_table$start_date[[25]], date_table$end_date[[25]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[26]], " to ", date_table$end_date[[26]]))
august_w4b <- scraper_function_batter(date_table$start_date[[26]], date_table$end_date[[26]])
august_w4p <- scraper_function_pitcher(date_table$start_date[[26]], date_table$end_date[[26]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[27]], " to ", date_table$end_date[[27]]))
august_w5b <- scraper_function_batter(date_table$start_date[[27]], date_table$end_date[[27]])
august_w5p <- scraper_function_pitcher(date_table$start_date[[27]], date_table$end_date[[27]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[28]], " to ", date_table$end_date[[28]]))
september_w1b <- scraper_function_batter(date_table$start_date[[28]], date_table$end_date[[28]])
september_w1p <- scraper_function_pitcher(date_table$start_date[[28]], date_table$end_date[[28]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[29]], " to ", date_table$end_date[[29]]))
september_w2b <- scraper_function_batter(date_table$start_date[[29]], date_table$end_date[[29]])
september_w2p <- scraper_function_pitcher(date_table$start_date[[29]], date_table$end_date[[29]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[30]], " to ", date_table$end_date[[30]]))
september_w3b <- scraper_function_batter(date_table$start_date[[30]], date_table$end_date[[30]])
september_w3p <- scraper_function_pitcher(date_table$start_date[[30]], date_table$end_date[[30]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[31]], " to ", date_table$end_date[[31]]))
september_w4b <- scraper_function_batter(date_table$start_date[[31]], date_table$end_date[[31]])
september_w4p <- scraper_function_pitcher(date_table$start_date[[31]], date_table$end_date[[31]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[32]], " to ", date_table$end_date[[32]]))
october_w1b <- scraper_function_batter(date_table$start_date[[32]], date_table$end_date[[32]])
october_w1p <- scraper_function_pitcher(date_table$start_date[[32]], date_table$end_date[[32]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[33]], " to ", date_table$end_date[[33]]))
october_w2b <- scraper_function_batter(date_table$start_date[[33]], date_table$end_date[[33]])
october_w2p <- scraper_function_pitcher(date_table$start_date[[33]], date_table$end_date[[33]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[34]], " to ", date_table$end_date[[34]]))
october_w3b <- scraper_function_batter(date_table$start_date[[34]], date_table$end_date[[34]])
october_w3p <- scraper_function_pitcher(date_table$start_date[[34]], date_table$end_date[[34]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[35]], " to ", date_table$end_date[[35]]))
october_w4b <- scraper_function_batter(date_table$start_date[[35]], date_table$end_date[[35]])
october_w4p <- scraper_function_pitcher(date_table$start_date[[35]], date_table$end_date[[35]])


# Binds all weeks of data pulled into single dataset for year
data2020b <- rbind(march_w1b,  march_w2b, july_w2b, july_w3b, july_w4b, august_w1b, august_w2b, august_w3b, august_w4b, august_w5b, 
                   september_w1b, september_w2b, september_w3b, september_w4b, october_w1b, october_w2b, october_w3b, october_w4b)
data2020p <- rbind(march_w1p,  march_w2p, july_w2p, july_w3p, july_w4p, august_w1p, august_w2p, august_w3p, august_w4p, august_w5p, 
                   september_w1p, september_w2p, september_w3p, september_w4p, october_w1p, october_w2p, october_w3p, october_w4p)

# Creates data frames for player ID numbers for all players who appeared in the season
batter_ids <- data2020b %>% 
  select(player_name, batter) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, batter)
batter_ids$name <- paste(batter_ids$first_name, batter_ids$last_name) 
colnames(batter_ids)[3] <- "player_id"
batter_ids <- batter_ids %>% select(name, player_id)


pitcher_ids <- data2020p %>% 
  select(player_name, pitcher) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, pitcher)
pitcher_ids$name <- paste(pitcher_ids$first_name, pitcher_ids$last_name) 
colnames(pitcher_ids)[3] <- "player_id"
pitcher_ids <- pitcher_ids %>% select(name, player_id)

all_player_ids <- rbind(batter_ids, pitcher_ids)
all_player_ids <- all_player_ids %>%
  unique()
data2020 <- data2020b

# Adds batter and pitcher names to data frame
data2020 <- data2020 %>%
  left_join(all_player_ids, by = c('batter' = 'player_id')) %>%
  left_join(all_player_ids, by = c('pitcher' = 'player_id'))

colnames(data2020)[93] <- "batter_name"
colnames(data2020)[94] <- "pitcher_name"

# Applies format_data() function to dataset to add additional variables
data2020 <- format_data(data2020)

# Move data into CSV file for permanent data storage
setwd('ENTER FILE PATH')
write_csv(data2020, "Statcast Scrape 2020")

# Update Player ID Database
player_id_database <- read_csv("Player ID Database")

player_id_database <- rbind(player_id_database, all_player_ids)
player_id_database <- player_id_database %>%
  rbind(all_player_ids) %>%
  unique() %>%
  write_csv("Player ID Database")


# ------------------------------------------------------------------------------
#   Scraping 2021 Data
# ------------------------------------------------------------------------------

date_table <- dates(2021)             # Creates date table for 2019 season

# Section is the functional scraping of StatCast data by week - prints message to denote the week being scraped

message(paste0("Scraping Statcast Data for ", date_table$start_date[[1]], " to ", date_table$end_date[[1]]))
march_w1b <- scraper_function_batter(date_table$start_date[[1]], date_table$end_date[[1]])
march_w1p <- scraper_function_pitcher(date_table$start_date[[1]], date_table$end_date[[1]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[2]], " to ", date_table$end_date[[2]]))
march_w2b <- scraper_function_batter(date_table$start_date[[2]], date_table$end_date[[2]])
march_w2p <- scraper_function_pitcher(date_table$start_date[[2]], date_table$end_date[[2]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[3]], " to ", date_table$end_date[[3]]))
march_w3b <- scraper_function_batter(date_table$start_date[[3]], date_table$end_date[[3]])
march_w3p <- scraper_function_pitcher(date_table$start_date[[3]], date_table$end_date[[3]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[4]], " to ", date_table$end_date[[4]]))
march_w4b <- scraper_function_batter(date_table$start_date[[4]], date_table$end_date[[4]])
march_w4p <- scraper_function_pitcher(date_table$start_date[[4]], date_table$end_date[[4]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[5]], " to ", date_table$end_date[[5]]))
march_w5b <- scraper_function_batter(date_table$start_date[[5]], date_table$end_date[[5]])
march_w5p <- scraper_function_pitcher(date_table$start_date[[5]], date_table$end_date[[5]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[6]], " to ", date_table$end_date[[6]]))
april_w1b <- scraper_function_batter(date_table$start_date[[6]], date_table$end_date[[6]])
april_w1p <- scraper_function_pitcher(date_table$start_date[[6]], date_table$end_date[[6]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[7]], " to ", date_table$end_date[[7]]))
april_w2b <- scraper_function_batter(date_table$start_date[[7]], date_table$end_date[[7]])
april_w2p <- scraper_function_pitcher(date_table$start_date[[7]], date_table$end_date[[7]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[8]], " to ", date_table$end_date[[8]]))
april_w3b <- scraper_function_batter(date_table$start_date[[8]], date_table$end_date[[8]])
april_w3p <- scraper_function_pitcher(date_table$start_date[[8]], date_table$end_date[[8]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[9]], " to ", date_table$end_date[[9]]))
april_w4b <- scraper_function_batter(date_table$start_date[[9]], date_table$end_date[[9]])
april_w4p <- scraper_function_pitcher(date_table$start_date[[9]], date_table$end_date[[9]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[10]], " to ", date_table$end_date[[10]]))
may_w1b <- scraper_function_batter(date_table$start_date[[10]], date_table$end_date[[10]])
may_w1p <- scraper_function_pitcher(date_table$start_date[[10]], date_table$end_date[[10]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[11]], " to ", date_table$end_date[[11]]))
may_w2b <- scraper_function_batter(date_table$start_date[[11]], date_table$end_date[[11]])
may_w2p <- scraper_function_pitcher(date_table$start_date[[11]], date_table$end_date[[11]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[12]], " to ", date_table$end_date[[12]]))
may_w3b <- scraper_function_batter(date_table$start_date[[12]], date_table$end_date[[12]])
may_w3p <- scraper_function_pitcher(date_table$start_date[[12]], date_table$end_date[[12]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[13]], " to ", date_table$end_date[[13]]))
may_w4b <- scraper_function_batter(date_table$start_date[[13]], date_table$end_date[[13]])
may_w4p <- scraper_function_pitcher(date_table$start_date[[13]], date_table$end_date[[13]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[14]], " to ", date_table$end_date[[14]]))
june_w1b <- scraper_function_batter(date_table$start_date[[14]], date_table$end_date[[14]])
june_w1p <- scraper_function_pitcher(date_table$start_date[[14]], date_table$end_date[[14]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[15]], " to ", date_table$end_date[[15]]))
june_w2b <- scraper_function_batter(date_table$start_date[[15]], date_table$end_date[[15]])
june_w2p <- scraper_function_pitcher(date_table$start_date[[15]], date_table$end_date[[15]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[16]], " to ", date_table$end_date[[16]]))
june_w3b <- scraper_function_batter(date_table$start_date[[16]], date_table$end_date[[16]])
june_w3p <- scraper_function_pitcher(date_table$start_date[[16]], date_table$end_date[[16]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[17]], " to ", date_table$end_date[[17]]))
june_w4b <- scraper_function_batter(date_table$start_date[[17]], date_table$end_date[[17]])
june_w4p <- scraper_function_pitcher(date_table$start_date[[17]], date_table$end_date[[17]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[18]], " to ", date_table$end_date[[18]]))
june_w5b <- scraper_function_batter(date_table$start_date[[18]], date_table$end_date[[18]])
june_w5p <- scraper_function_pitcher(date_table$start_date[[18]], date_table$end_date[[18]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[19]], " to ", date_table$end_date[[19]]))
july_w1b <- scraper_function_batter(date_table$start_date[[19]], date_table$end_date[[19]])
july_w1p <- scraper_function_pitcher(date_table$start_date[[19]], date_table$end_date[[19]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[20]], " to ", date_table$end_date[[20]]))
july_w2b <- scraper_function_batter(date_table$start_date[[20]], date_table$end_date[[20]])
july_w2p <- scraper_function_pitcher(date_table$start_date[[20]], date_table$end_date[[20]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[21]], " to ", date_table$end_date[[21]]))
july_w3b <- scraper_function_batter(date_table$start_date[[21]], date_table$end_date[[21]])
july_w3p <- scraper_function_pitcher(date_table$start_date[[21]], date_table$end_date[[21]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[22]], " to ", date_table$end_date[[22]]))
july_w4b <- scraper_function_batter(date_table$start_date[[22]], date_table$end_date[[22]])
july_w4p <- scraper_function_pitcher(date_table$start_date[[22]], date_table$end_date[[22]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[23]], " to ", date_table$end_date[[23]]))
august_w1b <- scraper_function_batter(date_table$start_date[[23]], date_table$end_date[[23]])
august_w1p <- scraper_function_pitcher(date_table$start_date[[23]], date_table$end_date[[23]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[24]], " to ", date_table$end_date[[24]]))
august_w2b <- scraper_function_batter(date_table$start_date[[24]], date_table$end_date[[24]])
august_w2p <- scraper_function_pitcher(date_table$start_date[[24]], date_table$end_date[[24]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[25]], " to ", date_table$end_date[[25]]))
august_w3b <- scraper_function_batter(date_table$start_date[[25]], date_table$end_date[[25]])
august_w3p <- scraper_function_pitcher(date_table$start_date[[25]], date_table$end_date[[25]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[26]], " to ", date_table$end_date[[26]]))
august_w4b <- scraper_function_batter(date_table$start_date[[26]], date_table$end_date[[26]])
august_w4p <- scraper_function_pitcher(date_table$start_date[[26]], date_table$end_date[[26]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[27]], " to ", date_table$end_date[[27]]))
august_w5b <- scraper_function_batter(date_table$start_date[[27]], date_table$end_date[[27]])
august_w5p <- scraper_function_pitcher(date_table$start_date[[27]], date_table$end_date[[27]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[28]], " to ", date_table$end_date[[28]]))
september_w1b <- scraper_function_batter(date_table$start_date[[28]], date_table$end_date[[28]])
september_w1p <- scraper_function_pitcher(date_table$start_date[[28]], date_table$end_date[[28]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[29]], " to ", date_table$end_date[[29]]))
september_w2b <- scraper_function_batter(date_table$start_date[[29]], date_table$end_date[[29]])
september_w2p <- scraper_function_pitcher(date_table$start_date[[29]], date_table$end_date[[29]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[30]], " to ", date_table$end_date[[30]]))
september_w3b <- scraper_function_batter(date_table$start_date[[30]], date_table$end_date[[30]])
september_w3p <- scraper_function_pitcher(date_table$start_date[[30]], date_table$end_date[[30]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[31]], " to ", date_table$end_date[[31]]))
september_w4b <- scraper_function_batter(date_table$start_date[[31]], date_table$end_date[[31]])
september_w4p <- scraper_function_pitcher(date_table$start_date[[31]], date_table$end_date[[31]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[32]], " to ", date_table$end_date[[32]]))
october_w1b <- scraper_function_batter(date_table$start_date[[32]], date_table$end_date[[32]])
october_w1p <- scraper_function_pitcher(date_table$start_date[[32]], date_table$end_date[[32]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[33]], " to ", date_table$end_date[[33]]))
october_w2b <- scraper_function_batter(date_table$start_date[[33]], date_table$end_date[[33]])
october_w2p <- scraper_function_pitcher(date_table$start_date[[33]], date_table$end_date[[33]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[34]], " to ", date_table$end_date[[34]]))
october_w3b <- scraper_function_batter(date_table$start_date[[34]], date_table$end_date[[34]])
october_w3p <- scraper_function_pitcher(date_table$start_date[[34]], date_table$end_date[[34]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[35]], " to ", date_table$end_date[[35]]))
october_w4b <- scraper_function_batter(date_table$start_date[[35]], date_table$end_date[[35]])
october_w4p <- scraper_function_pitcher(date_table$start_date[[35]], date_table$end_date[[35]])


# Binds all weeks of data pulled into single dataset for year
data2021b <- rbind(march_w1b,  march_w2b,  march_w3b,  march_w4b,  march_w5b,  april_w1b,  april_w2b,  april_w3b,  april_w4b, may_w1b, may_w2b, may_w3b, may_w4b, 
                   june_w1b, june_w2b, june_w3b, june_w4b, june_w5b, july_w1b, july_w2b, july_w3b, july_w4b, august_w1b, august_w2b, august_w3b, august_w4b, august_w5b, 
                   september_w1b, september_w2b, september_w3b, september_w4b, october_w1b, october_w2b, october_w3b, october_w4b)
data2021p <- rbind(march_w1p,  march_w2p,  march_w3p,  march_w4p,  march_w5p,  april_w1p,  april_w2p,  april_w3p,  april_w4p, may_w1p, may_w2p, may_w3p, may_w4p, 
                   june_w1p, june_w2p, june_w3p, june_w4p, june_w5p, july_w1p, july_w2p, july_w3p, july_w4p, august_w1p, august_w2p, august_w3p, august_w4p, august_w5p, 
                   september_w1p, september_w2p, september_w3p, september_w4p, october_w1p, october_w2p, october_w3p, october_w4p)

# Creates data frames for player ID numbers for all players who appeared in the season
batter_ids <- data2021b %>% 
  select(player_name, batter) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, batter)
batter_ids$name <- paste(batter_ids$first_name, batter_ids$last_name) 
colnames(batter_ids)[3] <- "player_id"
batter_ids <- batter_ids %>% select(name, player_id)


pitcher_ids <- data2021p %>% 
  select(player_name, pitcher) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, pitcher)
pitcher_ids$name <- paste(pitcher_ids$first_name, pitcher_ids$last_name) 
colnames(pitcher_ids)[3] <- "player_id"
pitcher_ids <- pitcher_ids %>% select(name, player_id)

all_player_ids <- rbind(batter_ids, pitcher_ids)
all_player_ids <- all_player_ids %>%
  unique()
data2021 <- data2021b

# Adds batter and pitcher names to data frame
data2021 <- data2021 %>%
  left_join(all_player_ids, by = c('batter' = 'player_id')) %>%
  left_join(all_player_ids, by = c('pitcher' = 'player_id'))

colnames(data2021)[93] <- "batter_name"
colnames(data2021)[94] <- "pitcher_name"

# Applies format_data() function to dataset to add additional variables
data2021 <- format_data(data2021)

# Move data into CSV file for permanent data storage
setwd('ENTER FILE PATH')
write_csv(data2021, "Statcast Scrape 2021")

# Update Player ID Database
player_id_database <- read_csv("Player ID Database")

player_id_database <- rbind(player_id_database, all_player_ids)
player_id_database <- player_id_database %>%
  rbind(all_player_ids) %>%
  unique() %>%
  write_csv("Player ID Database")

# ------------------------------------------------------------------------------
#   Scraping 2022 Data
# ------------------------------------------------------------------------------

date_table <- dates(2022)             # Creates date table for 2019 season

# Section is the functional scraping of StatCast data by week - prints message to denote the week being scraped

message(paste0("Scraping Statcast Data for ", date_table$start_date[[1]], " to ", date_table$end_date[[1]]))
march_w1b <- scraper_function_batter(date_table$start_date[[1]], date_table$end_date[[1]])
march_w1p <- scraper_function_pitcher(date_table$start_date[[1]], date_table$end_date[[1]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[2]], " to ", date_table$end_date[[2]]))
march_w2b <- scraper_function_batter(date_table$start_date[[2]], date_table$end_date[[2]])
march_w2p <- scraper_function_pitcher(date_table$start_date[[2]], date_table$end_date[[2]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[3]], " to ", date_table$end_date[[3]]))
march_w3b <- scraper_function_batter(date_table$start_date[[3]], date_table$end_date[[3]])
march_w3p <- scraper_function_pitcher(date_table$start_date[[3]], date_table$end_date[[3]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[4]], " to ", date_table$end_date[[4]]))
march_w4b <- scraper_function_batter(date_table$start_date[[4]], date_table$end_date[[4]])
march_w4p <- scraper_function_pitcher(date_table$start_date[[4]], date_table$end_date[[4]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[5]], " to ", date_table$end_date[[5]]))
march_w5b <- scraper_function_batter(date_table$start_date[[5]], date_table$end_date[[5]])
march_w5p <- scraper_function_pitcher(date_table$start_date[[5]], date_table$end_date[[5]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[6]], " to ", date_table$end_date[[6]]))
april_w1b <- scraper_function_batter(date_table$start_date[[6]], date_table$end_date[[6]])
april_w1p <- scraper_function_pitcher(date_table$start_date[[6]], date_table$end_date[[6]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[7]], " to ", date_table$end_date[[7]]))
april_w2b <- scraper_function_batter(date_table$start_date[[7]], date_table$end_date[[7]])
april_w2p <- scraper_function_pitcher(date_table$start_date[[7]], date_table$end_date[[7]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[8]], " to ", date_table$end_date[[8]]))
april_w3b <- scraper_function_batter(date_table$start_date[[8]], date_table$end_date[[8]])
april_w3p <- scraper_function_pitcher(date_table$start_date[[8]], date_table$end_date[[8]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[9]], " to ", date_table$end_date[[9]]))
april_w4b <- scraper_function_batter(date_table$start_date[[9]], date_table$end_date[[9]])
april_w4p <- scraper_function_pitcher(date_table$start_date[[9]], date_table$end_date[[9]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[10]], " to ", date_table$end_date[[10]]))
may_w1b <- scraper_function_batter(date_table$start_date[[10]], date_table$end_date[[10]])
may_w1p <- scraper_function_pitcher(date_table$start_date[[10]], date_table$end_date[[10]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[11]], " to ", date_table$end_date[[11]]))
may_w2b <- scraper_function_batter(date_table$start_date[[11]], date_table$end_date[[11]])
may_w2p <- scraper_function_pitcher(date_table$start_date[[11]], date_table$end_date[[11]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[12]], " to ", date_table$end_date[[12]]))
may_w3b <- scraper_function_batter(date_table$start_date[[12]], date_table$end_date[[12]])
may_w3p <- scraper_function_pitcher(date_table$start_date[[12]], date_table$end_date[[12]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[13]], " to ", date_table$end_date[[13]]))
may_w4b <- scraper_function_batter(date_table$start_date[[13]], date_table$end_date[[13]])
may_w4p <- scraper_function_pitcher(date_table$start_date[[13]], date_table$end_date[[13]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[14]], " to ", date_table$end_date[[14]]))
june_w1b <- scraper_function_batter(date_table$start_date[[14]], date_table$end_date[[14]])
june_w1p <- scraper_function_pitcher(date_table$start_date[[14]], date_table$end_date[[14]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[15]], " to ", date_table$end_date[[15]]))
june_w2b <- scraper_function_batter(date_table$start_date[[15]], date_table$end_date[[15]])
june_w2p <- scraper_function_pitcher(date_table$start_date[[15]], date_table$end_date[[15]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[16]], " to ", date_table$end_date[[16]]))
june_w3b <- scraper_function_batter(date_table$start_date[[16]], date_table$end_date[[16]])
june_w3p <- scraper_function_pitcher(date_table$start_date[[16]], date_table$end_date[[16]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[17]], " to ", date_table$end_date[[17]]))
june_w4b <- scraper_function_batter(date_table$start_date[[17]], date_table$end_date[[17]])
june_w4p <- scraper_function_pitcher(date_table$start_date[[17]], date_table$end_date[[17]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[18]], " to ", date_table$end_date[[18]]))
june_w5b <- scraper_function_batter(date_table$start_date[[18]], date_table$end_date[[18]])
june_w5p <- scraper_function_pitcher(date_table$start_date[[18]], date_table$end_date[[18]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[19]], " to ", date_table$end_date[[19]]))
july_w1b <- scraper_function_batter(date_table$start_date[[19]], date_table$end_date[[19]])
july_w1p <- scraper_function_pitcher(date_table$start_date[[19]], date_table$end_date[[19]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[20]], " to ", date_table$end_date[[20]]))
july_w2b <- scraper_function_batter(date_table$start_date[[20]], date_table$end_date[[20]])
july_w2p <- scraper_function_pitcher(date_table$start_date[[20]], date_table$end_date[[20]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[21]], " to ", date_table$end_date[[21]]))
july_w3b <- scraper_function_batter(date_table$start_date[[21]], date_table$end_date[[21]])
july_w3p <- scraper_function_pitcher(date_table$start_date[[21]], date_table$end_date[[21]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[22]], " to ", date_table$end_date[[22]]))
july_w4b <- scraper_function_batter(date_table$start_date[[22]], date_table$end_date[[22]])
july_w4p <- scraper_function_pitcher(date_table$start_date[[22]], date_table$end_date[[22]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[23]], " to ", date_table$end_date[[23]]))
august_w1b <- scraper_function_batter(date_table$start_date[[23]], date_table$end_date[[23]])
august_w1p <- scraper_function_pitcher(date_table$start_date[[23]], date_table$end_date[[23]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[24]], " to ", date_table$end_date[[24]]))
august_w2b <- scraper_function_batter(date_table$start_date[[24]], date_table$end_date[[24]])
august_w2p <- scraper_function_pitcher(date_table$start_date[[24]], date_table$end_date[[24]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[25]], " to ", date_table$end_date[[25]]))
august_w3b <- scraper_function_batter(date_table$start_date[[25]], date_table$end_date[[25]])
august_w3p <- scraper_function_pitcher(date_table$start_date[[25]], date_table$end_date[[25]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[26]], " to ", date_table$end_date[[26]]))
august_w4b <- scraper_function_batter(date_table$start_date[[26]], date_table$end_date[[26]])
august_w4p <- scraper_function_pitcher(date_table$start_date[[26]], date_table$end_date[[26]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[27]], " to ", date_table$end_date[[27]]))
august_w5b <- scraper_function_batter(date_table$start_date[[27]], date_table$end_date[[27]])
august_w5p <- scraper_function_pitcher(date_table$start_date[[27]], date_table$end_date[[27]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[28]], " to ", date_table$end_date[[28]]))
september_w1b <- scraper_function_batter(date_table$start_date[[28]], date_table$end_date[[28]])
september_w1p <- scraper_function_pitcher(date_table$start_date[[28]], date_table$end_date[[28]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[29]], " to ", date_table$end_date[[29]]))
september_w2b <- scraper_function_batter(date_table$start_date[[29]], date_table$end_date[[29]])
september_w2p <- scraper_function_pitcher(date_table$start_date[[29]], date_table$end_date[[29]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[30]], " to ", date_table$end_date[[30]]))
september_w3b <- scraper_function_batter(date_table$start_date[[30]], date_table$end_date[[30]])
september_w3p <- scraper_function_pitcher(date_table$start_date[[30]], date_table$end_date[[30]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[31]], " to ", date_table$end_date[[31]]))
september_w4b <- scraper_function_batter(date_table$start_date[[31]], date_table$end_date[[31]])
september_w4p <- scraper_function_pitcher(date_table$start_date[[31]], date_table$end_date[[31]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[32]], " to ", date_table$end_date[[32]]))
october_w1b <- scraper_function_batter(date_table$start_date[[32]], date_table$end_date[[32]])
october_w1p <- scraper_function_pitcher(date_table$start_date[[32]], date_table$end_date[[32]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[33]], " to ", date_table$end_date[[33]]))
october_w2b <- scraper_function_batter(date_table$start_date[[33]], date_table$end_date[[33]])
october_w2p <- scraper_function_pitcher(date_table$start_date[[33]], date_table$end_date[[33]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[34]], " to ", date_table$end_date[[34]]))
october_w3b <- scraper_function_batter(date_table$start_date[[34]], date_table$end_date[[34]])
october_w3p <- scraper_function_pitcher(date_table$start_date[[34]], date_table$end_date[[34]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[35]], " to ", date_table$end_date[[35]]))
october_w4b <- scraper_function_batter(date_table$start_date[[35]], date_table$end_date[[35]])
october_w4p <- scraper_function_pitcher(date_table$start_date[[35]], date_table$end_date[[35]])
message(paste0("Scraping Statcast Data for ", "2022-11-01", " to ", "2022-11-05"))
november_w1b <- scraper_function_batter(start_date = "2022-11-01", end_date = "2022-11-05")
november_w1p <- scraper_function_pitcher(start_date = "2022-11-01", end_date = "2022-11-05")

# Binds all weeks of data pulled into single dataset for year
data2022b <- rbind(march_w3b,  march_w4b,  march_w5b,  april_w1b,  april_w2b,  april_w3b,  april_w4b, may_w1b, may_w2b, may_w3b, may_w4b, 
                   june_w1b, june_w2b, june_w3b, june_w4b, june_w5b, july_w1b, july_w2b, july_w3b, july_w4b, august_w1b, august_w2b, august_w3b, august_w4b, august_w5b, 
                   september_w1b, september_w2b, september_w3b, september_w4b, october_w1b, october_w2b, october_w3b, october_w4b, november_w1b)
data2022p <- rbind(march_w3p,  march_w4p,  march_w5p,  april_w1p,  april_w2p,  april_w3p,  april_w4p, may_w1p, may_w2p, may_w3p, may_w4p, 
                   june_w1p, june_w2p, june_w3p, june_w4p, june_w5p, july_w1p, july_w2p, july_w3p, july_w4p, august_w1p, august_w2p, august_w3p, august_w4p, august_w5p, 
                   september_w1p, september_w2p, september_w3p, september_w4p, october_w1p, october_w2p, october_w3p, october_w4p, november_w1p)

# Creates data frames for player ID numbers for all players who appeared in the season .
# Begins with separating batter & pitcher IDs before combining the data frames before removing duplicates.
# Duplicates would appear when NL pitchers (and AL pitchers when playing NL team in NL ballpark) batted

batter_ids <- data2022b %>% 
  select(player_name, batter) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, batter)
batter_ids$name <- paste(batter_ids$first_name, batter_ids$last_name) 
colnames(batter_ids)[3] <- "player_id"
batter_ids <- batter_ids %>% select(name, player_id)


pitcher_ids <- data2022p %>% 
  select(player_name, pitcher) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, pitcher)
pitcher_ids$name <- paste(pitcher_ids$first_name, pitcher_ids$last_name) 
colnames(pitcher_ids)[3] <- "player_id"
pitcher_ids <- pitcher_ids %>% select(name, player_id)

all_player_ids <- rbind(batter_ids, pitcher_ids)
all_player_ids <- all_player_ids %>%
  unique()
data2022 <- data2022b

# Join batter and pitcher names to main season data frame
data2022 <- data2022 %>%
  left_join(all_player_ids, by = c('batter' = 'player_id')) %>%
  left_join(all_player_ids, by = c('pitcher' = 'player_id'))

colnames(data2022)[93] <- "batter_name"
colnames(data2022)[94] <- "pitcher_name"

# Applies format_data() function to dataset to add additional variables
data2022 <- format_data(data2022)

# Move data into CSV file for permanent data storage
# Set the working directory to ensure correct location of saved CSV file

setwd('ENTER FILE PATH')
write_csv(data2022, "Statcast Scrape 2022")

# Update Player ID Database to include new rookies and international FA signings if applicable
player_id_database <- read_csv("Player ID Database")

player_id_database <- rbind(player_id_database, all_player_ids)
player_id_database <- player_id_database %>%
  rbind(all_player_ids) %>%
  unique() %>%
  write_csv("Player ID Database")





# ------------------------------------------------------------------------------
#   Scraping 2023 Data
# ------------------------------------------------------------------------------

date_table <- dates(2023)             # Creates date table for 2019 season

# Section is the functional scraping of StatCast data by week - prints message to denote the week being scraped

message(paste0("Scraping Statcast Data for ", date_table$start_date[[1]], " to ", date_table$end_date[[1]]))
march_w1b <- scraper_function_batter(date_table$start_date[[1]], date_table$end_date[[1]])
march_w1p <- scraper_function_pitcher(date_table$start_date[[1]], date_table$end_date[[1]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[2]], " to ", date_table$end_date[[2]]))
march_w2b <- scraper_function_batter(date_table$start_date[[2]], date_table$end_date[[2]])
march_w2p <- scraper_function_pitcher(date_table$start_date[[2]], date_table$end_date[[2]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[3]], " to ", date_table$end_date[[3]]))
march_w3b <- scraper_function_batter(date_table$start_date[[3]], date_table$end_date[[3]])
march_w3p <- scraper_function_pitcher(date_table$start_date[[3]], date_table$end_date[[3]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[4]], " to ", date_table$end_date[[4]]))
march_w4b <- scraper_function_batter(date_table$start_date[[4]], date_table$end_date[[4]])
march_w4p <- scraper_function_pitcher(date_table$start_date[[4]], date_table$end_date[[4]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[5]], " to ", date_table$end_date[[5]]))
march_w5b <- scraper_function_batter(date_table$start_date[[5]], date_table$end_date[[5]])
march_w5p <- scraper_function_pitcher(date_table$start_date[[5]], date_table$end_date[[5]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[6]], " to ", date_table$end_date[[6]]))
april_w1b <- scraper_function_batter(date_table$start_date[[6]], date_table$end_date[[6]])
april_w1p <- scraper_function_pitcher(date_table$start_date[[6]], date_table$end_date[[6]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[7]], " to ", date_table$end_date[[7]]))
april_w2b <- scraper_function_batter(date_table$start_date[[7]], date_table$end_date[[7]])
april_w2p <- scraper_function_pitcher(date_table$start_date[[7]], date_table$end_date[[7]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[8]], " to ", date_table$end_date[[8]]))
april_w3b <- scraper_function_batter(date_table$start_date[[8]], date_table$end_date[[8]])
april_w3p <- scraper_function_pitcher(date_table$start_date[[8]], date_table$end_date[[8]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[9]], " to ", date_table$end_date[[9]]))
april_w4b <- scraper_function_batter(date_table$start_date[[9]], date_table$end_date[[9]])
april_w4p <- scraper_function_pitcher(date_table$start_date[[9]], date_table$end_date[[9]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[10]], " to ", date_table$end_date[[10]]))
may_w1b <- scraper_function_batter(date_table$start_date[[10]], date_table$end_date[[10]])
may_w1p <- scraper_function_pitcher(date_table$start_date[[10]], date_table$end_date[[10]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[11]], " to ", date_table$end_date[[11]]))
may_w2b <- scraper_function_batter(date_table$start_date[[11]], date_table$end_date[[11]])
may_w2p <- scraper_function_pitcher(date_table$start_date[[11]], date_table$end_date[[11]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[12]], " to ", date_table$end_date[[12]]))
may_w3b <- scraper_function_batter(date_table$start_date[[12]], date_table$end_date[[12]])
may_w3p <- scraper_function_pitcher(date_table$start_date[[12]], date_table$end_date[[12]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[13]], " to ", date_table$end_date[[13]]))
may_w4b <- scraper_function_batter(date_table$start_date[[13]], date_table$end_date[[13]])
may_w4p <- scraper_function_pitcher(date_table$start_date[[13]], date_table$end_date[[13]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[14]], " to ", date_table$end_date[[14]]))
june_w1b <- scraper_function_batter(date_table$start_date[[14]], date_table$end_date[[14]])
june_w1p <- scraper_function_pitcher(date_table$start_date[[14]], date_table$end_date[[14]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[15]], " to ", date_table$end_date[[15]]))
june_w2b <- scraper_function_batter(date_table$start_date[[15]], date_table$end_date[[15]])
june_w2p <- scraper_function_pitcher(date_table$start_date[[15]], date_table$end_date[[15]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[16]], " to ", date_table$end_date[[16]]))
june_w3b <- scraper_function_batter(date_table$start_date[[16]], date_table$end_date[[16]])
june_w3p <- scraper_function_pitcher(date_table$start_date[[16]], date_table$end_date[[16]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[17]], " to ", date_table$end_date[[17]]))
june_w4b <- scraper_function_batter(date_table$start_date[[17]], date_table$end_date[[17]])
june_w4p <- scraper_function_pitcher(date_table$start_date[[17]], date_table$end_date[[17]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[18]], " to ", date_table$end_date[[18]]))
june_w5b <- scraper_function_batter(date_table$start_date[[18]], date_table$end_date[[18]])
june_w5p <- scraper_function_pitcher(date_table$start_date[[18]], date_table$end_date[[18]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[19]], " to ", date_table$end_date[[19]]))
july_w1b <- scraper_function_batter(date_table$start_date[[19]], date_table$end_date[[19]])
july_w1p <- scraper_function_pitcher(date_table$start_date[[19]], date_table$end_date[[19]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[20]], " to ", date_table$end_date[[20]]))
july_w2b <- scraper_function_batter(date_table$start_date[[20]], date_table$end_date[[20]])
july_w2p <- scraper_function_pitcher(date_table$start_date[[20]], date_table$end_date[[20]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[21]], " to ", date_table$end_date[[21]]))
july_w3b <- scraper_function_batter(date_table$start_date[[21]], date_table$end_date[[21]])
july_w3p <- scraper_function_pitcher(date_table$start_date[[21]], date_table$end_date[[21]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[22]], " to ", date_table$end_date[[22]]))
july_w4b <- scraper_function_batter(date_table$start_date[[22]], date_table$end_date[[22]])
july_w4p <- scraper_function_pitcher(date_table$start_date[[22]], date_table$end_date[[22]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[23]], " to ", date_table$end_date[[23]]))
august_w1b <- scraper_function_batter(date_table$start_date[[23]], date_table$end_date[[23]])
august_w1p <- scraper_function_pitcher(date_table$start_date[[23]], date_table$end_date[[23]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[24]], " to ", date_table$end_date[[24]]))
august_w2b <- scraper_function_batter(date_table$start_date[[24]], date_table$end_date[[24]])
august_w2p <- scraper_function_pitcher(date_table$start_date[[24]], date_table$end_date[[24]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[25]], " to ", date_table$end_date[[25]]))
august_w3b <- scraper_function_batter(date_table$start_date[[25]], date_table$end_date[[25]])
august_w3p <- scraper_function_pitcher(date_table$start_date[[25]], date_table$end_date[[25]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[26]], " to ", date_table$end_date[[26]]))
august_w4b <- scraper_function_batter(date_table$start_date[[26]], date_table$end_date[[26]])
august_w4p <- scraper_function_pitcher(date_table$start_date[[26]], date_table$end_date[[26]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[27]], " to ", date_table$end_date[[27]]))
august_w5b <- scraper_function_batter(date_table$start_date[[27]], date_table$end_date[[27]])
august_w5p <- scraper_function_pitcher(date_table$start_date[[27]], date_table$end_date[[27]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[28]], " to ", date_table$end_date[[28]]))
september_w1b <- scraper_function_batter(date_table$start_date[[28]], date_table$end_date[[28]])
september_w1p <- scraper_function_pitcher(date_table$start_date[[28]], date_table$end_date[[28]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[29]], " to ", date_table$end_date[[29]]))
september_w2b <- scraper_function_batter(date_table$start_date[[29]], date_table$end_date[[29]])
september_w2p <- scraper_function_pitcher(date_table$start_date[[29]], date_table$end_date[[29]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[30]], " to ", date_table$end_date[[30]]))
september_w3b <- scraper_function_batter(date_table$start_date[[30]], date_table$end_date[[30]])
september_w3p <- scraper_function_pitcher(date_table$start_date[[30]], date_table$end_date[[30]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[31]], " to ", date_table$end_date[[31]]))
september_w4b <- scraper_function_batter(date_table$start_date[[31]], date_table$end_date[[31]])
september_w4p <- scraper_function_pitcher(date_table$start_date[[31]], date_table$end_date[[31]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[32]], " to ", date_table$end_date[[32]]))
october_w1b <- scraper_function_batter(date_table$start_date[[32]], date_table$end_date[[32]])
october_w1p <- scraper_function_pitcher(date_table$start_date[[32]], date_table$end_date[[32]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[33]], " to ", date_table$end_date[[33]]))
october_w2b <- scraper_function_batter(date_table$start_date[[33]], date_table$end_date[[33]])
october_w2p <- scraper_function_pitcher(date_table$start_date[[33]], date_table$end_date[[33]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[34]], " to ", date_table$end_date[[34]]))
october_w3b <- scraper_function_batter(date_table$start_date[[34]], date_table$end_date[[34]])
october_w3p <- scraper_function_pitcher(date_table$start_date[[34]], date_table$end_date[[34]])
message(paste0("Scraping Statcast Data for ", date_table$start_date[[35]], " to ", date_table$end_date[[35]]))
october_w4b <- scraper_function_batter(date_table$start_date[[35]], date_table$end_date[[35]])
october_w4p <- scraper_function_pitcher(date_table$start_date[[35]], date_table$end_date[[35]])
message(paste0("Scraping Statcast Data for ", "2023-11-01", " to ", "2023-11-05"))
november_w1b <- scraper_function_batter(start_date = "2023-11-01", end_date = "2023-11-05")
november_w1p <- scraper_function_pitcher(start_date = "2023-11-01", end_date = "2023-11-05")

# Binds all weeks of data pulled into single dataset for year
data2023b <- rbind(march_w3b,  march_w4b,  march_w5b,  april_w1b,  april_w2b,  april_w3b,  april_w4b, may_w1b, may_w2b, may_w3b, may_w4b, 
                   june_w1b, june_w2b, june_w3b, june_w4b, june_w5b, july_w1b, july_w2b, july_w3b, july_w4b, august_w1b, august_w2b, august_w3b, august_w4b, august_w5b, 
                   september_w1b, september_w2b, september_w3b, september_w4b, october_w1b, october_w2b, october_w3b, october_w4b, november_w1b)
data2023p <- rbind(march_w3p,  march_w4p,  march_w5p,  april_w1p,  april_w2p,  april_w3p,  april_w4p, may_w1p, may_w2p, may_w3p, may_w4p, 
                   june_w1p, june_w2p, june_w3p, june_w4p, june_w5p, july_w1p, july_w2p, july_w3p, july_w4p, august_w1p, august_w2p, august_w3p, august_w4p, august_w5p, 
                   september_w1p, september_w2p, september_w3p, september_w4p, october_w1p, october_w2p, october_w3p, october_w4p, november_w1p)

# Creates data frames for player ID numbers for all players who appeared in the season .
# Begins with separating batter & pitcher IDs before combining the data frames before removing duplicates.
# Duplicates would appear when NL pitchers (and AL pitchers when playing NL team in NL ballpark) batted

batter_ids <- data2023b %>% 
  select(player_name, batter) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, batter)
batter_ids$name <- paste(batter_ids$first_name, batter_ids$last_name) 
colnames(batter_ids)[3] <- "player_id"
batter_ids <- batter_ids %>% select(name, player_id)


pitcher_ids <- data2023p %>% 
  select(player_name, pitcher) %>%
  unique() %>%
  separate(col = player_name, into = c("last_name", "first_name"), sep = ", ") %>%
  select(first_name, last_name, pitcher)
pitcher_ids$name <- paste(pitcher_ids$first_name, pitcher_ids$last_name) 
colnames(pitcher_ids)[3] <- "player_id"
pitcher_ids <- pitcher_ids %>% select(name, player_id)

all_player_ids <- rbind(batter_ids, pitcher_ids)
all_player_ids <- all_player_ids %>%
  unique()
data2023 <- data2023b

# Join batter and pitcher names to main season data frame
data2023 <- data2023 %>%
  left_join(all_player_ids, by = c('batter' = 'player_id')) %>%
  left_join(all_player_ids, by = c('pitcher' = 'player_id'))

colnames(data2023)[93] <- "batter_name"
colnames(data2023)[94] <- "pitcher_name"

# Applies format_data() function to dataset to add additional variables
data2023 <- format_data(data2023)

# Move data into CSV file for permanent data storage
# Set the working directory to ensure correct location of saved CSV file

setwd('ENTER FILE PATH')
write_csv(data2023, "Statcast Scrape 2023")

# Update Player ID Database to include new rookies and international FA signings if applicable
player_id_database <- read_csv("Player ID Database")

player_id_database <- rbind(player_id_database, all_player_ids)
player_id_database <- player_id_database %>%
  rbind(all_player_ids) %>%
  unique() %>%
  write_csv("Player ID Database")

# ------------------------------------------------------------------------------
# Combining each season together 
# ------------------------------------------------------------------------------

data_all <- rbind(data2016, data2017, data2018, data2019, data2020, data2021, data2022, data2023)                                
setwd('ENTER FILE PATH')
write_csv(data_all, "Baseball Savant Data 2016-2023")

