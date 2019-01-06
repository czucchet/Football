get_PL_season_stats = function() {
  library(rvest);library(tidyverse);library(DBI);library(RSQLite);library(purrr);library(zoo);library(stringr);library(BBmisc)
  con = dbConnect(SQLite(), "Football_Records.sqlite")
  all_player_games =  dbGetQuery(con, "SELECT * FROM Player_Game_Detail")
  dis_rows = all_player_games %>% select(Player,Team,SeasonID) %>% distinct() %>% nrow()
  distinct_players = all_player_games %>% select(Player,Team,SeasonID) %>% distinct() %>%
    mutate(Player_New  = str_replace_all(Player," ","-"),
           Player_Link = paste0("https://www.worldfootball.net/player_summary/",str_replace_all(Player," ","-"), "/2/")
    )
  for(i in 1:length(distinct_players$Player_Link)){
    games = read_html(distinct_players$Player_Link[i]) %>% html_table(fill = TRUE)
    get_matches = sapply(games, function(x) nrow(x));get_matches_cols = sapply(games, function(x) ncol(x))
    find_season_data = which(get_matches > 0 & get_matches_cols == 12)
    if(length(find_season_data) != 0){
      season_data = data.frame(games[find_season_data]); names(season_data)  = c("League_Icon", "League", "Season", "Team", "Matches", "Goals", "Starts", "Sub_In", "Sub_Out", "Yellows", "Two_Yellows","Red")
      season_data$League = str_replace_all(str_replace_all(season_data$League, "[\r\n\t]" , ""), "[[:punct:]]", "")
      season_data$Player_Name = distinct_players$Player_New[i];season_data$Player_Link = distinct_players$Player_Link[i]
      season_data = season_data %>% select(-League_Icon)
        }
    do_DB_Season_Stats = function() {
      db_check = try(dbGetQuery(con, "SELECT * FROM Player_Season_Data"),silent = TRUE)
      if(class(db_check) == "try-error"){
        dbWriteTable(con, "Player_Season_Data", season_data,overwrite = T)
      }
      if(class(db_check) != "try-error"){
        dbWriteTable(con, "Player_Season_Data", season_data,append = T)
      }
      db_clean = try(dbGetQuery(con, "SELECT * FROM Player_Season_Data"),silent = TRUE)
      if(class(db_clean) != "try-error"){
        all_records_temp =  dbGetQuery(con, "SELECT * FROM Player_Season_Data")
        all_records = unique(all_records_temp)
        dbWriteTable(con, "Player_Season_Data", all_records,overwrite = T)
      }
    }
#  }
  do_DB_Season_Stats()
  }
  test <<- distinct_players
}

get_PL_season_stats()



get_PL_player_stats()
tail(distinct_players,10)
head(distinct_players,15)
con = dbConnect(SQLite(), "Football_Records.sqlite")
all_records =  dbGetQuery(con, "SELECT * FROM Player_Season_Data")
head(test,15)
tail(test,10)
sum(is.na(test$DOB))/length(test$DOB)

    