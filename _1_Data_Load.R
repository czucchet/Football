get_PL_fixture = function(start, end, league = "ALL",...){
  library(rvest);library(tidyverse);library(DBI);library(RSQLite);library(purrr);library(zoo);library(stringr)
  if(end <= start){
    stop("Start year argument must be prior to end year")
  }
  understand_no_years = end - start 
  start_vec = start:end; end_vec = (start+1):(end+1)
  years_in_scope = data.frame(start_year = start_vec,end_year = end_vec) %>% filter(end_year<=end) %>% 
    mutate(years =  paste0("https://www.worldfootball.net/all_matches/eng-premier-league-",start_year,"-",end_year,"/")) 
  for(i in 1:length(years_in_scope$years)){
    line_up = read_html(years_in_scope$years[i]) %>% html_table(fill = TRUE)
    select_fixture = sapply(line_up, function(x) nrow(x));find_fixture = which(select_fixture > 1)
    fixture_temp = line_up[[find_fixture]];fixture_temp[fixture_temp == ""] <- NA 
    names(fixture_temp) = c("Day_of_Game","Time_of_Game", "Home_Team","Dash","Away_Team","Score","Has_Finished","Round");fixture_temp[1, "Round"] <- "1. Round"
    fixture = fixture_temp %>% 
      mutate(Day_of_Game = format(base::as.Date(Day_of_Game, format = "%d/%m/%Y"), "%d/%m/%Y"),
             Has_Finished = ifelse(Day_of_Game < format(Sys.Date(), "%d/%m/%Y"), 1, 0),
             Round_No = gsub("\\. Round", "", Round),
             FT_Home = str_sub(Score,1,1),FT_Away = str_sub(Score,3,3),
             HT_Home = str_sub(Score,6,6),HT_Away = str_sub(Score,8,8),
             Day_of_Game = na.locf(Day_of_Game),Has_Finished = na.locf(Has_Finished),Round_No = na.locf(Round_No),
             Season_ID = paste0(years_in_scope$start_year[i],"/",years_in_scope$end_year[i]),
             Teams_ID = str_replace_all(paste0(str_replace_all(Home_Team, " ","-"),Dash,str_replace_all(Away_Team, " ","-"),"/"), "&-",""),
             Season_Start = years_in_scope$start_year[i],
             Season_End = years_in_scope$end_year[i],
             web_ID = paste0("https://www.worldfootball.net/report/premier-league-",Season_Start,"-",Season_End,"-",Teams_ID),
             FT_Home = as.numeric(FT_Home),FT_Away = as.numeric(FT_Away),
             Winner = ifelse(FT_Home > FT_Away, "Home",ifelse(FT_Home < FT_Away, "Away", "Draw")),
             Diff = FT_Home - FT_Away
              ) %>% filter(Dash == "-") %>% select(-Round,-Teams_ID)
    con = dbConnect(SQLite(), "Football_Records.sqlite")
    db_check = try(dbGetQuery(con, "SELECT * FROM Fixture_Detail"),silent = TRUE)
    if(class(db_check) == "try-error"){
      dbWriteTable(con, "Fixture_Detail", fixture,overwrite = T)
      }
    if(class(db_check) != "try-error"){
      dbWriteTable(con, "Fixture_Detail", fixture,append = T)      
      }
    db_clean = try(dbGetQuery(con, "SELECT * FROM Fixture_Detail"),silent = TRUE)
    if(class(db_clean) != "try-error"){
      all_records_temp =  dbGetQuery(con, "SELECT * FROM Fixture_Detail")
      all_records = unique(all_records_temp)
      dbWriteTable(con, "Fixture_Detail", all_records,overwrite = T)
      }
    }
  }
get_PL_fixture(2017,2018)

get_PL_games = function(){
  library(rvest);library(tidyverse);library(DBI);library(RSQLite);library(purrr);library(zoo);library(stringr)
  con = dbConnect(SQLite(), "Football_Records.sqlite")
  all_fixtures =  dbGetQuery(con, "SELECT * FROM Fixture_Detail")
  for(i in 1:length(all_fixtures$web_ID)){
  game = read_html(all_fixtures$web_ID[i]) %>% html_table(fill = TRUE)
  select_game = sapply(game, function(x) nrow(x));find_game = which(select_game > 12 & select_game < 22)
  game_temp = game[find_game]
  if(length(game_temp) != 0) {
  names(game_temp[[1]]) = c("Number","Player","Subbed_Time")
  game_temp[[1]] = game_temp[[1]] %>% 
    mutate(Is_Starting = 1,Team = rep(all_fixtures[i, "Home_Team"], nrow(game_temp[[1]])),Is_Home = 1,
           GameID = rep(all_fixtures[i,"web_ID"],nrow(game_temp[[1]])),SeasonID = rep(all_fixtures[i,"Season_ID"],nrow(game_temp[[1]]))
          )
    game_temp[[1]][which(str_sub(game_temp[[1]]$Subbed_Time,1,3) == "Sub"):nrow(game_temp[[1]]),"Is_Starting"] <- 0
    game_temp[[1]]$Player = str_replace_all(game_temp[[1]]$Player, "[\r\n\t]" , "")

    names(game_temp[[2]]) = c("Number","Player","Subbed_Time")
    game_temp[[2]] = game_temp[[2]] %>% 
      mutate(Is_Starting = 1,Team = rep(all_fixtures[i, "Away_Team"], nrow(game_temp[[2]])),Is_Home = 0,
             GameID = rep(all_fixtures[i,"web_ID"],nrow(game_temp[[2]])),SeasonID = rep(all_fixtures[i,"Season_ID"],nrow(game_temp[[2]]))
      )
    game_temp[[2]][which(str_sub(game_temp[[2]]$Subbed_Time,1,3) == "Sub"):nrow(game_temp[[2]]),"Is_Starting"] <- 0
    game = bind_rows(game_temp)
    game = game[game$Subbed_Time != "Substitutes",] 
    game$Player = gsub('[0-9]+', '',str_replace_all(str_replace_all(game$Player, "[\r\n\t]" , ""), "[[:punct:]]", ""))
    game$Player_Key = paste0(game$Player,"_",game$Team,"_",game$SeasonID)
    db_check = try(dbGetQuery(con, "SELECT * FROM Player_Game_Detail"),silent = TRUE)
    if(class(db_check) == "try-error"){
      dbWriteTable(con, "Player_Game_Detail", game,overwrite = T)
        }
    if(class(db_check) != "try-error"){
      dbWriteTable(con, "Player_Game_Detail", game,append = T)      
        }
    db_clean = try(dbGetQuery(con, "SELECT * FROM Player_Game_Detail"),silent = TRUE)
    if(class(db_clean) != "try-error"){
      all_records_temp =  dbGetQuery(con, "SELECT * FROM Player_Game_Detail")
      all_records = unique(all_records_temp)
      dbWriteTable(con, "Player_Game_Detail", all_records,overwrite = T)
          }
        }
      }
    }
get_PL_games()


get_PL_metadata = function() {
  library(rvest);library(tidyverse);library(DBI);library(RSQLite);library(purrr);library(zoo);library(stringr);library(BBmisc)
  con = dbConnect(SQLite(), "Football_Records.sqlite")
  all_player_games =  dbGetQuery(con, "SELECT * FROM Player_Game_Detail")
  dis_rows = all_player_games %>% select(Player,Team,SeasonID) %>% distinct() %>% nrow()
  distinct_players = all_player_games %>% select(Player,Team,SeasonID) %>% distinct() %>%
    mutate(Player_New  = str_replace_all(Player," ","-"),
           Player_Link = paste0("https://www.worldfootball.net/player_summary/",str_replace_all(Player," ","-"), "/2/"),
           DOB = rep(NA, dis_rows),Country = rep(NA, dis_rows),Height = rep(NA, dis_rows),
           Height = rep(NA, dis_rows),Weight = rep(NA, dis_rows),Position = rep(NA, dis_rows),Foot = rep(NA, dis_rows)
    );
  #distinct_players = distinct_players_temp[start:end,]
  for(i in 1:length(distinct_players$Player_Link)){
    games = read_html(distinct_players$Player_Link[i]) %>% html_table(fill = TRUE)
    get_metadata = sapply(games, function(x) nrow(x));get_metadata_cols = sapply(games, function(x) ncol(x))
    find_metadata = which(get_metadata > 5 & get_metadata <= 10 & get_metadata_cols == 2)
    find_matchdata = which(get_metadata > 0 & get_metadata_cols == 12)
    if(sum(is.na(data.frame(games[find_metadata])$X1)) != length(is.na(data.frame(games[find_metadata])$X1))){ 
      if((which(data.frame(games[find_metadata])$X1 == "Born:") == 1 & length(data.frame(games[find_metadata])$X1 == 7)) |
         nrow(data.frame(games[find_metadata])) >= 9  &
         length(data.frame(games[find_metadata])$X1[which(str_detect(data.frame(games[find_metadata])$X1, "Born"))]) > 0
      ){
        metadata = data.frame(games[find_metadata]); names(metadata)  = c("Names", "Info")
        metadata$Info = str_replace_all(str_replace_all(metadata$Info, "[\r\n\t]" , ""), "[[:punct:]]", "")
        length(metadata$Info)
        metadata = metadata[which(metadata$Names  == "Born:"):nrow(metadata),]
        metadata = metadata[1:length(metadata$Info),]
        distinct_players[i,"DOB"] =  metadata$Info[1]; distinct_players[i,"Country"] =  metadata$Info[3];distinct_players[i,"Height"] =  metadata$Info[4];  
        distinct_players[i,"Weight"] =  metadata$Info[5];distinct_players[i,"Position"] =  metadata$Info[6]; distinct_players[i,"Foot"] = ifelse(length(metadata$Info) <= 6,NA,metadata$Info[7])
      } 
    }
    do_DB_Player_Metadata = function() {
      db_check = try(dbGetQuery(con, "SELECT * FROM Player_Metadata"),silent = TRUE)
      if(class(db_check) == "try-error"){
        dbWriteTable(con, "Player_Metadata", distinct_players,overwrite = T)
      }
      if(class(db_check) != "try-error"){
        dbWriteTable(con, "Player_Metadata", distinct_players,append = T)      
      }
      db_clean = try(dbGetQuery(con, "SELECT * FROM Player_Metadata"),silent = TRUE)
      if(class(db_clean) != "try-error"){
        all_records_temp =  dbGetQuery(con, "SELECT * FROM Player_Metadata")
        all_records = unique(all_records_temp)
        dbWriteTable(con, "Player_Metadata", all_records,overwrite = T)
      }
    }
  }
  distinct_players$num_Height = as.numeric(str_replace_all(distinct_players$Height," cm",""));distinct_players$num_Weight = as.numeric(str_replace_all(distinct_players$Weight," kg",""))
  distinct_players$DOB = format(as.Date(distinct_players$DOB,format = "%d%m%Y"),"%d/%m/%Y")
  distinct_players$Field_Position = ifelse(distinct_players$Position %in% distinct_players$Position[str_which(tolower(distinct_players$Position), "midfielder")] &distinct_players$Position %in% distinct_players$Position[str_which(tolower(distinct_players$Position), "back")],"Defense and Midfield",
                                           ifelse(distinct_players$Position %in% distinct_players$Position[str_which(tolower(distinct_players$Position), "midfielder")] &distinct_players$Position %in% distinct_players$Position[str_which(tolower(distinct_players$Position), "winger")],"Midfield and Attack",
                                                  ifelse(distinct_players$Position %in% distinct_players$Position[str_which(tolower(distinct_players$Position), "midfielder")] &distinct_players$Position %in% distinct_players$Position[str_which(tolower(distinct_players$Position), "forward")],"Midfield and Attack",
                                                         ifelse(distinct_players$Position %in% distinct_players$Position[str_which(tolower(distinct_players$Position), "winger")] | distinct_players$Position %in% distinct_players$Position[str_which(tolower(distinct_players$Position), "forward")],"Attack",
                                                                ifelse(distinct_players$Position %in% distinct_players$Position[str_which(tolower(distinct_players$Position), "midfielder")],"Midfielder",
                                                                       ifelse(distinct_players$Position %in% distinct_players$Position[str_which(tolower(distinct_players$Position), "back")], "Defense",
                                                                              ifelse(distinct_players$Position %in% distinct_players$Position[str_which(tolower(distinct_players$Position), "keeper")], "Goalkeeper","Other")))))))
  distinct_players$Player_Key = paste0(distinct_players$Player,"_",distinct_players$Team,"_",distinct_players$SeasonID)
  do_DB_Player_Metadata()
  test <<- distinct_players
}
get_PL_metadata()



dbListTables(con)
dbRemoveTable(con,"Fixture_Detail")
dbRemoveTable(con,"Player_Game_Detail")

