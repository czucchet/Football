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

















table(distinct_players$Position)


                                         


get_PL_player_stats()
tail(distinct_players,10)
head(distinct_players,15)
con = dbConnect(SQLite(), "Football_Records.sqlite")
all_records =  dbGetQuery(con, "SELECT * FROM Player_Metadata")
head(test,15)
tail(test,10)
sum(is.na(test$DOB))/length(test$DOB)

    