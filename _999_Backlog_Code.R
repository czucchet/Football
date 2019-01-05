player_search = paste0(distinct_players$Team[i],"_",distinct_players$SeasonID[i])
player_lookup = c(paste0("https://www.worldfootball.net/player_summary/", distinct_players$Player_New[i],"/2/"),paste0("https://www.worldfootball.net/player_summary/", distinct_players$Player_New[i],"_", 2:7, "/2/"))

matchdata = data.frame(games[find_matchdata]); match_lookup = paste0(matchdata$Team,"_",matchdata$Season)
match_first = ifelse(is.na(match(player_search,match_lookup)),0,1)
has_match[i] = ifelse(match_first > 0 , 1,0)
if(has_match[i] == 1000){
  has_match_results = rep(NA,length(player_lookup));has_match_results[1] = 0
  for(j in 2:length(player_lookup)){
    games = read_html(player_lookup[j]) %>% html_table(fill = TRUE)
    get_metadata = sapply(games, function(x) nrow(x));get_metadata_cols = sapply(games, function(x) ncol(x))
    find_matchdata = which(get_metadata > 0 & get_metadata_cols == 12)
    matchdata = data.frame(games[find_matchdata]); match_lookup = paste0(matchdata$Team,"_",matchdata$Season)
    match_first = ifelse(is.na(match(player_search,match_lookup)),0,1)
    has_match = ifelse(match_first > 0 , 1,0)
    has_match_results[j] <<- has_match
  }
  distinct_players$URL_to_Use[i] = ifelse(length(player_lookup[which(has_match_results == 1)]) == 0, player_lookup[1],player_lookup[which(has_match_results == 1)])
}
if(has_match[i] == 1000){
  distinct_players$URL_to_Use[i] = player_lookup[1]
}
distinct_players$URL_to_Use[i] = player_lookup[1]
check = rep(NA,length(distinct_players$URL_to_Use))
check[i] = ifelse(length(which_player_url) ==0 ,NA, which_player_url)

