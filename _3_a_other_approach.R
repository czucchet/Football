library(tidyverse);library(magrittr);library(RSQLite);library(stringr);library(h2o)

con = dbConnect(SQLite(), "Football_Records.sqlite")
dbListTables(con)
Fixture_Detail =  dbGetQuery(con, "SELECT * FROM Fixture_Detail")
Player_Game_Detail =  dbGetQuery(con, "SELECT * FROM Player_Game_Detail") %>% 
  mutate(Player_Name_Season = paste0(Player,"_", substr(SeasonID,1,4)))
Player_Metadata =  dbGetQuery(con, "SELECT * FROM Player_Metadata")
Player_Season =  dbGetQuery(con, "SELECT * FROM Player_Season_Data")
head(Fixture_Detail);head(Player_Game_Detail);head(Player_Metadata);head(Player_Season)

temp_train = Fixture_Detail %>% select(Day_of_Game,Time_of_Game,Home_Team,Away_Team,Season_Start,Season_End,Winner) %>%
  mutate(Season_short = paste0(Season_Start,"/",Season_End))
head(temp_train);write.csv(temp_train,'temp_train.csv')

###### Create player summaries for results of games played to then join onto player season stats #############

Player_Results_T = left_join(Player_Game_Detail, Fixture_Detail, by = c("GameID" = "web_ID")) %>% 
  filter(Subbed_Time != "" | Is_Starting == 1) %>%  
  mutate(Points = ifelse(Team == Home_Team & Winner == "Home", 3,ifelse(Team == Away_Team & Winner == "Away", 3, 
                                ifelse(Winner == "Draw", 1, 0)))) %>% 
  group_by(Player, Team,SeasonID) %>% 
  summarise(Start_Avg_Points =  round(sum(Points[Is_Starting == 1])/(length(Is_Starting[Is_Starting == 1]*3)),2),
            Start_Wins =  length(Points[Is_Starting == 1 & Points == 3]),Start_Draws =  length(Points[Is_Starting == 1 & Points == 1]),Start_Loss =  length(Points[Is_Starting == 1 & Points == 0]),
            N_Start_Avg_Points =  round(sum(Points[Is_Starting == 0])/(length(Is_Starting[Is_Starting == 0]*3)),2),
            N_Start_Wins =  length(Points[Is_Starting == 0 & Points == 3]),N_Start_Draws =  length(Points[Is_Starting == 0 & Points == 1]),N_Start_Loss =  length(Points[Is_Starting == 0 & Points == 0])
            ) %>%
  mutate(Player_Key = paste0(Player,"_",Team,"_",substr(SeasonID,1,4))) %>% data.frame() 
#%>% select(-Player, -SeasonID,-Team) 

Player_Results_T[is.na(Player_Results_T)] <- 0
Player_Results_T %>%  filter(Player == "Luis Suárez")

###### Create player summaries to take up to game level #############
player_summ_t =   Player_Season %>%
  mutate(mins_per_league = ((Starts - Sub_Out) *90) + (Sub_In *20) + (Sub_Out * 70),Season_short = str_sub(Season,1,9),
         Player_Key = paste0(Player_Name,"_",substr(Season_short,1,4),"_",Team)) %>%
  group_by(Player_Link,Season_short,Team,Player_Key,Player_Name) %>%
  summarise(diff_leagues = length(unique(League)),mins = sum(mins_per_league),n_matches = sum(Matches),goals = sum(Goals)
  ) %>%
  data.frame() %>% arrange(desc(Season_short)) %>% mutate(Player_Name = str_replace(Player_Name, "-"," "))
pl_player_summ_t = Player_Season %>% filter(League == "Pr League") %>%
  mutate(mins_per_league = ((Starts - Sub_Out) *90) + (Sub_In *20) + (Sub_Out * 70),Season_short = str_sub(Season,1,9),
         Player_Key = paste0(Player_Name,"_",substr(Season_short,1,4),"_",Team)) %>%
  group_by(Player_Link,Season_short,Team,Player_Key,Player_Name) %>%
  summarise(pl_mins = sum(mins_per_league),pl_n_matches = sum(Matches),pl_goals = sum(Goals)
  ) %>% 
  data.frame() %>% arrange(desc(Season_short)) 
# %>% select(-Player_Link,-Season_short,-Team) 

Player_Season %>% filter(Player_Name == "Luis Suárez")
player_summ_t %>% filter(Player_Name == "Luis Suárez")

player_summ_t2 =   left_join(player_summ_t, Player_Results_T) 

  head(player_summ_t2,10)



player_summ_t2 =  left_join(player_summ_t,pl_player_summ_t);player_summ_t2[is.na(player_summ_t2)] <- 0
player_summ_t3 = player_summ_t2 %>% mutate(Player_Key = paste0(Player_Name, "_",Season_short,"_",Team),
                                           Season = substr(Season_short,1,4)) %>% select(-Player_Link,Season_short,-Team,-player_key)
##### Selection for which seasons in scope placed here ###############

#best used for player_summ_t3 
player_stats = function(data, years, start,end){
  data_t1 = data %>% filter(as.numeric(Season) >= start & as.numeric(Season) <= end)
  n_seasons = as.numeric(sort(unique(data_t1$Season)))
  
  df_years_t = data.frame(matrix(NA, length(n_seasons), years)); names(df_years_t) = paste0("year_", 1:years)
  for(i in 1:nrow(df_years_t)){
    years_filt = n_seasons[i:(i+years-1)]
    df_years_t[i,] = years_filt 
      }
  df_years <<- df_years_t %>% na.omit()
  years_min = as.vector(apply(df_years,1,min));years_max = as.vector(apply(df_years,1,max))
  
  ls_years =  nrow(df_years) %>% 
  rerun(data.frame(matrix(NA, nrow(data_t1),ncol(data_t1)))) %>% 
  map(~set_names(.x,names(data_t1)))
  
  for(i in 1:length(ls_years)){
    ls_years[[i]] = data_t1 %>% filter(Season >= years_min[i] & Season <= years_max[i])
    ls_years[[i]] = ls_years[[i]] %>% 
      mutate(team_ID = paste0(sapply(strsplit(Player_Key,"_",fixed = T), `[`, 1),"_", sapply(strsplit(Player_Key,"_",fixed = T), `[`, 3)),
             season_ID = max(Season)) %>%
    group_by(Player_Name,season_ID) %>%
      summarise(n_teams = length(unique(team_ID)),
                sum_mins = sum(mins),sum_pl_mins = sum(pl_mins),
                sum_goals = sum(goals),sum_pl_goals = sum(pl_goals),
                avg_diff_leagues = mean(diff_leagues),mins_pl_ratio = round(sum(pl_mins)/sum(mins),2),goals_pl_ratio = round(sum(pl_goals)/sum(goals),2)
      ) %>% data.frame();ls_years[[i]][is.na(ls_years[[i]])] <- 0
  }
  all_years <<- bind_rows(ls_years)
  all_years <<- all_years %>%  mutate(Player_Name_Season = paste0(Player_Name,"_",season_ID))
  }
player_stats(player_summ_t3,2,2012,2018)
head(all_years)
  


Player_Game_Detail_all = left_join(all_years, Player_Game_Detail, by =c("Player_Name_Season" ="Player_Name_Season")) %>% na.omit()
head(Player_Game_Detail_all)


##### Define which teams are at home in any game, and which are away #####


Player_Game_Detail = Player_Game_Detail_all %>%
  group_by(GameID, Team,Is_Starting) %>% #taking out  players
  summarise(#mr_avg_teams = mean(mr_n_teams), mr_avg_mins = mean(mr_sum_mins),
    #mr_avg_pl_mins = mean(mr_sum_pl_mins),
    #mr_sum_gls = mean(mr_sum_goals),mr_sum_pl_gls = mean(mr_sum_pl_goals),
    #mr_avg_lgs = mean(mr_avg_diff_leagues),mr_med_pl_ratio = median(mr_mins_pl_ratio),
    avg_teams = mean(n_teams), avg_mins = mean(sum_mins),
    avg_pl_mins = mean(sum_pl_mins),sum_gls = mean(sum_goals),sum_pl_gls = mean(sum_pl_goals),avg_lgs = mean(avg_diff_leagues),pl_ratio = median(mins_pl_ratio)
  ) %>% filter(Is_Starting == 1) %>% data.frame() %>% select(-Is_Starting)

col_names = c("avg_teams","avg_mins","avg_pl_mins","sum_gls","sum_pl_gls","avg_lgs","pl_ratio")
home_names = paste0("home_",col_names); away_names = paste0("away_",col_names)
HM_Player_Game_Detail = Player_Game_Detail %>% rename_at(vars(col_names), ~ home_names)
AY_Player_Game_Detail = Player_Game_Detail %>% rename_at(vars(col_names), ~ away_names)

train_temp = inner_join(Fixture_Detail,HM_Player_Game_Detail, by =  c("web_ID" = "GameID","Home_Team" = "Team"))
train = inner_join(train_temp,AY_Player_Game_Detail, by =  c("web_ID" = "GameID","Away_Team" = "Team")) %>% 
  select(-web_ID,-Diff,-Day_of_Game,-Time_of_Game,-Home_Team,-Dash,-Away_Team,-Score,-Has_Finished,-FT_Home,-FT_Away,-HT_Home,-HT_Away,-Season_ID,-Season_End) %>%
  mutate(Round_No = as.numeric(Round_No), Winner = as.factor(Winner))

set.seed(456)
samp = sample(1:nrow(train),round(nrow(train)*.9,0))
train_samp = train[samp,];train_samp_val = train[samp,] %>% select(-Winner)

test_samp = train[-samp,] %>% select(-Winner);test_y = train[-samp,] %>% select(Winner);test_all = train[-samp,] 

h2o.init()
pl_hex <- as.h2o(train_samp);pl_hex_test <- as.h2o(test_samp);pl_hex_test_val <- as.h2o(train_samp_val)

test <- h2o.automl(y = "Winner", training_frame = pl_hex, max_runtime_secs = 90,balance_classes = T, stopping_metric = "mean_per_class_error",sort_metric = "mean_per_class_error")

h2o_pred = h2o.predict(test,pl_hex_test)
h2o_pred_val = h2o.predict(test,pl_hex_test_val);h2o_pred_val_df = as.data.frame(h2o_pred_val)

train_val_t = inner_join(train_temp,AY_Player_Game_Detail, by =  c("web_ID" = "GameID","Away_Team" = "Team")) 
train_val = train_val_t[samp,] %>% 
  mutate(predict = h2o_pred_val_df$predict,home = h2o_pred_val_df$Home,draw = h2o_pred_val_df$Draw,away = h2o_pred_val_df$Away)
table(train_val$Winner, train_val$predict)

table(test_y$Winner,as.vector(h2o_pred$predict))
table(as.vector(h2o_pred$predict))


pred_results = 
  (table(test_y$Winner,as.vector(h2o_pred$predict))[1,1]+table(test_y$Winner,as.vector(h2o_pred$predict))[2,2]+table(test_y$Winner,as.vector(h2o_pred$predict))[3,3])/
  sum(table(test_y$Winner,as.vector(h2o_pred$predict)))
#0.35 w starting players only in prediction - sampling changes results

results = data.frame(approach = "Last 3 years", result = pred_results, time_run  = as.character(Sys.Date()))
con = dbConnect(SQLite(), "Football_Records.sqlite")
