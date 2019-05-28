library(tidyverse);library(magrittr);library(RSQLite);library(stringr);library(h2o)

con = dbConnect(SQLite(), "Football_Records.sqlite")
dbListTables(con)
Fixture_Detail =  dbGetQuery(con, "SELECT * FROM Fixture_Detail")
Player_Game_Detail =  dbGetQuery(con, "SELECT * FROM Player_Game_Detail")
Player_Metadata =  dbGetQuery(con, "SELECT * FROM Player_Metadata")
Player_Season =  dbGetQuery(con, "SELECT * FROM Player_Season_Data")
head(Fixture_Detail);head(Player_Game_Detail);head(Player_Metadata);head(Player_Season)

temp_train = Fixture_Detail %>% select(Day_of_Game,Time_of_Game,Home_Team,Away_Team,Season_Start,Season_End,Winner) %>%
  mutate(Season_short = paste0(Season_Start,"/",Season_End))
head(temp_train);write.csv(temp_train,'temp_train.csv')

###### Create player summaries to take up to game level #############
player_summ_t =   Player_Season %>%
  mutate(mins_per_league = ((Starts - Sub_Out) *90) + (Sub_In *20) + (Sub_Out * 70),Season_short = str_sub(Season,1,9),
         player_key = paste0(Player_Link,"_",Season_short,"_",Team)) %>%
  group_by(Player_Link,Season_short,Team,player_key,Player_Name) %>%
  summarise(diff_leagues = length(unique(League)),mins = sum(mins_per_league),n_matches = sum(Matches),goals = sum(Goals)
  ) %>%
  data.frame() %>% arrange(desc(Season_short)) %>% mutate(Player_Name = str_replace(Player_Name, "-"," "))
head(player_summ_t)
pl_player_summ_t = Player_Season %>%
  filter(League == "Pr League") %>%
  mutate(mins_per_league = ((Starts - Sub_Out) *90) + (Sub_In *20) + (Sub_Out * 70),Season_short = str_sub(Season,1,9),
         player_key = paste0(Player_Link,"_",Season_short,"_",Team)) %>%
  group_by(Player_Link,Season_short,Team,player_key) %>%
  summarise(pl_mins = sum(mins_per_league),pl_n_matches = sum(Matches),pl_goals = sum(Goals)
  ) %>% 
  data.frame() %>% arrange(desc(Season_short)) %>% select(-Player_Link,-Season_short,-Team)

player_summ_t2 =  left_join(player_summ_t,pl_player_summ_t);player_summ_t2[is.na(player_summ_t2)] <- 0
player_summ_t3 = player_summ_t2 %>% mutate(Player_Key = paste0(Player_Name, "_",Season_short,"_",Team),
                                           Season = substr(Season_short,1,4)) %>% 
  select(-Player_Link,Season_short,-Team,-player_key)
head(player_summ_t3)
##### Selection for which seasons in scope placed here ###############

most_recent = 2016
player_summ_t4 = player_summ_t3 %>%
  filter(Season >= 2015 & Season <= 2016) %>%
  mutate(team_ID = paste0(sapply(strsplit(Player_Key,"_",fixed = T), `[`, 1),"_", sapply(strsplit(Player_Key,"_",fixed = T), `[`, 3))) %>%
  group_by(Player_Name) %>%
  summarise(n_teams = length(unique(team_ID)),
            sum_mins = sum(mins),sum_pl_mins = sum(pl_mins),
            sum_goals = sum(goals),sum_pl_goals = sum(pl_goals),
            avg_diff_leagues = mean(diff_leagues),mins_pl_ratio = round(sum(pl_mins)/sum(mins),2),goals_pl_ratio = round(sum(pl_goals)/sum(goals),2)
  ) %>% data.frame();player_summ_t4[is.na(player_summ_t4)] <- 0

player_summ_t5 = player_summ_t3 %>%
  filter(Season == most_recent ) %>%
  mutate(team_ID = paste0(sapply(strsplit(Player_Key,"_",fixed = T), `[`, 1),"_", sapply(strsplit(Player_Key,"_",fixed = T), `[`, 3))) %>%
  group_by(Player_Name) %>%
  summarise(mr_n_teams = length(unique(team_ID)),mr_sum_mins = sum(mins),mr_sum_pl_mins = sum(pl_mins),mr_sum_goals = sum(goals),mr_sum_pl_goals = sum(pl_goals),
            mr_avg_diff_leagues = mean(diff_leagues),mr_mins_pl_ratio = round(sum(pl_mins)/sum(mins),2),mr_goals_pl_ratio = round(sum(pl_goals)/sum(goals),2)
  ) %>% data.frame();player_summ_t5[is.na(player_summ_t5)] <- 0

player_summ = left_join(player_summ_t5,player_summ_t4, by = "Player_Name") %>% na.omit() ;player_summ[is.na(player_summ)] <- 0

Player_Game_Detail_17_t = Player_Game_Detail %>% filter(SeasonID == "2017/2018")
Player_Game_Detail_17_t2 = left_join(Player_Game_Detail_17, player_summ, by =c("Player" ="Player_Name")) %>% na.omit()


##### Define which teams are at home in any game, and which are away #####


Player_Game_Detail_17 = Player_Game_Detail_17_t2 %>%
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
HM_Player_Game_Detail = Player_Game_Detail_17 %>% rename_at(vars(col_names), ~ home_names)
AY_Player_Game_Detail = Player_Game_Detail_17 %>% rename_at(vars(col_names), ~ away_names)

train_temp = inner_join(Fixture_Detail,HM_Player_Game_Detail, by =  c("web_ID" = "GameID","Home_Team" = "Team"))
train = inner_join(train_temp,AY_Player_Game_Detail, by =  c("web_ID" = "GameID","Away_Team" = "Team")) %>% 
  select(-web_ID,-Diff,-Day_of_Game,-Time_of_Game,-Home_Team,-Dash,-Away_Team,-Score,-Has_Finished,-FT_Home,-FT_Away,-HT_Home,-HT_Away,-Season_ID,-Season_Start,-Season_End) %>%
  mutate(Round_No = as.numeric(Round_No), Winner = as.factor(Winner))

set.seed(456)
samp = sample(1:nrow(train),round(nrow(train)*.8,0))
train_samp = train[samp,]
test_samp = train[-samp,] %>% select(-Winner);test_y = train[-samp,] %>% select(Winner);test_all = train[-samp,] 

h2o.init()
pl_hex <- as.h2o(train_samp);pl_hex_test <- as.h2o(test_samp)

test = h2o.automl(y = "Winner", training_frame = pl_hex, max_runtime_secs = 300)

h2o_pred = h2o.predict(test,pl_hex_test)
table(test_y$Winner,as.vector(h2o_pred$predict))

pred_results = 
  (table(test_y$Winner,as.vector(h2o_pred$predict))[1,1]+table(test_y$Winner,as.vector(h2o_pred$predict))[2,2]+table(test_y$Winner,as.vector(h2o_pred$predict))[3,3])/
sum(table(test_y$Winner,as.vector(h2o_pred$predict)))
#0.35 w starting players only in prediction - sampling changes results

results = data.frame(approach = "Last 3 years", result = pred_results, time_run  = as.character(Sys.Date()))
con = dbConnect(SQLite(), "Football_Records.sqlite")

db_check = try(dbGetQuery(con, "SELECT Player FROM Model_Results"),silent = TRUE)
if(class(db_check) == "try-error"){
  dbWriteTable(con, "Model_Results", results,overwrite = T)
}
if(class(db_check) != "try-error"){
  dbWriteTable(con, "Model_Results", results,append = T)      
}
db_clean = try(dbGetQuery(con, "SELECT * FROM Model_Results"),silent = TRUE)
if(class(db_clean) != "try-error"){
  all_records_temp =  dbGetQuery(con, "SELECT * FROM Model_Results")
  all_records = unique(all_records_temp)
  dbWriteTable(con, "Model_Results", all_records,overwrite = T)
}

