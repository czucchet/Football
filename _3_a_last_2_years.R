library(tidyverse);library(magrittr);library(RSQLite);library(stringr);library(h2o)

con = dbConnect(SQLite(), "Football_Records.sqlite")
dbListTables(con)
Fixture_Detail =  dbGetQuery(con, "SELECT * FROM Fixture_Detail")
Player_Game_Detail =  dbGetQuery(con, "SELECT * FROM Player_Game_Detail")
Player_Metadata =  dbGetQuery(con, "SELECT * FROM Player_Metadata")
Player_Season =  dbGetQuery(con, "SELECT * FROM Player_Season_Data")
head(Fixture_Detail);head(Player_Game_Detail);head(Player_Metadata);head(Player_Season)

Fixture_Detail = Fixture_Detail %>% filter(Season_ID == "2017/2018")

temp_train = Fixture_Detail %>% select(Day_of_Game,Time_of_Game,Home_Team,Away_Team,Season_Start,Season_End,Winner) %>%
  mutate(Season_short = paste0(Season_Start,"/",Season_End))
head(temp_train);write.csv(temp_train,'temp_train.csv')

###### Create player summaries to take up to game level #############
player_summ_t =   Player_Season %>%
  mutate(mins_per_league = ((Starts - Sub_Out) *90) + (Sub_In *20) + (Sub_Out * 70),Season_short = str_sub(Season,1,9),
         player_key = paste0(Player_Link,"_",Season_short,"_",Team),Is_Ch_Lge = ifelse(League == "Ch League", 1,0),Is_Pr_League = ifelse(League == "Pr League", 1,0)
         ) %>% 
  group_by(Player_Link,Season_short,Team,player_key,Player_Name) %>%
  summarise(diff_leagues = length(unique(League)),mins = sum(mins_per_league),n_matches = sum(Matches),goals = sum(Goals)) %>%
  data.frame() %>% arrange(desc(Season_short)) %>% mutate(Player_Name = str_replace(Player_Name, "-"," "))

#For validation when re-run of web scrape takes place - Player_Season %>% filter(Player_Name == "Marcus-Rashford" & Season == "2018/2019");Player_Season %>% filter(Player_Name == "Aaron-Ramsey" & Season == "2018/2019");player_summ_t %>% filter(Player_Name == "Marcus Rashford" & Season_short == "2018/2019");Player_Game_Detail %>% filter(Player == "Marcus Rashford" & SeasonID == "2018/2019")

player_summ_t_CHL =   Player_Season %>%
  mutate(mins_per_league = ((Starts - Sub_Out) *90) + (Sub_In *20) + (Sub_Out * 70),Season_short = str_sub(Season,1,9),
         player_key = paste0(Player_Link,"_",Season_short,"_",Team),Is_Ch_Lge = ifelse(League == "Ch League", 1,0),Is_Pr_League = ifelse(League == "Pr League", 1,0)
  ) %>% filter(Is_Ch_Lge == 1) %>% 
  group_by(Player_Link,Season_short,Team,player_key,Player_Name) %>%
  summarise(cl_matches = sum(Matches),cl_mins = sum(mins_per_league),cl_goals = sum(Goals)) %>%
  data.frame() %>% arrange(desc(Season_short)) %>% mutate(Player_Name = str_replace(Player_Name, "-"," "))

player_summ_t_PREM =   Player_Season %>%
  mutate(mins_per_league = ((Starts - Sub_Out) *90) + (Sub_In *20) + (Sub_Out * 70),Season_short = str_sub(Season,1,9),
         player_key = paste0(Player_Link,"_",Season_short,"_",Team),Is_Ch_Lge = ifelse(League == "Ch League", 1,0),Is_Pr_League = ifelse(League == "Pr League", 1,0)
  ) %>% filter(Is_Pr_League == 1) %>% 
  group_by(Player_Link,Season_short,Team,player_key,Player_Name) %>%
  summarise(pl_mins = sum(mins_per_league),pl_matches = sum(Matches),pl_goals = sum(Goals)) %>%
  data.frame() %>% arrange(desc(Season_short)) %>% mutate(Player_Name = str_replace(Player_Name, "-"," "))

player_summ_t2 =   left_join(player_summ_t,player_summ_t_CHL);player_summ_t2[is.na(player_summ_t2)] <- 0
player_summ_t2t =  left_join(player_summ_t2,player_summ_t_PREM);player_summ_t2t[is.na(player_summ_t2t)] <- 0

player_pre_summ = player_summ_t2t %>% mutate(Player_Key = paste0(Player_Name, "_",Season_short,"_",Team),
                                           Season = substr(Season_short,1,4)) %>% select(-Player_Link,Season_short,-Team,-player_key)
head(player_pre_summ)

##### Selection for which seasons in scope placed here ###############

player_stats = function(data, years, start,end){
  data_t1 = data %>% filter(as.numeric(Season) >= start & as.numeric(Season) <= end)
  n_seasons = as.numeric(sort(unique(data_t1$Season)))
  
  df_years_t = data.frame(matrix(NA, length(n_seasons), years)); names(df_years_t) = paste0("year_", 1:years)
  for(i in 1:nrow(df_years_t)){
    years_filt = n_seasons[i:(i+years-1)]
    df_years_t[i,] = years_filt 
  }
  df_years = df_years_t %>% na.omit()
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
      summarise(n_teams = length(unique(team_ID)),sum_mins = sum(mins)/years,sum_mtchs = sum(n_matches)/years,sum_gls = sum(goals)/years,
                sum_cl_mins = sum(cl_mins)/years,sum_cl_mtchs = sum(cl_matches)/years,sum_cl_gls = sum(cl_goals)/years,
                sum_pl_mins = sum(pl_mins)/years,sum_pl_mtchs = sum(pl_matches)/years,sum_pl_gls = sum(pl_goals)/years,
                avg_diff_leagues = mean(diff_leagues),mins_pl_ratio = round(sum(pl_mins)/sum(mins),2)/years,goals_pl_ratio = round(sum(pl_goals)/sum(goals),2)/years
      ) %>% data.frame();ls_years[[i]][is.na(ls_years[[i]])] <- 0
  }
  all_years <<- bind_rows(ls_years)
}
player_stats(player_pre_summ,2,2015,2018)
head(all_years)


Player_Game_Detail_17_t = left_join(Player_Game_Detail, all_years, by =c("Player" ="Player_Name")) %>% na.omit()


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

train_temp2 = inner_join(train_temp,AY_Player_Game_Detail, by =  c("web_ID" = "GameID","Away_Team" = "Team"))%>% select(Home_Team,Dash,Away_Team,Score,Has_Finished,FT_Home,FT_Away,HT_Home,HT_Away,Season_ID,Winner,Diff,Round_No)

set.seed(456)
samp = sample(1:nrow(train),round(nrow(train)*.9,0))
train_samp = train[samp,]
test_samp = train[-samp,] %>% select(-Winner);test_y = train[-samp,] %>% select(Winner);test_all = train[-samp,]

#h2o model
h2o.init();pl_hex <- as.h2o(train_samp);pl_hex_test <- as.h2o(test_samp)
test = h2o.automl(y = "Winner", training_frame = pl_hex, max_runtime_secs = 30)
h2o_pred = h2o.predict(test,pl_hex_test)
table(test_y$Winner,as.vector(h2o_pred$predict))


#deep learning
train_h2o <- as.h2o(train_samp, destination_frame = "train_samp")

model_h2o <- h2o.deeplearning(x = c(1,3:ncol(train_h2o)),
                              y = 2,
                              training_frame = "train_samp",
                              activation = "Maxout", # ReLU as activation functions
                              hidden = c(8,8,8,8), # Two hidden layers with 16 and 8 neurons
                              epochs = 500,
                              rate = 0.001, # Learning rate
                              adaptive_rate = FALSE, # Simple SGD
                              loss = "Automatic")

h2o_dl_pred = h2o.predict(model_h2o,pl_hex_test)
table(test_y$Winner,as.vector(h2o_pred$predict))







test_matches = train_temp2[-samp,] 
test_matches$Model_Pred = as.vector(h2o_pred$predict)
table(test_matches$Winner,test_matches$Model_Pred)
  
pred_results = 
  (table(test_y$Winner,as.vector(h2o_pred$predict))[1,1]+table(test_y$Winner,as.vector(h2o_pred$predict))[2,2]+table(test_y$Winner,as.vector(h2o_pred$predict))[3,3])/
sum(table(test_y$Winner,as.vector(h2o_pred$predict)))
#0.35 w starting players only in prediction - sampling changes results

results = data.frame(approach = "Last 2 yrs, 17' - 5 mins - h2o", result = pred_results, time_run  = as.character(Sys.Date()))
con = dbConnect(SQLite(), "Football_Records.sqlite")

db_check = try(dbGetQuery(con, "SELECT * FROM Model_Results"),silent = TRUE)
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

saveRDS(test,'model_last_2_yrs_17')  
