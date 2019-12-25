library(tidyverse);library(magrittr);library(RSQLite);library(stringr);library(h2o);library(caroline)

con = dbConnect(SQLite(), "Football_Records.sqlite")
dbListTables(con)
Fixture_Detail =  dbGetQuery(con, "SELECT * FROM Fixture_Detail")
Player_Game_Detail =  dbGetQuery(con, "SELECT * FROM Player_Game_Detail")
Player_Metadata =  dbGetQuery(con, "SELECT * FROM Player_Metadata")
Player_Season =  dbGetQuery(con, "SELECT * FROM Player_Season_Data")
head(Fixture_Detail);head(Player_Game_Detail);head(Player_Metadata);head(Player_Season)
write.delim(Fixture_Detail,"Fixture_Detail.txt", sep = "~");write.delim(Player_Game_Detail,"Player_Game_Detail.txt", sep = "~");write.delim(Player_Metadata,"Player_Metadata.txt", sep = "~");write.delim(Player_Season,"Player_Season.txt", sep = "~")

temp_train = Fixture_Detail %>% select(Day_of_Game,Time_of_Game,Home_Team,Away_Team,Season_Start,Season_End,Winner) %>%
  mutate(Season_short = paste0(Season_Start,"/",Season_End))
head(temp_train);write.csv(temp_train,'temp_train.csv')

###### Use fixture data to create aggregate points  #############
Fixture_Detail_t =  Fixture_Detail %>% mutate(Home_Points = ifelse(Winner == "Home", 3,ifelse(Winner == "Draw", 1,0)),
                           Away_Points = ifelse(Winner == "Home", 0,ifelse(Winner == "Draw", 1,3)),
                           Day_of_Game = as.POSIXct(Day_of_Game, format = "%d/%m/%Y")) %>% arrange(Day_of_Game)

Fixture_Detail_t2 =  bind_rows(Fixture_Detail_t, Fixture_Detail_t) %>% 
                     mutate(Team =  c(Fixture_Detail_t$Home_Team,Fixture_Detail_t$Away_Team), Round_No = as.numeric(Round_No),
                            Act_Points = ifelse(Home_Team == Team, Home_Points, Away_Points)) %>% arrange(Team, Season_ID, Round_No) %>% 
                    mutate(TS_Key = paste0(Team, "_",Season_ID)) 
Fixture_Detail_t3 = Fixture_Detail_t2 %>% split(.$TS_Key)
for(i in 1:length(Fixture_Detail_t3)){
  Fixture_Detail_t3[[i]]$running_points = cumsum(Fixture_Detail_t3[[i]]$Act_Points)
  Fixture_Detail_t3[[i]]$last_rd_pts = lag(Fixture_Detail_t3[[i]]$running_points,1)
    }
Fixture_Detail_t4 = bind_rows(Fixture_Detail_t3) %>%
  mutate(TSR_Key = paste0(TS_Key, "_",Round_No),
         Home_Team_key = paste0(Home_Team, "_",Season_ID,"_",Round_No),Away_Team_key = paste0(Away_Team, "_",Season_ID,"_",Round_No)) %>% select(-TS_Key)
Fixture_Detail_t4[is.na(Fixture_Detail_t4)] <- 0 
H_Team_Results =Fixture_Detail_t4 %>% select(TSR_Key,last_rd_pts) %>% rename(ht_last_rd_pts = last_rd_pts);
A_Team_Results =Fixture_Detail_t4 %>% select(TSR_Key,last_rd_pts) %>% rename(at_last_rd_pts = last_rd_pts)
Fixture_Detail_t5 = Fixture_Detail_t4 %>% select(-TSR_Key) 
Fixture_Detail_t5 = left_join(Fixture_Detail_t5,H_Team_Results, by = c("Home_Team_key" = "TSR_Key"))
Fixture_Detail_t6 = left_join(Fixture_Detail_t5,A_Team_Results, by = c("Away_Team_key" = "TSR_Key"))


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










##### Define which teams are at home in any game, and which are away #####

head(Fixture_Detail)


Player_Game_Detail = Player_Game_Detail_17_t %>%
  group_by(GameID, Team) %>% #taking out is_starting players
  summarise(#mr_avg_teams = mean(mr_n_teams), mr_avg_mins = mean(mr_sum_mins),
            #mr_avg_pl_mins = mean(mr_sum_pl_mins),
            #mr_sum_gls = mean(mr_sum_goals),mr_sum_pl_gls = mean(mr_sum_pl_goals),
            #mr_avg_lgs = mean(mr_avg_diff_leagues),mr_med_pl_ratio = median(mr_mins_pl_ratio),
            avg_teams = mean(n_teams), avg_mins = mean(sum_mins),
            avg_pl_mins = mean(sum_pl_mins),sum_gls = mean(sum_goals),sum_pl_gls = mean(sum_pl_goals),avg_lgs = mean(avg_diff_leagues),pl_ratio = median(mins_pl_ratio)
            ) %>% data.frame() 
            
col_names = c("avg_teams","avg_mins","avg_pl_mins","sum_gls","sum_pl_gls","avg_lgs","pl_ratio")
home_names = paste0("home_",col_names); away_names = paste0("away_",col_names)
HM_Player_Game_Detail = Player_Game_Detail %>% rename_at(vars(col_names), ~ home_names)
AY_Player_Game_Detail = Player_Game_Detail %>% rename_at(vars(col_names), ~ away_names)

train_temp = inner_join(Fixture_Detail,HM_Player_Game_Detail, by =  c("web_ID" = "GameID","Home_Team" = "Team"))
train = inner_join(train_temp,AY_Player_Game_Detail, by =  c("web_ID" = "GameID","Away_Team" = "Team")) %>% 
  select(-web_ID,-Diff,-Day_of_Game,-Time_of_Game,-Home_Team,-Dash,-Away_Team,-Score,-Has_Finished,-FT_Home,-FT_Away,-HT_Home,-HT_Away,-Season_ID,-Season_Start,-Season_End) %>%
  mutate(Round_No = as.numeric(Round_No), Winner = as.factor(Winner))

set.seed(123)
train_samp = train[sample(1:nrow(train),round(nrow(train)*.90,0)),]
test_samp = train[sample(1:nrow(train),round(nrow(train)*.10,0)),] %>% select(-Winner)
test_y = train[sample(1:nrow(train),round(nrow(train)*.10,0)),] %>% select(Winner)

h2o.init()
pl_hex <- as.h2o(train_samp);pl_hex_test <- as.h2o(test_samp)

test <- h2o.automl(y = "Winner", training_frame = pl_hex, max_runtime_secs = 30)

h2o_pred = h2o.predict(test,pl_hex_test)
table(test_y$Winner,as.vector(h2o_pred$predict))
sum(table(test_y$Winner,as.vector(h2o_pred$predict)))
#0.382 w starting players only in prediction #0.30 w all players only in prediction



###### Understand prior winning record for each player  #############
Fixture_Detail %>% mutate(rec_no = 1:nrow(Fixture_Detail)) %>% head()
Fixture_Detail %>% 
  
###################


  

