archived code
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
