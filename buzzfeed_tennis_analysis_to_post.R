rm(list=ls())
options(stringsAsFactors = FALSE)
install.packages(c("doBy","dplyr"))
library(doBy)
library(dplyr)
nsims= 1000000
# In [2]
betting_data = read.csv("/Users/Nic/Downloads/anonymous_betting_data.csv")
median = summaryBy(implied_prob_winner_open~match_uid, data = betting_data,FUN =median)
betting_data2 = merge(betting_data,median, by.x = "match_uid", by.y = "match_uid")%>%
  mutate(deviation=abs(implied_prob_winner_open-implied_prob_winner_open.median))
selected_betting_data = betting_data2[betting_data2$deviation<=0.1 & betting_data2$is_cancelled_or_walkover == "False",]
nrow(sqldf("select distinct match_uid from betting_data"))-nrow(sqldf("select distinct match_uid from selected_betting_data"))
selected_betting_data = mutate(selected_betting_data, 
                               winner_movement = implied_prob_winner_close - implied_prob_winner_open, 
                               loser_movement = implied_prob_loser_close - implied_prob_loser_open)%>%
  mutate(abs_winner_movement=abs(winner_movement))
length(unique(selected_betting_data$match_uid))
high_move_matches = selected_betting_data[selected_betting_data$abs_winner_movement>0.1,]%>%
  arrange(desc(abs_winner_movement))%>%
  distinct(match_uid)
define_high_movement_matches = function(name){
  high_move_matches = rbind(selected_betting_data[selected_betting_data$winner_movement>0.1 & selected_betting_data$loser == name,],
                            selected_betting_data[selected_betting_data$loser_movement>0.1 & selected_betting_data$winner == name,])%>%
    arrange(abs_winner_movement)%>%
    distinct(match_uid)
  return(data.frame(name=name,high_move_matches = nrow(high_move_matches),high_move_losses = nrow(high_move_matches[high_move_matches$loser==name,])))
}
name_list = unique(rbind(data.frame(name=unique(selected_betting_data$loser)),data.frame(name=unique(selected_betting_data$winner))))
for (i in 1:nrow(name_list)){
  name = name_list[i,1]
  if(i==1){
   player_high_move_counts = define_high_movement_matches(name)
  } else {
    player_high_move_counts2 = define_high_movement_matches(name)
    player_high_move_counts = rbind(player_high_move_counts,player_high_move_counts2)
  }
selected_players = player_high_move_counts[player_high_move_counts$high_move_losses>10,]
}
summary_table = data.frame(selected_players, likelihood_open=0) #PEOML = PROBABILITY OF EQUAL OR MORE LOSSES
ptm <- proc.time()
## Now for the simulations
name_list2 = data.frame(name=selected_players$name)
for (i in 1:nrow(name_list2)){
  name = name_list2[i,"name"]
  print(name)
  player_matches = rbind(selected_betting_data[selected_betting_data$winner_movement>0.1 & selected_betting_data$loser == name,],
                         selected_betting_data[selected_betting_data$loser_movement>0.1 & selected_betting_data$winner == name,])%>%
    arrange(desc(abs_winner_movement))%>%
    distinct(match_uid)
  player_matches$player_odds_open = ifelse(player_matches$winner=="name",player_matches$implied_prob_winner_open,player_matches$implied_prob_loser_open)
  player_matches$player_odds_close = ifelse(player_matches$winner=="name",player_matches$implied_prob_winner_close,player_matches$implied_prob_loser_close)
  sims_raw = NULL
  wins_raw = NULL
  wins_raw_temp = as.data.frame(matrix(rep(0,nrow(player_matches)*nsims),nrow=nrow(player_matches),ncol=nsims))
  for (j in 1:nrow(player_matches)){
    sims_raw_temp = data.frame(t(runif(nsims)))
    sims_raw = rbind(sims_raw,sims_raw_temp)
    for (k in 1:nsims){
      if(sims_raw_temp[1,k] < player_matches[j,"player_odds_open"]){
        wins_raw_temp[j,k] =  1
      } else {
        wins_raw_temp[j,k] =  0
      }
    }
    sims_raw_temp = NULL
    print(paste0(name," is ",round(j/nrow(player_matches),2)*100,"% way through their matches"))
  }
  win_totals = colSums(wins_raw_temp)
  lose_totals = as.data.frame(nrow(player_matches)-win_totals)
  sim_losses = 0
  sim_losses = ifelse(lose_totals<=selected_players[selected_players$name==name,"high_move_losses"],0,1)
  summary_table[i,"likelihood_open"] = colSums(sim_losses)/nsims
  }
summary_table2 = arrange(summary_table,likelihood_open)%>%
filter(likelihood_open < 0.05)
summary_table2