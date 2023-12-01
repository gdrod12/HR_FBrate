library(tidyverse)
library(baseballr)
options(scipen=9999, digits=3)
#getting park adjustments
get_home_team_hrfb<-function(season) {
  #filename per season
  filename <- paste("all", season, ".csv", sep="")
  #reading in data
  data <- read_csv(filename) %>%
    #get proper team ids
    mutate(hometeam_id = substr(game_id, 1, 3)) %>%
    group_by(hometeam_id) %>%
    #get flyballs and popups and homeruns
    summarise(home_flyballs=sum(battedball_cd=="F", na.rm=T)+sum(battedball_cd=="P", na.rm=T),
              home_homeruns=sum(event_cd==23)) %>%
    #derive hr/fb rate
    mutate(home_hr_fb = home_homeruns/home_flyballs, season=season)
  return(data)
}
get_road_team_hrfb<-function(season) {
  #filename per season
  filename <- paste("all", season, ".csv", sep="")
  #reading in data
  data <- read_csv(filename) %>%
    mutate(hometeam_id = substr(game_id, 1, 3)) %>%
    #get proper team ids
    group_by(away_team_id) %>%
    #get flyballs and popups and homeruns
    summarise(away_flyballs=sum(battedball_cd=="F", na.rm=T)+sum(battedball_cd=="P", na.rm=T),
              away_homeruns=sum(event_cd==23)) %>%
    #derive hr/fb rate
    mutate(away_hr_fb = away_homeruns/away_flyballs, season=season)
  return(data)
}
#loop through all years
home_team_data<-reduce(lapply(2002:2022, get_home_team_hrfb), rbind)
road_team_data<-reduce(lapply(2002:2022, get_road_team_hrfb), rbind)
#merging to get park data
park_data<-merge(home_team_data, road_team_data, by.x=c("hometeam_id", "season"), 
                 by.y=c("away_team_id", "season")) %>%
  reframe(hometeam_id, season, park_factor=home_hr_fb-away_hr_fb,
          flyballs=home_flyballs+away_flyballs)
#calculating 3 year factors
get_three_year_factors <- function(seasonid){
  #getting team ids
  teams <- unique(park_data$hometeam_id)
  #setting empty data output
  data_output <- data.frame()
  #initialize for loop
  for (team in teams) {
    #get current season data
    current<-park_data %>%
      filter(season==seasonid, hometeam_id==team)
    #for seasons that are not at tailends
    if (seasonid>2002 & seasonid<2022) {
      minus_one <- park_data %>%
        filter(season==seasonid-1, hometeam_id==team)
      plus_one <- park_data %>%
        filter(season==seasonid+1, hometeam_id==team)
    }
    #for seasons that are backended
    else if (seasonid==2002)
    {
      minus_one <- park_data %>%
        filter(season==seasonid+2, hometeam_id==team)
      plus_one <- park_data %>%
        filter(season==seasonid+1, hometeam_id==team)
    }
    else if (seasonid==2022)
    {
      minus_one <- park_data %>%
        filter(season==seasonid-1, hometeam_id==team)
      plus_one <- park_data %>%
        filter(season==seasonid-2, hometeam_id==team)
    }
    #20/60/20 split
    factor <- (current[1,3]*current[1,4]*0.6+minus_one[1,3]*minus_one[1,4]*0.2+
                 plus_one[1,3]*plus_one[1,4]*0.2)/(current[1,4]*0.6+minus_one[1,4]*0.2+plus_one[1,4]*0.2)
    #new dataframe with data
    new_data <- data.frame(hometeam_id=team, raw_park_factor=factor)
    #bind to og dataset
    data_output <- rbind(data_output, new_data)
  }
  data_output$season <- seasonid
  return(data_output)
}
#loop through seasons
park_factors<-reduce(lapply(2002:2022, get_three_year_factors), rbind)
#save dataset
write_csv(park_factors, "homerun_park_factors.csv")
#clear environment
rm(list = ls())
#function to calculate seasonl hrfbs
get_seasonal_hrfb<-function(season) {
  #get filename by season
  filename <- paste("all", season, ".csv", sep="")
  #summarise accordingly
  data <- read_csv(filename) %>%
    reframe(flyballs=sum(battedball_cd=="F", na.rm=T)+sum(battedball_cd=="P", na.rm=T),
              homeruns=sum(event_cd==23)) %>%
    mutate(league_hr_fb = homeruns/flyballs, season=season)
  return(data)
}
#loop through seasons
seasonal_factors <- reduce(lapply(2002:2022, get_seasonal_hrfb), rbind)
#save file
write_csv(seasonal_factors, "seasonal_hr_factors.csv")
#clear envionment
rm(list = ls())
#read in previously saved and cleared datasets
park_factors <- read_csv("homerun_park_factors.csv")
seasonal_factors <- read_csv("seasonal_hr_factors.csv")
#function to era and park adjust hr_fb
get_controlled_hrfb<-function(season) {
  #filename by year
  filename <- paste("all", season, ".csv", sep="")
  #read in data
  data <- read_csv(filename) %>%
    mutate(hometeam_id = substr(game_id, 1, 3), season=year)
  #merge by season and team
  data <- merge(merge(data, seasonal_factors, by="season"), 
                park_factors, 
                by=c("season", "hometeam_id")) %>%
    #calculate averarge hr_fb in environment
    mutate(expected_hr_fb=league_hr_fb+raw_park_factor) %>%
    #filter to just flyballs
    filter(battedball_cd=="F"|battedball_cd=="P") %>%
    #group by individual pitchers
    group_by(pit_id) %>%
    #get playe outcomes
    summarise(player_flyballs=sum(battedball_cd=="F", na.rm=T)+sum(battedball_cd=="P", na.rm=T),
              player_homeruns=sum(event_cd==23),
              expected_hr_fb=mean(expected_hr_fb, na.rm=T)) %>%
    #get residual, which represents the pitcher's "true" hr/fb
    mutate(player_hr_fb = player_homeruns/player_flyballs, season=season,
           expected_hr=expected_hr_fb*player_flyballs) %>%
    mutate(residual=(player_hr_fb-expected_hr_fb))
  return(data)
}
#getting pitcher data loop
pitcher_data<-reduce(lapply(2002:2022, get_controlled_hrfb), rbind)
#saving file
write_csv(pitcher_data, "pitcher_hrfbdata.csv")
#clearing environment
rm(list = ls())
#reloading file
pitcher_data<-read_csv("pitcher_hrfbdata.csv")
#getting n+1 pitcher data, in a sense
next_pitcher_data <- pitcher_data %>%
  reframe(pit_id, next_player_flyballs=player_flyballs,
          next_player_homeruns=player_homeruns,next_expected_hr_fb=expected_hr_fb,
          next_player_hr_fb=player_hr_fb, season=season-1,
          next_residual=residual)
#adding in n+1 to the dataset
year_by_yeardata <- merge(pitcher_data, next_pitcher_data, by=c("pit_id", "season")) 
#reading in pitchfx data
pitch_data<-read_csv("pitcher_pitch_data.csv")
#reading in chadwick playerid data
chadwick<-baseballr::chadwick_player_lu()%>%
  reframe(pit_id=key_retro, pitcher=key_mlbam) %>%
  filter(str_length(pit_id)>1)
#iding data
pitch_data<-merge(pitch_data, chadwick, by="pitcher")
#removing used data
rm(chadwick)
#adding in pitchfx data to current data
year_by_yeardata <- merge(year_by_yeardata, pitch_data, by=c("pit_id", "season"))
#removing used data
rm(pitch_data)
#setting nas to 0
year_by_yeardata[is.na(year_by_yeardata)] <- 0
grouped_pitches<-year_by_yeardata %>%
  reframe(pit_id, season, player_flyballs,expected_hr_fb, player_hr_fb, expected_hr, residual,
          hard_velo=(pitches_FF*velo_FF+velo_FC*pitches_FC+pitches_SI*velo_SI)/
            (pitches_FF+pitches_FC+pitches_SI),
          offspeed_velo=(pitches_CH*velo_CH+velo_FS*pitches_FS+pitches_FO*velo_FO+pitches_SC*velo_SC)/
            (pitches_CH+pitches_FS+pitches_FO+pitches_SC),
          breaking_velo=(pitches_CU*velo_CU+pitches_KC*velo_KC+pitches_CS*velo_CS+
                           pitches_SL*velo_SL+pitches_ST*velo_ST+pitches_SV*velo_SV+
                           pitches_KN*velo_KN+pitches_EP*velo_EP)/
            (pitches_CU+pitches_KC+pitches_CS+pitches_SL+pitches_ST+pitches_SV+
               pitches_KN+pitches_EP),
          hard_rate=(pitches_FF+pitches_FC+pitches_SI)/total_pitches,
          offspeed_rate=(pitches_CH+pitches_FS+pitches_FO+pitches_SC)/total_pitches,
          breaking_rate=(pitches_CU+pitches_KC+pitches_CS+pitches_SL+pitches_ST+pitches_SV+
                           pitches_KN+pitches_EP)/total_pitches) %>%
  mutate(hard_yes=ifelse(hard_velo>0, 1, 0),
         breaking_yes=ifelse(breaking_velo>0, 1, 0),
         offspeed_yes=ifelse(offspeed_velo>0, 1, 0))
grouped_pitches[is.na(grouped_pitches)] <- 0
model_values <- data.frame()
for (i in 8:16){
  
  filtered_df <- grouped_pitches[grouped_pitches[, i] > 0, ] %>%
    filter(player_flyballs>100)
  model<-lm(unlist(filtered_df[7])~unlist(filtered_df[i]))
  variable_name <- names(grouped_pitches[i])
  print(variable_name)
  r_squared <- summary(model)$r.squared
  coefficient <- model$coefficients[2]
  p_value <- coef(summary(model))[, "Pr(>|t|)"][2]
  max_fitted <- max(model$fitted.values)
  min_fitted<-min(model$fitted.values)
  new_data <- data.frame(variable_name=variable_name, 
                         r_squared=r_squared,
                         coefficient=coefficient,
                         p_value = p_value,
                         max_fitted = max_fitted,
                         min_fitted = min_fitted)
  model_values<-rbind(model_values, new_data)
  
}
funmodel1 <- lm(residual~hard_yes+hard_velo:hard_yes+
                  offspeed_yes+
                  breaking_yes+
                  hard_rate:hard_yes+offspeed_rate:offspeed_yes,
                data=grouped_pitches %>%
                  filter(player_flyballs>50))
summary(funmodel1)
max(funmodel1$fitted.values)
min(funmodel1$fitted.values)
summary(funmodel1$fitted.values)
funmodel2 <- lm(residual~hard_yes+hard_yes:hard_velo,
                data=grouped_pitches %>%
                  filter(player_flyballs>50))
summary(funmodel2)
cor_matrix_test<-cor(filtered_df[4:16])
funmodel3 <- lm((I(next_residual * 100)) ~ (I(residual * 100)), data = year_by_yeardata %>% filter(player_flyballs > 50,
                                                                                                   next_player_flyballs>50))
summary(funmodel3)
pitcher_totals<-year_by_yeardata %>%
  group_by(season) %>%
  filter(player_flyballs>50) %>%
  reframe(pitcher_total=n())
ggplot(data=year_by_yeardata %>%
         filter(player_flyballs>50, next_player_flyballs>50)) +
  geom_histogram(aes(x=next_player_hr_fb-player_hr_fb)) +
  theme_minimal() +
  xlab("Difference in HR/FB year to year") +
  ylab("")
ggplot(data=year_by_yeardata %>%
         filter(player_flyballs>50)) +
  geom_histogram(aes(x=player_hr_fb)) +
  theme_minimal() +
  xlab("HR/FB rate") +
  ylab("")
sum(pitcher_totals$pitcher_total)
test<-year_by_yeardata %>%
  filter(player_flyballs>50)
year_by_yeardata%>%
  filter(player_flyballs>50) %>%
  reframe(mean_diff=mean((player_hr_fb-next_player_hr_fb)),
          sd_diff = sd((player_hr_fb-next_player_hr_fb)))
