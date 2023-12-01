library(baseballr)
library(tidyverse)
get_pitch_data <- function(season){
#setting start and end dates
start_date <- as.numeric(as.Date(paste(season,"03", "20", sep="-")))
end_date <- as.numeric(as.Date(paste(season,"11", "10", sep="-")))
#initializing empty year dataframe
year_data <- data.frame()
#initialize for loop to get every day from year
for (day in start_date:end_date){
  #set day number to date format 
  new_date <- as.Date(day)
  #print for progress sake
  print(new_date)
  #summarise necessary variables
  new_data<-scrape_statcast_savant(start_date=new_date,
                              end_date=new_date,
                              player_type="pitcher") %>%
  summarise(pitch_type, game_date, release_speed, release_pos_x, release_pos_y,
            release_pos_z, player_name, batter, pitcher, events, description,
            spin_dir, zone, stand, p_throws, home_team, type,
            hit_location, bb_type, pfx_x, pfx_z, plate_x, plate_z,
            hc_x, hc_y, vx0, vy0, vz0, ax, ay, az, sz_top, sz_bot,
            release_pos_y, woba_value, pitch_name, spin_axis, delta_run_exp)
  #append data
  year_data <- rbind(new_data, year_data)
}
csv_name <- paste("x", season, "_pitches.csv", sep="")
write_csv(year_data, csv_name )
}
overall_data<-lapply(2009:2023, get_pitch_data)
get_2008_data <- function(season){
  start_date <- as.numeric(as.Date(paste(season,"03", "25", sep="-")))
  end_date <- as.numeric(as.Date(paste(season,"11", "10", sep="-")))
  year_data <- data.frame()
  for (day in start_date:end_date){
    new_date <- as.Date(day)
    print(new_date)
    new_data<-scrape_statcast_savant(start_date=new_date,
                                     end_date=new_date,
                                     player_type="pitcher") %>%
      summarise(pitch_type, game_date, release_speed, release_pos_x, release_pos_y,
                release_pos_z, player_name, batter, pitcher, events, description,
                spin_dir, zone, stand, p_throws, home_team, type,
                hit_location, bb_type, pfx_x, pfx_z, plate_x, plate_z,
                hc_x, hc_y, vx0, vy0, vz0, ax, ay, az, sz_top, sz_bot,
                release_pos_y, woba_value, pitch_name, spin_axis, delta_run_exp)
    year_data <- rbind(new_data, year_data)
  }
  return(year_data)
}
x2008 <- get_2008_data(2008)
write_csv(x2008, "x2008_pitches.csv")
get_grouped_pitch_data<-function(season) {
filename <- paste("x", season, "_pitches.csv", sep="")
data<-read_csv(filename)
widened_data<-pivot_wider(data %>%
                     group_by(player_name, pitcher, pitch_type) %>%
                     reframe(pitches=n(), velo=mean(release_speed)),
            names_from = pitch_type, 
            values_from = c(velo, pitches), 
            values_fill = 0)
return(widened_data)
}
grouped_pitch_data <- reduce(lapply(2008:2023, get_grouped_pitch_data), rbind)
