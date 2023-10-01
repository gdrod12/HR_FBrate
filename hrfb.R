library(tidyverse)
options(scipen=9999)
data<-read_csv("hrflyball_starters.csv")
n_plus <- data %>%
  reframe(playerid=PlayerId, name=Name, nplus_ip=IP, nplus_hr_fb=`HR/FB`, season=Season+1)
data <- data %>%
  reframe(playerid=PlayerId, name=Name, ip=IP, hr_fb=`HR/FB`, season=Season)
data <- merge(data, n_plus, by=c("playerid", "name", "season"))
all_model <- lm(nplus_hr_fb~hr_fb, data=data)
summary(all_model)
data$low_sample_fitted <- all_model$fitted.values
filtered_data <- data %>%
  filter(ip>150, nplus_ip>150)

library(ggrepel)
ggplot() +
  ggtitle("Justin Verlander consistently allowed fewer home runs per fly ball",
          subtitle="HR/FB rate vs N+1 HR/FB rate, 2002-2022, min 150 innings, Verlander highlighted in red")+
  geom_point(data=filtered_data, aes(x=hr_fb, y=nplus_hr_fb)) +
  geom_smooth(data=filtered_data, aes(x=hr_fb, y=nplus_hr_fb), method="lm") +
  geom_point(data=verlander, aes(x=hr_fb, y=nplus_hr_fb), color="red", size=5) + 
  geom_label_repel(data=verlander, aes(x=hr_fb, y=nplus_hr_fb, label=season)) +
  xlab("Home Run per Fly Ball Rate") +
  ylab("Next Season Home Run per Fly Ball Rate") +
  geom_hline(yintercept=mean(filtered_data$nplus_hr_fb), lty="dashed") +
  geom_vline(xintercept=mean(filtered_data$hr_fb), lty="dashed")
verlander <- filtered_data %>%
  filter(name == "Justin Verlander")
filtered_model <- lm(nplus_hr_fb~hr_fb, data=filtered_data)
filtered_data$fitted <- filtered_model$fitted.values
summary(filtered_model)
detailed_data<-read_csv("detailed_data.csv")
career_data <- read_csv("career_data.csv")
career_data[4]
filtered_model$coefficients
r_values <- data.frame()

for (index in 4:30){
  print(career_data[index])
  model <- lm(unlist(career_data[5])~unlist(career_data[index]))
  new_data<-data.frame(variable=names(career_data[index]), 
                       r_squared = summary(model)$r.squared,
                       p_value = summary(model)$coefficients[-1, "Pr(>|t|)"],
                       r_value = cor(unlist(career_data[5]),unlist(career_data[index]),  use = "complete.obs"),
                       min = min(model$fitted.values),
                       max=max(model$fitted.values))
  r_values <- rbind(r_values, new_data)
}
career_data[index]
write_csv(r_values, "variable_r_values.csv")

