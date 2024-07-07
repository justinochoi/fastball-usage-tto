library(tidyverse)

# Read in data, remove bat tracking stuff
data <- read.csv(file.choose()) 
data <- data %>% select(-bat_speed, -swing_length) 
pitchers <- read.csv(file.choose()) 

# We want to consider starting pitchers only 
starters <- 
  pitchers %>% 
  filter(IP / GS >= 3 & GS >= 1 & GS == G) %>% 
  select(MLBAMID) 

# This will be our main dataframe 
starter_data <- inner_join(data, starters, by = c('pitcher' = 'MLBAMID')) 

# Indicate how many times pitcher has faced batter in outing  
# Can't think of a better, cleaner way to do this, but it works 
# Note that I'm ignoring four or more times through the order 
starter_data <- 
  starter_data %>%
  group_by(game_date, pitcher, batter) %>%
  arrange(inning) %>% 
  filter(n_distinct(inning) < 4) %>%
  mutate(times_faced = case_when(
    n_distinct(inning) == 1 ~ 1, 
    n_distinct(inning) == 2 ~ case_when(inning == min(inning) ~ 1, 
                                        .default = 2), 
    .default = case_when(inning == min(inning) ~ 1, 
                         inning == max(inning) ~ 3, 
                         .default = 2)
    ) 
  )

# Define whether a pitch is a primary fastball
starter_data <- 
  starter_data %>% 
  group_by(pitcher, pitch_type) %>% 
  mutate(pitch_count = n()) %>% 
  ungroup() %>% 
  group_by(pitcher) %>% 
  mutate(total_count = n()) %>% 
  ungroup() %>% 
  mutate(
    usage_rate = round(pitch_count / total_count, 3), 
    primary_flag = case_when(pitch_type %in% c('FF','SI','FC') & 
                             usage_rate > 1/3 ~ 'Y', .default = 'N') 
  ) 

# Find primary fastball usage and wOBA allowed for each pitcher 
usage_data <- 
  starter_data %>%
  group_by(pitcher, times_faced) %>% 
  summarize(
    total_pitches = n(), 
    primary_count = sum(primary_flag == 'Y'), 
    mean_woba = round(mean(woba_value, na.rm = T), 3), 
    primary_usage = round(primary_count / total_pitches, 3) * 100
    ) %>% 
  filter(primary_usage > 0) 

# Calculate pitch usage variance for each pitcher 
# Measure of how pitchers adapt to each time through the order 
usage_data <- 
  usage_data %>% 
  group_by(pitcher) %>% 
  mutate(
    primary_usage_var = round(var(primary_usage), 3), 
    var_quantile = case_when(
      primary_usage_var > quantile(usage_data$primary_usage_var, 0.66) ~ 'High',
      primary_usage_var < quantile(usage_data$primary_usage_var, 0.33) ~ 'Low',
      .default = 'Moderate')
  )

library(Hmisc)
 
# Summarize results by time through the order and variance quantile 
# Results are weighted by each pitcher's number of pitches 
result <- 
  usage_data %>%
  group_by(times_faced, var_quantile) %>% 
  dplyr::summarize(
    pitchers = n(), 
    woba = weighted.mean(mean_woba, total_pitches), 
    sd = sqrt(wtd.var(mean_woba, total_pitches)), 
    lower95 = woba - qt(0.975, pitchers-1)*sd/sqrt(pitchers), 
    upper95 = woba + qt(0.975, pitchers-1)*sd/sqrt(pitchers)
  )

# League-wide primary fastball usage by time through order 
league <- 
  usage_data %>% 
  group_by(times_faced) %>% 
  summarize(primary_usage = weighted.mean(primary_usage, total_pitches)) 

# For graphing purposes 
result$times_faced <- as.factor(result$times_faced) 
result$diff_group <- as.factor(result$var_quantile) 
league$times_faced<- as.factor(league$times_faced)
usage_data$times_faced <- as.factor(usage_data$times_faced) 

library(scales) 

# Visualization 
result %>% 
  ggplot(aes(x=times_faced, y=woba, color=var_quantile, group=var_quantile)) + 
  geom_errorbar(aes(ymin = lower95, ymax = upper95), alpha = 0.5, width = 0.1) +
  geom_line() + 
  geom_point() +
  theme_bw() +
  scale_y_continuous(breaks = breaks_pretty(n=5)) +
  labs(x="Times Faced", 
       y="Mean wOBA", 
       title="Primary Fastball Usage Variance vs. TTO Penalty", 
       subtitle="2023, SP only w/ 95% CI", 
       color="Usage Variance") + 
  theme(plot.title = element_text(face = "bold")) 
  
league %>%
  ggplot(aes(x=times_faced, y=primary_usage, group=1)) + 
  geom_line() + 
  geom_point() + 
  theme_bw() + 
  labs(x="Times Faced", 
       y="Primary FB Usage (%)")

usage_data %>% 
  ggplot(aes(x=primary_usage_var)) + 
  geom_histogram() + 
  geom_vline(xintercept = median(usage_data$primary_usage_var), linetype="dashed",
             color="red") + 
  theme_bw() + 
  labs(x="Usage Variance",  
       y="Count") 
  
  
