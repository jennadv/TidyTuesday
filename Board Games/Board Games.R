library(tidyverse)
library(ggthemes)

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

glimpse(board_games)

# 2 Player Games Play Times -------------------------------------

#2 player games with at least 100 ratings
twoplayer <- board_games %>%
  filter(min_players == 2 & max_players == 2 & users_rated > 100)

#create rating bins 
twoplayer <- twoplayer %>%
  mutate(rating_bins =  ifelse(average_rating < 6.0, "< 6.0",
                        ifelse(average_rating < 6.5, "6.0 - 6.5",
                        ifelse(average_rating < 7.0, "6.5 - 7.0",
                        ifelse(average_rating < 7.5, "7.0 - 7.5",
                        ifelse(average_rating >= 7.5, "7.5 and Above", "NA"))))))

#long playtimes?
twoplayer %>%
  filter(playing_time > 1000) %>%
  select(name, playing_time) %>%
  arrange(desc(playing_time))

#plot play time based on rating bins
twoplayer %>%
  ggplot(aes(rating_bins, playing_time, fill = rating_bins)) +
  geom_violin() +
  ylim(0, 600) +
  ggtitle("2 Player Board Games Play Time by Average User Rating") +
  ylab("Play Time") +
  xlab("Average Rating") +
  theme_minimal(base_family = "Helvetica") +
  theme(legend.position = "none",
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))

# for us ------------------------------------------------------------------

highestratedtwoplayer <- twoplayer %>%
  filter(rating_bins == "7.5 - 8.5", users_rated > 499) %>%
  select(name, year_published, average_rating, users_rated, description, category, playing_time) %>%
  arrange(desc(average_rating))

djlist <- highestratedtwoplayer %>%
  filter(name %in% c("7 Wonders Duel", "Hive Pocket", "YINSH")) %>%
  select(name, category, description)

# Play Times Over Time ----------------------------------------------------

#average play times by year
avgplaytime <- board_games %>%
  select(playing_time, year_published, name) %>%
  filter(playing_time <= 1000 & year_published >= "2001") %>%
  group_by(year_published) %>%
  summarize(avgplaying_time = mean(playing_time), num_games = n())

avgplaytime %>%
  ggplot(aes(year_published, avgplaying_time)) +
  ylim(0, 90) +
  geom_area() +
  transition_reveal(year_published)
