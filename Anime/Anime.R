library(tidyverse)
library(lubridate)
library(ggridges)

dat <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

anime <- dat %>%
  mutate(startyear = year(floor_date(start_date))) %>%
  filter(startyear >= 2008 & startyear <= 2018,
         scored_by >= 500,
         genre == "Sci-Fi") %>%
  mutate(startyear = fct_rev(as.factor(startyear)))

anime %>%
  ggplot(aes(score, startyear)) +
  geom_density_ridges(quantile_lines = TRUE,
                      quantiles = 2,
                      fill = "#23696b",
                      color = "grey92") +
  scale_x_continuous("Average User Score\n",
                     breaks = seq(0, 10, 1)) +
  scale_y_discrete("Anime Release Year") +
  theme_jdv() +
  theme(plot.background = element_rect(fill = "#dad2d8")) +
  labs(title = "Is Sci-Fi Anime Decreasing in Quality?",
       subtitle = "The average score for Sci-Fi anime by MyAnimeList users has decreased in the last 10 years.\n",
       caption = "Source: MyAnimeList | #TidyTuesday
       Design: Jenna DeVries | Twitter: @jennaldevries")
  
ggsave("SciFiAnimePlot.png")


# anime %>%
#   ggplot(aes(start_date, score)) +
#   geom_point(color = "#23696b", alpha = .5, size = 3) +
#   geom_smooth(method = "lm", color = "black", alpha = .5) +
#   theme_jdv() +
#   theme(plot.background = element_rect(fill = "#dad2d8")) +
#   scale_y_continuous("Average User Score\n",
#                      breaks = seq(0, 10, 1)) +
#   scale_x_discrete("Anime Start Date")
# 