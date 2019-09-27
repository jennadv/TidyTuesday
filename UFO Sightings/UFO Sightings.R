library(tidyverse)
library(lubridate)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

ufo <- ufo_sightings %>%
  mutate(date_time = mdy_hm(date_time),
         month = month(date_time), 
         day = day(date_time), 
         year = year(date_time),
         hour = hour(date_time))

holiday <- ufo %>%
  filter(country == "us", 
         month == 7, 
         between(day, 3, 5),
         year > 1999) %>%
  group_by(year, day, hour) %>%
  summarize(sightings = n())

datelabels <- c('3'="July 3rd\n",
                '4'="July 4th\n",
                '5'="July 5th\n")

holiday %>%
  ggplot(aes(hour, sightings, fill = factor(day))) +
  geom_histogram(stat = "identity", binwidth = 1) +
  facet_wrap(. ~ day, nrow = 1, labeller = as_labeller(datelabels)) +
  ggtitle("UFO Sightings on the Fourth of July (2000-2013)",
          subtitle = "Since the millennium, Americans have reported the most UFO sightings on the night of Independence Day.\n") +
  labs(caption = "Source: National UFO Reporting Center | #TidyTuesday
                  Design: Jenna DeVries | Twitter: @jennaldevries") +
  scale_x_continuous("Hour\n", limits = c(0, 24),
                     seq.int(0, 24, 4)) +
  scale_y_continuous("Sightings",limits = c(0, 300)) +
  scale_fill_manual(values = c("#a4b6c1", "#2e598f", "#a4b6c1")) +
  theme_jdv() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "#ededed"))






#817 between 20 and 24, compared to 206 on the third, and 116 on the 5th. 
#overall avg 144
#72 overall avg from 8pm to midnight 





# 
# holiday %>%
#   ggplot(aes(hour, sightings)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(year ~ day, nrow = 5) +
#   theme_jdv()

# 
# library(gganimate)
# 
# holiday %>%
#   ggplot(aes(hour, sightings)) +
#   geom_histogram(stat = "identity") +
#   theme_jdv() +
# transition_states(
#     day,
#     transition_length = 2,
#     state_length = 1) +
#   enter_fade() + 
#   exit_shrink() +
#   ease_aes('sine-in-out')


# holiday %>%
#   ggplot(aes(hour, day, fill = sightings)) +
#   geom_tile(color = "white") +
#   theme_jdv()


# holiday %>%
#   ggplot(aes(hour, sightings)) +
#   geom_histogram(stat = "identity") +
#   facet_wrap(year ~ day, nrow = 5) +
#   theme_jdv()
