library(tidyverse)


phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

glimpse(phd_field)
summary(phd_field)

phd_field$year <- as.numeric(phd_field$year)

#look at broad fields over the years 
broad_fields <- phd_field %>%
  group_by(broad_field, year) %>%
  summarize(total_phds = sum(n_phds, na.rm = TRUE))

broad_fields %>%
  filter(broad_field != "Other") %>%
  ggplot(aes(year, total_phds, fill = broad_field, color = broad_field)) +
  geom_area(alpha = .5) +
  theme_fivethirtyeight() +
  facet_wrap(broad_field ~ .) +
  xlab("Year (2008 - 2017)") +
  ylab("Total PhDs Awarded") +
  theme(legend.position = "None") +
  scale_x_continuous(breaks = seq(2008, 2017, by = 3)) +
  scale_y_continuous(breaks = seq(0, 25000, by = 5000)) +
  labs(title = "PhD's Awarded by Year", 
      caption = "ncses.nsf.gov\n@jennaldevries")
  
