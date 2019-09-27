library(tidyverse)

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

ghanaclassroomsizes <- student_ratio %>%
  filter(country == "Ghana",
         !indicator %in% c("Secondary Education",
                          "Post-Secondary Non-Tertiary Education",
                          "Tertiary Education")) %>%
  mutate(student_ratio = round(student_ratio, 1),
         indicator = factor(indicator,
                     levels = c("Pre-Primary Education",
                                "Primary Education", 
                                "Lower Secondary Education",
                                "Upper Secondary Education")))

ghanaplot <- ghanaclassroomsizes %>%
  ggplot(aes(year, student_ratio, 
             label = student_ratio, 
             fill = indicator)) +
  geom_col() +
  facet_wrap(indicator ~ ., nrow = 2) +
  geom_text(vjust = 1.5, size = 3.5, color = "#ffffff", family = "Arial Narrow") +
  scale_y_continuous(limits = c(0, 40), breaks = NULL) +
  scale_x_discrete(limits = c(2012, 2018)) +
  theme_jdv() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#ededed"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Average Classroom Sizes in Ghana (2012-2018)",
       subtitle = "\nThe average classroom size in Ghana has been on the decline since 2012, except in the upper levels of secondary education.\n\n",
       caption = "Source: UNESCO | #TidyTuesday
       Design: Jenna DeVries | Twitter: @jennaldevries") +
  scale_fill_manual(values = c("#193544", "#2c4e60", "#33617a", "#4e768e"))

