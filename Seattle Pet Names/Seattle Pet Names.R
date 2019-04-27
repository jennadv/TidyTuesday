library(tidyverse)

seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")


glimpse(seattle_pets)
summary(seattle_pets)

sibcats <- seattle_pets %>%
  filter(species == "Cat", primary_breed == "Siberian") %>%
  group_by(animals_name) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

seattle_pets <- seattle_pets %>%
  separate(license_issue_date, into = c("Day", "Month", "Year"), sep = " ")

catbreeds <- seattle_pets %>%
  filter(species == "Cat") %>%
  group_by(primary_breed) %>%
  count() %>%
  arrange(desc(`n`))



