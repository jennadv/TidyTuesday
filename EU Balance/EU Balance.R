library(tidyverse)
library(RColorBrewer)
library(scales)


eu_balance <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/eu_balance.csv")

glimpse(eu_balance)
eu_balance$account_type <- as.factor(eu_balance$account_type)


pdat <- eu_balance %>%
  mutate(country = factor(ifelse(country %in% c("Germany", "Greece", "Netherlands", "Spain"), 
                          country, "Others"), 
                          levels = c("Others", "Greece", "Netherlands", "Spain", "Germany"))) %>%
  mutate(account_type = factor(ifelse(account_type == "budget", "Budget Balance",
                               ifelse(account_type == "current", "Current-Account Balance", NA))))
 

pdat %>%
  ggplot(aes(year, value, fill = factor(country))) +
  geom_bar(position = "stack", stat = "identity") +
  geom_hline(yintercept = 0) +
  facet_grid(. ~ account_type) +
  theme_jdv() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.x = element_blank()) +
  guides(fill = guide_legend(label.position = "bottom",
                             reverse = T)) + 
  scale_fill_manual(values = c("#D7D9DD", "#EE7769", "#54B195", "#FFCD66", "#3E5E99")) +
  scale_y_continuous(name = "Euros (in billions)", 
                     label = unit_format(unit = "", scale = 1e-3)) +
  labs(title = "EU Balances", 
       subtitle = "In August 2016, Germany saw large surpluses for the third year in a row.",
       caption = "Source: Eurostat | The Economist
       Design: Jenna DeVries | Twitter: @jennaldevries")
