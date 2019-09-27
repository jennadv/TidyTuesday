library(tidyverse)
library(janitor)
library(cowplot)

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

#Clean names, season/episode columns 
bob_ross <- bob_ross %>% 
  clean_names(case = "upper_camel") %>% 
  mutate(Title = str_to_title(Title)) %>%
  separate(Episode, into = c("Season", "Episode"), sep = "E") %>%
  mutate(Season = str_extract(Season, "[:digit:]+")) %>% 
  mutate_at(vars(Season, Episode), as.integer)

#Redundant columns
bob_ross <- bob_ross %>%
  mutate(Clouds = ifelse(Cirrus == 1, 1, 
                  ifelse(Cumulus == 1, 1, Clouds))) %>%
  mutate(Lake = ifelse(Lakes == 1, 1, Lake)) %>%
  mutate(Mountain = ifelse(Mountains == 1, 1, 
                    ifelse(SnowyMountain == 1, 1, Mountain))) %>%
  mutate(Tree = ifelse(Trees == 1, 1, 
                ifelse(Conifer == 1, 1,
                ifelse(Deciduous == 1, 1,
                ifelse(PalmTrees == 1, 1, Tree))))) %>%
  select(-Lakes, -Mountains, -SnowyMountain, -Trees)


# Trees plot --------------------------------------------------------------
#Plotting data
trees <- bob_ross %>%
  pivot_longer(4:66, names_to = "Element", values_to = "Indicator") %>%
  group_by(Season, Element) %>%
  summarize(TotalPaintings = sum(Indicator)) %>%
  filter(Element %in% c("Tree", "Conifer", "Deciduous", "PalmTrees"))

p1 <- trees %>%
  filter(Element == "Tree") %>%
  ggplot(aes(Season, TotalPaintings)) +
  geom_area(fill = "lightgrey", alpha = .6) +
  geom_line(size = 1.5, color = "darkgreen") +
  scale_y_continuous(limits = c(0, 15)) +
  scale_x_continuous(limits = c(1, 30)) +
  theme_jdv() +
  theme(axis.title = element_blank()) +
  ggtitle("Happy Trees",
          subtitle = "Number of paintings including trees over 30 seasons of Bob Ross.
He painted more conifer, less deciduous, and only a few palm trees over time.\n\n")

treelabels <- c("Conifer" = "Conifer\n",
                "Deciduous" = "Deciduous\n",
                "PalmTrees" = "Palm\n")

p2 <- trees %>%
  filter(Element != "Tree") %>%
  ggplot(aes(Season, TotalPaintings)) +
  facet_wrap(. ~ Element, labeller = as_labeller(treelabels)) +
  geom_area(fill = "lightgrey", alpha = .6) +
  geom_line(size = 1, color = "darkgreen", alpha = .4) +
  scale_y_continuous(limits = c(0, 15)) +
  scale_x_continuous(limits = c(1, 30)) +
  theme_jdv() +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(caption = "\nSource: FiveThirtyEight | #TidyTuesday
                  Design: Jenna DeVries | Twitter: @jennaldevries")

treeplots <- plot_grid(p1, p2, ncol = 1)


# Clouds plot -------------------------------------------------------------
#Plotting data
clouds <- bob_ross %>%
  pivot_longer(4:66, names_to = "Element", values_to = "Indicator") %>%
  group_by(Season, Element) %>%
  summarize(TotalPaintings = sum(Indicator)) %>%
  filter(Element %in% c("Clouds", "Cirrus", "Cumulus"))

p1 <- clouds %>%
  filter(Element == "Clouds") %>%
  ggplot(aes(Season, TotalPaintings)) +
  geom_area(fill = "lightgrey", alpha = .6) +
  geom_line(size = 1.5, color = "#53789e") +
  scale_y_continuous(limits = c(0, 15)) +
  scale_x_continuous(limits = c(1, 30)) +
  theme_jdv() +
  theme(axis.title = element_blank()) +
  ggtitle("Fluffy Clouds",
          subtitle = "Number of paintings including clouds over 30 seasons of Bob Ross.
He painted more cumulus than cirrus clouds, but favored clearer skies over time.\n\n")

cloudlabels <- c("Cirrus" = "Cirrus\n",
                "Cumulus" = "Cumulus\n")

p2 <- clouds %>%
  filter(Element != "Clouds") %>%
  ggplot(aes(Season, TotalPaintings)) +
  facet_wrap(. ~ Element, labeller = as_labeller(cloudlabels)) +
  geom_area(fill = "lightgrey", alpha = .6) +
  geom_line(size = 1, color = "#53789e", alpha = .4) +
  scale_y_continuous(limits = c(0, 15)) +
  scale_x_continuous(limits = c(1, 30)) +
  theme_jdv() +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(caption = "\nSource: FiveThirtyEight | #TidyTuesday
       Design: Jenna DeVries | Twitter: @jennaldevries")

cloudplots <- plot_grid(p1, p2, ncol = 1)
