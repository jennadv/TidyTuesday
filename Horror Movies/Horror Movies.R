library(tidyverse)
library(ggbeeswarm)

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

#Data for average ratings by budget
plot <- horror_movies %>%
  filter(!is.na(review_rating),
         !is.na(budget), #Only keep those with budget data,
         grepl("\\$", budget), #In U.S. dollars
         release_country == "USA") %>%
  mutate(budget = gsub("\\$", "", budget), #Converting dollars to number for plotting
         budget = as.numeric(gsub("\\,", "", budget)), 
         budgetgrp = as.numeric(cut_number(budget, 6)), #Cutting to help define groups
         budgetcat = ifelse(budget > 5000000, "Over $5m",
                                   ifelse(budget > 1000000, "$1m-$5m",
                                          ifelse(budget > 350000, "$350k-$1m",
                                                 ifelse(budget > 60000, "$60k-$350k",
                                                        ifelse(budget > 10000, "$10k-$60k", "$10k and Under"))))),
         budgetcat = factor(budgetcat, levels = c("$10k and Under", 
                                                  "$10k-$60k", 
                                                  "$60k-$350k", 
                                                  "$350k-$1m", 
                                                  "$1m-$5m", 
                                                  "Over $5m")))
                                                 
#For setting an average line/label
avgrating <- round(mean(plot$review_rating), 1)             

#Plotting average ratings by budget category
plot %>%
  ggplot(aes(budgetcat, review_rating)) +
  geom_quasirandom(color = "#962e1a", size = 2) +
  geom_hline(aes(yintercept = avgrating), alpha = .5, size = 1.5) +
  scale_y_continuous(breaks = c(2, 6, 10)) +
  theme_jdv_dark() +
  theme(legend.position = "none") +
  
#Set labels
  labs(title = "Horror Film Ratings by Budget (2012-2017)",
       caption = 
       "\nSource: IMDB (2017)
       Design: Jenna DeVries | Twitter: @jennaldevries
       Created for #TidyTuesday") +
  ylab("IMDB Rating") +
  xlab("Budget Category") +
  
#Annotate!
  annotate("text", .55, 11, label = "Super low-budget horror films have a larger variety 
of average ratings, which are also less reliable 
due to smaller numbers of reviews. Regardless, 
they are above average surprisingly often.", 
           color = "grey", 
           size = 2.7, 
           hjust = 0) +
  
  annotate("rect", 
           xmin = .5, 
           xmax = 2.4, 
           ymin = 10, 
           ymax = 12, 
           fill = "grey", 
           alpha = .1) +
  
  annotate("text", 4.55, 11.5, label = "Horror films with higher budgets are more likely to 
receive higher ratings. With a budget of over $5m, 
it is more common that a film will receive above 
average reviews (e.g. World War Z, The Conjuring).
Exceptions include Scary Movie 5 and Tyler Perry's
Boo! A Madea Halloween (along with it's sequel).", 
           color = "grey", 
           size = 2.7, 
           hjust = 0) +
  
  annotate("rect", 
           xmin = 4.5, 
           xmax = 6.4, 
           ymin = 10, 
           ymax = 13, 
           fill = "grey", 
           alpha = .1) +
  
  annotate("text", 0, avgrating + .3, label = paste("Average: ", avgrating), 
           color = "grey", 
           size = 2.7, 
           hjust = 0) 


ggsave("Horror Film Ratings.png", height = 6, width = 10, units = "in")
