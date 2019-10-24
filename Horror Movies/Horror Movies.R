library(tidyverse)
library(beeswarm)

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

horror_movies$genres <- gsub("\\| ", " \\| ", horror_movies$genres)

top5 <- horror_movies %>%
  group_by(genres) %>%
  tally() %>%
  top_n(5, n)

plot <- horror_movies %>%
  filter(release_country == "USA",
         !is.na(review_rating),
         genres %in% top5$genres) %>%
  group_by(genres, review_rating) %>%
  tally()
  



facet_wrap(genres)


#density plot where the bottom is average rating. maybe bee plot? 
#facet by genres 
#top genres - horror, horror thriller, comedy horror, horror mystery thriller, and drama horror thriller. next is horror scifi thriller.