library(tidyverse)
library(sf)
library(osmdata)
library(ggmap)
library(ggtext)

#Read data
measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

# Streetmap ---------------------------------------------------------------

#Larger streets
chihighway <- opq(bbox = 'chicago') %>%
  add_osm_feature(key = "highway", 
  value = c("motorway", "primary"))
  
chihighway <- osmdata_sf(chihighway)

#Smaller streets
chistreets <- opq(bbox = 'chicago') %>%
  add_osm_feature(key = "highway", 
  value = c("secondary", "tertiary"))

chistreets <- osmdata_sf(chistreets)

#Coordinates
longitude <- c(-87.940101, -87.5240812)
latitude <- c(41.644531, 42.0230396)


# Chicago Data Setup ------------------------------------------------------

chi <- measles %>%
  group_by(state, city, enroll, mmr, lng, lat) %>%
  filter(city == "Chicago",
         between(lat, latitude[1], latitude[2]),
         between(lng, longitude[1], longitude[2])) %>%
  mutate(below95 = ifelse(mmr < 95, "Below 95", "95 or Above")) %>%
  unique()

below <- chi %>%
  filter(below95 == "Below 95")

above <- chi %>%
  filter(below95 == "95 or Above")


# Theme -------------------------------------------------------------------



theme_jdv_dark_void <- function(base_size = 11, 
                                base_family = "Arial Narrow") {
    theme_void(base_size = base_size,
             base_family = base_family) %+replace%
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#1c1c1c"),  #Was grey20 
          plot.title = element_text(size = 19, 
                                    face = 'bold', 
                                    hjust = 0, 
                                    vjust = 3, 
                                    color = "light grey"), 
          plot.margin = margin(.8, 4, .5, 1, 
                               unit = "cm"),
          plot.subtitle = element_text(hjust = 0, 
                                       vjust = 2, 
                                       color = "light grey"),
          plot.caption = element_markdown(hjust = .7, 
                                          size = 8,
                                          color = "light grey"),
          plot.tag.position = c(.95, .7), 
          plot.tag = element_markdown(size = 10, 
                                      color = "light grey"),
          strip.text = element_text(color = "#FFFFFF", 
                                    face = "bold", 
                                    size = 11),
          strip.background = element_rect(size = 8),
          legend.title = element_blank(), 
          legend.text = element_blank(),
          legend.position = "none",
          complete = FALSE,
          validate = TRUE)
}



# Plot --------------------------------------------------------------------


ggplot() +
  geom_sf(data = chihighway$osm_lines, 
          color = "#c2c2c2") +
  geom_sf(data = chistreets$osm_lines, 
          color = "#4f4f4f",
          size = .2) +
  geom_point(data = above,
             aes(x = lng, y = lat,
                 size = enroll),
             alpha = .3,
             color = "light grey",
             stroke = 0) +
  geom_point(data = below,
             aes(x = lng, y = lat,
                 size = enroll),
             alpha = .5,
             color = "red",
             stroke = 0) +
  theme_jdv_dark_void() +
  
#Annotate!
  
        labs(tag = "<span style='font-size:24pt'><b>Measles Vaccinations<br> 
in Chicago Schools</b></span></span><br><br>
To achieve herd immunity and <br>
stop the spread of measles, the CDC<br>
maintains that a 95% vaccination<br>
rate is the threshold that must be met.<br><br>
In the 2018-19 school year, <br>
there were at least 100 Chicago <br>
schools with rates <span style='color:#e3242b'><b>below 95%.</b></span> ",
             caption = "<br><b>Sources:</b> The Wall Street Journal, OpenStreetMap, CDC &#9679;
<b>Design:</b> Jenna DeVries &#9679; <b>Twitter:</b> @jennaldevries &#9679;
Created for #TidyTuesday")
 
  

ggsave("Chicago MMR.png", height = 11, width = 10, units = "in")
  