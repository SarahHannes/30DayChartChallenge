# Load libraries
extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
library(zoo)
library(maps)
library(ggfx)

# Load data
# Data courtesy of National UFO Reporting Center (NUFORC)
# Obtained from https://www.kaggle.com/NUFORC/ufo-sightings
ufo <- read.csv("https://raw.githubusercontent.com/SarahHannes/30DayChartChallenge/main/Day04_magical/ufo.csv")

world <- map_data(map='world')

# Preprocessing
cufo <- ufo %>%
  mutate(datetime = as.Date(datetime, tryFormats = c("%m/%d/%Y")),
         latitude = as.double(latitude)) %>%
  filter(latitude!=0, longitude!=0)

# Plot
p1 <- ggplot() +
  # base map
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="#242424", color='#242424') +
  # make geom_points glow :)
  with_outer_glow(
    geom_point(data=cufo, aes(x=longitude, y=latitude), color='white', size=0.01, alpha=0.05, shape=16),
                  colour = '#efce5b',
                  sigma = 30,
                  expand = 3) +
  labs(title='Reported UFO Sightings 1949 ~ 2005',
    caption = "Data: National UFO Reporting Center | #30DayChartChallenge | @saraahannes") +
  coord_cartesian(expand = T) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="#0a080b", color="#0a080b"),
    plot.title=element_text(family="Bahnschrift Condensed", color='darkgray', size=40, face='bold', hjust=0.5, vjust=0.5),
    plot.caption = element_text(family='Bahnschrift Light', hjust = 0.5, vjust = 0.5, size=8, color='darkgray')
  )

# Save plot
ggsave('04_magical.png', plot=p1, type='cairo', width=7, height = 4, dpi=400, units='in', bg="#0a080b")
