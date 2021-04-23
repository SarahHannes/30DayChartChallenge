#-----
# Day 03 #30DayChartChallenge
# Author: Sarah H
# Date: 4 Apr 2021
#-----

# Load libraries
extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
library(ggridges)
library(ggstream)

# Load data
# Data courtesy of WHO and Szamil
# obtained from : https://www.kaggle.com/szamil/who-suicide-statistics
who <- read.csv("https://raw.githubusercontent.com/SarahHannes/30DayChartChallenge/main/Day03_history/who_suicide_statistics.csv")

# Preprocessing
all <- who %>% 
  group_by(age, year) %>%
  mutate(total = sum(suicides_no, na.rm=T)) %>%
  summarise(suicides_no, total) %>%
  slice(1L) %>%
  ungroup()

# Plot
p1 <- ggplot(data=all, aes(x=year, y=total, fill=age)) +
  geom_stream() +
  geom_stream_label(aes(label = age), family='Bahnschrift', size=3, color='gainsboro') +
  scale_fill_brewer(palette = "Dark2") +
  theme_ridges() +
  theme(
    legend.position = 'none',
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill='#F4EEED', color='#F4EEED'),
    panel.background = element_rect(fill='#F4EEED', color='#F4EEED'),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=6, color='darkgray')
  )

p <- p1 + 
  labs(title=str_wrap("Historical View on the World's Suicide Occurrences by Age Group (1980 ~ 2016)", 50),
       subtitle='Data: World Health Organization',
       caption = "#30DayChartChallenge | @saraahannes") +
  theme(
    plot.title = element_text(family='Bahnschrift', color='darkgray', size=12.3),
    plot.subtitle = element_text(family='IBahnschrift', color='darkgray', size=8, vjust=0),
    plot.caption = element_text(family='Bahnschrift', hjust = 0.5, vjust = 0.5, size=4, color='darkgray')
  )

# Save plot
ggsave("03_history.png", plot = p, type = 'cairo', width = 5, height = 3, dpi = 500, units = "in", bg='#fffeea')


#------- Additional graph (ordered age group by ascending total suicides no -----------------------

p2 <- ggplot(data=all, aes(x=year, y=total, fill=reorder(age, total))) +
  geom_stream() +
  geom_stream_label(aes(label = age), family='Bahnschrift', size=3, color='gainsboro') +
  scale_fill_brewer(palette = "Dark2") +
  theme_ridges() +
  theme(
    legend.position = 'none',
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill='#F4EEED', color='#F4EEED'),
    panel.background = element_rect(fill='#F4EEED', color='#F4EEED'),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=6, color='darkgray')
  )


p_additional <- p2 + 
  labs(title=str_wrap("Historical View on the World's Suicide Occurrences by Age Group (1980 ~ 2016)", 50),
       subtitle='Data: World Health Organization',
       caption = "#30DayChartChallenge | @saraahannes") +
  theme(
    plot.title = element_text(family='Bahnschrift', color='darkgray', size=12.3),
    plot.subtitle = element_text(family='IBahnschrift', color='darkgray', size=8, vjust=0),
    plot.caption = element_text(family='Bahnschrift', hjust = 0.5, vjust = 0.5, size=4, color='darkgray')
  )

# Save plot
ggsave("03_history_reorderedAge.png", plot = p_additional, type = 'cairo', width = 5, height = 3, dpi = 500, units = "in", bg='#fffeea')
