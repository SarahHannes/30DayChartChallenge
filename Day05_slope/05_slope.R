#-----
# Day 05 #30DayChartChallenge
# Author: Sarah H
# Date: 6 Apr 2021
#-----

# Load libraries
extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
library(ggrepel)
library(ggfx)
library(ggtext)
library(showtext)
showtext_auto()
font_add_google("Signika", "Signika")
font_add_google("Roboto", "roboto")

# Load data and preprocessing
# Data courtesy of Sustainable Development Solutions Network
# Obtained from https://www.kaggle.com/unsdsn/world-happiness?select=2015.csv
df1 <- read.csv("https://raw.githubusercontent.com/SarahHannes/30DayChartChallenge/main/Day05_slope/2015.csv") %>%
  janitor::clean_names() %>%
  mutate(rank_2015 = happiness_rank, family_2015=family, year=2015) %>%
  summarise(country, rank_2015, family_2015, year)

df2 <- read.csv("https://github.com/SarahHannes/30DayChartChallenge/blob/main/Day05_slope/2019.csv") %>%
  janitor::clean_names() %>%
  mutate(country = country_or_region, rank_2019 = overall_rank, family_2019 = social_support, year=2019) %>%
  summarise(country, rank_2019, family_2019, year)

happiness <- left_join(x=df2, y=df1, by='country') %>%
  janitor::clean_names() %>%
  mutate(family_diff=family_2019-family_2015,
         rank_diff=rank_2019 - rank_2015) %>%
  drop_na()

top5col <- c("#8dbfa4", "#acccd4")
bgcol <- "#fafafc"

# Plot
p1 <- ggplot() +
  # All lines
  geom_segment(data=happiness, aes(x=1, xend=3, y=rank_2015, yend=rank_2019), color='gainsboro', alpha=0.35) +
  
  # Top 5 countries
  geom_segment(data=happiness %>%
              top_n(-5,row_number(rank_diff)),
            aes(x=1, xend=3, y=rank_2015, yend=rank_2019),
            color=top5col[1]) +
  
  # Annotations
  geom_text_repel(data=happiness %>%
              top_n(-5,row_number(rank_diff)),
            aes(x=0.9, y=rank_2015, label=country),
            hjust=1, size=10, direction='y', force=0.1, family='roboto', color=top5col[2]
            ) +
  geom_text_repel(data=happiness %>%
              top_n(-5,row_number(rank_diff)),
            aes(x=3.1, y=rank_2019, label=country),
            hjust=0, size=10, direction='y', force=0.1, family='roboto', color=top5col[2]
            ) +
  
  # Points
  geom_point(data=happiness %>%
              top_n(-5,row_number(rank_diff)),
            aes(x=1, y=rank_2015),
            size=2, color=top5col[1]
            ) +
  geom_point(data=happiness %>%
              top_n(-5,row_number(rank_diff)),
            aes(x=3, y=rank_2019),
            size=2, color=top5col[1]
            ) +
  
  # Title
  geom_richtext(aes(x=0.4, y=165, 
                label="<span style='color:#a8c4cc'>TOP 5</span>
                <span style='color:#e7bf1c'>INCREASED</span>
                <span style='color:#a8c4cc'>IN<br>WORLD</span>
                <span style='color:#e7bf1c'>HAPPINESS</span>
                <span style='color:#a8c4cc'>RANKING</span>"), 
            angle=90, vjust=0, hjust=0, size=30, lineheight=0.35, family="Signika", fontface='bold', fill='transparent', color='transparent') +
  
  # X Axis Labels
  geom_text(aes(x=0.9, y=-3, label='2015'), hjust=1, size=25, family='roboto', color='#a8c4cc') +
  geom_text(aes(x=3.1, y=-3, label='2019'), hjust=0, size=25, family='roboto', color='#a8c4cc') +

  scale_y_reverse(limits = c(170, -3)) + # reverse y axis to start from 0 from top
  coord_cartesian(xlim=c(0, 4)) +
  labs(caption=" Data: Sustainable Development Solutions Network | #30DayChartChallenge | @saraahannes " ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill=bgcol, color=bgcol),
    plot.caption = element_text(family='roboto', hjust = 0.5, vjust = 0.5, size=28, color="darkgray", margin = margin(0, 0, 0.3, 0, "cm"))
  )


# Save plot
ggsave("05_slope.png", plot = p1, type = 'cairo', width = 8, height = 6, dpi = 300, units = "in", bg =bgcol)
