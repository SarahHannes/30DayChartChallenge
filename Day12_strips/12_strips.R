#-----
# Day 12 #30DayChartChallenge
# Author: Sarah H
# Date: 23 Apr 2021
#-----
# Heavily inspired by @flowingdata, @HannahMBuckland and other supeR users who kindly share their wonderful works! Thank you!

# Load libraries
extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(showtext)
showtext_auto()
font_add_google("Oswald", "Oswald")
font_add_google("Raleway", "Raleway")

# Load data
post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

# Fonts and palette
font <- c('Oswald', 'Raleway')
annoscol <- c('white', 'gainsboro', 'darkgray', 'black')
bgcol <- "#fcfcfc"
pal <- c('#636db5', '#91d9cd')

# Pre-processing
est <- post_offices %>%
  filter(gnis_feature_class=='Post Office', !id %in% c(128882, 145399, 145448), !is.na(established)) %>% #filter out NAs est years & 3 post offices which has slightly (maybe) weird est date (ie they were est. in the year of 186, 189, 192)
  group_by(established) %>%
  count() %>%
  ungroup()

# Title tibble
title <- tibble(
  label=c("<span style='color:darkgray'>
          BY YEAR</span>
          <br><span style='font-size:50pt; color:black'>
          **POST  OFFICES  ESTABLISHMENT  TIMELINE  ACROSS  U.S.**</span>
          <span style='color:darkgray'><br>& OTHER  NOTABLE  EVENTS</span>
          </style>
          "))

# Events annotation tibble
events <- c(
  `1918` = 'Took over airmail service from the U.S. Army Air Service',
  `1937` = 'Oversaw the shipment of gold to U.S. Bullion Depository',
  `1970` = 'U.S. Postal Strike',
  `1823` = 'First use of steamboats to deliver mails',
  `1832` = 'First use of railroad to deliver mails',
  `1896` = 'Rural Free Delivery Launched',
  `1914` = '1st (and last) shipment of a living child'
) %>% as.data.frame()

# Clean up events annotation tibble
events_cl <- events %>%
  mutate(year = row.names(events)) %>%
  janitor::clean_names() %>%
  mutate(text=x, year=as.numeric(as.character(year)))%>%
  summarise(year, text) %>%
  arrange(year) %>%
  mutate(position=ifelse(year<1897, 'left', 'right'))

# Plot
p1 <- ggplot() +
  # Title
  geom_richtext(data=title, aes(x=1900, y=2, label=label), hjust=0.5, family=font[1], lineheight=0.3,  color=NA, fill=NA, size=13) +
  
  # Strip
  geom_tile(data=est %>% filter(established>=1800), aes(x=established, y=0.5, fill=n)) +
  
  # X Axis Labels
  geom_text(data=est %>% mutate(century=plyr::round_any(established, 100, ceiling), decade5=plyr::round_any(established, 50, ceiling)) %>% mutate(label=ifelse(decade5==century, century, decade5)) %>% distinct(label) %>% filter(label %in% c(1650, 1850, 1950)),
            aes(x=label, y=1.3, label=label), size=15, family=font[2], color=annoscol[3]) +
  
  geom_text(data=est %>% mutate(century=plyr::round_any(established, 100, ceiling), decade5=plyr::round_any(established, 50, ceiling)) %>% mutate(label=ifelse(decade5==century, century, decade5)) %>% distinct(label) %>% filter(label %in% c(1800, 1900, 2000)),
            aes(x=label, y=1.5, label=label), size=15, family=font[2], color=annoscol[3]) +
  
  # X Axis Segment Lines
  geom_segment(aes(x=1900, xend=1900, y=1.43, yend=0), color=annoscol[2], linetype='dashed', size=0.3, alpha=0.5) +
  geom_segment(aes(x=c(1800, 2000), xend=c(1800, 2000), y=1.43, yend=1), color=annoscol[2], linetype='dashed', size=0.3, alpha=0.5) +
  geom_segment(aes(x=c(1650, 1850, 1950), xend=c(1650, 1850, 1950), y=1.23, yend=0), color=annoscol[2], linetype='dashed', size=0.3, alpha=0.5) +
  
  # Events annotations
  geom_text_repel(data=events_cl %>% filter(position=='left'), 
                  aes(x=year, y=0, label=paste0(year, "\n", str_wrap(text, 15))),
                  hjust=1,
                  vjust=0,
                  lineheight=0.3,
                  size=8, 
                  max.overlaps = Inf,
                  seed=1,
                  nudge_x = -1,
                  box.padding = 0.5,
                  nudge_y = -1,
                  segment.curvature = -0.025,
                  segment.ncp = 6,
                  segment.angle = 100,
                  direction='both',
                  color=annoscol[3],
                  segment.color=annoscol[3],
                  family=font[2]) +
  
  geom_text_repel(data=events_cl %>% filter(position=='right'), 
                  aes(x=year, y=0, label=paste0(year, "\n",str_wrap(text, 15))),
                  hjust=0,
                  vjust=0,
                  lineheight=0.3,
                  size=8, 
                  max.overlaps = Inf,
                  seed=0,
                  nudge_x = 10,
                  box.padding = 0.5,
                  nudge_y = -1,
                  segment.curvature = 0.025,
                  segment.ncp = 6,
                  segment.angle = 100,
                  direction='both',
                  color=annoscol[3],
                  segment.color=annoscol[3],
                  family=font[2]) +
   
  # Annotation for blanks
  geom_text(aes(x=1998, y=-0.1, label='*'), size=15, color=annoscol[4]) +
  
  scale_fill_gradientn(colours=pal) +  
  xlim(1790, 2010) + # used xlim instead of coord_cartesian so that the strip is centered
  coord_cartesian(ylim=c(-1,2.3), expand = F, clip = 'off') +
  labs(
    caption="<span style='font-size:35pt; color:black'>(*) Blank bands depict years without data</span>
    <br>Data: Cameron Blevins & Richard W. Helbock | Text: Wikipedia | @saraahannes"
  ) +
  theme_void(base_family = font[2], base_size = 20) +
  guides(
    fill = guide_colourbar(
      barwidth=8, 
      barheight=0.8,
      legend.spacing.y = unit(0.3,"cm"),
      frame_color = annoscol[2],
      ticks=T,
      ticks.colour = annoscol[1],
      linewidth=0.2,
      lineheight=0.1,
      title=str_wrap('# OF POST OFFICES ESTABLISHED', 25),
      title.hjust=1,
      title.vjust=0.5
    )
  ) +
  theme(
    legend.position='bottom',
    legend.title = element_text(size=25, family = font[2], lineheight=0.3, color=annoscol[3]),
    legend.text = element_text(size=25, family = font[2], color=annoscol[3]),
    legend.box.background = element_rect(fill = bgcol, colour = bgcol),
    plot.background = element_rect(fill=bgcol, color=bgcol),
    plot.caption = element_markdown(hjust = 1, vjust = 1, size=28, color=annoscol[3], margin = margin(0.3, 0.3, 0.3, 0, "cm"), family=font[2], lineheight=0.3)
  )

# Save plot
ggsave("12_strips.png", plot = p1, type = 'cairo', width = 9, height = 6, dpi = 300, units = "in", bg =bgcol)
