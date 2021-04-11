# Load libraries
extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
library(maps)
library(ggfx)
library(ggalt)
library(ggforce)
#library(RColorBrewer)
library(magick)
library(patchwork)

font <- 'Bahnschrift'
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(135) # expand color palettes
mapcol <- c("#242424", "#B4B4B4", '#828282')
bgcol <- "#fcfcfc"
pathcol <- '#a8b8d1'
flightcol <- '#cccccc'

bird <- read.csv("birds_romania.csv")

world <- map_data(map='world') %>%
  filter(region %in% c('Romania', 'Hungary', 'Bulgaria', 'Serbia', 'Moldova'))

# Choose centroid for each country (for labelling)
regionmap1 <- world %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = max(lat)) %>%
  ungroup()

flight <- image_read("flight2.png") %>%
  image_background("none") %>%
  image_fill(flightcol, point = "+100+100", fuzz = 100)

bird <- bird %>%
  filter(lng!=0, lat!=0) %>%
  mutate(sp=factor(sp))



p1 <- ggplot() +
  
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill=mapcol[1], color=mapcol[1]) +
  geom_polygon(data=world %>% filter(region!='Romania'), aes(x=long, y=lat, group=group), fill=mapcol[2], color=mapcol[2]) +
  
  
  geom_text(data=regionmap1 %>% filter(region=='Romania'), aes(x=long, y=lat, label=region), color=mapcol[1], family=font) +
  with_bloom(
    geom_link2(data=bird, aes(x=lng, y=lat, group=1, alpha=stat(index/100000)), lineend = 'round', color=pathcol),
    threshold_lower = 50,
    threshold_upper = 100,
    sigma = 20,
    strength = 1,
    ) +
  
  # title
  as_reference(
  geom_text(aes(x=30, y=39, label=str_wrap('Migratory Bird Trips in & around Romania', 30)),
            size = 10, hjust=1, family=font, lineheight=0.7),
    id='title') +
  
  # bird image
  with_blend(
    annotation_raster(flight, xmin = 15, xmax = 25, ymin = 37, ymax = 40),
    bg_layer='title',
    blend_type="exclusion") +
  
  geom_text(aes(x=30, y=37, label=str_wrap('Visualizing the Flight Paths of 135 Bird Species', 100)),
            size = 5, hjust=1, family=font, color=pathcol, lineheight=0.4) +
  
  
  labs(caption='Data: Xeno-canto Foundation | #30DayChartChallenge | @saraahannes') +
  coord_cartesian(xlim=c(15,max(world$long)), ylim=c(33, max(world$lat))) +
  theme_void(base_family = font) +
  theme(
    legend.position = 'none',
    plot.background = element_rect(fill=bgcol, color=bgcol),
    plot.caption = element_text(family=font, hjust = 0.5, vjust=0.5, size=8, color=mapcol[2], margin = margin(-3, 0, 0.3, 0, "cm"))
  )


# Save plot
ggsave("08_animal.png", plot = p1, type = 'cairo', width = 5, height = 5, dpi = 300, units = "in", bg=bgcol)