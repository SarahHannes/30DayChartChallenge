extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
#remotes::install_github("hrbrmstr/waffle")
library(waffle)
#library(hrbrthemes)
#library(extrafont)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)
library(ragg)
library(magick)
library(patchwork)

stain <- image_read("coffee-stain.png") %>%
  image_transparent(color='red') %>%
  image_background("none") %>%
  image_rotate(-15) %>%
  image_resize("150x150")

p1 <- ggplot() +
  geom_text(data=NULL, aes(x=1, y=-0.5, label='Total espresso shots pulled\nsince Movement Control Order'), lineheight=0.7, hjust=0, vjust=0.5, size=5, family='Iced Tea') +
  #geom_text(data=NULL, aes(x=5, y=-0.7, label='1 espresso shot'), family='Iced Tea', size=3) +
  #geom_curve(data=NULL, aes(x=4, xend=5, y=-1, yend=-0.73),  arrow = arrow(length = unit(0.2, "cm"), type='closed'), color='#5c5351', curvature = -0.2) +
  
  scale_y_continuous(limits =c(-1,0), expand = c(0,0)) +
  scale_x_continuous(limits=c(0,9), expand=c(0,0)) +
  theme_pomological(base_theme = theme_void()) +
  theme(
    legend.position='none',
    #axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    #plot.margin = unit(c(0,-4,-4,-1), "cm")
  )

p2 <- ggplot() +
  geom_pictogram(data=NULL , aes(label="espresso", values=61), flip=T, color='#ee9c6a', n_rows = 8, make_proportional = T, size=5, family = "Font Awesome 5 Free Solid") + 
  annotation_raster(stain, xmin = 8.5, xmax = 12, ymin = 0, ymax = 5.5) +
  scale_label_pictogram(values=c('mug-hot')) +
  coord_equal() +
  theme_pomological(base_theme = theme_void()) +
  theme(
    legend.position='none',
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()
  )


p <- p2 + 
  labs(title='Total espresso shots pulled\nsince Movement Control Order',
       subtitle='hint: 61 cups',
       caption = "#30DayChartChallenge   |   @saraahannes\n") +
  theme(
    plot.title = element_text(family='Iced Tea', color='darkgray', size=12.3),
    plot.subtitle = element_text(family='Iced tea', color='pink', size=10),
    plot.caption = element_text(family='Bahnschrift', hjust = 0.5, vjust = 0.5, size=5, color='darkgray')
  )

ggsave("02_pictogram.png", plot = p, type = 'cairo', width = 3, height = 4, dpi = 300, units = "in", bg='#fffeea')