extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
#remotes::install_github("hrbrmstr/waffle")
library(waffle)
#devtools::install_github("gadenbuie/ggpomological")
library(ggpomological)
library(patchwork)

p1 <- ggplot() +
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

p <- p1 + 
  labs(title='Total espresso shots pulled\nsince Movement Control Order',
       subtitle='hint: 61 cups',
       caption = "#30DayChartChallenge   |   @saraahannes\n") +
  theme(
    plot.title = element_text(family='Iced Tea', color='darkgray', size=12.3),
    plot.subtitle = element_text(family='Iced tea', color='pink', size=10),
    plot.caption = element_text(family='Bahnschrift', hjust = 0.5, vjust = 0.5, size=5, color='darkgray')
  )

# Save plot
ggsave("02_pictogram.png", plot = p, type = 'cairo', width = 3, height = 4, dpi = 300, units = "in", bg='#fffeea')
