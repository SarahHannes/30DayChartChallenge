# Import Libraries
extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
library(ggtext)
library(showtext)
showtext_auto()
font_add_google("Quantico", 'Quantico')

# Plot
p1 <- ggplot() +
  geom_polygon(aes(x=c(1,100,100,1), y=c(1,1,2,2)), fill=NA, size=3) +
  geom_polygon(aes(x=c(1,70,70,1), y=c(1,1,2,2)), fill='#fcda90', size=3) +
  geom_curve(aes(x=100, xend=100, y=1, yend=2), color='#f0f0f5', lineend="round", size=3) +
  geom_path(aes(x=c(100,1,1,100), y=c(1,1,2,2)), color='#f0f0f5', size=3) +
  
  # Annotation
  geom_text(aes(x=71, y=2.2, label='71%'), color='#fcc20c', family='Quantico', size=30) +
  
  labs(title="<span style='color:darkgray'>Road to Graduation.. </span>", caption="<span style='color:darkgray'>#30DayChartChallenge   |   @saraahannes</span>") +
  coord_cartesian(xlim=c(0,120), ylim=c(1, 2.5)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="#fafafc", color='NA'),
    plot.title.position='plot',
    plot.title = element_markdown(family='Quantico', size=50),
    plot.caption = element_markdown(family='Quantico', hjust = 0.5, vjust = 0.5, size=20)
  )

# Save plot
ggsave("01_part-to-whole.png", plot = p1, type = 'cairo', width = 6, height = 2, dpi = 300, units = "in", bg = "#fafafc")