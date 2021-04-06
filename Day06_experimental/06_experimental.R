# Load libraries
extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)

# Palette
fgcol <- "#dbb98f"
bgcol <- "#f8e0d0"
textcol <- "#ac5c3c"

# Load data & plot
p1 <- read_csv("coord.csv") %>%
ggplot() +
  geom_polygon(aes(x=x, y=y, group=1), color=fgcol, fill=fgcol) +
  scale_y_reverse() +
  labs(
    caption=" © Mondelez International | #30DayChartChallenge | @saraahannes"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill=bgcol, color=bgcol),
    plot.caption = element_text(family='roboto', hjust = 0.5, vjust = 0.5, size=10, color=textcol, margin = margin(0, 0, 0.3, 0, "cm"))
  )

# Save plot
ggsave("06_experimental.png", plot = p1, type = 'cairo', width = 8, height = 6, dpi = 300, units = "in", bg =bgcol)