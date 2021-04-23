#-----
# Day 10 #30DayChartChallenge
# Author: Sarah H
# Date: 13 Apr 2021
#-----

# Load libraries
extrafont::loadfonts(device = "win", quiet=T)
library(tidyverse)
library(generativeart)
library(here)
library(magick)

#---------------- as obtained from generativeart docs -------------------
# Ref: https://github.com/cutterkom/generativeart

# set the paths
IMG_DIR <- "img/"
IMG_SUBDIR <- "everything/"
IMG_SUBDIR2 <- "handpicked/"
IMG_PATH <- paste0(IMG_DIR, IMG_SUBDIR)

LOGFILE_DIR <- "logfile/"
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, LOGFILE)

# create the directory structure
generativeart::setup_directories(IMG_DIR, IMG_SUBDIR, IMG_SUBDIR2, LOGFILE_DIR)

my_formula <- list(
  x = quote(runif(1, -3, 1) * x_i^2 - sin(y_i^2)),
  y = quote(runif(1, -2, 1) * y_i^3 - cos(x_i^2))
)

my_formula2 <- list(
  x = quote(runif(1, -3, 1) * x_i^2 - sin(y_i^4)),
  y = quote(runif(1, -0.1, 1) * y_i^3 - cos(x_i^2))
)

# Print images into stated directory
generativeart::generate_img(formula = my_formula, nr_of_img = 2, polar = T, color = "#7FFFD4", background_color = NA)
generativeart::generate_img(formula = my_formula, nr_of_img = 3, polar = T, color = "#FFF8DC", background_color = NA)
generativeart::generate_img(formula = my_formula2, nr_of_img = 3, polar = F, color = "#DDA0DD", background_color = NA)

#---------------------------------------------------

# Get images
img <- c(
  here::here("img", "handpicked", '3.png'),
  here::here("img", "handpicked", '4.png'),
  here::here("img", "handpicked", '5.png'),
  here::here("img", "handpicked", '1.png'),
  here::here("img", "handpicked", '8.png'),
  here::here("img", "handpicked", '2.png'),
  here::here("img", "handpicked", '6.png'),
  here::here("img", "handpicked", '7.png')
)

image <- image_read(img) %>%
  image_background("none")

# Reading this in individually because I want to resize this only lol
img9 <- here::here("img", "handpicked", '9.png')
image9 <- image_read(img9) %>%
  image_resize('3000x3000')

all <- c(image, image9)

# Combine both images
final <- image_flatten(all, 'Minus')

# Save combined images
image_write(final, path = "10_abstract.png", format = "png")

# for animation (not in use)
animation <- image_animate(image, fps = 2, optimize = TRUE)
print(animation)

# Thank you for the awesome Generativeart package!
