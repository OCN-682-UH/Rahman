### This is my OCN862 Week 03 Class
### Created by: Sk Abidur Rahman
### Created on: 2025-09-10
############################################

### Load lIbrary #################

install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
glimpse(penguins)
View(penguins)


### Load Data ###########
ggplot(data=penguins,
       mapping = aes(x=bill_depth_mm,
                     y=bill_length_mm,
                     color=species)) +
  
  geom_point()+ 
  labs(title = "Bill depth and length",
       subtitle = "Dimention for Adelie", 
       x = "Bill depth (mm)", y = "Bill Length (mm)",
       color = "species")+
       captions = ("Sources: Palmar State" ) +
scale_color_viridis_d()

