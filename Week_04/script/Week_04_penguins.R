############################################################
 #Week 04: Penguins Assignment
 #Part 1: Sumarries (mean/variances) of body mass by species,islands, sex (NA-free)
 #Part 2: Filter non-males, compute log body mass, select key cols, make/export plot
 #Author: Sk Abidur Rahman
 #Last Update Date: 2025-09-20
############################################################
 
 ### Load Packages ----
#packages: tidyverse, palmergenuis,here
 library(tidyverse)
 library(palmerpenguins)
 library(here)
 library(ggplot2)
 library(dplyr)

### Load Data and Wrangling ----
penguins_clean <- penguins %>%
  drop_na(species,island, sex, body_mass_g)

summary_tbl <- penguins_clean %>%
  group_by(species,island,sex) %>%
  summarise(
    mean_body_g= mean (body_mass_g, na.rm = TRUE),
    var_body_g= var (body_mass_g, na.rm=TRUE),
    .groups = "drop"
  )


 #### Filter out males, log, colums & plot ---
penguins_f <- penguins %>%
  drop_na(species,island,sex,body_mass_g)%>%    #this or
  filter(!is.na(body_mass_g), !is.na(sex))  %>% #drop NAs
  filter(sex !="male") %>%                      # exclude males
  mutate(log_body_mass = log(body_mass_g)) %>%  #natural log & create column
           select(species, island, sex, log_body_mass)

### Density plot
p<-ggplot(penguins_f,aes(x=log_body_mass, fill=species))+
  geom_density(alpha=0.4,adjust=1,color=NA)+
  facet_wrap(~island,nrow=1)+
  scale_x_continuous(name="Log body mass (g)")+
  scale_fill_brewer(palette = "Set2",name="species")+
  labs(
    title="Penguins Distribution of Log Body Mass by Spercies",
    caption= "OCN862 Week_04- Sk Abidur Rahman"
  )+
  theme_minimal(base_size = 12) +
  theme(
    legend.position="top",
    panel.grid.minor = element_blank(),
    plot.title=element_text(face="bold", size=15),
    plot.subtitle=element_text(size=11)
  )

### Save Output  ----
ggsave("Week_04/output/penguins_logmass_density.png", p, width = 9, height = 4.5, dpi = 320)

