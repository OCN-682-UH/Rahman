############################################################
# This is OCN862 - Week 03: Best Possible Plot (Non-Scatter)
# Title: Raincloud plot of penguin body mass by species, faceted by sex
# Author: Sk Abidur Rahman
# Date: 2025-09-13
# Description: Creates a violin + boxplot + jitter (“raincloud”) visualization
#              of body mass (g) across species and sex using palmerpenguins.
# Reproducibility: Uses here::here() for file paths; explicit NA handling.
############################################################

#### Load Packages ----
library(tidyverse)                    
library(palmerpenguins)                   # install.packages(c("palmerpenguins", "tidyverse", "here"))- once if needed
library(here)

#### Parameters & paths ----
outfile <- here("Week_03", "output", "penguins_raincloud_bodymass_by_species.png") # Output path (no hard-coded absolute paths)
dir.create(here("Week_03", "output"), recursive = TRUE, showWarnings = FALSE)     # confirming output dir exists

#### Imp Data & wrangling ----
dat <- penguins |>
  select(species, sex, body_mass_g) |>
  drop_na(species, sex, body_mass_g) |>     # drop NAs for variables 
  mutate(
    species = fct_relevel(species, "Adelie", "Chinstrap", "Gentoo"),
    sex = str_to_title(as.character(sex)) |> factor(levels = c("Female", "Male"))
  )
glimpse(penguins)

#### Plot (non-scatter): Violin + boxplot + jitter (“raincloud”) ----
p <- ggplot(dat, aes(x = species, y = body_mass_g, fill = species)) +
  geom_violin(width = 0.9, alpha = 0.6, color = NA, trim = FALSE) +                # distribution shape
  geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.9, color = "grey20") +  # robust summary
  geom_point(
    aes(color = species),
    position = position_jitter(width = 0.12, height = 0, seed = 123),             # individual points (jitter for context; not a scatter vs numeric x)
    size = 1.4, alpha = 0.25, show.legend = FALSE
  ) +
  stat_summary(
    fun = median, geom = "point", shape = 95, size = 6, color = "grey10",         # median line marker (thicker)
    position = position_dodge(width = 0.9), show.legend = FALSE
  ) +
  facet_wrap(~ sex, nrow = 1) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Penguin Body Mass by Species",
    subtitle = "Raincloud plot (violin + boxplot + jitter), faceted by sex",
    x = "Species",
    y = "Body mass (g)",
    caption = "Data: palmerpenguins • Visualization: Sk Abidur Rahman (OCN862 Week 03)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11)
  )

#### Save output ----
ggsave(filename = outfile, plot = p, width = 9, height = 5, dpi = 320)

#### Session info (transparency) ----
# sessionInfo()

