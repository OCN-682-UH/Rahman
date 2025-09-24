############################################################
# Week 05: lubridate join & resample
# Tasks: Read conductivity & depth,  Parse datetime columns, Round conductivity timestamps to nearest 10 seconds, Inner join (exact time matches; no NAs from join), Average by minute (date, depth, temperature, salinity), Make a plot with the averaged data,  Use pipes; save outputs in output/; comment clearly
# Author: Sk Abidur Rahman
# Date: 2025-09-23
############################################################

#### Load Packages ----
# install.packages(c("tidyverse","lubridate","here")) 
library(tidyverse)
library(lubridate)
library(here)


#### Load Data and Wrangling ----

cond_raw  <- read_csv(here("Week_05","data","CondData.csv"),  show_col_types = FALSE)
depth_raw <- read_csv(here("Week_05","data","DepthData.csv"), show_col_types = FALSE)
glimpse(cond_raw)
view(depth_raw)

#### Parse datetimes & round conductivity to nearest 10 seconds ----
# consistent timezone (Honolulu)
cond_10s <- cond_raw %>%
  mutate(
    date = mdy_hms(date, tz = "Pacific/Honolulu", quiet = TRUE),
    date = round_date(date, unit = "10 seconds")
  )

depth_10s <- depth_raw %>%
  mutate(
    date = mdy_hms(date, tz = "Pacific/Honolulu", quiet = TRUE),
    date = round_date(date, unit = "10 seconds")
  )

#### Inner join on exact timestamps ----
joined <- cond_10s %>%
  inner_join(depth_10s, by = "date")
# joined now has: date, Temperature, Serial, Salinity, AbsPressure, Depth
# sanity test
nrow(joined)           # should be > 0
range(joined$date)     # within expected time range

####  Average by minute: date, depth, temperature, salinity ----
# We’ll compute the minute bucket and summarise means for each minute.
avg_by_min <- joined %>%
  mutate(minute = floor_date(date, unit = "minute")) %>%
  group_by(minute) %>%
  summarise(
    date_avg  = first(minute),
    temp_avg  = mean(Temperature, na.rm = TRUE),
    sal_avg   = mean(Salinity,    na.rm = TRUE),
    depth_avg = mean(Depth,       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(date = date_avg, depth_avg, temp_avg, sal_avg)

# Save the averaged data
write_csv(avg_by_min, here("Week_05","output","avg_by_minute.csv"))

#### Plot : Faceted time-series ----
# Reshape to long for a consistent scale and clean legend/facets
avg_long <- avg_by_min %>%
  pivot_longer(c(temp_avg, sal_avg, depth_avg),
               names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
                           temp_avg  = "Temperature (°C)",
                           sal_avg   = "Salinity (PSU)",
                           depth_avg = "Depth (m)"))

p <- ggplot(avg_long, aes(x = date, y = value, color = variable)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  facet_wrap(~ variable, ncol = 1, scales = "free_y") +
  scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M") +
  labs(
    title    = "Minute-Averaged Time Series: Temperature, Salinity, and Depth",
    subtitle = "Conductivity & depth rounded to 10 s, inner-joined; averages per minute",
    x = "Time", y = NULL, color = "Variable",
    caption  = "OCN862 Week 05b — Sk Abidur Rahman"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  filename = here("Week_05","output","avg_timeseries_faceted.png"),
  plot = p, width = 10, height = 7, dpi = 320
)

# How many cond times don’t match any depth time?
anti_join(cond_10s, depth_10s, by = "date") %>% nrow()

# How many depth times don’t match cond?
anti_join(depth_10s, cond_10s, by = "date") %>% nrow()

