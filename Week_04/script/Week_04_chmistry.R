############################################################
#Week 04: Chemistry Assignment
#Task: Removes NAs, Seperate Tide tiime into Tide and Period, Filter, Pivot_longer to tidy multiple parameters, summary stats, non-boxplot
#Author: Sk Abidur Rahman
#Last Update Date: 2025-09-20
############################################################

### Load Packages ----
#packages: tidyverse,here
library(tidyverse)
library(here)

### Load Data and Wrangling ----
chem_raw<-read_csv(here("Week_04", "data","chemicaldata_maunalua.csv"),
                   show_col_types = FALSE)
glimpse(chem_raw)
head(chem_raw)

key_cols<- c(
  "Site", "Season", "Tide_time","Temp_in","Salinity","Phosphate", "Silicate", "NN","pH", "TA","percent_sgd"
)

chem <- chem_raw %>%
  select(any_of(key_cols)) %>%
  drop_na()      # drops rows with NA in any of those columns


#### Separate Tide_time into Tide and Time ----       # Assumes Tide_time is like "High_08:00/Low_Day" or "Low-13:30/High_Night" 
chem2<-chem%>%
  separate(Tide_time,into=c("Tide", "Period"), sep="_", remove=TRUE, fill="right")%>%
  mutate(
    Tide=factor(Tide, levels = c("Low", "High")),
    Period=factor(Period,levels= c("Day", "Night"))
  )


#### 4) Filter a subset ----          # keep only High tide samples at two sites 
chem_sub<- chem2 %>%
  filter(Tide=="High", Season == "SPRING", Site %in% c("W", "BP"))


#### 5) Pivot longer (wide to long) multiple parameters ----
param_cols <- c("Temp_in", "Salinity", "Phosphate", "Silicate", "NN","pH", "TA", "percent_sgd")

chem_long <- chem_sub %>%
  pivot_longer(
    cols =all_of (param_cols),
    names_to= "parameter",
    values_to="value"
  )

#### Summary statistics & export ----
chem_summary <- chem_long %>%
  group_by(Site, parameter) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value,   na.rm = TRUE),
    .groups = "drop"
  )

write_csv(chem_summary, here("Week_03","output","chemistry_summary.csv"))

####  Plot & export ----
plt <- ggplot(chem_summary, aes(x = parameter, y = mean, fill = Site)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    position = position_dodge(width = 0.75),
    width = 0.25,
    linewidth = 0.5
  ) +
  labs(
    title = "Chemistry (Subset: High tide, SPRING) — Mean ± SD by Parameter",
    subtitle = "Sites: W and BP • Data: chemicaldata_maunalua.csv",
    x = "Parameter",
    y = "Mean value (units vary)",
    fill = "Site",
    caption = "OCN862 Week 03 — Sk Abidur Rahman"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11)
  )

ggsave("Week_04/output/chemistry_plot.png", plot=plt, width = 10, height = 5, dpi = 320)

