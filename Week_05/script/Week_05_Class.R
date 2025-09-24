###########Using across() (tidyverse style)

library(tidyverse)
library(here)

# Load dataset

site_dat <- read_csv(here("Week_05", "data", "site.characteristics.data.csv"))


# Group by site and calculate mean + variance of all numeric columns
site_summary <- site_dat %>%
  group_by(site.letter) %>%
  summarise(
    across(where(is.numeric),
           list(mean = ~mean(.x, na.rm = TRUE),
                var  = ~var(.x,  na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  )

print(site_summary)



# Save 
write_csv(site_summary, "output/site_summary_mean_var.csv")

#########Using pivot_longer() (tidier/compact)

###########################################################33
site_long_summary <- site_dat %>%
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") %>%
  group_by(Site, variable) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    var  = var(value,  na.rm = TRUE),
    .groups = "drop"
  )

print(site_long_summary)

# Save tidy summary
write_csv(site_long_summary, "output/site_summary_long.csv")

#######################33
######Join
T1 <- tibble(Site.ID = C("A","B", "C"),
          Temperature = c(14.1, 16.7, 15.4))
T1_df <- data.frame(Site.ID = c("A", "B", "C")
                    Temperatuer = c(14.1,16.7,15.4))
left_join(T1,T2) ==
right_join(T2, T1) %>% arrange(Site.ID) %>% 

inner_join(T1, T2)

full_join(T1, T2) %>% drop_na()

semi_join(T1, T2)

install.packages("cowsay")
?cowsay
library(cowsay)
?cowsay
say ("Abdur Rahman", by = "cow")
