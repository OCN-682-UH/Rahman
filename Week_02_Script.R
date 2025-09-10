### This is my OCN862 Week 02 assignment
### Created by: Sk Abidur Rahman
### Created on: 2025-09-07
############################################

### Load Libraries ##########
library(tidyverse)
library(here)


#### Read IN my data #########
weightData<- read.csv(here("Week_02","Data","weightdata.csv"))


### Data Analysis #######
head(weightData)
tail(weightData) # Looks at the top 6 lines of the data frame
View(weightData)
