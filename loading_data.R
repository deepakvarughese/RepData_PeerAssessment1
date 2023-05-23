library(here)
library(rio)
library(janitor)
library(tidyverse)

data <- import(here("activity.zip"))

## Task 1 

### What is the mean number of steps taken per day? (Can ignore missing data)

#### What are the total number of steps taken per day
steps_per_day <- data %>% 
  group_by(date) %>% 
  summarize(total = sum(steps))

### Draw a histogram showing number of steps per day

hist(steps_per_day$total)

### Calculate and report the mean and median of the total number of steps taken per day

mean_steps_per_day <- mean(steps_per_day$total, na.rm = TRUE)
mean_steps_per_day