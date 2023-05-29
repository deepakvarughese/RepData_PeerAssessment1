library(here)
library(rio)
library(janitor)
library(tidyverse)
library(mice)

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

median_steps_per_day <- median(steps_per_day$total, na.rm = TRUE)
median_steps_per_day


## The 5 minute interval that has on average the most steps taken

most_steps_period <- data %>% 
  group_by(interval) %>% 
  summarize(mean_steps_per_interval = mean(steps, na.rm = TRUE)) %>% 
  arrange(desc(mean_steps_per_interval))

head(most_steps_period, 1)

plot <- ggplot(data = most_steps_period, mapping = aes(x = interval, y = mean_steps_per_interval))+
                 geom_col()
plot



## Missing Data


#Method 1 : Using the mice package

md.pattern(data)

tempdata <- mice(data,m=5,maxit=50,meth='pmm',seed=500)
complete_data <- complete(tempdata,1)


steps_per_day_impute <- complete_data %>% 
  group_by(date) %>% 
  summarize(total = sum(steps))

##histogram for imputed data
hist(steps_per_day_impute$total)

#mean and median for imputed data
mean_steps_per_day_impute <- mean(steps_per_day_impute$total, na.rm = TRUE)
mean_steps_per_day

median_steps_per_day_impute <- median(steps_per_day_impute$total, na.rm = TRUE)
median_steps_per_day

### Weekend

complete_data <- complete_data %>%
  mutate(weekday.end = ifelse(
    test = weekdays(date, abbreviate = TRUE) %in% c("Sat", "Sun"),
    yes = "Weekend",
    no = "Weekday"
  ))

steps_weekend <- complete_data %>%
  group_by(weekday.end) %>%
  summarise(steps = mean(steps, na.rm = TRUE)))

steps_weekend
