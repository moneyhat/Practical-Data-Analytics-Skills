# Load
library(tidyverse)
library(lubridate)

# Usual bikes data
bikes <- read_csv("https://docs.google.com/spreadsheets/d/1t4wJXlT2-P7djUjlK2FziedSrCsxtjTFHRD4mkqfvfo/gviz/tq?tqx=out:csv")






# ok, let's do all of our data cleaning and manipulation in one go!
# this creates our datetimes first
# then calulates trip time, converts to numeric, and filters out excessive times
# it then creates our speed, route, and rider_age features
bikes <- bikes %>%
  mutate(start_time = ymd_hms(bikes$start_time)) %>%
  mutate(end_time = ymd_hms(bikes$end_time)) %>%
  mutate(trip_time = end_time - start_time) %>%
  mutate(trip_time = as.numeric(trip_time)) %>%
  filter(trip_time <= 240) %>%
  mutate(speed = distance_miles/(trip_time/60)) %>%
  unite(route, c('from_station_id','to_station_id'), sep = '-', remove = FALSE) %>%
  mutate(rider_age = 2018 - birthyear) %>%
  filter(rider_age <= 80 | is.na(rider_age)) %>%
  mutate(one = 1)


bikes <- bikes %>%
  select(trip_time, usertype, tourist, one)


# let's groupy by just usertype and count how many observations there are

bikes %>%
  group_by(usertype) %>%
  summarise(number = sum(one))

# summarize both number of obs. and mean trip time

bikes %>%
  group_by(usertype) %>%
  summarise(number = sum(one),
            mean_trip_time = mean(trip_time, na.rm = TRUE))

# group by usertype and tourist



