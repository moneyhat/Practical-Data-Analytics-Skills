# a few more features and data aggregation

# Today we're going to start digging into grouping data. This is an extremely critical skill for data analytics. It's how you create quantitative insights into differences between groups. We're going to keep working with our bikeshare data.

# Lesson goals
# Reimport and create the same features as we did in our homework and previous class
# Do some selective filtering to make the dataset relevant
# Conduct groupings across various features
# Learn how to calculate around NA values

library(tidyverse)
library(lubridate)

bikes <- read_csv("https://docs.google.com/spreadsheets/d/1t4wJXlT2-P7djUjlK2FziedSrCsxtjTFHRD4mkqfvfo/gviz/tq?tqx=out:csv")

# make datetimes
bikes$start_time <- ymd_hms(bikes$start_time)
bikes$end_time <- ymd_hms(bikes$end_time)

bikes <- bikes %>%
  mutate(trip_time = end_time - start_time) %>%
  filter(trip_time <= 240) %>%
  mutate(speed = distance_miles/(as.numeric(trip_time)/60)) %>%
  unite(route, c('from_station_id','to_station_id'), sep = '-', remove = FALSE)


# OK, let's make our rider_age like we did in the homework

bikes <- bikes %>%
  mutate(rider_age =  2018 - bikes$birthyear) 

# let's check out a summary and focus on rider_age

summary(bikes)

# Let's filter but keep those NA values as there is information there...  mainly, is there a group that seems to be more likely to not input an age?  

bikes <- bikes %>%
  filter(rider_age <= 80 | is.na(rider_age)) # keep the NA values, if not inputed then r will ditch na values

summary(bikes)

# let's make another feature that is for weekdays.  Given we already have datetime objects, the internal calendar can figure out what weekday any given date occured on.  We'll focus on just the start times. Apply the function weekdays() to bikes$startime and assign it to xxx for now. 

xxx <- weekdays(bikes$start_time)


# Great, that worked, now do the same but create a column in bikes called weekday

bikes$weekday <- weekdays(bikes$start_time)
bikes$weekday <- factor(bikes$weekday)
summary(bikes)

# OK, now we have a pretty great dataset to start considering all sorts of groupings. 
# Groupings are really important to understand patterns within data...  which month has the greatest number of rides?  Does a certain age group do something differently than another age group?  Are specific times different in terms of user engagement?

# Tidyverse has a great way to start considering groups.  As you'll see, you use first a group_by() function followed by a summarize() function.  This will be saying "within these groups, summarize these specific variables.

# This is my personal dumb way of counting things, but it's easier than using other functions IMO.  All I do is add a new column called 'one' that is just the number 1.  This way I can quickly and easily just sum up how many observations there were within a group without using any new functions.

# create the variable one 

bikes$one <- 1 #gives you the number of times someone did something by assigning a value of one to a column

# let's just use a groupby and summarize to count the number of male vs. female riders

bikes %>% #call dataset
  group_by(tourist) %>%# group it by dataset (tourist?)
  summarize(number_rides = sum(one)) #(count your userbase based on weather they are a  tourist or not)

# what if you layer in another level of grouping?  Try group_by(gender, tourist).  

bikes %>% 
  group_by(gender, tourist) %>%
  summarize(number_rides = sum(one))



# So, let's think about the number of NA values in tourist vs. non-tourist sites... what does it suggest?  Is there one usertype that has a bigger chance of not providing their gender?  Go and do another groupby with gender, usertype




# Up until now we've been cutting out NA values.  R can calculate around NA's, but you have to tell it to do so.  Create the following test vector

xxx <- c(1, 2, 3, NA, 4, NA, 6)

# now get the mean of xxx using the function mean()

mean(xxx)
#we have to explicityly tell R to ignore the NA's

# now add an additional argument na.rm =TRUE to your mean function.  Remember arguments need to be separated by a comma.  also, na.rm simply means 'NA remove?' since we have the = TRUE, you're saying 'if NA is TRUE then remove them from the calculation'

mean(xxx, na.rm = TRUE)

#hey R Studio, skip over the NA's and divide by the TRUE values!

# with this in mind, let's also calculate the mean distance ridden by those coming from tourist vs. non-tourist locations.  Call the new column mean_distance.  You can create multiple columns all within summarize. You just need to split them with a comma. e.g. summarize(number_rides = sum(one), mean_time = mean(trip_time)).


bikes %>%#take bikes
  group_by(tourist)%>%#then group if tourist, or naaah
  summarise(number_rides = sum(one),#then calculate the number of rides
            mean_time = mean(trip_time, na.rm = TRUE))#then calculate the mean time of trip_time

  #now add min and max ride times as additional columns

bikes %>%#take bikes
  group_by(tourist)%>%#then group if tourist, or naaah
  summarise(number_rides = sum(one),#then calculate the number of rides
            mean_time = mean(trip_time, na.rm = TRUE),#then calculate the mean time of trip_time
            min_time = min(trip_time, na.rm = TRUE),
            max_time = max(trip_time, na.rm = TRUE),
            mean_distance = mean(distance_miles, na.rm = TRUE))

#aggrigating by time is critical to for staffing, how many people you need or will need.

# Let's group a time variable.  For example let's group by month to get an idea of seasonal patterns.  Specifying times within group_by functions is a bit more complex, but not too crazy...  you give it the name of the new grouping column, then say what it's equal to.  THen you use the function floor_date() and within that tell it what datetime column to use and what level you want it at. e.g. 'month', 'day', 'hour'

# what does the code below give you?

bikes %>%
  group_by(day = floor_date(start_time, 'day')) %>% #takes column day
  summarize(number_of_rides = sum(one)) #how many times in this dataset does number of rides occur on each day throughout the year and at what time


# Try to now summarize by month.  Also calculate the mean ride time and mean distance.  Call the new columns mean_ride_time and mean_distance, respectively.

bikes %>%
  group_by(month = floor_date(start_time, 'month')) %>% #change from day to month!
  summarize(number_of_rides = sum(one),
            mean_distance = mean(distance_miles, na.rm = TRUE),
            mean_trip_time = mean(trip_time, na.rm = TRUE))


# what if you want daily groupings within just a specific month?  You could do this two ways... group each day for the whole year, then filter out the month, or you could filter out the month first and then group.  Let's take the latter approach.

# first create a data frame of just July and call this bikes_july

bikes_july <- bikes %>%
  filter(start_time >= '2018-07-01' & start_time <= '2018-07-31')

nrow(bikes_july)
# now group by day within bikes_july

bikes_july <- bikes_july %>%
  group_by(day = floor_date(start_time, 'day')) %>%
  summarise(number_of_rides = sum(one),
            mean_distance = mean(distance_miles, na.rm = TRUE),
            mean_time = mean(trip_time, na.rm = TRUE))
    
  
# make a plot of the number of rides over the course of the month

ggplot(bikes_july,
       aes(x = day, y = number_of_rides)) +
  geom_line()




