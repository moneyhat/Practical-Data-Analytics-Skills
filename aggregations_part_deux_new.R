# LESSON GOALS - Aggregations part deux
# Make new data frames that group variables of interest and calulate relevant summary statistics
# Be able to group by more than one level
# plot results using ggplot


# Load
library(tidyverse)
library(lubridate)

# Usual bikes data
bikes <- read_csv("https://docs.google.com/spreadsheets/d/1t4wJXlT2-P7djUjlK2FziedSrCsxtjTFHRD4mkqfvfo/gviz/tq?tqx=out:csv")

# Article to check out...  we're doing better analytics in this class!
# https://chicago.suntimes.com/news/divvy-bikes-data-148-million-rides-map-bikeshare/

                          
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
  filter(rider_age <= 80 | is.na(rider_age)) %>% #we want to keep the na's "|" acts like an or
  mutate(one = 1)

#make weekdays - not tidyverse so you can't pipe it like above

bikes$weekday <- factor(weekdays(bikes$start_time), levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))


# we can also extract just the hour a trip started so we can later group.  So just like we applied weekday above, do the same with the function hour() to make a new column called start_hour.  Also convert it to a factor

bikes_start_hour <-  hour(bikes$start_time)
bikes_start_hour <-factor(bikes_start_hour)

# now just check out a quick glimpse of bikes

glimpse(bikes)

# At this point, we've worked with group_bys a good bit, but here we want to make our associated summary stats more useful.  Specifically, we are going to create a new column that calculates the percent change in ridership from the previous timepoint.  This requires first grouping, then creating a lagged column, then a quick mutate to calculate the percent change.  It sounds tricky, but it's just using only one new function.

# first, let's create a new dataframe called bikes_month that summarizes the total number of rides each month. 
# Remember to group by some aspect of a datatime you need to give it the new column name, then =, then use the function floor_date().  Inside that function you specifiy the column with the datetimes, and then the units you want to group by in quotes (e.g. 'hour', 'month', 'day').  We did this in class before break, so check there if needed.

bikes_month <- bikes %>%
  group_by(month = floor_date(start_time, 'month')) %>%
  summarize(number_rides = sum(one))

bikes_month
#NOTE:
#were summing the months and making a total. To get the total number of rides from month to month we can subtract the tally from the months

### ok, so lags are extremely useful but super simple to use.  All they do is is take the entry that is in the column and moves it down one place.

# try this
xxx <- c(1,2,3,4)
lag(xxx)
#Note:
#we can use lag to create a column that will offset the months by one

# Now, remember that our data frame bikes_month is sorted by date already, so if we call lag on the number_rides column, the first will be NA, but the next will be the entry from the first position (i.e. January) placed in the second position (i.e. February)

# give you're going to make a whole new column that's lagged, you'll need to use mutate.  Create the lagged column with the name number_rides_lag and then call lag() on the number_rides column

bikes_month <- bikes_month %>%
  mutate(number_rides_lag = lag(number_rides))



# check it out

bikes_month

# now let's calculate our percent change feature.  We want to subtract the previous timepoint's number of rides (e.g. number_rides_lag) from our current number of rides (e.g. number_rides) and then divide by the number of rides in that previous month.  Also multiply that by 100 to get it in the proper units.  This will be a mutate as you're calculating a new feature.  Call that feature perc_change.

bikes_month <- bikes_month %>%
  mutate(percent_change = ((number_rides - number_rides_lag)/number_rides_lag)*100)

bikes_month

#this is a usefull feature to indicate %increse/decrese in ridership

# now let's plot over the course of the year.  Month is on the x axis and then do perc_change on the y.  Try a regular line plot.  Also try a column plot using geom_col.

ggplot(bikes_month,
       aes(x = month, y = percent_change)) +
       geom_col()


# Let's group by multiple levels and calculate the percent change again.  Add usertype to your groupby so we can look at percent changes over time in the two user groups. Copy bikes_month down here and modify the groupby to include another level of usertype

bikes_month <- bikes %>%
  group_by(usertype,
           month = floor_date(start_time, 'month'))%>%
  summarize(number_rides = sum(one))

# now calculate a percent change feature again. 

bikes_month <- bikes_month %>%
  mutate(number_rides_lag = lag(number_rides)%>%
  mutate(percent_change = ((number_rides - number_rides_lag)/number_rides_lag)*100))
  

# now let's plot in a bar graph like before using geom_col().  We need to add some additional arguments given we have two groups.  To specify the two groups in aes() we use fill = ... in this case it equals usertype.  

ggplot(bikes_month,
       aes(x = month, y = percent_change, fill = usertype)) +
    geom_col()


# Sometimes you don't want to create a percent change, but instead want to know what percentage a single grouping accounted for overall. So, what percentage of total rides occured in March for example. This is really useful to detect what are you busy months, what months make the most money, how consistant your usage is, etc.


# We can do this by making two different grouped data frames and then joining them together. We already have our bikes_month.  To caluate what percentage of rides in a month contributes to the overall we need to divide by the rides in a month by the total rides for that usergroup.  So first make a data frame of just the total number of rides for each usergroup. Call this bikes_total. When summarizing call the new column number_rides_all




# we can use what's called a left join to then distribute those columns to the bikes_month data frame

bikes_month <- left_join(bikes_month, bikes_total, by = c('usertype' = 'usertype'))

# now check out bikes_month


# create a column called perc_total that divides the nuber of rides in a month by the number of rides overall and then multiply by 100



# plot!

ggplot(bikes_month,
       aes(x = month, y = perc_total, fill = usertype)) +
  geom_col( position = position_dodge())


