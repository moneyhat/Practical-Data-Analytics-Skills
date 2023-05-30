# Feature engineering
library(tidyverse)
library(lubridate)

bikes <- read_csv("https://docs.google.com/spreadsheets/d/1t4wJXlT2-P7djUjlK2FziedSrCsxtjTFHRD4mkqfvfo/gviz/tq?tqx=out:csv")

#Lesson goals
# Create several features using different methods
# Real age column with strange values filtered out
# usable ride time
# Make a speed column using this
# Make a route column

# The goal of this data challenge to generate several features that can help make additional inference about a given product.  Here we have a number of additional features on our bikeshare data, but some are not quite usable as is.  Additionally, we can work towards making new features that can help inform business decisions.  

# We've actually made features before in this class...  making an average test score column, or calculating the number of times a beer is reviewed per week.  These are all types of data manipulation that make a more useful and informative measure out of existing data!

# As always, explore the data...  there's a bit more than in your data challenge.
glimpse(bikes)



# I want you to whiteboard out the processes to take these existing variables and then create a rider speed measure.



# first, let's make our trip_time feature like we did in our data challenge.  Step one - convert columns to datetimes, step two, do subtraction.

bikes$start_time <- ymd_hms(bikes$start_time)
bikes$end_time <- ymd_hms(bikes$end_time)

# Now subtract

bikes <- bikes %>%
  mutate(trip_time = end_time - start_time)


# check it out with glimpse and making a histogram of trip_time

ggplot(bikes,
       aes(x = trip_time)) +
  geom_histogram()



# this suggests we have some really huge trip times. Let's filter out any trip times that took more than say four hours (240 minutes)

bikes <- bikes %>%
  filter(trip_time <= 240)


# we could probably filter more, but this seems resonable for now as it should exclude those who say rented a bike and never used it



# Now go and calculate a speed measurement in miles per hour.  Call the column speed.  Note, our current trip_time measurement is in minutes. You'll need to use mutate and also convert the time to numeric and to hours.


bikes <- bikes %>%
  mutate(speed = distance_miles/(as.numeric(trip_time)/60))


# Check out the new feature with glimpse, summary, and a histogram
glimpse(bikes)
summary(bikes$speed)




# seems like some pretty resonable speed measurments.  What are some reasons why these are not totally accurate? 


# Let's now explore how riding speed differs between several types of groups. MAke a boxplot looking at how speed differs between tourist and non-tourists.  Riding speed likely contributes to accident rates, and thus Divvy may want to target certain safety measures towards one group or the other.  

ggplot(bikes,
       aes(x = tourist, y = trip_time)) +
  geom_boxplot()



# sometimes we want to merge columns to create a new feature. Other times we migth want to separate out parts within a column to be able to operate on different parts independently.  Here we're going to make a new column called route.  We have both the starting and ending ID, and there's obviously different combinations of these.  Let's make a variable that has the format starting_ID-ending_ID.  So it would be something like 42-52 saying the rider started at station 42 and ended at 52.  This will let us identify the most popular routes people take.

# we can use the unite function which binds two columns.  It uses the typically tidyverse syntax.  There are several arugments you need to specific inside unite().  1) the new column name, 2) a list of what columns you want to unite, 3) what symbol you want to use to separate them, and 4) if you want to remove the columns you united.

# let's call the new column 'route', have the columns we want to join be 'from_station_id' and 'to_station_id'

bikes <- bikes %>%
  unite(route, c('from_station_id','to_station_id'), sep = '-', remove = FALSE)

glimpse(bikes)


# now let's quick convert route to a factor so we get a summary of how many are present

bikes$route <- factor(bikes$route)

summary(bikes$route)



# Also, you can do some filtering and selecting to get the station names based on the route.  What would happen if you removed the select statement?
bikes %>%
  filter(route == c('3-35', '90-35', '133-192')) %>%
  select(from_station_name, to_station_name) %>%
  unique()

# everyone google the to location...  whats the nearest attraction?
