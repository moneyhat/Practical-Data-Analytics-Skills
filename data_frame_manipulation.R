# Today we'll be working with some data on crime in Tucson. There are 100k rows, each representing an individual call for service. Our goal is to look at patterns in crime over different time scales, and if the type of crime differs over time. Buy, before we get there we need to import our data and do some slimming down of the data frame.  

###### learning objectives
# Get additional practice importing data and identifying errors in imported data
# Create new columns using mutate()
# Be able to slim down data to relevant columns using select()
# Understand what makes a good column name and use rename() to rename columns
# Understand how to use relational operators to filter data using filter()
# Apply these methods to create different data frames for different groupings of crimes 

# as always, load libraries
library(tidyverse)


# Go get data here: https://docs.google.com/spreadsheets/d/1QCSkCLDBhsdlSvCQ50McCc5VwC_WTa5W-Vn-in-UgTQ/edit?usp=sharing

crime <- read_csv(file.choose())


# Based on the column types upon import, what columns should you explore for errors in the data?


unique(crime$seconds) #finds unique values within datasets
unique(crime$year)

#Let's bring the data in again

crime <- read_csv(file.choose(), na = c('xyz', 'not_reported')) #makes a new list


# let's check how many NA values there are

sum(is.na(crime$seconds))
[1] 2583

sum(is.na(crime$year))
[1] 423


# So now let's make our data more useful.  Remember how we used mutate to do some math on a couple columns?  Let's do that  again to convert seconds to hour.  This will be better for plotting and interpretation. remember that mutate() works using the following general format: new_column_name = math_function(old_column_name).  Here, we just want to divide the column seconds by 3600 and call it hours.  

#take your data then we're going to mutate it
# we don't need 'second' level-resolution
#create anew column named hours FROM seconds

crime <- crime %>%
  mutate(hours = seconds/3600)


# just run this line below...  we'll cover what it's doing in a future class
crime$day_of_week <- crime$day_of_week %>%
  factor(levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))


# We also have some redundant columns.  Let's get rid of incident_datetime and seconds column using select()
# also, let's make this new data frame a bit cleaner by just dropping na values from our data frame

#use 'select' to make a reduced data frame to omit certain columns called crime_slim

#renaming a new data frame to  crime slim selecting crime and selecting the COLUMNS WE WANT and omiting empty or NA values (assuming na values are evenly distributed)

crime_slim <- crime %>% 
  select(day_of_week,parent_incident_type,date,year,hours) %>%
  na.omit() #don't need to fill in anything here


#How many rows were dropped?

nrow(crime)
#[1] 104857

nrow(crime_slim)
#[1] 101860

sum(is.na(crime_slim$year))
#no more NA's
#less to work with and eliminated random data
#we should probably clean up the names in the column in case we wanted to present this to some normie.

# I also don't like that the column name 'parent_incident_type'.  We can rename it using the rename() function.  You use rename as follows rename(new_column_name = name_of_old_column).  Let's change parent_incident_type to crime_type.  Let's change day_of_week to day as well



crime_slim <- crime %>%
  rename(day = day_of_week, crime_type = parent_incident_type)

#now check to verify your columns
 glimpse(crime_slim)

# What range of values are in that year column?

ggplot(crime_slim,
       aes(x = factor(year))) + 
  geom_bar()

#1969 has some useless data and the last two years in the plot don't have ENOUGH data



# We can use the filter() function to select just the years we want.  First, let's play with some logical operators to see how we might do this

xxx <- c(1,2,3,4,5,6,7,8,9,10)

xxx >= 10

xxx <= 5

xxx >= 2 & xxx <= 8 #pulls data INBETWEEN certain intervals

xxx %in% c(2,4,6,8)

# Let's use these operators to get just the years that we have full data for. This requires the filter() function

#ALSO: 'then' = '% > %'
#take my code then do something to it

#we can filter out the data we DON'T want 

crime_slim = crime_slim %>%
  filter(year>=2010) %>% filter(year<=2016) 

#NOTE: if you run this incorrectly, you'll have to go BACK to the original line AND run the original code crime_slim <- crime %>%
# rename(day = day_of_week, crime_type = parent_incident_type)


ggplot(crime_slim,
       aes(x = factor(year))) + 
  geom_bar()


# you can use filter for text too. Let's now make two data frames.  First, make a list of all the character strings you want for violent and non-violent crimes.

#Notes: you can also find variation within your data prior to making your inferences
unique(crime_slim$crime_type)

#makes a list of crimes that are clearly violent
violent <- c('Assault',  'Arson', 'Homicide', 'Weapons Offense', 'Sexual Assault', 'Robbery', 'Assault with Deadly Weapon', 'Theft of Vehicle', 'Disorder', 'Liquor', 'Disorder')

#makes a list of crimes that are clearly non violent
non_violent <- c('Traffic', 'Property Crime', 'Theft from Vehicle', 'Vehicle Recovery', 'Community Policing', 'Other', 'Vehicle Stop', 'Breaking & Entering')


crime_violent <- crime_slim %>%
  filter(crime_type %in% violent)
#looks for 
nrow(crime_violent)

crime_non_violent <- crime_slim %>%
  filter(crime_type %in% non_violent)

nrow(crime_non_violent)

# now we have two new data frames.  How many crimes are in each?


# let's plot over time to see what's going on.  What would you predict?

ggplot(crime_violent,
       aes(x = day)) + 
  geom_bar()

ggplot(crime_non_violent,
       aes(x = day)) + 
  geom_bar()

# Let's also get an idea of how each varies throughout the day.  Make the two figures below

ggplot(crime_violent,
       aes(x = hours)) + 
  geom_bar()

ggplot(crime_non_violent,
       aes(x = hours)) + 
  geom_bar()

#basically just changed the x= to alter the graph




