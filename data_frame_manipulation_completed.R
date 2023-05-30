# Today we'll be working with some data on crime in Tucson. There are 100k rows, each representing an individual call for service. Our goal is to look at patterns in crime over different time scales, and if the type of crime differs over time. Buy, before we get there we need to import our data and do some slimming down of the data frame.  

# recap plus a bit more
# best practices
# summary stats
# calling data using $
# applying functions to columns
# assigning objects
# making lists and assigning them to objects and doing math with them
# pulling out errors in data upon import
# considering NA values
# using select to gather only the columns we want
# using filter to further slim down data

# load packages


# Go get data here: https://docs.google.com/spreadsheets/d/1QCSkCLDBhsdlSvCQ50McCc5VwC_WTa5W-Vn-in-UgTQ/edit?usp=sharing

crime <- read_csv(file.choose())



# Based on the column types upon import, what columns should you explore for errors in the data?

unique(crime$seconds)

#Let's bring the data in again

crime <- read_csv(file.choose(), na = c('xyz', 'not_reported'))


# let's check how many NA values there are

sum(is.na(crime$seconds))

# So now let's make our data more useful.  Remember how we used mutate to do some math on a couple columns?  Let's do that  again to convert seconds to hour.  This will be better for plotting and interpretation.  

crime <- crime %>%
  mutate(hour = seconds/3600)

# just run this line below...  we'll cover what it's doing in a future class
crime$day_of_week <- crime$day_of_week %>%
  factor(levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

# We also have some redundant columns.  Let's get rid of incident_datetime and seconds column
# also, let's make this new data frame a bit cleaner by just dropping na values from our data frame

crime_slim <- crime %>%
  select(day_of_week, parent_incident_type, date, year, hour) %>%
  na.omit()

#How many rows were dropped?

# I also don't like that the column name 'parent_incident_type'.  We can rename it using the rename() function

crime_slim <- crime_slim %>%
  rename(crime_type = parent_incident_type, day = day_of_week)

# What range of values are in that year column?

ggplot(crime_slim,
       aes(x = factor(year))) + 
  geom_bar()

# We can use other tidyverse functions to select just the values we want.  First, let's play with some logical operators

xxx <- c(1,2,3,4,5,6,7,8,9,10)

xxx >= 10

xxx <= 5

xxx >= 2 & xxx <= 8

xxx %in% c(2,4,6,8)

# Let's use these logical operators to get just the years that we have full data for. This requires the filter() function 

crime_slim <- crime_slim %>%
  filter(year >= 2010 & year <=2016)

ggplot(crime_slim,
       aes(x = factor(year))) + 
  geom_bar()

# you can use filter for text too. Let's now make two data frames.  First, make a list of all the character strings you want for violent and non-violent crimes

violent <- c('Assault',  'Arson', 'Homicide', 'Weapons Offense', 'Sexual Assault', 'Robbery', 'Assault with Deadly Weapon', 'Theft of Vehicle', 'Disorder', 'Liquor', 'Disorder')

non_violent <- c('Traffic', 'Property Crime', 'Theft from Vehicle', 'Vehicle Recovery', 'Community Policing', 'Other', 'Vehicle Stop', 'Breaking & Entering')


crime_violent <- crime_slim %>%
  filter(parent_incident_type %in% violent)

crime_non_violent <- crime_slim %>%
  filter(parent_incident_type %in% non_violent)


ggplot(crime_violent,
       aes(x = day_of_week)) + 
  geom_bar()

ggplot(crime_non_violent,
       aes(x = day_of_week)) + 
  geom_bar()

# Let's also get an idea of how each varies throughout the day.  Make the two figures below


crime %>%
  summarise(count = count(day_of_week))
