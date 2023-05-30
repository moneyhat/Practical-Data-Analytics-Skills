# DATA TYPES IN R

############ Lesson goals
# Understand the difference between various data types
# Be able to convert between data types - e.g. character to factor or numeric
# Be able to reorder factor data types
# Create datetime data types and manipulate them

library(tidyverse)
library(lubridate)


# Let's stick with a reduced version of our beer data
beer<- read_csv("https://docs.google.com/spreadsheets/d/1xvQXHDeAwkE6gXyo7tG_364Co8eValM2eeu-uHomJsI/gviz/tq?tqx=out:csv")


# And just run this line to create a year column that we'll work with
beer$year <- beer$add_date %>%
  str_extract('[0-9]{4}')


# Run a summary on beer...  we get our mix of responses on character and numeric data types
summary(beer)



# some of these columns should be numeric.  Also, factors are often times much more useful than characters.  Characters are simply many individual characters with no explict structure.  Factors are distinct groupings with different levels.  What columns may make more sense as factors?  Numeric?


# Let's change year first.  You simply reassign the column using the same name and then use as.numeric() on beer$year

beer$year <- as.numeric(beer$year)

summary(beer)


# converting to numerics is pretty straightforward.  It's also nice as now we can graph using year on an an axis and it will be in order.

ggplot(beer,
       aes(x = year, y = abv)) +
  geom_point()


# converting to factors uses the same general code.  convert the style column to a factor using factor()

beer$style <- factor(beer$style)


# this allows for some extra flexibility in terms of what you can do with them and what you get in terms of summaries.  

summary(beer$style) # note that it now recognizes the groups and gives counts as to how many are present


# Although the major advantage to factors over characters is that you can order them. Lots of time we would want categories to be ordered if there is some sort of directional relationship underlying them.  For example, light, medium, dark, or small, medium, and large.  These are factors with a natural order to them. 

# Let's make new column that gets rid of the range of ABV values and makes a simple three groupings of light, medium, and heavy.  Have light be if the abv is less than 5.5, medium between 5.5 and 9, and everything else above 9 be heavy. 


beer$abv_fact <- ifelse(beer$abv < 5.5, 'light', ifelse(beer$abv >= 5.5 & beer$abv < 9, 'medium', 'heavy'))

# what happens when we plot this?

ggplot(beer,
       aes(x = abv_fact, y = beer_overall_score)) + 
  geom_boxplot()

# but it's still a character string summary(beer)


# let's make it a factor and add the order of levels.  You can add the order of the levels using the argument levels = and then specifying a list of levels in the order you want them
beer$abv_fact <- factor(beer$abv_fact, levels = c('light', 'medium', 'heavy'))


# Now let's plot again. Better?

ggplot(beer,
       aes(x = abv_fact, y = beer_overall_score)) + 
  geom_boxplot()



# Now let's make a new column name called popularity. Use an ifelse like before.  Make everything with less than 200 reviews 'not', anything between 200 and 500 'moderate', and everything else 'very'.  

beer$popularity <- ifelse(beer$number_of_reviews <200, 'not', ifelse(beer$number_of_reviews <= 200 & beer$number_of_reviews < 500, 'moderate', 'very'))


# now convert it to a factor with the levels in order of 'not', 'moderate', 'very'
 beer$popularity <- factor(beer$popularity, levels = c('not', 'moderate', 'very'))


# and make a quick boxplot with popularity on the x and overall score on the y

ggplot(beer,
       aes(x = popularity, y = beer_overall_score)) +
  geom_boxplot()




# OK, so now we know numeric, character, and factors.  The last one we're going to learn is called datetimes.  Datetimes allow you to convert characters to actual dates.  This way R knows the order of them, and also allows you to apply a number of special grouping functions that we'll learn about later. 

# so what happens if we plot aspects of our data using our add_date column?

ggplot(sample_n(beer, 40),
       aes(x = add_date, y = beer_overall_score)) +
  geom_point() +
  coord_flip()


# Ideally we want the dates to be in order, but obviously that isn't possible with the method we used as factors.  So, we can use the packcage lubridate and the function mdy to create a datetime. now for datetime conversions.  All you do is apply a specific function where the name mirrors the order the the date and/or time values present in the column.  For example, if you had the date 21-08-2018, that's in the day - month - year format, and thus you would use dmy().  Go look at some values in our add_date column.  What's the format? 

# Let's make a column called date_dt that takes our column add_date and makes a datetime.  Let's first assign it to a test vector, though.  

xxx <- mdy(beer$add_date)
# now that it worked, make the new column

beer$date_dt <- mdy(beer$add_date)

# checkout glimpse and then the same figure as above but with date_dt on the x

ggplot(sample_n(beer, 40),
       aes(x = date_dt, y = beer_overall_score)) +
  geom_point() 


# datetimes have a lot of other uses. Think about this graph for a second.  What's wrong with making inference about a beer's popularity based on the year in which it was released?

ggplot(beer,
       aes(x = date_dt, y = number_of_reviews)) +
  geom_point() +
  geom_smooth(method = 'lm')



# so let's make a new column that looks at the rate of reviews for a given beer rather than the raw number.  We can start by making a column that considers the number of days a beer has been on the market.  With datetimes we can actually just subtract the date it was released from the date the data was collected.


# step on is to make a single datetime object that has the date everything was collected on
collected_on <- mdy('06-01-2018')

# Now use mutate to create a column called weeks_on_market.  This should subtract the date the beer was added  'date_dt' from the date the data was collected on.  This whole thing should be divided by 7.  THis will give us the total time the beer has been getting reviews for

beer <- beer %>%
  mutate(weeks_on_market = (collected_on - date_dt)/7) 


# and now let's make our review rate column as the number of reviews per week.  Use mutate again to make the column reviews_per_week that takes the number of reviews and divides by the weeks the beer has been on the market

beer <- beer %>%
  mutate(reviews_per_week = number_of_reviews/weeks_on_market)



# but R doesn't like to divde by a datetime, so let's convert it to numeric within the call above

beer <- beer %>%
  mutate(reviews_per_week = number_of_reviews/as.numeric(weeks_on_market))




# and let's plot now to see if beers that are more popular (have a higher review rate) also have an overall higher score.  Is this the hype effect?  Or is it that more people like to review good beer?  Or both?

ggplot(beer,
       aes(x = reviews_per_week, y = beer_overall_score)) +
  geom_point() +
  geom_smooth(method = 'lm')



