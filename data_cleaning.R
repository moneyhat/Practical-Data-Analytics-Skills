# cleaning data in R!  

# The goal of today's less is to look at a reduced version of our beer data.  You want to do your analysis, but you see multiple things wrong with data in several columns.  So, before making plots we need to go and fix things individually.

# Lesson objectives
# Understand when to clean individual columns
# Use exploration methods to identify issues within character and numeric columns
# Understand when to replace/modify vs. remove erroneous values
# Be able to use ifelse statements to replace values

#load library as always
library(tidyverse)

# grab data

beer <- read_csv("https://docs.google.com/spreadsheets/d/18Iux-10Ggj2qLNEgH5WJGGUNTKET9Tpy3HHl1gc6L9Y/gviz/tq?tqx=out:csv")


# First, let's explore our data....  look at unique values in text columns, and histograms in numeric columns

# Unique brewery names... what's the issue here?
unique(beer$brewery_name) 

#Check brewery state
unique(beer$brewery_state)

# Now make histograms and check unique for abv and year

ggplot(beer,
       aes(x = abv)) +
  geom_histogram()

unique(beer$abv)


# so what's all wrong here?
#Answer: some invalid values due to human error

  # clear misspelings in names
  # xyz have no idea what that is (mislabled text)
  # some years that are zero, so we should just ditch those with NA
  # some years are clearly 2009 but inputted as 2209


# so here we're going to step out of tidyverse into the scary world of base R

# We're going to be using a function called ifelse().  ifelse functions have three parts, the first checks if something is TRUE, the second says what to do IF it's true, the third says if not TRUE, ELSE do this. 

# make a list of numbers
xxx <- c(1,2,3,4,5,6,7,8,9,10)
#Note: Values of columns in a data frame

# replace with different number
ifelse(xxx >= 5, 0, 1)
#if xxx is > 5, then put in a 0 or else, put in a 1

# [1] 1 1 1 1 0 0 0 0 0 0
# It imputs replaced values based on the statements. values 1-4 are true in this case so the output IS 1. Everything else is a 0.


# but can replace with things that are not numbers
ifelse(xxx >= 5, 'hotdog', 'not hotdog')
#[1] "not hotdog" "not hotdog" "not hotdog" "not hotdog" "hotdog"     "hotdog"     "hotdog" [8] "hotdog"     "hotdog"     "hotdog"  

# horray, we can do this with text substitution!!!


# and assign back to objects
xyz <- ifelse(xxx >= 5, 'hotdog', 'not hotdog')

# This is a bit confusing, but you can make the else statement equal to the orignal value
ifelse(xxx >= 5, 1, xxx)
# [1] 1 2 3 4 1 1 1 1 1 1
#The tru condition is finally met after the code runs into the value of 5, where it is replaced by the value of 1.

#is alcohol equal to a certain value? if so, change it to one value if not that then change it to this value!

# OK, let's try this on a list of character strings

food <- c('donut', 'pancake', 'beer', 'raisins')

#First, write an ifelse statement that makes the word 'raisins' a 0 and anything else a 1

ifelse(food == 'raisins', 0, 1)

#NOTE: use '==' and 'quotes' when assigning values
#it will think it is an object , just like beer is an object and food is an object

# now do the same but instead replace raisins with the work 'gross' and everything else with 'great'

yyy <- ifelse(food == 'raisins', 'gross', 'great')


# now instead of changing all the values, just change raisins to gross, but leave everything else as their original values. Also assign it to an object yyy 

yyy <- ifelse(food == 'raisins', 'gross', food)

yyy

# So why is this so useful? 

#Answer: This is useful because we can identify values based on a text string/numerical values to help arrange our vectors accordingly

# there's a really important thing to note...  we need to both call a single column and assign back to that same column

beer$abv

# let's first go and try and fix Ariz0na without replacing the beer$brewery_state column.  First, write an ifelse statement to replace all appearances of 'Ariz0na' with the proper spelling and capitalization.  Leave all other values as their original values.  Instead of assigning the output to xxx so you don't mess up your data frame if you make a mistake. 

unique(beer$brewery_state)

# assign this to xxx

xxx <- unique(beer$brewery_state)

xxx <- ifelse(beer$brewery_state == 'Ariz0na', 'Arizona', beer$brewery_state)

# now let's replace the actual values within the data frame

beer$brewery_state <- ifelse(beer$brewery_state == 'Ariz0na', 'Arizona', beer$brewery_state)

# go fix 'Wershington'

beer$brewery_state_2 <- ifelse(beer$brewery_state == 'Wershington', 'Washington', beer$brewery_state)

# what should we do about  'xyz'?  
# we should assign it to an NA value

beer$brewery_state <- ifelse(beer$brewery_state == 'xyz', 'NA', beer$brewery_state)


# now on to our numeric data.  Let's start by replacing all year of 2209 with 2009

beer$year <- ifelse(beer$year == 2209, 2009, beer$year)
unique(beer$year)

# now replace all year values of 0 with NA given we have no clue what year they should be

beer$year <- ifelse(beer$year == 0, NA, beer$year)
unique(beer$year_2)

sum(is.na(beer$year))
#only 8 values so no big deal!

# so let's try to replace that huge value in the ABV... should it be 10.50?  Or NA?

beer$abv <- ifelse(beer$abv == 110.50, 10.50, beer$abv)
unique(beer$abv_2)

# make a few figures to verify that everything works

ggplot(beer,
  aes(x = abv)) +
  geom_histogram(bins = 20)

ggplot(beer,
       aes(x = year, y = abv)) +
  geom_jitter() +
  geom_smooth(method = 'lm')

ggplot(beer,
       aes(x = brewery_state, y = rating)) +
  geom_boxplot()

