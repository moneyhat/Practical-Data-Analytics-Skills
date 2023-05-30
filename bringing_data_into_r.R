library(tidyverse)

# To date, we've been bringing in data directly off the web.  This is great in that it's super simple and you just need a URL.  It's a problem if that data moves, changes, or ends up being protected in some way. 

# One way to work with data, and a way you'll be needing to use for your end of semester project is by getting data locally.

# first step is to go to the data and download as a csv: https://docs.google.com/spreadsheets/d/13VH80njACpv_v9nZ_Dhr9gYfMldYiJ0QeDemsvWyDZA/edit?usp=sharing

# A bit about the data file being a CSV...

# Good convention is to use lowercase with words separated by _ for the filename

# First we have to find our filepath.  I work across multiple comptures, so I assign them to objects.  You can just enter it in directly to the read_csv() function.

filepath_win <- 'C:/Users/ndirienzo/Google Drive/courses_esoc_214/datasets/planes_dirty_google.csv'
filepath_x1 <- '/home/nick/Documents/esoc_local/planes_dirty_google.csv'

planes <- read_csv(filepath_win)

# There are pros and cons to this as well!
  # PROS - 
  # CONS - 


# So now our data is in and called planes.  This is a dataset on all recorded plane crashes. Take some time and explore it a bit using the methods you've been using to this point.  What are the columns?  What data do they contain?  Any redundant columns? Any errors in the data? How many crashes have there been?  


# So there are a couple things we want to get rid of and replace with NA values.  NA's are critical so R knows there's nothing in that cell and can do math or whatever function around them.  Rather than fixing things in every single row, you can do this on import

planes <- read_csv(filepath_win, na = c('____'))


# One big reasons NA values are useful at this point is you can count them!  the is.na() function will tell R what values are an NA.  Try it

is.na(planes$operator)

# Ugh, I'm tired of seeing 1000 values at a go ever time I view a column. But a great thing about R is that you can nest functions.  Instead of seeing all 1000, toss head() around is.na(planes$operator).  This will tell R to "Tell me if the first six values of the operator column are NA or not"  

head(is.na(planes$operator))

# but we might want to see a bit deeper than six.  You can pass multiple arguments to most functions. Run this below.

head(is.na(planes$operator), n = 20)

# So you told the function head to get the head of is.na(planes$operator) but then also told it to grab the first 20 rows.  This is similar to how when we importated data and told R what NA values were using an additional argument. 

# Here's an interesting thing about TRUE and FALSE in R... they also can act as 1's and 0's. Thus you can count, add, and multiply them. Try the following

TRUE + TRUE

TRUE + TRUE + FALSE

TRUE * 8

FALSE * 8 

# Since we can add them, we can use the sum() function on a column to count the number of NA values. Remember how we used the is.na() function.  Use that again, but inside the sum() function to count the number of true values in the column

sum(is.na(planes$operator))

# go try some of the other columns that have NA values.  How many total rows are there?  What does the number of NA values relative to the number of total rows suggest about the value of the data?

# There are other perks to NA values.  Bring in your data again without replacing the improper values and assign it to planes_original

planes_original <- read_csv(filepath_win)

#now try and calculate the number of dead in both the original data and that where ? and xxx have been replaced with NA.  Note how we added an additional argument 'na.rm' to the sum function. This just means remove NA values = TRUE

sum(planes_original$dead, na.rm = TRUE)
sum(planes$dead, na.rm = TRUE)


# It's also good to slim down your data frame at this point if there are columns you don't need.  There's no point in sucking up memory if you don't need to.  Tidyverse has a super clean way of doing this. Based on your exploration and count of NA values, let's keep just a few columns... 

planes %>%
  select(dead, passengers)

# Now call planes...  how come all the columns are still there?  How many should be there?  How do we fix this?  



# OK, great, but we also need more than just these two columns.  How do we go back?  One thing that is useful is to assign all your manipulations to a new object as you go and then change/overwrite at the end.  All depends on how big your data are. 


# Now we can do some different figures to get an idea to further explore

# just run these lines quick
install.packages('lubridate')
library(lubridate)
planes$date <- mdy(planes$date)
planes$dead <- as.numeric(planes$dead)
planes$passengers <- as.numeric(planes$passengers)


ggplot(planes,
       aes(x = date, y = dead)) + 
  geom_point() + 
  facet_grid(military ~ .)

ggplot(planes,
       aes(x = passengers, y = dead)) + 
  geom_point()
