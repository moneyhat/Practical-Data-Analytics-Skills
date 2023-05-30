# ANALYTICS 1 - DATA PRODUCT FUNNELS

# This lessons will center around a dataset from the company Yammer
# Yammer is a communications tool for companies to use internally
# As with most data products, engagement is key.  
# Thus, as a data analyst, your job is going to be to take in these raw data, manipulate data types, and assess the engagment funnels
# There are three datasets here, each of which has it's own funnel

# signup - this looks at how effective the signup funnel is, from going to the signup page to creating an account

# engage - this measures engagement with the actual messaging service. Individuals will go to the homepage, look at inboxes, sent messages, etc.  

# search - this looks at how individuals interact with the search feature within the messager service.  how many people enter a search, wait for results, and then actually click a result?

library(tidyverse)
library(lubridate)

signup <- read_csv("https://docs.google.com/spreadsheets/d/1ZLNuxq_ONAAKRNh9AfVvsFQawYJdICiEFvwJx9pUXcM/gviz/tq?tqx=out:csv")

engage <- read_csv("https://docs.google.com/spreadsheets/d/1KJAEVAMiwqyJDEytW3Q8legeyImdjNW9dAMv2BqHUCY/gviz/tq?tqx=out:csv")

search <- read_csv("https://docs.google.com/spreadsheets/d/1eHh06cYgzJan2JywyhGrP9o95gQPmm4x4aUftRcyjck/gviz/tq?tqx=out:csv")

########################## Making your funnel

# add a column with the numeric value of 1

signup$one <- 1

# what unique values are in the event_name column?

unique(signup$event_name)

# so given this is a pipeline, there's an order... what should the order be?  Talk with your group and then convert the event_name column to a factor with the correct order specified in levels.

signup$event_name <- factor(signup$event_name, levels = c('create_user', 'enter_email', 'enter_info', 'complete_signup' ))

# now you can make your funnel!  First, create a new data frame that counts the number of times each event appeared in your original dataframe.  You'll need to group_by and summarize

signup_funnel <- signup %>%
  group_by(event_name) %>%
  summarize(total_events = sum(one)) 

# check out your data frame

signup_funnel

# plot your funnel with a bar graph!  What should be on the x and y?  Use geom_col()
# What does this plot suggest about the signup funnel?

ggplot(signup_funnel,
       aes(x = event_name, y = total_events)) +
  geom_col()


# Make a signup_week data frame that groups both by event_name and weeks.  This way we can track actual usage and conversion over this fiscal quarter

signup_week <- signup %>%
  group_by(event_name, week = floor_date(occurred_at, 'week')) %>%
  summarize(number_events = sum(one))

ggplot(signup_week,
       aes(x = week, y = number_events, color = event_name)) +
  geom_line()

# This looks good, but the way in which the times are grouped leaves those tiny values on either end.  Let's filter out those numbers.  Take your signup_week dataframe and filter it with filter(week <= '2014-08-24 00:00:00' & week >= '2014-05-04 00:00:00')

signup_week <- signup_week %>%
  filter(week <= '2014-08-24 00:00:00' & week >= '2014-05-04 00:00:00')


# Now plot it again.  Feel free to just copy down the former plot.  What does this tell us about signups over time?  Anything we should be concerned about?
ggplot(signup_week,
       aes(x = week, y = number_events, color = event_name)) +
  geom_line()




# engagment
engage$one <- 1

unique(engage$event_name)

engage_funnel <- engage %>%
  group_by(event_name) %>%
  summarize(number_events = sum(one))

engage$event_name <- factor(engage$event_name, levels = c('home_page', 'view_inbox', 'like_message', 'send_message'))

engage_week <- engage %>%
  group_by(event_name, week = floor_date(occurred_at, 'week')) %>%
  summarize(number_events = sum(one)) %>%
  mutate(number_events_lag = lag(number_events)) %>%
  mutate(number_change = number_events - number_events_lag) %>%
  filter(week <= '2014-08-24 00:00:00' & week >= '2014-05-04 00:00:00')

ggplot(engage_week,
       aes(x = week, y = number_change, color = event_name)) +
  geom_line()



#### search 
search_1 <- search
search <- search_1

search$one <- 1

unique(search$event_name)

search_funnel <- search %>%
  group_by(event_name) %>%
  summarize(number_events = sum(one))

search$event_name <- factor(search$event_name, levels = c('search_autocomplete', 'search_run', 'search_result'))


search_week <- search %>%
  group_by(event_name, week = floor_date(occurred_at, 'week')) %>%
  summarize(number_events = sum(one)) %>%
  mutate(number_events_lag = lag(number_events)) %>%
  mutate(number_change = number_events - number_events_lag) %>%
  filter(week <= '2014-08-24 00:00:00' & week >= '2014-05-04 00:00:00')

ggplot(search_week,
       aes(x = week, y = number_events, color = event_name)) +
  geom_line()


