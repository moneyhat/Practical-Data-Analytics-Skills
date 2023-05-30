# ANALYTICS 1 - DATA PRODUCT FUNNELS

# This lessons will center around a dataset from the company Yammer
# Yammer is a communications tool for companies to use internally
# As with most data products, engagement is key.  
# Thus, as a data analyst, your job is going to be to take in these raw data, manipulate data types, and assess the engagment funnels
# There are three datasets here, each of which has it's own funnel. 

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

engage$one <- 1

# what unique values are in the event_name column?
unique(engage)

##Answer:
# home_page, like_message, view_inbox, send_message

# so given this is a pipeline, there's an order... what should the order be?  Talk with your group and then convert the event_name column to a factor with the correct order specified in levels.

#Answer:
#the order has 4 steps. 
# 1)home_page 2) view_inbox  3)like_message 4)send_message


# now you can make your funnel!  First, create a new data frame that counts the number of times each event appeared in your original dataframe.  You'll need to group_by and summarize

engage_funnel <- engage%>%
  group_by(event_name) %>%
  summarize(event_engagement = sum(one))



# check out your data frame

glimpse(engage_funnel)

# plot your funnel with a bar graph!  What should be on the x and y?  Use geom_col()
# What does this plot suggest about the signup funnel?

ggplot(engage_funnel,
       aes(event_name, event_engagement)) +
  aes(color = event_name) +
  geom_col() +
  theme_minimal()+
  ylab('Number of Engagements')+
  xlab('Event Name')



# Make a new data frame that groups both by event_name and weeks.  This way we can track actual usage and conversion over this fiscal quarter. If I was using the signup data I would call it signup_week for example.

actual_usage <- engage%>%
  group_by(event_name,
           emails_weeks = floor_date(occurred_at, 'week')) %>%
  summarize(occured_at = sum(one))

glimpse(actual_usage)

# This looks good, but the way in which the times are grouped leaves those tiny values on either end.  Let's filter out those numbers.  Take your weekly dataframe and filter it with filter(week <= '2014-08-24 00:00:00' & week >= '2014-05-04 00:00:00'). Remember to assign it back to the same weekly dataframe.


actual_usage <- actual_usage %>%
  filter(emails_weeks >= '2014-05-04 00:00:00')%>%
  filter(emails_weeks <='2014-08-24 00:00:00')

# Now plot it again.  Feel free to just copy down the former plot.  What does this tell us about usage over time?  Anything we should be concerned about?


ggplot(actual_usage,
       aes(x = emails_weeks, y = occured_at, color = event_name))+
  geom_point()+
  geom_line(method = 'lm')




