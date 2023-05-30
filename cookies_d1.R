library(tidyverse)

cookies <- read_csv("https://docs.google.com/spreadsheets/d/1KhUkAxtGmGCjH70QJ0nudwAGFLWMIVIXYzoJMrDemf0/gviz/tq?tqx=out:csv")


# check/explore cookies

glimpse(cookies)
unique(cookies)
summary(cookies)

# get the mean and variance of cookies... would be a good place to use group_by() and summarize().  Remember mean is just mean(), but variance is var()

cookies %>%
  group_by(brand) %>%
  summarize(mean = mean(num_chips), variance = var(num_chips))
#taking cookies and figuring out the variance using what we already have


# make a boxplot with brand on the x and num_chips on the y.  use both geom_boxplot() and geom_jitter() so we can see the spread of points.  Does the spread of point match your intuition about variance from the above? 


ggplot(cookies,
       aes(x = brand, y =num_chips, fill = brand)) +
  aes(color = num_chips) +
  ylab('Number of Chips') +
  xlab('Chip Brand') +
  theme_minimal()+
  geom_boxplot() +
  geom_jitter()

#jitter lets us look at each individual point(cookie)

t.test(num_chips~brand, data= cookies)

#95 percent confidence interval:
#-3.005924  1.637503

#we are 95% positive that the mean will fall between the specified range +1.638 or -3.006

