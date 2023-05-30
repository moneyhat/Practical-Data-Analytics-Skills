# This is an R studio script.
# Scripts are written sets of code that perform certain tasks.
# One useful feature is that you can simply save a script and reopen it or run it again later.

# Note that I'm starting each text line with a pound sign ('#').  Doing this causes R to not actually run this text.  This is very useful to make notes to yourself within your script... otherwise you may forget why you did specific things, or others may not know why you wrote your code a certain way.

for now, move your cursor over this line and hit control + enter

# note that it gives you an error message without it...  that's just R saying that you gave it a command that it doesn't know how to process. Now try the same on the line below

3 + 2 + 827

# you can also just go directly over to the console and type commands in there. Go try it.  When might you want to type things into your script vs type thing directly into the console?



####################################################
### Getting into our data challenge

# OK, enough on R scripts for now, let's get into our intro data science workup.  The dataset looks at student performace based on ethnicity group, parental level of education, gender, if they had a school lunch provided (a proxy for income), and if they took a test prepration course. 

# You're working as a data scientist for the local school district, and the school wants to figure out what are the main predictors of student performance so they can better target programs to improve disadvantaged individuals.  


####################################################
### Setting up our environment and importing data

# The first thing in any R script should be to import all the packages you are going to use.
# packages contain additional functions that let you expand the functionality of R
# They often let you do really specific tasks, but others just make R more usable!
# We're going to be using a package called tidyverse in virtually every lesson
# You load it by running the following command

library(tidyverse)



# Now we need to import our data.  We're going to load this from a google doc I have on my google drive

# import our data and assign it to the object 'schools'
schools <- read_csv("https://docs.google.com/spreadsheets/d/1fEM8xP0WvqR5TGig4sxwO1e7QXhhg-GWncDhEhp0VCM/gviz/tq?tqx=out:csv")

# there's a lot to unpack up there...  the big thing to know is that we read a CSV file (sort of like an excel file) and assigned it to an object called 'schools'.

# Now we have that data file stored in our local enviornment so we don't have to go download it every single time we want to do somethings


####################################################
### Explore the contents of schools

# R will import CSV files in something called a data frame, which is a lot like an excel spreadsheet.  Why don't we go and look at the first few rows of schools
head(schools)
# what is all being said there?  
tail(schools)

# there are other ways to look at your data frame
# try this
glimpse(schools)

# and this
summary(schools)

# what are the pros and cons of each method? What information does say glimpse give you that summary doesn't?  Is one easier to read? 

# the last thing you normally want to look at is how big your data frame is. For that go ahead and try
nrow(schools)
ncol(schools)


####################################################
### Visualizing the contents of schools

# it's normally really hard to just look at summaries of a data frame and know what it means.  It's much better to visualize the various columns to understand the features.  
#ggplot2 is a library within tidyverse.  It's a very common graphing package and is intended to make the process of plotting in R more simple (which it does... sort of). 

#Don't worry about understanding this code fully.  Just run it for now
ggplot(schools,
     aes(x = reading_score)) + 
     geom_histogram()
# chat with a neighbor - Can you figure out what the different parts of the code are telling R to do?

# also, what do you think about the graph you just made?  What does it tell you about the average score?  Anything funny about the two peaks?

# we know that some took a test preperation course... let's make two histograms for reading score, one for those who took it and one where they didn't

ggplot(schools,
     aes(x = reading_score)) + 
     geom_histogram(bins = 20) + 
     facet_grid(test_preparation_course ~ .)

# so, they look different... let's keep this in mind for later


# how would we look at a histogram of math scores?
# I always forget the names of variables, so go type names(schools) into the console and then fill in the _________ below for math scores
ggplot(schools,
     aes( x = __________)) + 
     geom_histogram()


# and make one for writing_scores
ggplot(_______,
     aes(x = __________)) + 
     _________________()


#  not all of our data is numeric... some is in text form.  It's hard to see just what different categories are present

#let's start by looking at what levels of education are in our data frame
unique(schools$parental_level_of_education)

# next it's good to consider how many observations do we have in each?
# you can quickly make a bar graph of this
ggplot(schools,
     aes(x = parental_level_of_education)) + 
     geom_bar()

# What would you think about this data if there were only a few observations for a given category? How confident would you be in your inference? 

# we can look at what values are present for our other text data as well
unique(schools$gender)
unique(schools$lunch)
unique(schools$test_preparation_course)


#the last step our our EDA will be looking at the relatinoship between our test scores

# run thise next three plots
ggplot(schools,
     aes(x = reading_score, y = writing_score)) + 
     geom_point() + 
     geom_abline()

ggplot(schools,
     aes(x = math_score, y = writing_score)) + 
     geom_point() + 
     geom_abline()

ggplot(schools,
     aes(x = math_score, y = reading_score)) + 
     geom_point() + 
     geom_abline()

# what do these suggest about test scores and how testing in one area predicts another?

####################################################
### Understanding factors that influence performance

# It's nice to start by writing down your specific questions
# Also any curious notes you learned from your exploratory analysis above

# for example, we think that taking a test prepration course may influence math scores
# let's make a boxplot to look at this

ggplot(schools,
     aes(x = test_preparation_course, y = math_score)) +
     geom_boxplot()
# chat with a neighbor - if the black line in the middle(ish) of each box is the mean, what do the other parts of the boxes represent?

# error and noise in the data is why we need statistics.  We can run something called a t-test which compares the mean scores of two groups and tells us if they're different
t.test(math_score ~ test_preparation_course, data = schools)

# the ouptut is confusing at first, but we'll get into all the detail as the course progresses.  For now, all I want you to focus on is the thing called the p-value.  In brief, if the p is less than 0.05, then we can say the two groups are 'significantly different'

# thus, the mean in the group that completed the test prep is significantly higher than those that didn't

# this seems to suggest that taking a test prep could really help improve scores, but are their other things that could be contributing to success?  What could those be? 



###################
#let's dig into the idea that parents with more education may be more likely to enroll their students in these programs


#First let's plot how many in each group enroll vs. don't enroll
ggplot(schools,
     aes(x = parental_level_of_education, fill = test_preparation_course)) + 
     geom_bar(position = 'dodge')

# Messy - let's use some stats - here we're just going to predict if they did enroll (1) or didn't enroll (0)
enroll <- glm(test_preparation_course ~ parental_level_of_education, data = schools,
     family = 'binomial')

# oops, R doesn't like that we're feeding it text... let's convert to 0 and 1 for none and enrolled.  (IT'S FINE IF YOU DON'T GET THIS CODE)
schools$enrolled <- ifelse(schools$test_preparation_course == 'none', 0, 1)

#  Run the new model
enroll_ed <- glm(enrolled ~ parental_level_of_education, data = schools,
     family = 'binomial')

#look at the summary and p-values.  Are any less than 0.5?  What does this mean
summary(enroll_ed)



################
# maybe this is related to economic status, so let's look at how that influences enrollment rates.  Maybe people from lower economic statuses can't afford to pay for these courses

#first make a binary variable
schools$lunch_binom <- ifelse(schools$lunch == 'standard', 1, 0)

enroll_lunch <- glm(enrolled ~ lunch_binom, data = schools,
     family = 'binomial')

summary(enroll_lunch)
# thoughts?



###############
# ok, so test prep courses appear to help.  What about parental education?

# graph math scores against level of education
ggplot(schools,
     aes(x = parental_level_of_education, y = math_score)) +
     geom_boxplot()

# That graph is hard to see visually... let's again use some stats. we can't quite use a t-test here, so instead we're going to so something called linear regression
grade <- lm(math_score ~ parental_level_of_education + test_preparation_course, data = schools)

summary(grade)

# can you do the same analysis for writing_score?
# can you do the same to look at the effect of lunch on one of the scores?


######################################################
########### conclusion

# after all this is said and done, what can you condclude and why?
# similarly, what can't you conclude?


