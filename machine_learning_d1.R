# ML predicting weight from height

# go here and put in your height and weight https://docs.google.com/spreadsheets/d/1SrdBlhuwrQICkHfeZDtbXJvD5ZhTU9IlPSk-SIKJMGk/edit?usp=sharing

# FYI, this is anonymous.  If you feel uncomfortable entering in your own height and weight, enter that of a family member, friend, or rough guess of your own.  

# Please enter height in inches and weight in pounds

library(tidyverse)

# now we can import our data
size <- read_csv("https://docs.google.com/spreadsheets/d/1SrdBlhuwrQICkHfeZDtbXJvD5ZhTU9IlPSk-SIKJMGk/gviz/tq?tqx=out:csv")


# scatterplot to look at this relationship
ggplot(size, 
       aes(x = height, y = weight)) +
         geom_point()


# Now we're going to make what's called a regression model.  The function is simply lm().  Inside LM you first say what you're going to predict, also known as your target.  You then include a ~ symbol.  The you include the feature you're going to use to predict your target.  You have to include an additional argument of data = size to tell the function that the data we're using is our size dataframe. 

size_model <- lm(weight ~ height, data = size)

#  Running that model is the training step of machine learning.  We can actually see the predicted relationship by using summary()

summary(size_model)

# this tells us a lot that we'll cover in detail later.  
######################################################################
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)   
#(Intercept) -246.444    132.750  -1.856  0.08456 . 
#height         5.998      1.960   3.060  0.00848 **

#for every inch taller someone is, they weigh 5.998 pounds more
#the standard error could be utilized as a confidence interval
######################################################################
#added sex parameter
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept) -133.940    108.054  -1.240  0.22543   
#height         4.061      1.650   2.462  0.02025 * 
#  sexm          33.644     11.284   2.982  0.00588 **
######################################################################

# Now we can use this trained model to predict a new observation.  Specifically we want to predict my weight using my height 

# I'm 75 inches tall
test_height <- 75

predicted_weight <- predict(size_model, newdata = data.frame(height = test_height))

predicted_weight
# let's plot this fitline

# scatterplot
ggplot(size, 
       aes(x = height, y = weight)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_point(aes(x = test_height, y = predicted_weight), 
             color = 'red', shape = 18, size = 5) 


# Let's now look at this prediction relative to my actual weight
actual_weight <- 250

# scatterplot
ggplot(size, 
       aes(x = height, y = weight)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_point(aes(x = test_height, y = predicted_weight), 
             color = 'red', shape = 18, size = 5) +
  geom_point(aes(x = test_height, y = actual_weight), 
             color = 'blue', shape = 18, size = 5)





# feel free to try it on your own
test_height <- 68
predicted_weight <- predict(size_model, newdata = data.frame(height = test_height))
actual_weight <- 200

# scatterplot
ggplot(size, 
       aes(x = height, y = weight)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_point(aes(x = test_height, y = predicted_weight), 
             color = 'red', shape = 18, size = 5) +
  geom_point(aes(x = test_height, y = actual_weight), 
             color = 'blue', shape = 18, size = 5)
