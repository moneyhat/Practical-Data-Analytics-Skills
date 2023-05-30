library(tidyverse)

# Go get your wine data
wine <- read_csv("https://docs.google.com/spreadsheets/d/13uZEUTfQg8TweeVWiYtF4BqCxNmRm9RpzC5cuetXw1s/gviz/tq?tqx=out:csv")

# explore
glimpse(wine)
unique(wine)

# make a model. Start with volatile_acidity predicting quality. What's your target? What's your feature? Remember the format to make a model is lm(TARGET ~ FEATURE, data = your_data_name)

############################### VOLITILE ACIDITY ###############################################

#H0: volitile acidity has no affect on wine quality
#HA: volitile acidity does have an affect on wine quality

volitile_acidity_model<- lm(quality ~ volatile_acidity, data = wine)

# get a summary for the model.  What can you tell about the direction of the effect from this?
summary(volitile_acidity_model)

# make a scatterplot along with fitline.  Does the fitline make sense when looking at your model summary?  Would you reject HO based on this?

ggplot(wine,
       aes(volatile_acidity, quality)) +
  geom_point() +
  geom_smooth(method = 'lm') 
  

# now do the same for the rest of the features. 

###################################ALCOHOL#################################################
alcohol_model <- lm(quality ~ alcohol, data = wine)

summary(alcohol_model)

ggplot(wine,
       aes(alcohol, quality)) +
  geom_point() +
  geom_smooth(method = 'lm',se = TRUE) +
  theme_classic() +
  theme(text = element_text(size = 20))

################################## PRICE ##################################################
price_model <- lm(quality ~ price, data = wine)

summary(price_model)

ggplot(wine,
       aes(price, quality)) +
  geom_point() +
  geom_smooth(method = 'lm',se = TRUE) +
  theme_classic() +
  theme(text = element_text(size = 20))

####################################### PH #############################################

pH_model <- lm(quality ~ pH, data = wine)

summary(pH_model)

ggplot(wine,
       aes(pH, quality)) +
  geom_point() +
  geom_smooth(method = 'lm',se = TRUE) +
  theme_classic() +
  theme(text = element_text(size = 20))

##################################### RESIDUAL SUGAR ###############################################

residual_sugar_model <- lm(quality ~ residual_sugar, data = wine)

summary(residual_sugar_model)

ggplot( wine,
        aes(residual_sugar, quality)) +
  geom_point() +
  geom_smooth(method = 'lm',se = TRUE) +
  theme_classic() +
  theme(text = element_text(size = 20))

####################################################################################

