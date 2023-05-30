# The goal of today is to dig into ggplot a bit more.  We've dabbled and I've given you competed code, but now we're going to work on understanding the syntax along with pros and cons to different types of common plots.

### Learning objectives
# When to use what type of plot
# categorical variables
# continous variables
# ggplot syntax
# making things readable and more useful

# ggplot is the plotting library within tidyverse.  It's rapidly become the go-to package for plotting data in R given it's overall simplicity and range of options.

# As we talked about in class, the tidyverse website is a great resource: https://ggplot2.tidyverse.org/reference/


# load packages right away
library(tidyverse)

#bring in our admin data - this is already cleaned
admin <- read_csv("https://docs.google.com/spreadsheets/d/1FOTs_wH39OevX2DayChKNzRIsmXjcyLGAa1Q06ZBr8E/gviz/tq?tqx=out:csv")


# Just run these for now...  get into what they do in a future lesson
admin$uni_rank <- as.factor(admin$uni_rank)
admin$research <- as.factor(admin$research)
admin$LOR_factor <- as.factor(admin$LOR)

# There are there parts 1) your data, 2) your aesthetic or aes(), and 3) then your geom.  You can add all sorts of things within your aes and your geom to increase the clarity and quality of your plots.  You can also layer geoms to create more detailed plots. 

##################################################################
### Histograms

# let's start with a super basic histogram in our admin data

ggplot(admin, # here's your data
       aes(x = GRE_score)) +      # then your aesthetic
  geom_histogram() # then add on the type of plot... note the + in the previous line


# We might want to change bin width on our histogram as right now it seems a bit narrow.  You can specify that within geom_histogram.  Take some time to play with it. How does it change the plot?

ggplot(admin,
       aes(x = GRE_score)) + 
  geom_histogram(binwidth = 4)  


# We can also tweak our axis labels and titles to make things more readable. You literally just add on labs() and specify x, y ,title, and subtitle.  Grab the code above then add things on

ggplot(admin,
       aes(x = GRE_score)) + 
  geom_histogram(binwidth = 4) +
  labs( x = 'GRE Score', y = 'Number', title = 'Basic Histogram', subtitle = '... of GRE Scores')


# we can make things a bit cleaner looking to with a theme.  In this case, add theme_classic() to the plot above and see what happens. 

ggplot(admin,
       aes(x = GRE_score)) + 
  geom_histogram(binwidth = 4) +
  labs( x = 'GRE Score', y = 'Number', title = 'Pretty Histogram', subtitle = '... of GRE Scores') +
  theme_classic() + 
  theme(text = element_text(size = 20))


##################################################################
### Box vs. Barplots

# Histograms are really useful to show how your data is distributed, but they don't contain much information beyond that.


# What if you want to make a plot showing the mean GRE scores of the groups that did and did not do research?  Just run this below to make a bar plot.  We haven't learned group_by or summarize yet, so don't worry if you don't understand the code

admin %>%
  group_by(research) %>%
  summarize(mean = mean(GRE_score)) %>%
  ggplot( aes(x = research, y = mean)) + 
  geom_col()


# now let's make the same graph in boxplot form.  We need to fill in our aesthetic with research on the x and GRE_score on the y to visualize if doing research impacts GRE scores. Also the geom for boxplot is just geom_boxplot()
  
ggplot(admin,
       aes(_ = _____, _ = _______)) + 
  _____________

# what are some pros and cons to each?




# we can also layer geom's to add more info to the plot itself.  For example, what if you add geom_point() to it?

ggplot(admin,
       aes(x = research, y = GRE_score)) +
  geom_boxplot() + 
  geom_point()


# try geom_jitter instead?  Add the argument width = 0.1 within the geom_jitter() function

ggplot(admin,
       aes(x= research, y = GRE_score, color = research)) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c('No', 'Yes')) + 
  geom_jitter(width = 0.1)


# what does adding the points do?  What does the jitter do differently than just geom_points?



# polish it up!

ggplot(admin,
       aes(x= research, y = GRE_score)) + 
  geom_boxplot() + 
  scale_x_discrete(labels = c('No', 'Yes')) + 
  geom_jitter(width = 0.1) + 
  xlab('Did Research') + ylab('GRE Score') +
  theme_classic() + 
  theme(text = element_text(size = 20)) 


##################################################################
### Scatterplots 

# So far, we've only been plotting the relationship between some categorical variable and then a continous one.  For example, in the previous figures out categories were if they did research or not (1 or 0 for yes or no) against GRE scores which can have a range of values from 0-360



# If you have two continous variables, you'll likely want a way to display the relationship among a bunch of points (e.g. height vs. weight for 100's of individuals). 

ggplot(admin,
       aes(x = GRE_score, y = chance_admit)) +
  geom_point()


# But, we can do better!  This is a really clear relationship, but sometimes it's less clear and thus a fit line helps a lot.  Let's add one on using geom_smooth(). Be sure to add the argument method='lm' within geom_smooth()

ggplot(admin,
       aes(x = GRE_score, y = chance_admit)) + 
  geom_point() + 
  geom_smooth(method = 'lm')


# It's clear that there's a lot of variation still in this relationship, especially in the bottom end.We already made a boxplot for doing research or not, but can we just add that information here?  Color coding a plot is a good way to add categorgical variables into a scatterplot.  Try adding color = research into your aesthetic aes(...). 

ggplot(admin,
       aes(x = GRE_score, y = chance_admit, color = research)) +
  geom_point() + 
  geom_smooth(method = 'lm')


# is it really worth having the two lines? 

ggplot(admin,
       aes(x = GRE_score, y = chance_admit)) + 
  geom_point(aes(color = research)) + 
  geom_smooth(method = 'lm')


# OK, let's add a final dimension... let's scale the size of the point by how good the letters of rec were.  Since we want to scale the points (geom_point()), we need to add the argument size = LOR in there.  This is saying 'scale the size of the points by the LOR score.  

ggplot(admin,
       aes(x = GRE_score, y = chance_admit)) + 
  geom_point(aes(color = research, size = LOR)) +
  scale_color_brewer(palette = 'Dark2')


# Try adding this on the end + scale_color_brewer(palette = 'Dark2')


# Can you change it so it considers the quality of the university the student came from?

ggplot(admin,
       aes(x = GRE_score, y = chance_admit)) + 
  geom_point(aes(color = research, size = uni_rank, alpha = 1/100)) +
  scale_color_brewer(palette = 'Dark2')


# Can save the plot
ggsave('my_plot.jpg', height = 8, width = 8)


  
