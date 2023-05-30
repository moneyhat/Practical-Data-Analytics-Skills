library(tidyverse)

telco <- read_csv("https://docs.google.com/spreadsheets/d/1-gKtn7Qi-CeBdvo7sps7wF5CGKObG0OpFqJWL8aNnOA/gviz/tq?tqx=out:csv")


# Today is pretty straight-forward.  We have a dataset looking at churn in a telcom company. In other words, if the individual stayed a customer or not.  Your goal is to explore it, and then run t-tests to look at how the various other features in the dataset relate churn.  

# You need to make a hypothesis and set of predictions for each, then make a boxplot and t-test, and describe how these influence your hypothesis. 

glimpse(telco)
unique(telco)

t.test()

#Tenure
#H0: There is no difference in the tenure and churn (turnover).
#HA: There is a significant difference in tenure and churn (turnover).
t.test(tenure~churn, data=telco)

ggplot(data = telco, 
       mapping = aes(x=churn, y =tenure , color = tenure)) +
  xlab("Churn") +
  ylab("Tenure") +
  geom_jitter() +
  geom_boxplot()
  


#Conclusion: with p-value < 2.2e-16 we can conslude that there is a significant difference in tenure and churn, the confidence interval that floats around the 

#Monthly_Charges
#H0: There is no difference in monthly charges per line and churn
#HA: There is a significant difference in monthly charges per lineand churn
t.test(monthly_charges~churn, data = telco)

ggplot(data = telco, 
       mapping = aes(x=churn, y =monthly_charges , color = monthly_charges)) +
  xlab("Churn") +
  ylab("Monthly Charges") +
  geom_jitter() +
  geom_boxplot()

#Conclusion: with p-value < 2.2e-16 we can reject the null and conclude that there is a significant difference between Monthly Charges and churn.


#Total_Charges
#H0: There is no difference in Total Monthly Charges and Churn
#HA: There is a significant difference in Total Monthly Charges and Churn
t.test(total_charges~churn, data = telco)

ggplot(data = telco, 
       mapping = aes(x=churn, y =total_charges , color = total_charges)) +
  xlab("Churn") +
  ylab("Total Charges") +
  geom_jitter() +
  geom_boxplot()

#Conclusion: with p-value < 2.2e-16


#Age
#H0: There is no difference in Age and churn
#HA: There is a significant difference in Age and churn
t.test(age~churn, data = telco)

#Number of Kids
#H0: There is no difference in Number of Kids and churn
#HA: There is a significant difference in Number of Kids and churn

#Conclusion: with a p-value = 0.9515 we cannot reject the null and accept that there is no difference in the number of kids a client has and churn


#Credit_Score
#H0: There is no difference in Credit Score and churn
#HA: There is a significant difference in Credit Score and churn
t.test(credit_score~churn, data = telco)

#Conclusion: with p-value = 0.06319 we cannot reject the null and accep that there is no difference in credit score and churn


