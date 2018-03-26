##############################################################
#   Name      :   Krutika Bele
#   Purpose   :   Assignment 2 
##############################################################

library(data.table)
library(tidyverse)
library(ggplot2)
library(SciViews)

############Question 1 #########
context1 <- fread('attend.csv')

context1$attendrt <- context1$attend/32
context1$hwrt <- context1$hw/8

model1 <- lm(termGPA ~ priGPA + ACT + attendrt + hwrt , data = context1)

summary(model1)


# Predict the termGPA for a student with a 32 ACT and a 2.2 priGPA who attended 28 lectures and
#turned-in 8 homework assignments.

pred1 <- data.frame(priGPA = 2.2, ACT = 32.00, attendrt = (28/32), hwrt= 8/8)

predict(model1, newdata = pred1 ) # 2.909

#d. Predict the termGPA for a student with a similar attendance and homework pattern who had a 20 ACT
#and a 3.9 priGPA.
pred2 <- data.frame(priGPA = 3.9, ACT = 20.00, attendrt = (28/32), hwrt= 8/8)

predict(model1, newdata = pred2) # 3.4097


#f. Predict the termGPA for a student with a 25 ACT and a 3.0 priGPA who attends all the classes, but only
#…nishes half the homework assignments.

pred3 <- data.frame(priGPA = 3.0, ACT = 25, attendrt = (32/32), hwrt= 1/2)

predict(model1, newdata = pred3) # 2.771

pred4 <- data.frame(priGPA = 3.0, ACT = 25, attendrt = (16/32), hwrt= 8/8)

predict(model1, newdata = pred4) # 2.771



##### Answers ##############
# a. For every unit increase in the attendance rate of a student , its is associated with 1.05 increase in 
#   termGPA cotnrolling priGPA, ACT and homeworkrate.
# 
# b. For every unit increase in the homework rate of a student ,it is associated with .913 increase in 
# termGPA
# 
# c. The termGPA for a student with priGPA = 3.9, ACT = 20.00, attnended lectures =28 and 8 homework is 
# 2.909
# 
# d.Ther termGPA for a student with a attendance  of 28 lectures and  8 homework , a 20 ACT
# and a 3.9 priGPA is 3.4097
# 
# e.priGPA is more important for termGPA than ACT score
# 
# f.TermGPA for a student with a 25 ACT and a 3.0 priGPA who attends all the classes, but only
# …nishes half the homework assignments is 2.771
# 
# g.TermGPA for a student with a 25 ACT and a 3.0 priGPA who attends half of  the classes, but 
# …finishes all the homework assignments is 2.701
# 
# h.Attendance is improtant for the termgpa than the homework assignment.
# 
# i. Since Attendacne rate and homework rates are the ratios without any mesaure 
#but ACT and priGPA are not in the same scale.



#####Question 2 ###########


context2 <- fread('CEOSAL2.csv')



#ln [salaryi] = 0 + 1 ln [mktvali] + 2pro…tsi + 3ceoteni + ei

model2 <- lm(log(salary) ~ log(mktval)+ profits + ceoten , data = context2)

summary(model2)

#ln [salaryi] = 0 + 1 ln [mktvali] + 2pro…tsi + 3ceoteni + 4ln [salesi] + ei

model3 <- lm(log(salary) ~ log(mktval)+ profits + ceoten + log(sales) , data =context2) 


summary(model3)

# Models with log(y) as the dependent variable often more closely satisfy the classical linear
# model assumptions. For example, the model has a better chance of being linear, homoskedasticity
# is more likely to hold, and normality is often more plausible.
# 
# a. Taking log for the profit variable Logs are always taken on the variables which are always positive.
# Here if find out summary of the the profit values are in negatives so we are not taking the logs for profit variables.
# 
# b. For every 10% increase in the market value is associated with the 2.386% rise in the CEO salary.
# 
# c. For every 10% increase in the market value is associated with the 1.018% rise in the CEO salary.
# 
# d. Model 2 is baised towards mktval so the mktval is significant in the model 2. In model the 3 we introduced 
#  variable sale and the bias error in the model 2 is removed since the CEO salary is more significant with the
#  sale variable
#  
# e. coefficient of profits is not significant  
# 
# f. For every 10 % increase in sales is associated with the 1.622% rise in the CEO salary.


### Question 3 #####

#pricei = 0 + 1bdrmsi + 2 ln [lotsizei] + 3 ln [sqrfti] + 4coloniali + ei

context3 <- fread('hprice1.csv')

table(is.na(context3))
summary(context3)

model4 <- lm(price ~ bdrms + log(lotsize) + log(sqrft) + colonial, data = context3)

summary(model4)

#ln [pricei] = 0 + 1bdrmsi + 2 ln [lotsizei] + 3 ln [sqrfti] + 4coloniali + ei

model5 <- lm(log(price) ~ bdrms + log(lotsize) + log(sqrft) + colonial, data = context3)

summary(model5)

############## Interpretation #############

# 1. For every 10 % increase in the lotsize is associated with the $6144.6 increase in the price
#     Controlling the bdrms , sqrft and colonial
# 
# 2. For every 10 % increase in the lotsize is associated with the 1.6% increse in the price controlling
#   bdrms, sqrft and colonial
#   
# 4. If we compare the R-Squared values for the both of the models then we can conclude that model4 is best fit 
#  for the given data set. Also the variable in the model4 are more significant with price than that of model5.
# 
# 3. If the home is colonial style then the increase in the price is $4134. 
# 
# 5. If we put our data in the model4 then it is good fit for deciding to the investment on the house or not .
#   Since if we increase in bedroom in the house then price will increase by $18,572. Also this addition of the bedroom 
#   will add 10% space which again adds cost $22,550. So total there will be increase of $41,122. So adding another $20K enjoyment 
#   will increase the price of the house total by $61,122. So the model is appropriate to the expansion since we are considering 
#   ily $50K increase in the price but as per model the increase actually is $61K.
#   
##### Question 4 #####
context4 <- fread('JTRAIN2.csv')

summary(context4)
  #re78i = 0 + 1re75i + 2traini + 3educi + 4blacki + ei

model6 <- lm(re78 ~ re75 + train + educ + black, data = context4)


summary(model6)


######## Interpertations#########
# 
# a. In 1978 the income is increased by $146 when compared with income in 1975 and controlling  train, educ, balck 
# parameters.
# 
# b. If the job training is assigned then the increase in the earning of is associated with $1684.22 
# controlling education income in 75 and black race 
# 
# c. If the race is black then there is $2211.7 decrease in the annual income of men  in 1978  controlling training,
# educatiion and income in 1975.
