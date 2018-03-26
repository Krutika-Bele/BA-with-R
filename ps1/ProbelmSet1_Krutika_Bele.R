rm(list=ls(all=TRUE))

## Import packages
library(data.table)

## Data import and validation
context1   <- fread('WAGE1.csv')
summary(context1)

# Generate a new variable that is the natural logarithm of wage
lwage <- log(context1$wage)

# Run Models
model1 <-    lm(wage~ educ, data = context1)
model2 <-    lm(wage~ educ + exper + tenure, data = context1)
model3 <-    lm(lwage~ educ + exper + tenure, data = context1)

#Summarize Models
summary(model1)
#Call: lm(formula = wage ~ educ, data = context1)
#Residuals:
#  Min      1Q  Median      3Q     Max 
# -5.3707 -2.1578 -0.9854  1.1864 16.3975 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) -0.93389    0.68769  -1.358    0.175    
#  educ         0.54470    0.05346  10.189   <2e-16 ***
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 3.392 on 524 degrees of freedom
# Multiple R-squared:  0.1654,	Adjusted R-squared:  0.1638 
# F-statistic: 103.8 on 1 and 524 DF,  p-value: < 2.2e-16


summary(model2)
#Call:
#  lm(formula = wage ~ educ + exper + tenure, data = context1)

# Residuals:
#  Min      1Q     Median   3Q     Max 
# -7.6498 -1.7708 -0.6407  1.2051 14.7201 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.91354    0.73172  -3.982 7.81e-05 ***
# educ         0.60268    0.05148  11.708  < 2e-16 ***
# exper        0.02252    0.01210   1.861   0.0633 .  
# tenure       0.17002    0.02173   7.825 2.83e-14 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 3.096 on 522 degrees of freedom
# Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3032 
# F-statistic: 77.15 on 3 and 522 DF,  p-value: < 2.2e-16


summary(model3)
#Call:
#  lm(formula = lwage ~ educ + exper + tenure, data = context1)

# Residuals:
#  Min       1Q      Median    3Q      Max 
# -2.05911 -0.29563 -0.03302  0.28590  1.42657 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.282635   0.104331   2.709  0.00697 ** 
# educ        0.092256   0.007340  12.569  < 2e-16 ***
# exper       0.004137   0.001726   2.397  0.01687 *  
# tenure      0.022112   0.003098   7.138 3.19e-12 ***
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.4415 on 522 degrees of freedom
# Multiple R-squared:  0.3165,	Adjusted R-squared:  0.3125 
# F-statistic: 80.56 on 3 and 522 DF,  p-value: < 2.2e-16


## INTERPRETATIONS :
#model1
#a : Every 1 year increase of education of workers is expected to increase the wage by $0.54/ hour 
#     for workers, controlling for other factors

#model2
#b : For every one year increase in education for workers is expected to increase the wage by $0.60/ hour
#     for workers, controlling for experience and tenure of employment

#c : According to model2, Experince is not expected a significant determinant or
#     it does not effect the wage for workers

#d : For every one year increase in tenure of worker with the current employer is expected to increase
#     the wage by $0.17/ hour for worker, controlling for years of education and years of experince

#e : For a worker with no education, no experience and new to employer, the model predicts negative $2.91/ hour
#     of wage, i.e. the worker might have to pay the employer to learn the work

# model3 (log/percentage change of wage)
#f : For every one year increase in education for workers is expected to increase 9.22 percentage
#     point of wage of workers, controlling for experience and tenure of employment 

#g : For every one year increase in experience for workers is expected to increase 0.41 percentage
#     point of wage of workers, controlling for years of education and tenure of employment 

#h : For every one year increase in tenure of worker with current employer is expected to increase
#     2.22 percentage point of wage of worker, controlling for years of education and years of experince


