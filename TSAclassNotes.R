dude2 <- read.csv(file.choose(), header=T)

attach(dude2)

head(dude2)


dude2 <- lm(Value ~ Revenue)

summary(dude2)
## y = 5.93X-601.93
##as X(revenue) increases by 1 unit then Y(value) increases by 5.93
#F-stat is 129.2 and P-value 5.29e-12 
#p-value is basically zero 
#if pvalue< Alpha - reject the null
#alpha is the level of significance
#Fstat is how likely those variables come together to make a good predictor
##how important is the set of variables 
#H0: set of variables are not significant
#H1: set of variables is significant

#T-value: 11.367
#is each variable individually significant?
#H0 : the slope of x is not significant
#H1 : the slope of x is significant
#Pvalue : 5.29e-12 therefore slope is extremely significant

#R-squared : .8219
#want to be above .5
#82.19% of the reason Y(value) varies is due to X(revenue)

##Use cooks distance to calculate outliers

cook1 <- cooks.distance(dude2)
plot(cook1)
#team 9 and team 21 seem to be outliers
qf(p=.5, df1=2,df2=28,lower.tail=F)
#F distribution having (k+1) df in the numerator and (n-k-1) df in the denominator at a 0.50 level of significance.  
#qnorm(p.25,lower.tail=F) = 1.96

#qf : .7105929 which is the cutoff, any pint above .7105 is influential
max(cook1)
#T9 is the only influential point according to cooks
#possibly remove this point

plot(dude2)
#Linear: plot1 slight bend
#Normal QQ : want all on line

#Independence:  want scatter across
#Equal Variance: want a scatter across
plot(dude2.Revenue,residuals(dude2))


######## Multiple Regression Models
warecost <- read.csv(file.choose(), header=T)
attach(warecost)
head(warecost)
waremod <- lm(Cost ~ Sales + Orders)
summary(waremod)
#equation
#Y = -.27825 + .04711X1 + 0.011947X2
#k is the number of X variables
#for f-test include:
#H0 - all of the slopes are zero meaning there is no relationship between the variables
#H1 - at least one slope is not 0 meaning there is a relationship between the variables
#fstat: 74.13 and pvalue: 3.043e-10 basically 0 (anything smaller than e-5 is basically 0)
##pvalue <alpha then reject the null
#what did you learn: at least one of the X variables has a non zero slope

#For t-test include:
#H0: slope is zero for Xi (Sales, Orders)
#H1: slope is not zero for Xi (Sales, Orders)
#Sales Tvalue: 2.318 p-value: .0306
##p-value< .05 meaning reject the null
#meaning the slope of Sales is statistically significant to the Costs model
#Orders Tvalue: 3.043e-10 p-value: 2.87e-5
##p-value< .05 meaning reject the null
#meaning the slope of Orders is statistically significant to the Costs model


confint(waremod)
#do not want 0 in any of the confidence intervals (negative to positive)

cook1 <- cooks.distance(waremod)
plot(cook1)
qf(p=.5, df1=2,df2=28,lower.tail=F)
cook1
#outliers for cooks

plot(waremod)
##plot(waremod,residuals(waremod))
#Check Residuals using LINE
#linearity - 
#independence - Derbin Watson
#Normality - (normal QQ plot) roughly a line good to go
#equal variance - 