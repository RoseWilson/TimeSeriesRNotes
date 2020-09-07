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

confint(waremod)

###