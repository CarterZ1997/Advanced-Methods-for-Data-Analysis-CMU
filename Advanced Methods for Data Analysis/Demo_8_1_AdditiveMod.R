# Additive model example on Wage data from the ISL book

library(ISLR)
names(Wage)
attach(Wage)

par(mfrow=c(1,3))
plot(year,wage)
plot(age,wage)
plot(education,wage,xlab="Education Level",ylab="Wage")

# Fit an additive model of wage on year, age, and education,
# with smoothing splines for year and age, and a step function
# for education 
library(gam)
M3 = gam(wage~s(year,4)+s(age,5)+education, data=Wage)

par(mfrow=c(1,3))
plot.Gam(M3, se=TRUE, col="blue")  # plotting with plot.Gam

# Looks like the nonlinear effect of year may be suspect;
# let's test it
M1 = gam(wage~s(age,5)+education, data=Wage) # exclude year
M2 = gam(wage~year+s(age,5)+education, data=Wage) # linear function of year

anova(M1,M2,M3,test="F")

#Analysis of Deviance Table

#Model 1: wage ~ s(age, 5) + education
#Model 2: wage ~ year + s(age, 5) + education
#Model 3: wage ~ s(year, 4) + s(age, 5) + education
#
#    Resid. Df   Resid. Dev   Df   Deviance   F        Pr(>F)    
#1      2990     3711731                                  
#2      2989     3693842       1    17889.2    14.4771  0.0001447 ***
#3      2986     3689770       3     4071.1     1.0982  0.3485661    

# According to the F-test, there is strong evidence that a GAM with a linear function of year is better than a GAM that does not include year at all. But, there is no evidence that a non-linear function of year is needed. In other words, Model 2 is preferred.

par(mfrow=c(1,3))
plot.Gam(M2, se=TRUE, col="blue")


