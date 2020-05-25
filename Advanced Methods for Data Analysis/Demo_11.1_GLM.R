# Demo 11.1: Generalized Linear Models (Poisson Regression)
# Smoking and number of deaths due to coronory heart disease 
# (Ref: Based on Ex 9.2.1 in Dobson 2002)

#####################################################
# Input and plot data
#####################################################

# Data: Deaths from chd after 10 years among British male doctors
# categorized by age and smoking status in 1951.

deaths = c(32,104,206,186,102,2,12,28,28,31)  # expected number of deaths
age    = c(40,50,60,70,80,40,50,60,70,80)
py     = c(52407,43248,28612,12663,5317,18790,10673,5710,2585,1462) # number of doctors at risk in each age group ("person-years")
smoke  = c(1,1,1,1,1,0,0,0,0,0)

# Deaths from coronary heart disease per 100,000 person years 
# for smokers (circles) and non-smokers (squares)
x=age
y=deaths/py*100000
plot(x,y,xlab="Average age in each age group", ylab="Deaths per 100,000 person years",pch=smoke)
legend(40,2000,c("smokers","non-smokers"),pch=c(1,0))

# logarithm of death rate
logy=log(deaths)-log(py)
plot(x,logy,xlab="Average age in each age group", ylab="log(death rate)",pch=smoke)
legend(55,-7,c("smokers","non-smokers"),pch=c(1,0))


# Using the available covariates, and including potential interactions and quadratic terms, we explore the models prediction of the rate of deaths due to coronary heart disease.... From this class of models we chose a model that includes a quadratic term for age and an interaction term for age and smoking status.
agesq  = age*age  # to take into account of the non-linear rate of increase
sm.age = smoke*age # to describe a differential rate of increase with age

# Questions of interest:
# 1. Is the death rate higher for smokers than non-smokers?
# 2. If so, by how much?
# 3. Is the differential effect related to age?


#####################################################
# MODELING: Poisson Regression on Rate Data
#####################################################
#Notice the use of the offset for person-years below
#
myGlm = glm(deaths~smoke+age+agesq+sm.age, offset=log(py),family=poisson)
summary(myGlm)
summ = summary(myGlm)

# Are interactions necessary?
myGlm0 = glm(deaths~smoke+age+agesq,family=poisson) # model without interaction
anova(myGlm0, myGlm,test="Chisq")  # drop in deviance=13.017 with 1 degree of freedom
# 1 - pchisq(13.017,1)  # p-value = 0.00031: clearly significant

#####################################################
# INFERENCE
# Is the death rate higher for smokers than non-smokers, and if so, by how much?
#####################################################

beta=myGlm$coefficients # Extract estimated coefficients
print(beta)
# Compare rates of smokers to nonsmokers with age=40 in our population
exp(beta[2]+40*beta[5])  #    3.106274
# Interpretation: Smokers in the age=40 group have a death rate due to coronory heart disease that is 3.1 times higher than non-smokers 

##
# Alternative calculation of effect of smoking (but don't confuse association with causation!):
##
summ = summary(myGlm)
v    = summ$dispersion * summ$cov.unscaled 
  #summ$dispersion is 1 unless we allow "over dispersion" 
  #relative to the model. This is a topic I skipped over.
  #
print(v)   # var(beta_hat) 
ell  = c(0,1,0,0,40)    
gam  = sum(ell*myGlm$coef)    # =beta2_hat+40*beta5_hat  
print(exp(gam))  # 3.106274 


#################################
# Compute confidence intervals:
#################################
se   = sqrt(ell %*% v %*% ell)
ci   = exp(c(gam- 2*se, gam+ 2*se))
print(round(ci,2))     # [1.77, 5.45]; compare with point estimate 3.1

# Interpretation: Smokers in the age=40 group have a death rate due to coronory heart disease that is 2 to 5 times higher than non-smokers in the same age group.
# (Since heart disease is much more common than lung cancer, the risk of smoking has a bigger impact on public health for heart disease than smoking.)

#####################################################
# GOODNESS OF FIT (deviance test)
#####################################################
summary(myGlm)    # Residual deviance:   1.6354  on 5  degrees of freedom
# This LRT statistic is approximately distributed as a chi-square deviate with n-q degrees of freedom, where q is the number of covariates. If D is larger than expeced (i.e., the p-value is small) this means that the Poisson model with the covariates included is not sufficient to explain the data.

# Global goodness-of-fit
print(1-pchisq(myGlm$deviance,df=5))    #p-value = 0.897
# Conclusion: For these data, the model appears to fit well.

# Plot fitted model
plot(x,logy,xlab="Average age in each age group", ylab="log(death rate)",pch=smoke)
p = myGlm$linear.predictors
logy_fit=p-log(py)
#logy_fit = predict.glm(myGlm)-log(py)
lines(x[smoke==0],logy_fit[smoke==0])
lines(x[smoke==1],logy_fit[smoke==1])

####################################
# ADDED: plots of deviance residuals
####################################
# The global fit is good, but check residuals also, to detect more subtle deviations from the assumptions (goodness-of-fit tests are not usually powerful, so they don't have much power to detect deviations from the assumed model. Moreover, they rely on large-sample theory)

# Create residuals:
res = resid(myGlm,type="deviance")
plot(x,res,pch=19,xlab="linear predictor",ylab="deviance residuals")
print(sum(res^2))   # Note: returns the (global) deviance

# Create Standardized Residuals:
stdres = rstandard(myGlm)
plot(x,stdres,pch=19,xlab="linear predictor",ylab="standardized deviance residuals")

# QQ Plot:
qqnorm(scale(stdres))
abline(c(0,1))

