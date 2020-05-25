# Demo 11.2: Fitting Generalized Additive Models
#####

# Additive logistic regression example on Wage data from the ISL book
library(ISLR)
library(mgcv)
colnames(Wage)

###
# Generalized Linear Model
###
# Recall Demo 8.1: We saw wage, year, age and education as well as
# some other variables.
#
# health_ins is a categorical variable indicating whether people have
# health insurance.  We can look at how having health insurance is
# related to the other variables

################################################
# GLM of insurance on year,age, education, wage
################################################
higlm = glm(health_ins~year+age+education+wage,family=binomial,data=Wage)  
summary(higlm)
higlm$deviance

################################################
# Generalized Additive Model (Additive Logistic Regression)
################################################
higam = gam(health_ins ~ s(year,k=3) + s(age,k=6) +
            education+s(wage,k=6), data=Wage, family="binomial") 

plot(higam, se=T,scale=0,pages=1,all.terms=T)
summary(higam)
higam$deviance

################################################
# Test whether the non-linear terms add anything
################################################
anova(higlm,higam,test="Chisq")

################################################
# Confidence interval for a new probability via linear predictor
################################################
pred=predict(higam,newdata=data.frame(year=2009,age=65,wage=100,
	education="5. Advanced Degree",type="link"),se.fit=T)

# Use the logistic function to get back to probability scale

logistic=function(x){exp(x)/(1+exp(x))}

logistic(c(pred$fit-2*pred$se.fit,pred$fit+2*pred$se.fit))



