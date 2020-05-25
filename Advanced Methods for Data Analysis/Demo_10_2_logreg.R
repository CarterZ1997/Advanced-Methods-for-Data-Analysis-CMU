# Sigmoid
x = seq(-10,10,length=1000)
plot(x,y = exp(x)/(1+exp(x)),type="l")

# Log odds
p = seq(0,1,length=1000)
plot(p,log(p/(1-p)),type="l")

#####
# South African heart disease data from ESL book
heart = read.table("SAheart.data",sep=",",head=TRUE,row.names=1)
sa.data=heart
attach(sa.data)

# Fit a logistic regression of chd (chronic heart disease)
# on all variables
logmod=glm(chd~., family="binomial", data=sa.data)

# Standard inferential tools
summary(logmod)
confint(logmod)

# Backwards variable selection
logmod2 = step(logmod,direction="backward")

summary(logmod2)
confint(logmod2)

exp(logmod2$coef)
exp(confint(logmod2))  
# Thus an increase of 1 kg in lifetime tobacco usage accounts for an increase in the odds of coronary heart disease of exp(0.080)=1.084 or 8.4%. Incorporating the standard error we get an approximate 95% confidence interval [exp(0.031),exp(0.133)]=[1.03,1.14].

p = logmod2$fitted.values
names(p)=NULL
n = nrow(sa.data)
predict  = rep(0,n)
predict[p > .5] = 1
print(table(chd,predict))
error = sum( ((chd==1)&(predict==0)) | ((chd==0)&(predict==1)) )/n
print(error)    # in-sample error rate (i.e. training error rate )