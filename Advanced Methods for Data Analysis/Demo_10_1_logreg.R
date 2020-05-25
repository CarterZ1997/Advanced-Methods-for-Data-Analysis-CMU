library(Sleuth2)
#colnames(case2101)
attach(case2101)
data=case2101

names(data)
head(data)

logArea = log(Area)       # log(Area)  
plot(logArea,Extinct, xlab="log(Area) [km^2]",ylab="Number of Extinct Species")

p = Extinct/Atrisk   # Proportion Extinct
plot(logArea, p, xlab="log(Area) [km^2]",ylab="Proportion Extinct")

plot(logArea, qlogis(p),xlab="log(Area) [km^2]",ylab="Logit(Proportion Extinct)")