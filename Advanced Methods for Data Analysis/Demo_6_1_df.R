# Demo_6_1_df.R

################################################
# Create a simple nonlinear example
################################################
set.seed(0)
n = 500
x = sort(runif(n,0,6*pi))
r = x*sin(x) 
y = r + rnorm(n)     # y = x*sin(x) + eps, where eps~N(0,1)

plot(x,y,col="gray50")
lines(x,r,col="blue",lwd=2)

################################################
# Kernel regression
################################################
bws = c(5,1,0.1)
kernmod1 = ksmooth(x,y,kernel="normal",bandwidth=bws[1])
kernmod2 = ksmooth(x,y,kernel="normal",bandwidth=bws[2])
kernmod3 = ksmooth(x,y,kernel="normal",bandwidth=bws[3])

par(mfrow=c(1,3))
plot(x,y,col="gray50",main=paste("bandwidth =",bws[1]))
lines(kernmod1$x,kernmod1$y,col="red",lwd=2)

plot(x,y,col="gray50",main=paste("bandwidth =",bws[2]))
lines(kernmod2$x,kernmod2$y,col=3,lwd=2)

plot(x,y,col="gray50",main=paste("bandwidth =",bws[3]))
lines(kernmod3$x,kernmod3$y,col="blue",lwd=2)

################################################
# Regression splines
################################################
library(splines)
knots = seq(2,16,length=6)     # choose N=6 evenly spaced knots
#G = bs(x,knots=knots,degree=3)  # generate B-spline basis matrix for cubic spline; dimension: n-by-N matrix
G = ns(x,knots=knots) # natural spline
regmod = lm(y~G)  # regress y on B-spline basis vectors (OLS)

par(mfrow=c(1,1))
plot(x,y)
lines(x,regmod$fitted,lwd=3,col="blue")
abline(v=knots,lty=3,lwd=3)

################################################
# Smoothing splines
################################################
win.graph()
lambdas=c(0.01, 2E-5,1E-20)
# lambda=0.001 corresponds to df=10
# lambdas=c(1000,1.876005e-05,1E-20)


splinemod1 = smooth.spline(x,y,lambda=lambdas[1])
splinemod2 = smooth.spline(x,y,lambda=lambdas[2])
splinemod3 = smooth.spline(x,y,lambda=lambdas[3])

par(mfrow=c(1,3))
plot(x,y,col="gray50",main=paste("lambda =",lambdas[1]))
lines(splinemod1$x,splinemod1$y,col="red",lwd=2)

plot(x,y,col="gray50",main=paste("lambda =",lambdas[2]))
lines(splinemod2$x,splinemod2$y,col=3,lwd=2)

plot(x,y,col="gray50",main=paste("lambda =",lambdas[3]))
lines(splinemod3$x,splinemod3$y,col="blue",lwd=2)

# REMARKS: You can use k-fold CV for model selection and model comparison (see Lecture 3).

# In addition, you can compute the effective degrees of freedom for the kernel smoother to put the CV-curves of the two methods in the same plot (to be discussed)

# ksmooth is R's default kernel regression smoother; better kernel smoothers are available in other packages such as "np" (see e.g. HW 1)