# Demo_bootg.R (Lecture 5, Part II)
#
#  Fit splines to Geyser data
#  Bootstraps cases to get CI's 
#  Add OLS line to spline fit

####################################################
# Fit smoothing spline using default value of lambda
####################################################
library(MASS)  # The data are supplied as part of this library
data(geyser)
names(geyser)
y=geyser$waiting  # Set the names to match earlier discussion
x=geyser$duration
out=smooth.spline(y~x)  # This syntax actually works
lambda0=out$lambda   # internally chosen smoothing parameter 

####################################################
# Plot residuals from spline fit
####################################################
#out$residuals=y-predict(out,x=x)$y  # Create the usual residuals
#plot(x,out$residuals)
plot(x,residuals(out),xlab="Duration",ylab="Residuals", main="Smoothing Spline Residuals") # Figure 3

####################################################
# Linear fit
####################################################
plot(x,y,xlab="Duration",ylab="Waiting",
main="Geyser Data with Linear and Spline Fits") # Begin Figure 2
lmgeyser=lm(waiting~duration,data=geyser)  # Linear fit
abline(lmgeyser$coef) # Add linear fit to Figure 2

##################################################################
# Add spline fit to same plot using "predict" for equally spaced point
##############################################################
x0=0.5+5*c(0:100)/100 # Equally-spaced points for spline fit
out$pred=predict(out,x=x0)$y # Predictions at x0
lines(x0,out$pred,lty=2) # Add least squares line to Figure 2
legend("bottomleft",lty=c(1:2),legend=c("Least squares",
"Smoothing spline")) # Add legend to Figure 2

####################################################################
# Compute bootstrap CI for Expectation[spline fit] by resampling cases
####################################################################
B=1000
n=length(x)
thecurves=NULL
thecurves2=NULL
for(b in 1:B){
   cases=sample(n,replace=T)
   outb=smooth.spline(y[cases]~x[cases],lambda=lambda0) # fixed lambda (HW 6)
   thecurves=rbind(thecurves,predict(outb,x=x0)$y)
   outb=smooth.spline(y[cases]~x[cases])     # choose new lambda for each bootstrap sample (prefered)
   thecurves2=rbind(thecurves2,predict(outb,x=x0)$y)
}
quants=apply(thecurves,2,quantile,prob=c(0.025,0.975))
lines(x0,2*out$pred-quants[1,],lty=3,col="blue")
lines(x0,2*out$pred-quants[2,],lty=3,col="blue")
quants2=apply(thecurves2,2,quantile,prob=c(0.025,0.975))
lines(x0,2*out$pred-quants2[1,],lty=3,col="red")  # pivotal bootstrap CIs
lines(x0,2*out$pred-quants2[2,],lty=3,col="red")
legend("bottom",lty=c(3,4),col=c("blue","red"),legend=c("95% CI's (one lambda)","95% CI's (bootstrap lambda)"))
