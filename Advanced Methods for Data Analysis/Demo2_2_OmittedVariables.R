## ----R Demo 2.2 Omitted Variables Bias

############################################################
# Part a: Make correlated variables X (which is included in the regression)
# and Z (which is omitted)
############################################################

library(lattice) # for making 3D scatter plots with 'cloud' command
library(MASS)  # for multivariate normal generator

# Make correlated normal variables X and Z
x.z = mvrnorm(100,c(0,0),matrix(c(1,0.1,0.1,1),nrow=2))   # 100 \times 2 - matrix
# Y = X+Z + small noise
y = x.z[,1] + x.z[,2] + rnorm(100,0,0.1) 
# 3D scatterplot
cloud(y~x.z[,1]*x.z[,2],xlab="X",ylab="Z",zlab="Y")


############################################################
# Part b: Change the correlation between X (which is included in the regression) and Z (which is omitted)
############################################################

# Change the correlation between X and Z to -0.1 instead of +0.1
new.x.z = mvrnorm(100,c(0,0),matrix(c(1,-0.1,-0.1,1),nrow=2))
new.y = new.x.z[,1] + new.x.z[,2] + rnorm(100,0,0.1)
cloud(new.y~new.x.z[,1]*new.x.z[,2],xlab="X",ylab="Z",zlab="Y")


############################################################
# Part c: Omit Z and plot Y versus X
############################################################
# Now omit Z and plot
plot(x.z[,1],y,xlab="x",xlim=range(c(x.z[,1],new.x.z[,1])),ylim=range(c(y,new.y)))
# Make sure the range encompasses both data sets!
rug(x.z[,1],side=1)
axis(y,side=2)

points(new.x.z[,1],new.y,col="blue")
rug(new.x.z[,1],side=1,col="blue")
rug(new.y,side=2,col="blue")

############################################################
# Part d: Regress Y on X (with Z omitted)
############################################################
# ... and regress
old.lm = lm(y ~ x.z[,1])
abline(old.lm$coefficients)

new.lm = lm(new.y ~ new.x.z[,1])
abline(new.lm$coefficients,col="blue")

