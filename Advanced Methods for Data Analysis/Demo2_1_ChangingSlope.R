## ----R Demo 2.1 slope-varies-with-location
# Updated, Jan 2019

############################################################
# Part a: Generate three different distributions for X;
# assume same nonlinear model
############################################################

# Create three distributions for X
x1 = runif(100)                # X~Uniform(0,1)
x2 = rnorm(100,0.5,0.1)        # X~Normal(0.5,0.1)  
x3 = runif(100,2,3)            # X~Uniform(2,3)

# Create matching Y variables from the same (nonlinear) model
# Y = sqrt(X) + N(0.05^2)
y1 = sqrt(x1) + rnorm(length(x1),0,0.05)
y2 = sqrt(x2) + rnorm(length(x2),0,0.05)
y3 = sqrt(x3) + rnorm(length(x3),0,0.05)

# Plot the first set of (X,Y) points (green squares), making sure the plotting region is big
  # enough for all the later ones
plot(x1,y1,xlim=c(0,3),ylim=c(0,3), xlab="X", ylab="Y", col="darkgreen",pch=15)

# Rugs for the those points, to indicate the marginal distribution
rug(x1,side=1, col="darkgreen")
rug(y1,side=2, col="darkgreen")

# Add the second set of points in a different color and plotting symbol (blue circles)
points(x2,y2,pch=16,col="blue")
rug(x2,side=1,col="blue")
rug(y2,side=2,col="blue")

# And the third (red triangles)
points(x3,y3,pch=17,col="red")
rug(x3,side=1,col="red")
rug(y3,side=2,col="red")


############################################################
# Part b: Fit linear regression lines to each data set
############################################################

# Fit the regression lines and add them, in matching colors
lm1 = lm(y1 ~ x1)
abline(lm1$coefficients, col="darkgreen", lty="dotted")

lm2 = lm(y2 ~ x2)
abline(lm2$coefficients,col="blue", lty="dashed")

lm3 = lm(y3 ~ x3)
abline(lm3$coefficients,col="red", lty="dotdash")

############################################################
# Part c: Combine the data and fit a regression line to all data
############################################################

# Combine the data, fit an over-all regression line
x.all=c(x1,x2,x3)
y.all=c(y1,y2,y3)
lm.all = lm(y.all~x.all)
abline(lm.all$coefficients,lty="solid")

# The true regression curve.
curve(sqrt(x),col="grey",add=TRUE)

legend("topleft", legend = c("Unif[0,1]", "N(0.5, 0.1)", "Unif[2,3]", "Union of above", "True regression line"), col = c("darkgreen", "blue", "red", "black", "grey"), pch = c(15, 16, 17, NA, NA), lty = c("dotted", "dashed", "dotdash", "solid", "solid"))
                            