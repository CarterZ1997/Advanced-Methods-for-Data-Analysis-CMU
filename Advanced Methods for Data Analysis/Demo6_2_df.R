# R Demo 6.2 Estimating the Effective Degrees of Freedom via the Bootstrap

################################################
# Create example data for linear regression
################################################
set.seed(0)
n = 100
p = 20
x = matrix(rnorm(n*p),nrow=n,ncol=p)
beta = 2*runif(p)  # fixed parameters (although I generated these here)
y = x %*% beta + rnorm(n)     # linear function + iid normal errors

##############################################################
# Part a: Use simulation to estimate degrees of freedom
# This estimate is close to the true value, df=p
##############################################################
R = 500
y.real = matrix(0,R,n)
yhat.real = matrix(0,R,n)

for (r in 1:R) {
  y.r = x %*% beta + rnorm(n)

  y.real[r,] = y.r
  yhat.real[r,] = lm(y.r~x+0)$fitted
}

# Function to compute trace of a matrix
tr = function(A) { return(sum(diag(A))) }

tr(cov(y.real,yhat.real)) # should be about p (for linear regression)



##############################################################
# Part b: Use "residual bootstrap" to estimate degrees of freedom
#############################################################
yhat = lm(y~x+0)$fitted
ehat = y-yhat #residuals (computed for original data)

B = 500
y.boot = matrix(0,B,n)
yhat.boot = matrix(0,B,n)

for (b in 1:B) {
  index.b = sample(1:n,replace=TRUE)
  y.b = yhat + ehat[index.b]
  
  y.boot[b,] = y.b
  yhat.boot[b,] = lm(y.b~x+0)$fitted
}

tr(cov(y.boot,yhat.boot)) # pretty close to the true optimism (p=20)

##############################################################
# Part c: Use "pairs bootstrap" to estimate degrees of freedom,
# that is resample cases
##############################################################
B = 500
y.boot = matrix(0,B,n)
yhat.boot = matrix(0,B,n)

for (b in 1:B) {
  index.b = sample(1:n,replace=TRUE)
  x.b = x[index.b,]
  y.b = y[index.b]
  
  y.boot[b,] = y.b
  yhat.boot[b,] = lm(y.b~x.b+0)$fitted
}

tr(cov(y.boot,yhat.boot)) # way too big!! why??

# Recall: the optimism is *defined as* the bias in the in-sample error
