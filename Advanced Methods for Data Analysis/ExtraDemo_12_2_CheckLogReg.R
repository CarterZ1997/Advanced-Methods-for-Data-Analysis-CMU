# Extra Demo 12.2
# Testing parametric regression specifications (GLM: logistic regression)
# with nonparametric regression (GAM).
#
# This is the worked R example from Shalizi Sec 11.6

#install.packages("faraway")
sim.logistic <- function(x, beta.0,beta,bind=FALSE) {
  require(faraway) # For accessible logit and inverse-logit functions
  linear.parts <- beta.0+(x%*%beta)
  y <- rbinom(nrow(x),size=1,prob=ilogit(linear.parts))
  if (bind) { return(cbind(x,y)) } else { return(y) }
}

plot.logistic.sim <- function(x, beta.0, beta, n.grid=50,
                              labcex=0.3, col="grey", ...) {
  grid.seq <- seq(from=-1,to=1,length.out=n.grid)
  plot.grid <- as.matrix(expand.grid(grid.seq,grid.seq))
  require(faraway)
  p <- matrix(ilogit(beta.0 + (plot.grid %*% beta )),nrow=n.grid)
  contour(x=grid.seq,y=grid.seq,z=p, xlab=expression(x[1]),
          ylab=expression(x[2]),main="",labcex=labcex,col=col)
  y <- sim.logistic(x,beta.0,beta,bind=FALSE)
  points(x[,1],x[,2],pch=ifelse(y==1,"+","-"),col=ifelse(y==1,"blue","red"))
  invisible(y)
}

## ----logistic-regression-sim-results, echo=FALSE, results="hide"---------
x <- matrix(runif(n=50*2,min=-1,max=1),ncol=2)
par(mfrow=c(2,2))
plot.logistic.sim(x,beta.0=-0.1,beta=c(-0.2,0.2))
y.1 <- plot.logistic.sim(x,beta.0=-0.5,beta=c(-1,1))
plot.logistic.sim(x,beta.0=-2.5,beta=c(-5,5))
plot.logistic.sim(x,beta.0=-2.5e2,beta=c(-5e2,5e2))

## ----our-first-logistic-regression---------------------------------------
df <- data.frame(y=y.1, x1=x[,1], x2=x[,2])
logr <- glm(y ~ x1 + x2, data=df, family="binomial")

## ------------------------------------------------------------------------
summary(logr,digits=2,signif.stars=FALSE)

## ------------------------------------------------------------------------
mean(ifelse(fitted(logr)<0.5,0,1) != df$y)

## ----our-first-gam-------------------------------------------------------
library(mgcv)
(gam.1 <- gam(y~s(x1)+s(x2), data=df, family="binomial"))

## ----gam-fit-to-logistic-simulation, out.width="0.49\\textwidth", echo=FALSE----
plot(gam.1,residuals=TRUE,pages=0)

## ----code:sim.fitted.logistic--------------------------------------------
# Simulate a fitted logistic regression and return a new data frame
# Inputs: data frame (df), fitted model (mdl)
# Outputs: new data frame
# Presumes: df contains columns with names for the covariates of mdl
simulate.from.logr <- function(df, mdl) {
  probs <- predict(mdl,newdata=df,type="response")
  df$y <- rbinom(n=nrow(x),size=1,prob=probs)
  return(df)
}

## ----delta.deviance.sim--------------------------------------------------
# Simulate from an estimated logistic model, and refit both the logistic
  # regression and a generalized additive model
# Hard-codes the formula; better code would be more flexible
# Inputs: data frame with covariates (df), fitted logistic model (mdl)
# Output: difference in deviances
# Presumes: df has columns names x.1 and x.2.
delta.deviance.sim <- function (df,mdl) {
  sim.df <- simulate.from.logr(df,mdl)
  GLM.dev <- glm(y~x1+x2,data=sim.df,family="binomial")$deviance
  GAM.dev <- gam(y~s(x1)+s(x2),data=sim.df,family="binomial")$deviance
  return(GLM.dev - GAM.dev)
}

## ----run.delta.deviance.sim----------------------------------------------
(delta.dev.observed <- logr$deviance - gam.1$deviance)
delta.dev <- replicate(100,delta.deviance.sim(df,logr))
mean(delta.dev.observed > delta.dev)

## ----diff-in-deviance-when-null-is-true, echo=FALSE----------------------
hist(delta.dev, main="",
     xlab="Amount by which GAM fits better than logistic regression")
abline(v=delta.dev.observed,col="grey",lwd=4)

