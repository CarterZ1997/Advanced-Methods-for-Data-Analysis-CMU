# Demo 1 (Introduction and Regression)
# Compute training and test errors for linear regression and k-NN regression

###########################################################
# Part a: Load data
###########################################################
load("nonlin.Rdata")

# Plot the first training set (column 1)
first=data.frame(x = xtrain[,1],y = ytrain[,1])
x = xtrain[,1]
y = ytrain[,1]
plot(x,y)
lines(x0,r0,lwd=2)  # true regression function (simulated with intro-prep.R)

###########################################################
# Part b: Linear regression on the first training set
###########################################################
# Look at linear regression the first training set 
linmodel=lm(y~x,data=first)
plot(x,y)
abline(a=linmodel$coef[1],b=linmodel$coef[2],col="red",lwd=2)

###########################################################
# Part c: k-NN regression on the first training set
###########################################################
# install.packages("FNN")
library(FNN)

# kNN regression for k=3, 14, 45
ks = c(3,15,45)  
knnmodel1 = knn.reg(train=matrix(x,ncol=1),test=matrix(x0,ncol=1),y=y,k=ks[1])
knnmodel2 = knn.reg(train=matrix(x,ncol=1),test=matrix(x0,ncol=1),y=y,k=ks[2])
knnmodel3 = knn.reg(train=matrix(x,ncol=1),test=matrix(x0,ncol=1),y=y,k=ks[3])

# Plot predictions (component 'pred' in the output of knn.reg)
par(mfrow=c(1,3))
plot(x,y,main=paste("k =",ks[1]))
lines(x0,knnmodel1$pred,col="red",lwd=2)
plot(x,y,main=paste("k =",ks[2]))
lines(x0,knnmodel2$pred,col="green4",lwd=2)
plot(x,y,main=paste("k =",ks[3]))
lines(x0,knnmodel3$pred,col="blue",lwd=2)

###########################################################
# Part d: Training and test errors as a function of k 
# for the first data set.
###########################################################

# Training and test errors for linear regression
errtrain.lin = mean((y - linmodel$fitted)^2)
errtest.lin = mean((ytest[,1] - predict(linmodel,data.frame(x=xtest[,1])))^2)

# Let k=1,2,....,60
nk = 60
ks = 1:nk
errtrain.knn = errtest.knn = numeric(nk)
for (i in 1:nk) {
  knnmodel = knn.reg(matrix(x,ncol=1),matrix(x,ncol=1),y=y,k=ks[i]) # "train=test"
  errtrain.knn[i] = mean((y-knnmodel$pred)^2)
  knnmodel = knn.reg(matrix(x,ncol=1),matrix(xtest[,1],ncol=1),y=y,k=ks[i]) 
  errtest.knn[i] = mean((ytest[,1]-knnmodel$pred)^2)
}

# Plot errors as function of k
ylim = range(c(errtrain.knn,errtest.knn))

par(mfrow=c(1,1))
plot(ks,errtrain.knn,type="l",ylim=ylim,col="red",
     main="Training and test errors",xlab="k",ylab="Error")
abline(h=errtrain.lin,lty=2,col="red")
lines(ks,errtest.knn,col="blue")
abline(h=errtest.lin,lty=2,col="blue")
legend("topleft",col=c("red","red","blue","blue"),lty=c(1,2,1,2),
       legend=c("KNN training error","LS training error",
                "KNN test error","LS test error"))

###########################################################
# Part e: Compute average training and test errors
###########################################################
# Repeat for all 50 training/test sets.
p = 50
errtrain.lin.all = numeric(p)
errtest.lin.all = numeric(p)
errtrain.knn.all = matrix(0,nk,p) #each column represents a simulation
errtest.knn.all = matrix(0,nk,p)
for (j in 1:p) {
  cat(paste(j,",",sep=""))
  alld=data.frame(x = xtrain[,j],  y = ytrain[,j])
  x = xtrain[,j]
  y = ytrain[,j]
  
  linmodel = lm(y~x,data=alld)
  errtrain.lin.all[j] = mean((y - linmodel$fitted)^2)
  errtest.lin.all[j] = mean((ytest[,j] - predict(linmodel,data.frame(x=xtest[,1])))^2)

  for (i in 1:nk) {
    knnmodel = knn.reg(matrix(x,ncol=1),matrix(x,ncol=1),y,k=ks[i]) 
    errtrain.knn.all[i,j] = mean((ytrain[,j]-knnmodel$pred)^2)
    knnmodel = knn.reg(matrix(x,ncol=1),matrix(xtest[,j],ncol=1),y,k=ks[i])
    errtest.knn.all[i,j] = mean((ytest[,j]-knnmodel$pred)^2)
  }
}

# Average training/test errors over all 50 simulations (columns)
errtrain.lin.ave = mean(errtrain.lin.all)
errtest.lin.ave = mean(errtest.lin.all)
errtrain.knn.ave = rowMeans(errtrain.knn.all)
errtest.knn.ave = rowMeans(errtest.knn.all)


# Plot results as a function of k
ylim = range(c(errtrain.knn.ave,errtest.knn.ave))

par(mfrow=c(1,1))
plot(ks,errtrain.knn.ave,type="l",ylim=ylim,col="red",
     main="Averaged training and test errors",xlab="k",ylab="Error")
abline(h=errtrain.lin.ave,lty=2,col="red")
lines(ks,errtest.knn.ave,col="blue")
abline(h=errtest.lin.ave,lty=2,col="blue")
legend("topleft",col=c("red","red","blue","blue"),lty=c(1,2,1,2),
       legend=c("KNN ave training error","LS ave training error",
                "KNN ave test error","LS ave test error"))

