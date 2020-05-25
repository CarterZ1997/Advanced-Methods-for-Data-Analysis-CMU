# Setup
load("engine.Rdata")
#mpg=read.csv("engine.ytrain",header=F)[,1]
#eng.disp=read.csv("engine.xtrain",header=F)[,1]
mpg=engine.ytrain[,1]
eng.disp=engine.xtrain[,1]
plot(eng.disp,mpg)

# A common set of predictors for plotting fits
x0=70+c(0:100)*4.3

# Linear spline
cutoff1=(eng.disp>200)*(eng.disp-200)
mod1=lm(mpg~poly(eng.disp-200,1)+cutoff1)
lines(x0,predict(mod1,newdata=data.frame(eng.disp=x0,cutoff1=(x0>200)*(x0-200))),col="green",lw=2)

# Quadratic spline
cutoff2=(eng.disp>200)*(eng.disp-200)^2
mod2=lm(mpg~poly(eng.disp-200,2)+cutoff2)
lines(x0,predict(mod2,newdata=data.frame(eng.disp=x0,cutoff2=(x0>200)*(x0-200)^2)),col="red",lw=2)

# Cubic spline
cutoff3=(eng.disp>200)*(eng.disp-200)^3
mod3=lm(mpg~poly(eng.disp-200,3)+cutoff3)
lines(x0,predict(mod3,newdata=data.frame(eng.disp=x0,cutoff3=(x0>200)*(x0-200)^3)),col="blue",lw=2)

legend("topright",lw=2,col=c("green","red","blue"),legend=c("Linear","Quadratic","Cubic"))

