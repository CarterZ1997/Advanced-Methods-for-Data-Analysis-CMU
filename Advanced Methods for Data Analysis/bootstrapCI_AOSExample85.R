### R code for Example 8.5, page 112

#install.packages("mrfDepth")
#library(mrfDepth)
#attach(bloodfat)
#names(bloodfat)
#Cholesterol: concentration of plasma cholesterol [mg/dl] for 320 patients with
#heart disease

x1 = rnorm(100,0,1); # fill in with real data
x2 = rnorm(200,1,1); # fill in with real data
n1 = length(x1);
n2 = length(x2);
th.hat = median(x2) - median(x1);
B = 1000;
Tboot = rep(0,B);
for(i in 1:B) {
  xx1 = sample(x1,n1,replace=T);
  xx2 = sample(x2,n1,replace=T);
  Tboot[i] = median(xx2)-median(xx1);
}
se = sd(Tboot);
Normal = c(th.hat - 2*se, th.hat + 2*se);
percentile = c(quantile(Tboot, 0.025), quantile(Tboot, 0.975));
pivotal = c(2*th.hat - quantile(Tboot,.975),
            2*th.hat - quantile(Tboot,.025));
print(Normal);
print(percentile);
print(pivotal);
