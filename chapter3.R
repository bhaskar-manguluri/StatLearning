library(MASS)
names(Boston)
#linear model between medv~lstat (median value of owner occupied homes~lpwer status of population(percent))
attach(Boston)
lm.fit = lm(medv~lstat)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=c(10,5,15)))
predict(lm.fit,data.frame(lstat = c(10,5,15)),interval="confidence")
predict(lm.fit,data.frame(lstat=c(10,5,15)),interval="prediction")
plot(lstat~medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat~medv,pch=20)
plot(lstat~medv,pch="+")
plot(1:20,1:20,pch=1:20)
# diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow=c(1,1))
#alternatively
residuals = residuals(lm.fit)
studentisedResiduals = rstudent(lm.fit)
plot(predict(lm.fit),residuals)
plot(predict(lm.fit),studentisedResiduals)
# on the basis of residual plots there is some evidence of non linearity
#leverage statistics can be computed for any number of predictors using hatvalues
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
#MULTIPLE LINEAR REGRESSION
lm.fit = lm(medv~lstat+age)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow=c(1,1))
lm.fit = lm(medv~.,data=Boston)
summary(lm.fit)
car::vif(lm.fit)
# VIF's are low to moderate for this data
#R^2
summary(lm.fit)$r.sq
#RSE
summary(lm.fit)$sigma
lm.fit = lm(medv~.-age,data=Boston)
# interaction terms
summary(lm(medv~lstat:age))
summary(lm(medv~lstat*age))
#Non Linear Transformation of Predictors
lm.fit2 = lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
plot(lm.fit2)
lm.fit2 = lm(medv~poly(lstat,2))
summary(lm.fit2)
#anova
anova(lm.fit,lm.fit2)
lm.fit= lm(medv~lstat)
anova(lm.fit,lm.fit2)
# null hypothesis both models same, alternate hypothesis lstat^2 superior, F statistic 135 => very clear evidence of better model
#Qualitative predictors
library(ISLR)
names(Carseats)
# automatically adds qualitative for variable like ShelvLoc
summary(lm(Sales~.,data=Carseats))
contrasts(Carseats$ShelveLoc)
loadLibraries = function(){
        cat("loading libraries")
        library(MASS)
        cat("loaded MASS")
        library(ISLR)
        print("finished loading libraries")
}
loadLibraries()
#1
#null hypothesis - no effect by TV on sales in presence of newspaper and radio, similarly for others, null hypothesis true only for newspaper
#2
# knn  classification mode(neighbours) -> prediction
# knn regreesion mean(neighbours) -> prediction
#3
#a
#considering male = 0 in gender, iii is true
#b
#50+20*4+0.07*110+35+0.01*4*110-10*4
#c
# F, significance is determined by p value of coefficient, not the coefficient
#4
#a
# training residual sum of cubic is atleast as small as linear
#b
#test RSS of cubic might be higher as the true relation is linear
#c
# even now cubic RSS is smaller in training
#D
# test RSS of cubic might be smaller as true model is non linear
#5
#6
#7
#8 
lm.fit = lm(mpg~horsepower,data=Auto)
summary(lm.fit)
plot(lm.fit)
#a
#1
i
# i there is relation between predictor and response, pvalue of coefficient is small(also large Fstatistic, 
#and small p value assosiated with F statistic) which contradicts null hypothesis of no relationship
# ii mpg decrases by 15 for  increase in horsepower by 100 units
# iii negative
# iv 
predict(lm.fit,data.frame(horsepower = 98),interval="confidence")
#b
plot(mpg~horsepower,data=Auto,pch="+")
abline(lm.fit,lwd=3,col="red")
par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow=c(1,1))
#presense of non linearity observed from residuals and studentized residuals plots
#9
#a
pairs(Auto)
#b
cor(Auto[,-9])
cor(subset(Auto,select = -name))
#c
lm.fit = lm(mpg~.-name,data=Auto)
summary(lm.fit)
#i there is relationship between response and one of the predictors which is backed by small p value for F statistic
#ii weight,year,origin and displacement (close to 95% confidence)
#iii more miles per gallon for latest cars
#d
par(mfrow=c(2,2))
plot(lm.fit)
# observation 14 is high leverage, model does not fit data well from Q-Q plot, indication for non linearity
# from residual and stidentized residual plots
#e
colonInteracFormula = function(){
        f = paste("mpg ~",paste(names(Auto)[2:8],collapse = "+"),sep=" ")
        m= rep("",dim(a)[2])
        for(i in 1:lengtha){m[i]=paste(a[,i],collapse=":")}
        g = paste(m,collapse="+")
        paste(f,g,sep="+")
}
# similar function can be writtewn using collapse "*" instead of ":"
f = colonInteracFormula()
summary(lm(f,data=Auto))
#f
summary(lm(mpg~log(horsepower),data=Auto))
plot(lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2),data=Auto))
#However, 2 problems are observed from the above plots: 1) the residuals vs fitted plot indicates
# heteroskedasticity (unconstant variance over mean) in the model. 2) The Q-Q plot indicates somewhat unnormality of the residuals.
#10
#a
lm.fit = lm(Sales~Price+Urban+US,data=Carseats)
summary(lm.fit)
#b
# no significant relation between beeing urban area and sales, sales are more in US, sales decreases with increase in proce
#c
#Price=intercept+alpha*price+beta*UrbanYes+gamma*USYes
#d US,Price
#e 
lm.fit = lm(Sales~Price+US,data=Carseats)
summary(lm.fit)
#f removing US predictor improved 
#g
p = predict(lm.fit,interval = "confidence")[,c(2,3)]
#h
par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow=c(1,1))
# leverage-368 outlier - 377
plot(predict(lm.fit), rstudent(lm.fit))
# all studentised residuals with -3 and 3 , hence no potential outliers

#11
set.seed(1)
x = rnorm(100)
y = rnorm(100)+2*x

var(x)
cov(y,y)
cov(x,y)
#a
lm.fit = lm(y~x)
coef(lm.fit)[2] * var(x)
summary(lm.fit)
summary(lm(y~x+0))
# both violate null hypothesis, intercept is not statistically significant
#b
summary(lm(x~y+0))
coef(lm(x~y))[2]*var(y)
#c coefficient 
# both are same regression line
#d
(sqrt(length(x)-1) * sum(x*y)) / (sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2))
# same t statistic as shown by lm.fit
#e 
#equation for t statistic is symmetric with x and y.(Note that constant length(x) = length(y))
#f
#from summary both fit gave same t statistic

#12 #a when sum of squares of both x and y same
#b
set.seed(1);x=rnorm(100);set.seed(1);y=2*rnorm(100)
summary(lm(x~y))
summary(lm(y~x))
#c
set.seed(1);x=rnorm(100);set.seed(1);y=rnorm(100)
#13
#a
set.seed(1)
x = rnorm(100)
#b
set.seed(22)
eps = rnorm(100,0,0.25)
#c
y =-1+0.5*x+eps
y
#d
plot(x,y,main="Scatter plot Y vx X")
#e
summary(lm(y~x))
# intercept and slope from lm, pretty close to actual values
#f
plot(x,y)
abline(-1,0.5,col="red")
abline(-0.97938,0.5022,col="blue")
legend(0, legend = c("model fit", "true"), col=c("blue","red"),lwd = 1)
#g
#h
#i
#j
#14
set.seed(1)
x1 = runif(100)
set.seed(1)
x2 = 0.5*x1+rnorm(100)/10
y = 2+2*x1+0.3*x2+rnorm(100)
#a
lm.fit = lm(y~x1+x2)
summary(lm.fit)
# coefficients almost the same , but x3 coefficient not significant
#b
cor(x1,x2)
plot(x1,x2)
abline(0,0.5)
#c
# beta1 rehject null, b2 accept null
#d
summary(lm(y~x1))
# reject null
#e
summary(lm(y~x2))
# reject null hypothesis
#f
# in absense of all other predictions
#g
x1 = c(x1,0.1)
x2 = c(x2,0.8)
y = c(y,6)
summary(lm(y~x1+x2))
# x2 significant and x1 not
par(mfrow=c(2,2))
plot(lm(y~x1+x2))
par(mfrow=c(1,1))
plot(predict(lm(y~x1+x2)),rstudent(lm(y~x1+x2)))
# all residuals are in between -3 and 3.
#15
#a
names(Boston)
for(x in names(Boston)[-1]){
        f = as.formula(paste("crim~",x))
        print(summary(lm(f,data=Boston)))
}
#all except chas are individually significant
#b
summary(lm(crim~.,data=Boston))
