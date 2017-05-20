# resampling techniques

library(ISLR)
library(boot)
library(MASS)
# validation set apprach
dim(Auto)
set.seed(1)
train = sample(392,196)
#find test accuracy w.r.t linear,quadratic,and cubic terms
lm.fit = lm(mpg~horsepower,Auto,train)
yhat = predict(lm.fit,Auto[-train,])
mean((yhat-Auto[-train,"mpg"])^2)
lm.fit2 = lm(mpg~poly(horsepower,2),Auto,train)
yhat2 = predict(lm.fit2,Auto[-train,])
mean((yhat2-Auto[-train,"mpg"])^2)
lm.fit3 = lm(mpg~poly(horsepower,3),Auto,train)
yhat3 = predict(lm.fit3,Auto[-train,])
mean((yhat3-Auto$mpg[-train])^2)
# large decrease in test MSE by using horsepower^2 , very small further decrease by using horsepower^3
# using different seed(=>  using different train,test set), we get different validation errors

#find test accuracy with different seed
set.seed(2)
train = sample(392,196)
lm.fit = lm(mpg~horsepower,Auto,train)
yhat = predict(lm.fit,Auto[-train,])
mean((yhat-Auto[-train,"mpg"])^2)
lm.fit2 = lm(mpg~poly(horsepower,2),Auto,train)
yhat2 = predict(lm.fit2,Auto[-train,])
mean((yhat2-Auto[-train,"mpg"])^2)
lm.fit3 = lm(mpg~poly(horsepower,3),Auto,train)
yhat3 = predict(lm.fit3,Auto[-train,])
mean((yhat3-Auto$mpg[-train])^2)
# now adding cubic term lead to a slight increase in MSE 

#LOOCV
loocv.LM = function(x,y,fit){
        n = length(x)
        xbar = mean(x)
        h = 1/n + ((x-xbar)^2)/(sum((x-xbar)^2)) # vector of leverages
        yhat = predict(fit)
        sumOferror = sum(((y-yhat)/(1-h))^2)
        loocv.error = sumOferror/n
        return(loocv.error)
}
loocv.LM(Auto$horsepower,Auto$mpg,lm(mpg~horsepower,Auto)) # 24.23151
# can also use glm for fitting lm, and use cv.glm for boot package
glm.fit = glm(mpg~horsepower,data = Auto)
cv.err = cv.glm(glm.fit,data = Auto)
cv.err$delta # first component cv error, second component is adjusted cv.error(adjusted for bias introduced by not using loocv)
# cv.glm does not use the formula available for loocv (loocv.LM uses that formula), but first component of delta is same as 
# loocv.LM proving that the formula is valid
cv.error = rep(-1,5)
cv.adj.error = rep(-1,5)
for(i in 1:5){
        glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
        cv.error[i] = cv.glm(glmfit = glm.fit,data = Auto)$delta[1]
        cv.adj.error[i] = cv.glm(glmfit = glm.fit,Auto)$delta[2]
}
cv.error;cv.adj.error
# again sharp decrease in test MSE by using power 2, change due to adding additional terms is small, but it can be either increase/decrease in testMSE

#K-fold CV
set.seed(17)
for(i in 1:5){
        glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
        cv.error[i] = cv.glm(glmfit = glm.fit,data = Auto,K = 10)$delta[1]
        cv.adj.error[i] = cv.glm(glmfit = glm.fit,Auto,K = 10)$delta[2]
}
cv.error;cv.adj.error # seed is set only for one time, but results are reproducible even though there are 2 random steps in for loop

#Bootstrap

alpha.fn = function(data,index){
        X = data$X[index]
        Y = data$Y[index]
        return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
dim(Portfolio)
alpha.fn(Portfolio,1:100)
?boot
alpha.fn(Portfolio,sample(100,100,replace = T))
set.seed(1)
boot(Portfolio,alpha.fn,1000)
# using bootstrap to find SE of lm coefficients
boot.fn = function(data,index){
        return(coef(lm(mpg~horsepower,data=data,subset=index)))
}
set.seed(15)
boot.fn(Auto,sample(392,100,replace = T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,Auto))$coefficients

#1g
x = 1:10000
y = rep(-1,10000)
for( i in x){
        y[i] = 1-((1-1/i)^i)
}
plot(x,y)
# asymptote at 0.63
#1h
store = rep(NA,10000)
for( i in 1:10000){
        store[i] = sum(sample(1:100,rep=T)==4)>0
}
mean(store)
# probability that 4th observation is in the sample is 0.63
(1-(1-1/10000)^10000)
(1-(1-1/16)^16)

#5
#a
glm.fit = glm(default~income+balance,data = Default,family = "binomial")
#b
dim(Default)
set.seed(1)
train = sample(10000,8000)
glm.fit = glm(default~income+balance,data = Default,family = "binomial",subset = train)
glm.probs = predict(glm.fit,newdata = Default[-train,],type = "response")
glm.pred = ifelse(glm.probs>0.5,1,0)
table(glm.pred,Default$default[-train])
52/2000#0.026
#c
dim(Default)
set.seed(17)
train = sample(10000,8000)
glm.fit = glm(default~income+balance,data = Default,family = "binomial",subset = train)
glm.probs = predict(glm.fit,newdata = Default[-train,],type = "response")
glm.pred = ifelse(glm.probs>0.5,1,0)
table(glm.pred,Default$default[-train])
44/2000#0.022
set.seed(18)
train = sample(10000,8000)
glm.fit = glm(default~income+balance,data = Default,family = "binomial",subset = train)
glm.probs = predict(glm.fit,newdata = Default[-train,],type = "response")
glm.pred = ifelse(glm.probs>0.5,1,0)
table(glm.pred,Default$default[-train])
57/2000#0.0285
set.seed(111)
train = sample(10000,8000)
glm.fit = glm(default~income+balance,data = Default,family = "binomial",subset = train)
glm.probs = predict(glm.fit,newdata = Default[-train,],type = "response")
glm.pred = ifelse(glm.probs>0.5,1,0)
table(glm.pred,Default$default[-train])
54/2000#0.027
#d
set.seed(1)
train = sample(10000,8000)
glm.fit = glm(default~income+balance+student,data = Default,family = "binomial",subset = train)
glm.probs = predict(glm.fit,newdata = Default[-train,],type = "response")
glm.pred = ifelse(glm.probs>0.5,1,0)
table(glm.pred,Default$default[-train])
50/2000#0.025
# adding student improved the fit. although through validation approach the reduction is small

#6
#a
glm.fit = glm(default~income+balance,data = Default,family = "binomial")
summary(glm.fit) # 0.00002082 0.000004985 ; 0.005647, 0.0002274
coef(glm.fit)
#b
boot.fn = function(data,index){
return(coef(glm(default~income+balance,data = data,family = "binomial",subset = index)))        
}
boot.fn(Default,1:nrow(Default))
#c
boot(data = Default,statistic = boot.fn,R = 1000)
#d
#values from bootstrap and summary(glm) are pretty close

#7
#a
glm.fit = glm(Direction~Lag1+Lag2,data=Weekly,family = binomial)
#b
glm.fit = glm(Direction~Lag1+Lag2,data=Weekly[-1,],family = binomial)
#c
glm.prob = predict(glm.fit,newdata = Weekly[1,],type = "response")
glm.pred = ifelse(glm.prob>0.5,"Up","Down")
Weekly$Direction[1] == glm.pred
#d
looerror = rep(NA,nrow(Weekly))
for(i in 1:nrow(Weekly)) {
        glm.fit = glm(Direction~Lag1+Lag2,data=Weekly[-i,],family = binomial)
        glm.prob = predict(glm.fit,newdata = Weekly[i,],type = "response")
        glm.pred = ifelse(glm.pred == T,"Up","Down")
        looerror[i] = as.integer(glm.pred == Weekly$Direction[i])
}
#e
mean(looerror)

#8
#a
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x -2*x^2+rnorm(100)
plot(y,x)
#b quadratic in x
plot(x,y)
#c
data = data.frame(x,y)
lm1 = glm(y~x,data = data)
lm2 = glm(y~I(x)+I(x^2),data=data)
summary(lm1)
summary(lm2)
lm3 = glm(y~poly(x,3),data = data)
summary(lm3)
lm4 = glm(y~poly(x,4),data = data)
summary(lm4)
cv.error = data.frame(cverror = rep(NA,4),adjerror = rep(NA,4))
set.seed(12)
for( i in 1:4) { glmfit = glm(y~poly(x,i),data = data);cv.error[i,]=cv.glm(glmfit,data = data)$delta}
plot(1:4,cv.error$cverror,type = "b")
cv.error[,1]
#d
set.seed(130)
for( i in 1:4) { glmfit = glm(y~poly(x,i),data = data);cv.error[i,]=cv.glm(glmfit,data = data)$delta}
cv.error[,1]
# does not depend on random splitting
#e
#quadratic model has the least cv.error as it matches with the true form of y
# x,x^2 significant

#9
?Boston
mean(Boston$medv) # population mean estimate of median value in 1000's
sd(Boston$medv)/sqrt(nrow(Boston)) # std.error of sample mean = sample standard deviation/sqrt(number of obs.)
boot.fn = function(data,index){
        return(mean(data[index,"medv"]))
}
boot(Boston,boot.fn,1000) # estimating standard error by bootstap, very close to formula approximation
22.53281-2*0.4087208;22.53281+2*0.4087208
t.test(Boston$medv)
median(Boston$medv) # estimate of population median
boot.fn = function(data,index){return(median(data[index,"medv"]))};boot(Boston,boot.fn,1000)# std.error of sample median
quantile(Boston$medv,probs = 0.1)
boot.fn = function(data,index){return(quantile(data[index,"medv"],0.1))}
str(boot.fn(Boston,sample(1:nrow(Boston),replace = T)))
boot(Boston,boot.fn,1000)
