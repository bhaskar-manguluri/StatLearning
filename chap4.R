library(ISLR)
library(MASS)
library(class)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[,-9])
plot(Smarket$Volume)
plot(Smarket$Volume~as.factor(Smarket$Year))
boxplot(Smarket$Volume~as.factor(Smarket$Year))
# volume of traes increased over time from 2001 to 2005
#Logistic regression
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = "binomial",data = Smarket)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
glm.probs = predict(glm.fit,type = "response")
glm.pred = rep("Down",1250)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred,Smarket$Direction)
#validation approach
train = Smarket$Year < 2005
Smarket.2005 = Smarket[!train,]
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,subset = train,family = "binomial")
glm.probs = predict(glm.fit,newdata = Smarket.2005,type = "response")
glm.pred = ifelse(glm.probs>0.5,"Up","Down")
table(glm.pred,Smarket.2005$Direction)
mean(glm.pred == Smarket.2005$Direction)
summary(glm.fit)
#only Lag1 and Lag2 seems significant, checking if accuracy can be improved by removing statistically insignificant variables
glm.fit = glm(Direction~Lag1+Lag2,data = Smarket,family="binomial",subset = train)
glm.probs = predict(glm.fit,Smarket.2005,type = "response")
glm.pred = ifelse(glm.probs>0.5,"Up","Down") 
table(glm.pred,Smarket.2005$Direction)
mean(Smarket.2005$Direction == glm.pred)
#accuracy improved
table(Smarket.2005$Direction)
141/252
# but that is the same accuracy as , blind guess but from the table the accuracy of market going up 106/182(approx 58)
# hence if a stategy of ignoring the market going down predictions and only using predicitonis of market going up, is better than blind guess 
# even then hypothesis testing whether the difference in predictions being by chance should be verified

# Linear Discriminatory analysis
lda.fit = lda(Direction~Lag1+Lag2,data = Smarket,subset = train)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit,newdata = Smarket.2005)
names(lda.fit)
sum(lda.pred$posterior[,1]>0.5)
sum(lda.pred$posterior[,2]>0.5)
max(lda.pred$posterior[,1])
max(lda.pred$posterior[,2])
lda.pred = predict(lda.fit)
max(lda.pred$posterior[,1])
max(lda.pred$posterior[,2])
# less probability even in training , why?

#QDA
qda.fit = qda(Direction~Lag1+Lag2,data = Smarket,subset = train)
qda.fit
coef(qda.fit)
#no coefficients as the function is quadratic 
qda.pred = predict(qda.fit,newdata = Smarket.2005)
table(qda.pred$class,Smarket.2005$Direction)
sum(qda.pred$posterior[,1]>0.5)

#KNN

trainX = cbind(Smarket$Lag1,Smarket$Lag2)[train,]
testX = cbind(Smarket.2005$Lag1,Smarket.2005$Lag2)
train.Direction = Smarket$Direction[train]
set.seed(1)

knn.pred = knn(train = trainX,test = testX,cl = train.Direction,k = 1)
table(knn.pred,Smarket.2005$Direction)
knn.pred = knn(train = trainX,test = testX,cl = train.Direction,k = 3)
table(knn.pred,Smarket.2005$Direction)
# prediction is improved using k= 3 usually k is chosen through cross validation
# scaling effects knn predictions, as Lag1 and Lag2 are measured in same scale, no scaling is done for this example
# if any tie in distance of nearest neighbour, knn resolves it by random selection , hence set.seed

# classification on caravan insurance data
dim(Caravan)
attach(Caravan)
summary(Purchase)
standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
# now mean zero and var1 for all the columns
test = 1:1000
train.X =standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
knn.pred = knn(train = train.X,test = test.X,cl = train.Y)
mean(test.Y != knn.pred)
mean(test.Y != "No")
# a simple "no all the time has better test error of 5.9%
table(knn.pred,test.Y)
# what if the company decides to sell insurance to not every one, but only for those who knn with k=1 predicts they will buy
10/77
# with this strategy success rate of sales improved to 12.98
knn.pred = knn(train = train.X,test = test.X,cl = train.Y,k=3)
table(knn.pred,test.Y)
5/26
# success rate further improved to 19.23%
knn.pred = knn(train = train.X,test = test.X,cl = train.Y,k = 5)
table(knn.pred,test.Y)
4/15
# improved to 26.67%
# Comparing with LOGISTIC REGRESSION
glm.fit = glm(Purchase~.,data=Caravan,family = binomial,subset = -test)
glm.probs = predict(glm.fit,newdata = Caravan[test,],type = "response")
glm.pred = ifelse(glm.probs > 0.5,"Yes","No")
table(glm.pred,test.Y)
# all 7 predicted by glm didn't actually buy insurance
# reduce the cut off from 0.5
glm.pred = ifelse(glm.probs>0.25,"Yes","No")
mean(glm.pred == test.Y)
table(glm.pred,test.Y)
11/33


#1 -9 conceptual
#10
#a
library(ISLR)
head(Weekly)
cor.weekly = cor(Weekly[,-9])
library(corrplot)
corrplot(cor.weekly)
plot(Weekly$Volume)
with(Weekly,plot(as.factor(Year),Volume))
# volume of trades increased with year all the time, except in 2012
plot(Weekly$Direction,Weekly$Lag1)
plot(Weekly$Direction,Weekly$Lag2)
plot(Weekly$Direction,Weekly$Lag3)
plot(Weekly$Direction,Weekly$Lag4)
plot(Weekly$Direction,Weekly$Lag5)
plot(Weekly$Direction,Weekly$Volume)
plot(Weekly$Lag1,Weekly$Lag2)
# no other relationship
#b
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume+Year,data = Weekly,family = "binomial")
glm.fit
summary(glm.fit)
# only lag2 seems significant
glm.probs = predict(glm.fit,type = "response")
glm.pred = ifelse(glm.probs > 0.5,"Up","Down")
table(glm.pred,Weekly$Direction)
(47+558)/((56+428)+(47+558))#prior probability# 0.56
(56+558)/((47+428)+(56+558))# accuracy
#c
558/(428+558)# 0 .56
#d
trainIndex = Weekly$Year < 2009 
sum(trainIndex)
dim(Weekly)
test.weekly = Weekly[!trainIndex,]
glm.fit = glm(Direction~Lag2,data = Weekly,subset = trainIndex,family = "binomial")
glm.probs = predict(glm.fit,newdata = test.weekly[,-9],type = "response")
glm.pred = ifelse(glm.probs>0.5,"Up","Down")
table(glm.pred,test.weekly$Direction)
65/104 #0.625 accuracy
#e
train.weekly = Weekly[trainIndex,]
lda.fit = lda(Direction~Lag2,data = train.weekly)
lda.fit
lda.pred = predict(lda.fit,newdata = test.weekly[,-9],type = "response")
# how $x and $posterior has 1089 rows instead of given 886 rows of training 
table(lda.pred$class,test.weekly$Direction)
65/104 #0.536 accuracy
 #f
qda.fit = qda(Direction~Lag2,data=Weekly,subset = trainIndex)
qda.pred = predict(qda.fit,newdata = test.weekly[,-9],data = "response")
head(qda.pred)
table(qda.pred$class,test.weekly$Direction)
61/104 #0.586
#g
set.seed((1))
knn.fit = knn(train.weekly[,2:3],test.weekly[,2:3],cl = train.weekly[,9],k=1)
table(knn.fit,test.weekly[,9])
50/104 # 0.48
#h
# lda,logistic>qda> knn.k=1
#i

#11
names(Auto)
mpg01 = ifelse(Auto$mpg>median(Auto$mpg), 1,0)
Auto = data.frame(Auto,mpg01)
plot(mpg01,cylinders)
str(Auto)
levels(as.factor(Auto$cylinders))
# can be a factor with 5 levels
levels(as.factor(Auto$displacement))
levels(as.factor(Auto$horsepower))
levels(as.factor(Auto$weight))
levels(as.factor(Auto$acceleration))
levels(as.factor(Auto$year))
levels(as.factor(Auto$origin))
# can be a fctor with 3 levels
levels(as.factor(Auto$name))
Auto$mpg01 = as.factor(Auto$mpg01)
plot(Auto$mpg01,Auto$mpg)
# but mpg01 itself is created from this variable, hence we should not use it for model
plot(as.factor(Auto$cylinders),Auto$mpg01)
# factor , factor relation shipo best seen in stacked bar plot?
plot(table(Auto$mpg01,as.factor(Auto$cylinders)))
plot(table(as.factor(Auto$cylinders),Auto$mpg01))
# cylinder seem to have a relation , but find better plot for indicating these
plot(Auto$displacement,Auto$mpg01)
plot(Auto$mpg01,Auto$displacement)
# displacement seems to have a relation with mpg01 as group means of displacement is 0 for mpg01
plot(Auto$weight,Auto$mpg01)
plot(Auto$mpg01,Auto$weight)
# weight seems to have a relation 
# relation between a factor and numeric is best visualised in difference in median of boxplot in all the levels of factors
plot(Auto$mpg01,Auto$horsepower)
# there seems to be a relation between horsepower and mpg01
plot(Auto$mpg01,Auto$acceleration)
# again there seems to be some relation ship
plot(Auto$mpg01,Auto$year)
# there seems to be some relation ship
plot(Auto$mpg01,Auto$origin)
plot(Auto$mpg01,as.factor(Auto$origin))
plot(as.factor(Auto$origin),Auto$mpg01)
# origin seems to have some relation ship
plot(Auto$mpg01,Auto$name)
# too many levels to have a relation, overfitting variable?

#except mpg,name everything can be included in model
# what about correlation between predictors?
cor = cor(ISLR::Auto[,-9])
corrplot::corrplot(cor)
# cylinders have high correlation with horsepower,weight
# split the data into test and train
train = (Auto$year%%2 == 0)  # if the year is even
Auto.train = Auto[train,]
Auto.test = Auto[!train, -c(mpg01)]
mpg01.test = mpg01[!train]
lda.fit = lda(mpg01~horsepower+weight+year+origin+displacement,data=Auto.train)
lda.fit
lda.pred = predict(lda.fit,newdata = Auto.test,type = "response")
table(lda.pred$class,mpg01.test)
157/182
22/182
# QDA
qda.fit = qda(mpg01~horsepower+weight+year+origin+displacement, data = Auto.train)
qda.pred = predict(qda.fit, Auto.test)
mean(qda.pred$class != mpg01.test)
# Logistic regression
glm.fit = glm(mpg01~horsepower+weight+year+origin+displacement, data = Auto, 
              family = binomial, subset = train)
glm.probs = predict(glm.fit, Auto.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != mpg01.test)
#knn
trainX= Auto.train[,c("horsepower","weight","year","origin","displacement")]
trainY = Auto.test[,c("horsepower","weight","year","origin","displacement")]
train.mpg01 = Auto.train[,"mpg01"]
knn.pred = knn(train = trainX,test = trainY,cl =as.character(train.mpg01) ,k = 1)
mean(knn.pred != mpg01.test)
knn.pred = knn(train = trainX,test = trainY,cl =as.character(train.mpg01) ,k=10)
mean(knn.pred != mpg01.test)
knn.pred = knn(train = trainX,test = trainY,cl =as.character(train.mpg01) ,k=20)
mean(knn.pred != mpg01.test)
# shouldn't we use factor to estimate knn ??,  with train.mpg01 as a factor , there is no change in the test set performance for k=1 ,2,100?

#12
#a,b,c
power=function(x,n){
        x^ n
}
power(2,3)
power(10,3)
power(8,17)
power(131,3)
power3 = function(x,n){
        a = x^n
        return(a)
}
a = 1:10
b= vector(mode = "numeric",length = 10)
for( i in a){
        b[i] = power3(i,2)
}
plot(a,b,type = "l",main="X square")

plotpower = function(x,a){
        y = sapply(x,function(x){x^a})
        plot(x,y)
}
x = 1:10
plotpower(x,3)
#13
head(Boston)
library(dplyr)
Boston = Boston %>% mutate(crime01 = ifelse(crim > median(crim),1,0))
#logistic
glm.fit = glm(crime01~.-crim,data = Boston,family = "binomial")
glm.probs = predict(glm.fit,type = "response")
glm.pred = ifelse(glm.probs>0.5,1,0)
table(glm.pred,Boston$crime01)
(229+234)/(229+234+24+19) #0.915
#lda
lda.fit = lda(crime01~.-crim,data = Boston)
lda.pred = predict(lda.fit)
names(lda.pred)
table(lda.pred$class,Boston$crime01)
(240+193)/(240+193+13+60) # 0.85
#qda
qda.fit = qda(crime01~.-crim,data = Boston)
qda.pred = predict(qda.fit)
table(qda.pred$class,Boston$crime01)
(252+206)/(252+206+1+47) #9051
#knn
names(Boston)
deselectBoston = c("crim","crime01")
train.X = Boston[,deselectBoston]
head(train.X)
test.X = train.X
train.Y = Boston[,"crime01"]
knn.pred = knn(train = train.X,test = test.X,cl = train.Y,k=1)
table(knn.pred,train.Y)
knn.pred = knn(train = train.X,test = test.X,cl = as.character((train.Y)),k=100)
table(knn.pred,train.Y)
# no matter what the k is, same training error of zero??


