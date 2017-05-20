library(class)
library(ISLR)
library(MASS)
x = rnorm(50)
y = rnorm(50,mean=50,sd = 0.1)
sqrt(var(y))
y= c(5,7,9,11)
sqrt(11)
plot(x,y,xlab="x values")
#2
#2.7
x1  = c(0,2,0,0,-1,1)
x2 = c(3,0,1,1,0,1)
x3 = c(0,0,3,2,1,1)
y = c("Red","Red","Red","Green","Green","Red")
dataFrame = cbind(x1,x2,x3,y)
dataFrame = as.data.frame(dataFrame)
test.dataFrame = data.frame(x1=0,x2=0,x3=0)
knn.fit = knn(dataFrame[,c(1,2,3)],test.dataFrame,dataFrame[,"y"],1)
knn.fit
knn.fit = knn(dataFrame[,c(1,2,3)],test.dataFrame,dataFrame[,"y"],3)
knn.fit
#small value of k for non linear boundary

#2.8
library(ISLR)
head(College)
row.names(College)
summary(College)
pairs(College[,1:10])
plot(College$Private,College$Outstate,xlab="private",ylab="Outstate",main="Outstate vs Private")
Elite = rep("No",nrow(College))
Elite[College$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
College = data.frame(College,Elite)
summary(College$Elite)
hist(College$Apps)
hist(College$Accept)
hist(College$Enroll)

#9
str(Auto)
summary(Auto)
#mpg,displacement,horsepower,weight,acceleration quantitative
# cylinders,origin => can be converted to qualitative due to relatively small number of possible values
Auto$cylinders = as.factor(Auto$cylinders);summary(Auto$cylinders);Auto$origin = factor(Auto$origin);summary(Auto$origin)
levels(Auto$cylinders);levels(Auto$origin)
summary(Auto)
levels(as.factor(Auto$year)) # treating as continous variable to preserve the natural ordering in years
quantitativeIndex = c(1,3,4,5,6,7)
sapply(Auto[,quantitativeIndex],function(x){return(range(x))})
sapply(Auto[,quantitativeIndex],function(x){return(mean(x))})
sapply(Auto[,quantitativeIndex],function(x){return(sd(x))})
sapply(Auto[-c(10:85),quantitativeIndex],function(x){return(mean(x))})
sapply(Auto[-c(10:85),quantitativeIndex],function(x){return(sd(x))})
#e
pairs(Auto[,quantitativeIndex])
# mpg seems to decrease with displcement,horsepower,weight ; which are intern positively correlated with each other
cor(Auto$displacement,Auto$horsepower)
plot(mpg~acceleration,data=Auto,main="mpg vs acceleration")
abline(lm(mpg~acceleration,data = Auto))
cor(Auto$mpg,Auto$acceleration)
plot(mpg~cylinders,Auto)
# diffent mean mpg for different number of cylinders
plot(mpg~as.factor(year),data=Auto)
plot(mpg~origin,data = Auto)
# milage seems to slowly oincrease with year
#f
#displacement,horsepower,weight,year,origin,acceleration,cylinders
# adding name as predictor leads to too few observations for each and may result in overfit

#10
?Boston
summary(Boston)
levels(as.factor(Boston$chas))
Boston$chas = as.factor(Boston$chas)
pairs(Boston)
plot(crim~chas,Boston) # not much related to crim rates
summary(Boston$chas)
plot(crim~chas,Boston[Boston$chas==1,])
plot(crim~chas,Boston[Boston$chas==0,]) # too many outliers
plot(Boston$nox,Boston$crim)
plot(Boston$age,Boston$crim)
plot(log(Boston$age),Boston$crim)
# very old houses, higher crimes
plot(Boston$dis, Boston$crim)
# high crim rate in areas very close to work
plot(Boston$rad, Boston$crim)
levels(as.factor(Boston$rad))
Boston$rad01 = ifelse(Boston$rad > 8,1,0)
plot(as.factor(Boston$rad01),Boston$crim)
# Higher index of accessibility to radial highways, more crime
plot(Boston$tax, Boston$crim)
Boston$tax01 = ifelse(Boston$tax > 400 ,1,0)
plot(as.factor(Boston$tax01), Boston$crim)
# Higher tax rate, more crime
plot(Boston$ptratio, Boston$crim)
Boston$ptratio01 = ifelse(Boston$ptratio>median(Boston$ptratio),1,0)
plot(as.factor(Boston$ptratio01),Boston$crim)
# Higher pupil:teacher ratio, more crime

subset(Boston, medv == min(Boston$medv))
subset(Boston, rm > 8)
range(Boston$crim)
# relatively low crim rate

