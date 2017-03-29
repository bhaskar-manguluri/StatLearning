#2
#2.7
x1  = c(0,2,0,0,-1,1)
x2 = c(3,0,1,1,0,1)
x3 = c(0,0,3,2,1,1)
y = c("Red","Red","Red","Green","Green","Red")
dataFrame = cbind(x1,x2,x3,y)
dataFrame = as.data.frame(dataFrame)
test.dataFrame = data.frame(x1=0,x2=0,x3=0)
library(class)
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
