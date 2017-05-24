#3 LIGHT
p = seq(0, 1, 0.01)
gini = p * (1 - p) * 2
entropy = -(p * log(p) + (1 - p) * log(1 - p))
class.err = 1 - pmax(p, 1 - p)
matplot(p, cbind(gini, entropy, class.err), col = c("red", "green", "blue"))
#7 LIGHT
set.seed(1101)

# Construct the train and test matrices
train = sample(dim(Boston)[1], dim(Boston)[1]/2)
X.train = Boston[train, -14]
X.test = Boston[-train, -14]
Y.train = Boston[train, 14]
Y.test = Boston[-train, 14]

p = dim(Boston)[2] - 1
p.2 = p/2
p.sq = sqrt(p)
# rf LSO CREATES 
rf.boston.p = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                           mtry = p, ntree = 500)
rf.boston.p.2 = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                             mtry = p.2, ntree = 500)
rf.boston.p.sq = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                              mtry = p.sq, ntree = 500)

plot(1:500, rf.boston.p$test$mse, col = "green", type = "l", xlab = "Number of Trees", 
     ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.boston.p.2$test$mse, col = "red", type = "l")
lines(1:500, rf.boston.p.sq$test$mse, col = "blue", type = "l")
legend("topright", c("m=p", "m=p/2", "m=sqrt(p)"), col = c("green", "red", "blue"), 
       cex = 1, lty = 1)

#8
library(caret)
trainIndex = createDataPartition(Carseats$Sales,p = 0.8,list = F)
train_data = Carseats[trainIndex,]
test_data = Carseats[-trainIndex,]

rtree_carseats = tree(Sales~.,train_data)
plot(rtree_carseats)
text(rtree_carseats,pretty = 0)
summary(rtree_carseats)
# 16 terminal nodes,ShelvLoc is the most important predictor,
yhat = predict(rtree_carseats,test_data)
ModelMetrics::mse(actual = test_data$Sales,yhat)#3.811
#TRY TO PRUNE TREE
cv_rtree_carseats = cv.tree(rtree_carseats)
dataF = data.frame(no_of_Tnodes = cv_rtree_carseats$size,error = cv_rtree_carseats$dev,cost=cv_rtree_carseats$k)
ggplot(aes(no_of_Tnodes,error),data = dataF)+geom_line(color="blue")
ggplot(aes(cost,error),data = dataF)+geom_line(color="green")

pruned_tree_carseats = prune.tree(rtree_carseats,best = 14)
yhat = predict(pruned_tree_carseats,newdata = test_data)
ModelMetrics::mse(actual = test_data$Sales,predicted = yhat) #3.5753
library(randomForest)
bag_carseats = randomForest(Sales~.,data = train_data,mtry=10,importance=T)
importance(bag_carseats)#price,shelvloc,age,compprice are imortant predictors
# on another note the statified sampling to random sampling changes the important coefficient order atleast 
#take good care on how test_data would be
yhat = predict(bag_carseats,newdata = test_data)
ModelMetrics::mse(actual = test_data$Sales,yhat) # 1.761642

testMse_rf =rep(NA,10)
for(m in 1:10){
        fit = randomForest(Sales~.,data = train_data,mtry=10,importance=T)
        yhat = predict(fit,newdata=test_data)
        testMse_rf[m] = ModelMetrics::mse(actual = test_data$Sales,predicted = yhat)
}
plot(testMse_rf)
# minimum for 6
testMse_rf[6] #1.752 
testMse_rf[2] #1.753

# little improvement but, m=2 model will be less probable to overfit

#9
dim(OJ)
?OJ # minutemaid orange juice

trainIndex = createDataPartition(OJ$Purchase,p = 800/dim(OJ)[1],list=F)
train_data = OJ[trainIndex,]
dim(train_data)
test_data = OJ[-trainIndex,]

#b
tree_OJ = tree(Purchase~.,data=train_data)
plot(tree_OJ)
text(tree_OJ,pretty = 0)
summary(tree_OJ)

# lets see how random split works with the data
randomSample = sample(dim(OJ)[1],800)
train_data2 = OJ[randomSample,]
test_data2 = OJ[-randomSample,]
tree_OJ2 = tree(Purchase~.,data=train_data2)
plot(tree_OJ2)
text(tree_OJ2)
summary(tree_OJ2)
# different trees resulted from different splits on training data

?OJ
# data is collected from 5 possible stores ,based on item and customer chars, hence random split is better as we cant say the test set
# will retain the class balance
summary(OJ$Purchase)

#c
tree_OJ2
# terminal node ten resulted from loyalch<0.48,and price diff < 0.31, and there are 197 obs in this node, deviance is 209
#d
plot(tree_OJ2)
text(tree_OJ2)
#e
yhat = predict(tree_OJ2,newdata = test_data2,type="class")
table(actual = test_data2$Purchase,predicted = yhat)
# classification error rate = 57/270 = 0.212
cv_tree_OJ = cv.tree(tree_OJ2,FUN = prune.misclass)
dataF = data.frame(no_of_Tnodes = cv_tree_OJ$size,error = cv_tree_OJ$dev,cost=cv_tree_OJ$k)
ggplot(aes(no_of_Tnodes,error),data = dataF)+geom_line()
which.min(cv_tree_OJ$dev)
# min at 5
pruned_tree_oj = prune.misclass(tree_OJ2,best=5)
yhat_pruned = predict(pruned_tree_oj,newdata = test_data2,type="class")
summary(tree_OJ2) # training miscalss 0.1612
summary(pruned_tree_oj) # train 0.1662
table(predicted=yhat_pruned,actual=test_data2$Purchase)
#57270 which is same as unpruned tree

#10
sapply(Hitters,function(x){sum(is.na(x))})
library(dplyr)
Hitters = Hitters %>% filter(!is.na(Salary))

Hitters$Salary = log(Hitters$Salary)

train_data = Hitters[1:200,]
test_data = Hitters[201:dim(Hitters)[1],]
lambda = c(10^-10,10^-5,0.0001,0.01,0.1,0.2,0.3,0.4)
train_errors = rep(NA,8)
test_errors = rep(NA,8)
library(gbm)
        for ( i in 1:length(lambda)){
        gbm_hitters = gbm(formula = Salary~.,distribution = "gaussian",data = train_data,shrinkage = lambda[i],n.trees = 5000)
        yhat_train = predict(gbm_hitters,newdata = train_data,n.trees = 5000)
        yhat_test = predict(gbm_hitters,newdata = test_data,n.trees = 5000)
        train_errors[i] = ModelMetrics::mse(actual = train_data$Salary,predicted = yhat_train)
        test_errors[i] = ModelMetrics::mse(actual = test_data$Salary,predicted = yhat_test)
}

plot(lambda, train_errors, type = "b", xlab = "Shrinkage", ylab = "Train MSE", 
     col = "blue", pch = 20)

plot(lambda, test_errors, type = "b", xlab = "Shrinkage", ylab = "Test MSE", 
     col = "blue", pch = 20)
lambda[which.min(test_errors)]
summary(gbm(formula = Salary~.,distribution = "gaussian",data = train_data,shrinkage = 0.01,n.trees = 5000))
#11
dim(Caravan)
summary(Caravan$Purchase) # class imbalance
Caravan$Purchase = ifelse(Caravan$Purchase=="Yes",1,0)
train_data = Caravan[1:1000,]
summary(as.factor(train_data$Purchase))
test_data = Caravan[1001:dim(Caravan)[1],]
#b
gbm_caravan = gbm(Purchase~.,data=train_data,distribution = "bernoulli",n.trees = 1000,shrinkage = 0.01)
purchaseProbs = predict(gbm_caravan,n.trees = 1000,type="response",newdata = test_data)
summary(gbm_caravan)
#PPERSAUT, MKOOPKLA and MOPLHOOG are three most important variables in that order.
#c
logit_caravan = glm(Purchase~.,data=train_data,family = binomial)
logit_prob = predict(logit_caravan,newdata = test_data,type="response")
yhat_logit = ifelse(logit_prob>0.2,1,0)
table(actual=test_data$Purchase,predicted=yhat_logit)
yhat_gbm = ifelse(purchaseProbs>0.2,1,0)
table(actual=test_data$Purchase,predicted=yhat_gbm) #GBM better accuracy
#precsion logit -> 0.142 , gbm = 0.209

#12 refer titanic in kaggle
