library(ISLR)

# best subset selection
names(Hitters)
dim(Hitters)
sapply(Hitters,function(X){sum(is.na(X))})
59/320 # proportion of missing values of salary
Hitters = na.omit(Hitters)
regfit.full = leaps::regsubsets(Salary~.,Hitters,nvmax=19)
summary(regfit.full)
regfit.summary = summary(regfit.full)
names(regfit.summary) # all adjusted rss to select best possible subset
par(mfrow = c(2,2))
plot(regfit.summary$rss)
plot(regfit.summary$adjr2)
which.max(regfit.summary$adjr2)
points(11,regfit.summary$adjr2[11],col="red",cex=4,pch=20)
plot(regfit.summary$cp)
which.min(regfit.summary$cp)
points(10,regfit.summary$cp[10],col="blue",cex=3,pch=20)
plot(regfit.summary$bic)
points(which.min(regfit.summary$bic),regfit.summary$bic[which.min(regfit.summary$bic)],col="green",pch=20,cex=4)
par(mfrow=c(1,1))

#alternatively we can use the built in plot functions by regsubsets function
plot(regfit.full,scale = "r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="bic")
coef(regfit.full,6)

#Forward and backward subset selection
regfit.fwd = leaps::regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
coef(regfit.full,7)
coef(regfit.fwd,7)

# using corss validation to select best model (instead of adjR2 cp BIC AIC)
set.seed(1)
train = sample(c(T,F),nrow(Hitters),replace = T)
test = !train
library(leaps)
regfit.best = regsubsets(Salary~.,data=Hitters[train,],nvmax = 19)
test.mat = model.matrix(Salary~.,data = Hitters[test,])
#note : regsubsets does not have a predict function, using the below loop
val.errors = rep(NA,19)
for(i in 1:19) {
        coefi = coef(regfit.best,id = i)
        pred = test.mat[,names(coefi)] %*% coefi
        val.errors[i] = (mean(Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)

# best model with 7 variables using full dataset may contain different coefficients compared to the best model using full train set
# ISLR used full data set to recompute best 7 variable model, but i disagree
regfit.best = regsubsets(Salary~.,nvmax = 19,data=Hitters)
coef(regfit.best,id=7)
regfit.best = lm(Salary~AtBat+Hits+HmRun+CAtBat+CHits+League+PutOuts,data=Hitters) #using best 7 coef by validation method
# predict function
predict.regsubsets = function(object,newdata,id,...){
        form = as.formula(object$call[[2]])
        mat = model.matrix(form,newdata)
        coefi = coef(object,id=id)
        xvars = names(coefi)
        mat[,xvars] %*% coefi
}

# using cross validation 
k=10
set.seed(10)
folds = sample(1:k,nrow(Hitters),replace=T)
cv.errors = matrix(NA,k,19,dimnames = list(NULL<paste(1:19)))
for(j in 1:k) {
        best.fit = regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax = 19)
        for(i in 1:19){
                pred = predict.regsubsets(best.fit,Hitters[folds==j,],id=i)
                cv.errors[j,i] = mean((Hitters$Salary[folds==j]-pred)^2)
        }
}
cv.errors
plot(apply(cv.errors,2,mean))
points(which.min(apply(cv.errors,2,mean)),apply(cv.errors,2,mean)[10],col="blue",pch=20)

#Ridge and lasso
#using glmnet
library(glmnet)
# note: model.matrix also converts qualitative variables into dummy variables
x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary
# argument alpha = 0 for ridge regression alpha=1 for lasso
grid = 10^seq(-2,10,length.out = 100)
ridge.fit = glmnet(x = x,y = y,alpha = 0,lambda = grid)
#Note : by default glmnet standardises variable, can be turned off with argument standardised =F
dim(coef(ridge.fit));coef(ridge.fit)[,1] #  a column for each lambda
ridge.fit$lambda[50]
coef(ridge.fit)[,50]
predict(ridge.fit,s=50,type = "coefficients")[1:20,]
predict(ridge.fit,s=0,newx = x[1:20,],type = "response")[1:20,]
Hitters$Salary[1:20]
predict(lm(Salary~.,Hitters))[1:20]
# s= 0 predictions very close to lm functions least sqaure fit

# split observation into train and test set
set.seed(1)
train = sample(1:nrow(x),nrow(x)/2)
test = -train
y.test = y[test]
ridge.fit = glmnet(x[train,],y[train],alpha=0,lambda = grid)
ridge.fit2 = glmnet(x[train,],y[train],alpha=0,lambda = grid,thresh = 1e-12) #default threshhold is 1e+7
ridge.pred = predict(ridge.fit,newx = x[test,],s = 4)
mean((ridge.pred-y.test)^2)
ridge.pred = predict(ridge.fit2,newx=x[test,],s=4)
mean((ridge.pred-y.test)^2)

# if we simply use model with just an intercept , we predict mean of training observationall the time
mean((y.test-mean(y[train]))^2)

# we shoulg get same MSE when we use very high value of lambda

ridge.pred = predict(ridge.fit,s=1e+10,newx = x[test,])
mean((ridge.pred-y.test)^2)

# check if there is any benifit of using lambda = 4 over simple least squares
ridge.pred = predict(ridge.fit,s=0,newx = x[test,],type = "coefficients")
ridge.pred[1:20,]
lm.fit = lm(y~x,subset=train)
lm.pred = predict(lm.fit,newdata = data.frame(x = x[test,]))
predict(ridge.fit,s=0,newx = x[test,],type = "coefficients",exact = T)[1:20,]
# still the coefficients differ from least squares fit

# we can use cv to estimate best lambda using built in cv.glmnet function
cv.out = cv.glmnet(x[train,],y = y[train],alpha=0) # default 10 fold cv, can be changed with 'nfolds' argument
plot(cv.out)
cv.out$lambda.min
ridge.pred = predict(ridge.fit,newx = x[test,],s = 24.34514)
mean((y.test - ridge.pred)^2)

#LASSO
lasso.fit = glmnet(x = x[train,],y = y[train],alpha = 1,lambda = grid)
plot(lasso.fit) # we can see that some coefficeients are exactly zero
predict(lasso.fit,s = 0,type="nonzero")
predict(lasso.fit,s=150,type="nonzero")

#cv to find best lambda
set.seed(1)
cv.out = cv.glmnet(x[train,],y = y[train],alpha=1) # default value of alpha is 1 , hence we need not specify it
plot(cv.out)
cv.out$lambda.min
lasso.pred = predict(lasso.fit,s=35.32063,newx = x[test,])
mean((y.test - lasso.pred)^2) # best than lm and similar to ridge


# PCR and PLS regression

library(pls)
set.seed(2)
pcr.fit = pcr(Salary~.,data=Hitters,scale=T,validation="CV")
summary(pcr.fit) # MSE = RMSE ^2 ;least RMSEP reported for 16 components,but RMSEP of using 1 component is almost the same
validationplot(pcr.fit,val.type = "RMSEP")

set.seed(1)
pcr.fit = pcr(Salary~.,data=Hitters,subset=train,scale=T,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")
summary(pcr.fit)
pcr.pred = predict(object = pcr.fit,newdata = x[test,],ncomp = 7)
mean((pcr.pred - y.test)^2)

# fit PCR using full data and 7 components
pcr.fit = pcr(Salary~.,data=Hitters,scale=T,validation="CV",ncomp=7)
summary(pcr.fit)

#PLS
set.seed(1)
pls.fit = plsr(Salary~.,data=Hitters,scale=T,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type = "RMSEP") #best 11 components
#5 variation explained in Salary with 2 variables in PLS is same as that by using 7 components of PCR,as PCR does not try to maximise 
# variance in y


#EX
#6
#assumptions no intercept,x is a diagonal matrix with 1 in diagonal and 0 elsewhere
y=2 #let y =2
lambda = 2 # say
p = 1
beta = seq(from = -10,to=10,by = 0.1)
ridgecost = (y-beta)^2+2*(beta^2)
plot(beta,ridgecost)
betahat = y/(1+lambda)
ridge_estimated_min = (y - betahat)^2+2*(betahat^2)
points(betahat,ridge_estimated_min,col="red",pch=4,cex=3)
lassocost = (y-beta)^2+2*abs(beta)
plot(beta,lassocost)
betahat = y - lambda/2
lasso_estimated_min = (y - betahat)^2+2*abs(betahat)
points(betahat,lasso_estimated_min,col="blue",pch=4,cex=3)

#7 ?? likelihood function??

#8
X = rnorm(n = 100,mean = 2,sd = 2)
epsilon = rnorm(100,0,1)
beta0 = 1;beta1 = 2;beta2 = 3;beta3 = 4  # say
y = beta0+beta1*X+beta2*X^2+beta3*X^3+epsilon
sampleData = data.frame(x=X,y=y)
bestsubsets.fir = regsubsets(y~poly(x, 10,raw = T),data=sampleData,nvmax = 10)
mod.summary = summary(bestsubsets.fir)
which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
plot(mod.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(4, mod.summary$cp[4], pch = 4, col = "red", lwd = 7)
plot(mod.summary$bic, xlab = "Subset Size", ylab = "aic", pch = 20, type = "l")
points(3, mod.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(mod.summary$adjr2, xlab = "Subset Size", ylab = "adjr2", pch = 20, type = "l")
points(9, mod.summary$adjr2[9], pch = 4, col = "red", lwd = 7)

forwardselect.fit = regsubsets(y~poly(x, 10,raw = T),data=sampleData,nvmax = 10,method = "forward")
mod.summary = summary(forwardselect.fit)
which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
plot(mod.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(4, mod.summary$cp[4], pch = 4, col = "red", lwd = 7)
plot(mod.summary$bic, xlab = "Subset Size", ylab = "aic", pch = 20, type = "l")
points(3, mod.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(mod.summary$adjr2, xlab = "Subset Size", ylab = "adjr2", pch = 20, type = "l")
points(10, mod.summary$adjr2[10], pch = 4, col = "red", lwd = 7)

backwardselect.fit = regsubsets(y~poly(x, 10,raw = T),data=sampleData,nvmax = 10,method="backward")
mod.summary = summary(backwardselect.fit)
which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
plot(mod.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(4, mod.summary$cp[4], pch = 4, col = "red", lwd = 7)
plot(mod.summary$bic, xlab = "Subset Size", ylab = "aic", pch = 20, type = "l")
points(3, mod.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(mod.summary$adjr2, xlab = "Subset Size", ylab = "adjr2", pch = 20, type = "l")
points(9, mod.summary$adjr2[9], pch = 4, col = "red", lwd = 7)

xmat = model.matrix(y ~ poly(x, 10, raw = T), data = sampleData)[, -1]
mod.lasso = cv.glmnet(xmat, y, alpha = 1)
mod.lasso
best.lambda = mod.lasso$lambda.min
best.lambda
plot(mod.lasso)
points()