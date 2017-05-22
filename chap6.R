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
# or use this function
cv_bestsubsets = function(trainSet, max_number_of_predictors,response_name,num_of_folds,...){
        folds = sample(1:max_number_of_predictors,nrow(trainSet),replace = T)
        cv_errors = matrix(NA, nrow = num_of_folds, ncol = max_number_of_predictors)
        for(j in 1:num_of_folds){
                train_data = trainSet[folds!=j,]
                test_data = trainSet[folds==j,]
                formuala = as.formula(paste(response_name,"~.",sep=""))
                regfit_fold = regsubsets(formuala,nvmax = max_number_of_predictors,data = train_data)
                test_matrix = model.matrix(object = formuala,data = test_data)
                for(i in 1:max_number_of_predictors){
                        coefi = coef(regfit_fold,id=i)
                        model_columns = names(coefi)
                        predictions = test_matrix[,model_columns] %*% coefi
                        cv_errors[j,i] = ModelMetrics::mse(test_data[,response_name],predictions)
                }
        }
        return(cv_errors)
}

#Ridge and lasso
#using glmnet
library(glmnet)
# note: model.matrix also converts qualitative variables into dummy variables
x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary
# argument alpha = 0 for ridge regression alpha=1 for lasso
grid = 10^seq(-2,10,length.out = 100)
# always better to set grid from exponential values
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
ridge.fit2 = glmnet(x[train,],y[train],alpha=0,lambda = grid,thresh = 1e-12) 
# threshhold = thresh hold error value for gradient descent default threshhold is 1e+7
ridge.pred = predict(ridge.fit,newx = x[test,],s = 4)
mean((ridge.pred-y.test)^2)
ridge.pred = predict(ridge.fit2,newx=x[test,],s=4)
mean((ridge.pred-y.test)^2)

# if we simply use model with just an intercept , we predict mean of training observation all the time
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
summary(pcr.fit) # MSE = RMSE ^2 ;least RMSEP(P=of Prediction) reported for 16 components,but RMSEP of using 1 component is almost the same
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

#7  theoretical, for prior double exponential,normal => lasso,ridge estimate is the equivalent of mode 

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
xmat = model.matrix(Y ~ poly(X, 10, raw = T), data = dataF)[, -1]
cv_lasso = cv.glmnet(xmat, y, alpha = 1)
cv_lasso
best_lambda = cv_lasso$lambda.min
best_lambda
plot(cv_lasso)
# fit lasso on entrire data
mod_lasso = glmnet(xmat,Y,alpha = 1) # using default lambda sequence
predict(mod_lasso,s = best_lambda,type = "coefficients")

#f
y = beta0+7*x^7+eps
dataF = data.frame(x,y)
regfit_10vars = regsubsets(y~poly(x,10,raw = T),data = dataF,nvmax = 10)
summary(regfit_10vars)
regfit_10vars_summary = summary(regfit_10vars)
names(regfit_10vars_summary)
ggplot(aes(X,cp),data=data.frame(X=1:10,cp=regfit_10vars_summary$cp))+geom_point()# almost same cp from 3
which.min(regfit_10vars_summary$cp) # 2 var best
ggplot(aes(X,adjr2),data=data.frame(X=1:10,adjr2=regfit_10vars_summary$adjr2))+geom_point()# almost same adjr2 from 3
which.max(regfit_10vars_summary$adjr2) # 7 var best
ggplot(aes(X,bic),data=data.frame(X=1:10,bic=regfit_10vars_summary$bic))+geom_point() # 2 var best
which.min(regfit_10vars_summary$cp) #3
xmat = model.matrix(y ~ poly(x, 10, raw = T), data = dataF)[, -1]
cv_lasso = cv.glmnet(xmat, y, alpha = 1)
cv_lasso
best_lambda = cv_lasso$lambda.min
best_lambda
plot(cv_lasso)
# fit lasso on entrire data
mod_lasso = glmnet(xmat,y,alpha = 1) # using default lambda sequence
predict(mod_lasso,s = best_lambda,type = "coefficients") # best is 7 var but intercept and slope differ, mainly intercept
# checking the coefficients of best subsets
coefficients(regfit_10vars,id=1) # better estimate of intercep and coefficients
coefficients(regfit_10vars,id=2)

#9
#a
library(caret)
train_index = createDataPartition(College$Apps,p = 0.7,list = F)
train_College = College[train_index,]
test_College = College[-train_index,]
#b
linear_fit_college = lm(Apps~.,data = train_College)
yhat_linear_fit = predict(object = linear_fit_college,newdata = test_College,type = "response")
error_lm = ModelMetrics::rmse(actual = test_College$Apps,predicted = yhat_linear_fit)
#c
library(glmnet)
xmat = model.matrix(Apps~.,data=train_College)[,-1] # -1 => remove intercept
y = train_College$Apps
cv_ridge_fit_college = cv.glmnet(xmat,y,alpha=0) # standardise = T by default
best_lambda_ridge = cv_ridge_fit_college$lambda.min
# fit on entire training set
ridge_fit_college = glmnet(x =xmat,y=y,alpha = 0)
newx_mat = model.matrix(Apps~.,data = test_College)[,-1]
yhat_ridge_college = predict(ridge_fit_college,newx = newx_mat,s=best_lambda_ridge)
error_ridge = ModelMetrics::rmse(actual = test_College$Apps,predicted = yhat_ridge_college)
#d
cv_lasso_fit_college = cv.glmnet(xmat,y,alpha=1)
best_lambda_lasso = cv_lasso_fit_college$lambda.min
# fit on entire training set
lasso_fit_college = glmnet(x =xmat,y=y,alpha = 1)
yhat_lasso_college = predict(lasso_fit_college,newx = newx_mat,s=best_lambda_lasso)
error_lasso = ModelMetrics::rmse(actual = test_College$Apps,predicted = yhat_lasso_college)
predict(lasso_fit_college,newx = newx_mat,s=best_lambda_lasso,type = "coefficients") 
#e
library(pls)
set.seed(2)
cv_pcr_fit = pcr(Apps~.,data=train_College,scale=T,validation="CV")
summary(cv_pcr_fit)
validationplot(cv_pcr_fit)# using 10 as minimum
# fit to entire training data
pcr_fit = pcr(Apps~.,data=train_College,scale=T)
yhat_pcr = predict(pcr_fit,newdata = test_College,ncomp = 10) # it turns outcv_pcr_fit = pcr_fit
error_pcr = ModelMetrics::rmse(actual = test_College$Apps,predicted = yhat_pcr)
#f
set.seed(2)
cv_plsr_fit = plsr(Apps~.,data=train_College,scale=T,validation="CV")
summary(cv_plsr_fit)
validationplot(cv_plsr_fit)# using 6 as minimum
# fit to entire training data
plsr_fit = plsr(Apps~.,data=train_College,scale=T)
yhat_plsr = predict(cv_plsr_fit,newdata = test_College,ncomp = 6) # it turns outcv_plsr_fit = plsr_fit
error_plsr = ModelMetrics::rmse(actual = test_College$Apps,predicted = yhat_plsr)
#g
x = c('linear','ridge','lasso','pcr','plsr')
y = c(error_lm,error_ridge,error_lasso,error_pcr,error_plsr)
ggplot(aes(X,Y),data=data.frame(X=x,Y=y))+geom_bar(stat = "identity")

#10
#a
set.seed(27)
beta = rnorm(20)
beta[3,7,11,15,16,17,18,19,20] = 0
X = matrix(rnorm(20*1000,mean = 8,sd = 2),nrow = 1000)
X[,11:20] = X[,11:20]+matrix(rnorm(10*1000,mean = 8),nrow = 1000)
set.seed(28)
eps = rnorm(1000)
Y = X %*% beta + eps
dataF = data.frame(x=X,y=Y)
#b
# take a random sample
train_index = sample(1:1000,100)
train_data = dataF[train_index,]
test_data = dataF[-train_index,]
#c
best_subsets_train = regsubsets(y~.,data = dataF,nvmax = 20)
summary_best_subsets = summary(best_subsets_train)
ggplot(aes(x =index ,y=rss),data = data.frame(index=1:20,rss = summary_best_subsets$rss)) +geom_line()
#d
val_errors = rep(NA,20)
Xtest = model.matrix(y~.,test_data)
for ( i in 1:20) {
        coefi = coef(best_subsets_train,id=i)
        yhat = Xtest[,names(coefi)] %*% coefi
        val_errors[i] = ModelMetrics::mse(actual = test_data$y,predicted = yhat) * 1000
}
ggplot(aes(x = index,y = rss ),data=data.frame(index=1:20,rss=val_errors)) +geom_line()
which.min(val_errors)
#e
#f
#g

#11
?Boston
library(MASS)
#a
# Note that its better to use CV for performance evaluation too but using validation set for simplicity
trainIdex = createDataPartition(Boston$crim,p = 0.8,list=F)
trainData = Boston[trainIdex,]
testData = Boston[-trainIdex,]
best_subset_boston = regsubsets(crim~.,data = trainData,nvmax = 13)
valErrors = rep(NA,13)
test_data_matrix = model.matrix(crim~.,testData)
for( i in 1:13) {
        coefi = coef(best_subset_boston,id=i)
        model_columns = names(coefi)
        predictions = test_data_matrix[,model_columns] %*% coefi
        valErrors[i] = ModelMetrics::mse(testData[,"crim"],predictions)
}
which.min(valErrors) # 8 vars
# CV function for best subsets
cv_bestsubsets = function(trainSet, max_number_of_predictors,response_name,num_of_folds,...){
        folds = sample(1:max_number_of_predictors,nrow(trainSet),replace = T)
        cv_errors = matrix(NA, nrow = num_of_folds, ncol = max_number_of_predictors)
        for(j in 1:num_of_folds){
                train_data = trainSet[folds!=j,]
                test_data = trainSet[folds==j,]
                formuala = as.formula(paste(response_name,"~.",sep=""))
                regfit_fold = regsubsets(formuala,nvmax = max_number_of_predictors,data = train_data)
                test_matrix = model.matrix(object = formuala,data = test_data)
                for(i in 1:max_number_of_predictors){
                        coefi = coef(regfit_fold,id=i)
                        model_columns = names(coefi)
                        predictions = test_matrix[,model_columns] %*% coefi
                        cv_errors[j,i] = ModelMetrics::mse(test_data[,response_name],predictions)
                }
        }
        return(cv_errors)
}
# using 10 folds for hyperparameter cv as train set is not so big
cvErrors = cv_bestsubsets(trainData,13,"crim",10)
which.min(apply(cvErrors,2,mean)) # 2 is best
# in case of validation apprach 8 is best, and in case of cv its 2. using 2 crudely instead of further testing(to minimise work)
# use 2 to fit on entire train set
bestSubset = regsubsets(crim~.,data = trainData)
testMatrix = model.matrix(crim~.,testData)
coefi = coef(bestSubset,id=2)
yhatFinal = testMatrix[,names(coefi)] %*% coefi
rmse_bestsubset = ModelMetrics::rmse(actual = testData$crim,yhatFinal)
# ridge
xmat = model.matrix(crim~.,data=trainData)[,-1]
y = trainData$crim
cvRidgeBoston = cv.glmnet(x = xmat,y = y,alpha=0)
bestLambda = cvRidgeBoston$lambda.min
# fit on entire training set
ridgeBoston = glmnet(x =xmat,y=y,alpha = 0)
newDataMat = model.matrix(crim~.,data =testData )[,-1]
yhatRidge = predict(ridgeBoston,newx = newDataMat,s=bestLambda)
error_ridge = ModelMetrics::rmse(actual = testData$crim,predicted = yhatRidge)
error_ridge

#lasso
cvlassoBoston = cv.glmnet(x=xmat,y=y,alpha=1)
bestLambda = cvlassoBoston$lambda.min
#fit on entire
lassoBoston = glmnet(x=xmat,y=y,alpha=0)
yhatLasso = predict(object = lassoBoston,newx = newDataMat,s=bestLambda)
error_lasso = ModelMetrics::rmse(actual = testData$crim,predicted = yhatLasso)
error_lasso # almost same as ridge
predict(lassoBoston,s=bestLambda,type="coefficients") # can be though as a 8 variable model

#pcr
cvPCR = pcr(crim~.,data=trainData,scale=T,validation="CV")
validationplot(cvPCR) # using 8
yhatPCR = predict(cvPCR,ncomp = 8,newdata = testData)
error_pcr = ModelMetrics::rmse(actual = testData$crim,predicted = yhatPCR)
#plsr
cvPlsr = plsr(crim~.,data=trainData,scale=T,validation="CV")
validationplot(cvPlsr) # using 7 components
yhatPlsr = predict(cvPlsr,newdata = testData,ncomp = 7)
error_plsr = ModelMetrics::rmse(actual = testData$crim,yhatPlsr)
#b
x = c('bestSubsets','ridge','lasso','pcr','plsr')
y = c(rmse_bestsubset,error_ridge,error_lasso,error_pcr,error_plsr)
ggplot(aes(X,Y),data=data.frame(X=x,Y=y))+geom_bar(stat = "identity")

# RMSE is very close with all the methods, lasso edges slightly
#c
# lasso seems to be the best performing  which has atleast 5 coefficients very close to zero, hence we will use model with not all variables