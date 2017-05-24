# PARAMETRIC NON LINEAR
#1.polynomial regression #2.piecewise constant(step function) 3.regression splines & natural splines 4.smoothing splines 5.localregression 6.GAMS 

# WAGE DATA
library(ISLR)
library(ggplot2)
?Wage
poly_fit = lm(wage~poly(age,degree = 4),data=Wage)
summary(poly_fit)
poly_fit = lm(wage~poly(age,raw = T,degree = 4),data = Wage)
summary(poly_fit)
# by default poly function works with orthogonal transformed features
#by using raw =T we tell poly to use raw exponents as powers
#Notice that the coeffiecients are different in each case
#thus the coefficients differ wuth the choice of basis function

# we can use I instead
poly_fit2 = lm(wage~age+I(age^2)+I(age^3)+I(age^4),data = Wage)
summary(poly_fit2)

# Create grid of age for plotting
agelims = range(Wage$age)
agegrid = seq(agelims[1],agelims[2])
length(agegrid)
length(Wage$age)
predict_poly_wage = predict(poly_fit,newdata = list(age = agegrid),se.fit  = T)
predict_poly_wage$fit
dataF = data.frame(age = agegrid,wage=predict_poly_wage$fit,upper = predict_poly_wage$fit+2*predict_poly_wage$se.fit,lower=predict_poly_wage$fit-2*predict_poly_wage$se.fit)
ggplot(aes(age,wage),data=Wage)+geom_point(col="grey")+geom_line(data = dataF)+geom_ribbon(data = dataF,aes(ymin=lower,ymax=upper),alpha=0.3)+ggtitle(label = "FIT with se bands")+theme(plot.title = element_text(hjust = 0.5))

# USE ANOVA and poly fit p values to determine the significant model
# to use anova we must use nested models or else we cannot build any hypothesis to conclude
fit1 = lm(wage~age,data=Wage)
fit2 = lm(wage~poly(age,2),data=Wage)
fit3 = lm(wage~poly(age,3),data=Wage)
fit4 = lm(wage~poly(age,4),data=Wage)
fit5 = lm(wage~poly(age,5),data=Wage)
# remember that p values of lm output can be misleading if coefficients are correlated
summary(lm(wage~age+I(age^2)+I(age^3)+I(age^4)+I(age^5),data=Wage))
# see that no value is significant

# but poly provides orthogonal fits hence we van use the p-values
summary(fit5)

# ANOVA is very useful as we can use that to test any hypothesis
anova(fit1,fit2,fit3,fit4,fit5)

# we get the same result by both these approach, infact (lmsummary.tvalue)^2 = anova.Fstatistic

# LOGIT wage >250
logit_fit = glm(I(wage>250)~poly(age,4),data = Wage,family = binomial)
#predict_response_logit = predict(logit_fit,se.fit = T,newdata = data.frame(age=agegrid),type="response")
#predict_response_logit$fit -2*predict_response_logit$se.fit
#what is this output of sebands with type = 'response'? (thevalus are negatives,in this case function has some false sense to calculate se.bands )

# using the default link output to calculate probs and se bands
predict_logit = predict(logit_fit,newdata = list(age = agegrid),se.fit = T)
probs = exp(predict_logit$fit)/(1+exp(predict_logit$fit))
se_lower = predict_logit$fit - 2* predict_logit$se.fit
se_upper = predict_logit$fit +2 *predict_logit$se.fit
se_lower = exp(se_lower)/(1+exp(se_lower))
se_upper = exp(se_upper)/(1+exp(se_upper))
dataF = data.frame(wageHigher = probs,lower=se_lower,age=agegrid,upper=se_upper)

plot(Wage$age,I(Wage$wage>250),ylim=c(0,0.2))
points(jitter(Wage$age),I((Wage$wage>250)/5),pch="|")
lines(agegrid,probs)
matlines(agegrid,cbind(se_lower,se_upper))

ggplot(aes(age,wageHigher),data=data.frame(wageHigher = I(Wage$wage > 250),age=Wage$age))+geom_point
+geom_line(data=dataF)+geom_ribbon(data=dataF,aes(ymin=lower,ymax=upper))+ggtitle(label = "Logit FIT with se bands")+theme(plot.title = element_text(hjust = 0.5))
# ABOVE GRAPH NEEDS CORRECTION


# PIECE WISE CONSTANT (STEP FUNCTION)
lm_step = lm(wage~cut(age,breaks = 4),data=Wage)
predict_lm_step = predict(lm_step,newdata = list(age=agegrid),se.fit = T)
plot(agegrid,predict_lm_step$fit,type="l")

#SPLINES
library(splines)
# bs() can create entire matrix of basis for splines, default cubic spline for specified knotes
cubic_spline_fit = lm(wage~bs(age,knots = c(25,40,60)),data=Wage)
predict_spline_cubic = predict(cubic_spline_fit,newdata = list(age = agegrid),se.fit = T)
dataF = data.frame(wage = predict_spline_cubic$fit,lower = predict_spline_cubic$fit - 2* predict_spline_cubic$se.fit,upper = predict_spline_cubic$fit + 2* predict_spline_cubic$se.fit,age = agegrid)
ggplot(aes(age,wage),data=Wage)+geom_point(col="grey")+geom_line(data = dataF)+geom_ribbon(data = dataF,aes(ymin=lower,ymax=upper),alpha=0.3)+ggtitle(label = "spline FIT with se bands")+theme(plot.title = element_text(hjust = 0.5))
# remember cubic spline has 7 df ( 1 used by intercept+ 6 for basis)
cubic_spline_fit = lm(wage~bs(age,df = 6),data=Wage)
predict_spline_cubic = predict(cubic_spline_fit,newdata = list(age = agegrid),se.fit = T)
dataF = data.frame(wage = predict_spline_cubic$fit,lower = predict_spline_cubic$fit - 2* predict_spline_cubic$se.fit,upper = predict_spline_cubic$fit + 2* predict_spline_cubic$se.fit,age = agegrid)
ggplot(aes(age,wage),data=Wage)+geom_point(col="grey")+geom_line(data = dataF)+geom_ribbon(data = dataF,aes(ymin=lower,ymax=upper),alpha=0.3)+ggtitle(label = "spline FIT with se bands")+theme(plot.title = element_text(hjust = 0.5))
attr(bs(Wage$age,df=6),"knots") # knots selected by bs()
# ns() fro natural splines
natural_spline_fit = lm(wage~bs(age,df = 4),data=Wage)
predict_spline_natural = predict(natural_spline_fit,newdata = list(age = agegrid),se.fit = T)
dataF = data.frame(wage = predict_spline_natural$fit,lower = predict_spline_natural$fit - 2* predict_spline_natural$se.fit,upper = predict_spline_natural$fit + 2* predict_spline_natural$se.fit,age = agegrid)
ggplot(aes(age,wage),data=Wage)+geom_point(col="grey")+geom_line(data = dataF)+geom_ribbon(data = dataF,aes(ymin=lower,ymax=upper),alpha=0.3)+ggtitle(label = "natural spline FIT with se bands")+theme(plot.title = element_text(hjust = 0.5))
# smmothing spline => smooth.spline it has inbuit cv for df
smooth_spline_fit = smooth.spline(Wage$age,Wage$wage,cv=T)
smooth_spline_fit$df
predict_smooth_spline = predict(smooth_spline_fit,newdata = list(age = agegrid),se.fit = T)
dataF = data.frame(wage = predict_smooth_spline$fit,lower = predict_smooth_spline$fit - 2* predict_smooth_spline$se.fit,upper = predict_smooth_spline$fit + 2* predict_smooth_spline$se.fit,age = agegrid)
ggplot(aes(age,wage),data=Wage)+geom_point(col="grey")+geom_line(data = dataF)+geom_ribbon(data = dataF,aes(ymin=lower,ymax=upper),alpha=0.3)+ggtitle(label = "Smooth spline FIT with se bands")+theme(plot.title = element_text(hjust = 0.5))

# loess() => local regression
loess_fit = loess(wage~age,data=Wage,span = 0.2)
predict_loess_fit = predict(loess_fit,newdata = data.frame(age = agegrid),se = T)
dataF = data.frame(wage = predict_loess_fit$fit,lower = predict_loess_fit$fit - 2* predict_loess_fit$se.fit,upper = predict_loess_fit$fit + 2* predict_loess_fit$se.fit,age = agegrid)
ggplot(aes(age,wage),data=Wage)+geom_point(col="grey")+geom_line(data = dataF)+geom_ribbon(data = dataF,aes(ymin=lower,ymax=upper),alpha=0.3)+ggtitle(label = "local regression span 0.2")+theme(plot.title = element_text(hjust = 0.5))

# GAMS
gam1 = lm(wage~ns(year,4)+ns(age,5)+education,data = Wage) # gam can be buid through lm
# but as smoothing splnes and loess cannot be represented as basis function we use library(gam)
library(gam)
gam1 = gam(wage~s(age,5)+education,data=Wage)
gam2 = gam(wage~year+s(age,5)+education,data=Wage)
gam3 = gam(wage~s(year,4)+s(age,5)+education,data=Wage)
anova(gam1,gam2,gam3)
# adding 1 year is significant but higher degree aint
# gam has inbuit plot function
plot.gam(gam2,se = T)

gam_loess = gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data = Wage)
plot(gam_loess)
predict_gam = predict(gam_loess,se=T)
predict_gam$fit

gam_logit = gam(I(wage>250)~year+s(age,df=5)+education,data=Wage)
plot(gam_logit)
table(Wage$education,I(Wage$wage > 250))
# remove empty level and fit gam again
levels(Wage$education)
gam_logit = gam(I(wage>250)~year+s(age,df=5)+education,data=Wage,subset = (education != '1. < HS Grad'))
plot(gam_logit,se=T)

#Ex
#3
grid = seq(-2,2,length.out = 400)
y = 1+grid+-2*(((grid-1)^2) *(!(grid<1))) 
ggplot(aes(X,Y),data=data.frame(X=grid,Y=y))+geom_line()

#4
grid = seq(-2,2,length.out = 100)
X =c(2,1.5,0,-0.5,2,5)
b1 = ((grid > 0 | grid ==0) & (grid==2 |grid<2))-((grid-1)*((grid > 1 | grid ==1) & (grid==2 |grid<2)))
b2 = ((grid > 4) & (grid==5 |grid<5))+((grid-3)*((grid > 3 | grid ==3) & (grid==4 |grid<4)))
yhat = 1+b1+3*b2
ggplot(aes(X,Y),data=data.frame(X=grid,Y=yhat))+geom_line()

#5
#a g1 higher order so g1
# b g1 might overfit so cant say which will be better on test set
#c same

#6
library(boot)
cv.error = rep(NA,10)
for(d in 1:10){
        glm_fit = glm(wage~poly(age,degree = d),data = Wage)
        cv.error[d] = cv.glm(glm_fit,data = Wage,K = 5)$delta[1]
}
ggplot(aes(index,error),data=data.frame(index = 1:10,error=cv.error))+geom_line()
which.min(cv.error)
cv.error
fit1 = lm(wage~poly(age,degree = 1),data = Wage)
fit2 = lm(wage~poly(age,degree = 2),data = Wage)
fit3 = lm(wage~poly(age,degree = 3),data = Wage)
fit4 = lm(wage~poly(age,degree = 4),data = Wage)
fit5 = lm(wage~poly(age,degree = 4),data = Wage)
fit6 = lm(wage~poly(age,degree = 4),data = Wage)
fit7 = lm(wage~poly(age,degree = 4),data = Wage)
fit8 = lm(wage~poly(age,degree = 4),data = Wage)
fit9 = lm(wage~poly(age,degree = 4),data = Wage)
fit10 = lm(wage~poly(age,degree = 9),data = Wage)
anova(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10)
# cv if we strictly go by minimum we select 9, but 3 seems reasonable and anova selects 3
rm(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10)

error = matrix(NA,nrow = 5,ncol = 10)
getBreaks = function(index){
        c(sapply(levels(cut(Wage$age,breaks = index)),function(x){as.numeric(substr(x,2,stringr::str_locate(x,"[,]")))}),max(Wage$age))
}
sample = sample(1:5,nrow(Wage),replace = T)
for(n in 1:10){
        for(fold in 1:5){
        train_data = Wage[sample!=fold,c('wage','age')]
        test_data = Wage[sample==fold,c('wage','age')]
        if(n!=1){
         fit =  lm(wage~cut(age,breaks = getBreaks(n)),data=train_data)
        }else{
                fit = lm(wage~age,data=train_data)
        }
        prediction = predict.lm(fit,newdata = test_data)
        error[fold,n] = ModelMetrics::mse(test_data$wage,prediction)
        }
}
# ERROR in above function getBreaks check gain

#11 ---Backfitting
#a
beta1 = 4
beta2 = 8
eps = rnorm(100,mean=0.1)
X1 = rnorm(100,mean=2,sd=2)
X2 = rnorm(100,mean =4,sd=2)
beta0 = 5
Y = beta0+beta1*X1+beta2*X2+eps
lm_all_fit = lm(Y~X1+X2)
summary(lm_all_fit) # pretty close coeff estimate lets see how back propagation rolls
beta1hat = 12

#c
resid1 = Y-beta1hat*X1
beta2hat = lm(resid1~X2)$coef[2]
beta2hat
#d
resid2 = Y-beta2hat*X2
beta1hat = lm(resid2~X1)$coef[2]
beta1hat
#e,f,g
beta1 = rep(NA,1000)
beta2 = rep(NA,1000)
beta1[1] = 12
beta2[1] = 0
beta0 = rep(NA,1000)
beta0[1] = 0
for(i in 1:999){
        resid1 = Y - beta1[i]*X1
        beta2[i+1] = lm(resid1~X2)$coef[2]
        resid2 = Y - beta2[i+1] * X2
        lm_fit = lm(resid2~X1)
        beta1[i+1] = lm_fit$coef[2]
        beta0[i+1] = lm_fit$coef[1]
}
plot(beta0,type="l")
plot(beta1,type="l")
plot(beta2,type="l")
#it seems that they attain true value after 1 interval as relation ship is linear and X1 and X2 satisfy linear assumptins
#12
# same as 11 but with 100 predictors , so we may need a little more number of iterations to converge
