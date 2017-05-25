#1
X1= rnorm(100)
X2= rnorm(100)
Y1 = 1+3*X1-X2
Y1_binary = ifelse(Y1>0,1,0) 
dat1 = data.frame(X1 =X1,X2=X2,Y=Y1,binary=as.factor(Y1_binary))
Y2 = -2+X1+2*X2
Y2_binary = ifelse(Y2>0,1,0)
dat2 = data.frame(X1 =X1,X2=X2,Y=Y2,binary=Y2_binary)
#ggplot(aes(x=X1),data = dat1)+geom_point(aes(y=X2,col=binary))+geom_smooth(aes(y=Y),data = dat1,fill=NA)+geom_smooth(aes(y=Y),data = dat2,fill=NA)
ggplot(aes(x=X1),data = dat1)+geom_point(aes(y=X2,col=binary))+geom_abline(aes(y=X2),slope = 3,intercept = 1)+geom_abline(aes(y=X2),slope = -0.5,intercept = 1)

# straight Forward

#3
X1 = c(3,2,4,1,2,4,4)
X2 = c(4,2,4,4,1,3,1)
obs = 1:7
Y=c(rep("Red",4),rep("Blue",3))
dataF = data.frame(X1,X2,Y,obs)
ggplot(dataF,aes(X1,X2,color=Y))+geom_point()
# (c,h) trivial

#4

