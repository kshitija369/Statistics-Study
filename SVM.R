
### SVM
set.seed(1)

x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1,10), rep(1,10))
x[y==1, ] = x[y==1, ] + 1

plot(x, col=(3-y))

dat = data.frame(x=x, y= as.factor(y))
library("e1071")
svmfit = svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)

plot(svmfit, dat)
svmfit$index
summary(svmfit)

set.seed(1)
tune.out = tune(svm, y~., data = dat, kernel="linear", 
                ranges = list(cost = c(0.001 , 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
best.mod = tune.out$best.model
summary(best.mod)

### Predict

x_test = matrix(rnorm(20*2), ncol = 2)
y = sample(c(-1,1), 20, replace = T)
x[y==1, ] = x[y==1, ] + 1
testdata = data.frame(x=x, y=as.factor(y))

ypred = predict(best.mod, testdata)
table(predict=ypred, truth= testdata$y)

### Linearly separable
x[y==1,] = x[y==1,] +.5
plot(x=x, col=(y+5)/2, pch=19)

dat = data.frame(x=x, y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit,dat)


svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)

### Non-Linear decision boundaries
set.seed(1)
x = matrix(rnorm(200*2), ncol=2)
x[1:100, ] = x[1:100, ] + 2
x[101:150, ] = x[101:150, ] - 2
y=c(rep(1,150), rep(2,50))
dat = data.frame(x=x, y=as.factor(y))

plot(x, col=y)

train=sample(200,100)
svmfit = svm(y~., dat[train,], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat[train,])

### Select best gamma
set.seed(1)
tune.out=tune(svm , y~. , data=dat[train ,], kernel =" radial ",
              ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                           gamma=c(0.5,1,2,3,4) ))
summary(tune.out)
table(truth=dat[-train,"y"], predict=predict(tune.out$best.model, newdata = dat[-train,]))

### ROC
library(ROCR)
rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)
}

svmfit.opt=svm(y~., data=dat[train ,], kernel ="radial",
               gamma =2, cost=1, decision.values =T)
fitted=attributes(predict(svmfit.opt, dat[train, ], decision.values =TRUE))$decision.values

par(mfrow =c(1,2))
rocplot (fitted ,dat [train ,"y"], main=" Training Data")

svmfit.flex=svm(y~., data=dat[train ,], kernel ="radial",
                  gamma =50, cost=1, decision.values =T)
fitted=attributes(predict(svmfit.flex, dat[train, ], decision.values =TRUE))$decision.values
rocplot (fitted ,dat [train ,"y"], add =T,col ="red ")
                          
fitted=attributes(predict(svmfit.opt, dat[-train, ], decision.values =TRUE))$decision.values
rocplot (fitted ,dat [-train ,"y"], main=" Test Data")

fitted=attributes(predict(svmfit.flex, dat[-train, ], decision.values =TRUE))$decision.values
rocplot (fitted ,dat [-train ,"y"], add =T,col ="red ")

### Multiple classes
set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y = c(y, rep(0,50))
x[y==0, 2] = x[y==0, 2] +2
dat=data.frame(x=x, y=as.factor(y))

par(mfrow=c(1,1))
plot(x,col=(y+1))

svmfit = svm(y~., data=dat, kernel="radial", gamma=1, cost=10)
plot(svmfit, dat)





