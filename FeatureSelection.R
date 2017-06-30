
### 6.5 Lab 1: Subset Selection Methods
my.install <- function(pkg, ...){
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}

# Best Subset Selection
my.install("ISLR", character.only = TRUE, warn.conflicts = FALSE)
names(Hitters)
dim(Hitters)

Hitters = na.omit(Hitters)

my.install("leaps", character.only = TRUE, warn.conflicts = FALSE)
regfit.full = regsubsets(Salary~.,Hitters)
summary(regfit.full)

regfit.full = regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary = summary(regfit.full)

names(reg.summary)
reg.summary$rsq

par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Predictors", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Predictors", ylab = "Adjusted RSq", type = "l")

which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red", cex=2, pch=20)

plot(reg.summary$cp,xlab = "Number of Predictors", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10], col="red", cex=2, pch=20)

plot(reg.summary$bic,xlab = "Number of Predictors", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(6,reg.summary$bic[6], col="red", cex=2, pch=20)

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")


### Forward/Backward Stepwise
regfit.fwd = regsubsets(Salary~., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Salary~., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

set.seed(1)
train = sample(c(TRUE,FALSE), nrow(Hitters), replace = TRUE)
test = (!train)

regfit.best = regsubsets(Salary~.,  data = Hitters[train,], nvmax = 19)
test.mat = model.matrix(Salary~., data = Hitters[test,])

var.errors = rep(0,19)
for(i in 1:19){
  coefi=coef(regfit.best, id=i)
  pred=test.mat[, names(coefi)]%*%coefi
  var.errors[i] = mean((Hitters$Salary[which(test)] - pred)^2)
}

print(var.errors)
which.min(var.errors)
coef(regfit.best,10)

### Ridge and Lasso
my.install("glmnet", character.only = TRUE, warn.conflicts = FALSE)
x=model.matrix(Salary~., data = Hitters)[,-1]
y=Hitters$Salary

grid = 10^seq(10,-2, length=100)
ridge.mod = glmnet(x,y, alpha = 0, lambda = grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
coef(ridge.mod)[,60]


set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)


par(mfrow = c(1,1))
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=213,newx=x[test,])
mean((ridge.pred-y.test)^2)

### for Lasso alpha=1
set.seed(1)
lasso.mod=glmnet(x[train,],y[train], alpha = 1, lambda = grid, thresh = 1e-12)
lasso.pred=predict(lasso.mod,s=4,newx=x[test,])
mean((lasso.pred-y.test)^2)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=16,newx=x[test,])
mean((lasso.pred-y.test)^2)









