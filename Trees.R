
my.install <- function(pkg, ...){
  if (!(pkg %in% installed.packages()[,1])) {
    install.packages(pkg)
  }
  return (library(pkg, ...))
}

my.install("ISLR", character.only=T, warn.conflicts=F)
my.install("tree", character.only=T, warn.conflicts=F)

### Fitting a classification tree
attach(Carseats)
High = ifelse(Sales<=8, "No", "Yes")

Carseats = data.frame(Carseats, High)

tree.carseats = tree(High~.-Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats)

set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]

tree.carseats = tree(High~.-Sales, Carseats, subset = train)
tree.pred = predict(tree.carseats, Carseats.test, type="class")

table(tree.pred, High.test)

set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)

cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)

### Fitting a regression tree
my.install("MASS", character.only=T, warn.conflicts=F)
attach(Boston)
set.seed(1)
train = sample (1: nrow(Boston ), nrow(Boston )/2)
tree.boston = tree(medv~., Boston, subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")
prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston )
text(prune.boston ,pretty =0)

yhat=predict (tree.boston ,newdata =Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat ,boston.test)
abline (0,1)
mean((yhat -boston.test)^2)


