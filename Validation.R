require(ISLR)
require(boot)
plot(mpg~horsepower, data=Auto)

## LOOCV
glm.fit= glm(mpg~horsepower, data=Auto)
cv.glm(Auto, glm.fit)$delta

##Lets write a simple function to use formula
loocv = function(fit){
  h = lm.influence(fit)$h
  mean(residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)

cv.error = rep(0,5)