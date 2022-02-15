#Problem 4--------------------------------------------------------
standardizeObservation = function(x0,x){
  
  for (i in 1:ncol(x)){
    x0[i] = (x0[i] - mean(x[,i]))/sd(x[,i])
  }
  return(x0)
  
}

x =matrix(c(1,2,3,4,5,6,7,8,9),byrow=TRUE,nrow=3)
x
x0 = c(1,2,3)
dim(x0) <- c(1,3)
myfunc = standardizeObservation(x0,x)
myfunc

#PROBLEM 5--------------------------------------------------------
eucliddistance = function(x0,x){
  #print(x0)
  #print(x)
  sqrt(rowSums(sweep(x,2,x0)^2))
}
a <- c(1,2) 
b <- c(3,1,2,2)
dim(b) <- c(2,2)
eucliddistance(a,b)


#PROBLEM 6---------------------------------------------------------
kNearestIndices = function(x0,x,k){
 
 y = standardizeObservation(x0,x)
 x = scale(x)
 
 z = eucliddistance(y,x)
 b <- order(z)
 c <- b[1:k]
 return(c)
}
kNearestIndices(x0,x,1)



#PROBLEN 7---------------------------------------------------------

#Part(a)
kNNReg = function(x0,x,k,Y){
y <- kNearestIndices(x0,x,k)
b <- mean(Y[y])
return(b)
}
Y <- matrix(c(1,2,3),ncol=1)
kNNReg(x0,x,3,Y)

#Part(b)
knnClass = function(x0,x,k,Y){
  y <- kNearestIndices(x0,x,k)
  a <- table(Y[y])
  a
  b <- sort(a, decreasing = TRUE)
  b
  c <- names(b[1])
return(c)
}
Y <- as.factor(c("spoon","fork","spoon"))
knnClass(x0,x,3,Y)



#PROBLEM 8------------------------------------------------------------------------

#Part(a)
matrix = as.matrix(cbind(iris$Petal.Length,iris$Petal.Width))
x0 <- c(5.5,1.2)
Y <- as.matrix(iris$Species)
knnClass(x0,matrix,5,Y)

#Part(b)
matrixA = as.matrix(cbind(iris$Sepal.Length,iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width))
x1 <- c(5.2,3.7,1.4,0.6)
Y <- as.matrix(iris$Species)
knnClass(x1,matrixA,5,Y)

#Part(c)
library(MASS)
matrixB = as.matrix(Cars93$Horsepower)
x2 <- c(125)
Y <- as.matrix(Cars93$Price)
kNNReg(x2,matrixB,5,Y)

#Part(d)
plot(Cars93$Horsepower,Cars93$Price)
xAxis <- seq(50,300,0.5)
pred <- rep(0, length(xAxis))
k = 5
for (i in 1:length(xAxis)){
  pred[i] = kNNReg(xAxis[i],matrixB,k,Y)
}
points(xAxis,pred, type = 'l', col = 'blue')


#Part(e)
matrixs = as.matrix(cbind(Cars93$Horsepower,Cars93$Weight,Cars93$Width))
x3 <- c(125,2500,69)
Y <- as.matrix(Cars93$Price)
kNNReg(x3,matrixs,5,Y)



#LINEAR REGRESSION
#Problem 9------------------------------------------------------------------------

#Part(a)

X = as.matrix(cbind(rep(1,length(Cars93$Horsepower)),Cars93$Horsepower))
Y <- as.matrix(Cars93$Price)
Beta <- solve(t(X)%*%X,t(X)%*%Y)
print(Beta)

#Part(b)
plot(Cars93$Horsepower,Cars93$Price)
abline(Beta[1],Beta[2])

#Part(c)
x0 <- c(1,125)
dim(x0) <- c(1,2)
Y <- x0%*%Beta
Y

#Problem 10-------------------------------------------------------------------------
#Part(a)
X = cbind(rep(1,length(Cars93$Horsepower)),
          Cars93$Horsepower,
          Cars93$Weight,
          Cars93$Width,
          as.numeric(Cars93$AirBags=="Driver only"),
          as.numeric(Cars93$AirBags== "Driver & Passenger"),
          as.numeric(Cars93$Origin == "non-USA"))
X

#Part(b)
Y <- as.matrix(Cars93$Price)
Beta <- solve(t(X)%*%X,t(X)%*%Y)
print(Beta)

#Part(c)
X0 <- c(1,140,3300,70,1,0,0)
dim(X0) <- c(1,length(X0))
Y <- X0%*%Beta
Y

#Problem 11---------------------------------------------------------------------------
logistic = function(x){
  y <- 1/(1+exp(-x))
  return(y)
}
logistic(0)

#Problem 12----------------------------------------------------------------------------
negLogLikelihood = function(beta,X,Y){
  output <- -sum(log(logistic(X%*%beta))*Y)-sum(log(1-logistic(X%*%beta))*(1-Y))
}

logisticReg <- function(X, Y){ 
  betaInit <- matrix(rep(0,ncol(X)), nrow = ncol(X))  
  beta <- optim(betaInit, negLogLikelihood, X=X, Y=Y)$par  
  return(beta)
}
  

#Problem 13----------------------------------------------------------------------------
#Part(a)
X <- as.matrix(cbind(rep(1,length(iris$Petal.Length)),iris$Petal.Length))
Y <- as.numeric(iris$Species=="virginica")

#Part(b)
beta = logisticReg(X,Y)
beta

#Part(c)
x0 <- c(1,4.5)
y <- logistic((x0)%*%beta)
y

#Part(d)
x1 <- c(1,5)
y1 <- logistic((x1)%*%beta)
y1


#Problem 14------------------------------------------------------------------------------

oneVersusAllLogistic = function(x0,X,Y){
  prob <- rep(0,length(unique(Y)))
  for (i in 1:length(prob)) {
    Ytemp <- as.numeric(Y==unique(Y)[i])
    beta <- logisticReg(X,Ytemp)
    print(beta)
    y1 <- logistic((x0)%*%beta)
    prob[i] <- y1
  }
  print(prob)
  unique(Y)[which.max(prob)]
 }

#Problem 15--------------------------------------------------------------------------------

x0 <- c(1,5.2,3.7,1.4,0.6)
X <- as.matrix(cbind(rep(1,length(iris$Petal.Length)),iris$Sepal.Length,iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width))
Y <- as.matrix(iris$Species)
a <- oneVersusAllLogistic(x0,X,Y)
a


