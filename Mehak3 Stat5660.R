#Problem 2------------------------------------------------------------------------
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

eucliddistance = function(x0,x){
  sqrt(rowSums(sweep(x,2,x0)^2))
}
a <- c(1,2) 
b <- c(3,1,2,2)
dim(b) <- c(2,2)
eucliddistance(a,b)

library(MASS)
gaussianKernel = function(x,h){
  ker = (1/h)*((1/sqrt(2*pi))*exp(-(x/h)^2/2))
  return(ker)
}
x<-0
h<-1
gaussianKernel(x,h)


#Problem 3------------------------------------------------------------------------------
kernelReg = function(x0,x,Y,h){
  
  u = standardizeObservation(x0,x)
  x = scale(x)
  
  z = eucliddistance(u,x)
  w <- gaussianKernel(z,h) 
  ker <- sum(w*Y)/(sum(w))
  return(ker)
}


#Problem 4-------------------------------------------------------------------------------
matrixA = as.matrix(cbind(Cars93$Horsepower,Cars93$Weight,Cars93$Width))
x1 <- c(140,3300,70)
Y <- as.matrix(Cars93$Price)
kernelReg(x1,matrixA,Y,0.2)



#Problem 5--------------------------------------------------------------------------------
localLinearReg = function(x0,x,Y,h){
  
  u = standardizeObservation(x0,x)
  Xscaled = scale(x)
  
  z = eucliddistance(u,Xscaled)
  w <- gaussianKernel(z,h) 
  #ker <- sum(w*Y)/(sum(w))
  #return(ker)
  W <- diag(w)
  x2 <- as.matrix(cbind(rep(1,nrow(x)),x))
  Beta <- solve(t(x2)%*%W%*%x2,t(x2)%*%W%*%Y)
  print(Beta)
  x0 <- c(1,x0)
  Y <- x0%*%Beta
  return(Y)
}
  
#Problem 6-----------------------------------------------------------------------------
x = as.matrix(cbind(Cars93$Horsepower,Cars93$Weight,Cars93$Width))
x0 <- c(140,3300,70)
dim(x0) = c(1,ncol(x))
Y <- as.matrix(Cars93$Price)
localLinearReg(x0,x,Y,0.2)


#Problem 7-----------------------------------------------------------------------------
getLDAparameters = function(X,Y){
  levels <- unique(Y)
  K <- length(levels)
  p <- ncol(X)
  N <- nrow(X)
  priors <- rep(0,K)
  meanMatrix <- matrix(0,K,p)
  for (i in 1:K) {
   priors[i] <- sum(Y == levels[i])/length(Y)
   meanMatrix[i,] <- colMeans(X[which(Y==levels[i]),])
  }
  xcenter <- matrix(0,nrow(X),p)
  for (i in 1:K) {
    xcenter[which(Y==levels[i]),] <- as.matrix(sweep(X[which(Y==levels[i]),],2,meanMatrix[i,]))         
       }
  sigma = (1/(N-K))*t(xcenter)%*%xcenter
  print(sigma)
  model <- list(meanMatrix = meanMatrix, sigma = sigma, levels = levels, priors = priors)
  return(model)
}



#Problem 8------------------------------------------------------------
LDA = function(x0,model){
mean <- model$meanMatrix
sigma <- model$sigma
levels <- model$levels
K <- length(levels)
priors <- model$priors
lin.disc <- rep(0,K)
for (i in 1:K) {
  tempmean <- mean[i,]
  dim(tempmean) <- dim(x0)
  lin.disc[i] <- x0 %*% solve(sigma) %*% t(tempmean) - (1/2)*tempmean%*%solve(sigma)%*%t(tempmean) + log(priors[i])
}
print(lin.disc)
pred <- levels[which.max(lin.disc)]
}

#Problem 9----------------------------------------------------------------------
X <- as.matrix(cbind(iris$Sepal.Length,iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width))
Y <- iris$Species
model <- getLDAparameters(X,Y)
x0 <- c(5.2,3.7,1.4,0.6)
dim(x0) <- c(1,4)
pred <- LDA(x0,model)
pred


#Problem 10----------------------------------------------------------------------------------
getQDAparameters = function(X,Y){
  levels <- unique(Y)
  K <- length(levels)
  p <- ncol(X)
  N <- nrow(X)
  priors <- rep(0,K)
  meanMatrix <- matrix(0,K,p)
  for (i in 1:K) {
    priors[i] <- sum(Y == levels[i])/length(Y)
    meanMatrix[i,] <- colMeans(X[which(Y==levels[i]),])
  }
  xcenter <- matrix(0,nrow(X),p)
  for (i in 1:K) {
    xcenter[which(Y==levels[i]),] <- as.matrix(sweep(X[which(Y==levels[i]),],2,meanMatrix[i,]))         
  }
  sigmaArray <- array(rep(0,K*ncol(X)^2),dim =c(ncol(X),ncol(X),K))
  
  for (j in 1:K) {
   xtemp <-  xcenter[which(Y==levels[j]),]
    sigmaArray[,,j] = (1/(sum(Y==levels[j])-1))*t(xtemp)%*%xtemp
  }
  
  print(sigmaArray)
  model <- list(meanMatrix = meanMatrix, sigmaArray = sigmaArray, levels = levels, priors = priors)
  return(model)
}



#Problem 11------------------------------------------------------------
QDA = function(x0,model){
  mean <- model$meanMatrix
  sigmaArray <- model$sigmaArray
  levels <- model$levels
  K <- length(levels)
  priors <- model$priors
  quad.disc <- rep(0,K)
  for (i in 1:K) {
    quad.disc[i] <- -(1/2)*log(det(sigmaArray[,,i]))-(1/2)*t(x0-mean[i,]) %*% solve(sigmaArray[,,i]) %*% (x0-mean[i,]) +log(priors[i])
  }
  print(quad.disc)
  pred <- levels[which.max(quad.disc)]
}

#Problem 12----------------------------------------------------------------------
X <- as.matrix(cbind(iris$Sepal.Length,iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width))
Y <- iris$Species
model <- getQDAparameters(X,Y)
x0 <- c(5.2,3.7,1.4,0.6)
pred <- QDA(x0,model)
pred
