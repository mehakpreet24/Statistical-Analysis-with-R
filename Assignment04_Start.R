# These functions are provided for your convenience ---------------------------

install.packages("utils")
install.packages("quadprog")
library(utils)
library(quadprog)

makeCategoricalSubsets <- function(x){
  library(utils)
  n <-  length(levels(x))
  if(n%%2==0){      # Even number of levels
    storage <-matrix(rep(0, (2^(n-1)-1)*(n/2)), nrow =2^(n-1)-1)
    if(n==2){
      storage <- 1
      dim(storage) <- c(1,1)
    } else{
      i <- 1                  # Will serve as index on rows
      for(j in 1:(n/2-1)){    # Loop over sample size, over columns
        storage[i:(i+choose(n,j)-1),1:j] <- t(combn(1:n, j))
        i <- i+choose(n,j)
      } # Next line takes half of the n/2 sized subsets
      storage[i:(i+choose(n,n/2)/2-1),1:(n/2)] <- t(combn(1:n, n/2))[1:(choose(n,n/2)/2),]
    }
  } else{           # Odd number of levels
    storage <-matrix(rep(0, (2^(n-1)-1)*(n-1)/2), nrow =2^(n-1)-1)
    i <- 1                  # Will serve as index on rows
    for(j in 1:((n-1)/2)){  # Loop over sample size, over columns
      storage[i:(i+choose(n,j)-1),1:j] <- t(combn(1:n, j))
      i <- i+choose(n,j)
    }
  }
  storage
  catSubsets = list(indices = storage, levels = levels(x))
  #print("Completed making subsets")
  return(catSubsets)
}

getCategoricalSubsets <- function(catSubsets, i){
  indices = catSubsets$indices[i,]
  subset <- catSubsets$levels[indices[indices!=0]]
  #print(subset)
  return(subset)
}

supportVectorMachine <- function(X, Y, Cost){
  eps <- .00001
  Xscaled <- scale(X)
  N <- nrow(X)
  p <- ncol(X)
  Xsvm <- cbind(rep(1,nrow(X)),Xscaled)
  # Replace X inner products with a kernel
  K <- matrix(0, N, N)
  for(i in 1:N){
    for(j in 1:N){
      K[i,j] <- exp(-sum((Xsvm[i,]-Xsvm[j,])^2))
    }
  }
  J <- Y %*% t(Y) * K + diag(rep(eps, N))
  d <- rep(1, N)
  At <- rep(0, (2*N+1)*N)
  dim(At) <- c(2*N+1, N)
  At[1,1:N] <- Y
  At[2:(N+1),1:N] <- diag(rep(1,N))
  At[(N+2):(2*N+1),1:N] <- diag(rep(-1,N))
  B <- c(rep(0,N+1), rep(-Cost,N))
  output = solve.QP(Dmat = J, dvec = d, Amat = t(At), bvec = B, meq = 1)$solution
  { # This part will calculate beta from alpha for the linear inner product
    # betaScaled <- colSums(sweep(Xsvm, 1, output*Y, "*"))
    # beta <- rep(0,p+1)
    # beta[1] <- betaScaled[1] - sum(betaScaled[2:(p+1)]*colMeans(X)/apply(X, 2, sd))
    # beta[2:(p+1)] <- betaScaled[2:(p+1)]/apply(X, 2, sd)
    # return(beta)
  }
  return(output)
}

svmPredict <- function(x0, X, Y, alpha){
  Xscaled <- scale(X)
  x0scaled <- standardizeObservation(x0, X)
  Xsvm <- cbind(rep(1, nrow(X)),Xscaled)
  K <- exp(-rowSums(sweep(Xsvm, 2, c(1, x0scaled))^2))
  f <- sum(alpha*Y*K)
  return(f)
}

# Complete this function ---------------------------------------------------

findBestSplit <- function(X, Y){
  bestGoodness <- Inf           
  bestSplitVar <- NA
  bestSplitVal <- NA
  for(i in 1:ncol(X)){                                     # Loop over all variables
    
        if(is.numeric(X[,i])){
          tempx <- sort(X[,i])
          tempy <- Y[order(X[,i])]
          
          for(j in 1:(length(unique(X[,i]))-1)){ 
        tempx <- unique(tempx)
        #tempy <- Y[order(tempx)]
          split <- (tempx[j]+tempx[j+1])/2 
        leftY <- Y[X[,i] < split]
        rightY <- Y[X[,i] > split]# Loop for numeric predictors
        if(is.numeric(Y)){
          goodness <- sum((leftY-mean(leftY))^2)+sum((rightY -mean(rightY))^2)
        }
        print(goodness)
        
        if(is.factor(Y)){
          tempx <- sort(X[,i])
        tempy <- Y[order(X[,i])]
        
        for(j in 1:(length(unique(X[,i]))-1)){ 
          tempx <- unique(tempx)
          split <- Y[order(tempx)== j]
          leftY <- Y[X[,i] %in% split]
          rightY <- Y[X[,i] %in% split]  # Loop for numeric predictors
          if(is.numeric(Y)){
            goodness <- sum((leftY-mean(leftY))^2)+sum((rightY -mean(rightY))^2)
          }
          print(goodness)
          
        }
        
        # Fill in the missing code
        
        if(goodness < bestGoodness){
          bestGoodness <- goodness
          bestSplitVar <- i
          bestSplitVal <- split
        }
      }
    }
    if(is.factor(X[,i])){
      catSubsets <- makeCategoricalSubsets(X[,i])
      for(j in 1:(2^(length(levels(X[,i]))-1)-1)){     # Loop for categorical predictors
        split <- getCategoricalSubsets(catSubsets, j)
        
        # Fill in the missing code
        
        if(goodness < bestGoodness){
          bestGoodness <- goodness
          bestSplitVar <- i
          bestSplitVal <- split
        }
      }
    }
  }
  output <- list(goodness = bestGoodness, splitVar = bestSplitVar, splitVal = bestSplitVal)
}
print(output)

}












X1 <- c(2,8,4,3,6)
X2 <- c("red","green","red","blue","green")
X <- data.frame(X1,X2)
Y <- c(3,10,5,3,6)
Y <- c("spoon","fork","spoon","fork","knife")

# Complete this function ---------------------------------------------------

findBestSplit <- function(X, Y){
  bestGoodness <- Inf           
  bestSplitVar <- NA
  bestSplitVal <- NA
  for(i in 1:ncol(X)){                                     # Loop over all variables
    
    if(is.numeric(X[,i])){
      tempx <- sort(X[,i])
      tempy <- Y[order(X[,i])]
      
      for(j in 1:(length(unique(X[,i]))-1)){ 
        tempx <- unique(tempx)
        split <- (tempx[j]+tempx[j+1])/2 
        leftY <- Y[X[,i] < split]
        rightY <- Y[X[,i] > split]# Loop for numeric predictors
        
        if(is.numeric(Y)){
          goodness <- sum((leftY-mean(leftY))^2)+sum((rightY -mean(rightY))^2)
        }
        print(goodness)
        
        if(is.factor(Y)){
          levels <- unique(Y)
          K <- length(levels)
          priors <- rep(0,length(levels))
          for (i in 1:length(levels)) {
            Ileft <- 1-(levels == levels[leftY[i]])/length(levels)
            
            goodness <- 
              
              # Fill in the missing code
              
              if(goodness < bestGoodness){
                bestGoodness <- goodness
                bestSplitVar <- i
                bestSplitVal <- split
              }
          }
        }
        if(is.factor(X[,i])){
          catSubsets <- makeCategoricalSubsets(X[,i])
          for(j in 1:(2^(length(levels(X[,i]))-1)-1)){     # Loop for categorical predictors
            split <- getCategoricalSubsets(catSubsets, j)
            
            # Fill in the missing code
            
            if(goodness < bestGoodness){
              bestGoodness <- goodness
              bestSplitVar <- i
              bestSplitVal <- split
            }
          }
        }
      }
      output <- list(goodness = bestGoodness, splitVar = bestSplitVar, splitVal = bestSplitVal)
    }
    print(output)
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  X1 <- c(2,8,4,3,6)
  X2 <- c("red","green","red","blue","green")
  X <- data.frame(X1,X2)
  Y <- c(3,10,5,3,6)
  Y <- c("spoon","fork","spoon","fork","knife")
  
  xFactor <- as.factor(c("Fork", "Knife", "Spoon", "Chainsaw"))
  catSubsets <- makeCategoricalSubsets(xFactor)
  for(i in 1:7){
    print(getCategoricalSubsets(catSubsets,i))
}