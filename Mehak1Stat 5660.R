#PROBLEM 1---------------------------------------------------------------

print_factors <- function(x) {
  countEvents=0
  print(paste("The factors of",x,"are:"))
  for(i in 1:x) {
    if((x %% i) == 0) {
      countEvents=countEvents+1
      print(i)
    }
    else{}
  }
  
  print(paste("The number of factors of",x,"are:"))
  return(countEvents)
  }
print_factors(120)
    countEvents
    
    
#PROBLEM 2-----------------------------------------------------------------

harmonicMean = function(x){
  a = 1/x
  b = length(x)/sum(a)
  print(b)
  return(b)
}


#PROBLEM 3------------------------------------------------------------------

standardize = function(matrix){
  
  for (i in 1:ncol(matrix)){
    matrix[,i] = (matrix[,i] - mean(matrix[,i]))/sd(matrix[,i])
  }
  print(matrix)
  return(matrix)
  
}
    
matrix=matrix(c(1,2,3,4,5,6,7,8,9),byrow=TRUE,nrow=3)
myfunc = standardize(matrix)
 builtinfunc = scale(matrix)
 
 myfunc == builtinfunc

 
#PROBLEM 4------------------------------------------------------------------
 
kNearestNeighbors <- function(x,vec,k){
 a <- abs(x-vec)
 b <- order(a)
 c <- vec[b[1:k]]
 return(c)
}
 vec <- c(6,3,8,5,6)
answer = kNearestNeighbors(7,vec,3)
answer

#PROBLEM 5------------------------------------------------------------------

bisectSearch = function(y,tol){
  min = 0
  max = y
  while(((min+max)/2)^2-y > tol){
    if((((min+max)/2)^2) < y ){
      min = (min+max)/2
    }else{
      max = (min+max)/2
      
    }
  }
  return((min+max)/2)
}
answer = bisectSearch(16,.01)
answer


#PROBLEM 6--------------------------------------------

processData = function(x) {
  newmatrix = matrix(,byrow=TRUE,ncol=2)
  i = 1
  n = nrow(x)
  while (i<=n){
    if ((x[i,2] <= x[i,1])){
      x = x[-i,]
      n = n-1
    }
    else{
      i = i+1
    }
  }
  c <- x[order(x[,2]),]
  d <- c[order(c[,1]),]
  print(d)
  }
 x = matrix(c(4,3,3,5,2,4,5,5,3,4),byrow=TRUE, ncol=2)
 processData(x)

 
 
