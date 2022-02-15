# The hashtag is the comment indicator for R.  Lines beginning with this symbol
# are not executed, so they are used for documenting instructions
# or your reasoning.

# Basic arithmetic -------------------------------------------------------------------------------

# If you are new to programming, think of it as a powerful and customizeable
# calculator.  Let's begin with basic arithmetic calculations.
# To execute a line in a script, place your cursor on that line
# and press CTRL+ENTER.  The output will appear in the console at the bottom
# of the screen.  Execute each of the following lines.

1+1
5-2
5*4
20/3
2^3
(5-3)^(3-2)
5 %% 3        # This returns the remainder after division

# Data classes -------------------------------------------------------------------------------

# R has many types or "classes" of data.  Here are some of them.
# Numeric, which is exactly what it sounds like.
# Character, for working with text and labels. Enclose in quotation marks.
# Factor, for working with categorical variables.
# Logical, which is a Boolean or "True/False" classification.

class(3)     # The class() function will return the class of the input.
class("3")   # Quotes makes it a character, not numeric
class(TRUE)  # TRUE and FALSE are keywords for logicals

# Double equals signs is not an assignment, but a test returning a logical.
1+2==3           # This will return TRUE.
1+2==4           # This will return FALSE.
class(1+2==3)    # But either is of the logical type.
class(1+2==4)

3+"3"               # This will throw an error.  One is numeric, one is character.
as.numeric("3")     # This will change the character into a numeric.
3+as.numeric("3")   # So this works without error..
as.character(3)     # You can also turn numerics into characters.

is.numeric(3)       # Here are a set of functions for testing if a value
is.numeric("3")     # is of a certain type or not.  They will return logicals.
is.character(3)
is.character("3")
is.logical(3)
is.logical(1+2==3)

is.logical(2==3)   # Before you run this line, can you predict what it will return?

# Creating objects -------------------------------------------------------------------------------

# Create an object by assigning a value to a variable name.  You can use either
# the = or <- commands.  They are almost exactly the same, but
# have slightly different behavior in advanced situations.  
# Experts recommend getting in the habit of using <-.  
# Press ALT and the underscore symbol _ (to the right of zero) to insert <-.

a <- 3      # Creates a new object with value 3.
a + 5       # Objects can be used in new calculations...
b <- a*2     # ... or used to create new objects.
# Notice that when you made an assignment it didn't show output in the console.
# You can make a line with the name of an object if you want to see it.
b

# After running those lines, look in the top right corner of the screen at the
# Environment pane.  You can see any objects you've created there.

# Vectors -------------------------------------------------------------------------------

# Other data structure can be built out of these basic classes.
# A vector is a set of values of the same class.  Use c() for "c"ombine.
numVec = c(1, 3, 5, 6, 3, 2)  # This is a vector of numbers
numVec[2]                     # Use bracket indexing for retrieval.
numVec+1                      # By default, vector calculations are componentwise
numVec^2
charVec = c("Blue", "Green", "Green", "Blue", "Red", "Yellow")
charVec[3]
charVec == "Blue"            # Returns a vector of logicals
as.numeric(charVec=="Blue")  # Converts logicals into 0's and 1's
as.logical(c(1,0,0))         # Converts a numeric vector to a logical vector

sum(charVec=="Blue")         # A trick for counting occurence of a value
numVec>4
sum(numVec>4)

# Here are a few ways to create structured numeric vectors
1:8            # : makes a vector of all integers between specified start and end
3:15
?seq           # Putting a question mark before a function name shows documentation.
seq(from=2, to=53, by=3)  # Counts by the 'by' argument from start to end
rep(0, length=10)       # Repeats an entry.  Useful for initalization.

# Matrices -------------------------------------------------------------------------------

# Matrices are two-dimensional collections of values.  They are created
# with the matrix() function.  
# The first input is a vector of values.
# The second input is optional, and has a name, nrow, to specify the number of rows.  
# Alternatively, use ncol to specify the number of columns.
# By default, the matrix will be filled in by columns going downward.
# Setting byrow = TRUE will fill it in by row from left to right.
numMatrix <- matrix(numVec, nrow=2, byrow=TRUE)
numMatrix
numMatrix[1,3]      # Access elements by row then by column
numMatrix[,2]       # Leave the first index blank to get the entire column
numMatrix[2,]       # Leave the second index blank to get the entire row
numMatrix[1:2,2:3]  # Indices can also be vectors, returning a submatrix.
numMatrix[,c(1,3)]
numMatrix[,-2]      # Indexing with a negative integer will remove that row/column.

nrow(numMatrix)     # Here are a few ways to get information about a matrix
ncol(numMatrix)
dim(numMatrix)

# You can also index using logicals.  This will be very useful for choosing
# a set of values that meet some condition.
numVec[c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)]  # Returns only the indices with TRUE
numVec>3            # This logical gives a true where the entry is greater than 3
numVec[numVec>3]    # Returns the entries greater than 3

charMatrix <- matrix(charVec, nrow=2, byrow=TRUE)
charMatrix          # A matrix doesn't have to be numeric.  It can contain characters.
# But a matrix can't contain mixed types.

# Factors -------------------------------------------------------------------------------

# A factor is similar to a character, but contains information about
# the number of possible levels
exampleFactor <- factor(charVec)      # Here's one way to create a factor
exampleFactor
as.factor(charVec)                    # This gives equivalent output
class(exampleFactor)

# Data frames -------------------------------------------------------------------------------

# The "data frame" is a versatile data structure
exampleDF = data.frame(numVec,charVec)
# Try clicking on the dataframe's name in the Environment pane in the upper right
summary(exampleDF)      # A bit of information about each variable
exampleDF[3,2]          # Data frame entries can be accessed like a matrix
exampleDF$charVec       # Columns can be extracted entirely with a $
exampleDF$charVec[3]
exampleDF[2,]           # Leave an index blank to get an entire row/column

# Creating functions-----------------------------------------------------------------------------

# Let's see an example of making our own function.  This one will accept
# a number representing the radius of a circle, and will calculate the area
# of that circle.

circleArea <- function(radius){
  area <- pi*radius^2
  return(area)
}

a <- circleArea(3)     # Try changing the input to see how the output changes
a

# Installing and loading packages ----------------------------------------------------------------

# Sometimes you need functionality not included in base R
# For example, generating multivariate normal values
# My Google-Fu tells me to use mvrnorm() in the MASS library
# In RStudio, you can go to Tools->Install Packages...
install.packages('MASS')       #... or use this commmand
# install.packages() needs to be run once on each machine
library(MASS)                         # Now load it.  Needs to be run every session
randomData <-  mvrnorm(500, mu=c(1,1), Sigma=matrix(c(1,.6,.6,1), nrow=2))
head(randomData)                      # Shows only a few rows.  Good for large data.
plot(randomData[,1],randomData[,2])  

# Importing external data -------------------------------------------------------------------------------

# The R interface is terrible for entering large amounts of data
# Instead, use Excel or something similar, and import into R.
# Let's first tell R where to look for any data sets we need to import.
# I suggest creating a folder for materials for this course.
# In the "File" pane of the lower-right window of RStudio, navigate to your folder
# using the "..." button.  Click "More", and "Set as Working Directory".

# Now let's use a command to automate that process in the future.
# This will get the working directory and display it in the console.
getwd()
# Copy and paste the working directory in the command below.
setwd("C:/Users/scarden/Documents/STAT 5660 Data and Scripts")
# Place that command at the beginning of any script that imports data.

# Now let's import from a text file.
# This is a dataset actually built into R, but this will still provide practice importing for later.
carsDF = read.table("Assignment01_cars93.txt")  # Download from Folio into your working directory
carsDF               # Hard to read with a big data set

# Investigating a data set--------------------------------------------------------------------------

head(carsDF)         # Useful for seeing a small portion of the data.  Shows first 6 rows.
colnames(carsDF)     # Useful for seeing variable names
ncol(carsDF)         # There are 27 variables
nrow(carsDF)         # There are 93 observations
summary(carsDF)

carsDF$Manufacturer        # The dollar sign can select a variable by name
table(carsDF$Manufacturer) # A frequency table for categorical variables
carsDF$Horsepower          
mean(carsDF$Horsepower)    # Average
sd(carsDF$Horsepower)      # Standard deviation
range(carsDF$Horsepower)   # Minimum and maximum
sum(carsDF$Horsepower>200) # How many beefy cars?

# Visualizing data -------------------------------------------------------------------------------

# Let's see how to produce basic plots
hist(carsDF$Horsepower)  # The most basic way to produce a histogram
?hist                    # View the documentation  
# Now jazz it up with some custom parameters.
# Notice you can break the command onto multiple lines if it is very long.
hist(carsDF$Horsepower, breaks=10,
     xlab = 'Horsepower', main='Histogram of Horsepower')  

plot(carsDF$Horsepower, carsDF$Price)    # Basic scatterplot
cor(carsDF$Horsepower, carsDF$Price)     # Linear correlation
boxplot(carsDF$Horsepower)               # Basic boxplot
boxplot(carsDF$Horsepower ~ carsDF$Type) # Break down by factor
barplot(carsDF$Type)        # This will throw an error. Find out why.
?barplot                    # It wants numbers. Heights/Frequencies
table(carsDF$Type)          # Get frequencies this way
barplot(table(carsDF$Type)) # Pass that into barplot

x = seq(1,10,.05)    # Make a vector of evenly spaced values
y = x^2              # Componentwise squaring
plot(x,y)            # Scatterplot doesn't look right.  Smooth lines would be better.
?plot                # Notice the 'type' argument can be changed
plot(x,y,type='l')   # This looks better

# For, if/else, and while loops ---------------------------------------------------------------
# A 'for' loop will repeatedly execute a segment of code for a changing index.
for(i in 1:12){  # The index is 'i', and will go from 1 to 12.
  print(i^2)     # This is the code executed for each value of i.
}

# An 'if/else' statement will only run a segment of code if a condition is TRUE
a = 7
if(a > 3){    # In parentheses is the necessary condition
  print('a is bigger than 3')
} else{       # The 'else' portion is optional. Runs only if the condition is FALSE.
  print('a is not bigger than 3')
}
# Go back and change the value of 'a' and see what the if/else statement outputs.

# You can combine 'for' and 'if/else' statements.
# The following loop will count how many even numbers there are from 1 to 12.
countEvens = 0                 # Start the count at zero
for(i in 1:12){
  if(i%%2==0){                 # %% is "remainder". See if remainder is zero
    countEvens=countEvens+1    # If it is, increment countEvens by 1
  }
}
countEvens

# Now we can build more complex functions.
# This function tests to see if a number is prime.
# It's very 'brute force' and not very smart, but will work ok for small numbers.
is.prime = function(x){   # x must be a positive integer
  answer = TRUE           # Assume it is prime, and then seek evidence it is not
  for(i in 2:(x-1)){
    if(x%%i==0){          # See if anything divides it evenly
      answer = FALSE
      return(answer)    
    }
  }
  return(answer)
}
is.prime(9993311)     # Change the input value on this line and test it several times

# A 'while' loop will continue execution while a condition is TRUE, and stops
# when the condition is FALSE.
# Here's a loop that will randomly generate numbers between 1000 and 2000 until
# a prime number is found.
stopFlag = FALSE                  # This variable will serve as the stop condition
while(!stopFlag){                 # The exclamation point negates a TRUE or FALSE
  number = sample(1000:2000,1)    # Generates a random number
  if(is.prime(number)){
    stopFlag = TRUE               # This means the loop will end
  }
}
number                            # See what number it found


# A small list of useful commands and tricks for reference -------------------------------------------
a = c(5,15,20,8)
length(a)
?length                 # Use this to see documentation for function
sum(a)
prod(a)
mean(a)
sd(a)                   # Standard deviation
min(a)
max(a)
median(a)
sort(a)
a[a>10]                 # You can use logicals for indexing
which(a>10)             # Useful for finding indices satisfying condition
which.min(a)
which.max(a)
rank(a)                 # Ranks the observations by value from smallest to largest
order(a)                # Gives order of the original indices for ranking

7 %/% 3                 # integer quotient
7 %%  3                 # remainder; modulo

!(5<3)                  # logical NOT
(5<3) | (9>7)           # logical OR
(5<3) & (9>7)           # logical AND

log(2)                  # logarithm base e
exp(2)                  # exponentiation base e, that is, e^x
sqrt(2)
factorial(2)
choose(4,2)             # binomial coefficients
floor(2.3)              # round down to nearest integer
ceiling(2.3)            # round up to nearest integer
runif(3)                # generate "r"andom "unif"orm numbers
rnorm(3)                # generate random normal numbers

1:5                     # make integer sequence
seq(1,5,.2)             # make sequences with arbitrary intervals
rep(1,5)                # repeat an entry
diag(3)                 # make the identity matrix
diag(numMatrix)         # or extract diagonal entries from a matrix

unique(charVec)         # extract entries, but disallowing duplication