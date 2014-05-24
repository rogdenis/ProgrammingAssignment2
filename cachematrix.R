#There are two functions.
#makeCacheMatrix creates an object with data,
#cacheSolve calculates inversed matrix by data and prints it 

#You can try it:
#source("[your working directory here]/cachematrix.R")
#matr <- makeCacheMatrix()
#cacheSolve(matr)
#cacheSolve(matr)

#Function takes matrix as an input, but also it has default huge matrix
#to see the difference in calculation time of inversed matrix

makeCacheMatrix <- function(x = matrix(rnorm(1024),32,32)) {
  
  inverse <- NULL # set null to inverse first of all
  set <- function(y) {
    x <<- y ##set the value here
    inverse <<- NULL
  }
  get <- function() x #function that lets to read the data
  setInverse <- function(inversedMatrix) inverse <<- inversedMatrix #just set the value of inversed matrix
  getInverse <- function() inverse #read value of inversed matrix for cache function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  #above is what function return
}


## This function reads the value of inversed matrix of the object given
# if given, calculates inversed matrix one more time

cacheSolve <- function(x, ...) {
  m <- x$getInverse() #read the inversed data here
  if(!is.null(m)) { #if it exists return it
    message("getting cached data")
    return(m)
  }
  data <- x$get() #otherwise read the data of the first matrix
  m <- solve(data, ...) #make inverse matrix
  x$setInverse(m) #set the value to the object
  m #and voila! prints it
}

#here you can print the initial matrix
printMatrix <- function(x, ...) x$get()

