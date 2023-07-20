## Lianeris Estremera-Rodriguez; 7/20/2023

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCachematrix creates a matrix function where values 
## x and 'inv' were set. x (first object) was assigned to 
## be numeric. 'inv' is the second object set in NULL and
## it will be used the second part of the code, since 
## this is the object part of the code.
## After, the 'get' command retrieves the function from the 
## environment previously created.'Setinverse' sets the
## inverse of the matrix we want in a function called inverse,
## and attributed to 'inv'. The 'getinverse' is the retriever 
## command in the function to get the inverse matrix. Lastly,
## the list command transforms the functions in elements in order 
## to retrieve each one as attribute from makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) 
    inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function.

## cacheSolve is the function that uses data and completes 
## the commands previously stated on makeCacheMatrix. It
## starts by defining 'inv' as a caller of the getinverse. 
## Then the if statement verifies if the result is null (equal  
## or not equal to Null), and if the matrix value is not equal 
## to null the code will print a message (getting cached data) 
## and the value stored in 'inv'.
## After the if statement, I assigned to data the get function 
## for x, and assigned a solve command to 'inv' using data 
## previously defined in the environment. Lastly 'inv' alone
##  should return the expected value, inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#Confirmation of working functions using testing cases
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1

myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)
