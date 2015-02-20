
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.



### The makeCacheMatrix function creates a special list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    
    x <<- y
    inv <<- y
    
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


# The cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. 
# If so, it gets the cached result and skips the computation.
# If not, it computes the inverse, sets the value in the cache via the setinverse function and returns it


cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
    
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}

## Sample Output

# Input the matrix
#x = matrix(c(5,2,3,4), nrow = 2, ncol = 2)

## Call the makeCacheMatrix function
#m = makeCacheMatrix(x)

## Display the matrix
#m$get()

#     [,1] [,2]
#[1,]    5    3
#[2,]    2    4

## Compute the inverse of the matrix and returns it (No cache involved)
#cacheSolve(m)

#           [,1]       [,2]
#[1,]  0.2857143 -0.2142857
#[2,] -0.1428571  0.3571429

## Compute inverse of same matrix, returns the cached inverse already computed 
#cacheSolve(m)

# Getting cached data
#           [,1]       [,2]
#[1,]  0.2857143 -0.2142857
#[2,] -0.1428571  0.3571429
