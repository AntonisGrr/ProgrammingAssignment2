## Put comments here that give an overall description of what your
## functions do.

## Write a short comment describing this function



# makeCacheMatrix creates a list containing fuctions that:
# 1. Set the value of the matrix.
# 2. Get the value of the matrix.
# 3. Set the value of inverse of the matrix.
# 4. Get the value of inverse of the matrix.

# cacheSolve
# 1. Checks if the inverse is computed already.
# 2. Returns the inverse of the matrix along with a message "Getting cached data."
# 3. Else, calculates the inverse.
# 4. Chaches the value of the iverse matrix using the setinverse function
# 5. Returns the inverted matrix.

# These functions assume that the matrix is always invertible.

## _____________________________________________________________ ##

## Function that takes an invertible matrix as an arguement
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## Sub-function that caches the matrix in 'y'
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Sub-function that returns 'x' (our matirx)
  get <- function() x
  ## Sub-function that assigns to 'I' the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  ## Sub-function that returns the inverse matrix 'I'
  getinverse <- function() inv
  ## List of the functions in 'makeCacheMatrix'
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Function that takes an invertible matrix as an arguement
cacheSolve <- function(x, ...) {
  ## Calls 'getinverse' on 'x' and assigns the value to 'inv' (that's
  ##  a NULL or the inverted matrix if already calculated) 
  inv <- x$getinverse()
  ## If I is not null, it has to be the inverted matrix that has been calculated and chached
  if(!is.null(inv)) {
    message("getting cached data.")
    ## In that case the cacheSolve returns 'inv' along with the message, and terminates
    return(inv)
  }
  ## If 'inv' is NULL, the cached matrix is taken from x$get
  data <- x$get()
  ## It's inverse is calculated
  inv <- solve(data)
  ## x$setinverse is called to cache the 'solved' value to 'inv'
  x$setinverse(inv)
  ## The fucntion returns I, which is the inverse of x
  inv
}

## Sample run:
## > goat = matrix(c(1,2,2,2,1,2,2,2,1),ncol=3)
# > g = makeCacheMatrix(goat)
# > g$get()
#       [,1] [,2] [,3]
# [1,]    1    2    2
# [2,]    2    1    2
# [3,]    2    2    1

# > cacheSolve(g)   ## NOT 'goat', cacheSOlve 'g' ##c
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667


# cacheSolve(g)
#       [,1] [,2] [,3]
# [1,] -0.6  0.4  0.4
# [2,]  0.4 -0.6  0.4
# [3,]  0.4  0.4 -0.6



#cacheSolve(g)   (second or further time)
#getting cached data.
#      [,1] [,2] [,3]
#[1,] -0.6  0.4  0.4
#[2,]  0.4 -0.6  0.4
#[3,]  0.4  0.4 -0.6
