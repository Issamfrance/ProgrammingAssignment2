## makeCacheMatrix creates a list containing a function to
# set a value of the matrix
# get a value of the matrix
# set an inverse value of the matrix
# get an inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
       inv <<- NULL
        setvalmat <- function (y) {
               X <<- y
               inv <<- NULL
       }
        getvalmat <- function() x
        setinvmat <-  function(inverse) inv <<- inverse 
        getinvmat <-  function() inv 
        list( setvalmat = setvalmat, getvalmat = getvalmat, setinvmat = setinvmat, getinvmat = getinvmat)
}


## The following function returns the inverse of the matrix.
## The function will start by checking if the inverse value has been computed. 
## If so, it gets the result and shows the message "Getting cached data!"
## If not, it computes the inverse, and sets the value in cache via setinvmat function.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinvmat()
        if (!is.null(inv)) {
                message ("Getting cached data!")
                return (inv)
                
        }
        inverse <- x$getvalmat()
        inv <- solve(inverse)
        x$setinvmat(inv)     
        inv
}

## Function test
## > x = rbind (c(1, 2, 3), c(0, 1, 5), c(5, 6, 0))
## > x
##       [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    5
## [3,]    5    6    0
## > m <- makeCacheMatrix(x)
## > m$getvalmat()
##       [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    5
## [3,]    5    6    0


## This is the 1st run, so no cache.
## > cacheSolve(m)
##       [,1] [,2] [,3]
## [1,]   -6  3.6  1.4
## [2,]    5 -3.0 -1.0
## [3,]   -1  0.8  0.2

## If a 2nd run is done
## > cacheSolve(m)
## Getting cached data!
##         [,1] [,2] [,3]
##  [1,]   -6  3.6  1.4
##  [2,]    5 -3.0 -1.0
##  [3,]   -1  0.8  0.2














