## Put comments here that give an overall description of what your
## functions do

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
        ## Return a matrix that is the inverse of 'x'
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
