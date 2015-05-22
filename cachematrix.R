## This function creates a function which contains a list of functions: 
## set - initialize inner matrix
## get - get inner matrix
## setInverse - set inverse matrix
## getInverse - get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns a matrix which is the inverse of inner matrix of x
## First, it check whether the inverse has been already computed 
## by getting invers. If so (in this case invers != NULL), 
## it simply returns this precompured value.
## If not, then it computes the inverse by means of solve function, 
## stores received value in the inverse field of x and returns value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invers <- x$getInverse()
    if(!is.null(invers)) {
        message("getting cached data")
        return(invers)
    }
    data <- x$get()
    invers <- solve(data)
    print (invers)
    x$setInverse(invers)
    invers
}
