## These functions work together to accept a matrix object, and calculate the inverse provided
## the matrix is inversible. The cacheSolve() function ensures efficiency as previously calculated 
## matrices are read from the cache rather than re-calculated.
## ***It is implied that the matrix is square AND invertible***
## A random square invertible matrix can be made using this function : 
## my_matrix <- matrix(rnorm(9), nrow=3, ncol=3)
## This will usually produce an invertible matrix that can be used to test the functions below.

## This func will take a matrix object and make it available through the get() methods in 
## the list object returned. The matrix can be manipulated via the set() methods. Setinverse
## and getinverse assumes that the matrix variable is actually inversed.Both set functions cache
## the data.
 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() works in conjunction with makeCacheMatrix to get the inverse of a matrix.
## A makeCacheMatrix object is passed to cacheSolve which will make a look-up (getinverse) to
## see if the object has been inversed (setinverse) before. If the look-up is True, the cached 
## inverse is returned, else the solve() function is called on the matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
