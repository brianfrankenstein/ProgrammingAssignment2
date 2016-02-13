## This collection of functions gives us the ability to make more
## efficient calls to retrieve an inverted matrix by caching the inverse

## creates a special matrix that caches its inverse
makeCacheMatrix <- function(m = matrix()) {
    cachedInverse <- NULL
    
    set <- function(newmatrix)
    {
        m <<- newmatrix
        cachedInverse <<- NULL
    }
    setinverse <- function(mi)
    {
        cachedInverse <<- mi
    }
    getinverse <- function()
    {
        cachedInverse
    }
    
    list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## Invert the matrix m, where m is a special matrix  created by 
## the above makeCacheMatrix. The data in m is assumed to be invertable

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedInverse <- m$getInverse()
    
    if(!is.null(m))
    {
        message("getting cached inverse")
        return(cachedInverse)
    }
    matrixData <- m$get()
    
    cachedInverse <- solve(matrixData)
    
    m$setinverse(cachedInverse)
    cachedInverse
}
