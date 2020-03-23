# The code includes two functions: 'makeCacheMatrix' and 'cacheSolve'.
# 'makeCacheMatrix' computes a matrix that saves its inverse to cache
# 'cacheSolve' computes the inverse of the special "matrix" returned by 'makeCacheMatrix'
# If the inverse has already been calculated, then function retrieves the inverse from the cache.


# 'makeCacheMatrix' function creates a special "matrix" object 
# that returns a list of functions to 1) set matrix, 2) get matrix,
# 3) set inverse matrix, 4) get inverse matrix. Returns input for 'cachesolve'

makeCacheMatrix <- function(x = matrix()) {
    m <<- NULL
    
    setMatrix <- function(y){
        x <<- y
        m <<- NULL
    }
    
    getMatrix <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    list(setMatrix = setMatrix, getMatrix=getMatrix, 
         setInverse= setInverse,
         getInverse = getInverse)
}


## 'cacheSolve' function computes the inverse of matrix returned
## by 'makeCacheMatrix' function. The functiont tests if the inverse 
## has already been calculated (and the matrix has not changed). 
## In that case, the function retrieves the inverse from cache

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    
    ## Tests if the inverse has already been computed 
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    ## If not, the inverse is computed here
    else {
        data <- x$getMatrix()    
        m <- solve(data, ...)
        #x$setInverse(m)
        return(m)
    }
    
}