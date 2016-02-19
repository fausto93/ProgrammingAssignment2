## Assignment2: Caching the Inverse of a Matrix
## Create a special object that stores matrix and caches its inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                cacheM <- NULL
                
                setmatrix <- function(y){
                        x <<- y
                        cacheM <<- NULL
                }
                
                getmatrix <- function() x
                
                inversecache <- function(inverse) cacheM <<- inverse
                
                inverseget <- function() cacheM
                
                list(setmatrix = setmatrix,
                     getmatrix = getmatrix,
                     inversecache = inversecache, 
                     inverseget = inverseget)
}


## This function will do the inverse of the matrix created before. If it has been 
## already calculated, it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        cacheM <- x$inverseget()
        
        if(!is.null(cacheM)){
                message("getting cached data")
                return(cacheM)
        }
        
        mdata <- x$getmatrix()
        cacheM <- solve(mdata, ...)
        x$inversecache(cacheM)
        
        cacheM
}
