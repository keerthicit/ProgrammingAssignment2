## Function to cache inverse of the matrix and return inverse from cache is called again

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
        
        c <- NULL
        
        set <- function(d) {

                mat <<- d
                
                c <<- NULL
                
                }
        
        get <- function() mat
        
        setinverse <- function(inverse) c <<- inverse
        
        getinverse <- function() c
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

        }


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- mat$getinverse()
        
        if(!is.null(m)) {
        
                message("getting cached data")
                
                return(m)
        
                }
        
        data <- mat$get()
        
        m <- solve(data, ...)
        
        mat$setinverse(m)
        
        m
        
}


