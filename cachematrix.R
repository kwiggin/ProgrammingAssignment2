## The following functions calculate the inverse of an inputted matrix. 
## When an inverse is calculated for the first time, it is cached, and in 
## any subsequent inverse calculations for the same matrix, the value is
## picked up from the cache.

## The makeCacheMatrix function gets the value of the inputted matrix, sets 
## the value of the inputted matrix, sets the inverse of the inputted
## matrix, and gets the inverse of the inputted matrix.

makeCacheMatrix <- function(x = matrix()) { 	## Input a matrix
        m <- NULL								
        set <- function(y) {				## Set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x				## Get the value of the matrix
        setinverse <- function(solve) m <<- solve	## Set the value of the inverse
        getinverse <- function() m			## Get the value of the inverse
        list(set = set, get = get,			## Create a list of the four components
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix.  If the inverse has already been 
## calculated, it is recognized and picks up the previously calculated value.  If the inverse has
## not been calculated, it calculates the inverse. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()				## Picks up getinverse from the makeCacheMatrix function
        if(!is.null(m)) {				## If the inverse has been calculated, takes the cached value
                message("getting cached data")
                return(m)
        }
        data <- x$get()					## If the inverse hasn't been calculated, calculates the inverse
        m <- solve(data, ...)
        x$setinverse(m)					## Sets the inverse for the next attempted calculation
        m

        ## Return a matrix that is the inverse of 'x'
}
