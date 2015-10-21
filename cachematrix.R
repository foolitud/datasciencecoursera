## It may make sense to cache the value of the inverse of a matrix so that when we need it again,
## it can be looked up in the cache rather than recomputed

## makeCacheMatrix creates a list of functions and an environment to store a matrix and its inverse

## Usage: x <- matrix(1:1000000, 2)
## Usage: z <- makeCacheMatrix(x)

## Arguments: x - a complex matrix

## Returns: list of functions set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
        invcache <- NULL
        set <- function(y) {
                x <<- y
                invcache <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invcache <<- solve
        getinverse <- function() invcache
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Usage: cacheSolve(z,...)

## The cacheSolve function inverses the matrix or fetches it from memory if available

## The following function calculates the inverse of the special "matrix" created with the above function.

cacheSolve <- function(z, verbose=FALSE,...) { ## Return a matrix that is the inverse of 'x'
        invcache <- z$getinverse()
        if(!is.null(invcache)) {
                message("getting cached data")
                return(invcache)
        }
        data <- z$get()
        inverse <- solve(data, ...)
        z$setinverse(inverse)
        inverse
}

