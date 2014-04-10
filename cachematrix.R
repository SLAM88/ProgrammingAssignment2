
## These functions calcluate the inverse of a matrix, then cache its value.
## This means that when the inverse is calculated later on,
## the system can look up the cache value and return that 
## instead of redoing a calculation it has already done.

## This function takes an NxN matrix as input and generates the matrix inverse. 
## The invest is then cached  using a list implementation.
 
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL                                   ## Variables initialized
        set <- function(y) {
            x <<- y
            s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve  ## Solve calculates inverse
        getinverse <- function() s
        list(set = set, get = get,                 ## Cache list populated
             setinverse = setinverse,
             getinverse = getinverse)
    
}


## This function returns the cached inverse value if it exists.
## If it doesn't exist, it calls the earlier function to calculate the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()                        ## Checks if cache exists
        if(!is.null(s)) {                          ## If yes, returns cache
            message("getting cached data")
            return(s)
        }
        data <- x$get()                            ## If no, calculates inverse
        s <- solve(data, ...)
        x$setinverse(s)
        s
    }

## End functions.
## Below are simple test scripts used to check that everything works.

## Generate valid test matrix
test <- matrix(c(1,8,3, 11,25,13, 3,6,2), nrow = 3, ncol = 3)
test

## Test each function once to ensure no errors
test1 <- cacheSolve(makeCacheMatrix(test))
test1

## Test cacheSolve's ability to return cached inverse
test2 <- makeCacheMatrix(test)
cacheSolve(test2)
cacheSolve(test2)

## Validate inverse calculation is correct by checking whether two calls
## of each function returns the original matrix
test3 <- cacheSolve(makeCacheMatrix(cacheSolve(makeCacheMatrix(test))))
test3