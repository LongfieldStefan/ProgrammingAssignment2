## Analogue to the example, makeCacheMatrix creates a special list which contains 4 entries.
## These can be used to set and get the matrix as well as getting and setting the inverse 
##      of the matrix in a cache one environment higher than the current one.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes the spicial list created by makeCacheMatrix and 
##      checks whether the inverse has been calculated already or not. 
## If not the inverse is calculated and saved by the setinverse command

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
