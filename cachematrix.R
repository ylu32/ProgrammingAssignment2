## programming assignment 2

## a matrix object that can cache its inverse


mmakeCacheMatrix <- function(x = numeric()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) v <<- solve
        getinverse <- function() v
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## computes the inverse of a matrix returned by makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        v <- x$getinverse()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinverse(v)
        v
}
