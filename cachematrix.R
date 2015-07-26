## Creates two functions, one takes a matrix and and stores it and gets it
## the other one will check to see if the cache has the value yet, if not
## then it will compute the inverse with the solve() and output it.



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
