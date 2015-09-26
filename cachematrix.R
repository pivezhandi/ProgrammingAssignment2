## Put comments here that give an overall description of what your
## functions do

## this function create a special with cached inverse capability

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(ginv) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## inverse of matrix created in makeCacheMatrix comes from here
## this function Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
		}
