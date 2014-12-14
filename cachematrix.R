## Put comments here that give an overall description of what your
## functions do

## returns a function that can store a matrix and the invertse of tha matrix
# set and get operate on the matrix
# setinverse and getinverse operte on the inverse
# setting the matrix will invalidate previous calculated inv.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inv <<- i
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# returns a cached inv of a makeCacheMatrix if there is one
# if there isn't, calculates the inv, stores it with setinverse and returns inv
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setinverse(inv)
        inv
}
