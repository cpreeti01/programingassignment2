##Function creates a matix object which caches the inverse of given function

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) xinv <<- inverse
    getinverse <- function() xinv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##Function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated, then the 
##cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    xinv <- x$getinverse()
    if(!is.null(xinv)) {
        message("getting cached data.")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data)
    x$setinverse(xinv)
    xinv
}
