## This is a set of functions that allows caching of a computed inverse.

## Creates a special kind of matrix wrapper containg a value for its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Provides the inverse of CacheMatrix x from its internal cache if available
## or computes and caches it in x otherwise.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
