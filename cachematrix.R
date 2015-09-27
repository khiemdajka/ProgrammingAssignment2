## R Programming Assignment 2
## Matrix with its inverse stored

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set,
		 get = get,
		 setinv = setinv,
		 getinv = getinv)
}

## This function is TL;DR :3
cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if (!is.null(xi)) {
		message("getting cached inverse")
		return(m)
	}
	xm <- x$get()
	m <- solve(xm, ...)
	x$setinv(m)
	m
}
