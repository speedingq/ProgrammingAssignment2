## Cache the inverse of a matrix

## Cache a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(matrix) m <<- matrix
	getmatrix <- function() m
	list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## If the inverse has already been calculated, then return from the inverse from the cache
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(x, ...)
	x$setmatrix(m)
	m
}
