## makeCacheMatrix returns a list of 4 function (set, get, setinv, getinv) which performs the following:
## 1. set: sets the value of a matrix
## 2. get: gets the value of that matrix
## 3. setinv: sets the inverse of the matrix
## 4. getinv: gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function () inv
	list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns a matrix that is the inverse of argument "x". 
## If the inverse is previously cached, it returns string "getting cached data" and then the inverse matrix
## If the inverse is not previously cached, it returns just the inverse and caches the output.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}