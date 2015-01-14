## These functions will make a matrix, cache/store it for later use,
## and then 'solve' or return the inverse of the matrix.

## This function creates the matrix and stores it

makeCacheMatrix <- function(x = matrix()){
	invmat <- NULL
	setnew <- function(y) {
		x <<- y
		invmat <<- NULL
	}
	getx <- function() {x}
	setmat <- function(matrix) {invmat <<- matrix}
	getmat <- function() {invmat}
	list(setnew = setnew, getx = getx, setmat = setmat, getmat = getmat)
}


## This function solves/returns the inverse of the matrix

cacheSolve <- function(x, ...) {
	invmat <- x$getmat()
	if(!is.null(invmat)) { 
		message("getting cached data")
		return(invmat)
	}
	matorig <- x$getx()
	invmat <- solve(matorig, ...)
	x$setmat(invmat)
	invmat
}