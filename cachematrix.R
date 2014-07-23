## A function that calculates the inverse of a matrix or returns the
## cahced inverse matrix from a closure that stores it.


## A closure that defines the get and set properties for the cached matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function(){
		x
	}
	setSolve <- function(solve){
		m <<- solve
	}
	getSolve <- function(){
		m
	}
	
	list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## a function to calculate the inverse of a matrix or return the
## cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getSolve()
		
		if( !is.null(m) ){
			message("getting cached data")
			return(m)
		}

		data <- x$get()
		m <- solve(data)
		x$setSolve(m)
		m
}
