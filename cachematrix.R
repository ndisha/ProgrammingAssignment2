## This is a description to two functions, whose details are broken down into two, below.


## functions: makeCacheMatrix; this function encapsulates a couple of functions that are then called by the second function
makeCacheMatrix <- function(x = matrix()) {
	
	## we receive our matrix, set the value of m (the soon to be used cached value, to nothing
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## here, we initialize all the functions we will need to us
	## we also cache the value given to m, by getinverse(), using the function setinverse()
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## function: cacheSolve ; this function checks if the matrix inverse has been calculated,
## if yes, it returns a cached value, if no, it solves the matrix inverse and caches it accordingly

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		## the cacheInverse function checks first is there is any value in m, which is the variable assigned to the calculated and cached inverse
		## we use the getinverse() function to pick the "cached" value, that is if its been cached
		cacheInverse <- function(x, ...) {
		
		m <- x$getinverse()
		
		## if m is null, then this is the first time we are doing the calculations, so we get to work, calculating the inverse
		## if m is not null, we prompt the user that we are showing a cached value, and we show the value of m
			if(!is.null(m)) {
				message("getting cached inverse for the matrix")
				return(m)
			}
		
		## in this case, m was null, so we are getting the matrix and calculating its inverse. 
		## we then set the value of m, by calling the setinverse() function which actually caches the value using the <<- sign
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m
		}	
		
}
