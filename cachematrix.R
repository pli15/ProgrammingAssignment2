## The purpose of this code is to caculate the inverse of a matrix.
## Because calculating the inverse of a matrix is a repeatedly complicated 
## procedure, to simplify the computation, to cache the inverse in a function 
## and retrieve later will increase the efficiency.

## The first step is to create a matrix that stores the inverse value.

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
			x <<- y
			i <<-NULL
	}
		get <- function() x
		setinverse <- function(inverse) i <<-inverse
		getinverse <- function() i
		list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## The second step is to retrieve the inverse from step 1
## by checking if the inverse is calculated already. If yes, the inverse will
## be retrieved from the cache directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  i <- x$getinverse()
	  if(!is.null(i)){
			message("getting cached data")
			return(i)
	  	}
	  data <- x$get()
	  i <- solve(data,...)
	  x$setinverse(i)
	  i
}
