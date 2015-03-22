## NOTE: I cannot create a Github repository on my local machine,
## because Github for MAC OS X is only compatible for 10.9 or higher
## versions. I have 10.8.5.
## Therefore, I am editing the repository online.

## "makeCacheMatrix" creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
	##Following the example given at the beginning
	##of the assignment description (by R.D. Peng).
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get,
		 setinverse=setinverse,
		 getinverse=getinverse)
}


## "cacheSolve" funtion computes the inverse of the matrix returned by "makeCacheMatrix" above
## using the solve() function.
## If the result has already been calculated then "cacheSolve" function will
## retrieve the inverse from the cache.
## That way the inverse of a matrix would not have to be calculated repeatedly,
## which can be computation-heavy.

cacheSolve <- function(x, ...){
	##Following the example given at the beginning
	##of the assignment description (by R.D. Peng).
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("Getting cached data.")
		return(inv)
	}
	
	##Use the R function solve() to get the inverse
	##of a matrix. 
	##The input matrix for this MUST be an invertible
	##matrix.
	##If the matrix is invertible then,
	##A^{-1} A = Identity matrix.
	
	data <- x$get()	
	inv <- solve(data,...)
	x$setinverse(inv)
	inv	
	
	##If the input matrix is not an invertible matrix, 
	##one can use the R function ginv(), from MASS library,
	##to invert a general matrix. 
	##However, the assignment asked us to use solve(). 
	##Example code for ginv() given below as comments.
	
	##library(MASS)
	##inv <- ginv(data,...) 
}
