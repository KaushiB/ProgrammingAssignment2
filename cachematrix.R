## I cannot create a create a Github repository on my local machine,
## because Github for MAC OS X is only compatible for 10.9 or higher
## versions. I have 10.8.5.
## Therefore, I am editing the repository online.

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function.

makeCacheMatrix <- function(x = matrix()){
	##Following the example given at the beginning
	##of the assignment description.
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


cacheSolve <- function(x, ...){
	##Following the example given at the beginning
	##of the assignment description.
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
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
