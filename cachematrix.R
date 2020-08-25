## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
#this function creates a special matrix that allows us to cache its inverse
cacheSolve<-function(x, ...){
	m<-x$getinverse()
	if (!is.null(m)){
		message("getting cached data")
		return (m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$setinverse(m)
	m
}
#this function computes the inverse of the special matrix created in the previous function
