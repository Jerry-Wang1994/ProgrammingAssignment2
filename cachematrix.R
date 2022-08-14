## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	j<-NULL
	set<-function(y){
		x<<-y
		j<<-NULL
	}
	get<-function()x
	setInv<-function(inverse)m<<-inverse
	getInv<-function()j
	list(set = set,get = get,setInv = setInv,getInv = getInv)
}


## Write a short comment describing this function

## The second function computes the inverse of the matrix returned
## by the first function. If the inverse has already been calculated,
## then the second function retrives the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	j<-x$getInv()
	if(!is.null(j)){
		message("retrieve cached data")
		return(j)
	}
	i<-x$get()
	j<-solve(i, ...)
	x$setInv(j)
	j
}

## to test the two functions
## source the R file
>source ("ProgrammingAssignment2\cachematrix.R")
## define a matrix
>test_matrix <- makeCacheMatrix(matrix(2:5,2,2))

>test_matrix$get()
     [,1] [,2]
[1,]    2    4
[2,]    3    5
>test_matrix$getInv()
NULL

>cacheSolve(test_matrix)
     [,1] [,2]
[1,] -2.5    2
[2,]  1.5   -1



