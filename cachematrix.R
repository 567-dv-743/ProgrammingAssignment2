rm(list = ls())

## The script contains two functions: makeCacheMatrix and cacheSolve.
## 1) makeCacheMatrix takes as input a matrix and creates getters and setters for 
## both the matrix and its inverse.
## 2) cacheSolve takes as input a makeCacheMatrix object, and either gets and 
## returns the previously calculated matrix inverse OR, if there is no previously
## calculated inverse, calculates it, stores it in the makeCacheMatrix object, and 
## then returns it.

## makeCacheMatrix takes as input a matrix and creates getters and setters for 
## both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function(){
        x
    } 
    
    setinverse <- function(inverse){
        inv <<- inverse
    }
    
    getinverse <- function(){
        inv
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## cacheSolve takes as input a makeCacheMatrix object, and either gets and returns
## the previously calculated matrix inverse OR, if there is no previously
## calculated inverse, calculates it, stores it in the makeCacheMatrix object, and 
## then returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}