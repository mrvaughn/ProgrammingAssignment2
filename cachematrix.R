## This file contains two functions to satisfy the requirements of Assignment 2 in the course
##   R Programming (Coursera/Johns Hopkins University Data Science series).
##   Last modified 5/23/2014 by Michael Vaughn

## The makeCacheMatrix function accepts a matrix vector, and returns an object with methods
##   that can be used to facilitate manipulating the values stored in the object in a way
##   that allows an inverse calculation to be cached.

makeCacheMatrix <- function(x = matrix()) {
    ## i will store the inverse of x, initially set to NULL
    i <- NULL
    
    ## "set" is a method that can change the matrix vector used by this object
    set <- function(y) {
        ## use the super assignment operator to replace the original matrix with the
        ##   one called by the set method
        x <<- y
        
        ## if the set method is called, reset the value of i to NULL
        ## we should assume that the new matrix is different, and i should be recalculated
        i <<- NULL
    }
    
    ## "get" is a method that can be used to return the matrix stored in this object
    get <- function() x
    
    ## "setinverse" is a method that uses the super assignment operator to store the
    ##    passed argument (a matrix, the inverse of x) to i
    setinverse <- function(inverse) i <<- inverse
    
    ## "getinverse" is a method that returns i, which is NULL or a cached copy of the
    ##    inverse of x
    getinverse <- function() i
    
    ## return a list of functions. In other words, create an object with methods.
    ##   Think of this function as being a class, in the Object Oriented Programming sense,
    ##   and assigning the results of a call to this "class" as an object, or instance of 
    ##   the class.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function will calculate the inverse of a "matrix object", as defined by
##   the function makeCacheMatrix (you can think of the makeCacheMatrix function as being a
##   class definition, in Obect Oriented Programming terms). If the "matrix object" already
##   has a value for the inverse, cacheSolve will simply return it. Otherwise, cacheSolve
##   will calculate the inverse, store it to the matrix object, and return its value.
##   NOTE: this function also prints "getting cached data" to the console to prove that
##   it is using caching, for the sake of this exercise.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x', which is assumed to be an object created
    ##   by the makeCacheMatrix function.
    
    ## get the current value of the inverse stored in the matrix object
    i <- x$getinverse()
    
    ## the inverse value will be NULL when the matrix object is created, or its matrix value
    ##   is changed using the set() method. Check for the NULL value, and if it is NOT null,
    ##   return the cached value and exit this function.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Continue here if i is NULL, which means the inverse has not been calculated.
    ## Pull the matrix to be inverted into "data". The matrix is stored in the matrix object
    ##   when the object is created, so use the get() method to retrieve it.
    data <- x$get()
    
    ## Calculate the inverse of the matrix using the solve() function.
    ## NOTE: the matrix must be a square invertible matrix, or an error will occur.
    i <- solve(data, ...)
    
    ## Store the inverse calculation to the matrix object using its setinverse() method.
    x$setinverse(i)
    
    ## return the value
    i
}
