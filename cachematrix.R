#Firstly, we assume that the matrix supplied is always invertible, 
#so makeCacheMatrix is a function that creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
    
    a <- NULL
    set <- function(y) {
        x<<- y
        a <<- NULL
    }
    
    get <- function()x
    setInverse <- function(inverse) a<<-inverse
    getInverse <- function() a
    list(set = set, get= get, 
         setInverse = setInverse,
         getInverse = getInverse)
}

#Now we need to compute the inverse of the special matrix returned by makeCacheMatrix above. 
#If the inverse has been already computed, and the matrix hasn’t changed, 
#cacheSolve should retrieve the inverse from the cache
#We make use of the solve function in R to calculate the inverse of a square matrix

cacheSolve <- function(x, ...){
    a <- x$getInverse()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    mat <- x$get()
    a <- solve(mat, ...)
    x$setInverse(a)
    a
}

