


## makeCacheMatrix creates a special Matrix Object which can cache its inverse 

## cacheSolve function computes the inverse of the special matrix returned by the function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        ## This function returns a list of getter and setter methods for a matrix and its inverse

        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
       
        inv = x$getinv()
        ## if inverse of the matrix already exists get it from Cache 
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## if the inverse does not exists calculate it and add it to Cache
        mat = x$get()
        inv = solve(mat, ...)

        x$setinv(inv)
        
        return(inv)
}
















