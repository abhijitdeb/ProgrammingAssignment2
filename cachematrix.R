## The following two functions can be used when you need to
## get the inverse of a matrix in a repeated fashion. Since
## creating the inverse of a matrix time-consuming it is 
## better to use makeCacheMatrix function to create a matrix
## that will have its inverse associated with it in the cache. 
## cacheSolve will immediately return the cached inverse of 
## the matrix when called.

## makeCacheMatrix function associates four additional functions 
## to the matrix passed as an argument. The inverse is not yet  
## calclauted in the makeCacheMatrix call. The four functions that
## get associated with the matrix are: 1) set, which saves the
## matrix passed as an argument in the cache. It also removes the 
## inverse matrix data associated with this matrix 2) get, which retrievs 
## the matrix from the cache that was set there by the call to "set" 
## 3) setsolve, which saves the matrix passed in as an argument as the
## inverse in the cache, which is separate from the cache used in the  
## "set" function, and, 4) getsolve, which retrieves the matrix that was 
## saved by the setsolve function in the cache.


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)       
}


## cacheSolve is the function which should be passed a matrix
## that was prepared by the makeCacheMatrix function, so that
## the cache that was associated with the matrix in the 
## makeCacheMatrix function call can be used here. It first tries
## to retrieve the inverse of the matrix from the cache. If
## it is not found there, it will calculate the inverse and 
## save the resultant matrix in the cache associated with the 
## original matrix. At the end, the inverse of the matrix is 
## returned. As evident, if in the first step itself, the inverse 
## matrix is found in the cache, that would be sent back 
## immediately.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}