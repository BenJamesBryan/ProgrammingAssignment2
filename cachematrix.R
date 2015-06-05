## This is very similar to the vector example in the assignment description. The 
## primary differences are creation of a matrix insetad of a vector, using language
## that indicates seeking an inverse rather than a mean, and then using the second
## function to look for and then solve for the inverse of the matrix.

## The first function "makeCacheMatrix" sets the four necessary functions in order to 
## set the matrix in the cache.  There is no printed output; it is only to set up the 
## second function.

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<-y
                a <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) a <<-inverse
        get_inv <- function() a
        list(get = get, set= set, get_inv = get_inv, set_inv = set_inv)
}


## The second function "cacheSolve" looks first to see whether the inverse is available.
## If not, the inverse is created using the function solve(). 

cacheSolve <- function(x, ...) {
       a <- x$get_inv()
       if(!is.null(a)) {
               message("getting cached data")
               return(a)
       }
       b <- x$get()
       a <- solve(b, ...)  ## The function "solve" returns the inverse of the matrix
       x$set_inv(a)
       a
               ## Return a matrix that is the inverse of 'x'
}
