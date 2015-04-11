## These functions can cache a matrix and its inverse.

## Creates list of functions
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        getm <- function() x            #get x
        geti <- function() inv          #get inv
        seti <- function(inverse) {     #set variable inv to input
                inv <<- inverse
        }
        list(getm = getm, geti = geti, seti = seti)     #make list of functions
}


## Checks if the inverse already exists, and if not, calculates it
cacheSolve <- function(x, ...) {
        inv <- x$geti()                                 #subsets inverse
        if (!is.null(inv)) {                            #checks if inverse exists
                message("Getting cached data...")
                return(inv)                             #returns cached inverse
        }
        else {
                mat <- x$getm()                         #subsets matrix
                inv <- solve(mat)                       #calculates inverse
                x$seti(inv)                             #caches inverse
                return(inv)
        }
}
