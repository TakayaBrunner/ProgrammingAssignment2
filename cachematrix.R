## Assignment completed by Takaya Brunner

## makeCacheMatrix  stores the functions as global 
## variables. These global variable functions are subsequently called  
## in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        ## set internal variable to null
        m <- NULL
        
        ## Create a function called set that globally sets x to y and m to null.
        ## y is the matrix you initially called in the function. Set is never
        ## called again, unless you re-run makeCacheMatrix.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Set the variable "get" to a function that pulls the x value
        ## (The x value is the un-inverted matrix initially called).
        get <- function() {
                x
        }
        
        ## Sets the m variable to the parameter passed. The value passed to 
        ## function is the inverse of the matrix calculated by cacheSolve.
        setInverse <- function(solve) {
                m <<- solve
        }
        
        ## Sets the variable to the function that pulls the value m.
        ## m will be null until it is set by setInverse.
        ## setInverse is called by the cacheSolve function.
        getInverse <- function() {
                m
        }
        
        ## Creates the list that is queriable by the cacheSolve function.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## First it pulls the value of getInverse from the above function. If that
## value is null, it calculates the inverse, and sets the inverse back into
## the function above. If it the value is not null, simply outputs the 
## calculated value.

cacheSolve <- function(x, ...) {
        ## Runs the getInverse function from the list created and sets it to m.
        ## This will give you the value of m. m is either a null, or the stored
        ## value of the inverse of the matrix.
        m <- x$getInverse()
        
        ## tests if the global variable m is null or not. If not null
        ## returns the value of m. 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If m is null, it pulls the un-inverted matrix and sets it to the
        ## "data" variable.
        data <- x$get()
        
        ## Inverts the matrix ("data") using the solve function and sets to m.
        m <- solve(data, ...) 
        
        ## sets the value of the inverted matrix to the list created in 
        ## makeCacheMatrix. If run again, the m value will no longer be null.
        x$setInverse(m)
        
        ## prints the value of the inverted matrix.
        m
}