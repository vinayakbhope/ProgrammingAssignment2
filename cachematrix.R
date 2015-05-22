## First function is used to provide cache methods for inverse of a matrix
## Second function is used to access cache or calculation of inverse of matrix

## makeCacheMatrix provides four methods to set and get the inout values and set and
# get the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        
        inverseMatrix <- NULL
        
        # set method for input
        setInput <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        # get method for inout
        getInput <- function() {
                x
        }
        
        # set method for inverse
        setInverse <- function(inverse_matrix) {
                inverseMatrix <<- inverse_matrix
        }
        
        # get method for inverse
        getInverse <- function() {
                inverseMatrix
        }
        
        list(setInput = setInput, getInput = getInput, setInverse = setInverse, getInverse = getInverse)
        
}


## This function uses the list output created by makeCacheMatrix function as its input
## using getInverse function from makeCacheMatrix function it first checks if the inverse
## is calculated or not. If yes, it is read from get method else inverse is calculated.
## This calculated inverse is then set into makeCacheMatrix function for caching
cacheSolve <- function(x, ...) {
        
        inverseOfMatrix <- x$getInverse()
        
        if(!is.null(inverseOfMatrix)) {
                print("Inverse from cached data")
                return(inverseOfMatrix)
        }
        input_data <- x$getInput()
        inverseOfMatrix <- solve(input_data)
        x$setInverse(inverseOfMatrix)
        return(inverseOfMatrix)
}