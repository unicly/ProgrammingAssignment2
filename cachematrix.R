## The task of these two functions is to invert a matrix and save it to the cache

## Creates a "matrix" object and caches its inverse
makeCacheMatrix <- function(originalMatrix = matrix()) {
    
    invertedMatrix <- NULL
    
    ## If the makeCacheMatrix object is already created, the value is changed with this function
    set <- function(y) {
        originalMatrix <<- y
        invertedMatrix <<- NULL
    }
    
    get <- function(){
        originalMatrix
    }
    
    ## Assigns calculated inverse of the matrix to the parent environment
    setInverse <- function(originalMatrix){
        invertedMatrix <<- originalMatrix
    }
    
    ## Inverts the matrix
    getInverse <- function() {
        invertedMatrix
    }
    
    ## Returns and names each element of the list.
    ## Enables the use of the "$" operator
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Retrieves the cached inverse of the matrix, if already in cache, or inverts and stores it in cache  
cacheSolve <- function(matrixObj, ...) {
    
    ## Checks if an inverted version is available in cache
    matrix <- matrixObj$getInverse()
    
    ## Returns cached data if exists
    if(!is.null(matrix)) {
        message("Getting cached matrix")
        return(matrix)
    }
    
    ## Calculates the inverse matrix and saves it in the cache
    matrix <- matrixObj$get()
    invertedMatrix <- solve(matrix)
    matrixObj$setInverse(invertedMatrix)
    matrix
}

## Generate the matrix
## exMatrix <- matrix(sample.int(10, 4, TRUE), nrow = 2, ncol = 2)

## Create the matrix cache object
## matrixObject <- makeCacheMatrix(exMatrix)

## Get the matrix
## cacheSolve(matrixObject)