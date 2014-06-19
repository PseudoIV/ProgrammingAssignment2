## These functions create a new object that will hold a matrix and its
##  cached inverse.  The inverse is calculated only when needed.

## makeCacheMatrix
## This function takes a matrix as input and stores it.  It also contains
##  space for the matrix's inverse if needed.

makeCacheMatrix <- function(A = matrix()) 
{
    matrixInverse <- NULL               ## No inverse yet
    
    ## Cache the input matrix when the value is changed
    set <- function(inputMatrix) 
    {
        A <<- inputMatrix
        matrixInverse <<- NULL
    }
    
    ## Cache the inverse
    setInverse <- function(inputInverse) matrixInverse <<- inputInverse
    
    ## These just return the data we are storing
    getInverse <- function() matrixInverse
    get <- function() A
    
    ## The actual object returned is a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## cacheSolve
## This function takes a matrix in an object of the type created above and
##  returns its inverse.  If the inverse has not yet been cached, this 
##  calculates and caches it.

cacheSolve <- function(B) 
{
    ## Check to see if it is cached and if so return the inverse
    theInverse <- B$getInverse()
    if(!is.null(theInverse)) 
    {
        ## message("getting cached data")
        return(theInverse)
    }
    
    ## Calculate and cache the inverse
    theMatrix <- B$get()
    theInverse <- solve(theMatrix)
    B$setInverse(theInverse)
    
    ## Return
    theInverse
    
}
