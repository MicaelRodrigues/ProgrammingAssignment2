#This function creates a special vector which is really a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inversed matrix
##  get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #get the value of the matrix
    get <- function() x
    
    #set the value of the inverse matrix with solve function,
    ##Note: solve(a, b,...) If missing, b is taken to be an identity matrix and solve will return the inverse of a.
    setinverse <- function(solve) m <<- solve
    
    #get the value of the inverse matrix
    getinverse <- function() m
    
    #return list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Returns the inverse of the matrix which is passed to or set in the makeCacheMatrix function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<- x$getinverse()
    if(!is.null(m)){
        message("Getting cached Data")
        return(m)
    }
    matrix<- x$get()
    m<-solve(matrix, ...)
    x$setinverse(m)
    m
}
