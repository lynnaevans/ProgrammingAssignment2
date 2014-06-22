## The following two functions and the associated test harness below are 
## designed to cache a matrix inversion
## Created by Lynn A. Evans

## This function takes a matrix data type as an argument and returns a list of 
## four functionsto facilitate the operations required. 

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y){
                x<<- y
                I<<-NULL
        }
        get <- function() x
        setInverse <- function(solve) I<<- solve
        getInverse <- function() I
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## Checks to see if the matrix inverse has already been cached. If it has uses 
## it.  If not calculatesit, caches it and returns the inverse.

cacheSolve <- function(x, ...) {
        I<-x$getInverse()
        if(!is.null(I)){
                message("Getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data)
        x$setInverse(I)
        I
        ## Return a matrix that is the inverse of 'x'
}
## Test harness
## Generate the list of 4 functions
i <- makeCacheMatrix()
## Create a matrix to test and get its inverse
theMatrix <- matrix(c(1,-1,1,1),nrow=2)
theMatrix
theInverse <- solve(theMatrix)
theInverse
## Use the set function to create the matrix
i$set(theMatrix)
## Now get it
i$get()
## Use the cacheSolve funciton to calculate the inverse the first time
cacheSolve(i)
## Do it again - should see a message as to source of inverse this time
cacheSolve(i)
## Repeat the above steps with a second test matrix
theMatrix2 <- matrix(c(2,-2,2,2),nrow=2)
theMatrix2
theInverse2 <- solve(theMatrix2)
theInverse2
i$set(theMatrix2)
i$get()
cacheSolve(i)
cacheSolve(i)