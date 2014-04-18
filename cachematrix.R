## The following code is use to help with potentially time-consuming calculations
## by making a calculation only once (in this case an inverse of a given matrix)
## and enabling the user the access the result without constantly repeating
## the calculation

## This function creates a special "matrix" object
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        invM <- NULL
        set <- function(y) 
        {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(IM) invM <<- IM
        getInvMatrix <- function() invM
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
        
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then the
## `cachesolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        #ptm <- proc.time()
        invM <- x$getInvMatrix()
        if(!is.null(invM)) 
        {
                message("getting cached data")
                cat("Process Time", proc.time() - ptm)
                return(invM)
        }
        data <- x$get()
        invM <- solve(data)
        x$setInvMatrix(invM)
        invM
        #cat("Process Time", proc.time() - ptm)
}
