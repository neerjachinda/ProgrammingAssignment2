## For running time consuming computations, caching the value makes sense which 
## can be looked up again when needed rather than computing again

## makeCacheMatrix creates a list containing following functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y=matrix()) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment.       
                x<<-y
                inv<<-NULL
        }
        
        get<- function () {
                x
        }
        
        setInverse<- function(inv) {
                inv<<-solve(x)
        }
        
        getInverse<-function() {
                inv
        }
        
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setInverse function.

cacheSolve <- function(x, ...) {
        
        inverse <-x$getInverse()
        if(!is.null(inverse)) {
                message("Getting cached data")
                return(inverse)
        }
        matData<-x$get()
        
        #Checks if the matrix is invertible
        if(det(matData)==0) {
                message("Matrix is not invertible")
                return(matData)
        }
        inverse<-solve(matData)
        x$setInverse(inverse)
        inverse
}
