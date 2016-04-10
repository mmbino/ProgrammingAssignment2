## Put comments here that give an overall description of what your
## functions do

## Create  a  function which creates a  matrix  which stores inverse  in cache so that  when it is seond  time the value  is  retrieved  from the  
# and give the associated  funtions to  set  and retrieve value
makeMatrix <- function(x = matrix()) {
        matinv <- NULL  
        set <- function(y) {                      
                x <<- y
                matinv <<- NULL              
        }
        ## gets the value of the inverse
        get <- function() x                           
        setinverse <- function(solve) matinv <<- solve 
        # gets the inverse     
        getinverse <- function() matinv        
        ## passes the value of the function makeCacheMatrix        
        list(set = set, get = get,                    
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Function  to retrieve  

cacheMatrix <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getinverse()
        
        if(!is.null(matinv)) {                 
                message("getting cached data - Inverse of the matrix")
                return(matinv)
        }
        
        data <- x$get()  
        matinv <- solve(data, ...)
        x$setinverse(matinv)
        matinv
}
