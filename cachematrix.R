## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly 

## the following function stores a matrix and caches its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                           
  set <- function(y) {                                    
    cache_x <<- y                                   
    cache_m <<- NULL                                        
  }
  get <- function() cache_x                               
  set_cache_m <- function(m) cache_m <<- m    
  get_cache_m <- function() cache_m                       
  list(set = set, get = get,
       set_cache_m = set_cache_m,
       get_cache_m = get_cache_m)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {                     
  m<- x$get_cache_m()               
  if(!is.null(m)) {                 
    message("getting cached data")  
    return(m)
  }                                      
  a <- x$get()                  
  b <- solve(a)  
  x$set_cache_m(b)             
  b                           
}
