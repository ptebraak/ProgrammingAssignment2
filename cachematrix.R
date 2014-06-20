# There are 2 functions for interating with the cache
# makeCacheMatrix is a method (function) for interacting with the cache
# cacheSolve interacts with makeCacheMatrix to return 

# makeCacheMatrix is the list container for interacting 
# with the cached objects (note it forces to global cache through using the <<- operator
# essentially, its  a method for interating with the 'CacheMatrix'
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y){     # set the global (cached) var x (matrix)
        x <<- y
        m <<- solve(x)		# note if new set, new solve is required
    }
    setinverse <- function(solve) m <<- solve    
    get <- function()x              # return the global (cached) matrix x
    getinverse <- function() m      # returns cached m

    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# 	cacheSolve checks globaly for variable m (ie the cache object holding the inverse) 
#	if exists ==> return the value otherwise determine the value, cache it(via makeCache) and reutrn it
cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    if (!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    data <- x$get
    m <- solve(data, ...)
    x$setinverse(m)
    m

}
