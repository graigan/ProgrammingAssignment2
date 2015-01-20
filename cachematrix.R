## Set of functions for storing inverse matrices in the cache
## Written on Jan 20, 2015 for R Programming class on Coursera

## Function list for storing matrix and its inverse

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL

    set <- function(A){ mat <<- A; inv <<- NULL }   ## provisions for getting/setting the matrix
    get <- function(){ mat }

    setInv <- function(B){ inv <<- B }              ## provisions for getting/setting the inverse
    getInv <- function(){ inv }

    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Solves for the inverse, or retrieves inverse from cache if previously solved
## Note: '...' argument removed to prevent unintended uses.

cacheSolve <- function(A) {

    inv <- A$getInv()                       ## Retrieve inverse from cache

    if(!is.null(inv)) {
        message("getting cached data")      ## If 'inv' is NOT null, notify user
    } else {
        mat <- A$get()                      ## Otherwise, solve for inverse and store in cache
        inv <- solve(mat)
        A$setInv(inv)
    }

    inv                                     ## Return 'inv'
}
