# makeCacheMatrix: This function takes a matrix and creates a list with 4 functions:
# reset the matrix, get the matrix, set the inverse and get the invere (if it has been stored)

makeCacheMatrix <- function(m1 = matrix(rnorm(4),2,2) ) {
    
    inv<-NULL
    
    set<-function(m2) {m1<<-m2; inv<<-NULL}
    get <- function() m1
    setinv <- function(m3) inv <<- m3
    getinv <- function() inv
    
    list(set=set,get=get,setinv=setinv,getinv=getinv)
    
}


# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse is stored, then cacheSolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(lm1, ...) {
    inv <- lm1$getinv()
    if(!is.null(inv)) {message("Getting inverse from cache")}
    else {
        message("Inverse has been calculated")
        inv <- solve(lm1$get())
        lm1$setinv(inv)
    }
    inv
}
