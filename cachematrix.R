## this function creates a matrix 

makeCacheMatrix <- function(a = matrix() ) {
    
    ## inverse property
    i <- NULL

    ##matrix methodology for setting
        set <- function(matrix) {
            a <<- matrix
            i <<- NULL
        }
        ## method to get values of  matrix
        get <- function(){
            ## return matrix
            a
        }
    ##sets value of inverse
    setinverse <- function(inverse) {
        i <<- inverse
        
    }
    
    ##gets value of inverse
    getinverse <- function(){
    ##return inverse
        i
        
    }
    ## return list
    list(set = set, get = get, 
         setinverse = setinverse
         getinverse = getinverse)
}

## this function returns the inverse of the matrix above

cacheSolve <- function(x, ...) {
    a <- x$getinverse() 
    
    if (!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    ##shows the inverse is wanted
    data <- x$get()
    
    ##calculating inverse using matrix multiplication
    a <- solve(data) %*% data  

    ## set the inverse to the object
    x$setinverse(a)
    
    ## return the matrix
    a
    }
