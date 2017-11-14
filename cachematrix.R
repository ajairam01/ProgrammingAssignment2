## Generates the inverse of a matrix

## This function creates a special function 
## that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
          invm <- NULL

            set <- function(y) {

                    x <<- y

                    invm <<- NULL

            }

            get <- function() x

            setinv <- function(solve) invm <<- solve
            getinv <- function() invm

            list(set = set, get = get,

                 setinv = setinv,

                 getinv = getinv)

}


## This function returns the inverse of a matrix 
## It returns it from the cache is present

cacheSolve <- function(x, ...) {
                    invm <- x$getinv()

            if(!is.null(invm)) {

                    message("getting cached data")

                    return(invm)

            }

            data <- x$get()

            invm <- solve(data, ...)

            x$setinv(invm)

            invm


}
