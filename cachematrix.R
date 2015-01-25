## Below are the functions to create a special Matrix
## and cache its inverse

## This function creates a Matrix so that the inverse could be cached

makeCacheMatrix <- function(x = matrix()) {
                     m<-NULL
                     set<-function(y){
                     x<<-y
                     m<<-NULL
  }
                     get<-function() x
                     setmatrix<-function(solve) m<<- solve
                     getmatrix<-function() m
                     list(set=set, get=get,
                       setmatrix=setmatrix,
                       getmatrix=getmatrix)

}


## This function computes the inverse of the matrix from above function.
## If inverse already calculated then retreive from cache else calculate Inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                    m<-x$getmatrix()
                    if(!is.null(m)){
                      message("getting cached data")
                      return(m)
                    }
                    matrix<-x$get()
                    m<-solve(matrix, ...)
                    x$setmatrix(m)
                    m
}
