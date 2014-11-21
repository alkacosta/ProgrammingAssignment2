##  R function to cache potentially time-consuming computations. This takes advantage of the scoping rules of R
##  language and how they can be manipulated to preserve state inside of an R object.
##

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL           ## sets the value to NULL (default if cacheSolve has not yet been used)
  set<-function(y){ ## set the value of the matrix
    x<<-y           ## caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
    m<<-NULL        ## sets the value of m (the matrix inverse if used cacheSolve) to NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##             above. If the inverse has already been calculated (and the matrix has not changed), then the 
##             cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
  # compare matrix to previous value if any
  invmtx<-x$getmatrix()  # get an inverse if it has already been calculated
  if(!is.null(invmtx)){  # check to see if cacheSolve has been run before 
    print("getting cached data")
    return(invmtx)
  } else {
    matrix<-x$get()       # run the getmatrix function to get the value of the input matrix
    invmtx<-solve(matrix, ...) # compute the value of the inverse of the input matrix
    x$setmatrix(invmtx)        # run the setmatrix function on the inverse to cache the inverse
    return(invmtx)
  }
}