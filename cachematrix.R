## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will create a matrix x, and expose three methods to set/get x and its inverse

makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL ## initialize inverse
  set <- function(userValue = matrix()) {
    x <<- userValue ## set x in parent env with the desired value
    cachedInv <<- NULL ## if inverse is already set, get rid of it!
  }
  get <- function() x ## get x
  setInverse <- function(invVal) {
    cachedInv <<- invVal ##set inverse variable in parent env to desired value
    return(cachedInv) ## return the value as a convenience
  }
  getInverse  <- function() cachedInv ##get inverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## given a matrix x, will attempt to solve its inverse and return it 

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { ##special matrix provided or create a test 2x2 matrix
  ## Return a matrix that is the inverse of 'x'
  calculatedInverse <- x$getInverse() ## let's see if there's something there already
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { ##check if there's a cached value AND its a matrix
    message("We found cached data and saved valuable cpus!!!")
    return(calculatedInverse)
  }
  matrixToSolve <- x$get()  ##otherwise get the matrix
  calculatedInverse <- tryCatch({ ## try to solve the matrix and catch errors and warnings otherwise
    solve(matrixToSolve)
  }, warning=function(w) {
    message("This may not be the result you're looking for")
    message(w)
  }, error=function(e) {
    message("Something went wrong solving your matrix")
    message(e)
    message("\n")
  })
  message("Setting provided matrix to:") ## whatever the case, set the value of the inverse (Null if something went wrong)
  x$setInverse(calculatedInverse)
}
