## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Mat, Inv - global variables
## Mat contains an invertible square matrix
## Inv contains the inverse of Mat
## function set(matr) replaces Mat by matrix matr
##			and nulls inverse Inv in case matr is different from Mat
##			otherwise nothing happens

makeCacheMatrix <- function(Mat = matrix()) {
        Inv <- NULL
        set <- function(matr) {
		    if(
!(is.matrix(matr) && is.matrix(Mat) && dim(matr) == dim(Mat) && all(matr == Mat))){
        		Mat <<- matr
 		                  Inv <<- NULL}
        }
        get <- function() Mat
        setInv <- function(Inverse=NULL){
		if(!is.null(Mat)){
			if(is.null(Inverse)){
				Inv<<-solve(Mat)
			}
			else{if(is.null(Inv)){
				Inv <<- Inverse}
			}
		}
	  }
        getInv <- function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(MatLst, ...) {
      ## Return a matrix that is the inverse of 'Mat' in MatLst
	## if it was not computed before, compute it, otherwise
	## use cached value of 'Inv' in MatLst
        Inverse <- MatLst$getInv()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- MatLst$get()
        Inverse <- solve(data, ...)
        MatLst$setInv(Inverse)
        Inverse

}
