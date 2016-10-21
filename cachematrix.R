## below are two functions that stores a matrix and other caches the inverse 
## of a matrix (if inverse is calculated it fetches the inverse else it find the inverse)

## In the first function makeCacheMatrix creates a special list containing function to
## set the value of matrix
## get the value of matrix
## set the value of inverse
## get the value of inverse

makeCacheMatrix <- function(x = matrix()) {

			i<-NULL
				set<-function(y)
					{
						x<<-y
						i<<-NULL
					}
			    get<-function()x
				setinv<-function(inverse)i<<-inverse
				getinv<-function()i
				
		list(set = set,get = get,setinv = setinv,getinv = getinv)
					

}

## Write a short comment describing this function
## this second function finds the inverse of the matrix got from above function ,however it first checks to see
## if the inverse has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i<-x$getinv()
			if(!is.null(i)){
				message("getting cached data")
				return(i)
			}
		data<-x$get()
		i<-solve(data,...)
		x$setinv(i)
		i
}
