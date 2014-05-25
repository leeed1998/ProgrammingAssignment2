## Edward Lee - 5/24/2014
# R Programming Assignment 2
# CacheMatrix - and Get Inverse of the Matrix for caching


makeCacheMatrix <- function(x = matrix()) {
	#Defines/Resets the cache m
	InverseMatrix <- NULL   
	        set <- function(y) {
                x <<- y
                InverseMatrix <<- NULL
        }

	#Get the matrix
	get <- function () x
	# now get the inverse of the matrix
	setInverse <- function(I) InverseMatrix <<- I
	getInverse <- function() InverseMatrix 
	# get the inverse now
	list (set = set, get = get, 
		setInverse = setInverse,
		getInverse  = getInverse )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		      #Invert the matrix
        InverseMatrix <- x$getInverse ()
      # Look for previous matrix
        if(!is.null(InverseMatrix)) {
                return(InverseMatrix)
        }
      # if not: get and solve the inverse of the matrix.
        cm <- x$get()
        InverseMatrix <- solve(cm, ...)
      # set the inverse of the matrix.
        x$setInverse (InverseMatrix)
        InverseMatrix

}

##Test and Use - got this from the Course Forum - Test cases Please!


amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
amatrix$getInverse()  # Returns matrix inverse
cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getInverse()  # Returns matrix inverse


