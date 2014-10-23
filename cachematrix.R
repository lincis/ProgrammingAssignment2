##
## Set of functions for caching inverses of matrices
##
# Function converts input to square matrix and tests for singularity
makeSquareMatrix <- function(Mx = matrix())
{
#	Convert input data to matrix
	Mx <- as.matrix(Mx)
#	Read matrix dimensions
	dims <- dim(Mx)
	if( dims[1] != dims[2] )
	{
#		Convert non-square matrix to square matrix by expanding
		dimSq <- ceiling(sqrt(dims[1] * dims[2]))
		Mx <- matrix(Mx, dimSq, dimSq)
		d1 <- dims[1]
		d2 <- dims[2]
		message(paste("Non square matrix (", d1, "x", d2, ") converted to square matrix (", \
					  dimSq, "x", dimSq, ")"))
	}
#	Test if matrix is singular and return NULL if true
	if( det(Mx) == 0 )
	{
		message("Sorry the matrix is singular, NULL object returned")
		return(NULL)
	}
#	Return a non-singular matrix
	Mx
}
# Function to make matrix with cached inverse matrix
makeCacheMatrix <- function(Mi = matrix())
{
	Mx <- makeSquareMatrix(Mi)
#	Variable i will store inverse of Mx, initialize to NULL
    i <- NULL
#	Function to set matrix to new value
    set <- function(My)
    {
        Mx <<- makeSquareMatrix(My)
#		New matrix requires resetting inverse matrix as well
        i <<- NULL
    }
#	Function to return "original" matrix
    get <- function() Mx
#	Function to set inverse matrix
    setinv <- function(inv) i <<- inv
#	Function to return the inverse matrix
    getinv <- function() i
#	Return statement: a list of all defined functions
    list(set = set,
        get = get,
        setinv = setinv,
        getinv = getinv)
}

# Function to return inverse matrix (either cahced or freshly calculated)
cacheSolve <- function(Mx, ...) {
#	Let's get current inverse matrix
	i <- Mx$getinv()
#	If it is not NULL return the stored value
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
#	Otherwise calculate the inverse matrix
	i <- solve(Mx$get(), ...)
#	And cache the calculated matrix
	Mx$setinv(i)
#	And return it
	i
}