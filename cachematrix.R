## Function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(mtx = matrix())
{
	invMtx <- NULL;	
	set <- function(x)	# set values in cache
	{
		mtx <<- x;
		invMtx <<- NULL;
	}
	get <- function()	# get value from cache
	{
		return(mtx);
	}
	setinv <- function(y)	# set the matrix inverse in cache
	{
		invMtx <<- y;
	}
	getinv <- function()	# get the inverse matrix from cache
	{
		return(invMtx);
	}
	return(list(set = set, get = get, setinv = setinv, getinv = getinv));
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...)
{
	invMtx <- mtx$getinv();	# get the inverse from the cache
	if(!is.null(invMtx))	# return the inverse if the inverse is not null
	{
		message("Getting cached data...");
		return(invMtx);
	}
	data <- mtx$get();	# if the inverse is not calculated then get the matrix
	invMtx <- solve(data, ...);	# solve for its inverse
	mtx$setinv(invMtx);	# set the value of the inverse matrix
	return(invMtx);		# return the inverse matrix
}
