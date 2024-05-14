## Code that caches the inverse of a matrix for
## fast lookup later

## constructs the cacheable matrix

makeCacheMatrix <- function(x = matrix()) {

	# this holds the inverse of 'x'
	ii <- NULL

	# sets a new matrix which will get its inverse cached
	set <- function(y) {
		x <<- y
		# clear the inverse, since we changed the matrix
		ii <<- NULL
	}

	# retreive the matrix
	get <- function() {
		x
	}

	set_inverse <- function(inv) {
		ii <<- inv		
	}

	get_inverse <- function() {
		ii
	}

	list(
		set = set,
		get = get,
		set_inverse = set_inverse,
		get_inverse = get_inverse
	)
}


## Pass this a 'cacheMatrix' as constructed by the above function.
## If the inverse has already been cached, this returns the cached
## value.  Otherwise, it computes the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

		inverse <- x$get_inverse()
		if ( ! is.null( inverse )) {
			# yay, it was cached
			message( "found inverse in cache" )
			return( inverse );
		}

		orig_matrix <- x$get()

		# compute the inverse
		inverse <- solve( orig_matrix )

		# and cache it
		x$set_inverse( inverse )

		inverse
}

## test

my_mat <- makeCacheMatrix( matrix( c( 1, 2, 3, 4 ), nrow = 2, ncol = 2 ))
print( cacheSolve( my_mat ))
print( cacheSolve( my_mat ))
