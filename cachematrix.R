# This function creates a special matrix object that can cache its inverse, 
# it contains various other functions to achieve the same.

makeCacheMatrix <- function(x = matrix())
{
	inverse_matrix = NULL

	#Step 1: Setting the matrix
        set = function(y) 
	{
		x <<- y
            inverse_matrix <<- NULL
      }

	#Step 2: Getting the matrix 
	get = function() x

	#Step 3: Setting the inverse 
      set_inverse = function(inverse_matrix) inverse_matrix <<- inverse 

	#Step 4: Getting the inverse
      get_inverse = function() inverse_matrix
      list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}

# This funcation calculates inverse of the matrix
# This fnction first checks in invesrse of matrix is available in cache, if found in cache it returns the same, otherwise it camputes the inverse and returns it.  
cacheSolve <- function(x, ...) 
{
      inverse_matrix = x$get_inverse()
        
	#if function checks if the inverse is returned from cache in above statement
      if (!is.null(inverse_matrix))
	{
      	# if inverse is found in cache returning same without calculating again 
            message("getting cached data")
            return(inverse_matrix)
	}
        
      # Calcuting inverse if it is not present in cache 
      data = x$get()
      inverse_matrix = solve(data, ...)
        
      # sets the value of the inverse in the cache
      x$set_inverse(inverse_matrix)
      return(inverse_matrix)
}
