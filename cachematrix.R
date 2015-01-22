# The following functions meet the requirements for Assignment 2.  Detailed explanations for what each function
# does is available as comments before each function.

# Furthermore, I provided an example at the end of this commit that shows what the functions would return.  It is
# marked as separate from the assignment by a comment indicating it as the example.

# The makeCacheMatrix function creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{ 
         # The initial value of m is null
         m <- NULL 
         
         # This function sets the initial value of x to be y and m to be NULL
         set <- function(y) 
         { 
                 x <<- y 
                 m <<- NULL 
         }
         
         # Returns the value of x
         get <- function() x 
         
         # This function sets m global equal to the value of m local returned by cacheSolve below
         # It sets the inverse of the matrix specified when the function is called
         setinv <- function(cacheSolve) m <<- cacheSolve 
         
         #Returns the value of m which is the inverse of x
         getinv <- function() m
         
         #List of all available functions (set, get, setinv, getinv)
         list(set = set, get = get, 
              setinv = setinv, 
              getinv = getinv) 
} 


# The cacheSolve function computes the inverse of the special 'matrix' returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix hasn't changed), then the cacheSolve 
# function returns the inverse from the cach rather than calculating it again.
cacheSolve <- function(x, ...) 
{ 
         # Returns the inverse of x
         m <- x$getinv() 
         
         # The if statement here checks to see if m is not null.  If it isn't null, then that means
         # that m has already been calculated and should be returned
         if(!is.null(m)) 
         { 
                 message("getting cached data") 
                 return(m) 
         }
         
         # If m is null (using the else statement), then the function continues to the following series of statements which 
         # solve for the inverse of x and return that value
         else
         {
         data <- x$get() 
         m <- solve(data)
         x$setinv(m) 
         m
         }
} 

# This is a working example for a simple 2x2 matrix

# Creates a matrix for x
x <- matrix(1:4,nrow=2,ncol=2)

# Returns the contents of x to show that it is a 2 x 2 matrix
x
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# Creates a list of functions above
MatrixFunctions <- makeCacheMatrix(x)

# Return the x matrix
MatrixFunctions$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# Shows that the inverse is null as it has not been calculated prior
MatrixFunctions$getinv()
# NULL

# Set the inverse of x
MatrixFunctions$setinv(solve(x))

# Returns the inverse calculated
MatrixFunctions$getinv()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# If getinv() is run again it would return the same matrix from the cache
MatrixFunctions$getinv()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
