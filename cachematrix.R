
# Coursera; R Programming, Assignment 2 on Lexical Scoping
# by Matthew Johnson



# The two companion functions written for this assignment use the lexical scoping rules of R. They allow for the inverse of an  
# input matrix to be calculated once and cached so that, when needed again, it can be accessed directly rather than recalculated.


# task 1: create makeCacheMatrix() function -- this creates an R object containing four functions that will later be used 
#   by the cacheSolve() function to retrieve or calculate the inverse of an input matrix; the makeCacheMatrix() and 
#   cacheSolve() functions must be used together to achieve this result

  makeCacheMatrix <- function(x = matrix()) {
    # initialise an empty object 'x' by way of the makeCacheMatrix() function argument, defining 'x' as an empty matrix by 
    # default; at function execution the input value for x will be a matrix
    
    matr_inv <- NULL 
    # initialise 'matr_inv' as an empty object that will store the inverse of an input matrix
    
    set <- function(y) {
      x <<- y
      matr_inv <<- NULL
    }
    # define a 'setter' function, named set(), with the following behaviours:
    #   - assign the input argument of the function ('y') to the value of the 'x' object (assumed to be a matrix) from the 
    #   set() function's parent environment, which is the makeCacheMatrix() function (use of the <<- operator allows access to 
    #   the parent environment)
    #   - clear any cached value from the 'matr_inv' object by reassigning it the value of NULL so that it becomes empty again
    
    get <- function() x
    # define a 'getter' function, named get(), to retrieve the value of 'x' from the parent environment (as the value of 'x' is  
    # not defined within get())
    
    set_inv <- function(solve) matr_inv <<- solve
    # define a 'setter' function, named set_inv(), to calculate the inverse of a matrix using the 'solve' function from base 
    # R, and assign it to the 'matr_inv' object in the parent environment
    
    get_inv <- function() matr_inv
    # define a 'getter' function, named get_inv(), to retrieve the value of 'matr_inv' from the parent environment (as the 
    # value of 'matr_inv' is not defined within get_inv())
    
    list(
      set = set, 
      get = get,
      set_inv = set_inv,
      get_inv = get_inv
    )
    # assign each of the previously created functions to a named element within a list, and return the complete list to the 
    # parent environment as a new object of type 'makeCacheMatrix()'
  }


# task 2: create cacheSolve function -- execution of this function requires the object created by makeCacheMatrix() as its  
#   input argument, and uses it to retrieve the inverse of an input matrix if already stored in cache, or otherwise to calculate 
#   the inverse directly and store it in the cache
  
  cacheSolve <- function(x, ...) {
    
    matr_inv <- x$get_inv()
    # call the get_inv() function from the makeCacheMatrix() object and pass the result (when applied to an input matrix 'x') 
    # into 'matr_inv'; as the makeCacheMatrix() object is a list the $ operator can be used to call the function by name
    
    if(!is.null(matr_inv)) {
      message("getting cached data")
      return(matr_inv)
    }
    # check whether 'matr_inv' (and therefore the the result of get_inv()) is NULL; if it is not then the cache already 
    # contains a matrix inverse which is then returned alongside the console message "getting cached data" as notification
    
    matr <- x$get()
    matr_inv <- solve(matr, ...)
    x$set_inv(matr_inv)
    matr_inv
    # else if 'matr_inv' is NULL the input matrix 'x' is retrieved from the makeCacheMatrix() object, the solve function is 
    # used to calculate its inverse, the setter function 'set_inv'() is used to set this matrix inverse in the 
    # makeCacheMatrix() object, and finally the matrix inverse is returned to the parent environment by printing
    
  }

