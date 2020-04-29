## Coursera - Programming With R Assignment 2


## This function will recive a matrix as input and will pass value to called environment 
## and will receive inverse matrix from called environment

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
  inv_matrix <- NULL                        ## initialize  inverse matrix as NULL; will hold value of matrix inverse 
  set <- function(y) {                      ## define the set function to assign new 
    x <<- y                                 ## value of matrix in parent environment
    inv_matrix <<- NULL                     ## if there is a new matrix, reset  inverse matrix to NULL
  }
  get <- function() x                       ## define the get fucntion - returns value of the matrix argument
  
  setinverse <- function(inverse) inv_matrix <<- inverse  ## assigns value of  inverse matrix in parent environment
  getinverse <- function() return(inv_matrix)             ## gets inverse matrix where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## order to refer 
  ## to the functions with the $ operator
}


## This function will recive original matrix from another function and 
## will convert to inverse matrix and will update inverse into other environment
## If the inverse has already been calculated 
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setinverse(inv_matrix)
  inv_matrix
}


#Test cases below "need to un commit to check output
#my_matrix <- makeCacheMatrix(matrix(c(1:4,10:14), 3, 3)) #set original matrix
#my_matrix$get() #check get function is returning the matrix or not

#my_matrix$getinverse() #check getinverse function with initial inverse matrix is NULL 
#cacheSolve(my_matrix)  # inverse matrix and update parant variable 
#cacheSolve(my_matrix)   # check the matrix is not inverse again as it is updated already 
#my_matrix$getinverse()  # echo inverse matrix into console
#my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))  #set original matrix uise set function
#my_matrix$get()            #check get function is returning the matrix or not
#my_matrix$getinverse() #check getinverse function where initial inverse matrix should be NULL 
#cacheSolve(my_matrix)  # inverse matrix and update parant variable
#cacheSolve(my_matrix)  # check the matrix is not inverse again as it is updated already 
#my_matrix$inv         # echo inverse matrix into console

