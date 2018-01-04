## Thefunction creates list of 4 functions (set, get, setinv and getinv), It takes input of an invertible matrix
## The function creates or sets matrix based on the function selected from its list


makecachematrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      
      setinv <- function(inversematrix) inv <<- inversematrix
      
      getinv <- function() inv
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)
      
}



## The function displays inverse of a matrix, first trying to get the matrix from cache if avaiable
## If the cache is not available it creates inverse matrix, saves to cache and displays on console

cachesolve <- function(x, ...){
      inv <- x$getinv()
      if(!is.null(inv)){
            message("Gettign cache data")
            return(inv)
      }
      
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}