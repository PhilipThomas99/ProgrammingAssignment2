### In this file, I wrote makeCacheMatrix function and cacheSolve function with brief example ###


## makeCacheMatrix function is used to initiate a list of function that we will used to cache the inverse of a matrix. 
##(in the OOP term, we'll instantiate the object using this function. The object in this case is the matrix that we want to invert)

makeCacheMatrix <- function(x = matrix()) {
    
    m_inv <- NULL # when we define new x, it'll automatically set the m_inv to NULL
    
    set <- function(y) { # the first function, "set" ,is another way to initialize the setup. 
        x <<- y          # set the x into the matrix that we want to invert
        m_inv <<- NULL   # set the inverse matrix to null
                         # example usage of set below
    }
    
    get <- function() { # we use "get" function to get the matrix that we want to invert. in this case, x
        x               # the get function is evoke when we can't find the inverse in the cache. see cacheSolve function below
    } 
    
    setinv <- function(mat_inverse) { # we use "setinv" function is to cache the solution of the inverse.  
        m_inv <<- mat_inverse         # this function is evoked after we calculate the inverse matrix, so that when we run the same matrix, we'll get the cache instead.
    }
    
    getinv <- function () { # we use "getinv" function to get m_inv variable from CacheMatrix.
        m_inv               # Specifically, "getinv" is evoked to check either there is a cache or not.
    }                       # see the implementation on cacheSolve function
    
    list(set = set, get = get, # we use this to save the instantiation of makeCacheMatrix as list
         setinv = setinv,  #where we name each function equal to the function we define here.
         getinv = getinv)
}


## We use cacheSolve function to check that for the given input data, is there any cache on the inverse matrix or not, if not then we calculate the inverse

cacheSolve <- function(z, ...) {
    
    m_inv <- z$getinv() #first, we check whether there is inverse matrix cache for z or not
    if(!is.null(m_inv)) { #if cache is exist, then we print the results without recalculating the matrix
        message("getting cached data") #this message is printed out to specify that we are getting the inverse from the cache
        return(m_inv) #return the inverse matrix from the cache
        
    }
    # if cache is not exist, then we execute the codes below
    data <- z$get() # get the matrix for z
    m_inv <- solve(data,...) #calculate the inverse, save it in m_inv
    z$setinv(m_inv) #set m_inv in makeCacheMatrix to be equal to (calculated) m_inv in cacheSolve 
    return(m_inv) #show the result of inverse matrix
}
