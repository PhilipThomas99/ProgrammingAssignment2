### In this file, I show that it is faster to use makeCacheMatrix rather than recalculate the inverse matrix many times ###


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
        #message("getting inverse from cache") #this is commented because we run the function in a loop
        return(m_inv) #return the inverse matrix from the cache
        
    }
    # if cache is not exist, then we execute the codes below
    data <- z$get() # get the matrix for z
    m_inv <- solve(data,...) #calculate the inverse, save it in m_inv
    z$setinv(m_inv) #set m_inv in makeCacheMatrix to be equal to (calculated) m_inv in cacheSolve 
    return(m_inv) #show the result of inverse matrix
}

#########################
## Uncomment lines below to see the difference between caching inverse and not caching inverse. This case is inverting 100 by 100 matrix 1000 times
## non_cachetime save the results without caching and cachetime is when we get the result from cache
## the printout of the non_cachetime and cachetime show that cachetime is faster

test_matrix <- matrix(runif(10000,1,10000),ncol = 100, nrow = 100)

##Calculate Non Cache Inverse 1000 times
proc_time1 <- proc.time() 
for (i in 1:1000) {
    nonCacheInv <- solve(test_matrix)
}
non_cachetime<- proc.time() - proc_time1
message("Time Spent Using Non-Cache Method")
print(non_cachetime) #print runtime for non cache method

##Calculate Cache Inverse 1000 times
proc_time2 <- proc.time()
inst_z <- makeCacheMatrix(test_matrix)
for (i in 1:1000) {
    nonCacheInv <- cacheSolve(inst_z)
}

cacheTime <- proc.time() - proc_time2
message("Time Spent Using Cache Method")
print(cacheTime) #print runtime for cache method