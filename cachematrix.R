# The function creates the necessary objects needed for lexical scoping which is needed for creating the cache
makeCacheMatrix <- function(x = matrix()) {
        Cache_Inverse <- NULL
        set <- function(y) { # For a new Matrix, it clears all the previous input Matrix and cached inverse matrix to solve for the new Inverse of the new Matrix 
                x <<- y
                Cache_Inverse <<- NULL
        }
        get <- function() x #To Get the input Matrix to be solved for
        Set_Inverse <- function(Inverse_Matrix) Cache_Inverse <<- Inverse_Matrix # It caches the result of an obtained Inverse matrix from cacheSolve
        Get_Inverse <- function() Cache_Inverse # Returns the Inverse of the Matrix if the result is already cached
        list(set = set, get = get,
             Set_Inverse = Set_Inverse,
             Get_Inverse = Get_Inverse) # The list of arguments required as input arguments for cacheSolve Function
}


# This function is used to solve and obtain the inverse of the matrix and caching the result(Inverse Matrix)
cacheSolve <- function(x, ...) {
        Inverse_Function <- x$Get_Inverse() #Checks if there is a cached values
        if(!is.null(Inverse_Function)) {
                message("getting cached data")
                return(Inverse_Function) # If there is  cached values, it returns the inverse for that matrix
        }
        Input_Matrix <- x$get() # If theere isn't a cached value, it gets the input matrix for which the inverse should be calculated
        Inverse_Function <- solve(Input_Matrix, ...) # It obtains the inverse of the matrix
        x$Set_Inverse(Inverse_Function)# Caches thee result of the Inverse Matrix Obtained
        Inverse_Function
}