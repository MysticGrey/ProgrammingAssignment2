makeCacheMatrix <- function(x = matrix()) {
        Cache_Inverse <- NULL
        set <- function(y) {
                x <<- y
                Cache_Inverse <<- NULL
        }
        get <- function() x
        Set_Inverse <- function(Inverse_Matrix) Cache_Inverse <<- Inverse_Matrix
        Get_Inverse <- function() Cache_Inverse
        list(set = set, get = get,
             Set_Inverse = Set_Inverse,
             Get_Inverse = Get_Inverse)
}



cacheSolve <- function(x, ...) {
        Inverse_Function <- x$Get_Inverse()
        if(!is.null(Inverse_Function)) {
                message("getting cached data")
                return(Inverse_Function)
        }
        Input_Matrix <- x$get()
        Inverse_Function <- solve(Input_Matrix, ...)
        x$Set_Inverse(Inverse_Function)
        Inverse_Function
}