makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
        set <- function(y) {                                ## La primera función, makeCacheMatrix crea una "matriz" especial
                x <<- y                                     ## que es realmente una lista que contiene una función para:
                inv <<- NULL                                  ## establecer la matriz
        }                                                   ## obtener la matriz
        get <- function() x                                 ## establecer la matriz inversa
        set_matrix_inverse <- function(solve) inv <<- solve   ## obtener la matriz inversa
        get_matrix_inversa <- function() inv
        list(set = set, get = get,
             set_matrix_inverse = set_matrix_inverse,
             get_matrix_inversa = get_matrix_inversa)
}

## This function creates a special "matrix" object that can cache its inverse

cacheSolve <- function(x, ...) {
        inv <- x$get_matrix_inversa()
        if(!is.null(inv)) {
                message("getting cached data")            ## La siguiente función calcula la matriz inversa de la “matriz” creada con la función anterior
                return(inv)                                ## Sin embargo, primero verifica si la matriz inversa ya se ha calculado
        }                                                 ## Si es así, obtiene la matriz inversa del caché y omite el cálculo
        data <- x$get()                                   ## De lo contrario, calcula la matriz inversa and
        inv <- solve(data, ...)                           ## establece matriz inversa en la memoria caché mediante la función set_matrix_inverse()                  
        x$set_matrix_inverse(inv)
        inv  
        ## Return a matrix that is the inverse of 'x'
}
