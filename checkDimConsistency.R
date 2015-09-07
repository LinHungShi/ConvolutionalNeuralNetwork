checkDimConsistency <- function(x_dim, window_dim, stride){
    
    if(!all(trunc((x_dim - window_dim) / stride) == (x_dim - window_dim) / stride))
        return (FALSE)
    return (TRUE)
}