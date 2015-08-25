transKerForm <- function(input, input_dim, stride, window_dim, batch, num_fp){
    
    library(plyr)
    x <- t(input)
    arr_form  <- array(x, dim = c(input_dim[1], input_dim[2],batch, num_fp))
    
    arr_kerinput <- aaply(arr_form, 3, patch3dArr, window = window_dim, stride = stride, .drop = FALSE)
    arr_kerinput <- aperm(arr_kerinput, perm = c(2,3,1))
    dim_arrkerinp <- dim(arr_kerinput)
    dim(arr_kerinput) <- c(dim_arrkerinp[1], prod(dim_arrkerinp[2:3]))
    indexform <- matrix(1:length(arr_kerinput), dim(arr_kerinput)[1], dim(arr_kerinput)[2])
    return (list(value = arr_kerinput, index = indexform))
}