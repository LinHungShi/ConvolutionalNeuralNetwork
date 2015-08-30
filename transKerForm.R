transKerForm <- function(inputs, input_dim, stride, window_dim, batch, num_fp){
    
    library(plyr)
    x <- t(inputs)
    arr_form  <- array(x, dim = c(input_dim[1], input_dim[2],batch, num_fp))
    index_mat <- array(1:length(x), dim = dim(arr_form))
    arr_kerinput <- aaply(arr_form, 3, patch3dArr, window = window_dim, stride = stride, .drop = FALSE)
    arr_index <- aaply(index_mat, 3, patch3dArr, window = window_dim, stride = stride, .drop = FALSE)
    arr_kerinput <- aperm(arr_kerinput, perm = c(2,3,1))
    arr_index <- aperm(arr_index, perm = c(2,3,1))
    dim_arrkerinp <- dim(arr_kerinput)
    dim_arrindex <- dim(arr_index)
    dim(arr_kerinput) <- c(dim_arrkerinp[1], prod(dim_arrkerinp[2:3]))
    dim(arr_index) <- c(dim_arrindex[1], prod(dim_arrindex[2:3]))
    
    return (list(value = arr_kerinput, index = arr_index))
}