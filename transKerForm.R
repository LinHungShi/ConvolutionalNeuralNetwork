transKerForm <- function(inputs, input_dim, stride, window_dim, batch, num_fp){
    
    library(plyr)
    x <- t(inputs)
    arr_form  <- array(x, dim = c(input_dim[1], input_dim[2], num_fp, batch))
    index_mat <- array(1:length(x), dim = dim(arr_form))
    arr_kerinput <- aaply(arr_form, 4, patch3dArr, window = window_dim, stride = stride, .drop = FALSE)
    arr_index <- aaply(index_mat, 4, patch3dArr, window = window_dim, stride = stride, .drop = FALSE)
    arr_kerinput <- aperm(arr_kerinput, perm = c(2,3,1))
    arr_index <- aperm(arr_index, perm = c(2,3,1))
    input_mat <- matrix(arr_kerinput, ncol = dim(arr_kerinput)[1], byrow = TRUE)
    index_mat <- matrix(arr_index, ncol = dim(arr_index)[1], byrow = TRUE)
    
    return (list(value = input_mat, index = index_mat))
}