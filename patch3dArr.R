patch3dArr <- function(arr_3d, window, stride){
    
    if(is.matrix(arr_3d))
        dim(arr_3d) <- c(dim(arr_3d), 1)
    dim_arr <- dim(arr_3d)
    dim_row <- dim_arr[1]
    dim_col <- dim_arr[2]
    num_fp <- dim_arr[3]
    row_interval <- c(seq(window[1], dim_row, stride[1]))
    col_interval <- c(seq(window[2], dim_col, stride[2]))

    trans_mat <- matrix(NA, prod(window) * num_fp, length(col_interval) * length(row_interval))
    index <- 1
    for(col in col_interval){
        for(row in row_interval){
            trans_mat[, index] <- as.vector(arr_3d[(row - window[1] + 1):row, (col - window[2] + 1):col, ])
            
            index  <- index + 1
        }
    }
    trans_mat
}