updateValue.ConvLayer <- function(layer, input, input_dim, batch, pre_numfp, simplify){
#     library(plyr)
#     x <- t(input)arr
#     input_dim <- layer$input_dim
#     batch <- layer$batch
#     pre_numfp <- layer$pre_numfp
#     stride <- layer$stride
#     dim_kernel <- layer$dim_kernel
#     arr_form  <- array(x, dim = c(input_dim[1], input_dim[2],batch, pre_numfp))
#     arr_kerinput <- aaply(arr_form, 3, patch3dArr, window = dim_kernel, stride = stride)
#     arr_kerinput <- aperm(arr_kerinput, perm = c(2,3,1))
#     dim_arrkerinp <- dim(arr_kerinput)
#     dim(arr_kerinput) <- c(dim_arrkerinp, prod(dim_arrkerinp[2:3]))
    stride <- layer$stride
    kernel_dim <- layer$kernel_dim
    arr_form <- transKerForm(input, input_dim, stride, kernel_dim, batch, pre_numfp)
    layer$input <- arr_form$value
    layer$input_index <- arr_form$index
    layer$is.update <- FALSE
    layer$output <- updateOutput(layer, simplify)
    layer$is.update <- TRUE
    return (layer)
    
}