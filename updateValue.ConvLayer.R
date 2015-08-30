updateValue.ConvLayer <- function(layer, input){
#     library(plyr)
#     x <- t(input)
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
    actFun <- layer$actFun
    weight <- layer$weight 
    value <- weight %*% input
    value <- actFunction(value, actFun)
    return (value)
    
}