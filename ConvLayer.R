ConvLayer <- function(input_dim, pre_numfp, num_fp, kernel_dim, init_method, actFun, stride){
    
    convlayer <- list()
   
    crosskernel_dim <- c(num_fp, pre_numfp * prod(kernel_dim))
    
    #convlayer$input_size <- input_dim
    #convlayer$batch <- batch
    #convlayer$pre_numfp
    convlayer$num_fp <- num_fp
    convlayer$weight <- initWeight(crosskernel_dim, init_method)
    convlayer$kernel_dim <- kernel_dim
    convlayer$crosskernel_dim <- crosskernel_dim
    convlayer$stride <- stride
    convlayer$w_grad <- NA
    convlayer$w_grad[] <- NA
    convlayer$value <- NA
    convlayer$grad <- NA
    convlayer$actFun <- actFun
    class(convlayer) <- 'ConvLayer'
    return (convlayer)
}