ConvLayer <- function(input_dim, pre_numfp, num_fp, kernel_dim, init_method, actfun, stride){
    
    #convlayer$input_size <- input_dim
    #convlayer$batch <- batch
    #convlayer$pre_numfp
    #convlayer$crosskernel_dim <- crosskernel_dim
    convlayer <- Layer(actfun)
    crosskernel_dim <- c(pre_numfp * prod(kernel_dim), num_fp)
    convlayer$weight <- initWeight(crosskernel_dim, init_method)
    convlayer$num_fp <- num_fp
    convlayer$kernel_dim <- kernel_dim
    convlayer$stride <- stride
    convlayer$w_grad <- NA
    convlayer$grad <- NA
    convlayer$input_index <- NA
    convlayer$type <- 'Conv'
    class(convlayer) <- c(class(convlayer), 'ConvLayer')
    return (convlayer)
}