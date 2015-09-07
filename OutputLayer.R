OutputLayer <- function(num_neuron, input_dim, init_method, outFun){
    outlayer <- Layer(outFun)
    outlayer$num_neuron <- num_neuron
    outlayer$weight <- initWeight(c(input_dim, num_neuron), init_method)
    outlayer$grad <- NA
    outlayer$type <- 'Output'
    outlayer$w_grad <- NA
    class(outlayer) <- c(class(outlayer), 'OutputLayer')
    return (outlayer)
}