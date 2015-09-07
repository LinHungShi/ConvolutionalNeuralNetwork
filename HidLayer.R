HidLayer <- function(num_neuron, input_dim, init_method, actfun){
    #browser()
    hidlayer <- Layer(actfun)
    hidlayer$num_neuron <- num_neuron
    hidlayer$weight <- initWeight(c(input_dim,num_neuron), init_method)
    hidlayer$grad <- NA
    hidlayer$w_grad <- NA
    hidlayer$type <- 'Hid'
    class(hidlayer) <- c(class(hidlayer), 'HidLayer')
    return (hidlayer)
}