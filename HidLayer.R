HidLayer <- function(num_neuron, input_dim, init_method, actFun){
    #browser()
    hidlayer <- list()
    hidlayer$num_neuron <- num_neuron
    hidlayer$weight <- initWeight(c(input_dim,num_neuron), init_method)
    
    hidlayer$value <- 
    hidlayer$grad <- NA
    hidlayer$actFun <- actFun
    class(hidlayer) <- 'HidLayer'
    hidlayer
}