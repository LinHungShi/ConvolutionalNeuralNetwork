OutputLayer <- function(num_neuron, input_dim, init_method, outFun){
    outlayer <- list()
    outlayer$num_neuron <- num_neuron
    outlayer$weight <- initWeight(c(input_dim, num_neuron), init_method)
    outlayer$value <- NA
    outlayer$grad <- NA
    outlayer$outFun <- outFun
    class(outlayer) <- 'OutputLayer'
    outlayer
}