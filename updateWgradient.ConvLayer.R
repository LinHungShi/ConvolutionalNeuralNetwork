updateWgradient.ConvLayer <- function(layer){
    
    num_fp <- layer$num_fp
    output <- layer$output
    input <- layer$input
    batch <- dim(output)[1]
    w_grad <- layer$weight
    w_grad[] <- 0
    for(i in 1:num_fp){
        
        w_grad[, i] <- colSums(output[,i] * input)
    }
    return (w_grad)
}