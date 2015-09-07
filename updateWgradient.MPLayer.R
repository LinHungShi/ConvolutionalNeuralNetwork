updateWgradient.MPLayer <- function(layer){
    
    num_fp <- layer$num_fp
    grad <- layer$grad
    input <- layer$input
    w_grad <- layer$weight
    w_grad[] <- 0
    w_grad <- t(t(grad) %*% input)
    
    return (w_grad)
}