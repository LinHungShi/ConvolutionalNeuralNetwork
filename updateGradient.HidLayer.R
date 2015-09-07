updateGradient.HidLayer <- function(layer, n_layer){
    ##browser()
    output <- layer$output
    actfun <- layer$actfun
    der_output <- diffActFunction(output, actfun)
    n_weight <- n_layer$weight
    n_grad <- n_layer$grad
    grad <- hadamard.prod(output, (n_grad %*% t(n_weight)))
    return (grad)
}