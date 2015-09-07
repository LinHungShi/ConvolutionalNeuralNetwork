updateWeight.MPLayer <- function(layer, n_layer, alpha){
    
    input <- layer$input
    output <- layer$output
    n_grad <- n_layer$grad
    layer$grad <- updateGradient(layer, n_layer)
    layer$w_grad <- updateWgradient(layer)
    #layer$weight <- updateWeight(layer$weight, layer$w_grad, alpha)
    return (layer)
}