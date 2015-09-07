updateWeight.HidLayer <- function(layer, n_layer, alpha){
    
    grad <- n_layer$grad
    layer$grad <- updateGradient(layer, n_layer)
    layer$w_grad <- updateWgradient(layer)
    layer$weight <- updateWeight_(layer$weight, layer$w_grad, alpha)
    return (layer)
}