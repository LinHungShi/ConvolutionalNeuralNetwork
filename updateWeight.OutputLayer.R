updateWeight.OutputLayer <- function(layer, y, alpha){
    
    actfun <- layer$actfun
    layer$grad <- updateGradient(layer, y, actfun)
    layer$w_grad <- updateWgradient(layer)
    layer$weight <- updateWeight_(layer$weight, layer$w_grad, alpha)
    return (layer)
}