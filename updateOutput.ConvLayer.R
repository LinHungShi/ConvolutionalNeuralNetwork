updateOutput.ConvLayer <- function(layer,simplify){
    
    if(layer$is.update == TRUE)
        return (layer$output)
    
    actfun <- layer$actfun
    weight <- layer$weight
    input <- layer$input
    value <- input %*% weight
    value <- actFunction(value, actfun)
    #value <- simplifyOutput(value)
    return (value)
}