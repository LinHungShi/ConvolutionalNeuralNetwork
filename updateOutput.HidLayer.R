updateOutput.HidLayer <- function(layer){
    
    if(layer$is.update == TRUE)
        return (layer$output)
   
    weight <- layer$weight
    input <- layer$input
    value <- input %*% weight
    actfun <- layer$actfun
    value <- actFunction(value, actfun)
    return (value)
}