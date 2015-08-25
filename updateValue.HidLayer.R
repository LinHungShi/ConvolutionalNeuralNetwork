updateValue.HidLayer <- function(layer, input){
    weight <- layer$weight
    value <- input %*% weight
    value <- activationFunction(value, layer$actFun)
    return (value)
}