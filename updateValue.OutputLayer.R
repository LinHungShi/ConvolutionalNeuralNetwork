updateValue.OutputLayer <- function(layer, input){
    weight <-layer$weight
    value <- input %*% weight
    value <- outFunction(value, layer$outFun)
    return (value)
}