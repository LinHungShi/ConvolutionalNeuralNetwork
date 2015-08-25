updateValue.OutputLayer <- function(layer, input){
    weight <-layer$weight
    value <- input %*% weight
    value <- outputFunction(value, layer$outFun)
    return (value)
}