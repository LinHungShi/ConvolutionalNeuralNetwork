updateValue.OutputLayer <- function(layer, input){

    layer$input <- input
    layer$is.update <- FALSE
    layer$output <- updateOutput(layer)
    layer$is.update <- TRUE
    return (layer)
}