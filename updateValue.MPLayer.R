updateValue.MPLayer <- function(layer, input, input_dim, batch, pre_numfp, simplify){
    
    stride <- layer$stride
    neighbor_dim <- layer$neighbor_dim
    arr_form<- transKerForm(input, input_dim, stride, neighbor_dim, batch, pre_numfp)
    layer$input <- arr_form$value
    layer$input_index <- arr_form$index
    layer$is.update <- FALSE
    result<- updateOutput.MPLayer(layer, simplify)
    layer$output <- result$value
    layer$max_loc <- result$max_loc
    layer$is.update <- TRUE
    return (layer)
}