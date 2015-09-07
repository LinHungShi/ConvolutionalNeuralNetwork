predictNN <- function(x, x_dim, layers, batch){
    updated_layer <- feedForward(x = x, x_dim = x_dim, layers = layers, batch = batch)
    output <- updated_layer[[length(updated_layer)]]$output
    return (output)
}