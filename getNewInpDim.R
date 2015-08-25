cgetNewInpDim <- function(input_dim, window_dim, stride){
    return ((input_dim - window_dim ) / stride + 1)
}