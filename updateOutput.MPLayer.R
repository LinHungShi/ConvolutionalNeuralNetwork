updateOutput.MPLayer <- function(layer, simplify){
    if(layer$is.update == TRUE)
        return (layer$output)
    num_fp <- layer$num_fp
    input <- layer$input
    ninput_dim <- dim(input)
    neighbor_dim <- ninput_dim[2] / num_fp
    pre_value <- input 
    value <- matrix(NA, nrow = ninput_dim[1], ncol = num_fp)
    index <- matrix(NA, nrow = ninput_dim[1], ncol = 2)
    index[,1] <- 1:ninput_dim[1]
    max_loc <- value
    input_index <- layer$input_index
    for(fp_index in 0:(num_fp - 1)){
        sta.row <- fp_index * neighbor_dim + 1
        fin.row <- (fp_index + 1) * neighbor_dim
        value[, fp_index + 1] <- apply(pre_value[, sta.row:fin.row], 1, max)
        index[, 2]  <- apply(pre_value[, sta.row:fin.row], 1, function(x) which.max(x))
        max_loc[, fp_index + 1] <- input_index[, sta.row:fin.row][index]
    }
    actfun <- layer$actfun
    value <- actFunction(value, actfun)
    return (list(value = value, max_loc = max_loc))
}