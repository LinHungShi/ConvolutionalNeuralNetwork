updateValue.MPLayer <- function(layer, input){
    
    num_fp <- layer$num_fp
    ninput_dim <- dim(input)
    neighbor_dim <- ninput_dim[1] / num_fp
    
    pre_value <- input 
    value <- matrix(NA, num_fp, ninput_dim[2])
    for(fp_index in 0:(num_fp-1)){
        sta.row <- fp_index * neighbor_dim + 1
        fin.row <- (fp_index + 1) * neighbor_dim
        value[fp_index + 1, ] <- apply(pre_value[sta.row:fin.row, ], 2, max)
    }
    
    return (value)
}