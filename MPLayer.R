MPLayer <- function(input_dim, num_fp, neighbor_dim, stride, actfun){
    #value_dim <- getNewInpDim(input_dim, neighbor_dim, stride)
    mplayer <- Layer(actfun)
    mplayer$num_fp <- num_fp
    mplayer$neighbor_dim <- neighbor_dim
    mplayer$stride <- stride
    mplayer$grad <- NA
    mplayer$max_loc <- NA
    mplayer$input_index <- NA
    mplayer$type <- 'MP'
    class(mplayer) <- c(class(mplayer), 'MPLayer')
    return (mplayer)
}

    