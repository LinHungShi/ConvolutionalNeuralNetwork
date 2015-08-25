MPLayer <- function(input_dim, num_fp, neighbor_dim, stride){
    value_dim <- getNewInpDim(input_dim, neighbor_dim, stride)
    mplayer <- list()
    mplayer$value <- NA
    mplayer$num_fp <- num_fp
    mplayer$pre_numfp <- num_fp
    mplayer$neighbor_dim <- neighbor_dim
    mplayer$stride <- stride
    mplayer$grad <- mplayer$value
    class(mplayer) <- 'MPLayer'
    mplayer 
}

    