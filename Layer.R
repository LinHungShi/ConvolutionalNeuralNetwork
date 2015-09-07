Layer <- function(actfun){
    layer <- list()
    layer$input <- NA
    layer$output <- NA
    layer$is.update <- TRUE
    layer$type <- NA
    layer$actfun <- actfun
    class(layer) <- 'Layer'
    return (layer)
}