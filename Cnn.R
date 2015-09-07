Cnn <- function(layers, x, x_dim, y, adapt, lrate, init_batch){
    cnn <- list()
    cnn$x <- x
    cnn$x_dim <- x_dim
    cnn$y <- y
    cnn$predict <- NA
    cnn$batch <- list()
    cnn$batch$init_batch <- init_batch
    cnn$batch$curr_batch <- init_batch
    cnn$adapt <- adapt
    cnn$error <- NA
    cnn$layers <- layers
    cnn$lrate <- lrate
    class(cnn) <- 'CNN'
    return (cnn)
}