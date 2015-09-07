Cnnet <- function(x, y, x_dim, alpha, batch, structure, num_fp, hid, conv_kernel, pool_neighbor, conv_overlap, pool_overlap, conv_stride, pool_stride, init_method, actfun, outfun){
    
    if(!is.matrix(x))
        stop('x must be a matrix')
    if(!is.numeric(x))
        stop('x must be a numeric matrix')
    if(!is.vector(y))
        stop('y must be a vector')
    if(!is.numeric(y))
        stop('y must be a numeric vector')
    if(nrow(x) != length(y))
        stop('dimensions of x and y arer inconsistent')
    
    
    num_hid <- length(hid)
    structure <- toupper(structure)
    structure <- c(structure, rep('H',num_hid),'O')
    layers <- createLayers(structure, x_dim, num_fp, conv_kernel, conv_overlap, conv_stride, pool_neighbor, pool_overlap, pool_stride, hid, init_method, actfun, outfun)
    cnn <- CNN(layers, x, x_dim, y, adapt, alpha, batch)
    result <- trainCnn(cnn, x, y, epison, batch, learning_rate, epoch)
    
}