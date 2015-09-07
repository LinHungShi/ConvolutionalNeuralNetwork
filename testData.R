
batch <- 3
x_dim <- c(32,32)
structure <- c('CONV','MP','CONV','MP','H','O')
num_fp <- c(10, 5)
x <- matrix(rnorm(prod(x_dim) * batch), nrow = batch, byrow = TRUE)
y <- c(1,2,3)
num_out_type <- length(unique(y))
conv_kernel <- list(c(5,5), c(2,2))
conv_stride <- list(c(1,1), c(2,2))
conv_overlap <- c(FALSE, TRUE)
pool_neighbor <- list(c(2,2), c(7,7))
pool_stride <- list(c(2,2), c(1,1))
pool_overlap <- c(TRUE, FALSE)
hid <- c(30)
init_method <- 'rnorm'
actfun <- 'sigmoid'
outfun <- 'softmax'
lrate <- 0.5

layers <- createLayers(structure, num_out_type, x_dim, num_fp, conv_kernel, conv_overlap, conv_stride, pool_neighbor, pool_overlap, pool_stride, hid, init_method, actFun, outFun)
cnn <- Cnn(layers, x, x_dim, y, FALSE, lrate, batch)
cnn <- feedForward(cnn)
updated_layers <- cnn$layers
updated_layers$layer[[2]]$max_loc
head(updated_layers$layer[[2]]$input)
dim(updated_layers$layer[[2]]$input)
dim(updated_layers$layer[[1]]$weight)
