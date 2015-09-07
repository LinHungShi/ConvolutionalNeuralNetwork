updateWgradient.OutputLayer <- function(layer){

    input  <- layer$input
    grad <- layer$grad
    inp_dim <- dim(input)[2]
    batch <- dim(input)[1]
    w_grad <- layer$weight
    w_grad[] <- 0
    for(i in 1:inp_dim){
        w_grad[i, ]  <- input[,i] %*% grad
    }
    return (w_grad)
}