updateWeight_ <- function(weight, w_grad, alpha){

    result <- weight - w_grad * alpha
    return (result)
}