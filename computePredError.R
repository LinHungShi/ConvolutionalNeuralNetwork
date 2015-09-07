computePredError <- function(y, pred, actfun){
    
    result <- 0
    if(actfun == 'softmax'){
        result <- sum(-hadamard.prod(y, log(pred)))
    }
    else if(actfun == 'regression'){
        result <- sum((pred - y)^2) / 2
    }
    return (result)
}