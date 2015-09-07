updateGradient.OutputLayer <- function(layer, y, actfun){

# n: index of observations
# k: index of classes
    
# Entropy:
#     sigma(k) t_k * ln(y_k)
# softmax:
#     e(a_j) / sigma(k) e(a_k)

# Regression:
#     (1/2) * sigma(n) (t_n - y_n) ^ 2
    
# square_error:
#     (t_n - y_n)
    
    output <- layer$output
    result <- NA
    if(actfun == 'softmax')
        result <- output - y
    else if(actfun == 'regression')
        result <- -(y - output)
    else
        stop('No such actfun')
    return (result)
}