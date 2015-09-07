diffActFunction <- function(value, actfun){
    if(actfun == 'sigmoid')
        value <- value * (1 - value)
    else if(actfun == 'tanh')
        value <- 1 - value^2
    else if(actfun == 'regression')
        value <- 1
    return (value)
}