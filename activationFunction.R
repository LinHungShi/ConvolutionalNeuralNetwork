actFunction <- function(value, actfun){
    if(actfun == 'sigmoid')
        value <- 1/(exp(-value) + 1)
    else if(actfun == 'tanh')
        value <- tanh(value)
    else if(actfun == 'regression')
        value <- value
    return (value)
}