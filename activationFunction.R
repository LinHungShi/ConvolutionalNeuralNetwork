activationFunction <- function(value, actFun){
    if(actFun == 'sigmoid')
        value <- 1/(exp(-value) + 1)
    else if(actFun == 'tanh')
        value <- tanh(value)
    return (value)
}