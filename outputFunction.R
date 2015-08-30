outFunction <- function(value, outFun){
    if(outFun == 'sigmoid')
        return (1 / (exp(-value) + 1 ))
    else if(outFun == 'softmax')
        return (exp(value) / rowSums(exp(value)))
}