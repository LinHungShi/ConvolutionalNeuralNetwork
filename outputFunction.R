outfunction <- function(value, outfun){
 
    if(outfun == 'softmax')
        return (exp(value) / rowSums(exp(value)))
    else if(outfun == 'regression')
        return (value)
}