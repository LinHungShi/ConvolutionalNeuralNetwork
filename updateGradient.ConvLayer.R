updateGradient.ConvLayer <- function(layer, n_layer, alpha){
    
    n_type <- n_layer$type
    n_input <- n_layer$input
    actfun <- layer$actfun
    der_ninput <- diffActFunction(n_input, actfun)
    n_weight <- n_layer$weight
    n_grad <- n_layer$grad
    
    
    if(n_type == 'Hid'){
        
        grad <- hadamard(der_ninput, (n_grad %*% t(n_weight)))
        return (grad)
        
    }else if(n_type == 'Conv'){
        
        n_index <- n_layer$input_index
        n_numfp <- n_layer$num_fp
        output_dim <- dim(layer$output)
        temp <- 0
        browser()
        for(i in 1:n_numfp){
            temp <- temp + t(t(der_ninput * n_grad[, i]) * n_weight[,i])
        }
        c_grad <- aggBasedonMatrix(temp, n_index,'+')
        grad <- matrix(c_grad, nrow = output_dim[1])
        return (grad)
    
    }else if(n_type == 'MP'){
        
        max_loc <- n_layer$max_loc
        num_fp <- layer$num_fp
        grad <- numeric(length(der_ninput))
        grad[] <- 0
        sapply(num_fp, function(fp){
            locs <- max_loc[,fp]
            delta <- n_grad[, num_fp] * der_ninput[locs]
            grad[locs]  <<- grad[locs] + delta
        })
        output_dim <- dim(layer$output)
        grad <- matrix(grad, nrow = output_dim[1])
        return (grad)
    }
}