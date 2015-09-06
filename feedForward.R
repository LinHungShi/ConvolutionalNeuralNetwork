feedForward <- function(cnn){
    
    layers <- cnn$layer
    layer_name <- cnn$layer_name
    input <- cnn$x
    input_dim <- cnn$x_dim
    pre_numfp <- 1
    simplify <- TRUE
    
    for(i in 1:length(layers)){
        
        if(layer_name[[i]] %in% c('CONV','MP')){
            
            layers[[i]] <- updateValue(layers[[i]], input, input_dim, batch, pre_numfp, simplify)
            stride <- layers[[i]]$stride
            
            if(layer_name[i] == 'CONV')
                window_dim <- layers[[i]]$kernel_dim
            else 
                window_dim <- layers[[i]]$neighbor_dim
            
            input_dim <- getNewInpDim(input_dim, window_dim, stride)
            input <- layers[[i]]$output
            
        }else{
            
            layers[[i]] <- updateValue(layers[[i]], input)  
            input <- layers[[i]]$output
            
        }
       
    }
    
    output <- input
    cnn$layers <- layers
    cnn$output <- output
    return (cnn)
}