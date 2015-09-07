feedForward <- function(x, x_dim, layers, batch){
    
    #layer <- layers$layer
    input <- x
    input_dim <- x_dim
    pre_numfp <- 1
    simplify <- TRUE
    batch <- cnn$batch$curr_batch
    
    for(i in 1:length(layers)){
        
        layer_type <- layers[[i]]$type
        if(layer_type %in% c('Conv','MP')){
            
            layers[[i]] <- updateValue(layers[[i]], input, input_dim, batch, pre_numfp, simplify)
            stride <- layers[[i]]$stride
            
            if(layer_type == 'Conv')
                window_dim <- layers[[i]]$kernel_dim
            else 
                window_dim <- layers[[i]]$neighbor_dim
            
            input_dim <- getNewInpDim(input_dim, window_dim, stride)
            input <- layers[[i]]$output
            pre_numfp <- layers[[i]]$num_fp
            
        }else{
            
            layers[[i]] <- updateValue(layers[[i]], input)  
            input <- layers[[i]]$output
            
        }
       
    }
    
    output <- input
    
    return (layers)
}