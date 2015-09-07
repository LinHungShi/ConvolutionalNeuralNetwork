createLayers <- function(structure, num_out_type, x_dim, num_fps, conv_kernel, conv_overlap, conv_stride, pool_neighbor, pool_overlap, pool_stride, hid, init_method, actfun, outfun){
    
    layers <- list()
    layers$layer <- list()
    layers$layer_name <- structure
    conv_index <- 1
    pool_index <- 1
    hid_index <- 1
    pre_numfp <- 1
    mp_actfun <- 'identity'
    input_dim <- x_dim
    for(i in seq_along(structure)){
        
        layer_type <- structure[i]
      
        if(layer_type == 'CONV'){
           
            kernel_dim <- conv_kernel[[conv_index]]
            overlap <- conv_overlap[conv_index]
            
            if(overlap == FALSE)
                stride <- conv_stride[[conv_index]]
            else
                stride <- kernel_dim
            
            if(!checkDimConsistency(input_dim, kernel_dim, stride))
                stop('combination of kernel size and stride does not fit input size')
            
            num_fp <- num_fps[conv_index]
            layer <- ConvLayer(input_dim, pre_numfp, num_fp, kernel_dim, init_method, actfun, stride)
            input_dim <- getNewInpDim(input_dim, kernel_dim, stride)
            pre_numfp <- num_fp
            conv_index <- conv_index + 1
            
        }else if(layer_type == 'MP'){
          
            if(i == 1)
                num_fp <- 1
            else
                num_fp <- pre_numfp
            
            neighbor_dim <- pool_neighbor[[pool_index]]
            overlap <- pool_overlap[pool_index]
            
            if(overlap == FALSE)
                stride <- pool_stride[[pool_index]]
            else
                stride <- neighbor_dim
            
            if(!checkDimConsistency(input_dim, neighbor_dim, stride))
                stop('combination of neighbor size and strude does not fit input size')    
        
            layer <- MPLayer(input_dim, num_fp, neighbor_dim, stride, mp_actfun)
            input_dim <- getNewInpDim(input_dim, neighbor_dim, stride)
            pool_index <- pool_index + 1
            
        }else if(layer_type == 'H'){
            
            input_dim <- pre_numfp
            neurons <- hid[hid_index]
            layer <- HidLayer(neurons,input_dim, init_method, actfun)
            pre_numfp <- neurons
            hid_index <- hid_index + 1
            
        }else if(toupper(layer_type) == 'O'){
            
            input_dim <- pre_numfp
     
            if(num_out_type == 1 && outfun == 'identity')
                layer <- OutputLayer(1, input_dim, init_method, outfun)
            else if(num_out_type == 1 && outfun != 'identity')
                stop('class type has only one value')
            else
                layer <- OutputLayer(num_out_type, input_dim, init_method, outfun)
               
        }
        
        else stop(paste('Does not provide',layer_type,'layer', sep = ' '))
   
        layers$layer[[i]] <- layer    
    }
    class(layers) <- 'CNN'
    return (layers)
}