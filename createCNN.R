createCNN <- function(structure, x_dim, num_fps, conv_kernel, conv_overlap, conv_stride, pool_neighbor, pool_overlap, pool_stride, hid, init_method, actFun, outFun){
    
    layers <- list()
    layers$layer <- list()
    layers$layer_name <- structure
    conv_index <- 1
    pool_index <- 1
    hid_index <- 1
    pre_numfp <- 1
    input_dim <- x_dim
    for(i in seq_along(structure)){
        
        layer_type <- structure[i]
        
        if(toupper(layer_type) == 'CONV'){
           
            kernel_dim <- conv_kernel[[conv_index]]
            overlap <- conv_overlap[conv_index]
            
            if(overlap == FALSE)
                stride <- conv_stride[conv_index]
            else
                stride <- kernel_dim[1]
            
            if(!checkDimConsistency(input_dim, kernel_dim, stride))
                stop('combination of kernel size and stride does not fit input size')
            
            num_fp <- num_fps[conv_index]
            
            
            layer <- ConvLayer(input_dim, pre_numfp, num_fp, kernel_dim, init_method, actFun, stride)
            input_dim <- getNewInpDim(input_dim, kernel_dim, stride)
            pre_numfp <- num_fp
            conv_index <- conv_index + 1
            
        }else if(toupper(layer_type) == 'MP'){
            
            if(i == 1)
                num_fp <- numfps[conv_index - 1]
            else
                num_fp <- pre_numfp
            
            neighbor_dim <- pool_neighbor[[pool_index]]
            overlap <- pool_overlap[pool_index]
            
            if(overlap == FALSE)
                stride <- pool_stride[pool_index]
            else
                stride <- neighbor_dim[1]
            
            if(!checkDimConsistency(input_dim, neighbor_dim, stride))
                stop('combination of neighbor size and strude does not fit input size')    
        
            layer <- MPLayer(input_dim, num_fp, neighbor_dim, stride)
            input_dim <- getNewInpDim(input_dim, neighbor_dim, stride)
            
            pool_index <- pool_index + 1
            
        }else if(toupper(layer_type) == 'H'){
           
            neurons <- hid[hid_index]
            layer <- HidLayer(neurons,pre_numfp, init_method, actFun)
            input_dim <- neurons
            hid_index <- hid_index + 1
        }else if(toupper(layer_type) == 'O'){
            
            num_out_type <- length(unique(y))
            if(num_out_type <= 1)
                stop('output type has only one value')
            else if(num_out_type == 2)
                layer <- OutputLayer(1, input_dim, init_method, outFun)
            else
                layer <- OutputLayer(num_out_type, input_dim, init_method, outFun)
        }
        else stop(paste('Does not provide',layer_type,'layer', sep = ' '))
        print(layer_type)
        layers$layer[[i]] <- layer    
    }
    class(layers) <- 'CNN'
    return (layers)
}