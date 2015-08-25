feedForward <- function(cnn, x, x_dim, batch){
    
    layers <- cnn$layer
 
    layer_name <- cnn$layer_name
    input <- x
    input_dim <- x_dim
    is.convfinish <- FALSE
    pre_numfp <- 1
    for(i in 1:length(layers)){
        if(toupper(layer_name[[i]]) %in% c('CONV','MP')){
            
            if(toupper(layer_name[i]) == 'CONV')
                window_dim <- layers[[i]]$kernel_dim
            else 
                window_dim <- layers[[i]]$neighbor_dim
            
             
            stride <- layers[[i]]$stride
            kernel_form <- transKerForm(input, input_dim, stride, window_dim, batch, pre_numfp)
            
            input_trans <- kernel_form$value
            
            #if(i == 3)browser()
            value <- updateValue(layers[[i]], input_trans)
          
            cnn$layer[[i]]$value <- input
            input <- value
            input_dim <- getNewInpDim(input_dim, window_dim, stride)
            cnn$v_kerform[[i]] <- input_trans
                
        }else{
            
            if(is.convfinish == FALSE){
                input <- matrix(input, nrow = batch, byrow = TRUE)
                is.convfinish = TRUE
            }
            
            value <- updateValue(layers[[i]], input)
            cnn$layer[[i]]$value <- value
            input <- value
            
        }
        pre_numfp <- layers[[i]]$num_fp
       
    }
    return (cnn)
}