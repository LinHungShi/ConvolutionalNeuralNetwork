backProp <- function(y, layers, alpha){
    
    #alpha <- cnn$lrate
    outlayer <- layers[[length(layers)]]
    #y <- cnn$y
    outlayer <- updateWeight(outlayer, y, alpha)
    layers[[length(layers)]] <- outlayer
    
    for(i in (length(layers) - 1):1){
        print(layers[[i]]$type)
        layers[[i]] <- updateWeight(layers[[i]], layers[[i + 1]], alpha)
    }
    layers$layer <- layers
    return (layers)
}