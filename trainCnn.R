trainCnn <- function(x, y, x_dim, layers, alpha, batch, epoch){
    
    updated_layers <- layers
    num_obs <- nrow(x)
    error_func <- updated_layers[[length(updated_layers)]]$actfun
    obs <- 1
    for(ep in 1:epoch){
        num_iter <- ceiling(num_obs / batch)
        while(obs <= num_iter){
            
            sta.row <- (obs - 1) * batch + 1
            fin.row <- obs * batch
            fin.row <- ifelse(fin.row > num_obs, num_obs, fin.row)
            
            x_batch <- x[sta.row:fin.row, ]
            y_batch <- y[sta.row:fin.row]
            updated_layers <- feedForward(x = x_batch, x_dim = x_dim, layers = updated_layers, batch = batch)
            updated_layers <- backProp(y = y_batch, layers = updated_layers, alpha = alpha)
            obs <- obs + 1
        }
        browser()
        pred <- predictNN(x = x, x_dim = x_dim, layers = layers, batch = batch)
        error <- computePredError(y = y, pred = pred, actfun = error_func)
        print(error)
    }
}