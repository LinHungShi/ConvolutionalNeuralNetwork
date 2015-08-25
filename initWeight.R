initWeight <- function(weight_dim, method){
    
    switch(method,
           'rnorm' = matrix(rnorm(prod(weight_dim)), weight_dim[1], weight_dim[2]), 
           'runif' = matrix(runif(prod(weight_dim)), weight_dim[1], weight_dim[2])
    )
}