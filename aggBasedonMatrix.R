aggBasedonMatrix <- function(eval_mat, base_mat, op){
    result <- rep(0, length(unique(c(base_mat))))
    num_col <- ncol(eval_mat)
    
    sapply(1:num_col, function(col){
        
        index <- base_mat[,col]  
        
        result[index] <<- (eval(call(op, result[index], eval_mat[,col])))
        
    })
    return (result)
}