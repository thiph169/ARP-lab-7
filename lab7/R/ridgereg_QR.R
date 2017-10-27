#' Ridge Regression - QR decomposition method
#'
#' @field formula formula. 
#' @field data data.frame. 
#' @field lambda numeric. 
#' @field data_name character. 
#' @field identity_matrix matrix. 
#' @field beta_hat_ridge_QR matrix. 
#' @field y_hat_ridge_QR matrix. 
#'
#' @return returns regression coeffiecient beta hat and fitted values.
#' @export
#' 
#'
#Examples:
# data(iris)
# ridgereg_QR$new(Petal.Length~Species,data=iris,lambda=0)$print_QR()
# ridgereg_QR$new(Petal.Length~Species,data=iris,lambda=0)$predict_QR()
# ridgereg_QR$new(Petal.Length~Species,data=iris,lambda=0)$coef_QR()

ridgereg_QR <- setRefClass( 
  Class = "ridgereg_QR",
  fields = list(
    formula = "formula",
    data = "data.frame",
    lambda = "numeric",
    data_name = "character",
    identity_matrix="matrix",
    beta_hat_ridge_QR ="matrix",
    y_hat_ridge_QR="matrix"             
    ),
  
  methods=list(
    initialize = function(formula, data, lambda, normalise = TRUE){
      formula  <<- formula
      data <<- data
      data_name <<- deparse(substitute(data))
      lambda <<- lambda
      
      x <- model.matrix(formula, data)
      x <- ((x[,-1]-mean(x[,-1])) / sd(x[,-1]))
      
      x <- x-mean(x)/sd(x)
      x_norm <- x
      yy  <- all.vars(expr = formula)[1]
      y <- (data[, yy])
      identity_matrix <<- diag(ncol(x_norm))# x_norm %*% (t(x_norm))  
      
      
      #using QR decompositon
      QR_ridge<- qr(x_norm)
      R <- qr.R(QR_ridge)
      Q <- qr.Q(QR_ridge)
      RR<- ((R %*% t(Q) %*% Q) + lambda *(t(Q) %*% Q %*% solve(t(R))))
      
      beta_hat_ridge_QR <<- backsolve(RR, crossprod(Q,y))
      y_hat_ridge_QR <<- (x_norm %*% beta_hat_ridge_QR)
    },     
 
    print_QR = function(){
      cat(sep = "\n")
      cat("Call:")
      cat(sep = "\n")
      cat(paste("ridgereg_QR(", "formula = ", formula[2], " ", formula[1], " ", formula[3], ", ", "data = ", data_name, ")", sep = "" ))
      
      cat(sep = "\n")
      cat(sep = "\n")
      cat("Coefficients:")
      
      cat(sep = "\n")
      cat(rownames(as.data.frame(beta_hat_ridge_QR)))
      
      cat("\n")
      cat(beta_hat_ridge_QR)
      cat("\n")

      
    },
    
    predict_QR = function(){
      cat("\n \n Predicted values or fitted values using QR decomposition:","\n\n")
      return(as.vector(round(y_hat_ridge_QR, 2)))
    },
    
    coef_QR = function(){
      return (beta_hat_ridge_QR)
    }
    
  )
)


