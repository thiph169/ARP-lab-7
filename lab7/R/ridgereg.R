#' Ridge Regression
#'
#' @field formula formula. 
#' @field data data.frame. 
#' @field lamda numeric. 
#' @field beta_hat_ridge matrix. 
#' @field y_hat numeric. 
#' @field coef_ridge numeric. 
#' @field data_name character. 
#' 
#' @return Regression coefficent beta hat and fitted values.
#' 
#' @export 
#' 
#' @import methods
#'
# examples
# data(iris)
# ridgereg$new(Petal.Length~Species, data = iris, lamda = 0)$print()
# ridgereg$new(Petal.Length~Species, data = iris, lamda = 0)$predict()
# ridgereg$new(Petal.Length~Species, data = iris, lamda = 0)$coef()

ridgereg <- setRefClass(
     Class = "ridgereg",
     fields = list(
       formula = "formula",
       data = "data.frame",
       lamda = "numeric",
       beta_hat_ridge = "matrix",
       y_hat = "numeric",
       coef_ridge ="numeric",
       data_name = "character"),
    
      methods = list(
        initialize = function(formula, data, lamda, normalize = FALSE){
          formula <<- formula
          data <<- data
          lamda <<- lamda
          
          # Define matrix
          X <- model.matrix(formula, data)
          
          # Dependant variable 
          y <- data[[(all.vars(formula)[1])]]
          
          #So normalize all covariates
          if(normalize == TRUE)
            for(i in 2:ncol(X))
              X[,i] <- (X[,i] - mean(X[,i])) / sd(X[,i])
          
          #Create an indentical matrix
          identical_mat <- matrix(c(0),nrow = ncol(X),ncol = ncol(X))
          
          #Contruct a diagonal matrix
          diag(identical_mat) <- sqrt(lamda)
          y_matrix <- as.matrix(y)
          
          #Calculate beta_hat_ridge
          beta_hat_ridge <<- solve(((t(X) %*% X) + identical_mat)) %*% (t(X) %*% y_matrix)
          
          #Calculate y_hat
          y_hat <<- as.numeric(X %*% beta_hat_ridge)
          
          coef_ridge <<- as.numeric(beta_hat_ridge)
          names(coef_ridge) <<- rownames(beta_hat_ridge)
          data_name <<- deparse(substitute(data))
          
          },
        
          #Build print function
           print = function(){
            
               cat(sep = "\n")
               cat("Call:")
               cat(sep = "\n")
               cat(paste("ridgereg(", "formula = ", formula[2], " ", formula[1], " ", formula[3], ", ", "data = ", data_name, ")", sep = "" ))
              
               cat(sep = "\n")
               cat(sep = "\n")
               cat("Coefficients:")
               
               cat(sep = "\n")
               cat(rownames(as.data.frame(coef_ridge)))
             
               cat("\n")
               cat(coef_ridge)
               cat("\n")
               
           },
        
           # Build predict function
           predict = function(){
             return(y_hat)
           },
        
           # Build coef function
           coef = function(){
             return(coef_ridge)
             
           }
          
          
      )
)

