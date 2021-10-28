#'Ridge Regression
#'
#'\code{ridgereg} performs linear regression, stores its result and offers methods for analysis.
#'
#'\code{ridgereg} is an RC class. Upon object instantiation, in the \code{initialize} method, it performs
#'all calculations of quantities of interest, namely estimated coefficients, fitted dependent variable values,
#'residuals, degrees of freedom of the system, residual variance, the variance of the coefficients,
#'t-values and p-values for each coefficient.
#'
#'Residuals, predicted values and coefficients can be obtained through the methods \code{resid()}, \code{pred()} and \code{coef()}.
#'
#'\code{print()} prints out the input information used in the regression and the resulting coefficients.
#'
#'\code{summary()} prints out statistics about the regression.
#'
#'\code{plot()} plots interesting graphics about the performed regression.
#'
#' @examples
#' data(iris)
#' regression_object = ridgereg(Petal.Length~Species, data = iris, lambda=1)
#' regression_object$pred()
#' regression_object$coef()
#' regression_object$print()
#' 
#' @source 
#' Read more at \url{https://en.wikipedia.org/wiki/Linear_regression}
#'
#' @export ridgereg

ridgereg = setRefClass(
  Class = "ridgereg",
  fields = c(  "coefs",
               "y_est",
               "local_data_name",
               "local_lambda"),
  methods = list(
    initialize = function(formula, data, lambda){
      
      formula<- Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length
      lambda = 1
      data = iris
      
      local_data_name <<- deparse(substitute(data))
      local_lambda<<- lambda

      my_data_x = model.matrix(object = formula, data = data)
      y = data[[all.vars(formula)[[1]]]]
      my_normalized_data = scale(my_data_x)
      X = as.matrix(my_normalized_data)[,-1]
      
      

      coefs <<-drop(solve((t(X)%*%X) + diag(rep(lambda, ncol(X)))) %*% (t(X)%*%y))
      beta_zero = mean(y)

      y_est <<- drop(X %*% coefs) #calculates estimated y

    },
    print = function(){
      cat(paste("ridgereg(formula = ", deparse(formula), ", data = ", local_data_name, ", lambda = ", local_lambda ,")\n\n", sep = ""))
      base::print(coefs)
    },
    predict = function(){
      return(y_est)
    },
    coef = function(){
      return(coefs)
    }
    
  )
)


# data(iris)
# mod_object_teste <- ridgereg(Petal.Length~Species, data = iris,lambda=0)
# mod_object_teste$coef()
# mod_object_teste <- ridgereg(Petal.Length~Species, data = iris,lambda=1)
# mod_object_teste$coef()
# mod_object_teste <- ridgereg(Petal.Length~Species, data = iris,lambda=2)
# mod_object_teste$coef()
# mod_object_teste <- ridgereg(Petal.Length~Species, data = iris,lambda=15)
# mod_object_teste$coef()
# 
# 
# 
# mod_object_teste$coef()
# mod_object_teste$predict()
# mod_object_teste$print()
