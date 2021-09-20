#'Linear Regression
#'
#'\code{linreg} performs linear regression, stores its result and offers methods for analysis.
#'
#'\code{linreg} is an RC class. Upon object instantiation, in the \code{initialize} method, it performs
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
#'
#'
#' @export
linreg = setRefClass(
  Class = "linreg",
  fields = c("coefs",
             "y_est",
             "residuals",
             "degrees_freedom",
             "residual_var",
             "var_coef",
             "t_coef",
             "p_coef",
             "local_formula",
             "local_data_name"),
  methods = list(
    initialize = function(formula, data){
      local_formula <<- formula
      local_data_name <<- deparse(substitute(data))
      X = model.matrix(object = formula, data = data) #extracts the model matrix, that is, the matrix originated from writing the problem as a set of linear equations
      y = data[[all.vars(formula)[[1]]]] #picks the name from the formula that is not in the columns of X and identifies it as the name of y. picks y from the data with this name
      coefs <<- drop(solve((t(X) %*% X), (t(X) %*% y))) #solves X'X coefs = X' y
      y_est <<- drop(X %*% coefs) #calculates estimated y
      residuals <<- y - y_est # calculates the residuals
      n_size = nrow(X)
      p_size = ncol(X)
      degrees_freedom <<- n_size - p_size
      residual_var <<- drop((t(residuals) %*% residuals) / degrees_freedom) #calculates the residual variance
      temp_var_coef = solve((t(X) %*% X), diag(ncol(X))) #gets the inverse of X'X by solving for the identity as a result
      colnames(temp_var_coef) = rownames(temp_var_coef)
      var_coef <<- residual_var * diag(temp_var_coef) #calculates the variance of the coefficients
      t_coef <<- coefs / sqrt(var_coef) #calculates t-values
      p_coef <<- 2*(1-pt(abs(t_coef), df=degrees_freedom)) #calculates p-values
      },
    print = function(){
      cat(paste("linreg(formula = ", deparse(local_formula), ", data = ", local_data_name, ")\n\n", sep = ""))
      base::print(coefs)
    },
    resid = function(){
      return(residuals)
    },
    pred = function(){
      return(y_est)
    },
    coef = function(){
      return(coefs)
    },
    summary = function(){
      cat(paste("Parameter", "Value", "Standard error", "T-Value", "P-Value", "\n", sep=" "))
      for (name in names(coefs)){
        cat(paste(name, coefs[[name]], sqrt(var_coef[[name]]), t_coef[[name]], p_coef[[name]], "***\n", sep=" "))
      }
      cat(paste("Residual standard error: ", sqrt(residual_var), " on ", degrees_freedom, " degrees of freedom\n", sep = ""))
    },
    plot = function(){
      #TODO
      }
  )
)