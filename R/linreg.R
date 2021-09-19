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
      variable_names = all.vars(formula) #all variables names in the formula
      y = data[[variable_names[which(!(all.vars(formula) %in% colnames(X)))]]] #picks the name from the formula that is not in the columns of X and identifies it as the name of y. picks y from the data with this name
      coefs <<- drop(solve((t(X) %*% X), (t(X) %*% y))) #solves X'X coefs = X' y
      y_est <<- drop(X %*% coefs) #calculates estimated y
      residuals <<- y - y_est # calculates the residuals
      n_size = nrow(X)
      p_size = ncol(X)
      degrees_freedom <<- n_size - p_size
      residual_var <<- drop((t(residuals) %*% residuals) / degrees_freedom) #calculates the residual variance
      var_coef = solve((t(X) %*% X), diag(ncol(X))) #gets the inverse of X'X by solving for the identity as a result
      colnames(var_coef) = rownames(var_coef)
      var_coef <<- residual_var * diag(var_coef) #calculates the variance of the coefficients
      t_coef <<- coefs / sqrt(var_coef) #calculates t-values
      p_coef <<- t_coef #TODO!!!!!! #calculates p-values
      },
    print = function(){
      base::cat(paste("linreg(formula = ", deparse(local_formula), ", data = ", local_data_name, ")\n", sep = ""))
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
      #TODO
    },
    plot = function(){
      #TODO
    }
  )
)