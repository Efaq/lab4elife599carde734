
ridgereg = setRefClass(
  Class = "ridgereg",
  fields = c(  "coefs",
               "y_est",
               "local_data_name",
               "local_lambda"),
  methods = list(
    initialize = function(formula, data, lambda){
      
      local_data_name <<- deparse(substitute(data))
      local_lambda<<- lambda

      my_data_x = model.matrix(object = formula, data = data)
      y = data[[all.vars(formula)[[1]]]]
      my_normalized_data = scale(my_data_x)
      X = as.matrix(my_normalized_data)[,-1]
      
      

      coefs <<-drop(solve((t(X)%*%X) + diag(rep(lambda, ncol(X)))) %*% (t(X)%*%y))
      beta_zero = mean(dep_var_matrix)

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
