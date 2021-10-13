
ridgereg = setRefClass(
  Class = "ridgereg",
  fields = c(  "coefs",
               "y_est",
               #"residuals",
               #"degrees_freedom",
               #"residual_var",
               #"var_coef",
               #"t_coef",
               #"p_coef",
               "local_formula",
               "local_data_name",
               "local_lambda"),
  methods = list(
    initialize = function(formula, data, lambda){
    
      local_formula <<- formula
      local_data_name <<- deparse(substitute(data))
      local_lambda <<- lambda
  
      
      data=iris
      formula<- Petal.Length~Species
      
      
      split_formula1<-unlist(strsplit(deparse(formula),"~"))
      split_formula2<-strsplit(split_formula1, " ")[[2]]
      dependent_var_name<- split_formula2[2]
      dependent_var<- data[[paste(dependent_var_name)]]
      
      data_2<-data
      
      #Remove the dependent variable before normalization of data 
      
      drop<- c(paste(dependent_var_name))
      data_2 = data[,!(names(data) %in% drop)]
      
      #Normalize data 
      
        normalized_data<-data_2
        x<-data_2
        i=1
        
        while(i<=length(x))
        { 
          normalized_data[i] = suppressWarnings((x[i]-colMeans(x[i]))/(sqrt(var(x[i]))))
          i=i+1
        }
    
      #Gather dependent and independent variables again
    
      all_normalized_data<-cbind.data.frame(normalized_data,dependent_var=dependent_var)
      colnames(all_normalized_data)[colnames(all_normalized_data) == "dependent_var"] <- c(dependent_var_name)
      
      X = model.matrix(object = formula, data = all_normalized_data) #extracts the model matrix, that is, the matrix originated from writing the problem as a set of linear equations
      y = all_normalized_data[all.vars(formula)][[1]] #picks the name from the formula that is not in the columns of X and identifies it as the name of y. picks y from the data with this name
      coefs <<- drop(solve(((t(X) %*% X)+lambda), (t(X) %*% y))) #solves X'X + lambda coefs = X' y
      y_est <<- drop(X %*% coefs) #calculates estimated y
      #residuals <<- y - y_est # calculates the residuals
      #n_size = nrow(X)
      #p_size = ncol(X)
      #degrees_freedom <<- n_size - p_size
      #residual_var <<- drop((t(residuals) %*% residuals) / degrees_freedom) #calculates the residual variance
      #temp_var_coef = solve((t(X) %*% X), diag(ncol(X))) #gets the inverse of X'X by solving for the identity as a result
      #colnames(temp_var_coef) = rownames(temp_var_coef)
      #var_coef <<- residual_var * diag(temp_var_coef) #calculates the variance of the coefficients
      #t_coef <<- coefs / sqrt(var_coef) #calculates t-values
      #p_coef <<- 2*(1-pt(abs(t_coef), df=degrees_freedom)) #calculates p-values
    },
    print = function(){
      cat(paste("ridgereg(formula = ", deparse(local_formula), ", data = ", local_data_name, ", lambda = ", local_lambda ,")\n\n", sep = ""))
      base::print(coefs)
    },
    pred = function(){
      return(y_est)
    },
    coef = function(){
      return(coefs)
    }
    
  )
)




data(iris)
mod_object_teste <- ridgereg(Petal.Length~Species, data = iris,lambda=2)
print(mod_object_teste)
