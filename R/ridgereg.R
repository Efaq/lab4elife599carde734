
ridgereg = setRefClass(
  Class = "ridgereg",
  fields = c(  "coefs",
               "y_est",
               "local_formula",
               "local_data_name",
               "local_lambda"),
  methods = list(
    initialize = function(formula, data, lambda){
      
      local_formula <<- formula
      local_data_name <<- deparse(substitute(data))
      local_lambda <<- lambda
      
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
      
      #all_normalized_data<-data 
      
      X = model.matrix(object = formula, data = all_normalized_data) #extracts the model matrix, that is, the matrix originated from writing the problem as a set of linear equations
      n<-dim(X)[2]
      y = all_normalized_data[all.vars(formula)][[1]] #picks the name from the formula that is not in the columns of X and identifies it as the name of y. picks y from the data with this name
      coefs <<- drop(solve(((t(X) %*% X)+(lambda*diag(rep(1,n)))), (t(X) %*% y))) #solves X'X + lambda.I coefs = X' y
      y_est <<- drop(X %*% coefs) #calculates estimated y
      n_size = nrow(X)
      p_size = ncol(X)
      
      
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
mod_object_teste <- ridgereg(Petal.Length~Species, data = iris,lambda=0)
mod_object_teste$print()
mod_object_teste$coef()
mod_object_teste$pred()

print(mod_object_teste)
