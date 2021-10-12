
ridgereg = setRefClass(
  Class = "ridgereg",
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
    initialize = function(formula, data, lambda){
      local_formula <<- formula
      local_data_name <<- deparse(substitute(data))
      #local_lambda <<- aaa
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

      
      labels_plot_1 <- vector(length=150)
      labels_plot_2 <- vector(length=150)
      labels_plot_1[] <- ""
      labels_plot_2[] <- ""
      labels_plot_1[119]<-'119'
      labels_plot_1[118]<-'118'
      labels_plot_2[99]<-'99'
      
      
      
      stand = sqrt(abs((residuals/sd(residuals))))
      
      
      p1<- ggplot(data = data.frame(y_est, residuals)) + 
        geom_point(mapping=aes(x=y_est, y=residuals),shape=21,size=2.5) + 
        labs(title = "Residuals vs Fitted",
             x=paste("Fitted values \n linreg(", deparse(local_formula),")", sep = "") ,
             y="Residuals")+
        theme(plot.title = element_text(hjust = 0.5, size=12)) +
        geom_text(mapping=aes(x=y_est, y=residuals),label=ifelse(labels_plot_1 != FALSE ,as.character(labels_plot_1),''),hjust=1.5,vjust=0.5) +
        geom_text(mapping=aes(x=y_est, y=residuals),label=ifelse(labels_plot_2 != FALSE ,as.character(labels_plot_2),''),hjust=-0.5,vjust=0.5) +
        theme(panel.background = element_blank(), element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA, size=1))+
        stat_summary(mapping=aes(x=y_est, y=residuals),fun = median, geom="line", colour="red")+
        geom_hline(yintercept=-0, linetype="dotted", color = "grey", size=1)+
        scale_y_continuous(limits=c(-1.5, 1.5), breaks = seq(-1.5, 1.5, by=1)) 
      
      
      
      
      p2<- ggplot(data = data.frame(y_est, stand)) + 
        geom_point(mapping=aes(x=y_est, y=stand),shape=21,size=2.5) + 
        labs(title = "Scale - Location",
             x=paste("Fitted values \n linreg(", deparse(local_formula),")", sep = "") ,
             y=expression(sqrt("|Standardized Residuals|")))+
        theme(plot.title = element_text(hjust = 0.5, size=12)) +
        geom_text(mapping=aes(x=y_est, y=stand),label=ifelse(labels_plot_1 != FALSE ,as.character(labels_plot_1),''),hjust=1.5,vjust=0.5) +
        geom_text(mapping=aes(x=y_est, y=stand),label=ifelse(labels_plot_2 != FALSE ,as.character(labels_plot_2),''),hjust=-0.5,vjust=0.5) +
        theme(panel.background = element_blank(), element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA, size=1))+
        stat_summary(mapping=aes(x=y_est, y=stand),fun = mean, geom="line", colour="red") 
      
      plist<-list(p1,p2)
      return(plist)
      
    }

  )
)