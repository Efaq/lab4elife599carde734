#install and load ggplot2

install.packages("ggplot2")
library(ggplot2)


data(iris)
mod_object <- lm(Petal.Length~Species, data = iris)
print(mod_object)



plot_p <- function()
{ 
  x1<-fitted(mod_object)
  y1<-resid(mod_object)
  
  df1<-data.frame(x1,y1)
    
    ggplot(df1,aes(x=x1,y=y1)) + 
      geom_point(shape=21) + 
      labs(title = "Residuals vs Fitted",
           x="Fitted values \n lm(Petal.Length ~ Species)",
           y="Residuals"  )+
      theme(plot.title = element_text(hjust = 0.5)) 
    
       
}

plot_p()








