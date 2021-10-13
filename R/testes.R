data(iris)
data_teste<- iris
formula_teste<- Petal.Length~Species
teste_matrix<- model.matrix(formula_teste, data = data_teste)
y_teste = data_teste[all.vars(formula_teste)][[1]]

#independent variable - petal.lenght 
#dependent variable - Species 


#formula_teste


#data_teste

#data_scaled=cbind(scale(data_teste[1:4]))
                  
split_formula1<-unlist(strsplit(deparse(formula_teste),"~"))
split_formula2<-strsplit(split_formula1, " ")[[2]]
dependent_var_name<- split_formula2[2]

dependent_var<- data_teste[[paste(dependent_var_name)]]


data_teste_2 <- data_teste

#Remove the dependent variable before normalization of data 

drop<- c(paste(dependent_var_name))
data_teste_2 = data_teste[,!(names(data_teste) %in% drop)]

#Normalize data 

normalize <-function(x)
{
  normalized_data<-x
  i=1
  
  while(i<=length(x))
  { 
     normalized_data[i] = suppressWarnings((x[i]-colMeans(x[i]))/(sqrt(var(x[i]))))
    i=i+1
  }
}

normalized_data<-normalize(data_teste_2)


#Gather dependent and independent variables again 

all_normalized_data<-cbind.data.frame(normalized_data,dependent_var)



