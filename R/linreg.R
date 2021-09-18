data("iris")


my_formula = Petal.Length ~ Sepal.Width + Sepal.Length #establishes independent and dependent variables
data = iris
###
X = model.matrix(object = my_formula, data = data) #extracts the model matrix, that is, the matrix originated from writting the problem as a set of linear equations
variable_names = all.vars(my_formula) #all variables names in the formula
y = data[[variable_names[which(!(all.vars(my_formula) %in% colnames(X)))]]] #picks the name from the formula that is not in the columns of X and identifies it as the name of y. picks y from the data with this name
coef = solve((t(X) %*% X), (t(X) %*% y)) #solves X'X coef = X' y