data("iris")


my_formula = Petal.Length ~ Sepal.Width + Sepal.Length
data = iris
###
X = model.matrix(object = my_formula, data = data)
variable_names = all.vars(my_formula)
y = data[[variable_names[which(!(all.vars(my_formula) %in% colnames(X)))]]]
coef = solve((t(X) %*% X), (t(X) %*% y))