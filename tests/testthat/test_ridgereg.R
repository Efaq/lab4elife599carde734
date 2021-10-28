
data("iris")


#lm.ridge
mass_object_0 <- MASS::lm.ridge(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=0)
lmridge0<- round(mass_object_0$coef,1)

mass_object_1 <- MASS::lm.ridge(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=1)
lmridge1<-round(mass_object_1$coef,1)

mass_object_15 <- MASS::lm.ridge(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=15)
lmridge15<-round(mass_object_15$coef,1)


#Ridgereg
ridgereg_object_0 <- ridgereg(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=0)
ridge0<-round(ridgereg_object_0$coef(),1)

ridgereg_object_1 <- ridgereg(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=1)
ridge1<-round(ridgereg_object_1$coef(),1)

ridgereg_object_15 <- ridgereg(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = iris,lambda=15)
ridge15<-round(ridgereg_object_15$coef(),1)


test_that("Ridgereg function is giving the same outputs as lm.ridge from MASS package", {
  
  expect_equal(lmridge0,ridge0)
  expect_equal(lmridge1,ridge1)
  expect_equal(lmridge15,ridge15)
})
