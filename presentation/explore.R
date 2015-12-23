library(ISLR)
library(dplyr)
library(ggplot2)
library(caret)
library(DT)

data("Auto")
data("mtcars")

auto <- Auto
cars <- mtcars

mt_names <- names(cars)
isl_names <- names(auto)

num_isl_var <- length(auto[1,])
num_isl_obs <- length(auto[,1])

num_mtc_var <- length(cars[1,])
num_mtc_obs <- length(cars[,1])

mt_pl <- ggplot(cars, aes(disp, mpg)) +
    geom_point() +
    geom_smooth(method="loess")
print(mt_pl)

isl_pl <- ggplot(auto, aes(displacement, mpg )) +
    geom_point() +
    geom_smooth(method="loess")
print(isl_pl)

mt_fit <- lm(mpg ~ wt 
             + disp 
             + hp 
             + cyl 
             + qsec, 
             data=cars)
mt_sum <- summary(mt_fit)

isl_fit <- lm(mpg ~ weight 
              + displacement 
              + horsepower 
              + cylinders
              + acceleration, 
              data=auto)
isl_sum <- summary(isl_fit)

var_list <- list("Compare","Variable Count","Unique Variables","Observations","Convenience","Other")
mt_list <- list("MTCars",num_mtc_var,"drat, vs, am, gear, carb",num_mtc_obs,"Built in","Carb, am, and drat are useful variables. Car names as rownames. Number of observations low.")
isl_list <- list("Auto (ISLR)",num_isl_var,"year, origin, name",num_isl_obs,"Have to load ISLR library","Year and origin are useful variables. Names as separate varible helpful. Many observations.")
comp_df <- NULL
comp_df <- as.data.frame(cbind(comp_df, var_list, mt_list, isl_list))



coef_tbl <- NULL
coef_tbl <- cbind(coef_tbl, c("(Intercept)", "wt/weight", "disp/displacement", "hp/horsepower", "cyl/cylinders", "qsec/acceleration"))
coef_tbl <- cbind(coef_tbl, c(35.87361203, -4.22527033,  0.01194976, -0.01583910, -1.15608107,  0.25381843))
coef_tbl <- cbind(coef_tbl, c( 4.626431e+01, -5.186917e-03, -8.313012e-05, -4.525708e-02, -3.979284e-01, -2.910471e-02))
coef_tbl <- as.data.frame(coef_tbl)
names(coef_tbl) <- c("Data","MTCars", "Auto (ISLR)")
print(knitr::kable(coef_tbl, col.names = colnames(coef_tbl)))


