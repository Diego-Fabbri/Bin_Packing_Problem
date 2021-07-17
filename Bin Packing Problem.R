#Set your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Bin Packing Problem")

# Import lpSolve package
library(lpSolve)

#Import required packages
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)

#Set weights
w <- c(48, 30, 19, 36, 36, 27, 42, 42, 36, 24, 30)

#Set bins' capacities
k <- c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)

#Set number of available Bins
m <- length(w)

#Set number of Items
n <- length(w)


#Build model
Model <- MIPModel() %>%
  add_variable(y[j], j = 1:m, type = "binary") %>% #define variables
  add_variable(x[i, j], i = 1:n , j = 1:m, type = "binary") %>%
  set_objective(sum_expr(y[j], j = 1:m), "min") %>%   #define objective function
  add_constraint(sum_expr(w[i] * x[i, j], i = 1:n) <= k[j] * y[j], j = 1:m) %>% #define constraints
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1 , i = 1:n) %>%
  solve_model(with_ROI(solver = "symphony", verbosity = 1))

#Model summary

##Objective Function
print(paste("Objective value: ", objective_value(Model)))

## X variables
for (r in 1:n) {
  for (c in 1:m) {
    tmp <- get_solution(Model, x[i, j]) %>%
      filter(variable == "x", i == r , j == c) %>%
      select(value)
    
    if (tmp > 0) {
      print(paste("x[", r, ",", c, "] = ", tmp))
    }
  }
}

## Y variables
for (c in 1:m) {
  tmp <- get_solution(Model, y[j]) %>%
    filter(variable == "y", j == c) %>%
    select(value)
  
  if (tmp > 0) {
    print(paste("y[", c, "] = ", tmp))
  }
}

