# Linear Progaming in R - Module 5 Lab
#install.packages("lpSolve")
library(lpSolve)

# Name the decision variables
Names <- c("x1", "x2", "x3")

# Define Obejective parameters
C <- c(20, 30, 40)

Objective <- C

# Define Constraints
Const1 <- c(1,1,1)
Const2 <- c(3,5,8)
Const3 <- c(0.6, -0.4, -0.4)
Const4 <- c(0,0,-1)
Const5 <- c(-1,0,0)
Const6 <- c(0,-1,0)

Constraints <- matrix(c(Const1, Const2, Const3, Const4,
                        Const5, Const6), ncol=3, byrow=TRUE)

# Constraints Inequality Types
Directions <- c("<=","<=","<=","<=","<=","<=")

# Contraints right sides
b <- c(1000, 5000, 0, -250, 0, 0)

# Solve the model 
LP <- lp("max", Objective, Constraints, Directions, b, compute.sens = TRUE)
LP

DF1 <- data.frame("Product"=Names, "Optimal Decisions"=round(LP$solution))
DF1

LP$solution # decision variable valies
LP$objval # optimized Z values
LP$duals # shaddow prices
LP$ duals.from

summary(LP)
