# Linear Progaming in R - Module 5 Lab
#install.packages("lpSolve")
library(lpSolve)

# Objective function coefficients (maximize reach)
objective <- c(2000, 3000)  # x = radio, y = TV

# Constraint coefficients
constraints <- matrix(c(
  300, 2000,      # Budget constraint
  0.2, -0.8,      # Radio ad limit
  -0.8, 0.2       # TV ad limit
), nrow = 3, byrow = TRUE)

# Direction of constraints
directions <- c("<=", "<=", "<=")

# Right-hand side of constraints
rhs <- c(20000, 0, 0)

# Solve the LP
solution <- lp("max", objective, constraints, directions, rhs, compute.sens = TRUE)

solution
# Optimal number of radio and TV ads
solution$solution  # [x, y]

# Total number of ads
round(sum(solution$solution))  # Round to nearest whole number
solution$solution
