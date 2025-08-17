library(lpSolve)

# Name the decision variables
# x1 = number of pressure washers to purchase
# x2 = number of go-karts to purchase  
# x3 = number of generators to purchase
# x4 = number of water pump cases to purchase

# Determine profit per unit 
pressure_washer_profit <- 499.99 - 330
go_kart_profit <- 729.99 - 370
generator_profit <- 700.99 - 410
water_pump_profit <- (269.99 * 5) - 635 #cases of 4

# Objective matrix
constants <- c(pressure_washer_profit, go_kart_profit, generator_profit,
               water_pump_profit)

# Budget constraints (in dollars)
# 330*x1 + 370*x2 + 410*x3 + 635*x4 <= 170,000
budget_constraint <- c(330, 370, 410, 635)

# Total warehouse space: 82 shelves * 30 ft * 5 ft = 12,300 sq ft
total_space <- 82*30*5

# Space constraints (in square feet)
space_per_pressure_washer <- 5*5
space_per_go_kart <- 8*5
space_per_generator <- 5*5
space_per_water_pump <- (5*5) / 4 # for 4 cases

space_constraint <- c(space_per_pressure_washer, space_per_go_kart, 
                      space_per_generator, space_per_water_pump)

# Marketing constraints 
# 1. at least 30% of budget to pressure washers and go-karts
# 330*x1 + 370*x2 >= 0.30 * (330*x1 + 370*x2 + 410*x3 + 635*x4)
marketing_constraint <- c(231, 259, -123, -190.5)


#2. sell at least twice as many generators as water pumps
## x3 >= 2*x4
### Rearranged: x3 - 2*x4 >= 0
generator_constraint <- c(0, 0, 1, -10)

# Constraint matrix
constraint_matrix <- rbind(
  budget_constraint,
  space_constraint,
  marketing_constraint,
  generator_constraint
)

# Constraint right-hand side
constraint_rhs <- c(
  170000, # total budget
  total_space,
  0, # marketing >= 0
  0 # generator >= 0
)

# Constraint directions
constraint_directions <- c("<=", "<=", ">=", ">=")

# Solve the problem
solution <- lp(direction = "max",
               objective.in = constants,
               const.mat = constraint_matrix,
               const.dir = constraint_directions,
               const.rhs = constraint_rhs,
               compute.sens = TRUE,
               all.int = TRUE)

# Results
names(solution$solution) <- c("Pressure Washers", "Go-Karts", "Generators", "Water Pump Cases")
solution$solution

solution$duals
