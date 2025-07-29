# ALY 6050 Module 2 Project
# Dam Projects Benefit-Cost Analysis
# Monte Carlo Simulation for 10,000 benefit-cost ratios

# Load required libraries
library(tidyverse)

##################################
##### Monte Carlo Simulation #####
##################################

#####################
##### Part One ######
#####################

# Set seed for reproducibility
set.seed(123)

# Number of simulations
n_sim <- 10000

# Set up triangular distribution

triangular_stats <- function(min_val, mode_val, max_val) {
  mean_theoretical <- (min_val + mode_val + max_val) / 3
  var_theoretical <- (min_val^2 + mode_val^2 + max_val^2 - 
                        min_val*mode_val - min_val*max_val - mode_val*max_val) / 18
  sd_theoretical <- sqrt(var_theoretical)
  return(list(mean = mean_theoretical, sd = sd_theoretical))
}

rtriangular <- function(n, min, mode, max) {
u <- runif(n)
f_mode <- (mode - min) / (max - min)

result <- ifelse(u <= f_mode,
                 min + sqrt(u * (mode - min) * (max - min)),
                 max - sqrt((1 - u) * (max - mode) * (max - min)))
return(result)
}

# Dam 1
dam1_benefits <- list(
  navigation = c(min = 1.1, mode = 2.0, max = 2.8),
  hydroelectric = c(min = 8.0, mode = 12.0, max = 14.9),
  fish_wildlife = c(min = 1.4, mode = 1.4, max = 2.2),
  recreation = c(min = 6.5, mode = 9.8, max = 14.6),
  flood_control = c(min = 1.7, mode = 2.4, max = 3.6),
  commercial = c(min = 0.0, mode = 1.6, max = 2.4)
)

dam1_costs <- list(
  capital = c(min = 13.2, mode = 14.2, max = 19.1),
  operations = c(min = 3.5, mode = 4.9, max = 7.4)
)

# Dam 2 
dam2_benefits <- list(
  navigation = c(min = 2.1, mode = 3.0, max = 4.8),
  hydroelectric = c(min = 8.7, mode = 12.2, max = 13.6),
  fish_wildlife = c(min = 2.3, mode = 3.0, max = 3.0),
  recreation = c(min = 5.9, mode = 8.7, max = 15.0),
  flood_control = c(min = 0.0, mode = 3.4, max = 3.4),
  commercial = c(min = 0.0, mode = 1.2, max = 1.8)
)

dam2_costs <- list(
  capital = c(min = 12.8, mode = 15.8, max = 20.1),
  operations = c(min = 3.8, mode = 5.7, max = 8.0)
)

# Simulation setup 
simulate_dam <- function(benefits_data, costs_data, n_simulations) {
  
  # Generate random samples for each benefit category
  benefits_sim <- map_dfc(benefits_data, function(x) {
    rtriangular(n_simulations, x["min"], x["mode"], x["max"])
  })
  
  # Generate random samples for each cost category
  costs_sim <- map_dfc(costs_data, function(x) {
    rtriangular(n_simulations, x["min"], x["mode"], x["max"])
  })
  
  # Calculate total benefits and costs for each simulation
  total_benefits <- rowSums(benefits_sim)
  total_costs <- rowSums(costs_sim)
  
  # Calculate benefit-cost ratios
  bc_ratios <- total_benefits / total_costs
  
  return(list(
    benefits = benefits_sim,
    costs = costs_sim,
    total_benefits = total_benefits,
    total_costs = total_costs,
    bc_ratios = bc_ratios
  ))
}


# Dam 1 simulation
alpha1_results <- simulate_dam(dam1_benefits, dam1_costs, n_sim)
alpha1 <- alpha1_results$bc_ratios

# Dam 2 simulation
alpha2_results <- simulate_dam(dam2_benefits, dam2_costs, n_sim)
alpha2 <- alpha2_results$bc_ratios



##################################
##### Frequency Distribution #####
##################################

# Determining bin size, bins = square root of sample size
print(round(sqrt(n_sim), digits = 0))
# Function to create frequency distribution table
create_frequency_table <- function(data, project_name, n_bins = 100) {
  
  # Determine range and create bins
  data_range <- range(data)
  bin_width <- (data_range[2] - data_range[1]) / n_bins
  
  # Create bin breaks
  breaks <- seq(data_range[1], data_range[2], length.out = n_bins + 1)
  
  # Create intervals using cut function
  intervals <- cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE)
  
  # Create frequency table
  freq_table <- table(intervals)
  
  # Convert to dataframe for better formatting
  freq_df <- data.frame(
    Interval = names(freq_table),
    Frequency = as.numeric(freq_table),
    Relative_Frequency = as.numeric(freq_table) / length(data),
    Cumulative_Frequency = cumsum(as.numeric(freq_table)),
    Cumulative_Relative_Frequency = cumsum(as.numeric(freq_table)) / length(data)
  )
  
  # Round relative frequencies
  freq_df$Relative_Frequency <- round(freq_df$Relative_Frequency, 4)
  freq_df$Cumulative_Relative_Frequency <- round(freq_df$Cumulative_Relative_Frequency, 4)
  
  return(list(
    table = freq_df,
    breaks = breaks,
    bin_width = bin_width
  ))
}

# Dam #1 Frequency Distribution
alpha1_freq <- create_frequency_table(alpha1, "Dam #1", n_bins = 100)

cat("Bin width:", round(alpha1_freq$bin_width, 4), "\n")

print(alpha1_freq$table)

# Dam #2 Frequency Distribution  
alpha2_freq <- create_frequency_table(alpha2, "Dam #2", n_bins = 100)

cat("Bin width:", round(alpha2_freq$bin_width, 4), "\n")

print(alpha2_freq$table)

# Individual histogram for Dam #1
hist_dam1 <- ggplot(data.frame(alpha1 = alpha1), aes(x = alpha1)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(alpha1), linetype = "solid", color = "darkblue", linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 1.2) +
  labs(title = "Dam #1 Benifit-Cost Ratio Distribution",
       x = "Benifit-Cost Ratio",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))

print(hist_dam1)

# Individual histogram for Dam #2
hist_dam2 <- ggplot(data.frame(alpha2 = alpha2), aes(x = alpha2)) +
  geom_histogram(bins = 15, fill = "forestgreen", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(alpha2), linetype = "solid", color = "darkgreen", linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 1.2) +
  labs(title = "Dam #2 Benifit-Cost Ratio Distribution",
       x = "Benifit-Cost Ratio",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))

print(hist_dam2)

##################################
### Theoretical vs Observed ######
##################################

# Dam 1 observations
observed_mean_benefits <- mean(alpha1_results$total_benefits)
observed_mean_benefits
observed_sd_benefits <- sd(alpha1_results$total_benefits)
observed_sd_benefits
observed_mean_costs <- mean(alpha1_results$total_costs)
observed_mean_costs
observed_sd_costs <- sd(alpha1_results$total_costs)
observed_sd_costs
observed_mean_bc_ratio <- mean(alpha1)
observed_mean_bc_ratio
observed_sd_bc_ratio <- sd(alpha1)
observed_sd_bc_ratio

# Dam 1 Benefits theoretical stats
theoretical_benefits_stats <- map_dfr(dam1_benefits, function(x) {
  triangular_stats(x["min"], x["mode"], x["max"])
})

# Costs theoretical stats  
theoretical_costs_stats <- map_dfr(dam1_costs, function(x) {
  triangular_stats(x["min"], x["mode"], x["max"])
})

# Total theoretical values
theoretical_mean_benefits <- sum(theoretical_benefits_stats$mean)
theoretical_sd_benefits <- sqrt(sum(theoretical_benefits_stats$sd^2))
theoretical_mean_costs <- sum(theoretical_costs_stats$mean)
theoretical_sd_costs <- sqrt(sum(theoretical_costs_stats$sd^2))


# Dam 2 observations
observed_mean_benefits2 <- mean(alpha2_results$total_benefits)
observed_sd_benefits2 <- sd(alpha2_results$total_benefits)
observed_mean_costs2 <- mean(alpha2_results$total_costs)
observed_sd_costs2 <- sd(alpha2_results$total_costs)
observed_mean_bc_ratio2 <- mean(alpha2)
observed_sd_bc_ratio2 <- sd(alpha2)

# Dam 2 Benefits theoretical stats
theoretical_benefits_stats2 <- map_dfr(dam2_benefits, function(x) {
  triangular_stats(x["min"], x["mode"], x["max"])
})

# Costs theoretical stats  
theoretical_costs_stats2 <- map_dfr(dam2_costs, function(x) {
  triangular_stats(x["min"], x["mode"], x["max"])
})

# Total theoretical values
theoretical_mean_benefits2 <- sum(theoretical_benefits_stats2$mean)
theoretical_sd_benefits2 <- sqrt(sum(theoretical_benefits_stats2$sd^2))
theoretical_mean_costs2 <- sum(theoretical_costs_stats2$mean)
theoretical_sd_costs2 <- sqrt(sum(theoretical_costs_stats2$sd^2))

#####################
##### Part Two ######
#####################

n_bins <- 100

##################################
#### Lognormal Distribution ######
##################################

# Calculate sample statistics from alpha 1
sample_mean <- mean(alpha1)
sample_sd <- sd(alpha1)
sample_skewness <- mean(((alpha1 - sample_mean) / sample_sd)^3)

# Fit lognormal distribution
log_alpha1 <- log(alpha1)
meanlog <- mean(log_alpha1)
sdlog <- sd(log_alpha1)

# Calculate theoretical statistics
theoretical_mean <- exp(meanlog + (sdlog^2)/2)
theoretical_var <- (exp(sdlog^2) - 1) * exp(2*meanlog + sdlog^2)
theoretical_sd <- sqrt(theoretical_var)

# Create 100 bins
alpha1_range <- range(alpha1)
bin_breaks <- seq(alpha1_range[1], alpha1_range[2], length.out = n_bins + 1)
bin_width <- (alpha1_range[2] - alpha1_range[1]) / n_bins

# Calculate observed frequencies
observed_intervals <- cut(alpha1, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
observed_freq <- as.numeric(table(observed_intervals))

# Calculate theoretical frequencies from fitted lognormal
theoretical_probs <- plnorm(bin_breaks[-1], meanlog = meanlog, sdlog = sdlog) - 
  plnorm(bin_breaks[-length(bin_breaks)], meanlog = meanlog, sdlog = sdlog)
theoretical_freq <- theoretical_probs * length(alpha1)

# Create comprehensive frequency comparison table
freq_table <- data.frame(
  Bin_Number = 1:n_bins,
  Bin_Lower = round(bin_breaks[-length(bin_breaks)], 6),
  Bin_Upper = round(bin_breaks[-1], 6),
  Bin_Midpoint = round((bin_breaks[-length(bin_breaks)] + bin_breaks[-1])/2, 6),
  Observed_Freq = observed_freq,
  Theoretical_Freq = round(theoretical_freq, 3),
  Observed_Prop = round(observed_freq / length(alpha1), 6),
  Theoretical_Prop = round(theoretical_probs, 6),
  Difference = observed_freq - round(theoretical_freq, 0)
)

# Chi-Squared goodness of fit test

# combine bins < 5
min_expected <- 5
combined_observed <- c()
combined_expected <- c()
combined_bins <- c()
current_obs <- 0
current_exp <- 0
current_bin_count <- 0

for(i in 1:length(theoretical_freq)) {
  current_obs <- current_obs + observed_freq[i]
  current_exp <- current_exp + theoretical_freq[i]
  current_bin_count <- current_bin_count + 1
  
  if(current_exp >= min_expected || i == length(theoretical_freq)) {
    combined_observed <- c(combined_observed, current_obs)
    combined_expected <- c(combined_expected, current_exp)
    combined_bins <- c(combined_bins, current_bin_count)
    current_obs <- 0
    current_exp <- 0
    current_bin_count <- 0
  }
}

# new bins
length(combined_observed)

chi_squared_stat <- sum((combined_observed - combined_expected)^2 / combined_expected)
df <- length(combined_observed) - 1 - 2  # degrees of freedom
p_value <- 1 - pchisq(chi_squared_stat, df)

cat("Chi-squared test results:\n")
cat("Test statistic:", round(chi_squared_stat, 4), "\n")
cat("Degrees of freedom:", df, "\n")
cat("P-value:", format(p_value, scientific = TRUE), "\n")

##################################
###### Results Comparisons ######
##################################

# Create a table for comparison
comparison_table <- data.frame(
  Metric = c("Minimum", "Maximum", "Mean", "Median", "Variance", "Standard Deviation", "P(α > 2)", "P(α > 1.8)", "P(α > 1.5)", "P(α > 1.2)", "P(α > 1)", "P(α > α2)"),
  alpha1 = c(min(alpha1), max(alpha1), mean(alpha1), median(alpha1), var(alpha1), sd(alpha1), mean(alpha1 > 2), mean(alpha1 > 1.8), mean(alpha1 > 1.5), mean(alpha1 > 1.2), mean(alpha1 > 1), mean(alpha1 > alpha2)),
  alpha2 = c(min(alpha2), max(alpha2), mean(alpha2), median(alpha2), var(alpha2), sd(alpha2), mean(alpha2 > 2), mean(alpha2 > 1.8), mean(alpha2 > 1.5), mean(alpha2 > 1.2), mean(alpha2 > 1), mean(alpha2 > alpha1))
)

print(comparison_table)

# compute skewness 
mean_alpha1 <- mean(alpha1_results$bc_ratios)
mean_alpha2 <- mean(alpha2_results$bc_ratios)
sd_alpha1 <- sd(alpha1_results$bc_ratios)
sd_alpha2 <- sd(alpha2_results$bc_ratios)
mean(((alpha1 - mean_alpha1) / sd_alpha1)^3)
mean(((alpha1 - mean_alpha1) / sd_alpha1)^3)
