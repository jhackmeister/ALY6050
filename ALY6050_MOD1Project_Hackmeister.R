###############
### Part 1  ###
# BOS NY BOS ##
###############

# Probabilities
p_win_home <- 0.6  # Red Sox win at home
p_win_away <- 0.43 # Red Sox win at Yankees' home (1 - 0.57)

# Probability of winning in 2 games
p_win_2 <- p_win_home * p_win_away
p_win_2

# Probability of winning in 3 games
p_win_3 <- (p_win_home * (1 - p_win_away) * p_win_home) + 
  ((1 - p_win_home) * p_win_away * p_win_home)
p_win_3

# Total probability of Red Sox winning the series
p_total <- p_win_2 + p_win_3
p_total

# Step 2: Construct the theoretical probability distribution for net winnings (X).
# Net winnings for each outcome
winnings_win_2 <- 1000
winnings_win_3 <- 480
winnings_lose_2 <- -1040
winnings_lose_3 <- -540

# Probabilities for each outcome
p_win_2
p_win_3

p_lose_2 <- (1 - p_win_home) * (1 - p_win_away)
p_lose_2

p_lose_3 <- (p_win_home * (1 - p_win_away) * (1 - p_win_home)) + 
  ((1 - p_win_home) * p_win_away * (1 - p_win_home))
p_lose_3

# Expected value (mean) of X
expected_value <- (winnings_win_2 * p_win_2) + 
  (winnings_win_3 * p_win_3) + 
  (winnings_lose_2 * p_lose_2) + 
  (winnings_lose_3 * p_lose_3)

# Variance and standard deviation of X
variance <- ((winnings_win_2 - expected_value)^2 * p_win_2) + 
  ((winnings_win_3 - expected_value)^2 * p_win_3) + 
  ((winnings_lose_2 - expected_value)^2 * p_lose_2) + 
  ((winnings_lose_3 - expected_value)^2 * p_lose_3)
std_dev <- sqrt(variance)

expected_value
std_dev

# Step 3: Simulate 10,000 series and compute a 95% confidence interval.
set.seed(123) # For reproducibility
n_simulations <- 10000

# Function to simulate a series
simulate_series <- function() {
  games <- c("Boston", "New York", "Boston")
  red_sox_wins <- 0
  yankees_wins <- 0
  net_winnings <- 0
  
  for (game in games) {
    if (game == "Boston") {
      red_sox_win <- runif(1) < p_win_home
    } else {
      red_sox_win <- runif(1) < p_win_away
    }
    
    if (red_sox_win) {
      red_sox_wins <- red_sox_wins + 1
      net_winnings <- net_winnings + 500
    } else {
      yankees_wins <- yankees_wins + 1
      net_winnings <- net_winnings - 520
    }
    
    if (red_sox_wins == 2 || yankees_wins == 2) {
      break
    }
  }
  
  return(net_winnings)
}

# Simulate 10,000 series
simulated_winnings <- replicate(n_simulations, simulate_series())

# Sample mean and 95% confidence interval
sample_mean <- mean(simulated_winnings)
sample_std_dev <- sd(simulated_winnings)
confidence_interval <- sample_mean + c(-1, 1) * 1.96 * 
  (sample_std_dev / sqrt(n_simulations))

sample_mean
confidence_interval

# Step 4: Frequency distribution and Chi-squared test.
# Frequency distribution of simulated winnings
freq_dist <- table(simulated_winnings)

# Theoretical probabilities
theoretical_probs <- c(p_win_2, p_win_3, p_lose_2, p_lose_3)

# Chi-squared test
chi_squared_test <- chisq.test(freq_dist, p = theoretical_probs)
chi_squared_test

# Visualization 1: Histogram of Simulated Winnings
hist(simulated_winnings, breaks=30, 
     main="Frequency Distribution of Simulated Net Winnings (Part 1)", 
     xlab="Net Winnings", ylab="Frequency", col="red")

# Visualization 2: Confidence Interval Plot
plot(1, sample_mean, xlim=c(0.5, 1.5), 
     ylim=c(confidence_interval[1], confidence_interval[2]), 
     pch=19, xlab="", ylab="Net Winnings", 
     main="95% Confidence Interval for Sample Mean (Part 1)")
arrows(1, confidence_interval[1], 1, confidence_interval[2], 
       length=0.1, angle=90, code=3)

# Visualization 3: Theoretical vs. Simulated Probabilities
theoretical_probs <- c(p_win_2, p_win_3, p_lose_2, p_lose_3)
simulated_probs <- table(simulated_winnings) / n_simulations

barplot(rbind(theoretical_probs, simulated_probs), beside=TRUE, 
        col=c("red", "navy"), 
        main="Theoretical vs. Simulated Probabilities (Part 1)", 
        xlab="Outcome", ylab="Probability", 
        legend.text=c("Theoretical", "Simulated"), args.legend=list(x="topright"))