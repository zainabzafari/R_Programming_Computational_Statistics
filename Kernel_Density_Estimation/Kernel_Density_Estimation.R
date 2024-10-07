
#Kernel Density Estimation


credit_scores <-read.csv("D:\\R.Programming\\data\\ex01_credit_scores.csv", sep = ";")
str(credit_scores)
head(credit_scores)

# Create a histogram with bin width of 1
# We want to plot scores so we should extract the score column
scores <- credit_scores$score
hist(scores, breaks = seq(floor(min(scores)), ceiling(max(scores)), by = 1),
     main = "Credit Score Distribution", 
     xlab = "Credit Scores", 
     col = "lightblue", 
     border = "black")


# Generate a sequence from 0 to 10 with a step of 0.01
x_values <- seq(0,10, by=0.1)
x_values

# Define parameters for the two normal distributions
mu1 <- 2
sigma1 <-0.4
mu2 <- 4
sigma2 <- 1.2

density <- 0.5*dnorm(x_values, mean = mu1, sd=sigma1) + 0.5*dnorm(x_values, mean = mu2, sd=sigma2)

plot(x_values, density, type = "l", main = "Mixture of two Normal distributions", xlab = "x", ylab = "density", col="blue")

#define cosine kernal function to smooth the density

cosin_kernel <- function(x, h){
  
  if (abs(x / h) <= 1) {
    return((pi / 4) * cos(pi * (x / h) / 2))  # Adjust by bandwidth (h)
  } else {
    return(0)
  }
}

# Define a function to smooth data with a given bandwidth (h)
smooth_data_bandwidth <- function(data, x_grid, h) {
  smoothed_values <- numeric(length(x_grid))
  
  # Iterate over each value in the grid
  for (i in 1:length(x_grid)) {
    kernel_values <- sapply(data, function(d) cosin_kernel(x_grid[i] - d, h))  # Apply kernel with bandwidth h
    smoothed_values[i] <- mean(kernel_values)  # Average the kernel values for smoothing
  }
  
  return(smoothed_values)
}

# Compute the true density (from Part B)
x_grid <- seq(0, 10, by = 0.01)
density_true <- 0.5 * dnorm(x_grid, mean = 2, sd = 0.4) + 
  0.5 * dnorm(x_grid, mean = 4, sd = 1.2)

# Define a function to compute the mean squared error (MSE)
compute_mse <- function(smoothed_values, true_values) {
  return(mean((smoothed_values - true_values)^2))  # Compute the MSE
}



# Test different bandwidths
bandwidths <- c(0.01, 0.1, 1, 2)  # Example bandwidths
mse_values <- numeric(length(bandwidths))  # Store MSE values

# Loop over each bandwidth and compute smoothed estimates and MSE
for (i in 1:length(bandwidths)) {
  smoothed_values <- smooth_data_bandwidth(scores, x_grid, bandwidths[i])
  mse_values[i] <- compute_mse(smoothed_values, density_true)  # Compute MSE
}

# Loop over each bandwidth and compute smoothed estimates and MSE
for (i in 1:length(bandwidths)) {
  smoothed_values <- smooth_data_bandwidth(scores, x_grid, bandwidths[i])
  mse_values[i] <- compute_mse(smoothed_values, density_true)  # Compute MSE
}

# Plot the original density and the smoothed estimates
plot(x_grid, density_true, type = "l", col = "blue", 
     main = "Original vs Smoothed Densities", 
     xlab = "x", ylab = "Density")
colors <- c("red", "green", "orange", "purple")

for (i in 1:length(bandwidths)) {
  smoothed_values <- smooth_data_bandwidth(scores, x_grid, bandwidths[i])
  lines(x_grid, smoothed_values, col = colors[i], lty = 2)  # Plot smoothed estimates
}

# Add legend
legend("topright", legend = paste("Bandwidth =", bandwidths), col = colors, lty = 2, title = "Smoothed Densities")

# Plot MSE values for each bandwidth
















