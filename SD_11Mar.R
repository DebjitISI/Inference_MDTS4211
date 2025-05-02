



set.seed(123)

### Wald test
# Generate random data
x <- rnorm(20)

# Estimate mean and standard error
theta_hat <- mean(x)            # Sample mean (θ̂)
se_theta_hat <- sd(x) / sqrt(length(x))  # Standard error

# Compute Wald statistic
W <- (theta_hat - 0)^2 / se_theta_hat^2  # Testing H0: θ = 0

# Compute p-value from Chi-square distribution with 1 df
p_value <- 1 - pchisq(W, df = 1)

# Print results
cat("Wald Statistic:", W, "\n")
cat("P-value:", p_value, "\n")

### LRT
# Generate random data
x <- rnorm(20)

# Compute sample statistics
n <- length(x)
mu_hat <- mean(x)   # Sample mean (MLE of mu)
sigma_hat <- sd(x)  # Sample standard deviation

# Log-likelihood under H0: mu = 0
logL0 <- sum(dnorm(x, mean = 0, sd = sigma_hat, log = TRUE))

# Log-likelihood under H1: mu = sample mean
logL1 <- sum(dnorm(x, mean = mu_hat, sd = sigma_hat, log = TRUE))

# Compute the likelihood ratio statistic
LRT_stat <- -2 * (logL0 - logL1)

# Compute p-value from Chi-square distribution with 1 degree of freedom
p_value <- 1 - pchisq(LRT_stat, df = 1)

# Print results
cat("Likelihood Ratio Statistic:", LRT_stat, "\n")
cat("P-value:", p_value, "\n")



###Score Test
# Generate random data
x <- rnorm(20)

# Compute sample statistics
n <- length(x)
sigma_hat <- sd(x)  # Sample standard deviation

# Compute the score function under H0: mu = 0
U0 <- sum(x) / sigma_hat^2  # Score function (∂ logL / ∂ mu at mu = 0)

# Compute the Fisher information under H0
I0 <- n / sigma_hat^2  # Fisher Information (Expected variance of U0)

# Compute the Score Statistic
S <- (U0^2) / I0

# Compute p-value from Chi-square distribution with 1 degree of freedom
p_value <- 1 - pchisq(S, df = 1)

# Print results
cat("Score Statistic:", S, "\n")
cat("P-value:", p_value, "\n")
