# Load necessary libraries
library(ggplot2)
library(stats)
#library(extraDistr)

# Step 1: Simulate 100 values from Binomial(p)
n <- 100  # number of trials
p_true <- 0.6  # true probability of success
x <- rbinom(n, 1, p_true)  # Simulating 100 Bernoulli trials

# Step 2: Bayesian Estimation with Normal Prior
mu_prior <- 0.5  # Mean of prior
sigma_prior <- 0.1  # Standard deviation of prior

# Likelihood parameters
alpha <- mu_prior * ((1 - mu_prior) / sigma_prior^2 - 1)
beta <- (1 - mu_prior) * ((1 - mu_prior) / sigma_prior^2 - 1)

# Posterior parameters
alpha_post <- alpha + sum(x)
beta_post <- beta + (n - sum(x))

# Bayes estimator (Posterior Mean)
p_bayes <- alpha_post / (alpha_post + beta_post)

# Step 3: Classical Estimator (Maximum Likelihood Estimator)
p_mle <- mean(x)

# Step 4: 95% Posterior Interval Estimate
posterior_ci <- qbeta(c(0.025, 0.975), alpha_post, beta_post)

# Step 5: 95% Classical Interval Estimate (Wald Confidence Interval)
se_mle <- sqrt((p_mle * (1 - p_mle)) / n)
classical_ci <- c(p_mle - 1.96 * se_mle, p_mle + 1.96 * se_mle)

# Plot Posterior Distribution
x_vals <- seq(0, 1, length.out = 1000)
posterior_pdf <- dbeta(x_vals, alpha_post, beta_post)

ggplot(data.frame(x = x_vals, y = posterior_pdf), aes(x, y)) +
  geom_line(color = 'blue') +
  geom_vline(xintercept = posterior_ci, color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = p_bayes, color = 'green') +
  labs(title = "Posterior Distribution of p", x = "p", y = "Density") +
  theme_minimal()

# Print results
cat(sprintf("Bayes Estimator: %.4f\n", p_bayes))
cat(sprintf("Classical (MLE) Estimator: %.4f\n", p_mle))
cat(sprintf("95%% Posterior Interval Estimate: (%.4f, %.4f)\n", posterior_ci[1], posterior_ci[2]))
cat(sprintf("95%% Classical Interval Estimate: (%.4f, %.4f)\n", classical_ci[1], classical_ci[2]))
