# Problem 1 - beta prior, data on bernoulli
alpha_prior <- 2
beta_prior <- 2
heads <- 12
tails <- 8

# Posterior parameters
alpha_posterior <- alpha_prior + heads
beta_posterior <- beta_prior + tails  ##(n - s + beta_prior) 
## n = total number of flips
## s = total number of heads 
##(success -> here is getting a head so summation xi = summations of 1 that is number of heads)

# Posterior mean
posterior_mean <- alpha_posterior / (alpha_posterior + beta_posterior)
cat("Posterior Mean:", posterior_mean, "\n")

# 95% credible interval
credible_interval <- qbeta(c(0.025, 0.975), alpha_posterior, beta_posterior)
cat("95% Credible Interval:", credible_interval, "\n")

### Problem 4 - gamma prior, data on poisson
complaints <- c(3, 2, 5, 1, 4, 3, 2, 6, 5, 2)
sum_complaints <- sum(complaints)
n <- length(complaints)

# Prior: Gamma(2, 1)
alpha_prior <- 2
beta_prior <- 1

# Posterior parameters
alpha_posterior <- alpha_prior + sum_complaints
##x_bar = mean of the sample obs; sum of obs =  (total number of obs)*(x_bar)
beta_posterior <- beta_prior + n

# Posterior mean
posterior_mean <- alpha_posterior / beta_posterior
cat("Posterior Mean:", posterior_mean, "\n")

# 95% Credible Interval
credible_interval <- qgamma(c(0.025, 0.975), alpha_posterior, 1/beta_posterior)
##check the pdf for gamma here - we have assumed a different pdf, to work with 
# that we need to do a reciprocal of the second parameter - beta for our case
cat("95% Credible Interval:", credible_interval, "\n")


##Problem 7
m = c(56,60,58,62,59,55,61,63,57,60)
a_pr = 60
b_pr = 25
n = length(m)
s = 2/sqrt(n)