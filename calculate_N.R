
library(pwr)

# define function
calculate_sample_size <- function(beta, p_value, power = 0.8) {
    f2 <- (beta^2) / (1 - beta^2)  # effect size calculation
    power_result <- pwr.f2.test(u = 1,  # one predictor
                                f2 = f2,
                                sig.level = p_value,
                                power = power)
    return(ceiling(power_result$u + power_result$v + 1)) # ensure N is rounded up to the nearest whole number
}

# define  parameters
betas <- c(0.30, 0.50)
p_values <- c(0.05, 0.025, 0.01)
power <- 0.8

# store results 
results <- data.frame(beta = numeric(), p_value = numeric(), N = numeric())

# apply function and loop through each combination of beta and p
for (beta in betas) {
    for (p in p_values) {
        sample_size <- calculate_sample_size(beta, p, power)
        results <- rbind(results, data.frame(beta = beta, p_value = p, N = sample_size))
    }
}

# show results
results



# Define a function 
calculate_sample_size_t_test <- function(effect_size, alpha, power = 0.80) {
    power_result <- pwr.t.test(d = effect_size, 
                               sig.level = alpha, 
                               power = power, 
                               type = "two.sample")
    N_per_group <- ceiling(power_result$n)  # Ensure N is rounded up to the nearest whole number
    total_N <- N_per_group * 2  # total sample size for two groups
    return(total_N)
}

# Define parameters
effect_size <- 0.6
alpha_levels <- c(0.05, 0.025, 0.01)
power <- 0.80

# Store results
t_test_results <- data.frame(alpha = numeric(), 
                             N = numeric())

# Apply function and loop through each alpha level
for (alpha in alpha_levels) {
    total_N <- calculate_sample_size_t_test(effect_size, alpha, power)
    t_test_results <- rbind(t_test_results, data.frame(alpha = alpha, N = total_N))
}


t_test_results



