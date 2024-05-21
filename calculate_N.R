
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
