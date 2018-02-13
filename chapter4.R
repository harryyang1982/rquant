library(tidyverse)

set.seed(100)
x <- rnorm(1e6, mean = 2.33, sd = 0.5)
mu <- mean(x)
sd <- sd(x)
hist(x, breaks = 100)

# ggplot way
ggplot(data_frame(x)) +
  geom_histogram(aes(x), bins = 100) +
  geom_vline(aes(xintercept = mu))

set.seed(12)
rnorm(5)

rnorm(5)

sample5 <- sample(x, 5, replace = T)
sample10 <- sample(x, 10, replace = T)
sample50 <- sample(x, 50, replace = T)

sample5
sample10
sample50

mean(sample5)
mean(sample10)
mean(sample50)

mean(sample(x, 1000, replace = T))
mean(sample(x, 10000, replace = T))

# central limit theorem

map(1:10000, sample, x=x, size = 10, replace = T)

sample_list <- list()

1:10000 %>% 
  map(sample, size = 10, replace = T)

sample(x, size = 10, replace = T)

# sample_list <- list()
# for(i in 1:10000) {
#   sample_list[[i]] <- sample(x, size = 10, replace = T)
# }
# sample_list
# mean_list <- map(sample_list, mean)
# mean_vec <- unlist(mean_list)
# 
# ggplot(data_frame(mean_vec)) +
#   geom_histogram(aes(mean_vec), bins = 500) +
#   geom_vline(aes(xintercept = mu))

arg_size10 <- as.list(rep(10, 10000))

sample_list <- arg_size10 %>% 
  map(sample, x = x, replace = T)

mean_vec <- sample_list %>% 
  map_dbl(mean) %>% 
  as_data_frame()

ggplot(mean_vec) +
  geom_histogram(aes(value), bins = 500) +
  geom_vline(aes(xintercept = mu), linetype = 2) +
  labs(x = "Mean of 10 samples from x", title = "Convergence of sample distribution"
       )

population <- sample(c(0, 1), 100000, replace = T)

ggplot(data_frame(population)) +
  geom_histogram(aes(population)) +
  geom_vline(xintercept = mean(population), linetype = 2) +
  labs(title = "Non-normal")

## non-normal iteration

sample_list2 <- arg_size10 %>% 
  map(sample, x = population, replace = T)
  

mean_vec2 <- sample_list2 %>% 
  map_dbl(mean) %>% 
  as_data_frame()

mean_vec2

ggplot(mean_vec2) +
  geom_histogram(aes(value)) +
  geom_vline(xintercept = 0.5, linetype = 2) +
  labs(x = "Average of 10 samples", title = "Distribution of averages"
  )

population_variance <- function(x) {
  mean <- sum(x) / length(x)
  return(sum((x-mean)^2)/length(x))
}

population <- as.numeric(1:100000)
variance <- population_variance(population)

variance

output <- arg_size10 %>% 
  map(sample, x = population, replace = T)

variance_estimates <- map_dbl(output, population_variance) %>% 
  as_data_frame()
variance_estimates %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 100, alpha = 0.7) +
  geom_vline(xintercept = mean(variance_estimates$value)) +
  geom_vline(xintercept = variance, linetype = 2)

sample_variance <- function(x) {
  mean <- sum(x) / length(x)
  return(sum((x-mean)^2) / (length(x) - 1))
}

output <- arg_size10 %>% 
  map(sample, x = population, replace = T)

sample_variance_estimates <- map_dbl(output, sample_variance) %>% 
  as_data_frame()

sample_variance_estimates %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 100, alpha = 0.7) +
  geom_vline(xintercept = mean(sample_variance_estimates$value)) +
  geom_vline(xintercept = variance, linetype = 2)

mean(sample_variance_estimates$value)

plot(c(-1, 1), c(0.5, 0.5), type = "h", lwd = 3,
     xlim = c(-2, 2), main = "Probability mass function of coin toss",
     ylab = "Probability",
     xlab = "Random Variable",
     cex.main = 0.9)

## Coin simulation with Bayesian Theorem

outcomes <- sample(c(0, 1), 1000, replace = T)
outcomes

set.seed(101)
biased_outcomes <- sample(c(0, 1), 1000, replace = T, prob = c(0.4, 0.6))
table(biased_outcomes)

prob_estimate <- sum(biased_outcomes) / length(biased_outcomes)
prob_estimate
