library(tidyverse)

# 적분

integrand <- function(x) 1 / ((x+1)*sqrt(x))
integrate(integrand, lower =0, upper = Inf)

# Vectorized Operation

x <- c(1, 5, 10, 15, 20)
x2 <- 2 * x
x3 <- x ^ 2
x4 <- x / x2
x5 <- round(x * (x / x2) ^ 3.5 + sqrt(x4), 3)
x6 <- round(c(c(x2[2:4], x3[1:2]), x5[4]), 2)
x6

tibble(x, x2, x3, x4, x5)

# Matrix Operation

mat1 <- matrix(rnorm(1000), nrow = 100)
mat2 <- mat1[1:25, ] ^2

mat2
head(round(mat2, 0), 9)[, 1:7]
round(mat2,0)[1:9, 1:7]

df <- data_frame(price = c(89.2, 23.2, 21.2),
                 symbol = c("MOT", "AAPL", "IBM"),
                 action = c("Buy", "Sell", "Buy"))
df

class(df$symbol)

price <- df[1, 1]
price

df2 <- data_frame(col1 = c(1,2,3),
                  col2 = c(1,2,3,4))

symbols <- df$symbol
class(symbols)
factor(symbols)

# new.env()

env <- new.env()
env[["first"]] <- 5
env[["second"]] <- 6
env$third <- 7
env

ls(env)
get("first", envir = env)

rm("second", envir = env)
ls(env)
env_2 <- env
env_2$third <- 42
get("third", envir = env)

# plotting vs ggplotting

x <- c(1, 2, 3.2, 4, 3, 2.1, 9, 19)
x
plot(x)


plot(x, type = "l")

ggplot(df) +
  geom_line(aes(x = index, y = x))

plot(rnorm(1000), main = "Some returns", cex.main = 0.9, xlab = "Time", ylab = "Returns")

grid()
abline(v = 400, lwd = 2, lty = 1)
abline(h = 2, lwd = 3, lty = 3)

## Ggploting

seq_along(x)
df <- data_frame(x, index = seq_along(x))
df
ggplot(df) +
  geom_point(aes(x = index, y = x))

df2 <- data_frame(x = rnorm(1000), index = seq_along(x))

ggplot(df2) +
  geom_point(aes(x = index, y = x), alpha = 0.3) +
  labs(xlab = "Time", ylab = "Returns") +
  ggtitle("Some returns")

## Multiple plots on the plotboard

par(mfrow = c(2, 2))
plot(rnorm(100), main = "Graph 1")
plot(rnorm(100), main = "Graph 2", type = "l")
plot(rnorm(100), main = "graph 3", type = "s")
plot(rnorm(100), type = "h", main = "Graph 4")
par(mfrow = c(1, 1))

plot(rnorm(100),
     main = "A line plot",
     cex.main = 0.8,
     xlab = "x-axis",
     ylab = "y-axis",
     type = "l")

mtext("Some text at the top", side = 3)
legend(40, -1, "A legend")
formals(plot.default)

# Functional Programming

ans <- sum(1:100)
ans

answer <- 0
for(i in 1:100) {
  answer <- answer + i
}

answer

# Functions

crack_eggs <- function(number_of_eggs) {
  return(have_all_eggs_been_cracked)
}

my_boolean <- 1 == 2

if (my_boolean) {
  print("not correct")
} else {
  print("XYZ")
}

for(i in seq_along(1:5)) {
  cat(i, "\n")
}

some_list <- list()
for(z in c("hello", "goodbye")) {
  some_list[[z]] <- z
}
some_list

## Correlation

filter_and_sort_symbols <- function(symbols) {
  symbols <- toupper(symbols)
  
  valid <- stringr::str_detect(symbols, "^[A-Z]{2,4}$")
  
  return(sort(symbols[valid == 1]))
}

filter_and_sort_symbols(c("MOT", "cvx", "123", "Gog2", "XLe"))

extract_prices <- function(filtered_symbols, file_path) {
  
  # read csv
  all_prices <- read_csv(file_path)
  
  # valid columns
  valid_columns <- colnames(all_prices) %in% filtered_symbols
  
  return(all_prices[, valid_columns])
}

filtered_prices <- function(prices) {
  valid_rows <- complete.cases(prices)
  
  missing_rows <- which(valid_rows == F)
  
  return(missing_rows)
}

compute_pairwise_correlations <- function(prices) {
  
  returns <- map_df(prices, function(x) diff(log(x)))
  
  pairs(returns, main = "Pairwise return scatter plot")
  
  correlation_matrix <- cor(returns, use = "complete.obs")
  
  return(correlation_matrix)
}

symbols <- c("IBM", "XOM", "2SG", "TEva", "G0og", "CVX", "AAPL", "BA")

file_path <- "https://github.com/hgeorgako/rfortraders/raw/master/Chapter_02/prices.csv"

filtered_symbols <- filter_and_sort_symbols(symbols)
filtered_symbols

prices <- extract_prices(filtered_symbols, file_path)

missing_rows <- filtered_prices(prices)
missing_rows

correlation_matrix <- compute_pairwise_correlations(prices)
correlation_matrix

pairs(correlation_matrix)
