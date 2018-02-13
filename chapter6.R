library(tidyverse)
library(quantmod)

pepsi <- getSymbols('PEP', from = '2013-01-01',
                    to = '2014-01-01', adjust = T, auto.assign = F)

coke <- getSymbols('COKE', from = '2013-01-01',
                   to = '2014-01-01', adjust = T, auto.assign = F)
Sys.setenv(TZ = "UTC")

prices <- bind_cols(pepsi = pepsi[, 6], coke = coke[, 6])

pepsi
prices

price_changes <- map_df(prices, diff)
price_changes

ggplot(price_changes) +
  geom_point(aes(pepsi, coke, color = pepsi), alpha = 0.3) +
  labs(title = "Pepsi vs Coke",
       xlab = "Coke price changes", ylab = "Pepsi price changes")


# prices2 <- cbind(pepsi[, 6], coke[, 6])
price_changes2 <- apply(prices2, 2, diff)

# plot(price_changes[, 1], price_changes[, 2],
#      xlab = "Coke price changes",
#      ylab = "Pepsi price changes",
#      main = "Pepsi vs. Coke",
#      cex.main = 0.8,
#      cex.lab = 0.8,
#      cex.axis = 0.8)
# grid()


ans <- lm(pepsi ~ coke, data = price_changes)
ans

beta <- ans$coefficients[2]
beta
ans2 <- lm(coke ~ pepsi, data = price_changes)
beta2 <- ans2$coefficients[2]

beta
beta2

# ans <- lm(price_changes2[, 1] ~ price_changes2[, 2])
# beta <- ans$coefficients[2]
# 
# ans2 <- lm(price_changes2[, 2] ~ price_changes2[, 1])
# beta2 <- ans2$coefficients[2]
# 
# beta
# beta2

# OLS vs TLS

SPY <- getSymbols('SPY', from = '2011-01-01',
                  to = '2012-12-31', adjust = T, auto.assign = F)
AAPL <- getSymbols('AAPL', from = '2011-01-01',
                   to = '2012-12-31', adjust = T, auto.assign = F)


tls_price <- bind_cols(SPY=SPY[,4], AAPL=AAPL[,4])
tls_price_changes <- map_df(tls_price, diff)

tls_price_changes
r <- prcomp(~SPY+AAPL, data=tls_price_changes)
r

# x <- diff(as.numeric(SPY[, 4]))
# y <- diff(as.numeric(AAPL[, 4]))
# r <- prcomp( ~ x + y )
# 
# r

slope <- r$rotation[2, 1] / r$rotation[1, 1]
intercept <- r$center[2] - slope * r$center[1]
intercept

ggplot(tls_price_changes) +
  geom_point(aes(SPY, AAPL)) +
  labs(title = "Scatter plot of returns. SPY vs. AAPL") +
  geom_smooth(aes(SPY, AAPL), method = "lm") +
  geom_smooth(aes(AAPL, SPY), method = "lm") +
  geom_abline(yintercept = intercept, slope = slope)

# Spread Composition

# 스프레드를 계산할 함수
calculate_spread <- function(x, y, beta) {
  return(y - beta * x)
}

# 주어진 시작, 종료 날짜에서 베타와 레벨을 계산할 함수
calculate_beta_and_level <- function(x, y, start_date, end_date) {
  library(xts)
  time_range <- paste(start_date, "::", end_date, sep = "")
  x <- x[time_range]
  y <- y[time_range]
  
  dx <- diff(x[time_range])
  dy <- diff(y[time_range])
  r <- prcomp( ~ dx + dy)
  
  beta <- r$rotation[2, 1] / r$rotation[1, 1]
  spread <- calculate_spread(x, y, beta)
  names(spread) <- "spread"
  level <- mean(spread, na.rm = T)
  
  outL <- list()
  outL$spread <- spread
  outL$beta <- beta
  outL$level <- level
  
  return(outL)
}

# 상단, 하단 임계치로 매수, 매도 신호를 계산하는 함수

calculate_buy_sell_signals <- function(spread, beta, level, lower_threshold, upper_threshold) {
  buy_signals <- ifelse(spread <= level - lower_threshold, 1, 0)
  sell_signals <- ifelse(spread >= level + upper_threshold, 1, 0)
  
  # output <- dplyr::bind_cols(spread=spread, buy_signals=buy_signals, sell_signals=sell_signals)
  output <- cbind(spread, buy_signals, sell_signals)
  colnames(output) <- c("spread", "buy_signals", "sell_signals")
  
  return(output)
}

## 함수 사용
start_date <- "2009-01-01"
end_date <- "2011-12-31"
x <- SPY[, 6]
y <- AAPL[, 6]

results <- calculate_beta_and_level(x, y, start_date, end_date)
results$beta
results$level

result_df <-results$spread
fortify(result_df)

autoplot.zoo(result_df, geom = "line") +
  labs(title = "AAPL - beta * SPY", ylab = "Spread Value")

ggplot(fortify(result_df), aes(Index, spread)) +
  geom_line() +
  labs(title = "AAPL - beta * SPY", ylab = "Spread Value", xlab = element_blank())

# 샘플 밖 시작, 종료 날짜

start_date_out_sample <- "2012-01-01"
end_date_out_sample <- "2012-10-22"
range <- str_c(start_date_out_sample, "::", end_date_out_sample, sep = "")
range

spread_out_of_sample <- calculate_spread(x[range], y[range], results$beta)

ggplot(fortify(spread_out_of_sample), aes(Index, AAPL.Adjusted)) +
  geom_line() + geom_hline(yintercept = results$level) +
  labs(title = "AAPL - beta * SPY")

# 트레이딩 가능한 주식 스프레드 계산

window_length <- 10
start_date <- "2011-01-01"
end_date <- "2011-12-31"
range <- str_c(start_date, "::", end_date, sep = "")
range

x <- SPY[range, 6]
y <- AAPL[range, 6]

# dF <- bind_cols(x = x, y =y)
dF <- cbind(x, y)
names(dF) <- c("x", "y")
str(dF)


run_regression <- function(dF) {
  return(coef(lm(y ~ x - 1, data = as.data.frame(dF))))
}

rolling_beta <- function(z, width) {
  rollapply(z, width = width, FUN = run_regression,
            by.column = FALSE, align = "right")
}

betas <- rolling_beta(diff(dF), 10)
dt <- merge(betas, dF)
dt$spread <- dt$y - lag(betas, 1) * dt$x
dt$spread


returns <- diff(dF) / dF
return_beta <- rolling_beta(returns, 10)
dt$spreadR <- diff(dt$y) / dt$y - return_beta * diff(dt$x) / dt$x
dt$spreadR
dt
tail(dt)

dt
# ggplot(fortify(dt), aes(Index, betas)) +
#   geom_line()

threshold <- sd(dt$spread, na.rm = T)
threshold

ggplot(fortify(dt), aes(Index, spread)) +
  geom_line() +
  geom_hline(yintercept = threshold, linetype = 2) +
  geom_hline(yintercept = -threshold, linetype = 2)

# 스프레드 수익성 평가

window_length <- 10

start_date <- "2012-01-01"
end_date <- "2013-12-31"
range <- str_c(start_date, "::", end_date, sep = "")

x <- SPY[range, 6]
x
y <- AAPL[range, 6]
y
dF <- cbind(x, y)
names(dF) <- c("x", "y")
# dF2 <- bind_cols(x=x, y=y)

beta_out_of_sample <- rolling_beta(diff(dF), 10)
beta_out_of_sample

## 매수, 매도 임계치
data_out <- merge(beta_out_of_sample, dF)
data_out$spread <- data_out$y - lag(beta_out_of_sample, 1) * data_out$x

data_out

# threshold <- sd(data_out$spread, na.rm = T)

ggplot(fortify(data_out), aes(Index, spread)) +
  geom_line() +
  geom_hline(yintercept = threshold, linetype = 2) +
  geom_hline(yintercept = -threshold, linetype = 2)

plot(data_out$spread, main = "AAPL vs. SPY out of sample",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
abline(h = threshold, lwd = 2)
abline(h = -threshold, lwd = 2)

# 매수, 매도 신호 생성
buys <- ifelse(data_out$spread > threshold, 1, 0)
sells <- ifelse(data_out$spread < -threshold, -1, 0)
data_out$signal <- buys + sells

ggplot(fortify(data_out), aes(Index, spread)) +
  geom_line() + geom_hline(yintercept = threshold, linetype = 2) +
  geom_hline(yintercept = -threshold, linetype = 2) +
  labs(title = "AAPL vs. SPY out of sample") 

point_type <- rep(NA, nrow(data_out))
buy_index <- which(data_out$signal == 1)
sell_index <- which(data_out$signal == -1)

point_type[buy_index] <- 21
point_type[sell_index] <- 24
points(data_out$spread, pch = point_type)

num_of_buy_signals <- sum(buys, na.rm = T)
num_of_sell_signals <- sum(abs(sells), na.rm = T)

# 거래 수량 계산
prev_x_qty <- 0
position <- 0
trade_size <- 100
signal <- as.numeric(data_out$signal)
signal[is.na(signal)] <- 0
beta <- as.numeric(data_out$beta_out_of_sample)

qty_x <- rep(0, length(signal))
qty_y <- rep(0, length(signal))

for(i in 1:length(signal)) {
  if(signal[i] == 1 && position == 0) {
    # buy the spread
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- -prev_x_qty
    qty_y[i] <- trade_size
    position <- 1
  }
  
  if(signal[i] == -1 && position == 0) {
    # sell the spread initially
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- prev_x_qty
    qty_y[i] <- -trade_size
    position <- -1
  }
  
  if(signal[i] == 1 && position == -1) {
    # we are short the spread and need to buy
    qty_x[i] <- -(round(beta[i] * trade_size) +
                    prev_x_qty)
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- 2 * trade_size
    position <- 1
  }
  
  if(signal[i] == -1 && position == 1) {
    # we are long the spread and need to sell
    qty_x[i] <- round(beta[i] * trade_size) + prev_x_qty
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- -2 * trade_size
    position <- -1
  }
}


qty_x[length(qty_x)] <- -sum(qty_x)
qty_y[length(qty_y)] <- -sum(qty_y)

data_out$qty_x <- qty_x
data_out$qty_y <- qty_y

data_out[1:3, ]
tail(data_out, 3)

# 수익 곡선을 계산할 함수
compute_equity_curve <- function(qty, price) {
  
  cash_buy <- ifelse(sign(qty) == 1,
                     qty * price, 0)
  cash_sell <- ifelse(sign(qty) == -1,
                      -qty * price, 0)
  position <- cumsum(qty)
  cumulative_buy <- cumsum(cash_buy)
  cumulative_sell <- cumsum(cash_sell)
  
  equity <- cumulative_sell - cumulative_buy +
    position * price
  return(equity)
}


data_out$equity_curve_x <- compute_equity_curve(data_out$qty_x, data_out$x)
data_out$equity_curve_y <- compute_equity_curve(data_out$qty_y, data_out$y)

data_out$equity_curve <- data_out$equity_curve_x + data_out$equity_curve_y
ggplot(fortify(data_out), aes(Index, equity_curve)) +
  geom_line() +
  labs(title = "AAPL / SPY spread", ylab = "P&L")

## Sharp Index

sharpe_ratio <- function(x, rf) {
  sharpe <- (mean(x, na.rm = T) - rf) / sd(x, na.rm = T)
  return(sharpe)
}

drawdown <- function(x) {
  cummax(x) - x
} 

par(mfrow = c(2,1))

equity_curve <- data_out$equity_curve_x + data_out$equity_curve_y

ggplot(fortify(data_out), aes(Index, equity_curve)) +
  geom_line() +
  labs(title = "Equity Curve")

ggplot(fortify(drawdown(equity_curve)), aes(Index, equity_curve_x)) +
  geom_line() +
  labs(title = "Drawdown of equity curve")

equity <- as.numeric(equity_curve[, 1])
equity
equity_curve_returns <- diff(equity) / equity[-length(equity)]

invalid_values <- is.infinite(equity_curve_returns) | is.nan(equity_curve_returns)

sharpe_ratio(equity_curve_returns[!invalid_values], 0.03)

omega_ratio <- function(r, T) {
  omega <- mean(pmax(r - T, 0)) / mean(pmax(T - r, 0))
  return(omega)
}

# 거래가 일어난 곳을 탐색
trade_dates <- data_out$qty_x[data_out$qty_x != 0]
duration <- as.numeric(diff(index(trade_dates)))

summary(duration)

hist(duration, breaks = 20, main = "Histogram of trade durations",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)

