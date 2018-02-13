library(tidyverse)

library(quantmod)

getSymbols("SPY")

# 주가를 뽑아내 통계치를 계산함
prices <- SPY$SPY.Adjusted
mean_prices <- round(mean(prices), 2)
sd_prices <- round(sd(prices), 2)

# 범례가 포함된 히스토그램 생성

ggplot(SPY) +
  geom_histogram(aes(SPY.Adjusted), bins = 100) +
  geom_vline(xintercept = mean_prices, linetype = 2) +
  labs(title = "Histogram of prices")

plot_4_ranges <- function(data, start_date, end_date, title) {
  
  # 2행 2열로 그래프 설정
  par(mfrow = c(2, 2))
  
  for(i in 1:4){
    range <- paste(start_date[i], "::", end_date[i], sep = "")
    
    time_series <- data[range]
    
    mean_data <- round(mean(time_series, na.rm = T), 3)
    sd_data = round(sd(time_series, na.rm = T), 3)
    
    hist_title <- paste(title, range)
    hist(time_series, breaks = 100, prob = T,
         xlab = "", main = hist_title, cex.main = 0.8)
    legend("topright", cex = 0.7, bty = 'n',
           paste("mean=", mean_data, "; sd=", sd_data))
  }
  
  par(mfrow = c(1,1))
}

begin_dates <- c("2007-01-01", "2008-06-06",
                 "2009-10-10", "2011-03-03")
end_dates <- c("2008-06-05", "2009-09-09",
               "2010-12-30", "2013-01-06")

plot_4_ranges(prices, begin_dates, end_dates, "SPY prices for:")

# log profit calculation

returns <- diff(log(prices))
returns

# 수익률 그래프 그리는 함수 사용
plot_4_ranges(returns, begin_dates, end_dates, "SPY log prices for:")

# normality test with URCA
library(urca)
test <- ur.kpss(as.numeric(SPY$SPY.Adjusted))

class(test)
test@teststat

test@cval

spy_returns <- diff(log(SPY$SPY.Adjusted))

test_returns <- ur.kpss(as.numeric(spy_returns))
test_returns@teststat

test_returns@cval

test_post_2013 <- ur.kpss(as.numeric(spy_returns['2013::']))
test_post_2013@teststat

# 정규성 가정

mu <- mean(returns, na.rm = T)
sigma <- sd(returns, na.rm = T)
x <- seq(-5 * sigma, 5 * sigma, length.out = nrow(returns))

hist(returns, breaks = 100, main = "Histogram of returns for SPY",
     cex.main = 0.8, prob = T)
lines(x, dnorm(x, mu, sigma), col = "red", lwd = 2)

# ggplot way

ggplot(returns, aes(SPY.Adjusted)) +
  geom_histogram(bins = 100) +
  geom_density() +
  ggtitle("Histogram of returns for SPY") +
  scale_y_continuous(labels = scales::percent)

?geom_density

# 그래프 창 설정
par(mfrow = c(1,2))

# Spy 데이터
qqnorm(as.numeric(returns),
       main = "SPY empirical returns qqplot()",
       cex.main = 0.8)
qqline(as.numeric(returns), lwd = 2)
grid()

# 정규분포를 따르는 임의의 데이터
normal_data <- rnorm(nrow(returns), mean = mu, sd = sigma)
qqnorm(normal_data, main = "Normal Returns", cex.main = 0.8)
qqline(normal_data, lwd = 2)
grid()

answer <- shapiro.test(as.numeric(returns))
answer[[2]]
answer$p.value

# 정규성 테스트2

set.seed(129)
normal_numbers <- rnorm(5000, 0, 1)

ans <- shapiro.test(normal_numbers)

ans$p.value

normal_numbers[50] <- 1000
ans <- shapiro.test(normal_numbers)

ans$p.value
hist(normal_numbers)

# 상관관계

getSymbols("VXX")
returns_matrix <- cbind(diff(log(SPY)), diff(log(VXX)))
returns_matrix
sv <- as.xts(returns_matrix["2009-02-02::", c(4, 10)])

head(sv)

ggplot(sv) +
  geom_point(aes(SPY.Close, VXX.Close))

cor(sv)
sv

outliers <- which(sv[, 2] > 1.0)

if(length(outliers) > 0){
  sv <- sv[-outliers, ]
}

cor(sv)

# R 구조식

my_formula <- as.formula("y ~ x")
my_formula
class(my_formula)

reg <- lm(VXX.Close ~ SPY.Close, data = sv)
summary(reg)

b <- reg$coefficients[1]
a <- reg$coefficients[2]

par(mfrow = c(2,2))
plot(reg$residuals,
     main = "Residuals through time",
     xlab = "Days", ylab = "Residuals")
hist(reg$residuals, breaks = 100,
     main = "Distribution of residuals",
     xlab = "Residuals")
qqnorm(reg$residuals)
qqline(reg$residuals)
acf(reg$residuals, main = "Autocorreclation")

vxx_lag_1 <- lag(VXX$VXX.Close, k = 1)
head(vxx_lag_1)
head(VXX$VXX.Close)

sv <- merge(sv, lag(sv[,1]), lag(sv[,2]))

str(sv)

ggplot(sv) +
  geom_point(aes(SPY.Close.1, VXX.Close), alpha = 0.3) +
  labs(title = "Scatter plot SPY lagged vs. VXX.",
       xlab = "SPY lagged", ylab = "VXX")

reg2 <- lm(VXX.Close ~ SPY.Close.1, data = sv)
summary(reg2)

par(mfrow = c(1,1))

ccf(as.numeric(sv[, 1]), as.numeric(sv[, 2]),
    main = "Cross correlation between SPY and VXX",
    ylab = "Cross correlation", xlab = "Lag", cex.main = 0.8, cex.lab = 0.8,
    cex.axis = 0.8, na.action = na.pass)


# ggplot.ccf = function(x, y, lag.min=NULL, lag.max=NULL) {
#   ccf.data = ccf(x,y,plot=F)
#   
#   indices = which(ccf.data$lag[,1,1] %in% lag.min:lag.max)
#   ccf.df = data.frame(lag = ccf.data$lag[indices,1,1],
#                       correlation = ccf.data$acf[indices,1,1])
#   
#   ggplot(ccf.df,
#          aes(x = lag, y = correlation)) +
#     geom_bar(stat = 'identity') +
#     geom_hline(yintercept=.3, color = 'blue', linetype = 'dashed') +
#     geom_hline(yintercept=-.3, color = 'blue', linetype = 'dashed')
# }
# 

## 선형 회귀 분석의 선형성

x <- seq(1:100)
y <- x ^2

qplot(x, y)

reg_parabola <- lm(y ~ x)

ggplot(data_frame(x, y), aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = 1, se = F)
summary(reg_parabola)

z <- sqrt(y)
reg_transformed <- lm(z ~ x)

ggplot(data_frame(x, z), aes(x, z)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = 1, se = F)

# 변동성

z <- rnorm(1000, 0, 1)
par(mfrow = c(2, 1))
acf(z, main = "returns", cex.main = 0.8,
    cex.lab = 0.8, cex.axis = 0.8)
grid()
acf(z^2, main = "returns squared",
    cex.lab = 0.8, cex.axis = 0.8)
grid()

par(mfrow = c(1, 1))
acf(sv[, 1] ^ 2, main = "Actual returns squared",
    cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, na.action = na.pass)
grid()

par(mfrow = c(1, 2))
acf(sv[, 1]^3, na.action = na.pass)
acf(abs(sv[,1]), na.action = na.pass)

