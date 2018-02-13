library(tidyverse)

aapl_2 <- read_csv("https://github.com/hgeorgako/rfortraders/raw/master/Chapter_03/aapl.csv")

aapl_2 %>% 
  mutate(Date = parse_date(Date, format = "%m/%d/%y")) %>% 
  arrange(Date) %>% 
  ggplot(aes(Date, Close)) +
  geom_line()

# Package Installation

install.packages("quantmod")
library(quantmod)

# Saving data and transferring

install.packages("RJSONIO")
library(RJSONIO)

out <- fromJSON(content = "https://github.com/hgeorgako/rfortraders/raw/master/Chapter_03/sample_json_file.json")
str(out)

## Dplyr

library(hflights)
glimpse(hflights)

hflights %>% 
  group_by(UniqueCarrier) %>% 
  summarise(avg_delay = mean(ArrDelay, na.rm = T))

# xts package usage

library(xts)

data("sample_matrix")
glimpse(sample_matrix)

str(sample_matrix)
xts_matrix <- as.xts(sample_matrix, descr = 'my new xts object')
xts_matrix

str(xts_matrix)

plot(xts_matrix[,1], main = "Our first xts plot",
     cex.main = 0.8)
plot(xts_matrix, main = "Candle plot on xts object", cex.main = 0.8, type = "candles")

start_date <- "2007-05-05"
end_date <- "2007-12-31"

plot(xts_matrix[paste(start_date, "::",
                      end_date, sep = "")])

price_vector <- c(101.02, 101.03, 101.03, 101.04, 101.05, 101.03, 101.02, 101.01, 101.00, 100.99)

dates <- c("03/12/2013 08:00:00.532123",
           "03/12/2013 08:00:01.982333",
           "03/12/2013 08:00:01.650321",
           "03/12/2013 08:00:02.402321",
           "03/12/2013 08:00:02.540432",
           "03/12/2013 08:00:03.004554",
           "03/12/2013 08:00:03.900213",
           "03/12/2013 08:00:04.050323",
           "03/12/2013 08:00:04.430345",
           "03/12/2013 08:00:05.700123")

time_index <- parse_datetime(dates, format = "%d/%m/%Y %H:%M:%OS")
time_index

xts_price_vector <- xts(price_vector, time_index)
xts_price_vector

options(digits.secs = 6)

plot(xts_price_vector, main = "Fictitious Price Series",
     cex.main = 0.8)

abline(h = mean(xts_price_vector), lwd = 2)
my_time <- parse_datetime("03/12/2013 08:00:03.004554", format = "%d/%m/%Y %H:%M:%OS")

abline(v = my_time, lwd = 2, lty = 2)

glimpse(xts_price_vector)
xts_price_vector

dygraph(xts_price_vector, ylab = "Close", main = "Fictitious price series")

# S&P 500 E-mini

es_price <- c(1700.00, 1700.25, 1700.50, 1700.00, 1700.75,
              1701.25, 1701.25, 1701.25, 1700.75, 1700.50)

es_time  <- c("09/12/2013 08:00:00.532123",
              "09/12/2013 08:00:01.982333",
              "09/12/2013 08:00:05.650321",
              "09/12/2013 08:10:02.402321",
              "09/12/2013 08:12:02.540432",
              "09/12/2013 08:12:03.004554",
              "09/12/2013 08:14:03.900213",
              "09/12/2013 08:15:07.090323",
              "09/12/2013 08:16:04.430345",
              "09/12/2013 08:18:05.700123")
xts_es <- xts(es_price, parse_datetime(es_time, format = "%d/%m/%Y %H:%M:%OS"))

names(xts_es) <- c("price")

time_diff <- difftime(index(xts_es)[2], index(xts_es)[1], units = "secs")
time_diff

diffs <- c()

for(i in 2:length(index(xts_es))) {
  diffs[i] <- difftime(index(xts_es)[i], index(xts_es)[i - 1], units = "secs")
}

diffs

diffs <- index(xts_es)[-1] - index(xts_es)[-length(index(xts_es))]
diffs

class(diffs)

es_times <- index(xts_es)
diffs <- es_times[-1] - es_times[-length(es_times)]
diffs

par(mfrow = c(2, 1))
diffs <- as.numeric(diffs)
plot(diffs, main = "Time difference in seconds for ES trades",
     xlab = "", ylab = "Time differences",
     cex.lab = 0.8,
     cex.main = 0.8)
grid()

hist(diffs, main = "Time difference in seconds for ES trades",
     xlab = "Time difference (secs)", ylab = "Observations",
     breaks = 20,
     cex.lab = 0.8,
     cex.main = 0.8)
grid()

# using quantmod package

library(quantmod)
AAPL <- getSymbols("AAPL", auto.assign = F)

tail(AAPL)

chartSeries(AAPL, subset = "2010::2010-04",
            theme = chartTheme("white"),
            TA = "addVo(); addBBands()")
reChart(subset='2009-01-01::2009-03-03')

reChart(subset='2018-01-01::')

## initial chart
chartSeries(AAPL, theme = chartTheme("white"), TA = NULL)

my_indicator <- function(x) {
  return(x + 90)
}

add_my_indicator <- newTA(FUN = my_indicator, preFUN = Cl,
                          legend.name = "My Fancy Indicator", on = 1)
add_my_indicator()

# using ggplot

df <- AAPL[, c("AAPL.Adjusted", "AAPL.Volume")]
names(df) <- c("price", "volume")

df$return <- diff(log(df[, 1]))
df <- df[-1, ]

df$cuts <- cut(abs(df$return),
               breaks = c(0, 0.02, 0.04, 0.25),
               include.lowest = T)

df$means <- NA

for(i in 1:3) {
  group <- which(df$cuts == i)
  if(length(group) > 0) {
    df$means[group] <- mean(df$volume[group])
  }
}

df

ggplot(df) +
  geom_histogram(aes(volume)) +
  facet_grid(cuts ~.) +
  geom_vline(aes(xintercept=means), linetype = "dashed", size = 1)
