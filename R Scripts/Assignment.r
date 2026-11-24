rm(list = ls())

data <- read.csv("Data Files/DGS10.csv")
View(data)

anyNA(data)

sum(is.na(data))

which(is.na(data[[1]]))

par(mfrow = c(1,1))
plot(data$DGS10, main = "10Y YS TS Series", type = "l")

library(dplyr)
library(lubridate)

# Parsing date format for SIFMA trading calendar index
data <- data %>%
  mutate(
    # Use mdy() for "1/2/1962" month-day-year format 
    observation_date = mdy(observation_date),
    DGS10 = suppressWarnings(as.numeric(DGS10))
  ) %>%
  arrange(observation_date)

# 1. Check any explicit NA rows in the file
na_rows <- data %>% filter(is.na(DGS10))
nrow(na_rows)

# 2. Remove non-trading / NA rows so only valid trading days remain
data_clean <- data %>%
  filter(!is.na(observation_date), !is.na(DGS10)) %>%
  distinct(observation_date, .keep_all = TRUE) # Guard against duplicate dates

# 3. Audit calendar-day gaps between consecutive trading days
data_clean <- data_clean %>%
  mutate(
    calendar_gap = as.numeric(observation_date - lag(observation_date))
  )

# Inspect any gaps longer than a normal 3-day weekend or 4-day holiday weekend
data_clean %>%
  filter(calendar_gap > 4) %>%
  select(observation_date, DGS10, calendar_gap)

# Differencing the data
data_clean <- data_clean %>%
  mutate(
    # Daily change in percentage points (e.g., 4.03 - 4.06 = -0.03)
    d_DGS10 = DGS10 - lag(DGS10),
    
    # Optional: Daily change in basis points (e.g., -3 bps)
    d_DGS10_bps = d_DGS10 * 100
  )

# Drop the first row (1962-01-02), which has NA for d_DGS10
final_data <- data_clean %>% filter(!is.na(d_DGS10))

plot(final_data$d_DGS10, main = "10Y US TS Volatility, vi (first diff., clean)", type = "l")

library(tseries)
par(mfrow = c(3,1))
plot(final_data$d_DGS10, main = "10Y US TS Volatility, vi (first diff., clean)", type = "l")
acf(final_data$d_DGS10, main = "10Y US TS Volatility, vi")
pacf(final_data$d_DGS10, main = "10Y US TS Volatility, vi")

final_data <- final_data %>%
  mutate(d_bps = round(100 * d_DGS10),           # integer bps
         v     = d_bps**2,                      # volatility proxy
         year  = year(observation_date),
         holiday_gap = as.integer(!(calendar_gap %in% c(1, 3))))

# 1. Zeros by year: decide the start date
final_data %>% group_by(year) %>%
  summarise(share_zero = mean(d_bps == 0), sd_bps = sd(d_bps)) %>%
  print(n = 70)

# 2. Long-memory diagnostics on v
par(mfrow = c(3, 1))
plot(final_data$v, type = "l", main = "squared Δi (bps)")
acf(final_data$v, lag.max = 500, main = "ACF of Δi squared")
r <- acf(final_data$v, lag.max = 1000, plot = FALSE)$acf[-1]
plot(log(1:1000), log(pmax(r, 1e-4)), xlab = "log h", ylab = "log ACF",
     main = "log-log ACF (slope ≈ 2d − 1)")


# Ciao sono Giovanni 


