rm(list = ls())

data <- read.csv("Data Files/DGS10.csv")
View(data)

anyNA(data)

sum(is.na(data))

which(is.na(data[[1]]))

par(mfrow = c(1,1))
plot(data$DGS10, main = "10Y YS TS Yield Series", type = "l")

library(dplyr)
library(lubridate)

stopifnot(!anyNA(data$observation_date))   # mdy() failures would otherwise be dropped silently
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

plot(final_data$d_DGS10, main = "10Y US TS Yield Changes", type = "l")

library(tseries)
par(mfrow = c(3,1))
plot(final_data$d_DGS10, main = "10Y US TS Yield Changes", type = "l")
acf(final_data$d_DGS10, main = "10Y US TS Yield changes, ACF")
pacf(final_data$d_DGS10, main = "10Y US TS Yield changes, PACF")

final_data <- final_data %>%
  mutate(d_bps = round(100 * d_DGS10),           # integer bps (GARCH input)
         v     = d_bps^2,                        # squared change (main FARIMA proxy)
         v_abs = abs(d_bps),                     # absolute change (backup proxy)
         year  = year(observation_date),
         holiday_gap = as.integer(!(calendar_gap %in% c(1, 3))),
         regime = case_when(
           observation_date <  as.Date("1979-10-06") ~ "pre_volcker",
           observation_date <= as.Date("1982-10-31") ~ "volcker",
           observation_date >= as.Date("2008-12-16") &
           observation_date <= as.Date("2015-12-16") ~ "zlb1",
           observation_date >= as.Date("2020-03-15") &
           observation_date <= as.Date("2022-03-16") ~ "zlb2",
           TRUE ~ "normal"))

# Zeros by year: decide the start date
final_data %>% group_by(year) %>%
  summarise(share_zero = mean(d_bps == 0), sd_bps = sd(d_bps)) %>%
  print(n = 70)

START <- as.Date("1966-01-01")
# 1963-65: 47-58% of daily changes are exactly 0 and sd ~1 bp, i.e. below the
# 1 bp recording precision -> volatility not measurable; sample starts 1966
# (share of zeros <= 17%, sd >= 3 bps thereafter). Drops ~1,000 of ~16,200 obs.
final_data <- final_data %>% filter(observation_date >= START)

# Figure for the data section: level, change, squared change
par(mfrow = c(3, 1))
plot(final_data$observation_date, final_data$DGS10, type = "l",
     main = "10Y Treasury yield (%)", xlab = "", ylab = "%")
plot(final_data$observation_date, final_data$d_bps, type = "l",
     main = "Daily change in 10Y yield (bps)", xlab = "", ylab = "bps")
plot(final_data$observation_date, final_data$v, type = "l",
     main = "Squared daily change, volatility proxy (bps^2)", xlab = "", ylab = "bps^2")

# Sanity check: do the largest moves correspond to known large events?
final_data %>% arrange(desc(v)) %>% select(observation_date, DGS10, d_bps) %>% head(20)
# Top-20 moves are genuine events, not data errors (checked against news/Fed sources):
#  - 18/20 fall in the Volcker reserve-targeting regime (Oct 1979 - Oct 1982),
#    e.g. Feb 1980 bond sell-off, Mar-Apr 1980 credit controls, 17 Aug 1982 Kaufman rally
#  - 1987-10-20: day after Black Monday crash (flight to Treasuries)
#  - 2009-03-18: FOMC announces $300bn Treasury purchases (QE1)
# => Supports the 'volcker' regime dummy.

# Unit-root tests: justify differencing
library(tseries)
adf.test(final_data$DGS10);  kpss.test(final_data$DGS10)   # We expect unit root here (non-stationary)
# Level: ADF p = 0.31 -> cannot reject H0 (unit root); KPSS p < 0.01 -> reject H0 (stationarity)
# => the yield level is non-stationary (behaves like a random walk), so we model changes

adf.test(final_data$d_bps);  kpss.test(final_data$d_bps)   # We expect no unit root here (stationary)
# Changes: ADF p < 0.01 -> reject H0 (unit root); KPSS p > 0.10 -> cannot reject H0 (stationarity)
# => daily changes are stationary; differencing once is sufficient (note: tseries ADF includes trend)

# Export final shared file with ISO dates
export <- final_data %>%
  transmute(date = format(observation_date, "%Y-%m-%d"),
            DGS10, d_bps, v, v_abs, regime, calendar_gap, holiday_gap)
write.csv(export, "Data Files/dgs10_clean.csv", row.names = FALSE)
