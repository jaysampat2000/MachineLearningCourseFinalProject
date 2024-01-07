library(TTR)
library(quantmod)
library(dplyr)

################################## APPLE ##################################

# List AAPL stock
symbols = c("AAPL")

# Get AAPL data
getSymbols(symbols)
aapl_stock_data <- get(symbols)
aapl_date_range <- range(index(aapl_stock_data))

# Get S&P 500 data for the same date range
getSymbols("^GSPC", from = aapl_date_range[1], to = aapl_date_range[2], auto.assign = TRUE)
sp500_data <- GSPC

# Calculate S&P 500 metrics
sp500_daily_return <- Delt(Cl(sp500_data), k = 1)
sp500_ma14 <- SMA(Cl(sp500_data), n = 14)
sp500_ma30 <- SMA(Cl(sp500_data), n = 30)
sp500_rsi <- RSI(Cl(sp500_data), n = 14)

# Initialize an empty data frame for the combined AAPL data
aapl_data <- data.frame(Date = numeric(0))
window_size <- 1  # Rolling window size for beta calculation

for (symbol in symbols) {
  stock_data <- get(symbol)
  
  if (nrow(stock_data) > window_size) {
    daily_return <- Delt(Cl(stock_data), k = 1)
    future_return <- lead(daily_return, n = 1)
    ma14 <- SMA(Cl(stock_data), n = 14)
    ma7 <- SMA(Cl(stock_data), n = 7)
    ma30 <- SMA(Cl(stock_data), n = 30)
    sd14 <- runSD(Cl(stock_data), n = 14)
    sd7 <- runSD(Cl(stock_data), n = 7)
    sd30 <- runSD(Cl(stock_data), n = 30)
    rsi <- RSI(Cl(stock_data), n = 14)
    bbands <- BBands(HLC(stock_data))
    ema <- EMA(Cl(stock_data), n = 14)
    closing_prices <- Cl(stock_data)
    volume_data <- Vo(stock_data)
    vwap <- VWAP(closing_prices, volume = volume_data)
    macd_values <- MACD(closing_prices)
    momentum_value <- momentum(Cl(stock_data), n = 14)
    obv <- OBV(closing_prices, volume_data)
    
    # Calculate rolling beta
    rolling_beta <- rep(NA, nrow(stock_data))
    for (i in (window_size + 1):nrow(stock_data)) {
      if (i <= nrow(sp500_daily_return)) {
        stock_returns <- daily_return[(i-window_size):i, 1]
        market_returns <- sp500_daily_return[(i-window_size):i]
        valid_data <- !is.na(stock_returns) & !is.na(market_returns)
        if (sum(valid_data) > window_size / 2) {  # Ensure sufficient non-NA data
          rolling_beta[i] <- cov(stock_returns[valid_data], market_returns[valid_data]) / var(market_returns[valid_data])
        }
      }
    }
    
    # Create a data frame for the current symbol
    symbol_data <- data.frame(
      Date = index(stock_data),
      daily_return,
      future_return,
      ma14,
      ma7,
      ma30,
      sd14,
      sd7,
      sd30,
      rsi,
      bbands_up = bbands$up,
      bbands_mavg = bbands$mavg,
      bbands_dn = bbands$dn,
      ema,
      vwap,
      macd = macd_values$macd,
      signal = macd_values$signal,
      momentum = momentum_value,
      beta = rolling_beta,
      obv,
      sp500_daily_return = approx(index(sp500_daily_return), sp500_daily_return, index(stock_data))$y,
      sp500_ma14 = approx(index(sp500_ma14), sp500_ma14, index(stock_data))$y,
      sp500_ma30 = approx(index(sp500_ma30), sp500_ma30, index(stock_data))$y,
      sp500_rsi = approx(index(sp500_rsi), sp500_rsi, index(stock_data))$y,
      stringsAsFactors = FALSE
    )
    
    symbol_data <- symbol_data %>%
      mutate(Stock_ID = symbol)
    
    aapl_data <- merge(aapl_data, symbol_data, by = "Date", all = TRUE)
  }
}

aapl_data <- aapl_data[!is.na(aapl_data$Date),]

colnames(aapl_data)[colnames(aapl_data) == "AAPL.Close"] <- "Close"
colnames(aapl_data)[colnames(aapl_data) == "AAPL.Close.1"] <- "Close.1"

colnames(aapl_data)[colnames(aapl_data) == "Delt.1.arithmetic"] <- "daily_return"
colnames(aapl_data)[colnames(aapl_data) == "Delt.1.arithmetic.1"] <- "future_return"

################################## AMAZON ##################################

# List amzn stock
symbols = c("AMZN")

# Get amzn data
getSymbols(symbols)
amzn_stock_data <- get(symbols)
amzn_date_range <- range(index(amzn_stock_data))

# Get S&P 500 data for the same date range
getSymbols("^GSPC", from = amzn_date_range[1], to = amzn_date_range[2], auto.assign = TRUE)
sp500_data <- GSPC

# Calculate S&P 500 metrics
sp500_daily_return <- Delt(Cl(sp500_data), k = 1)
sp500_ma14 <- SMA(Cl(sp500_data), n = 14)
sp500_ma30 <- SMA(Cl(sp500_data), n = 30)
sp500_rsi <- RSI(Cl(sp500_data), n = 14)

# Initialize an empty data frame for the combined amzn data
amzn_data <- data.frame(Date = numeric(0))
window_size <- 1  # Rolling window size for beta calculation

for (symbol in symbols) {
  stock_data <- get(symbol)
  
  if (nrow(stock_data) > window_size) {
    daily_return <- Delt(Cl(stock_data), k = 1)
    future_return <- lead(daily_return, n = 1)
    ma14 <- SMA(Cl(stock_data), n = 14)
    ma7 <- SMA(Cl(stock_data), n = 7)
    ma30 <- SMA(Cl(stock_data), n = 30)
    sd14 <- runSD(Cl(stock_data), n = 14)
    sd7 <- runSD(Cl(stock_data), n = 7)
    sd30 <- runSD(Cl(stock_data), n = 30)
    rsi <- RSI(Cl(stock_data), n = 14)
    bbands <- BBands(HLC(stock_data))
    ema <- EMA(Cl(stock_data), n = 14)
    closing_prices <- Cl(stock_data)
    volume_data <- Vo(stock_data)
    vwap <- VWAP(closing_prices, volume = volume_data)
    macd_values <- MACD(closing_prices)
    momentum_value <- momentum(Cl(stock_data), n = 14)
    obv <- OBV(closing_prices, volume_data)
    
    # Calculate rolling beta
    rolling_beta <- rep(NA, nrow(stock_data))
    for (i in (window_size + 1):nrow(stock_data)) {
      if (i <= nrow(sp500_daily_return)) {
        stock_returns <- daily_return[(i-window_size):i, 1]
        market_returns <- sp500_daily_return[(i-window_size):i]
        valid_data <- !is.na(stock_returns) & !is.na(market_returns)
        if (sum(valid_data) > window_size / 2) {  # Ensure sufficient non-NA data
          rolling_beta[i] <- cov(stock_returns[valid_data], market_returns[valid_data]) / var(market_returns[valid_data])
        }
      }
    }
    
    # Create a data frame for the current symbol
    symbol_data <- data.frame(
      Date = index(stock_data),
      daily_return,
      future_return,
      ma14,
      ma7,
      ma30,
      sd14,
      sd7,
      sd30,
      rsi,
      bbands_up = bbands$up,
      bbands_mavg = bbands$mavg,
      bbands_dn = bbands$dn,
      ema,
      vwap,
      macd = macd_values$macd,
      signal = macd_values$signal,
      momentum = momentum_value,
      beta = rolling_beta,
      obv,
      sp500_daily_return = approx(index(sp500_daily_return), sp500_daily_return, index(stock_data))$y,
      sp500_ma14 = approx(index(sp500_ma14), sp500_ma14, index(stock_data))$y,
      sp500_ma30 = approx(index(sp500_ma30), sp500_ma30, index(stock_data))$y,
      sp500_rsi = approx(index(sp500_rsi), sp500_rsi, index(stock_data))$y,
      stringsAsFactors = FALSE
    )
    
    symbol_data <- symbol_data %>%
      mutate(Stock_ID = symbol)
    
    amzn_data <- merge(amzn_data, symbol_data, by = "Date", all = TRUE)
  }
}

amzn_data <- amzn_data[!is.na(amzn_data$Date),]

colnames(amzn_data)[colnames(amzn_data) == "AMZN.Close"] <- "Close"
colnames(amzn_data)[colnames(amzn_data) == "AMZN.Close.1"] <- "Close.1"

colnames(amzn_data)[colnames(amzn_data) == "Delt.1.arithmetic"] <- "daily_return"
colnames(amzn_data)[colnames(amzn_data) == "Delt.1.arithmetic.1"] <- "future_return"

################################## MICROSOFT ##################################

# List msft stock
symbols = c("MSFT")

# Get msft data
getSymbols(symbols)
msft_stock_data <- get(symbols)
msft_date_range <- range(index(msft_stock_data))

# Get S&P 500 data for the same date range
getSymbols("^GSPC", from = msft_date_range[1], to = msft_date_range[2], auto.assign = TRUE)
sp500_data <- GSPC

# Calculate S&P 500 metrics
sp500_daily_return <- Delt(Cl(sp500_data), k = 1)
sp500_ma14 <- SMA(Cl(sp500_data), n = 14)
sp500_ma30 <- SMA(Cl(sp500_data), n = 30)
sp500_rsi <- RSI(Cl(sp500_data), n = 14)

# Initialize an empty data frame for the combined msft data
msft_data <- data.frame(Date = numeric(0))
window_size <- 1  # Rolling window size for beta calculation

for (symbol in symbols) {
  stock_data <- get(symbol)
  
  if (nrow(stock_data) > window_size) {
    daily_return <- Delt(Cl(stock_data), k = 1)
    future_return <- lead(daily_return, n = 1)
    ma14 <- SMA(Cl(stock_data), n = 14)
    ma7 <- SMA(Cl(stock_data), n = 7)
    ma30 <- SMA(Cl(stock_data), n = 30)
    sd14 <- runSD(Cl(stock_data), n = 14)
    sd7 <- runSD(Cl(stock_data), n = 7)
    sd30 <- runSD(Cl(stock_data), n = 30)
    rsi <- RSI(Cl(stock_data), n = 14)
    bbands <- BBands(HLC(stock_data))
    ema <- EMA(Cl(stock_data), n = 14)
    closing_prices <- Cl(stock_data)
    volume_data <- Vo(stock_data)
    vwap <- VWAP(closing_prices, volume = volume_data)
    macd_values <- MACD(closing_prices)
    momentum_value <- momentum(Cl(stock_data), n = 14)
    obv <- OBV(closing_prices, volume_data)
    
    # Calculate rolling beta
    rolling_beta <- rep(NA, nrow(stock_data))
    for (i in (window_size + 1):nrow(stock_data)) {
      if (i <= nrow(sp500_daily_return)) {
        stock_returns <- daily_return[(i-window_size):i, 1]
        market_returns <- sp500_daily_return[(i-window_size):i]
        valid_data <- !is.na(stock_returns) & !is.na(market_returns)
        if (sum(valid_data) > window_size / 2) {  # Ensure sufficient non-NA data
          rolling_beta[i] <- cov(stock_returns[valid_data], market_returns[valid_data]) / var(market_returns[valid_data])
        }
      }
    }
    
    # Create a data frame for the current symbol
    symbol_data <- data.frame(
      Date = index(stock_data),
      daily_return,
      future_return,
      ma14,
      ma7,
      ma30,
      sd14,
      sd7,
      sd30,
      rsi,
      bbands_up = bbands$up,
      bbands_mavg = bbands$mavg,
      bbands_dn = bbands$dn,
      ema,
      vwap,
      macd = macd_values$macd,
      signal = macd_values$signal,
      momentum = momentum_value,
      beta = rolling_beta,
      obv,
      sp500_daily_return = approx(index(sp500_daily_return), sp500_daily_return, index(stock_data))$y,
      sp500_ma14 = approx(index(sp500_ma14), sp500_ma14, index(stock_data))$y,
      sp500_ma30 = approx(index(sp500_ma30), sp500_ma30, index(stock_data))$y,
      sp500_rsi = approx(index(sp500_rsi), sp500_rsi, index(stock_data))$y,
      stringsAsFactors = FALSE
    )
    
    symbol_data <- symbol_data %>%
      mutate(Stock_ID = symbol)
    
    msft_data <- merge(msft_data, symbol_data, by = "Date", all = TRUE)
  }
}

msft_data <- msft_data[!is.na(msft_data$Date),]

colnames(msft_data)[colnames(msft_data) == "MSFT.Close"] <- "Close"
colnames(msft_data)[colnames(msft_data) == "MSFT.Close.1"] <- "Close.1"

colnames(msft_data)[colnames(msft_data) == "Delt.1.arithmetic"] <- "daily_return"
colnames(msft_data)[colnames(msft_data) == "Delt.1.arithmetic.1"] <- "future_return"

################################## GOOGLE ##################################

# List goog stock
symbols = c("GOOG")

# Get goog data
getSymbols(symbols)
goog_stock_data <- get(symbols)
goog_date_range <- range(index(goog_stock_data))

# Get S&P 500 data for the same date range
getSymbols("^GSPC", from = goog_date_range[1], to = goog_date_range[2], auto.assign = TRUE)
sp500_data <- GSPC

# Calculate S&P 500 metrics
sp500_daily_return <- Delt(Cl(sp500_data), k = 1)
sp500_ma14 <- SMA(Cl(sp500_data), n = 14)
sp500_ma30 <- SMA(Cl(sp500_data), n = 30)
sp500_rsi <- RSI(Cl(sp500_data), n = 14)

# Initialize an empty data frame for the combined goog data
goog_data <- data.frame(Date = numeric(0))
window_size <- 1  # Rolling window size for beta calculation

for (symbol in symbols) {
  stock_data <- get(symbol)
  
  if (nrow(stock_data) > window_size) {
    daily_return <- Delt(Cl(stock_data), k = 1)
    future_return <- lead(daily_return, n = 1)
    ma14 <- SMA(Cl(stock_data), n = 14)
    ma7 <- SMA(Cl(stock_data), n = 7)
    ma30 <- SMA(Cl(stock_data), n = 30)
    sd14 <- runSD(Cl(stock_data), n = 14)
    sd7 <- runSD(Cl(stock_data), n = 7)
    sd30 <- runSD(Cl(stock_data), n = 30)
    rsi <- RSI(Cl(stock_data), n = 14)
    bbands <- BBands(HLC(stock_data))
    ema <- EMA(Cl(stock_data), n = 14)
    closing_prices <- Cl(stock_data)
    volume_data <- Vo(stock_data)
    vwap <- VWAP(closing_prices, volume = volume_data)
    macd_values <- MACD(closing_prices)
    momentum_value <- momentum(Cl(stock_data), n = 14)
    obv <- OBV(closing_prices, volume_data)
    
    # Calculate rolling beta
    rolling_beta <- rep(NA, nrow(stock_data))
    for (i in (window_size + 1):nrow(stock_data)) {
      if (i <= nrow(sp500_daily_return)) {
        stock_returns <- daily_return[(i-window_size):i, 1]
        market_returns <- sp500_daily_return[(i-window_size):i]
        valid_data <- !is.na(stock_returns) & !is.na(market_returns)
        if (sum(valid_data) > window_size / 2) {  # Ensure sufficient non-NA data
          rolling_beta[i] <- cov(stock_returns[valid_data], market_returns[valid_data]) / var(market_returns[valid_data])
        }
      }
    }
    
    # Create a data frame for the current symbol
    symbol_data <- data.frame(
      Date = index(stock_data),
      daily_return,
      future_return,
      ma14,
      ma7,
      ma30,
      sd14,
      sd7,
      sd30,
      rsi,
      bbands_up = bbands$up,
      bbands_mavg = bbands$mavg,
      bbands_dn = bbands$dn,
      ema,
      vwap,
      macd = macd_values$macd,
      signal = macd_values$signal,
      momentum = momentum_value,
      beta = rolling_beta,
      obv,
      sp500_daily_return = approx(index(sp500_daily_return), sp500_daily_return, index(stock_data))$y,
      sp500_ma14 = approx(index(sp500_ma14), sp500_ma14, index(stock_data))$y,
      sp500_ma30 = approx(index(sp500_ma30), sp500_ma30, index(stock_data))$y,
      sp500_rsi = approx(index(sp500_rsi), sp500_rsi, index(stock_data))$y,
      stringsAsFactors = FALSE
    )
    
    symbol_data <- symbol_data %>%
      mutate(Stock_ID = symbol)
    
    goog_data <- merge(goog_data, symbol_data, by = "Date", all = TRUE)
  }
}

goog_data <- goog_data[!is.na(goog_data$Date),]

colnames(goog_data)[colnames(goog_data) == "GOOG.Close"] <- "Close"
colnames(goog_data)[colnames(goog_data) == "GOOG.Close.1"] <- "Close.1"

colnames(goog_data)[colnames(goog_data) == "Delt.1.arithmetic"] <- "daily_return"
colnames(goog_data)[colnames(goog_data) == "Delt.1.arithmetic.1"] <- "future_return"

################################## NVDIA ##################################

# List nvda stock
symbols = c("NVDA")

# Get nvda data
getSymbols(symbols)
nvda_stock_data <- get(symbols)
nvda_date_range <- range(index(nvda_stock_data))

# Get S&P 500 data for the same date range
getSymbols("^GSPC", from = nvda_date_range[1], to = nvda_date_range[2], auto.assign = TRUE)
sp500_data <- GSPC

# Calculate S&P 500 metrics
sp500_daily_return <- Delt(Cl(sp500_data), k = 1)
sp500_ma14 <- SMA(Cl(sp500_data), n = 14)
sp500_ma30 <- SMA(Cl(sp500_data), n = 30)
sp500_rsi <- RSI(Cl(sp500_data), n = 14)

# Initialize an empty data frame for the combined nvda data
nvda_data <- data.frame(Date = numeric(0))
window_size <- 1  # Rolling window size for beta calculation

for (symbol in symbols) {
  stock_data <- get(symbol)
  
  if (nrow(stock_data) > window_size) {
    daily_return <- Delt(Cl(stock_data), k = 1)
    future_return <- lead(daily_return, n = 1)
    ma14 <- SMA(Cl(stock_data), n = 14)
    ma7 <- SMA(Cl(stock_data), n = 7)
    ma30 <- SMA(Cl(stock_data), n = 30)
    sd14 <- runSD(Cl(stock_data), n = 14)
    sd7 <- runSD(Cl(stock_data), n = 7)
    sd30 <- runSD(Cl(stock_data), n = 30)
    rsi <- RSI(Cl(stock_data), n = 14)
    bbands <- BBands(HLC(stock_data))
    ema <- EMA(Cl(stock_data), n = 14)
    closing_prices <- Cl(stock_data)
    volume_data <- Vo(stock_data)
    vwap <- VWAP(closing_prices, volume = volume_data)
    macd_values <- MACD(closing_prices)
    momentum_value <- momentum(Cl(stock_data), n = 14)
    obv <- OBV(closing_prices, volume_data)
    
    # Calculate rolling beta
    rolling_beta <- rep(NA, nrow(stock_data))
    for (i in (window_size + 1):nrow(stock_data)) {
      if (i <= nrow(sp500_daily_return)) {
        stock_returns <- daily_return[(i-window_size):i, 1]
        market_returns <- sp500_daily_return[(i-window_size):i]
        valid_data <- !is.na(stock_returns) & !is.na(market_returns)
        if (sum(valid_data) > window_size / 2) {  # Ensure sufficient non-NA data
          rolling_beta[i] <- cov(stock_returns[valid_data], market_returns[valid_data]) / var(market_returns[valid_data])
        }
      }
    }
    
    # Create a data frame for the current symbol
    symbol_data <- data.frame(
      Date = index(stock_data),
      daily_return,
      future_return,
      ma14,
      ma7,
      ma30,
      sd14,
      sd7,
      sd30,
      rsi,
      bbands_up = bbands$up,
      bbands_mavg = bbands$mavg,
      bbands_dn = bbands$dn,
      ema,
      vwap,
      macd = macd_values$macd,
      signal = macd_values$signal,
      momentum = momentum_value,
      beta = rolling_beta,
      obv,
      sp500_daily_return = approx(index(sp500_daily_return), sp500_daily_return, index(stock_data))$y,
      sp500_ma14 = approx(index(sp500_ma14), sp500_ma14, index(stock_data))$y,
      sp500_ma30 = approx(index(sp500_ma30), sp500_ma30, index(stock_data))$y,
      sp500_rsi = approx(index(sp500_rsi), sp500_rsi, index(stock_data))$y,
      stringsAsFactors = FALSE
    )
    
    symbol_data <- symbol_data %>%
      mutate(Stock_ID = symbol)
    
    nvda_data <- merge(nvda_data, symbol_data, by = "Date", all = TRUE)
  }
}

nvda_data <- nvda_data[!is.na(nvda_data$Date),]

colnames(nvda_data)[colnames(nvda_data) == "NVDA.Close"] <- "Close"
colnames(nvda_data)[colnames(nvda_data) == "NVDA.Close.1"] <- "Close.1"

colnames(nvda_data)[colnames(nvda_data) == "Delt.1.arithmetic"] <- "daily_return"
colnames(nvda_data)[colnames(nvda_data) == "Delt.1.arithmetic.1"] <- "future_return"


save(aapl_data, amzn_data, msft_data, goog_data, nvda_data, file = "stocks_data_obj.RData")


write.csv(aapl_data, file = 'aapl_data')







######## OPTIONAL: MAKE LONG DATASET WITH ALL STOCKS ##########

# Make a final dataframe with all the stock data combined
df_final<- rbind(aapl_data, amzn_data, msft_data, goog_data, nvda_data)


write.csv(df_final, file = 'final_stock_data')

############# VISUALIZATIONS ###############

library(ggplot2)

# 1. Box Plot for Daily Returns (Compare Volatility)
ggplot(df_final, aes(x = Stock_ID, y = daily_return, fill = Stock_ID)) +
  geom_boxplot() +
  labs(title = "Box Plot of Daily Returns for Each Stock", y = "Daily Return", x = "Stock ID")

# 2. Time Series Plot for Moving Averages and Closing Prices
ggplot(df_final, aes(x = Date)) +
  geom_line(aes(y = Close, color = Stock_ID)) +
  geom_line(aes(y = SMA, linetype = "SMA 14 days")) +
  labs(title = "Moving Averages and Closing Prices Over Time", y = "Price", x = "Date") +
  facet_wrap(~Stock_ID)


# 3. Plot for RSI for each Stock

df_final$Date <- as.Date(df_final$Date) #To ensure that 'Date' is a Date type

ggplot(df_final, aes(x = Date)) +
  geom_line(aes(y = rsi, color = "RSI")) +
  facet_wrap(~ Stock_ID, scales = "free_y") +
  labs(title = "RSI for Each Stock",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("RSI" = "purple")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 4. Plot for MACD and signal for each stock

ggplot(df_final, aes(x = Date)) +
  geom_line(aes(y = macd, color = "MACD")) +
  geom_line(aes(y = signal, color = "Signal")) +
  facet_wrap(~ Stock_ID, scales = "free_y") +
  labs(title = "Overlay of MACD and Signal Line for Each Stock",
       x = "Date",
       y = "Value") +
  scale_color_manual(values = c("MACD" = "red", "Signal" = "skyblue")) +
  theme_minimal() +
  theme(legend.position = "bottom")




################ Lasso ################ 

# Load necessary libraries
library(glmnet)
library(caret)

# Preprocess the data
data <- na.omit(aapl_data)  # Remove rows with NAs
data$Date <- NULL      # Remove the Date column if it's not needed
data$Stock_ID <- NULL  # Remove the Stock_ID column

# Splitting the data into training and testing sets
set.seed(111111)  # for reproducibility
index <- createDataPartition(data$future_return, p = .8, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

# Standardize the features
preProcValues <- preProcess(train_data, method = c("center", "scale"))

train_data <- predict(preProcValues, train_data)
test_data <- predict(preProcValues, test_data)

# Extracting the predictors and response from the training data
x_train <- as.matrix(train_data[, -which(names(train_data) == "future_return")])
y_train <- train_data$future_return

# Fitting a Lasso model
lasso_model <- glmnet(x_train, y_train, alpha = 1)

# Optionally, use cross-validation to find the optimal lambda
cv_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda <- cv_lasso$lambda.min

# Refit the model using the best lambda
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

# Making predictions on the test data
x_test <- as.matrix(test_data[, -which(names(test_data) == "future_return")])
predictions <- predict(lasso_model, s = best_lambda, newx = x_test)

# Evaluate the model (e.g., using RMSE)
y_test <- test_data$future_return
rmse <- sqrt(mean((predictions - y_test)^2))
print(rmse)






################ XGBoost ################ 

library(xgboost)
library(caret)
library(ggplot2)
library(SHAPforxgboost)
library(gridExtra)

# Assuming aapl_clean is already created and is the cleaned version of your data
aapl_clean <- aapl_data[,-c(1, 18, 25)]
aapl_clean <- na.omit(aapl_clean)

# Split data into training and test sets
set.seed(111111)
total_obs <- nrow(aapl_clean)
split_index <- floor(0.8 * total_obs)
train_data <- aapl_clean[1:split_index,]
test_data <- aapl_clean[(split_index + 1):total_obs,]

# Prepare data for XGBoost
train_data_matrix <- as.matrix(train_data[, -which(names(train_data) == "future_return")])
train_labels <- train_data$future_return
test_data_matrix <- as.matrix(test_data[, -which(names(test_data) == "future_return")])
test_labels <- test_data$future_return

# Convert data to xgb.DMatrix format
dtrain <- xgb.DMatrix(data = train_data_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_data_matrix, label = test_labels)

# Model parameters
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.7,
  colsample_bytree = 0.7
)

# Training with cross-validation
xgb_cv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 1000,
  nfold = 10,
  showsd = TRUE,
  stratified = TRUE,
  print_every_n = 10,
  early_stopping_rounds = 100,
  maximize = FALSE
)

# Best number of rounds
best_nrounds <- xgb_cv$best_iteration

# Train the model
final_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds
)

# Make predictions
predictions <- predict(final_model, dtest)

# Convert predictions to binary classification (e.g., positive or negative return)
threshold <- 0  # Set a threshold for positive return
predicted_class <- ifelse(predictions > threshold, "Positive", "Negative")
actual_class <- ifelse(test_labels > threshold, "Positive", "Negative")

# Create factor class for confusion matrix
predicted_class <- factor(predicted_class, levels = c("Negative", "Positive"))
actual_class <- factor(actual_class, levels = c("Negative", "Positive"))

# Create and print confusion matrix
conf_matrix <- confusionMatrix(predicted_class, actual_class, positive = "Positive")
print(conf_matrix)

# Feature Importance
importance_matrix <- xgb.importance(feature_names = colnames(train_data_matrix), model = final_model)
xgb.plot.importance(importance_matrix)

# Calculate SHAP values
shap_values <- predict(final_model, dtest, predcontrib = TRUE)

# Create a data frame of feature names and their corresponding SHAP values
shap_long <- SHAPforxgboost::shap.prep(xgb_model = final_model, X_train = test_data_matrix)

# Plot the SHAP values (summary plot)
shap.plot.summary(shap_long)

# Plotting actual vs predicted values
plot_data_xgb <- data.frame(
  Actual = test_labels,
  Predicted = predictions
)

ggplot(plot_data_xgb, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(title = "Actual vs Predicted Values", x = "Actual Future Return", y = "Predicted Future Return")





# Define a grid of hyperparameters to explore
etas <- c(0.01, 0.05, 0.1, 0.3)
max_depths <- c(3, 6, 9, 12)

# Initialize an empty list to store results
results <- list()

# Loop over eta and max_depth values
for(eta in etas) {
  for(max_depth in max_depths) {
    set.seed(111111)
    params <- list(
      booster = "gbtree",
      objective = "reg:squarederror",
      eta = eta,
      max_depth = max_depth,
      subsample = 0.7,
      colsample_bytree = 0.7
    )
    
    cv_model <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 100,
      nfold = 5,
      showsd = TRUE,
      stratified = TRUE,
      print_every_n = 10,
      early_stopping_rounds = 10,
      maximize = FALSE
    )
    
    # Store the results
    results[[paste("eta:", eta, "max_depth:", max_depth)]] <- cv_model
  }
}

# Function to extract RMSE and plot
extract_rmse <- function(cv_model, title) {
  data.frame(
    Round = 1:nrow(cv_model$evaluation_log),
    RMSE = cv_model$evaluation_log$test_rmse_mean
  ) %>%
    ggplot(aes(x = Round, y = RMSE)) +
    geom_line() +
    ggtitle(title) +
    theme_minimal()
}

# Plot RMSE for different hyperparameter combinations
plots <- lapply(names(results), function(name) {
  extract_rmse(results[[name]], name)
})

# Arrange plots in a grid
do.call(grid.arrange, c(plots, ncol = 2))



