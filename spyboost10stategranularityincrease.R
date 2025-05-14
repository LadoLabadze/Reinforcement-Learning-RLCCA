





CapitalUSD=1000
DataSIze=13000
# EURUSD_mt5_ticks <- read.csv("EURUSD_mt5_ticks.csv",sep=""  )
# 
# EURUSD_mt5_ticks=EURUSD_mt5_ticks[c(5000:(5000+DataSIze)),]
# 
# data=EURUSD_mt5_ticks
# 
# dim(data)
# 
# head(data)
# View(data)
SPYUSUSD_mt5_bars <- read_csv("SPYUSUSD2years.csv")

SPYUSUSD_mt5_bars=as.data.frame(SPYUSUSD_mt5_bars)


SPYUSUSD_mt5_bars=SPYUSUSD_mt5_bars[c(1:(1+DataSIze)),]

data=SPYUSUSD_mt5_bars
head(data)
#
#View(data)













# Required packages
library(TTR)
library(quantmod)
library(dplyr)
library(zoo)




# state 



# Rename columns for clarity (if needed)
colnames(data) <- tolower(colnames(data))  # Ensure all lower-case
names(data) <- gsub(" ", "", names(data))  # Remove accidental white space

# --- Calculate Technical Indicators ---

# 1. Relative Strength Index (RSI)
data$rsi <- RSI(data$close, n = 14)

data=na.omit(data)


# 2. Simple Moving Average (SMA)
data$sma <- SMA(data$close, n = 14)
data=na.omit(data)


# 3. Exponential Moving Average (EMA)
data$ema <- EMA(data$close, n = 14)
data=na.omit(data)


# 4. Stochastic Oscillator (%K)
stoch_vals <- stoch(HLC(data[, c("high", "low", "close")]), nFastK = 14)
data$stoch_k <- stoch_vals[,1]
data=na.omit(data)


# 5. Moving Average Convergence Divergence (MACD)
macd_vals <- MACD(data$close, nFast = 12, nSlow = 26, nSig = 9)
data$macd <- macd_vals[, 1]  # MACD line
data=na.omit(data)


# 6. Accumulation/Distribution Oscillator (A/D)
data$ad <- chaikinAD(HLC(data[, c("high", "low", "close")]), volume = data$volume)
data=na.omit(data)


# 7. On-Balance Volume (OBV)
data$obv <- OBV(data$close, data$volume)
data=na.omit(data)


# 8. Price Rate Of Change (ROC)
data$roc <- ROC(data$close, n = 14)
data=na.omit(data)


# 9. Williams %R

data$williams_r <- WPR(HLC(data[, c("high", "low", "close")]), n = 14)
data=na.omit(data)


# 10. Disparity Index (percentage difference from EMA)
data$disparity_index <- 100 * ((data$close - data$ema) / data$ema)

# Remove initial NA rows from rolling indicators
data <- na.omit(data)

# --- Final Output ---
head(data[, c("timestamp", "close", "rsi", "sma", "ema", "stoch_k", "macd", 
              "ad", "obv", "roc", "williams_r", "disparity_index")], 10)



#state id

get_rsi_level <- function(x) {
  if (x < 30) return("RSI_Low")
  else if (x > 70) return("RSI_High")
  else return("RSI_Mid")
}

get_macd_level <- function(x) {
  if (x > 0) return("MACD_Pos")
  else return("MACD_Neg")
}

get_sma_ema_level <- function(close, sma, ema) {
  if (close > sma & close > ema) return("AboveBothMA")
  else if (close < sma & close < ema) return("BelowBothMA")
  else return("BetweenMA")
}

get_stoch_level <- function(x) {
  if (x * 100 > 80) return("Stoch_High")
  else if (x * 100 < 20) return("Stoch_Low")
  else return("Stoch_Mid")
}

get_ad_level <- function(x) {
  if (x > 0) return("AD_Pos")
  else return("AD_Neg")
}

get_obv_level <- function(x) {
  if (x > 0) return("OBV_Pos")
  else return("OBV_Neg")
}

get_roc_level <- function(x) {
  if (x > 0.001) return("ROC_Up")
  else if (x < -0.001) return("ROC_Down")
  else return("ROC_Flat")
}

get_williams_level <- function(x) {
  if (x * 100 < -80) return("Will_Oversold")
  else if (x * 100 > -20) return("Will_Overbought")
  else return("Will_Neutral")
}

get_disparity_level <- function(x) {
  if (x > 0.1) return("Disp_Pos")
  else if (x < -0.1) return("Disp_Neg")
  else return("Disp_Neutral")
}






data$StateID <- apply(data, 1, function(row) {
  paste(
    get_rsi_level(as.numeric(row["rsi"])),
    get_macd_level(as.numeric(row["macd"])),
    get_sma_ema_level(as.numeric(row["close"]), as.numeric(row["sma"]), as.numeric(row["ema"])),
    get_stoch_level(as.numeric(row["stoch_k"])),
    get_ad_level(as.numeric(row["ad"])),
    get_obv_level(as.numeric(row["obv"])),
    get_roc_level(as.numeric(row["roc"])),
    get_williams_level(as.numeric(row["williams_r"])),
    get_disparity_level(as.numeric(row["disparity_index"])),
    sep = "|"
  )
})

unique(data$StateID)



# Q table 

# Define action space
actions <- c("Buy", "Sell", "Hold")

# Unique states from your data
unique_states <- unique(data$StateID)

# Create full state-action combinations
Q_table <- expand.grid(State = unique_states, Action = actions)
Q_table$Q <- 0  # Initialize all Q-values to zero




library(dplyr)
Q_table <- Q_table %>% arrange(State, Action)


#############Let the Agent Take an Action for One State







# Parameters
epsilon <- 0.2  # 20% exploration, 80% exploitation

# Define possible actions
actions <- c("Buy", "Sell", "Hold")

# Let's pick row i from data
i <- 1  # first time step
current_state <- data$StateID[i]

# Epsilon-greedy action selection
if (runif(1) < epsilon) {
  chosen_action <- sample(actions, 1)  # random action (explore)
} else {
  # choose best action based on Q-table
  q_values <- Q_table %>% filter(State == current_state)
  max_q <- max(q_values$Q)
  best_actions <- q_values$Action[q_values$Q == max_q]
  chosen_action <- sample(best_actions, 1)  # break ties randomly
}

# Show chosen action for this state
# cat("Time:", data$Timestamp[i], "\n")
# cat("State:", current_state, "\n")
# cat("Chosen Action:", chosen_action, "\n")
# 







simulation=1 

while(simulation<105){
  
  #######reward
  
  
  # Hyperparameters
  alpha <- 0.2      # learning rate
  gamma <- 0.85      # discount factor
  epsilon <- 0.3     # exploration rate
  hold_reward_scale <- 100  # small reward during hold (scaled by price delta)
  
  # Actions
  actions <- c("Buy", "Sell", "Hold")
  
  # Initialize
  Q_table$Q <- 0
  position <- FALSE
  entry_price <- NA
  entry_index <- NA
  entry_state <- NA
  
  # Track rewards if needed
  total_reward <- 0
  i=4
  # Main loop
  for (i in 1:(nrow(data) - 1)) {
    
    current_state <- data$StateID[i]
    next_state <- data$StateID[i + 1]
    current_price <- data$close[i]
    next_price <- data$close[i + 1]
    
    # ε-greedy policy
    if (runif(1) < epsilon) {
      action <- sample(actions, 1)
    } else {
      q_values <- Q_table %>% filter(State == current_state)
      max_q <- max(q_values$Q)
      best_actions <- q_values$Action[q_values$Q == max_q]
      action <- sample(best_actions, 1)
    }
    
    reward <- 0
    
    # --- Buy ---
    if (action == "Buy" && !position) {
      position <- TRUE
      entry_price <- current_price
      entry_index <- i
      entry_state <- current_state
      reward <- 0  # No reward now
    }
    
    # --- Hold ---
    else if (action == "Hold" && position) {
      reward <- (current_price - entry_price) * hold_reward_scale
      
      # Update Hold Q-value
      old_q_hold <- Q_table$Q[Q_table$State == current_state & Q_table$Action == "Hold"]
      max_q_next_hold <- max(Q_table$Q[Q_table$State == next_state])
      
      Q_table$Q[Q_table$State == current_state & Q_table$Action == "Hold"] <- 
        old_q_hold + alpha * (reward + gamma * max_q_next_hold - old_q_hold)
    }
    
    # --- Sell ---
    else if (action == "Sell" && position) {
      profit <- current_price - entry_price
      reward <- profit * 1000  # Main reward
      
      # Update Buy (entry) Q-value
      old_q_buy <- Q_table$Q[Q_table$State == entry_state & Q_table$Action == "Buy"]
      max_q_next_buy <- max(Q_table$Q[Q_table$State == current_state])
      
      Q_table$Q[Q_table$State == entry_state & Q_table$Action == "Buy"] <- 
        old_q_buy + alpha * (reward + gamma * max_q_next_buy - old_q_buy)
      
      # ✅ Update Sell (current) Q-value
      old_q_sell <- Q_table$Q[Q_table$State == current_state & Q_table$Action == "Sell"]
      max_q_next_sell <- max(Q_table$Q[Q_table$State == next_state])
      
      Q_table$Q[Q_table$State == current_state & Q_table$Action == "Sell"] <- 
        old_q_sell + alpha * (reward + gamma * max_q_next_sell - old_q_sell)
      
      # Reset position
      position <- FALSE
      entry_price <- NA
      entry_index <- NA
      entry_state <- NA
    }
    
    # Track total reward
    total_reward <- total_reward + reward
  }
  
  
  
  # ✅ Q-table now contains learned values
  # 🔍 Check best action for a state
  Q_table %>% filter(State == "RSI_Mid_AboveSMA_MACD_Pos_MidVol") %>% arrange(desc(Q))
  
  #View(Q_table)
  
  
  
  # View the best action per state
  Q_policy <- Q_table %>%
    group_by(State) %>%
    slice_max(Q, n = 1) %>%
    ungroup() %>%
    arrange(desc(Q))
  
  #head(Q_policy, 10)
  
  
  
  
  
  
  
  # backtest
  
  run_backtest <- function(Q_table, data, CapitalUSD = 1000) {
    library(dplyr)
    library(PerformanceAnalytics)
    library(ggplot2)
    library(xts)
    
    position <- FALSE
    entry_price <- NA
    total_profit <- 0
    
    trade_log <- data.frame(
      EntryTime = character(),
      ExitTime = character(),
      EntryPrice = numeric(),
      ExitPrice = numeric(),
      Profit = numeric(),
      EntryIndex = numeric(),
      ExitIndex = numeric(),
      ActionConfidence = numeric(),
      stringsAsFactors = FALSE
    )
    
    data$index <- 1:nrow(data)
    i <- 1
    
    while (i < nrow(data)) {
      current_state <- data$StateID[i]
      current_price <- data$close[i]
      timestamp <- data$timestamp[i]
      
      q_values <- Q_table %>% filter(State == current_state)
      q_ordered <- q_values %>% arrange(desc(Q))
      
      max_q <- q_ordered$Q[1]
      second_max_q <- ifelse(nrow(q_ordered) > 1, q_ordered$Q[2], max_q)
      action_confidence <- max_q - second_max_q
      
      best_actions <- q_ordered$Action[q_ordered$Q == max_q]
      action <- sample(best_actions, 1)
      
      # --- Buy ---
      if (action == "Buy" && !position) {
        position <- TRUE
        entry_price <- current_price
        entry_time <- timestamp
        entry_index <- i
        entry_confidence <- action_confidence
      }
      
      # --- Sell ---
      else if (action == "Sell" && position) {
        exit_price <- current_price
        exit_time <- timestamp
        exit_index <- i
        
        price_change <- exit_price - entry_price
        profit <- (price_change / entry_price) * CapitalUSD-(CapitalUSD/current_price)*0.005-(CapitalUSD/current_price)*0.01
        total_profit <- total_profit + profit
        
        trade_log <- rbind(trade_log, data.frame(
          EntryTime = entry_time,
          ExitTime = exit_time,
          EntryPrice = entry_price,
          ExitPrice = exit_price,
          Profit = profit,
          EntryIndex = entry_index,
          ExitIndex = exit_index,
          ActionConfidence = entry_confidence
        ))
        
        position <- FALSE
        entry_price <- NA
        i <- exit_index + 1
        next
      }
      
      i <- i + 1
    }
    
    # Cumulative Profit
    trade_log$CumulativeProfit <- cumsum(trade_log$Profit)
    
    # Drawdown Calculation
    if (nrow(trade_log) > 1) {
      xts_cp <- xts(trade_log$CumulativeProfit, order.by = as.POSIXct(trade_log$ExitIndex, origin = "1970-01-01"))
      dd <- PerformanceAnalytics::Drawdowns(xts_cp)
      max_dd <- max(dd, na.rm = TRUE)
    } else {
      max_dd <- NA
    }
    
    # Buy & Hold Profit
    buy_hold_return <- (data$close[nrow(data)] - data$close[1]) / data$close[1] * CapitalUSD
    
    # Plot Trades
    ggplot(data, aes(x = index, y = close)) +
      geom_line() +
      geom_point(data = trade_log, aes(x = EntryIndex, y = EntryPrice), color = "green", size = 2) +
      geom_point(data = trade_log, aes(x = ExitIndex, y = ExitPrice), color = "red", size = 2) +
      ggtitle("Trades on EURUSD") +
      xlab("Time Index") + ylab("Close Price")
    
    return(list(
      Total_Profit = total_profit,
      Avg_Profit_Per_Trade = mean(trade_log$Profit),
      Std_Dev_Profit = sd(trade_log$Profit),
      Max_Drawdown = max_dd,
      Buy_and_Hold = buy_hold_return,
      Number_of_Trades = nrow(trade_log),
      Trade_Log = trade_log
    ))
  }
  
  
  
  
  
  
  
  results <- run_backtest(Q_table, data)
  
  
  # cor(results$Trade_Log$ActionConfidence,results$Trade_Log$Profit)
  # plot(results$Trade_Log$ActionConfidence,results$Trade_Log$Profit)
  # 
  # print(results$Total_Profit)
  # 
  # 
  # print(results$Total_Profit*100/CapitalUSD)
  # 
  # print(results$Avg_Profit_Per_Trade)
  # print(results$Max_Drawdown)
  # head(results$Trade_Log)
  
  
  
  
  
  
  
  
  # Add Trade Number column
  results$Trade_Log$TradeNumber <- 1:nrow(results$Trade_Log)
  
  # Plot Profit/Loss for each trade
  library(ggplot2)
  
  ggplot(results$Trade_Log, aes(x = TradeNumber, y = Profit, fill = Profit > 0)) +
    geom_bar(stat = "identity", color = "black", width = 0.7) +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
    geom_text(aes(label = round(Profit, 5)), vjust = ifelse(results$Trade_Log$Profit >= 0, -0.5, 1.2), size = 3) +
    theme_minimal() +
    labs(
      title = "Profit/Loss per Trade",
      x = "Trade Number",
      y = "Profit (in price units or pips)",
      fill = "Profit > 0"
    )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Get the trade log
  log <- results$Trade_Log
  
  # Basic stats
  total_trades <- nrow(log)
  winning_trades <- sum(log$Profit > 0)
  losing_trades <- sum(log$Profit < 0)
  zero_trades <- sum(log$Profit == 0)
  
  avg_profit <- mean(log$Profit)
  std_profit <- sd(log$Profit)
  win_rate <- winning_trades / total_trades * 100
  loss_rate <- losing_trades / total_trades * 100
  zero_rate <- zero_trades / total_trades * 100
  
  # Summary table
  trade_summary <- data.frame(
    Metric = c("Total Trades", "Winning Trades", "Losing Trades", "Zero P/L Trades", 
               "Win Rate (%)", "Loss Rate (%)", "Avg Profit", "Std Dev Profit"),
    Value = c(total_trades, winning_trades, losing_trades, zero_trades, 
              round(win_rate, 2), round(loss_rate, 2), round(avg_profit, 5), round(std_profit, 5))
  )
  
  print(trade_summary)
  
  
  
  
  library(ggplot2)
  
  log$Outcome <- ifelse(log$Profit > 0, "Win",
                        ifelse(log$Profit < 0, "Loss", "Zero"))
  
  p=ggplot(log, aes(x = Outcome)) +
    geom_bar(fill = "steelblue", color = "black", width = 0.6) +
    theme_minimal() +
    labs(
      title = "Frequency of Trade Outcomes",
      x = "Outcome",
      y = "Count"
    ) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)
  
  
  
  png(paste0( "Simulation", simulation,".png" ),  width = 600, height = 400)
  
  
  
  plot(results$Trade_Log$CumulativeProfit, type = "l", col = "blue", lwd = 2,
       main = "Cumulative Profit (USD)", xlab = "Trade #", ylab = "Profit in USD")
  abline(h = 0, col = "gray", lty = 2)
  
  
  dev.off()
  
  
  
  
  ########### summary
  
  
  
  profitable_trades <- log$Profit[log$Profit > 0]
  losing_trades <- log$Profit[log$Profit < 0]
  
  avg_profit_size <- if (length(profitable_trades) > 0) mean(profitable_trades) else 0
  avg_loss_size <- if (length(losing_trades) > 0) mean(losing_trades) else 0
  total_profit_winners <- sum(profitable_trades)
  total_loss_losers <- sum(losing_trades)
  
  full_trade_summary <- data.frame(
    Metric = c("Data Size", "Capital Used", "Total Trades", 
               "Winning Trades", "Losing Trades", 
               "Total Profit (USD)", "Profit % of Capital", 
               "Avg Profit Size", "Avg Loss Size", 
               "Total Profit from Winners", "Total Loss from Losers"),
    Value = c(
      nrow(data), CapitalUSD, total_trades, 
      winning_trades,(total_trades- winning_trades) , 
      round(results$Total_Profit, 2), 
      round(results$Total_Profit * 100 / CapitalUSD, 2),
      round(avg_profit_size, 2),
      round(avg_loss_size, 2),
      round(total_profit_winners, 2),
      round(total_loss_losers, 2)
    )
  )
  
  #print(full_trade_summary)
  
  
  
  # Horizontal trade summary
  horizontal_summary <- data.frame(
    DataSize = nrow(data),
    CapitalUsed = CapitalUSD,
    TotalTrades = total_trades,
    WinningTrades = winning_trades,
    LosingTrades =total_trades-winning_trades ,
    TotalProfitUSD = round(results$Total_Profit, 2),
    ProfitPercent = round(results$Total_Profit * 100 / CapitalUSD, 2),
    AvgProfitSize = round(avg_profit_size, 2),
    AvgLossSize = round(avg_loss_size, 2),
    TotalProfitFromWinners = round(total_profit_winners, 2),
    TotalLossFromLosers = round(total_loss_losers, 2),
    PerformanceRatePerOrder=round(results$Total_Profit, 2)/total_trades,
    PerformanceRatePerOrderPROF=round(total_profit_winners, 2)/winning_trades,
    PerformanceRatePerOrderLOss= -round(total_loss_losers, 2)/total_loss_losers
  )
  
  # View the horizontal table
  print(horizontal_summary)
  
  
  
  
  
  #horizontal_summaryG=horizontal_summary
  horizontal_summaryG=rbind(horizontal_summaryG,horizontal_summary)
  
  simulation =simulation+1
  print(simulation)
}

# 20


############   plot of simulation

df=horizontal_summaryG[c(7:95),]

library(ggplot2)
library(dplyr)
library(tidyr)









df_long <- df %>%
  mutate(ID = row_number()) %>%
  pivot_longer(cols = c("WinningTrades", "LosingTrades"), names_to = "Outcome", values_to = "Trades") %>%
  group_by(ID) %>%
  mutate(Percent = Trades / sum(Trades) * 100,
         Label = paste0(round(Percent), "%")) %>%
  ungroup()

ggplot(df_long, aes(x = factor(ID), y = Trades, fill = Outcome)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 2) +
  labs(title = "Winning vs Losing Trades per Scenario",
       x = "Scenario ID", y = "Number of Trades") +
  scale_fill_manual(values = c("WinningTrades" = "steelblue", "LosingTrades" = "firebrick")) +
  theme_minimal()






df <- df %>%
  mutate(ID = row_number())

ggplot(df, aes(x = factor(ID), y = ProfitPercent/2)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Profit Percentage per Scenario",
       x = "Scenario ID", y = "Profit %") +
  theme_minimal()







library(ggplot2)

# Step 1: Compute stats
profit_percentages <- df$ProfitPercent
n <- length(profit_percentages)
mean_profit <- mean(profit_percentages)
sd_profit <- sd(profit_percentages)

# Step 2: Prediction Intervals
# 95%
alpha_95 <- 0.05
t_crit_95 <- qt(1 - alpha_95 / 2, df = n - 1)
margin_95 <- t_crit_95 * sd_profit * sqrt(1 + 1/n)
pi95_lower <- mean_profit - margin_95
pi95_upper <- mean_profit + margin_95

# 80%
alpha_80 <- 0.20
t_crit_80 <- qt(1 - alpha_80 / 2, df = n - 1)
margin_80 <- t_crit_80 * sd_profit * sqrt(1 + 1/n)
pi80_lower <- mean_profit - margin_80
pi80_upper <- mean_profit + margin_80

# 70%
alpha_70 <- 0.30
t_crit_70 <- qt(1 - alpha_70 / 2, df = n - 1)
margin_70 <- t_crit_70 * sd_profit * sqrt(1 + 1/n)
pi70_lower <- mean_profit - margin_70
pi70_upper <- mean_profit + margin_70

# Step 3: Plot with all three intervals
ggplot(df, aes(x = factor(ID), y = ProfitPercent / 2)) +
  geom_bar(stat = "identity", fill = "blue") +
  
  # 95% Prediction Interval (lightest)
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = pi95_lower / 2, ymax = pi95_upper / 2,
           alpha = 0.15, fill = "skyblue") +
  
  # 80% Prediction Interval (medium)
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = pi80_lower / 2, ymax = pi80_upper / 2,
           alpha = 0.25, fill = "steelblue") +
  
  # 70% Prediction Interval (darkest)
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = pi70_lower / 2, ymax = pi70_upper / 2,
           alpha = 0.35, fill = "navy") +
  
  # Mean line
  geom_hline(yintercept = mean_profit / 2, color = "red", linetype = "dashed", size = 2) +
  
  # Labels
  labs(title = "Profit Percentage per Scenario\nWith 70%, 80%, and 95% Prediction Intervals",
       x = "Scenario ID", y = "Profit %") +
  theme_minimal()













# 4. Scatter Plot: TotalTrades vs. ProfitPercent (Bubble = TotalProfitUSD)
ggplot(df, aes(x = TotalTrades, y = ProfitPercent, size = TotalProfitUSD)) +
  geom_point(color = "purple", alpha = 0.7) +
  scale_size_continuous(name = "Total Profit USD") +
  labs(title = "Profit % vs Total Trades",
       x = "Total Trades", y = "Profit %") +
  theme_minimal()











# Rename if needed
df <- df %>% rename(n_col = n)  # only if "n" exists

# Add WinRate
df <- df %>% mutate(ID = row_number(),
                    WinRate = WinningTrades*100 / TotalTrades)

# Plot
ggplot(df, aes(x = factor(ID), y = WinRate)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Winning Trade Ratio per Scenario",
       x = "Scenario ID", y = "Winning Rate") +
  theme_minimal()




library(ggplot2)
library(dplyr)
library(tidyr)

# Prepare data
df_plot <- df %>%
  mutate(ID = row_number()) %>%
  select(ID, PerformanceRatePerOrderPROF, PerformanceRatePerOrderLOss) %>%
  pivot_longer(cols = c("PerformanceRatePerOrderPROF", "PerformanceRatePerOrderLOss"),
               names_to = "Type", values_to = "Rate") %>%
  mutate(Type = recode(Type,
                       "PerformanceRatePerOrderPROF" = "Profit",
                       "PerformanceRatePerOrderLOss" = "Loss"))

# Plot
ggplot(df_plot, aes(x = factor(ID), y = Rate, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("Profit" = "forestgreen", "Loss" = "firebrick")) +
  labs(title = "Per-Order Performance: Profit vs Loss by Scenario",
       x = "Scenario ID", y = "Performance per Order",
       fill = "Trade Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))







# Prepare data
df_plot <- df %>%
  mutate(ID = row_number(),
         TotalLossFromLosers = abs(TotalLossFromLosers)) %>%  # take absolute for visualization
  select(ID, TotalProfitFromWinners, TotalLossFromLosers) %>%
  pivot_longer(cols = c("TotalProfitFromWinners", "TotalLossFromLosers"),
               names_to = "Type", values_to = "Amount") %>%
  mutate(Type = recode(Type,
                       "TotalProfitFromWinners" = "Profit from Winners",
                       "TotalLossFromLosers" = "Loss from Losers"))

# Plot
ggplot(df_plot, aes(x = factor(ID), y = Amount, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("Profit from Winners" = "forestgreen",
                               "Loss from Losers" = "firebrick")) +
  labs(title = "Total Profit vs Loss per Scenario",
       x = "Simulation ID", y = "Amount (USD)",
       fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



















