
library(readr)

# Load the full dataset
bitcoin_transactionsfull <- read_csv("bitcoin_transactionsfull.csv")

# Get total number of rows
total_rows <- nrow(bitcoin_transactionsfull)

# Calculate the size of each part
part_size <- ceiling(total_rows / 9)

# Loop through 1 to 5 to split and save each part
for (i in 1:9) {
  start_row <- (i - 1) * part_size + 1
  end_row <- min(i * part_size, total_rows)
  part_data <- bitcoin_transactionsfull[start_row:end_row, ]
  
  # Save to CSV
  write_csv(part_data, paste0("bitcoin_transactions_part", i, ".csv"))
}


























# Load required libraries
library(TTR)
library(dplyr)

library(readr)
library(lubridate)  # For floor_date() function
library(dplyr)
library(TTR)
library(zoo)
library(TTR)
library(quantmod)
library(ggplot2)
library(dplyr)
library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)
library(tidyr)
EURUSD_mt5_ticks <- read.csv("EURUSD_mt5_ticks.csv",sep=""  )

EURUSD_mt5_ticks=EURUSD_mt5_ticks[c(5000:10000),]

data=EURUSD_mt5_ticks

dim(data)

head(data)
View(data)

######### state

# Assume `data` already contains columns: Date, Timestamp, Open, High, Low, Close, Volume

# --- Step 1: Calculate Indicators ---

# 1. RSI (Relative Strength Index)
data$RSI <- RSI(data$Close, n = 14)

# 2. SMA (Simple Moving Average, 20-period)
data$SMA_20 <- SMA(data$Close, n = 20)

# 3. Price vs SMA (trend position indicator)
data$Price_vs_SMA <- data$Close - data$SMA_20

# 4. MACD (Momentum/Trend indicator)
macd <- MACD(data$Close, nFast = 12, nSlow = 26, nSig = 9, maType = EMA)
data$MACD <- macd[,1]

# 5. Volatility (rolling standard deviation of Close)
data$Volatility <- runSD(data$Close, n = 14)

# Remove rows with NA values (introduced by rolling indicators)
data <- na.omit(data)

# --- Step 2: Discretize indicators into categories for StateID ---






get_rsi_level <- function(rsi) {
  if (rsi < 30) return("RSI_Low")
  else if (rsi > 70) return("RSI_High")
  else return("RSI_Mid")
}

get_price_vs_sma_level <- function(diff) {
  if (diff > 0.0002) return("AboveSMA")
  else if (diff < -0.0002) return("BelowSMA")
  else return("NearSMA")
}

get_macd_level <- function(macd) {
  if (macd > 0) return("MACD_Pos")
  else return("MACD_Neg")
}

get_volatility_level <- function(vol) {
  if (vol < 0.00005) return("LowVol")
  else if (vol > 0.00015) return("HighVol")
  else return("MidVol")
}

# --- Step 3: Generate StateID for Each Row ---

data$StateID <- apply(data, 1, function(row) {
  rsi_cat <- get_rsi_level(as.numeric(row["RSI"]))
  sma_cat <- get_price_vs_sma_level(as.numeric(row["Price_vs_SMA"]))
  macd_cat <- get_macd_level(as.numeric(row["MACD"]))
  vol_cat <- get_volatility_level(as.numeric(row["Volatility"]))
  paste(rsi_cat, sma_cat, macd_cat, vol_cat, sep = "_")
})

# --- View Sample Output ---
head(data[, c("Timestamp", "Close", "RSI", "Price_vs_SMA", "MACD", "Volatility", "StateID")])


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
cat("Time:", data$Timestamp[i], "\n")
cat("State:", current_state, "\n")
cat("Chosen Action:", chosen_action, "\n")





#######reward


# Hyperparameters
alpha <- 0.1       # learning rate
gamma <- 0.95      # discount factor
epsilon <- 0.2     # exploration rate
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

# Main loop
for (i in 1:(nrow(data) - 1)) {
  
  current_state <- data$StateID[i]
  next_state <- data$StateID[i + 1]
  current_price <- data$Close[i]
  next_price <- data$Close[i + 1]
  
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





# View the best action per state
Q_policy <- Q_table %>%
  group_by(State) %>%
  slice_max(Q, n = 1) %>%
  ungroup() %>%
  arrange(desc(Q))

head(Q_policy, 10)















############## interpretation##################################################

library(ggplot2)
library(dplyr)

# First, order states by total Q-value for better visualization
Q_table_plot <- Q_table %>%
  group_by(State) %>%
  mutate(TotalQ = sum(Q)) %>%
  ungroup() %>%
  arrange(desc(TotalQ))

# Create factor levels for ordered plot
Q_table_plot$State <- factor(Q_table_plot$State, levels = unique(Q_table_plot$State))

# Plot: Bar chart grouped by State with different colors for each action
ggplot(Q_table_plot, aes(x = State, y = Q, fill = Action)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Q-values per State and Action", x = "State (Ordered by Total Q)", y = "Q-value") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
        legend.position = "top") +
  scale_fill_brewer(palette = "Set1")




#######
library(ggplot2)

ggplot(Q_table_plot, aes(x = Action, y = State, fill = Q)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen", midpoint = 0) +
  labs(title = "Heatmap of Q-values", x = "Action", y = "State") +
  theme_minimal(base_size = 13) +
  theme(axis.text.y = element_text(size = 9))

#############

library(dplyr)

Q_policy <- Q_table %>%
  group_by(State) %>%
  slice_max(Q, n = 1) %>%
  ungroup() %>%
  arrange(desc(Q))

top_states <- Q_policy %>% slice_max(Q, n = 15)

ggplot(top_states, aes(x = reorder(State, Q), y = Q, fill = Action)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 15 States with Highest Q-values", x = "State", y = "Q-value") +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Dark2")



###################
state_freq <- data %>%
  group_by(StateID) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head(20)

ggplot(state_freq, aes(x = reorder(StateID, -Frequency), y = Frequency)) +
  geom_col(fill = "steelblue") +
  labs(title = "Top 20 Most Frequent States", x = "State", y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


