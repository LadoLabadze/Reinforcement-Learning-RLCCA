# --- Part 1: Data Preparation with Technical Indicators ---

import pandas as pd
import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
import torch.nn.functional as F
import matplotlib.pyplot as plt
import seaborn as sns
import random
import os
from sklearn.preprocessing import StandardScaler
from collections import deque
from ta import momentum, trend, volume

# Config
DATA_PATH = "SPYUSUSD2years.csv"
DataSize = 6500
CapitalUSD = 2000

# Load and prepare data
data = pd.read_csv(DATA_PATH).iloc[:1 + DataSize].copy()
data.columns = data.columns.str.lower().str.replace(' ', '')

# Technical indicators
data['rsi'] = momentum.RSIIndicator(data['close'], window=14).rsi()
data['sma'] = trend.SMAIndicator(data['close'], window=14).sma_indicator()
data['ema'] = trend.EMAIndicator(data['close'], window=14).ema_indicator()
data['stoch_k'] = momentum.StochasticOscillator(data['high'], data['low'], data['close'], window=14).stoch()
macd = trend.MACD(data['close'])
data['macd'] = macd.macd()
data['ad'] = volume.AccDistIndexIndicator(data['high'], data['low'], data['close'], data['volume']).acc_dist_index()
data['obv'] = volume.OnBalanceVolumeIndicator(data['close'], data['volume']).on_balance_volume()
data['roc'] = momentum.ROCIndicator(data['close'], window=14).roc()
data['williams_r'] = momentum.WilliamsRIndicator(data['high'], data['low'], data['close'], lbp=14).williams_r()
data['disparity_index'] = 100 * ((data['close'] - data['ema']) / data['ema'])

# Drop NaNs and reset index
data.dropna(inplace=True)
data.reset_index(drop=True, inplace=True)

# Feature list
features = [
    'rsi', 'sma', 'ema', 'stoch_k', 'macd', 'ad',
    'obv', 'roc', 'williams_r', 'disparity_index'
]

# Normalize
scaler = StandardScaler()
data[features] = scaler.fit_transform(data[features])

print(f"✅ Data Ready: {data.shape}")

# Train/Validation split
split_ratio = 0.8
split_index = int(len(data) * split_ratio)
train_data = data.iloc[:split_index].reset_index(drop=True)
val_data = data.iloc[split_index:].reset_index(drop=True)

print(f"✅ Train Data: {train_data.shape}, Validation Data: {val_data.shape}")

# --- Part 2: Define Deep Q-Network ---

class DQN(nn.Module):
    def __init__(self, input_dim, output_dim):
        super(DQN, self).__init__()
        self.fc1 = nn.Linear(input_dim, 128)
        self.fc2 = nn.Linear(128, 64)
        self.fc3 = nn.Linear(64, output_dim)

    def forward(self, x):
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        return self.fc3(x)

# Init networks
n_features = len(features)
n_actions = 3  # Buy, Sell, Hold

policy_net = DQN(n_features, n_actions)
target_net = DQN(n_features, n_actions)
target_net.load_state_dict(policy_net.state_dict())
target_net.eval()

optimizer = optim.Adam(policy_net.parameters(), lr=0.001)
loss_fn = nn.MSELoss()

print(f"✅ Networks initialized with {n_features} input features")

# --- Part 3: Replay Memory and Action Selection ---

memory = deque(maxlen=2000)
batch_size = 32
gamma = 0.95
epsilon = 1.0
epsilon_min = 0.01
epsilon_decay = 0.995

def select_action(state, epsilon):
    if np.random.rand() < epsilon:
        return random.randrange(n_actions)
    with torch.no_grad():
        return policy_net(state).argmax().item()

def optimize_model():
    if len(memory) < batch_size:
        return
    batch = random.sample(memory, batch_size)
    states, actions, rewards, next_states, dones = zip(*batch)

    states = torch.stack(states)
    actions = torch.tensor(actions)
    rewards = torch.tensor(rewards, dtype=torch.float32)
    next_states = torch.stack(next_states)
    dones = torch.tensor(dones, dtype=torch.float32)

    q_values = policy_net(states).gather(1, actions.unsqueeze(1)).squeeze()
    next_q_values = target_net(next_states).max(1)[0]
    expected_q = rewards + gamma * next_q_values * (1 - dones)

    loss = loss_fn(q_values, expected_q.detach())
    optimizer.zero_grad()
    loss.backward()
    optimizer.step()

# --- Part 4: Deep Q-Learning Training Loop ---

num_episodes = 5
update_target_every = 5
episode_profits = []

for episode in range(num_episodes):
    position = False
    entry_price = None
    total_profit = 0
    print(f"\n🧠 Episode {episode + 1}/{num_episodes}")

    for i in range(1, len(train_data) - 1):
        state = torch.tensor(train_data.loc[i, features].astype(np.float32).values).unsqueeze(0)
        next_state = torch.tensor(train_data.loc[i + 1, features].astype(np.float32).values).unsqueeze(0)

        action = select_action(state, epsilon)
        current_price = train_data.loc[i, 'close']
        next_price = train_data.loc[i + 1, 'close']
        reward = 0
        done = 0

        if action == 0 and not position:  # Buy
            position = True
            entry_price = current_price
        elif action == 1 and position:  # Sell
            price_change = next_price - entry_price
            commission = (CapitalUSD / current_price) * (0.005 + 0.01)
            profit = (price_change / entry_price) * CapitalUSD - commission
            reward = profit * 100
            total_profit += profit
            position = False
        elif action == 2 and position:  # Hold
            reward = (next_price - entry_price) * 10

        if i == len(train_data) - 2:
            done = 1

        memory.append((state.squeeze(0), action, reward, next_state.squeeze(0), done))
        optimize_model()

    if (episode + 1) % update_target_every == 0:
        target_net.load_state_dict(policy_net.state_dict())
        print("📌 Target updated.")

    epsilon = max(epsilon_min, epsilon * epsilon_decay)
    episode_profits.append(total_profit)
    print(f"💰 Profit: ${total_profit:.2f}, Epsilon: {epsilon:.4f}")

# --- Part 5: Plot Training Profit ---

os.makedirs("plots", exist_ok=True)

plt.figure(figsize=(10, 5))
plt.plot(episode_profits, label='Training Cumulative Profit', marker='o')
plt.axhline(np.mean(episode_profits), color='gray', linestyle='--', label='Train Avg')
plt.title("Training Performance per Episode")
plt.xlabel("Episode")
plt.ylabel("Profit (USD)")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.savefig("plots/train_episode_profits.png")
plt.show()

# --- Part 6: Validation (Live Trading Phase) ---

print("\n🚀 Starting Validation...")

position = False
entry_price = None
total_profit_val = 0
validation_trades = []

for i in range(1, len(val_data) - 1):
    state = torch.tensor(val_data.loc[i, features].astype(np.float32).values).unsqueeze(0)
    next_state = torch.tensor(val_data.loc[i + 1, features].astype(np.float32).values).unsqueeze(0)

    with torch.no_grad():
        action = policy_net(state).argmax().item()

    current_price = val_data.loc[i, 'close']
    next_price = val_data.loc[i + 1, 'close']

    if action == 0 and not position:  # Buy
        position = True
        entry_price = current_price
    elif action == 1 and position:  # Sell
        price_change = next_price - entry_price
        commission = (CapitalUSD / current_price) * (0.005 + 0.01)
        profit = (price_change / entry_price) * CapitalUSD - commission
        total_profit_val += profit
        position = False

        validation_trades.append({
            'BuyIndex': i,
            'SellIndex': i + 1,
            'EntryPrice': entry_price,
            'ExitPrice': next_price,
            'Profit': profit
        })

print(f"✅ Validation Total Profit: ${total_profit_val:.2f}")
print(f"✅ Validation Return: {total_profit_val / CapitalUSD * 100:.2f}%")

# Plot Validation Cumulative Profit
val_trade_df = pd.DataFrame(validation_trades)
val_trade_df['CumulativeProfit'] = val_trade_df['Profit'].cumsum()

plt.figure(figsize=(10, 5))
plt.plot(val_trade_df['CumulativeProfit'], label='Validation Cumulative Profit', color='orange')
plt.axhline(0, linestyle='--', color='gray')
plt.title("Validation Performance")
plt.xlabel("Trade #")
plt.ylabel("Profit (USD)")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.savefig("plots/validation_cumulative_profit.png")
plt.show()
