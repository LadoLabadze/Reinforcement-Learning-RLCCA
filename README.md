# 📘  Stock Q-Learning Trading Simulator

This repository implements a **Q-learning-based stock trading agent** trained on historical Tesla (TSLA) data. The agent learns to make profitable buy/sell/hold decisions using a discrete state representation of technical indicators. The system includes training, backtesting, and visualization of performance against benchmarks like SPY.

---

## 🚀 Overview

This project is a fully self-contained implementation of a reinforcement learning (RL) pipeline, featuring:

* ✅ Q-learning algorithm from scratch
* ✅ Symbolic state construction using multiple technical indicators
* ✅ Epsilon-greedy action selection and confidence-based capital allocation
* ✅ Reward shaping with slippage and transaction cost modeling
* ✅ Trade logging, backtesting, and performance benchmarking

---

## 🔧 How It Works

### 1. Data Loading & Preprocessing

The system loads historical OHLCV data from `TSLA.csv` and processes the first `N` rows (`DataSize`) for training. Column names are cleaned and converted to lowercase.

---

### 2. Technical Indicator Calculation

The following indicators are computed using the `ta` library:

| Indicator       | Description                             |
| --------------- | --------------------------------------- |
| RSI             | Relative Strength Index (momentum)      |
| SMA / EMA       | Moving Averages (trend)                 |
| MACD            | Moving Average Convergence Divergence   |
| Stochastic %K   | Market momentum                         |
| OBV / AD        | Volume-based indicators                 |
| ROC             | Rate of Change                          |
| Williams %R     | Momentum and overbought/oversold levels |
| Disparity Index | Price deviation from EMA                |

---

### 3. State Construction (StateID)

The model converts market conditions into discrete symbolic states using rules. For each row, a `StateID` is formed as a concatenation like:

```
RSI_Mid|MACD_Pos|AboveBothMA|Stoch_Low|AD_Neg|OBV_Pos|ROC_Up|Will_Neutral|Disp_Pos
```

This compact representation forms the **state space** used in the Q-learning table.

---

### 4. Q-Table Initialization

A Q-table is created with all combinations of unique states and three actions:

* `Buy`
* `Sell`
* `Hold`

All Q-values are initialized to 0. The table is updated during training.

---

### 5. Q-Learning Algorithm

A temporal loop simulates the agent's experience across time:

#### Action Selection

* Epsilon-greedy: explores with probability `ε`, exploits best known action otherwise.

#### Q-Value Update

Q-values are updated using the Bellman Equation:

```
Q(s, a) ← Q(s, a) + α * [r + γ * max(Q(s′, a′)) - Q(s, a)]
```

Where:

* `α` = learning rate
* `γ` = discount factor
* `r` = reward
* `s` = current state
* `s′` = next state

#### Reward Structure

* Profit-based reward for Buy → Hold → Sell cycle
* Intermediate reward for holding a rising asset
* Transaction costs and slippage modeled realistically

---

### 6. Capital Allocation Based on Confidence

The system calculates **confidence in actions** using Q-value differences. Based on this, it adjusts the capital fraction used in trades:

| Confidence (∆Q) | Capital Used |
| --------------- | ------------ |
| ≥ 0.6           | 100%         |
| 0.5–0.6         | 75%          |
| 0.4–0.5         | 60%          |
| ...             | ...          |
| < 0.1           | 10%          |

This mimics real-world **risk-based trade sizing**.

---

### 7. Backtesting

Once training is complete, the agent is backtested:

* Trades are executed per learned policy
* Trade log includes entry/exit, capital used, profit, and cumulative return
* Slippage and commission are subtracted from profit

---

### 8. Multiple Simulations

The full process is repeated across **4 simulations** to test stability and robustness. Each run logs:

* Cumulative profit curve
* Summary statistics
* Bar plots comparing:

  * Model performance per simulation
  * Model average
  * SPY benchmark (9% annual return)

---

## 📊 Metrics Tracked

| Metric                  | Description                        |
| ----------------------- | ---------------------------------- |
| Total Profit (USD)      | Net profit over simulation period  |
| Profit % Return         | Percentage return based on capital |
| Max Drawdown            | Worst peak-to-trough performance   |
| Avg Profit / Loss       | Mean profit and loss sizes         |
| # of Trades             | Total trades executed              |
| Win Rate                | % of trades with positive profit   |
| Performance Per Order   | Profit / trade                     |
| Capital Allocation Rate | Dynamic based on Q confidence      |

---

## 📁 Output Files

* `11.2.xlsx`: Summary of all simulation metrics
* `11.2Tesla.png`: Bar plot comparing profit % to SPY and average
* `plots/SimulationX_cumulative_profit.png`: Cumulative profit for each simulation

---

## 📦 Installation

```bash
pip install pandas numpy matplotlib seaborn ta scikit-learn
```

Make sure you have `TSLA.csv` in the same directory.

---

## 📈 Example Plots

---

## 🧠 Why This Project Matters

This project shows how **classic RL methods** can be applied in trading scenarios. Unlike black-box deep learning models, it is:

* ✅ Transparent and interpretable
* ✅ Easy to debug and extend
* ✅ A strong baseline for testing reward functions, feature encodings, and action strategies

---

## 💡 Extensions & Ideas

* Swap Q-table with Deep Q-Network (DQN)
* Train on one stock, test on another
* Add stop-loss / take-profit rules
* Introduce volatility-based indicators
* Use walk-forward validation for robustness


