# 🤖 RoboCup Soccer Simulation in Prolog ⚽

A simplified soccer match simulation between two teams written in **Prolog**, featuring basic AI behaviors, ball physics, and a 2D graphical field using the XPCE graphics library.

---

## 🎮 Overview

This Prolog program simulates a RoboCup-style soccer match with:

- 🟥 **Team 1 (Red)** and 🟦 **Team 2 (Blue)**  
- 👥 Each team has 3 players:
  - 1 Forward (Attacker)
  - 1 Defender
  - 1 Goalkeeper
- 🧠 Basic AI for movement, possession, and strategy
- ⚽ Real-time ball physics and game rules
- 🖼️ XPCE-based visual field rendering

---

## ✨ Features

- 📐 **2D Soccer Field** with boundaries and goal areas
- 🧍‍♂️ **Player Roles**:
  - Forward: Attacker and goal shooter
  - Defender: Tackler and protector
  - Goalkeeper: Goal defense and passing
- ⚙️ **Realistic Mechanics**:
  - Movement with stamina
  - Ball possession, passing, and shooting
  - Tackling and goalkeeping
- 📊 **Live Visualization**:
  - Player positions and roles
  - Ball location
  - Stamina indicators
  - Current match score
- ⏱️ **Timed Rounds** for continuous play
- 🧮 **Score Tracking and Goal Detection**

---

## 🧠 Game Mechanics

### 🕹️ Player Roles

**Forwards:**
- Attack opponents' goal
- Shoot when in range
- Dribble toward goal

**Defenders:**
- Guard home field
- Tackle attackers
- Pass to teammates

**Goalkeepers:**
- Block shots
- Catch and pass the ball

### ⚽ Ball Physics

- Starts at **midfield (50,25)**
- Moves linearly when kicked
- Can be caught by the goalkeeper
- Auto-resets if out-of-bounds or a goal is scored

### 🔁 Game Flow

- Players move based on roles and ball location
- Ball interactions include:
  - Dribbling
  - Passing
  - Shooting (forwards only)
  - Tackling (defenders only)
- Game ends after **450 rounds** (~1.5 min)

---

## ▶️ How to Run

1. Install **SWI-Prolog** with XPCE support
2. Load the code in the Prolog environment
3. Start the game with:

```prolog
start_game.
```
## 🛠️ Customization Options

You can tweak various parts of the simulation to suit your needs:

### 🎯 Starting Positions
Modify player starting positions by editing the predicates:
```prolog
new_simulation_1.
new_simulation_2.
```

## ⏱️ Game Duration
Change how long the match runs by adjusting the number of rounds:
```prolog
run_simulation(N).  % Replace N with desired number of rounds
```

# ⚙️ Customization & Dependencies

## 🧠 Player Behavior

Customize how players act by modifying the logic in:

- `movement` predicates  
- `action` predicates

This lets you alter how players:
- Make decisions
- Move
- Pass
- Shoot
- Defend

---

## 🎨 Visual Style

Edit XPCE-related drawing predicates to customize:

- 🟩 Field appearance  
- 🎨 Color schemes  
- 🔵 Player / ⚫ Ball representation  

Tweak these to suit your aesthetic or debugging preferences.

---

## 📦 Dependencies

Make sure the following are installed:

- [**SWI-Prolog**](https://www.swi-prolog.org/)  
- **XPCE** graphics library  
  *(usually bundled with SWI-Prolog — no separate installation needed)*

---

