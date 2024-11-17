# Rock, Paper, Scissors Tournament Simulation

Welcome to the **Rock, Paper, Scissors Tournament** project! This application simulates a tournament environment for the classic game of Rock, Paper, Scissors (RPS). Built in Erlang, it showcases concurrent programming principles through message-passing and process management.


## **Table of Contents**
1. [Introduction](#introduction)
2. [Game Rules](#game-rules)
3. [How It Works](#how-it-works)
4. [Setup and Usage](#setup-and-usage)
5. [Output](#output)
6. [Final Report](#final-report)


# Introduction

This project is a multi-process Erlang application that simulates a competitive RPS tournament. Players with a finite number of game credits interact in a tournament until only one player remains victorious. The implementation focuses on concurrency and the messaging model of Erlang, providing insights into functional programming's power for distributed systems.


# Game Rules

1. **Moves**:
   - Each player randomly selects one of three moves: **Rock**, **Paper**, or **Scissors**.
   - Victory is determined by:
     - Rock beats Scissors.
     - Paper beats Rock.
     - Scissors beat Paper.
   - Ties are resolved by replaying the game until a winner emerges.

2. **Player Credits**:
   - Players start with a predefined number of credits.
   - Each loss deducts one credit.
   - Players with zero credits are disqualified.

3. **Tournament Rules**:
   - Players randomly challenge others (except themselves) to a game.
   - A "master process" manages the game, keeping track of credits, scheduling games, and resolving ties.
   - The tournament ends when one player remains.


# How It Works

- **Input**: A text file (`.txt`) containing tuples of player names and their starting credits, e.g.:
`{sam, 26}. {jill, 12}. {ahmad, 17}.`


- **Processes**:
  - **Master Process**: Orchestrates the tournament, assigns unique game IDs, determines game outcomes, and manages player credits.
  - **Player Processes**: Represent individual players. They send and receive game requests and report their moves to the master.

- **Concurrency**:
  - Players can play multiple games simultaneously.
  - Games are asynchronous, ensuring smooth simulation without blocking processes.


# Setup and Usage

### **Prerequisites**
- Erlang installed on your system (OTP 23 or higher).

### **Running the Project**
1. Compile the source files:
 ```bash
 erlc game.erl player.erl
 ```
 2. Run the program with an input file:
 ```bash
 erl -noshell -run game start <input_file> -s init stop
 ```

&emsp; e.g. the input file name is `player.txt`,
```bash
 erl -noshell -run game start player.txt -s init stop
 ```

# Output
The program logs key events during the tournament, such as:
  - New game scheduling:
  ```bash
  + [1] New game for sam -> jill
  ```
  - Game results:
  ```bash
  $ (1) sam: rock -> jill: paper = sam loses [25 credits left]
  ```
  - Player disqualifications:
  ```bash
  - (36) sam: paper -> ahmad: rock = ahmad loses [0 credits left]
  ```

# Final Report
At the end of the tournament, a summary is displayed:
```bash
** Tournament Report **
Players:
 sam: credits used: 16, credits remaining: 10
 jill: credits used: 12, credits remaining: 0
 ahmad: credits used: 17, credits remaining: 0
-----
Total games: 45
Winner: sam
```