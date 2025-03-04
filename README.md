# Bot Pathfinding System

## Description

The **Bot Pathfinding System** is an Erlang-based solution designed to manage the movement and pathfinding of **5 bots** within a **10x10 grid**. Each bot is spawned at a specific position and has a goal to reach. The system calculates the best path for each bot, considering any grid reservations and obstacles. The bots' movements and assignments are efficiently handled using the **A* (A-star)** algorithm.

Key features include:
- **10x10 Grid**: The grid is stored and managed in **Mnesia**.
- **5 Bots**: The system spawns 5 bots at different grid positions.
- **Concurrent Path Requests**: Bots request their paths concurrently.
- **Path Reservation**: The system ensures that paths are reserved with no collisions, ensuring smooth navigation for each bot.

## How to Run

1. **Compile the Code**:
   Open the Erlang shell by running the following command:
   ```bash
   erl

2. **Run Test Cases**: 
    You can test the Bot Pathfinding System by running the following commands in the Erlang shell:
    **Test Case 1**:
    ```bash
    bot_paths:spawn_bots([{0,0}, {9,9}, {0,9}, {9,0}, {5,5}]).
    bot_paths:request_paths([{4,4}, {5,9}, {9,4}, {4,0}, {6,6}]).
    ```

    **Test Case 2**:
    ```bash
    bot_paths:spawn_bots([{1,1}, {8,8}, {1,8}, {8,1}, {4,4}]).
    bot_paths:request_paths([{7,7}, {2,2}, {7,2}, {2,7}, {5,5}]).
    ```

    **Test Case 3**:
    ```bash
    bot_paths:spawn_bots([{0,0}, {9,9}, {4,4}, {0,9}, {9,0}]).
    bot_paths:request_paths([{9,0}, {0,9}, {2,2}, {7,7}, {3,3}]).
    ```

    **Test Case 4**:
    ```bash
    bot_paths:spawn_bots([{0,1}, {0,3}, {0,5}, {0,7}, {0,9}]).
    bot_paths:request_paths([{9,1}, {9,3}, {9,5}, {9,7}, {9,9}]).
    ```