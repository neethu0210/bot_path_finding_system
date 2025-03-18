# Bot Pathfinding System

## Description

The **Bot Pathfinding System** is an Erlang-based implementation that manages the movement of **5 autonomous bots** on a **10x10 grid** using the **A-star algorithm**.

The bots operate within a **10x10 grid**, where each cell represents a **coordinate (X, Y)**. The grid is stored and managed using **Mnesia**, Erlang’s distributed database, allowing concurrent access and updates.

A total of **5 bots** are spawned at different locations on the grid, each with a designated target destination. The system calculates the optimal path using the **A-star algorithm**, considering obstacles, reserved paths, and efficient routing. Bots request paths concurrently, allowing multiple bots to calculate their path plans simultaneously.

To prevent conflicts, the system tries to **reserve grid cells** in advance for each bot’s path, ensuring that no two bots have conflicting routes.

If any bot’s path cannot be reserved due to conflicts with other bots' paths, the system does not attempt to adjust only that bot’s route. Instead, **all bots are shuffled**, and their paths are **recalculated together**. This process is repeated up to **50 times** to find a feasible set of non-conflicting path reservations.

If, after **50 attempts**, the system is still unable to assign valid paths to all bots, it returns the message:
**"Failed to assign paths after multiple attempts."**

## Documentation

For a detailed explanation of the implementation, refer to the **full documentation** for each module:

[Grid Mnesia Module Documentation](docs/grid_mnesia_documentation.md) - Manages the 10x10 grid, bot positions, obstacles, and reservations using Mnesia.
[A-star Module Documentation](docs/astar_documentation.md) - Implements A* pathfinding, handling heuristic calculations and optimal route selection.
[Bot Paths Module Documentation](docs/bot_paths_documentation.md) - Coordinates bot spawning, path requests, and conflict resolution strategies.

## How to Run

1. **Compile the Code**:
   Open the Erlang shell by running the following command:
   ```bash
   erl

2. **Run Test Cases**: 
    You can test the Bot Pathfinding System by running the following commands in the Erlang shell:  
    **Test Case 1**:
    ```erlang
    bot_paths:spawn_bots([{0,0}, {9,9}, {0,9}, {9,0}, {5,5}]).
    bot_paths:request_paths([{4,4}, {5,9}, {9,4}, {4,0}, {6,6}]).
    ```

    **Test Case 2**:
    ```erlang
    bot_paths:spawn_bots([{1,1}, {8,8}, {1,8}, {8,1}, {4,4}]).
    bot_paths:request_paths([{7,7}, {2,2}, {7,2}, {2,7}, {5,5}]).
    ```

    **Test Case 3**:
    ```erlang
    bot_paths:spawn_bots([{0,0}, {9,9}, {4,4}, {0,9}, {9,0}]).
    bot_paths:request_paths([{9,0}, {0,9}, {2,2}, {7,7}, {3,3}]).
    ```

    **Test Case 4**:
    ```erlang
    bot_paths:spawn_bots([{0,1}, {0,3}, {0,5}, {0,7}, {0,9}]).
    bot_paths:request_paths([{9,1}, {9,3}, {9,5}, {9,7}, {9,9}]).
    ```