# A* Pathfinding Module Documentation

## **Introduction**
The `astar` module implements the A* pathfinding algorithm in Erlang. It calculates the shortest path from a start position to a goal position while avoiding reserved positions. The module efficiently manages the open and closed sets using Erlang Term Storage (**ETS**) tables.

## **Purpose**
The primary objectives of this module are:
- **Find optimal paths** using the A* search algorithm.
- **Avoid reserved positions** while searching for a path.
- **Efficiently manage search state** using ETS for fast access.

## **How It Works**
1. Initializes ETS tables to store:
   - **OpenQueue** (priority queue for exploration).
   - **ClosedSet** (visited positions).
   - **ParentMap** (to reconstruct the final path).
2. Uses the Manhattan heuristic for estimating path costs.
3. Expands neighbors of the current position and updates the open queue.
4. Continues searching until the goal is reached or no path is found.
5. Constructs the final path using the `ParentMap`.

## **Types**
The module defines the following types:
```erlang
-type position() :: {integer(), integer()}.
-type grid_size() :: integer().
-type path() :: [position()].
-type reserved_positions() :: [position()].
-type search_result() :: {ok, path()} | {error, no_path}.
```
## **Function `manhattan/2`**

**Inputs**:
- **`Pos1 {X1, Y1}`**: The first position.
- **`Pos2 {X2, Y2}`**: The second position.

**Outputs**:
- **Distance**: The Manhattan distance between the two positions.

**Implementation**:
```erlang
manhattan({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2).
```
**Explanation**:
- **Extract coordinates**: `{X1, Y1}` and `{X2, Y2}`.
- **Compute absolute differences**: `abs(X1 - X2)` and `abs(Y1 - Y2)`.
- **Sum the differences**: This gives the Manhattan distance, which is the heuristic function for A*.

## **Function `find_path/4`**

**Inputs**:
- Start {X, Y}: The starting position.
- Goal {X, Y}: The target position.
- GridSize (integer): The size of the grid.
- Reserved [{X, Y}]: List of reserved positions.

**Outputs**:
- {ok, Path}: A successful path from Start to Goal.
- {error, no_path}: No valid path found.

**Implementation**:
```erlang
find_path(Start, Goal, GridSize, Reserved) ->
    search(Start, fun neighbour_fn/3, fun score_fn/3, Goal, GridSize, Reserved).
```
## **Explanation**:
- Calls search/6 to begin A* pathfinding.
- Passes:
    - Start position.
    - Goal position.
    - GridSize to limit boundaries.
    - Reserved positions to avoid.
    - neighbour_fn/3 (to get valid moves).
    - score_fn/3 (to calculate path cost).

**Function** search/6

**Inputs**:
- Start {X, Y}: The starting position.
- NeighbourFn (fun()): Function to generate neighboring positions.
- ScoreFn (fun()): Function to calculate path cost.
- Goal {X, Y}: The target position.
- GridSize (integer): The size of the grid.
- Reserved [{X, Y}]: List of reserved positions.

**Outputs**:
- {ok, Path}: A successful path from Start to Goal.
- {error, no_path}: No valid path found.

**Implementation**:
```erlang
search(Start, NeighbourFn, ScoreFn, Goal, GridSize, Reserved) ->
    {OpenQueue, ClosedSet, ParentMap} = data_sets(),
    push(OpenQueue, [], Start, ScoreFn([], Start, Goal)),
    ets:insert(ParentMap, {Start, none}),
    continue(OpenQueue, ClosedSet, ParentMap, NeighbourFn, ScoreFn, Goal, GridSize, Reserved).
```
**Explanation**:
- Initialize ETS tables using data_sets/0:
    - OpenQueue: Holds nodes to explore.
    - ClosedSet: Tracks visited nodes.
    - ParentMap: Maps each node to its predecessor.
- Push the start position into OpenQueue with its score.
- Mark Start as having no parent in ParentMap.
- Call continue/8 to begin pathfinding.

**Function** continue/8

**Inputs**:
- OpenQueue: The priority queue for exploration.
- ClosedSet: The set of already visited positions.
- ParentMap: Stores parent-child relationships.
- NeighbourFn: Function to get valid neighbors.
- ScoreFn: Function to compute path costs.
- Goal: The target position.
- GridSize: The size of the grid.
- Reserved: List of reserved positions.

**Outputs**:
- {ok, Path}: A successful path.
- {error, no_path}: No valid path found.

**Implementation**:
```erlang
continue(OpenQueue, ClosedSet, ParentMap, NeighbourFn, ScoreFn, Goal, GridSize, Reserved) ->
    case pop_best(OpenQueue, ClosedSet) of
        none -> {error, no_path};
        {_, _Path, Goal} -> reconstruct_path(ParentMap, Goal);
        {_BestScore, Path, State} ->
            lists:foreach(
                fun(Nstate) ->
                    case {is_closed(ClosedSet, Nstate), is_open(OpenQueue, Nstate), ets:lookup(ParentMap, Nstate)} of
                        {true, _, _} -> skip;
                        {_, true, _} -> skip;
                        {_, _, [{Nstate, _}]} -> skip;
                        _ ->
                            Nfullpath = [State | Path],
                            push(OpenQueue, Nfullpath, Nstate, ScoreFn(Nfullpath, Nstate, Goal)),
                            ets:insert(ParentMap, {Nstate, State})
                    end
                end,
                NeighbourFn(State, GridSize, Reserved)
            ),
            continue(OpenQueue, ClosedSet, ParentMap, NeighbourFn, ScoreFn, Goal, GridSize, Reserved)
    end.
```
**Explanation**:
- Pop the best node from OpenQueue using pop_best/2:
    - If no nodes remain, return {error, no_path}.
    - If the goal is reached, call reconstruct_path/2.
- Expand neighbors using NeighbourFn/3.
- Check if a neighbor is valid:
    - Skip if in ClosedSet or OpenQueue.
    - Otherwise, update ParentMap and push to OpenQueue.
- Recursively continue exploring until a path is found or all nodes are exhausted.

## **Function `reconstruct_path/2`**

**Inputs**:
- `ParentMap`: ETS table mapping positions to their parent positions.
- `Goal`: The target position.

**Outputs**:
- `{ok, Path}`: The reconstructed path.
- `{error, no_path}`: If the path cannot be reconstructed.

**Implementation**:
```erlang
reconstruct_path(ParentMap, Goal) ->
    reconstruct_path(ParentMap, Goal, []).

reconstruct_path(ParentMap, State, Acc) ->
    case ets:lookup(ParentMap, State) of
        [{State, none}] -> {ok, lists:reverse([State | Acc])};
        [{State, Parent} | _] -> reconstruct_path(ParentMap, Parent, [State | Acc]);
        _ -> {error, no_path}
    end.
```
**Explanation**:
- Calls the helper function with an empty accumulator.
- Looks up the parent of the current state in `ParentMap`.
- If `none`, reverses the accumulated list and returns the path.
- Otherwise, recursively follows the parent chain until `none` is reached.

## **Function `neighbour_fn/3`**

**Inputs**:
- `{X, Y}`: The current position.
- `GridSize`: The size of the grid.
- `Reserved`: List of reserved positions.

**Outputs**:
- A list of valid neighboring positions.

**Implementation**:
```erlang
neighbour_fn({X, Y}, GridSize, Reserved) ->
    Neighbors = [
        {X-1, Y}, {X+1, Y}, {X, Y-1}, {X, Y+1}
    ],
    ValidNeighbors = [{NX, NY} || {NX, NY} <- Neighbors, 
                                   NX >= 0, NX < GridSize, 
                                   NY >= 0, NY < GridSize, 
                                   not lists:member({NX, NY}, Reserved)],
    lists:sort(fun(_, _) -> rand:uniform() > 0.5 end, ValidNeighbors).
```
**Explanation**:
- Generates four possible neighbors (left, right, up, down).
- Filters out invalid neighbors (out of bounds or reserved).
- Randomly sorts the valid neighbors.

## **Function `score_fn/3`**

**Inputs**:
- `Path`: The path taken so far.
- `State`: The current position.
- `Goal`: The target position.

**Outputs**:
- The estimated cost of reaching the goal.

**Implementation**:
```erlang
score_fn(Path, State, Goal) ->
    G = length(Path),
    H = manhattan(State, Goal),
    G + H.
```
**Explanation**:
- `G` is the actual cost from the start.
- `H` is the heuristic (Manhattan distance to the goal).
- Returns `G + H` as the estimated total cost.

## **Function `data_sets/0`**

**Implementation**:
```erlang
data_sets() ->
    OpenQueue = ets:new(openqueue, [public, ordered_set]),
    ClosedSet = ets:new(closedset, [public, set]),
    ParentMap = ets:new(parentmap, [public, set, {keypos, 1}]),
    {OpenQueue, ClosedSet, ParentMap}.
```
**Explanation**:
- Creates ETS tables for managing search data.
- Returns references to these tables.

## **Function `push/4`**

**Inputs**:
- `OpenQueue`: ETS table storing open nodes.
- `Path`: The path taken to reach the current state.
- `State`: The current position.
- `Score`: The total estimated cost.

**Outputs**:
- `true` after inserting the entry.

**Implementation**:
```erlang
push(OpenQueue, Path, State, Score) ->
    ets:insert(OpenQueue, {Score, Path, State}).
```
**Explanation**:
- Inserts `{Score, Path, State}` into the open queue.
- Returns `true` after insertion.

## **Function `pop_best/2`**

**Inputs**:
- `OpenQueue`: ETS table storing open nodes.
- `ClosedSet`: ETS table storing visited nodes.

**Outputs**:
- `none` if the queue is empty.
- `{BestScore, Path, State}`: The best node to expand next.

**Implementation**:
```erlang
pop_best(OpenQueue, ClosedSet) ->
    case ets:first(OpenQueue) of
        '$end_of_table' -> none;
        BestScore ->
            case ets:lookup(OpenQueue, BestScore) of
                [{BestScore, Path, State}] ->  
                    ets:delete(OpenQueue, BestScore),
                    ets:insert(ClosedSet, {State}),
                    {BestScore, Path, State};
                _ -> none
            end
    end.
```
**Explanation**:
- Gets the lowest-cost entry from `OpenQueue`.
- If the queue is empty, returns `none`.
- Otherwise, retrieves the `{BestScore, Path, State}` entry.
- Deletes it from `OpenQueue` and adds `State` to `ClosedSet`.

## **Function `is_open/2`**

**Inputs**:
- `OpenQueue`: ETS table storing open nodes.
- `State`: Position to check.

**Outputs**:
- `true` if `State` is in `OpenQueue`.
- `false` otherwise.

**Implementation**:
```erlang
is_open(OpenQueue, State) -> ets:member(OpenQueue, State).
```
**Explanation**:
- Checks if `State` exists in `OpenQueue`.
- Returns `true` if found, `false` otherwise.

## **Function `is_closed/2`**

**Inputs**:
- `ClosedSet`: ETS table storing closed nodes.
- `State`: Position to check.

**Outputs**:
- `true` if `State` is in `ClosedSet`.
- `false` otherwise.

**Implementation**:
```erlang
is_closed(ClosedSet, State) -> ets:member(ClosedSet, State).
```
**Explanation**:
- Checks if `State` exists in `ClosedSet`.
- Returns `true` if found, `false` otherwise.