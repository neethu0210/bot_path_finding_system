# Bot Pathfinding Module Documentation

## **Introduction**
The `bot_paths` module is responsible for managing bot navigation within a grid using the A* pathfinding algorithm. It spawns bots at given positions and assigns paths to them while avoiding obstacles and other reserved positions. The module ensures efficient path allocation and retries pathfinding if initial assignments fail.

## **Purpose**
The primary objectives of this module are:
- **Spawn bots** at specified locations.
- **Assign paths** to each bot from its start position to its goal.
- **Ensure optimal pathfinding** using the A* algorithm.
- **Handle path reservations** to avoid conflicts.
- **Retry path assignments** if the initial attempt fails.

## **Types**
The module defines the following types:
```erlang
-type position() :: {integer(), integer()}.
-type path() :: [position()].
```

## **Function `spawn_bots/1`**

**Inputs**:
- **`Sources`**: A list of tuples representing the starting positions of the bots.

**Implementation**:
```erlang
spawn_bots(Sources) ->
    grid_mnesia:init(),
    grid_mnesia:initialize_grid(10),
    lists:foreach(fun({Id, Pos}) ->
        io:format("Spawning Bot ~p at ~p~n", [Id, Pos]),
        Result = grid_mnesia:add_bot(Id, Pos),
        grid_mnesia:update_grid(Pos, occupied),
        io:format("Bot ~p Insert Result: ~p~n", [Id, Result])
    end, lists:zip(lists:seq(1, length(Sources)), Sources)).
```
**Explanation**:
- **Initialize the Mnesia Grid**: Calls `grid_mnesia:init()` to set up the database.
- **Set Grid Size**: Calls `grid_mnesia:initialize_grid(10)` to define a 10x10 grid.
- **Iterate Through Each Bot Position**:
   - `lists:zip/2` pairs each bot ID with a position.
   - `lists:foreach/2` iterates over each `(Id, Pos)` pair.
- **Spawn Each Bot**:
   - `io:format/2` logs the bot's ID and position.
   - `grid_mnesia:add_bot/2` attempts to insert the bot into Mnesia.
   - `grid_mnesia:update_grid/2` marks the position as occupied.
   - The result of insertion is logged.

## **Function `request_paths/1`**

**Inputs**:
- **`Goals`**: A list of goal positions for the bots.

**Outputs**:
- `{ok, [{BotId, Path}]}` if successful, where `Path` is a list of positions representing the bot's movement from its start to goal.
- `{error, []}` if path assignment fails after retries.

**Implementation**:
```erlang
request_paths(Goals) ->
    {atomic, Bots} = mnesia:transaction(fun() -> mnesia:match_object({bot, '_', '_'}) end),
    BotPositions = [{Id, Pos} || {bot, Id, Pos} <- Bots],
    SortedBotPositions = lists:sort(fun({Id1, _}, {Id2, _}) -> Id1 < Id2 end, BotPositions),
    ReservedInitial = [Start || {_, Start} <- SortedBotPositions, Start =/= undefined],
    FullReservation = ReservedInitial ++ Goals,

    io:format("Starting pathfinding for all bots...~n"),
    BotGoalPairs = lists:zip(SortedBotPositions, Goals),
    ShuffledBotGoalPairs = shuffle_list(BotGoalPairs),
    ShuffledBots = [{Id, Pos} || {{Id, Pos}, _Goal} <- ShuffledBotGoalPairs],
    ShuffledGoals = [Goal || {_Bot, Goal} <- ShuffledBotGoalPairs],

    case find_paths_for_all(ShuffledBots, ShuffledGoals, FullReservation, []) of
        {ok, Paths} ->
            io:format("All bots found paths successfully. Reserving paths now.~n"),
            lists:foreach(fun({BotId, Path}) ->
                grid_mnesia:reserve_path(Path, BotId)
            end, Paths),
            ReversedPaths = [{BotId, lists:reverse(Path)} || {BotId, Path} <- Paths],
            print_paths(ReversedPaths),
            {ok, ReversedPaths};
        {error, _BlockedBots} ->
            retry_path_assignment(SortedBotPositions, Goals, FullReservation, 50)
    end.
```
**Explanation**:
- **Retrieve Active Bots from Mnesia**:
   - Uses `mnesia:transaction/1` to fetch all bot records.
   - Extracts bot positions into `BotPositions`.
- **Sort Bots by ID**:
   - Uses `lists:sort/2` to order bots by their IDs.
- **Prepare Reservation List**:
   - Stores initial reserved positions.
   - Combines reserved positions with goal positions.
- **Pair Bots with Goals**:
   - Uses `lists:zip/2` to pair each bot with a goal.
   - Calls `shuffle_list/1` to randomize the bot-goal pairs.
- **Attempt Pathfinding**:
   - Calls `find_paths_for_all/4` to compute paths.
   - If successful, reserves the paths using `grid_mnesia:reserve_path/2`.
   - If pathfinding fails, retries with `retry_path_assignment/4`.

## **Function `find_paths_for_all/4`**

**Inputs**:
- **Bots**: A list of bots and their positions.
- **Goals**: A list of goal positions.
- **Reserved**: A list of reserved positions.
- **Acc**: An accumulator for storing paths.

**Outputs**:
- `{ok, [{BotId, Path}]}` if successful, where `Path` is a list of positions representing the bot's movement from its start to goal.
- `{error, BlockedBots}` if pathfinding fails.

**Implementation**:
```erlang
find_paths_for_all(Bots, Goals, Reserved, Acc) ->
    BotGoalPairs = lists:zip(Bots, Goals),
    find_paths_for_all_pairs(BotGoalPairs, Reserved, Acc).
```
**Explanation**:
- **Pair Bots with Goals**:
   - Uses `lists:zip/2` to create bot-goal pairs.
- **Call `find_paths_for_all_pairs/3`**:
   - Passes the pairs to the recursive function.
   - Handles pathfinding for each bot-goal pair.

## **Function `find_paths_for_all_pairs/3`**

**Inputs**:
- **`[{{BotId, Start}, Goal} | Rest]`**: A list of bot-goal pairs.
- **Reserved**: A list of reserved positions.
- **Acc**: An accumulator for storing results.

**Outputs**:
- `{ok, [{BotId, Path}]}` if successful, where `Path` is a list of positions representing the bot's movement from its start to goal.
- `{error, BlockedBots}` if some bots cannot reach their goal.

**Implementation**:
```erlang
find_paths_for_all_pairs([], _, Acc) -> {ok, Acc};
find_paths_for_all_pairs([{{BotId, Start}, Goal} | Rest], Reserved, Acc) ->
    UpdatedReserved = Reserved -- [Start],
    FinalReservation = UpdatedReserved -- [Goal],
    case astar:find_path(Start, Goal, 10, FinalReservation) of
        {error, _} -> {error, [BotId | [B || {{B, _}, _} <- Rest]]};
        {ok, Path} -> find_paths_for_all_pairs(Rest, Reserved ++ Path, [{BotId, Path} | Acc])
    end.
```
**Explanation**:
- **Base Case**: If the list is empty, return accumulated results.
- **Extract Next Bot-Goal Pair**:
   - `BotId` and `Start` represent the bot's ID and start position.
   - `Goal` is the target position.
- **Update Reservation List**:
   - Removes the bot's start position.
   - Temporarily removes the goal position.
- **Attempt Pathfinding**:
   - Calls `astar:find_path/4` to compute a path.
   - If unsuccessful, returns `{error, BlockedBots}`.
   - If successful, recursively process the remaining pairs, adding the path to reservations.

## **Function `retry_path_assignment/4`**

**Inputs**:
- **`Bots`**: A list of bot IDs and their starting positions.
- **`Goals`**: A list of goal positions for the bots.
- **`Reserved`**: A list of positions that are already reserved.
- **`Attempts`**: The number of times to retry path assignment.

**Outputs**:
- `{ok, [{BotId, Path}]}` if successful, where `Path` is a list of positions representing the bot's movement from its start to goal.
- `{error, []}` if all retries fail.

**Implementation**:
```erlang
retry_path_assignment(_, _, _, 0) ->
    io:format("Failed to assign paths after multiple attempts.~n"),
    {error, []};
retry_path_assignment(Bots, Goals, Reserved, Attempts) ->
    SortedBots = lists:sort(fun({Id1, _}, {Id2, _}) -> Id1 < Id2 end, Bots),
    BotGoalPairs = lists:zip(SortedBots, Goals),
    ShuffledBotGoalPairs = shuffle_list(BotGoalPairs),
    ShuffledBots = [{Id, Pos} || {{Id, Pos}, _Goal} <- ShuffledBotGoalPairs],
    ShuffledGoals = [Goal || {_Bot, Goal} <- ShuffledBotGoalPairs],
    case find_paths_for_all(ShuffledBots, ShuffledGoals, Reserved, []) of
        {ok, Paths} ->
            lists:foreach(fun({BotId, Path}) ->
                grid_mnesia:reserve_path(Path, BotId)
            end, Paths),
            ReversedPaths = [{BotId, lists:reverse(Path)} || {BotId, Path} <- Paths],
            print_paths(ReversedPaths),
            {ok, ReversedPaths};
        {error, _BlockedBots} ->
            retry_path_assignment(Bots, Goals, Reserved, Attempts - 1)
    end.
```

**Explanation**:
- **Base Case:** If `Attempts` reaches 0, the function logs a failure message and returns `{error, []}`.
- **Sort Bots:** The list of bots is sorted based on their IDs to maintain order.
- **Pair Bots with Goals:** Each bot is paired with a goal using `lists:zip/2`.
- **Shuffle Pairs:** The bot-goal pairs are shuffled to introduce randomness in assignments.
- **Extract Data:** Extracts the shuffled bot positions and goals into separate lists.
- **Find Paths:** Calls `find_paths_for_all/4` to find paths for all bots.
- **Path Reservation:** If paths are found, they are reserved in `grid_mnesia`.
- **Reverse Paths:** Paths are reversed for correct traversal order.
- **Print Paths:** Calls `print_paths/1` to display assigned paths.
- **Retry Logic:** If pathfinding fails, the function recurses with `Attempts - 1`.

## **Function `shuffle_list/1`**

**Implementation**:
```erlang
shuffle_list(List) ->
    Randomized = [{rand:uniform(), X} || X <- List],
    Sorted = lists:sort(Randomized),
    [X || {_, X} <- Sorted].
```
**Explanation**:
- **Assign Random Weights**: Generates a random number for each element.
- **Sort Based on Weights**: Uses `lists:sort/1`.
- **Extract Sorted Elements**: Returns only the list elements, shuffled.

## **Function `print_paths/1`**

**Inputs**:
- **`Paths`**: A list of tuples where each tuple contains a bot ID and its assigned path.

**Outputs**:
- Prints the final assigned paths to the console.

**Implementation**:
```erlang
print_paths(Paths) ->
    io:format("Final bot paths:~n"),
    lists:foreach(fun({BotId, Path}) ->
        io:format("Bot ~p -> Path: ~p~n", [BotId, Path])
    end, Paths).
```

**Explanation**:
- **Print Header:** Logs "Final bot paths:" to indicate the start of path printing.
- **Iterate Over Paths:** Uses `lists:foreach/2` to process each bot's path.
- **Format Output:** Prints each bot ID alongside its path.
- **Completion:** Ends execution after printing all paths.