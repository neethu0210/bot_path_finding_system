# Grid Mnesia Module Documentation

The `grid_mnesia` module is responsible for managing the 10x10 grid, bot positions, and path reservations using Erlang's **Mnesia** database. This module provides functions for initializing the grid, adding bots, reserving paths, and retrieving grid-related information.

## **Purpose**

The purpose of this module is to:
- **Store and manage** grid data using Mnesia.
- **Handle bot positions** and path reservations efficiently.
- **Provide concurrent access** to grid-related operations using transactions.

## **How It Works**

- Initializes Mnesia schema and starts the database.
- Creates tables for **grid**, **bots**, and **reservations**.
- Supports functions to modify and retrieve data while ensuring consistency using transactions.

## **Records**
- grid: Represents a grid cell with `X`, `Y` coordinates and a type (e.g., `empty`, `obstacle`).
```erlang
-record(grid, {x :: integer(), y :: integer(), type :: atom()}).
```
- bot: Represents a bot with an `ID` and `position`.
```erlang
-record(bot, {id :: integer(), pos :: {integer(), integer()}}).
```
- reservation: Represents a `reserved position` for a bot to avoid conflicts.
```erlang
-record(reservation, {pos :: {integer(), integer()}, bot_id :: integer()}).
```

## **Function `init/0`**

**Implemenation**:
```erlang
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_table(grid),
    create_table(bot),
    create_table(reservation).
```
**Explanation**:
- Creates a schema for Mnesia on the current node.
- Starts the Mnesia database.
- Calls `create_table/1` to set up tables for `grid`, `bot`, and `reservation`.

## **Function `create_table/1`**

**Inputs**: 
- **TableName**: The name of the table to be created (`grid`, `bot`, or `reservation`).

**Implemenation**:
```erlang
create_table(grid) ->
    mnesia:create_table(grid, [{ram_copies, [node()]}, {attributes, record_info(fields, grid)}]);

create_table(bot) ->
    mnesia:create_table(bot, [{ram_copies, [node()]}, {attributes, record_info(fields, bot)}]);

create_table(reservation) ->
    mnesia:create_table(reservation, [{ram_copies, [node()]}, {attributes, record_info(fields, reservation)}]).
```
**Explanation**:
- Creates a table in Mnesia with attributes specific to the given table name.
- Uses RAM copies for fast access.

## **Function `add_bot/2`**

**Inputs**:
- **Id**: The unique identifier of the bot.
- **Pos** (tuple `{X, Y}`): The initial position of the bot on the grid.

**Implemenation**:
```erlang
add_bot(Id, Pos) ->
    F = fun() -> mnesia:write(#bot{id=Id, pos=Pos}) end,
    mnesia:transaction(F).
```
**Explanation**:
- Creates a function F that writes a bot record to the Mnesia table.
- Runs F inside a transaction to ensure atomicity.

## **Function `get_bot_position/1`**

**Inputs**:
- **Id**: The unique identifier of the bot.

**Outputs**:
- **Pos** (tuple `{X, Y}`): The current position of the bot on the grid or undefined if the bot is not found.

**Implemenation**:
```erlang
get_bot_position(Id) ->
    F = fun() ->
        case mnesia:read(bot, Id) of
            [#bot{pos=Pos}] -> Pos;
            _ -> undefined
        end
    end,
    mnesia:transaction(F).
```
**Explanation**:
- Reads the `bot` table in Mnesia for the given `Id`.
- If the bot exists, returns its position.
- If not found, returns `undefined`.

## **Function `reserve_path/2`**

**Inputs**:
- **Pos** (tuple `{X, Y}`): The grid position to be reserved.
- **BotId**: The unique identifier of the bot reserving the path.

**Implemenation**:
```erlang
reserve_path(Pos, BotId) ->
    F = fun() -> mnesia:write(#reservation{pos=Pos, bot_id=BotId}) end,
    mnesia:transaction(F).
```
**Explanation**:
- Creates a function F that writes a `reservation` for the given bot.
- Runs F inside a transaction.

## **Function `reserve_position/1`**

**Inputs**:
- **Pos** (tuple `{X, Y}`): The grid position to be reserved.

**Implemenation**:
```erlang
reserve_position(Pos) ->
    mnesia:transaction(fun() -> mnesia:write(#reservation{pos = Pos}) end).
```
**Explanation**:
- Reserves a grid position by writing a `reservation` record to Mnesia.
- Ensures transaction safety.

## **Function `get_reserved_positions/0`**

**Outputs**:
- **`[{X, Y}]`**: List of all reserved positions

**Implemenation**:
```erlang
get_reserved_positions() ->
    F = fun() -> [Pos || #reservation{pos=Pos} <- mnesia:match_object(#reservation{pos='_', bot_id='_'})] end,
    mnesia:transaction(F).
```
**Explanation**:
- Matches all objects in the `reservation` table.
- Extracts positions and returns them in a list.

## **Function `update_grid/2`**

**Inputs**:
- **tuple `{X, Y}`**: Coordinates of the `grid` cell to be updated.
- **Type**: The new type of the grid cell (e.g., `empty`, `obstacle`).

**Implemenation**:
```erlang
update_grid({X, Y}, Type) ->
    F = fun() -> mnesia:write(#grid{x=X, y=Y, type=Type}) end,
    mnesia:transaction(F).
```
**Explanation**:
- Writes a new `grid` record to Mnesia to update a cell's type.
- Runs inside a transaction.

## **Function `initialize_grid/1`**

**Inputs**:
- **Size**: The size of the grid (assumed to be square, e.g., 10 for a 10x10 grid).

**Implemenation**:
```erlang
initialize_grid(Size) ->
    F = fun() ->
        lists:foreach(
            fun(X) ->
                lists:foreach(
                    fun(Y) ->
                        mnesia:write(#grid{x=X, y=Y, type=empty})
                    end, lists:seq(0, Size-1))
            end, lists:seq(0, Size-1))
    end,
    mnesia:transaction(F).
```
**Explanation**:
- Iterates over all grid coordinates from (0,0) to (`Size-1`, `Size-1`).
- Each cell in the grid is initialized with the atom `empty` as its type.
- Uses transactions for atomic execution.
- If an error occurs during initialization, the transaction will roll back, preventing partial grid setup.