-module(grid_mnesia).
-include("/usr/local/lib/erlang/lib/mnesia-4.23.3/src/mnesia.hrl").

-export([init/0, add_bot/2, reserve_path/2, get_bot_position/1, get_reserved_positions/0, update_grid/2, initialize_grid/1, reserve_position/1]).

-record(grid, {x :: integer(), y :: integer(), type :: atom()}).
-record(bot, {id :: integer(), pos :: {integer(), integer()}}).
-record(reservation, {pos :: {integer(), integer()}, bot_id :: integer()}).

-spec init() -> ok.
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_table(grid),
    create_table(bot),
    create_table(reservation).

-spec create_table(atom()) -> {atomic, ok} | {aborted, term()}.
create_table(grid) ->
    mnesia:create_table(grid, [{ram_copies, [node()]}, {attributes, record_info(fields, grid)}]);

create_table(bot) ->
    mnesia:create_table(bot, [{ram_copies, [node()]}, {attributes, record_info(fields, bot)}]);

create_table(reservation) ->
    mnesia:create_table(reservation, [{ram_copies, [node()]}, {attributes, record_info(fields, reservation)}]).

-spec add_bot(integer(), {integer(), integer()}) -> {atomic, ok} | {aborted, term()}.
add_bot(Id, Pos) ->
    F = fun() -> mnesia:write(#bot{id=Id, pos=Pos}) end,
    mnesia:transaction(F).

-spec get_bot_position(integer()) -> {integer(), integer()} | undefined.
get_bot_position(Id) ->
    F = fun() ->
        case mnesia:read(bot, Id) of
            [#bot{pos=Pos}] -> Pos;
            _ -> undefined
        end
    end,
    mnesia:transaction(F).

-spec reserve_path({integer(), integer()}, integer()) -> {atomic, ok} | {aborted, term()}.
reserve_path(Pos, BotId) ->
    F = fun() -> mnesia:write(#reservation{pos=Pos, bot_id=BotId}) end,
    mnesia:transaction(F).

-spec reserve_position({integer(), integer()}) -> ok.
reserve_position(Pos) ->
    mnesia:transaction(fun() -> mnesia:write(#reservation{pos = Pos}) end).

-spec get_reserved_positions() -> {atomic, [{integer(), integer()}]}.
get_reserved_positions() ->
    F = fun() -> [Pos || #reservation{pos=Pos} <- mnesia:match_object(#reservation{pos='_', bot_id='_'})] end,
    mnesia:transaction(F).

-spec update_grid({integer(), integer()}, atom()) -> {atomic, ok} | {aborted, term()}.
update_grid({X, Y}, Type) ->
    F = fun() -> mnesia:write(#grid{x=X, y=Y, type=Type}) end,
    mnesia:transaction(F).

-spec initialize_grid(integer()) -> {atomic, ok} | {aborted, term()}.
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