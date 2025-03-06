-module(grid_mnesia_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    mnesia:start(),
    grid_mnesia:init(),
    ok.

cleanup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    ok.

init_test() ->
    grid_mnesia:init(),
    ?assertEqual(ok, mnesia:wait_for_tables([grid, bot, reservation], 5000)).

add_bot_test() ->
    grid_mnesia:init(),
    ?assertMatch({atomic, ok}, grid_mnesia:add_bot(1, {2,3})),
    {atomic, Pos} = grid_mnesia:get_bot_position(1),
    ?assertEqual({2,3}, Pos).

get_bot_position_test() ->
    grid_mnesia:init(),
    grid_mnesia:add_bot(2, {5,5}),
    {atomic, Pos} = grid_mnesia:get_bot_position(2),
    ?assertEqual({5,5}, Pos),
    {atomic, Pos1} = grid_mnesia:get_bot_position(99),
    ?assertEqual(undefined, Pos1).

reserve_path_test() ->
    grid_mnesia:init(),
    ?assertMatch({atomic, ok}, grid_mnesia:reserve_path({4,5}, 2)),
    {atomic, ReservedPositions} = grid_mnesia:get_reserved_positions(),
    ?assert(lists:member({4,5}, ReservedPositions)).

reserve_position_test() ->
    grid_mnesia:init(),
    grid_mnesia:reserve_position({3,3}),
    {atomic, ReservedPositions} = grid_mnesia:get_reserved_positions(),
    ?assert(lists:member({3,3}, ReservedPositions)).

get_reserved_positions_test() ->
    grid_mnesia:init(),
    grid_mnesia:reserve_path({1,1}, 1),
    grid_mnesia:reserve_path({2,2}, 2),
    {atomic, ReservedPositions} = grid_mnesia:get_reserved_positions(),
    ?assert(lists:member({1,1}, ReservedPositions)),
    ?assert(lists:member({2,2}, ReservedPositions)).

update_grid_test() ->
    grid_mnesia:init(),
    ?assertMatch({atomic, ok}, grid_mnesia:update_grid({3,3}, obstacle)).

initialize_grid_test() ->
    grid_mnesia:init(),
    ?assertMatch({atomic, ok}, grid_mnesia:initialize_grid(10)).

instantiator() ->
    [
        fun() -> init_test() end,
        fun() -> add_bot_test() end,
        fun() -> get_bot_position_test() end,
        fun() -> reserve_path_test() end,
        fun() -> reserve_position_test() end,
        fun() -> get_reserved_positions_test() end,
        fun() ->  update_grid_test() end,
        fun() -> initialize_grid_test() end
    ].

all_test_() ->
    {setup, fun setup/0, fun cleanup/0, instantiator()}.
