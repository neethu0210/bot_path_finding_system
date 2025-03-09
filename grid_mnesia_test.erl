-module(grid_mnesia_test).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    mnesia:start(),
    grid_mnesia:init(),
    ok.

cleanup(_) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    ok.

grid_mnesia_test_() ->
    {
        setup,
        fun setup/0,
        fun cleanup/1,
        [
            {
                "TestCase 1: Initialize Mnesia tables",
                fun() ->
                    grid_mnesia:init(),
                    ?assertEqual(ok, mnesia:wait_for_tables([grid, bot, reservation], 5000))
                end
            },
            {
                "TestCase 2: Add a bot",
                fun() ->
                    grid_mnesia:init(),
                    ?assertMatch({atomic, ok}, grid_mnesia:add_bot(1, {2,3})),
                    {atomic, Pos} = grid_mnesia:get_bot_position(1),
                    ?assertEqual({2,3}, Pos)
                end
            },
            {
                "TestCase 3: Get bot position",
                fun() ->
                    grid_mnesia:init(),
                    grid_mnesia:add_bot(2, {5,5}),
                    {atomic, Pos} = grid_mnesia:get_bot_position(2),
                    ?assertEqual({5,5}, Pos),
                    {atomic, Pos1} = grid_mnesia:get_bot_position(99),
                    ?assertEqual(undefined, Pos1)
                end
            },
            {
                "TestCase 4: Reserve a path",
                fun() ->
                    grid_mnesia:init(),
                    ?assertMatch({atomic, ok}, grid_mnesia:reserve_path({4,5}, 2)),
                    {atomic, ReservedPositions} = grid_mnesia:get_reserved_positions(),
                    ?assert(lists:member({4,5}, ReservedPositions))
                end
            },
            {
                "TestCase 5: Reserve a position",
                fun() ->
                    grid_mnesia:init(),
                    grid_mnesia:reserve_position({3,3}),
                    {atomic, ReservedPositions} = grid_mnesia:get_reserved_positions(),
                    ?assert(lists:member({3,3}, ReservedPositions))
                end
            },
            {
                "TestCase 6: Get reserved positions",
                fun() ->
                    grid_mnesia:init(),
                    grid_mnesia:reserve_path({1,1}, 1),
                    grid_mnesia:reserve_path({2,2}, 2),
                    {atomic, ReservedPositions} = grid_mnesia:get_reserved_positions(),
                    ?assert(lists:member({1,1}, ReservedPositions)),
                    ?assert(lists:member({2,2}, ReservedPositions))
                end
            },
            {
                "TestCase 7: Update grid",
                fun() ->
                    grid_mnesia:init(),
                    ?assertMatch({atomic, ok}, grid_mnesia:update_grid({3,3}, obstacle))
                end
            },
            {
                "TestCase 8: Initialize grid with size",
                fun() ->
                    grid_mnesia:init(),
                    ?assertMatch({atomic, ok}, grid_mnesia:initialize_grid(10))
                end
            }
        ]
    }.
