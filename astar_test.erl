-module(astar_test).
-include_lib("eunit/include/eunit.hrl").

astar_test_() ->
    {
        setup,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            {
                "TestCase 1: Find path in a 5x5 grid with one blocked position",
                fun() ->
                    GridSize = 5,
                    Reserved = [{2,2}],
                    Start = {0,0},
                    Goal = {4,4},
                    Result = astar:find_path(Start, Goal, GridSize, Reserved),
                    case Result of
                        {ok, Path} ->
                            ?assert(lists:member(Start, Path)),
                            ?assert(lists:member(Goal, Path)),
                            ?assertNot(lists:member({2,2}, Path));
                        {error, no_path} ->
                            ct:fail("Pathfinding failed when it should succeed")
                    end
                end
            },
            {
                "TestCase 2: No solution when entire middle column is blocked",
                fun() ->
                    GridSize = 3,
                    Reserved = [{1,0}, {1,1}, {1,2}],
                    Start = {0,0},
                    Goal = {2,2},
                    Result = astar:find_path(Start, Goal, GridSize, Reserved),
                    ?assertEqual({error, no_path}, Result)
                end
            }
        ]
    }.
