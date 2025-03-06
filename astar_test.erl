-module(astar_test).
-include_lib("eunit/include/eunit.hrl").

find_path_test() ->
    GridSize = 5,
    Reserved = [{2,2}], % Block one position
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
    end.

find_path_no_solution_test() ->
    GridSize = 3,
    Reserved = [{1,0}, {1,1}, {1,2}], % Block the entire middle column
    Start = {0,0},
    Goal = {2,2},
    Result = astar:find_path(Start, Goal, GridSize, Reserved),
    ?assertEqual({error, no_path}, Result).

all_test_() ->
    {setup, fun() -> ok end, fun() -> ok end, 
    [fun find_path_test/0, fun find_path_no_solution_test/0]}.